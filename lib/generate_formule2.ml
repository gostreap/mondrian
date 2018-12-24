open Couleur
open Utils
open Bsp
open Formule
open Bsp_sat
open Gen_utils

let choose coul n =
  match coul with
  | `Red -> Var n
  | `Blue -> Neg n

(* Renvoie une liste de liste de lit option correspondant
   à une fnc satisfaisable ssi il existe un choix de
   coloration possible pour la ligne bsp_sat
   ATTENTION : seulement pour cette ligne, pas pour ces fils *)
let generate_all_config (coul : [`Blue | `Red ] couleur_l ) nadja rs bs list =
  let chooseRed  n = Lit (choose `Red n)  in
  let chooseBlue n = Lit (choose `Blue n) in
  match coul with
  | Purple ->
     (* On suppose notamment ici que ni bs ni rs ne sont strictement supérieur à nadja/2 *)
     let red = get_n_tuples_in_list (nadja / 2 + 1 - bs) list in
     let litred = List.map (List.map chooseRed) red in
     let blue = get_n_tuples_in_list (nadja / 2 + 1 - rs) list in
     rev_map_ap (List.map chooseBlue) blue litred
  | C co ->
     begin
         match co with
         | `Red ->
            if rs > nadja / 2 then [[]]
            else
              let size = min (nadja - nadja / 2 - bs) (nadja - rs - bs) in
              List.map (List.map chooseRed) (get_n_tuples_in_list size list)
         | `Blue ->
            if bs > nadja / 2 then [[]]
            else
              let size = min (nadja - nadja / 2 - rs) (nadja - rs - bs) in
              List.map (List.map chooseBlue) (get_n_tuples_in_list size list)
     end
  | _ -> failwith "Unexpected Color"

(* POTENTIELLEMENT FUSIONNABLE AVEC GENERATE_ALL_CONFIG*)
(* Renvoie une liste de liste de formule correspondant
   à une fnc satisfaisable ssi il existe un choix de
   coloration possible pour la ligne bsp_sat
   ATTENTION : seulement pour cette ligne, pas pour ces fils
   -> appelle generate_all_config, retire les None et transforme les lits en formule*)
let get_list_list_of_bsp_sat (ligne: [`Blue | `Red ] bsp_sat) : formule list list =
  let rs,_,bs,list = get_adja_stat ligne in
  let size = rs + bs + List.length list in
  match ligne with
  | R_sat (_,_,_) -> []
  | L_sat (c,_,_,_) ->
     maybe [] (fun c -> generate_all_config c size rs bs list) c

(* Prend une liste de liste de formule et retourne une formule de la forme
 * (_ ou _ ou ... ou _) et (_ ou _ ou ... ou _) et ... et (_ ou _ ou ... ou _)*)
let get_formule_of_list_list (ll : formule list list) : formule =
  let disj_all (list : formule list) : formule =
    match list with
    | [] -> Vrai
    | x::q -> List.fold_left (fun acc x -> ou acc x) x q
  in
  List.fold_left (fun acc x -> et acc (disj_all x)) Vrai ll

(* Renvoie la formule correspondant à la conjonction des contraintes de bsp_sat
 et de tout ses fils*)
let rec get_formule_complete (bsp_sat : [`Red | `Blue] bsp_sat) : formule =
  let formfils =
    match bsp_sat with
    | R_sat (_,_,_) -> Vrai
    | L_sat (_,_,l,r) ->
       let fl = get_formule_complete l in
       let fr = get_formule_complete r in
       et fl fr
  in
  let form = get_formule_of_list_list (get_list_list_of_bsp_sat bsp_sat) in
  et form formfils

(* Renvoie la formule correspondant à la solution encodé dans bsp_sat *)
(* DÉJA SOUS FNC ET NIÉ*)
let rec get_actual_sol (orig : [`Red | `Blue] bsp_sat) : formule =
  match orig with
  | R_sat (i,s,c) ->
     if s then Vrai
     else
         begin
             match c with
             | None -> Vrai (*Si le rectangle est vide, la contraine est considéré vrai*)
             | Some co -> Lit (neg (choose co i))
         end
  | L_sat (_,s,l,r) ->
     if s
     then Vrai
     else ou (get_actual_sol l) (get_actual_sol r)

(* Renvoie une fnc satisfaisable si et seulement si le bsp à plusieurs solution *)
let get_fnc_of_bsp2 (prof : int) (bsp : [`Red | `Blue] bsp) =
  let sat = bsp_sat_of_bsp get_color_line2 bsp |> loop_sat prof in
  let sol = get_actual_sol sat in
  match sol with
  | Faux -> Faux
  | Vrai -> Vrai
  | solution -> et (get_formule_complete sat) solution

let get_fnc_of_bsp_soluce2 (prof : int) (working_bsp : [`Red | `Blue] bsp) (linetree :  [`Red | `Blue] linetree)=
  let sat = bsp_sat_of_working_bsp working_bsp linetree |> loop_sat prof in
  if not (check_all_lines sat)
  then Faux
  else get_formule_complete sat 
