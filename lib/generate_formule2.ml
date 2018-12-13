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
     match c with
     | None -> []
     | Some c -> generate_all_config c size rs bs list

(* Renvoie la formule correspondant à la conjonction des contraintes de bsp_sat
 et de tout ses fils*)
let rec get_formule_complete (bsp_sat : [`Red | `Blue] bsp_sat) : formule option =
  let formfils =
    match bsp_sat with
    | R_sat (_,_,_) -> None
    | L_sat (_,_,l,r) ->
       let fl = get_formule_complete l in
       let fr = get_formule_complete r in
       maybe2 (fun x y -> Et (x,y)) fl fr
  in
  let form = get_formule_of_list_list (get_list_list_of_bsp_sat bsp_sat) in
  (* print_formule form; *)
  match form, formfils with
  | None, None -> None
  | Some a, None -> Some a
  | None, Some b -> Some b
  | Some a, Some b -> Some (Et (a,b))

(* Renvoie la formule correspondant à la solution encodé dans bsp_sat *)
(* DÉJA SOUS FNC ET NIÉ*)
let rec get_actual_sol (orig : [`Red | `Blue] bsp_sat) =
  match orig with
  | R_sat (i,s,c) ->
     if s then None
     else maybe None (fun c -> Some (Lit (neg (choose c i)))) c
  | L_sat (_,s,l,r) ->
     if s
     then None
     else maybe2 (fun x y -> Ou (x,y)) (get_actual_sol l) (get_actual_sol r)

(* Renvoie une fnc satisfaisable si et seulement si le bsp à plusieurs solution *)
let get_fnc_of_bsp2 (prof : int) (bsp : [`Red | `Blue] bsp) =
  let sat = bsp_sat_of_bsp get_color_line2 bsp |> loop_sat prof in
  let sol = get_actual_sol sat in
  match sol with
    None -> None
  | Some sol ->
     let f = get_formule_complete sat in
     maybe None (fun fnc -> Some (Et (fnc, sol))) f

let get_fnc_of_bsp_soluce2 (working_bsp : [`Red | `Blue] bsp) (linetree :  [`Red | `Blue] linetree)=
  let sat = bsp_sat_of_working_bsp working_bsp linetree in
  if not (check_all_lines sat)
  then None
  else get_formule_complete sat
