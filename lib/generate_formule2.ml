open Couleur
open Utils
open Bsp
open Formule
open Bsp_sat
open Gen_utils

let choose coul n =
  match coul with
  | None -> None
  | Some `Red -> Some (Var n)
  | Some `Blue -> Some (Neg n)

(* Renvoie une liste de liste de lit option correspondant 
   à une fnc satisfaisable ssi il existe un choix de
   coloration possible pour la ligne bsp_sat 
   ATTENTION : seulement pour cette ligne, pas pour ces fils *)
let generate_all_config (coul : [< `Blue | `Red ] couleur_l ) nadja rs bs list =
  match coul with
  | Purple ->
     begin
         let red = get_n_tuples_in_list (nadja / 2 - rs) list in
         let litred = rev_map_ap (fun l -> List.map (fun n -> choose (Some `Red) n) l) red [] in
         let blue = get_n_tuples_in_list (nadja / 2 - bs) list in
         rev_map_ap (fun l -> List.map (fun n -> choose (Some `Blue) n) l) blue litred
     end
  | C co ->
     begin
         match co with
         | `Red ->
            begin
                let red = get_n_tuples_in_list (nadja / 2 + 1 - rs) list in
                rev_map_ap (fun l -> List.map (fun n -> choose (Some `Red) n) l) red []
            end
         | `Blue ->
            begin
                let blue = get_n_tuples_in_list (nadja / 2 + 1 - bs) list in
                rev_map_ap (fun l -> List.map (fun n -> choose (Some `Blue) n) l) blue []
            end
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
  let rec filter_None (f : (lit option) list) =
    match f with
    | [] -> []
    | x::xs ->
       match x with
       | None -> filter_None xs
       | Some x -> (Lit x) :: filter_None xs
  in
  let aux (list : lit option list list) : formule list list =
    List.map filter_None list
  in
  match ligne with
  | R_sat (_,_,_) -> []
  | L_sat (c,_,_,_) ->
     match c with
     | None -> []
     | Some c -> aux (generate_all_config c size rs bs list)

(* Prend une liste de liste de formule et retourne une formule de la forme
 * (_ ou _ ou ... ou _) et (_ ou _ ou ... ou _) et ... et (_ ou _ ou ... ou _)*)
let get_formule_of_list_list (ll : formule list list) : formule option =
  let disj_all (list : formule list) : formule option =
    match list with
    | [] -> None
    | x::q -> Some (List.fold_left (fun acc x -> Ou (acc,x)) x q)
  in
  let rec conj_all (list : formule list list) : formule option =
    match list with
    | [] -> None
    | x::q ->
       let f = conj_all q in
       let g = disj_all x in
       maybe2 (fun x y -> Et (x,y)) f g
  in
  conj_all ll

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
     else
       begin
         match choose c i with
         | None -> None
         | Some x -> Some (Lit (neg x))
       end
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
