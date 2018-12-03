open Couleur
open Utils
open Bsp
open Formule
open Bsp_sat
open Tseitin

(* Renvoie la liste de tout les tuples à n éléments que l'on
   peut former avec les éléments de list *)
let rec get_n_tuples_in_list (n : int) (list : 'a list) : 'a list list=
  let aux x = List.fold_left (fun acc ll -> (x::ll)::acc) []
  in
  if n != 0 then
    match list with
    | [] -> []
    | x::q -> List.rev_append (aux x (get_n_tuples_in_list (n-1) q)) (get_n_tuples_in_list n q)
  else [[]]

let choose coul n =
  match coul with
  | None -> None
  | Some `Red -> Some (Var n)
  | Some `Blue -> Some (Neg n)

(* Convertie une liste d'identifiant de rectangle en une liste de tuple de littéraux, qui une fois conjoncté sont vrais si et seulement si les rectangles sont de couleurs coul.
 *)
(* let formule_list_of_bsp_sat_list (coul : couleur) (list : int list) =
 *   List.map (choose coul) list *)

let get_compl c list =
  List.filter (fun a -> not (List.mem a c)) list

(* Similaire à rev_map mais ajoute une liste en fin*)
let rev_map_ap f l xs =
  let rec rmap_f accu = function
    | [] -> accu
    | a::l -> rmap_f (f a :: accu) l
  in rmap_f xs l

let concat_map_term f l = List.fold_left (fun acc x -> List.rev_append (f x) acc) [] l

let generate_config (r,b) (rs,bs) list =
  let red = get_n_tuples_in_list (r-rs) list in
  let mkredform = rev_map_ap (fun n -> choose (Some `Red) n) in
  let mkgreen r = get_n_tuples_in_list 0 (get_compl r list) in (* TODO remove the absurd work *)
  let mkgreenform =  List.map (fun _ -> None) in
  let mkblue r g = get_n_tuples_in_list (b-bs) (get_compl (List.rev_append r g) list) in
  let mkblueform = rev_map_ap (fun n -> choose (Some `Blue) n) in
  let mkgreenblueform r g =
    let g' = mkgreenform g in
    List.map (fun b ->  mkredform r (mkblueform b g')) (mkblue r g)
  in
  concat_map_term (fun r -> concat_map_term (mkgreenblueform r) (mkgreen r)) red

(* Genère les triplets (x,y,z) tels que (si coul = Red)
 - x+y+z = nadja
 - x > y && x > z
 - i >= is && i <= nadja \forall i \in {x,y,z}
*)
let generate_tuple is_valid (coul: [< `Blue | `Red] couleur_l) nadja rs bs =
  let rec genl f l =
    if f > l
    then []
    else f :: (genl (f+1) l) in
  let rl = genl rs (switch_coul_l (nadja/2) (nadja/2) (nadja/3) (nadja/3)
                                  (switch_coul2 nadja (nadja/2)) coul) in
  let bl = genl bs (switch_coul_l (nadja/2) (nadja/3) (nadja/2) (nadja/3)
                                  (switch_coul2 (nadja/2) nadja) coul) in
  let all_l = List.rev_map (fun x -> List.rev_map (fun y -> (x,y) ) bl) rl in
  let filtered = List.filter is_valid (List.concat all_l) in
  match coul with
  | C `Red -> (nadja/2+1,bs):: filtered (* TODO VÉRIFIER BORNES *)
  | C `Blue -> (rs,nadja/2+1)::filtered
  | _ -> filtered

let generate_all_config (coul : [< `Blue | `Red ] couleur_l ) nadja rs bs list=
  let is_valid (r,b) =
    match coul with
    | Purple -> r+b = nadja && r = b
    | C co ->
       begin
         match co with
         | `Red -> r+b = nadja && r > b
         | `Blue -> r+b = nadja && b > r
       end
    | _ -> false
  in
  let triplets = generate_tuple is_valid coul nadja rs bs in
  concat_map_term (fun x -> generate_config x (rs,bs) list) triplets

(* Renvoie une liste de liste de string correspondant
   à une fnd satisfaisable ssi il existe un choix de
   coloration possible pour la ligne bsp_sat
   ATTENTION : seulement pour cette ligne, pas pour ces fils *)
let get_list_list_of_bsp_sat (ligne: [`Blue | `Red ] bsp_sat) : formule list list =
  let rs,bs,list = get_adja_stat2 ligne in
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
 * (_ et _ et ... et _) ou (_ et _ et ... et _) ou ... ou (_ et _ et ... et _)*)
let get_formule_of_list_list (ll : formule list list) : formule option =
  let conj_all (list : formule list) : formule option =
    match list with
    | [] -> None
    | x::q -> Some (List.fold_left (fun acc x -> Et (acc,x)) x q)
  in
  let rec disj_all (list : formule list list) : formule option =
    match list with
    | [] -> None
    | x::q ->
       let f = disj_all q in
       let g = conj_all x in
       maybe2 (fun x y -> Ou (x,y)) f g
  in
  disj_all ll

(* L ({ coord=36; colored=true }, R (Some Blue), L ({ coord=596; colored=true }, R (Some Red), R (Some Red))) *)
(* Renvoie la formule correspondant à la conjonction des contraintes de bsp_sat
 et de tout ses fils*)
let rec get_formule_complete nvar (bsp_sat : [`Red | `Blue] bsp_sat) : tseitinD * formule option =
  let (nvar2,formfils) =
    match bsp_sat with
    | R_sat (_,_,_) -> (nvar, None)
    | L_sat (_,_,l,r) ->
       let (nvar1,fl) = get_formule_complete nvar l in
       let (nvar2,fr) = get_formule_complete nvar1 r in
       let f = maybe2 (fun x y -> Et (x,y)) fl fr in
       (nvar2,f)
  in
  let form = get_formule_of_list_list (get_list_list_of_bsp_sat bsp_sat) in
  (* on met sous FNC form *)
  let fnc_form = maybe None (fun x -> Some (tseitin nvar2 x)) form in
  match fnc_form, formfils with
  | None, None -> (nvar2, None)
  | Some (n,a), None -> (n, Some a)
  | None, Some b -> (nvar2, Some b)
  | Some (n,a), Some b -> (n, Some (Et (a,b)))

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
let get_fnc_of_bsp (prof : int) (bsp : [`Red | `Blue] bsp) =
  let sat = bsp_sat_of_bsp2 bsp |> loop_sat prof in
  let sol = get_actual_sol sat in
  match sol with
    None -> None
  | Some sol ->
     let (_,f) = get_formule_complete (-1,Hashtbl.create 100) sat in
     maybe None (fun fnc -> Some (Et (fnc, sol))) f

let get_fnc_of_bsp_soluce (* (prof : int) *) (working_bsp : [`Red | `Blue] bsp) (linetree :  [`Red | `Blue] linetree)=
  let _ = bsp_sat_of_working_bsp working_bsp linetree (*|> loop_sat prof*) in
  failwith "Not Implemented"
   (* if not (check_all_lines sat)
   * then None
   * else snd (get_formule_complete (-1,Hashtbl.create 100) sat) *)
