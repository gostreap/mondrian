open Couleur
(* open Utils *)
open Bsp
open Formule
open Bsp_sat
open Gen_utils
open Tseitin

let choose coul n =
  match coul with
  | `Red -> (Var (2*n), Var(2*n+1))
  | `Green -> (Var (2*n), Neg (2*n+1))
  | `Blue -> (Neg (2*n), Var (2*n+1))

let generate_config (r,g,b) (rs,gs,bs) list =
  let red = get_n_tuples_in_list (r-rs) list in
  let mkredform = rev_map_ap (fun n -> choose `Red n) in
  let mkgreen r = get_n_tuples_in_list (g-gs) (get_compl r list) in
  let mkgreenform =  List.map (fun n -> choose `Green n) in
  let mkblue r g = get_n_tuples_in_list (b-bs) (get_compl (List.rev_append r g) list) in
  let mkblueform = rev_map_ap (fun n -> choose `Blue n) in
  let mkgreenblueform r g =
    let g' = mkgreenform g in
    List.map (fun b ->  mkredform r (mkblueform b g')) (mkblue r g)
  in
  concat_map_term (fun r -> concat_map_term (mkgreenblueform r) (mkgreen r)) red

(* Genère les triplets (x,y,z) vérifiants is_valid *)
let generate_triplet is_valid coul nadja rs gs bs =
  let rec genl f l =
    if f > l
    then []
    else f :: (genl (f+1) l) in
  let rl = genl rs (switch_coul_l (nadja/2) (nadja/2) (nadja/3) (nadja/3)
                                  (switch_coul nadja (nadja/2) (nadja/2)) coul) in
  let gl = genl gs (switch_coul_l (nadja/3) (nadja/2) (nadja/2) (nadja/3)
                                  (switch_coul (nadja/2) nadja (nadja/2)) coul) in
  let bl = genl bs (switch_coul_l (nadja/2) (nadja/3) (nadja/2) (nadja/3)
                                  (switch_coul (nadja/2) (nadja/2) nadja) coul) in
  let all_l = List.rev_map (fun x -> List.rev_map (fun y -> List.rev_map (fun z -> (x,y,z)) bl) gl ) rl in
  let filtered = List.filter is_valid (List.concat (List.concat all_l)) in
  (* print_liste filtered; *)
  match coul with
  | C `Red -> filtered
  | C `Green -> filtered
  | C `Blue -> filtered
  | _ -> filtered

let generate_all_config coul nadja rs gs bs list=
  let is_valid (r,g,b) =
    match coul with
    | Purple -> r+g+b = nadja && r = b && r > g
    | Yellow -> r+g+b = nadja && r = g && r > b
    | Cyan -> r+g+b = nadja && g = b && g > r
    | White -> r+g+b = nadja && r = g && g = b
    | C co ->
       match co with
       | `Red -> r+g+b = nadja && r > g && r > b
       | `Green -> r+g+b = nadja && g > r && g > b
       | `Blue -> r+g+b = nadja && b > r && b > g
  in
  let triplets = generate_triplet is_valid coul nadja rs gs bs in
  concat_map_term (fun x -> generate_config x (rs,gs,bs) list) triplets

(* Renvoie une liste de liste de string correspondant
   à une fnd satisfaisable ssi il existe un choix de
   coloration possible pour la ligne bsp_sat
   ATTENTION : seulement pour cette ligne, pas pour ces fils *)
let get_list_list_of_bsp_sat (ligne : couleur bsp_sat) : formule list list =
  let rs,gs,bs,list = get_adja_stat ligne in
  let size = rs + gs + bs + List.length list in
  match ligne with
  | R_sat (_,_,_) -> [[]]
  | L_sat (c,_,_,_) ->
     match c with
     | None -> []
     | Some c -> List.map (List.map (fun (x,y) -> (Et(Lit x, Lit y)))) (generate_all_config c size rs gs bs list)

(* Prend une liste de liste de formule et retourne une formule de la forme
 * (_ et _ et ... et _) ou (_ et _ et ... et _) ou ... ou (_ et _ et ... et _)*)
let get_formule_of_list_list (ll : formule list list) : formule =
  let conj_all (list : formule list) : formule =
    match list with
    | [] -> Vrai
    | x::q -> List.fold_left (fun acc x -> Et (acc,x)) x q
  in
  List.fold_left (fun acc x -> ou acc (conj_all x)) Faux ll

(* Renvoie la formule correspondant à la conjonction des contraintes de bsp_sat
 et de tout ses fils*)
let rec get_formule_complete nvar (bsp_sat : couleur bsp_sat) : formule =
  let formfils =
    match bsp_sat with
    | R_sat (_,_,_) -> Vrai
    | L_sat (_,_,l,r) ->
       et (get_formule_complete nvar l) (get_formule_complete nvar r)
  in
  let form = get_formule_of_list_list (get_list_list_of_bsp_sat bsp_sat) in
  (* on met sous FNC form *)
  let fnc_form =
    match form with
    | Faux -> Faux
    | Vrai -> Vrai
    | f -> tseitin nvar f in
  et fnc_form formfils

(* Renvoie la formule correspondant à la solution encodé dans bsp_sat *)
(* DÉJA SOUS FNC ET NIÉ*)
let rec get_actual_sol (orig : couleur bsp_sat) =
  match orig with
  | L_sat (_,s,l,r) ->
     if s
     then Vrai
     else ou (get_actual_sol l) (get_actual_sol r)
  | R_sat (i,s,c) ->
     if s then Vrai
     else
         match c with
         | None -> Vrai (*Si le rectangle est vide, la contraine est considéré vrai*)
         | Some co ->
            let (x,y) = choose co i in
            Ou (Lit (neg x), Lit (neg y))

(* Renvoie une fnc satisfaisable si et seulement si le bsp à plusieurs solution *)
let get_fnc_of_bsp (prof : int) (bsp : couleur bsp) =
  let sat = bsp_sat_of_bsp get_color_line bsp |> loop_sat prof in
  let sol = get_actual_sol sat in
  match sol with
  | Faux -> Faux
  | Vrai -> Vrai
  | solution -> et (get_formule_complete (ref (-1),Hashtbl.create 100) sat) solution

let get_fnc_of_bsp_soluce (prof : int) (working_bsp : couleur bsp) (linetree : couleur linetree)=
  let sat = bsp_sat_of_working_bsp working_bsp linetree |> loop_sat prof in
  if not (check_all_lines sat)
  then begin print_endline "Non check_all_lines"; Faux end
  else get_formule_complete (ref (-1),Hashtbl.create 100) sat 
