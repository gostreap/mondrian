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
  | Some Red -> Some (Var (2*n), Var(2*n+1))
  | Some Green -> Some (Var (2*n), Neg (2*n+1))
  | Some Blue -> Some (Neg (2*n), Neg (2*n+1))

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

let generate_config (r,g,b) (rs,gs,bs) list =
  let red = get_n_tuples_in_list (r-rs) list in
  let mkredform = rev_map_ap (fun n -> choose (Some Red) n) in
  let mkgreen r = get_n_tuples_in_list (g-gs) (get_compl r list) in
  let mkgreenform =  List.map (fun n -> choose (Some Green) n) in
  let mkblue r g = get_n_tuples_in_list (b-bs) (get_compl (List.rev_append r g) list) in
  let mkblueform = rev_map_ap (fun n -> choose (Some Blue) n) in
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
  match coul with
  | C Red -> (nadja/2+1,gs,bs)::List.filter is_valid (List.concat (List.concat all_l))
  | C Green -> (rs,nadja/2+1,bs)::List.filter is_valid (List.concat (List.concat all_l))
  | C Blue -> (rs,gs,nadja/2+1)::List.filter is_valid (List.concat (List.concat all_l))
  | _ ->List.filter is_valid (List.concat (List.concat all_l))

let generate_all_config coul nadja rs gs bs list=
  let is_valid (r,g,b) =
    match coul with
    | Purple -> r+g+b = nadja && r = b && r > g
    | Yellow -> r+g+b = nadja && r = g && r > b
    | Cyan -> r+g+b = nadja && g = b && g > r
    | White -> r+g+b = nadja && r = g && g = b
    | C co ->
       match co with
       | Red -> r+g+b = nadja && r > g && r > b
       | Green -> r+g+b = nadja && g > r && g > b
       | Blue -> r+g+b = nadja && b > r && b > g
  in
  let triplets = generate_triplet is_valid coul nadja rs gs bs in
  concat_map_term (fun x -> generate_config x (rs,gs,bs) list) triplets

(* Renvoie une liste de liste de string correspondant
   à une fnd satisfaisable ssi il existe un choix de
   coloration possible pour la ligne bsp_sat
   ATTENTION : seulement pour cette ligne, pas pour ces fils *)
let get_list_list_of_bsp_sat (ligne : bsp_sat) : formule list list =
  let rs,gs,bs,list = get_adja_stat ligne in
  let size = rs + gs + bs + List.length list in
  let fusion (tuple : (lit * lit) option) : formule option =
    match tuple with
    | None -> None
    | Some (x,y) -> Some (Et(Lit x, Lit y))
  in
  let filter_None (f : formule option) : bool =
    match f with
    | None -> false
    | _ -> true
  in
  let form_of_form_option (sf : formule option) : formule =
    match sf with
    | None -> failwith "form_of_form_option : unexpected None"
    | Some f -> f
  in
  let aux (list : (lit*lit) option list list) : formule list list =
    let ll = List.map (fun listll -> List.map (fun t -> fusion t) listll) list in
    let llclean = List.map (fun l -> List.filter filter_None l) ll in
    List.map (fun l -> List.map (fun sf -> form_of_form_option sf) l) llclean
  in
  match ligne with
  | R_sat (_,_,_) -> []
  | L_sat (c,_,_,_) ->
     match c with
     | None -> []
     | Some c ->
        match c with
        | Purple -> aux (generate_all_config Purple size rs gs bs list)
        | Yellow -> aux (generate_all_config Yellow size rs gs bs list)
        | Cyan -> aux (generate_all_config Cyan size rs gs bs list)
        | White -> aux (generate_all_config White size rs gs bs list)
     (* noter que, dans le cas Purple, size est pair *)
        | C co ->
           match co with
           | Red -> aux (generate_all_config (C Red) size rs gs bs list)
           | Green -> aux (generate_all_config (C Green) size rs gs bs list)
           | Blue -> aux (generate_all_config (C Blue) size rs gs bs list)

(* Prend une liste de liste de formule et retourne une formule de la forme
 * (_ et _ et ... et _) ou (_ et _ et ... et _) ou ... ou (_ et _ et ... et _)*)
let get_formule_of_list_list (ll : formule list list) : formule option =
  let rec conj_all (list : formule list) : formule option =
    match list with
    | [] -> None
    | x::q ->
       let f = conj_all q in
       match f with
       | None -> Some x
       | Some a -> Some (Et (x,a))
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
let rec get_formule_complete nvar (bsp_sat : bsp_sat) : tseitinD * formule option =
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
let rec get_actual_sol (orig : bsp_sat) =
  match orig with
  | R_sat (i,s,c) ->
     if s then None
     else
         begin
         let tuple = choose c i
         in
         match tuple with
         | None -> None
         | Some (x,y) -> Some (Ou (Lit (neg x), Lit (neg y)))
         end
  | L_sat (_,s,l,r) ->
     if s
     then None
     else maybe2 (fun x y -> Ou (x,y)) (get_actual_sol l) (get_actual_sol r)

(* Renvoie une fnc satisfaisable si et seulement si le bsp à plusieurs solution *)
let get_fnc_of_bsp (prof : int) (bsp : bsp) =
  let sat = bsp_sat_of_bsp bsp |> loop_sat prof in
  let sol = get_actual_sol sat in
  match sol with
    None -> None
  | Some sol ->
     let (_,f) = get_formule_complete (-1,Hashtbl.create 100) sat in
     maybe None (fun fnc -> Some (Et (fnc, sol))) f

let get_fnc_of_bsp_soluce (* (prof : int) *) (working_bsp : bsp) (linetree : linetree)=
  let sat = bsp_sat_of_working_bsp working_bsp linetree (*|> loop_sat prof*) in
  if not (check_all_lines sat) then
      begin
          (* print_endline "NON CHECK !"; *)
          None
      end
  else
      begin
          (* print_endline "CHECK !"; *)
          let (_,f) = get_formule_complete sat in
          match f with
          | None -> None
          | _ -> f
      end
