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
  | Red -> (Var (2*n),Var(2*n+1))
  | Green -> (Var (2*n),Neg (2*n+1))
  | Blue -> (Neg (2*n),Neg (2*n+1))

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
  let mkredform = rev_map_ap (fun n -> choose Red n) in
  let mkgreen r = get_n_tuples_in_list (g-gs) (get_compl r list) in
  let mkgreenform =  List.map (fun n -> choose Green n) in
  let mkblue r g = get_n_tuples_in_list (b-bs) (get_compl (List.rev_append r g) list) in
  let mkblueform = rev_map_ap (fun n -> choose Blue n) in
  let mkgreenblueform r =
    List.map
      (fun g -> let g' = mkgreenform g in List.map (fun b -> mkblueform b g') (mkblue r g))
      (mkgreen r) in
  concat_map_term (fun r -> concat_map_term (fun y -> List.map (fun z -> mkredform r z) y) (mkgreenblueform r)) red
  
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
  List.filter is_valid (List.concat (List.concat all_l))

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
  let aux (list : (lit*lit) list list) : formule list list =
    List.map (fun listll -> List.map (fun (x,y) -> Et(Lit x, Lit y)) listll) list
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
           | Red ->
              if rs > size/2 then []
              else aux (generate_all_config (C Red) size rs gs bs list)
           | Green ->
              if gs > size/2 then []
              else aux (generate_all_config (C Green) size rs gs bs list)
           | Blue ->
              if bs > size/2 then []
              else aux (generate_all_config (C Blue) size rs gs bs list)

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
       match f with
       | None -> g
       | Some a ->
          match g with
          | None -> Some a
          | Some b -> Some (Ou (a,b))
  in
  let f = disj_all ll in
  f

(* Renvoie la formule correspondant à la conjonction des contraintes de bsp_sat
 et de tout ses fils*)
let rec get_formule_complete ?(nvar=(-1)) (bsp_sat : bsp_sat) : int * formule option =
  let (nvar2,formfils) =
    match bsp_sat with
    | R_sat (_,_,_) -> (nvar, None)
    | L_sat (_,_,l,r) ->
       let (nvar1,fl) = get_formule_complete ~nvar:nvar l in
       let (nvar2,fr) = get_formule_complete ~nvar:nvar1 r in
       let f = match fl, fr with
         | None, None -> None
         | Some a, None -> Some a
         | None, Some b -> Some b
         | Some a, Some b -> Some (Et (a,b)) in
       (nvar2,f)
  in
  let form = get_formule_of_list_list (get_list_list_of_bsp_sat bsp_sat) in
  (* on met sous FNC form *)
  (* let fnc_form = maybe None (fun x -> Some (tseitin ~nvar:nvar2 x)) form in *)
  match form, formfils with
  | None, None -> (nvar2,None)
  | Some (a), None -> (nvar2, Some a)
  | None, Some b -> (nvar2,Some b)
  | Some (a), Some b -> (nvar2, Some (Et (a,b)))

(* Renvoie la formule correspondant à la solution encodé dans bsp_sat*)
(* DÉJA SOUS FNC ET NIÉ*)
let rec get_actual_sol (orig : bsp_sat) =
  match orig with
  | R_sat (i,_,x) ->
     let x,y = choose x i
     in Et (Lit x, Lit y)
  | L_sat (_,_,l,r) -> Ou (get_actual_sol l, get_actual_sol r)

(* Renvoie une fnc satisfaisable si et seulement si le bsp à plusieurs solution*)
let get_fnc_of_bsp (prof : int) (bsp : bsp) =
  let sat = bsp_sat_of_bsp bsp |> loop_sat prof in
  let (_,f) = get_formule_complete sat in
  let sol = get_actual_sol sat in
  maybe None (fun fnc -> Some (snd (tseitin (Et (fnc, sol))))) f
