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
let formule_list_of_bsp_sat_list (coul : couleur) (list : int list) =
  List.map (choose coul) list

let get_compl c list =
  List.filter (fun a -> not (List.mem a c)) list

let concat_term l = List.fold_left (List.rev_append) [] l

let generate_config (r,g,b) (rs,gs,bs) list =
  let red = get_n_tuples_in_list (r-rs) list in
  let mkredform = List.map (fun n -> choose Red n) in
  let green = List.map (fun x -> (get_n_tuples_in_list (g-gs) (get_compl x list))) red in
  let mkgreenform =  List.map (fun y -> List.map (fun n -> choose Green n) y) in
  let blue = List.map2 (fun r x -> List.map (fun g -> (get_n_tuples_in_list (b-bs) (get_compl (List.rev_append r g) list))) x) red green in
  let mkblueform = List.map (fun y -> List.map (fun z -> List.map (fun n -> choose Blue n) z) y ) in
  let greenblueform = List.map2 (fun x y -> List.map2 (fun g z -> List.map (fun b -> List.rev_append g b) z) (mkgreenform x) (mkblueform y)) green blue in
  let form = List.map2 (fun r x -> List.map (fun y -> List.map (fun z -> List.rev_append (mkredform r) z) y) x) red greenblueform in
  form |> concat_term |> concat_term

(* Genère les triplets (x,y,z) tels que (si coul = Red)
 - x+y+z = nadja
 - x > y && x > z
 - i >= is && i <= nadja \forall i \in {x,y,z}
*)
let generate_triplet coul nadja rs gs bs =
  let rec genl f l =
    if f > l
    then []
    else f :: (genl (f+1) l) in
  let xl = genl rs (switch_coul nadja (nadja/2) (nadja/2) coul) in
  let yl = genl gs (switch_coul (nadja/2) nadja (nadja/2) coul) in
  let zl = genl bs (switch_coul (nadja/2) (nadja/2) nadja coul) in
  let is_valid (x,y,z) = x+y+z = nadja && x > y && x > z in
  let all_l = List.map (fun x -> List.map (fun y -> List.map (fun z -> (x,y,z)) zl) yl ) xl in
  List.filter is_valid (List.concat (List.concat all_l))

let generate_all_config coul nadja rs gs bs list=
  let triplets = generate_triplet coul nadja rs gs bs in
  let configs = List.map (fun x -> generate_config x (rs,gs,bs) list) triplets in
  List.flatten configs

(* Renvoie une liste de liste de string correspondant
   à une fnd satisfaisable ssi il existe un choix de
   coloration possible pour la ligne bsp_sat
   ATTENTION : seulement pour cette ligne, pas pour ces fils *)
let get_list_list_of_bsp_sat (ligne : bsp_sat) : formule list list =
  let r,g,b, list = get_adja_stat ligne in
  let size = r + g + b + List.length list in
  let aux (coul:couleur) (list : int list list) : formule list list =
    List.map (fun u -> List.map (fun (x,y) -> Et (Lit x,Lit y)) (formule_list_of_bsp_sat_list coul u)) list
  in
  match ligne with
  | R_sat (_,_,_) -> []
  | L_sat (c,_,_,_) ->
     match c with
     | None -> []
     | Some c ->
        match c with
        | Yellow -> failwith ""
        | Cyan -> failwith ""
        | White -> failwith ""
     (* noter que, dans le cas Purple, size est pair *)
        | Purple -> failwith ""
        | C co ->
           match co with
           | Red ->
              if r > size/2 then []
              else aux Red (get_n_tuples_in_list (size/2+1-r) list)
           | Green ->
              if g > size/2 then []
              else aux Green (get_n_tuples_in_list (size/2+1-g) list)
           | Blue ->
              if b > size/2 then []
              else aux Blue (get_n_tuples_in_list (size/2+1-b) list)

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
  let fnc_form = maybe None (fun x -> Some (tseitin ~nvar:nvar2 x)) form in
  match fnc_form, formfils with
  | None, None -> (nvar2,None)
  | Some (n,a), None -> (n,Some a)
  | None, Some b -> (nvar2,Some b)
  | Some (n,a), Some b -> (n,Some (Et (a,b)))

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
  maybe None (fun fnc -> Some (Et (fnc,sol))) f
