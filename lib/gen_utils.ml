open Formule
open Utils

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

let get_compl c list =
  List.filter (fun a -> not (List.mem a c)) list

let rev_map_ap f l xs =
  let rec rmap_f accu = function
    | [] -> accu
    | a::l -> rmap_f (f a :: accu) l
  in rmap_f xs l

let concat_map_term f l = List.fold_left (fun acc x -> List.rev_append (f x) acc) [] l

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
