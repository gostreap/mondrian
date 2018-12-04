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
