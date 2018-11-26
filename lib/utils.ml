(* Utilitaire *)
let maybe n f m =
  match m with
  | None -> n
  | Some s -> f s

let rec genl f l =
  if f > l
  then []
  else f :: (genl (f+1) l)

let maybe2 f x y =
  match x,y with
  | x, None -> x
  | None, x -> x
  | Some x, Some y -> Some (f x y)
