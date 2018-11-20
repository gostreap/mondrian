(* Utilitaire *)
let maybe n f m =
  match m with
  | None -> n
  | Some s -> f s

let rec genl f l =
  if f > l
  then []
  else f :: (genl (f+1) l)
