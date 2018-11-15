(* Utilitaire *)
let maybe n f m =
  match m with
  | None -> n
  | Some s -> f s
