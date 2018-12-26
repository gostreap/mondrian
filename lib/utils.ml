open Graphics

(* Utilitaire *)
let maybe n f m =
  match m with
  | None -> n
  | Some s -> f s

let fmap f x =
  match x with
  | None -> None
  | Some x -> Some (f x)

let rec genl f l =
  if f > l
  then []
  else f :: (genl (f+1) l)

let maybe2 f x y =
  match x,y with
  | x, None -> x
  | None, x -> x
  | Some x, Some y -> Some (f x y)
                   
let clean_message () =
  set_color white;
  fill_rect 27 64 796 21

let print_message string =
  clean_message ();
  set_color black;
  moveto 30 65;
  set_font "-misc-dejavu sans mono-bold-r-normal--18-0-0-0-m-0-iso8859-1";
  draw_string string
