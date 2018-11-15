open Couleur

type label = { coord : int; colored : bool; }
type bsp = R of Couleur.couleur option | L of label * bsp * bsp
type point = int * int

val string_of_bsp : bsp -> string
val random_bsp_naive :
  ?v:bool -> ?minsize:int -> ?start_larg:int -> ?start_haut:int -> int -> int -> int -> bsp
val change_color : ?v:bool -> bsp -> point -> bsp
val check_current : bsp -> bsp -> bool
val empty_copy_of_bsp : bsp -> bsp
val get_color_line : bsp -> couleur_l option

(* ################################################################################### *)
  
type linetree = Leef | Line of point * point * Couleur.couleur_l option * linetree * linetree
                             
val linetree_of_bsp : ?v:bool -> ?infx:int -> ?infy:int -> bsp -> int -> int -> linetree
