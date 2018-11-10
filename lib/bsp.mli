type label = { coord : int; colored : bool; }
type bsp = R of Graphics.color option | L of label * bsp * bsp
type point = int * int
type linetree = Leef | Line of point * point * Graphics.color option * linetree * linetree

val string_of_bsp : bsp -> string
val random_bsp_naive :
  ?v:bool -> ?minsize:int -> ?start_larg:int -> ?start_haut:int -> int -> int -> int -> bsp
val change_color : ?v:bool -> bsp -> point -> bsp
val compare_bsp : bsp -> bsp -> bool
val get_color_line : bsp -> Graphics.color option
val check_current : bsp -> bsp -> bool
val linetree_of_bsp : ?v:bool -> ?infx:int -> ?infy:int -> bsp -> int -> int -> linetree
