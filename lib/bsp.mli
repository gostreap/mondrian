type bsp
type point
   
val string_of_bsp : bsp -> string

val random_bsp_naive : ?v:bool -> ?start_larg:int -> ?start_haut:int -> int -> int -> int -> bsp
                                              
val change_color : ?v:bool -> bsp -> point -> bsp
