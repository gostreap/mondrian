type bsp
type point
   
val string_of_bsp : bsp -> string

val random_bsp_naive : int -> int -> int -> bsp
                                              
val change_color : bsp -> point -> bsp
