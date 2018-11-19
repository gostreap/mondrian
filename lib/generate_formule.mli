open Bsp
open Formule

val get_fnc_of_bsp : int -> bsp -> formule option
val generate_config : int * int * int -> int * int * int -> int list -> (lit * lit) list list
val generate_all_config : Couleur.couleur -> int -> int -> int -> int -> int list -> (lit * lit) list list
