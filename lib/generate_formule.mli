open Bsp
open Formule

val get_fnc_of_bsp : int -> bsp -> formule option
val generate_triplet : (int*int*int -> bool) -> Couleur.couleur_l
                       -> int -> int -> int -> int -> (int * int * int) list
val generate_config : int * int * int -> int * int * int -> int list -> (lit * lit) list list
val generate_all_config : Couleur.couleur_l ->
                          int -> int -> int -> int -> int list -> (lit * lit) list list
