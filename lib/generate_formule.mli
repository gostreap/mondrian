open Bsp
open Couleur
open Formule

val get_fnc_of_bsp : int -> couleur bsp -> formule
val get_fnc_of_bsp_soluce : int -> couleur bsp -> couleur linetree -> formule
val generate_triplet : (int*int*int -> bool) -> couleur couleur_l
                       -> int -> int -> int -> int -> (int * int * int) list
val generate_config : int * int * int -> int * int * int -> int list -> (lit * lit) list list
val generate_all_config : couleur couleur_l ->
                          int -> int -> int -> int -> int list -> (lit * lit) list list
