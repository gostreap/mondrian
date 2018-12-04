open Bsp
open Formule

val get_fnc_of_bsp2 : int -> [`Blue | `Red ] bsp -> formule option
val get_fnc_of_bsp_soluce2 : [`Blue | `Red ] bsp -> [`Blue | `Red ] linetree -> formule option
val generate_tuple : (int*int -> bool) -> int -> int -> int -> (int * int) list
val generate_config : int * int -> int * int -> int list -> Formule.lit option list list
val generate_all_config :
  [ `Blue | `Red ] Couleur.couleur_l ->
  int -> int -> int -> int list -> Formule.lit option list list
