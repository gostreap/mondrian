open Bsp
open Formule

val get_fnc_of_bsp2 : int -> [`Blue | `Red ] bsp -> formule option
val get_fnc_of_bsp_soluce2 : [`Blue | `Red ] bsp -> [`Blue | `Red ] linetree -> formule option
