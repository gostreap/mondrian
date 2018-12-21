open Bsp
open Bsp_sat
open Couleur
open Formule

val is_uniq : int -> couleur bsp -> bool
val print_maybe_other_sol : int -> couleur bsp -> unit

val print_maybe_other_sol_soluce : int -> couleur bsp_sat -> couleur bsp -> couleur linetree -> unit
  
val is_uniq2 : int -> [`Red | `Blue] bsp -> bool
val print_maybe_other_sol2 : int -> [`Red | `Blue] bsp -> unit
val print_maybe_other_sol_soluce2 : int -> [ `Blue | `Red ] bsp_sat -> [`Red | `Blue] bsp -> [`Red | `Blue] linetree -> unit

val check_all_secure_rect : 'a bsp_sat -> 'a bsp -> bool
val fill_one_rectangle : (int -> ([< `Blue | `Green | `Red > `Red] as 'a) bsp -> 'a linetree -> formule option) -> ('a bsp -> (bool*int) list -> 'a bsp) -> int -> 'a bsp_sat -> 'a bsp -> 'a linetree -> 'a bsp

val color_first : couleur bsp -> (bool * int) list -> couleur bsp
val color_first2 : [`Red | `Blue] bsp -> (bool * int) list -> [`Red | `Blue] bsp

