open Bsp
open Couleur

val is_uniq : int -> couleur bsp -> bool
val print_maybe_other_sol : int -> couleur bsp -> unit
val print_maybe_other_sol_soluce : couleur bsp -> couleur linetree -> unit
val fill_one_rectangle : couleur bsp -> couleur linetree -> couleur bsp
  
val is_uniq2 : int -> [`Red | `Blue] bsp -> bool
val print_maybe_other_sol2 : int -> [`Red | `Blue] bsp -> unit
val print_maybe_other_sol_soluce2 : [`Red | `Blue] bsp -> [`Red | `Blue] linetree -> unit
val fill_one_rectangle2 : [`Red | `Blue] bsp -> [`Red | `Blue] linetree -> [`Red | `Blue] bsp
