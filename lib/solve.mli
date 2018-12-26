open Bsp
open Bsp_sat
open Couleur
open Formule

type solution = Antilogie | Tautologie | L_sol of (bool * int) list
   
val print_maybe_other_sol : int -> couleur bsp -> unit

val print_maybe_other_sol_soluce : int -> couleur bsp_sat -> couleur bsp -> couleur linetree -> unit

val print_maybe_other_sol2 : int -> [`Red | `Blue] bsp -> unit
val print_maybe_other_sol_soluce2 : int -> [ `Blue | `Red ] bsp_sat -> [`Red | `Blue] bsp -> [`Red | `Blue] linetree -> unit

val check_all_secure_rect : 'a bsp_sat -> 'a bsp -> bool
val fill_one_rectangle : (int -> ([< `Blue | `Green | `Red > `Red] as 'a) bsp -> 'a linetree -> formule) -> ('a bsp -> (bool*int) list -> solution * 'a bsp) -> int -> 'a bsp_sat -> 'a bsp -> 'a linetree -> solution -> (solution * 'a bsp)
val fill_all_rectangle : (int -> ([< `Blue | `Green | `Red > `Red] as 'a) bsp -> 'a linetree -> formule) -> ('a bsp -> (bool*int) list -> solution * 'a bsp) -> int -> 'a bsp -> 'a bsp_sat -> 'a bsp -> 'a linetree -> solution -> 'a bsp

val color_first : couleur bsp -> (bool * int) list -> solution * couleur bsp
val color_first2 : [`Red | `Blue] bsp -> (bool * int) list -> solution * [`Red | `Blue] bsp
