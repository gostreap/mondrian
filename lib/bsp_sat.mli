open Couleur
open Bsp

type bsp_sat =
  | R_sat of int * bool * couleur (* id * secure * coul *)
  | L_sat of couleur_l option * bool * bsp_sat * bsp_sat (* coul * secure * left * right *)

val bsp_sat_of_bsp : bsp -> bsp_sat
val loop_sat: int -> bsp_sat -> bsp_sat
val string_of_bsp_sat : bsp_sat -> string
val secure_bsp_sat : bsp_sat -> bsp_sat
val get_adja_stat : bsp_sat -> (int * int * int * int list)
