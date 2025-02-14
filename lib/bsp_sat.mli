open Couleur

type 'a bsp_sat =
  | R_sat of int * bool * 'a option (* id * secure * coul *)
  | L_sat of 'a couleur_l option * bool * 'a bsp_sat * 'a bsp_sat (* coul * secure * left * right *)

val bsp_sat_of_bsp : ('a Bsp.bsp -> 'a couleur_l option) -> 'a Bsp.bsp -> 'a bsp_sat
val bsp_sat_of_working_bsp : ([< `Blue | `Green | `Red ] as 'a) Bsp.bsp ->
                             'a Bsp.linetree -> 'a bsp_sat
val loop_sat: int -> ([< `Blue | `Green | `Red ] as 'a) bsp_sat -> 'a bsp_sat
val string_of_bsp_sat : [< `Red | `Green | `Blue] bsp_sat -> string
val secure_bsp_sat : ([< `Blue | `Green | `Red ] as 'a) bsp_sat -> 'a bsp_sat
val get_adja_stat : [< `Blue | `Green | `Red ] bsp_sat -> (int * int * int * int list)
val check_all_lines :  [< `Red | `Green | `Blue] bsp_sat -> bool
