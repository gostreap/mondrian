open Couleur

type label = { coord : int; colored : bool; }
type 'a bsp = R of 'a option | L of label * 'a bsp * 'a bsp
type point = int * int

val machinestring_of_bsp : [< `Red | `Green | `Blue] bsp -> string
val rand_two_coul : unit -> [`Blue | `Red ]
val rand_three_coul : unit -> [`Blue | `Green | `Red ]
val random_bsp_naive :
  ?minsize:int ->
  (unit -> ([< `Blue | `Green | `Red ] as 'a)) ->
  int -> int -> int -> 'a bsp
val change_color : (([< `Blue | `Green | `Red ] as 'a) option -> 'a option) -> 'a bsp -> point -> 'a bsp
val check_current : ([< `Blue | `Green | `Red ] as 'a) bsp -> 'a bsp -> bool
val empty_copy_of_bsp : 'a bsp -> 'b bsp
val get_color_line : couleur bsp -> couleur couleur_l option
val get_color_line2 : [`Red | `Blue] bsp -> [`Red | `Blue] couleur_l option

(* ################################################################################### *)

type 'a linetree = Leef | Line of point * point * 'a Couleur.couleur_l option * 'a linetree * 'a linetree

val linetree_of_bsp : (([< `Blue | `Green | `Red ] as 'a) bsp -> 'a couleur_l option) -> 'a bsp -> int -> int -> 'a linetree

val change_coul_with_id : 'a bsp -> int -> 'a -> 'a bsp

val tryred :  (([< `Blue | `Green | `Red > `Red ] as 'a) bsp) -> (bool*('a bsp))
