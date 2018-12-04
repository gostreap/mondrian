open Couleur

type label = { coord : int; colored : bool; }
type 'a bsp = R of 'a option | L of label * 'a bsp * 'a bsp
type point = int * int

val string_of_bsp : couleur bsp -> string
val machinestring_of_bsp : couleur bsp -> string
val rand_two_coul : unit -> [`Blue | `Red ]
val rand_three_coul : unit -> [`Blue | `Green | `Red ]
val random_bsp_naive :
  ?v:bool ->
  ?minsize:int ->
  ?start_larg:int ->
  ?start_haut:int ->
  int ->
  int ->
  int -> (unit -> ([< `Blue | `Green | `Red ] as 'a)) -> 'a bsp
val change_color : ?v:bool -> couleur bsp -> point -> couleur bsp
val check_current : couleur bsp -> couleur bsp -> bool
val empty_copy_of_bsp : 'a bsp -> 'b bsp
val get_color_line : couleur bsp -> couleur couleur_l option
val get_color_line2 : [`Red | `Blue] bsp -> [`Red | `Blue] couleur_l option

(* ################################################################################### *)

type 'a linetree = Leef | Line of point * point * 'a Couleur.couleur_l option * 'a linetree * 'a linetree

val linetree_of_bsp : ?v:bool -> ?infx:int -> ?infy:int -> (([< `Blue | `Green | `Red ] as 'a) bsp -> 'a couleur_l option) -> 'a bsp -> int -> int -> 'a linetree
