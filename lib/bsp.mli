type label = { coord : int; colored : bool; }
type bsp = R of Couleur.couleur option | L of label * bsp * bsp
type point = int * int
type linetree = Leef | Line of point * point * Couleur.couleur_l option * linetree * linetree
type formule = Var of int | Neg of int | Et of formule * formule | Ou of formule * formule

val string_of_bsp : bsp -> string
val random_bsp_naive :
  ?v:bool -> ?minsize:int -> ?start_larg:int -> ?start_haut:int -> int -> int -> int -> bsp
val change_color : ?v:bool -> bsp -> point -> bsp
val get_color_line : bsp -> Couleur.couleur_l option
val check_current : bsp -> bsp -> bool
val linetree_of_bsp : ?v:bool -> ?infx:int -> ?infy:int -> bsp -> int -> int -> linetree
val empty_copy_of_bsp : bsp -> bsp

type bsp_sat = R_sat of int * bool * Couleur.couleur
             | L_sat of Couleur.couleur_l option * bool * bsp_sat * bsp_sat
val secure_bsp_sat : bsp_sat -> bsp_sat
val string_of_bsp_sat : bsp_sat -> string
val bsp_sat_of_bsp : bsp -> bsp_sat
val loop_sat : int -> bsp_sat -> bsp_sat
val get_adja_stat : bsp_sat -> int * int * bsp_sat list
val get_n_tuples_in_list : int -> bsp_sat list -> bsp_sat list list
val get_fnc_of_bsp : int -> bsp -> formule option
val print_formule  : formule option -> unit
val list_of_fnc : formule -> (bool*int) list list

val is_uniq : int -> bsp -> bool
val print_maybe_other_sol : int -> bsp -> unit
val tseitin : formule -> formule
