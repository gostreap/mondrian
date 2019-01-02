type lit = Var of int | Neg of int
type formule = | Faux | Vrai | Lit of lit | Et of formule * formule | Ou of formule * formule
val var : int -> formule
val non : int -> formule
val et : formule -> formule -> formule
val ou : formule -> formule -> formule
val string_of_formule : formule -> string
val print_formule : formule -> unit
val neg : lit -> lit
val get_v : lit -> int
