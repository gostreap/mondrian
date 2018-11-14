type formule =
    Var of int
  | Neg of int
  | Et of formule * formule
  | Ou of formule * formule
val string_of_formule : formule -> string
val print_formule : formule option -> unit
val same_var : formule -> formule -> bool
