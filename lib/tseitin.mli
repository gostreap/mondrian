open Formule

type tseitinD = int * (formule, int) Hashtbl.t

val tseitinDStart : tseitinD

val tseitin :
           ?nvar:tseitinD ->
           Formule.formule ->
           (int * (Formule.formule, int) Hashtbl.t) * Formule.formule
