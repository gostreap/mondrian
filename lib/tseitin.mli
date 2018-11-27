open Formule

type tseitinD = int * (formule, int) Hashtbl.t

val tseitin :
           tseitinD ->
           Formule.formule ->
           (int * (Formule.formule, int) Hashtbl.t) * Formule.formule
