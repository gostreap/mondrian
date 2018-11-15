type formule =
  | Var of int
  | Neg of int
  | Et of formule * formule
  | Ou of formule * formule

let rec string_of_formule f =
  match f with
  | Var x -> string_of_int x
  | Neg x -> "Neg " ^ (string_of_int x)
  | Et (x,y) -> "(" ^ (string_of_formule x) ^ " Et " ^ (string_of_formule y) ^ ")"
  | Ou (x,y) -> "(" ^ (string_of_formule x) ^ " Ou " ^ (string_of_formule y) ^ ")"

let print_formule (f : formule option) =
  match f with
  | None -> print_endline "VRAI"
  | Some f -> print_endline (string_of_formule f)

(* Renvoie vrai ssi il x et y sont des Var et si x = y *)
let same_var x y =
  match x, y with
  | Var x, Var y -> x = y
  | _ -> false

let rec size_of_formule (f : formule) =
  match f with
  | Var _ -> 1
  | Neg _ -> 1
  | Et (a,b) -> size_of_formule a + size_of_formule b
  | Ou (a,b) -> size_of_formule a + size_of_formule b
