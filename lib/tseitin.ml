open Formule
open Hashtbl

(* Nouveu type formule encapsulant aussi les booléens *)
type formule' = F of formule | B of bool

(* redéfinitions opérateurs *)
let et a b =
  match a with
    B c -> if c then b else B false
  | F f ->
     match b with
       B c -> if c then a else B false
     | F g -> F (Et (f,g))

let ou a b = Ou (Lit a, Lit b)

type tseitinD = int ref * (formule, int) t

(* Auxiliaire de Tseitin-Plaisted-Greenbaum *)
let rec tseitinPlaisted (((nvar,tabl) as t ): tseitinD) (f : formule) =
  match find_opt tabl f with
  | Some e ->
     (Var e, B true)
  | None ->
     match f with
     | Lit x -> x,B true
     | Ou (a,b) ->
        let (la,a) =  tseitinPlaisted t a in
        let (lb,b) = tseitinPlaisted t b in
        let q = Var !nvar in
        let clause =
          let c1 = F (Ou (Lit (neg q),ou la lb)) in
          et a (et b c1) in
        add tabl f !nvar;
        nvar := !nvar - 1 ;
        q,clause
     | Et (a,b) ->
        let (la,a) = tseitinPlaisted t a in
        let (lb,b) = tseitinPlaisted t b in
        let q = Var !nvar in
        let clause =
          let c2 = F (ou (neg q) lb) in
          let c3 = F (ou (neg q) la) in
          et a (et b (et c3 c2)) in
        add tabl f !nvar;
        nvar := !nvar - 1 ;
        q,clause

(* Algorithme de Tseitin (1970), transforme une formule en FNC de manière efficace *)
(* Utilise une Hashtabl pour éviter d'avoir vraiment plein de variables, à appeler avec une table vite à l'initialisation puis la réutiliser *)
let tseitin ((nvar,t) as n : tseitinD) (f : formule) =
  let (l,f') = tseitinPlaisted n f in
  let l' = Lit l in
  let fnc =
    match f' with
    | B b ->
       if b
       then l'
       else Et (var 0, non 0)
    | F f -> Et (l',f) in
  let l'' =
    match l with
    | Neg x -> x
    | Var x -> x in
  add t f l'';
  nvar := !nvar - 1 ;
  fnc
