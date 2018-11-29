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

type tseitinD = int * (formule, int) t

(* Auxiliaire de Tseitin-Plaisted-Greenbaum *)
let rec tseitinPlaisted (((_,tabl) as t ): tseitinD) (f : formule) =
  match find_opt tabl f with
  | Some e ->
     (t,Var e, B true)
  | None ->
     match f with
     | Lit x -> t,x,B true
     | Ou (a,b) ->
        let (x,la,a) =  tseitinPlaisted t a in
        let (x',lb,b) = tseitinPlaisted x b in
        let q = Var (fst x') in
        let clause =
          let c1 = F (Ou (Lit (neg q),ou la lb)) in
          et a (et b c1) in
        add (snd x') f (fst x');
        ((fst x')-1,snd x'),q,clause
     | Et (a,b) ->
        let (x,la,a) = tseitinPlaisted t a in
        let (x',lb,b) = tseitinPlaisted x b in
        let q = Var (fst x') in
        let clause =
          let c2 = F (ou (neg q) lb) in
          let c3 = F (ou (neg q) la) in
          et a (et b (et c3 c2)) in
        add (snd x') f (fst x');
        ((fst x')-1,snd x'),q,clause

(* Algorithme de Tseitin (1970), transforme une formule en FNC de manière efficace *)
(* Utilise une Hashtabl pour éviter d'avoir vraiment plein de variables, à appeler avec une table vite à l'initialisation puis la réutiliser *)
let tseitin nvar (f : formule) =
  let ((n,t),l,f') = tseitinPlaisted nvar f in
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
  ((n-1,t),fnc)
