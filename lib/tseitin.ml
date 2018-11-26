open Formule
open Hashtbl

(* Nouveu type formule encapsulant aussi les booléens *)
type formule' = F of formule | B of bool

(* )type ('a,'b) either = L of 'a | R of 'b *)

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

(* Auxiliaire de Tseitin *)
let rec tseitin' (((_,tabl) as t ): tseitinD) (f : formule) =
  match find_opt tabl f with
  | Some e ->
     (t,Var e, B true)
  | None ->
     match f with
     | Lit x -> t,x,B true
     | Ou (a,b) ->
        let (x,la,a) =  tseitin' t a in
        let (x',lb,b) = tseitin' x b in
        let q = Var (fst x') in
        let clause =
          let c1 = F (Ou (Lit (neg q),ou la lb)) in
          let c2 = F (ou (neg lb) q) in
          let c3 = F (ou (neg la) q) in
          et a (et b (et c3 (et c2 c1))) in
        add (snd x') f (fst x');
        ((fst x')-1,snd x'),q,clause
     | Et (a,b) ->
        let (x,la,a) = tseitin' t a in
        let (x',lb,b) = tseitin' x b in
        let q = Var (fst x') in
        let clause =
          let c1 = F (Ou (Lit (neg la), (ou (neg lb) q))) in
          let c2 = F (ou (neg q) lb) in
          let c3 = F (ou (neg q) la) in
          et a (et b (et c1 (et c3 c2))) in
        add (snd x') f (fst x');
        ((fst x')-1,snd x'),q,clause

(* Algorithme de Tseitin (1970), transforme une formule en FNC de manière efficace *)
let tseitin nvar (f : formule) =
  let ((n,t),l,f') = tseitin' nvar f in
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
