open Formule

(* Nouveu type formule encapsulant aussi les booléens *)
type formule' = F of formule | B of bool

(* type littéral *)
type lit = V of int | N of int

(* Transforme un littéral en formule *)
let to_f a =
  match a with
  | V x -> Var x
  | N x -> Neg x

(* redéfinitions opérateurs *)
let et a b =
  match a with
    B c -> if c then b else B false
  | F f ->
     match b with
       B c -> if c then a else B false
     | F g -> F (Et (f,g))

let ou a b = Ou (to_f a, to_f b)

let neg a =
  match a with
  | V x -> N x
  | N x -> V x

(* Auxiliaire de Tseitin *)
let rec tseitin' ?(nvar=(-1)) (f : formule') =
  match f with
  | B b ->
     if b
     then (nvar-1,(V nvar),F (Var nvar))
     else (nvar-1,(V nvar),F (Neg nvar))
  | F f ->
     match f with
       Var p -> nvar,V p,B true
     | Neg p -> nvar,N p,B true
     | Ou (a,b) ->
        let (x,la,a) = tseitin' ~nvar:nvar (F a) in
        let (x',lb,b) = tseitin' ~nvar:x (F b) in
        let q = V x' in
        let clause =
          let c1 = F (Ou (to_f (neg q),ou la lb)) in
          let c2 = F (ou (neg lb) q) in
          let c3 = F (ou (neg la) q) in
          et a (et b (et c3 (et c2 c1))) in
        x'-1,q,clause
     | Et (a,b) ->
        let (x,la,a) = tseitin' ~nvar:nvar (F a) in
        let (x',lb,b) = tseitin' ~nvar:x (F b) in
        let q = V x' in
        let clause =
          let c1 = F (Ou (to_f (neg la), (ou (neg lb) q))) in
          let c2 = F (ou (neg q) lb) in
          let c3 = F (ou (neg q) la) in
          et a (et b (et c1 (et c3 c2))) in
        x'-1,q,clause

(* Algorithme de Tseitin (1970), transforme une formule en FNC de manière efficace *)
let tseitin (f : formule) =
  let (_,l,f) = tseitin' (F f) in
  let l = to_f l in
  match f with
  | B b ->
     if b
     then l
     else Et (Var 0, Neg 0)
  | F f -> Et (l,f)
