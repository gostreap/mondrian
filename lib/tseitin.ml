open Formule

type formule' = F of formule | B of bool

let et a b =
  match a with
    B c -> if c then b else B false
  | F f ->
     match b with
       B c -> if c then a else B false
     | F g -> F (Et (f,g))

let ou a b =
  match a with
    B c -> if c then B true else b
  | F f ->
     match b with
       B c -> if c then B true else a
     | F g -> F (Ou (f,g))

let neg a =
  match a with
    B bool -> B (not bool)
  | F f ->
     match f with
     | Var x -> F (Neg x)
     | Neg x -> F (Var x)
     | _ -> failwith "neg'"

let rec tseitin' ?(nvar=(-1)) (f : formule') =
  match f with
  | B b ->
     if b
     then (nvar-1,F (Var nvar),F (Var nvar))
     else (nvar-1,F (Var nvar),F (Neg nvar))
  | F f ->
     match f with
       Var p -> nvar,F (Var p),B true
     | Neg p ->
        let (x,la,a) = tseitin' ~nvar:nvar (F (Var p)) in
        (x,neg la,a)
     | Ou (a,b) ->
        let (x,la,a) = tseitin' ~nvar:nvar (F a) in
        let (x',lb,b) = tseitin' ~nvar:x (F b) in
        let q = F (Var x') in
        let clause =
          let c1 = ou (neg q) (ou la lb) in
          let c2 = ou (neg lb) q in
          let c3 = ou (neg la) q in
          et a (et b (et c3 (et c2 c1))) in
        x'-1,q,clause
     | Et (a,b) ->
        let (x,la,a) = tseitin' ~nvar:nvar (F a) in
        let (x',lb,b) = tseitin' ~nvar:x (F b) in
        let q = F (Var x') in
        let clause =
          let c1 = ou (neg la) (ou (neg lb) q) in
          let c2 = ou (neg q) lb in
          let c3 = ou (neg q) la in
          et a (et b (et c1 (et c3 c2))) in
        x'-1,q,clause

let tseitin (f : formule) =
  let (_,l,f) = tseitin' (F f) in
  let l =
    match l with
    | F l -> l
    | _ -> failwith "" in
  match f with
  | B b ->
     if b
     then l
     else Et (Var 0, Neg 0)
  | F f -> Et (l,f)
