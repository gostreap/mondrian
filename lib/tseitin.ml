open Formule
open Hashtbl

let ou_lit a b =
  match a, b with
  | None, None -> Faux
  | None, Some y -> Lit y
  | Some x, None -> Lit x
  | Some x, Some y -> Ou(Lit x, Lit y)

type tseitinD = int ref * (formule, int) t

(* Auxiliaire de Tseitin-Plaisted-Greenbaum *)
let rec tseitinPlaisted (((nvar,tabl) as t ): tseitinD) (f : formule) =
  match find_opt tabl f with
  | Some e ->
     (Some (Var e), Vrai)
  | None ->
     match f with
     | Faux -> None, Faux
     | Vrai -> None, Vrai
     | Lit x -> Some x, Vrai
     | Ou (a,b) ->
        let (la,a) = tseitinPlaisted t a in
        let (lb,b) = tseitinPlaisted t b in
        let q = Var !nvar in
        let clause =
          let c1 = Ou (Lit (neg q),ou_lit la lb) in
          et a (et b c1) in
        add tabl f !nvar;
        nvar := !nvar - 1 ;
        Some q,clause
     | Et (a,b) ->
        let (la,a) = tseitinPlaisted t a in
        let (lb,b) = tseitinPlaisted t b in
        let q = Var !nvar in
        let clause =
          let c2 = ou_lit (Some (neg q)) lb in
          let c3 = ou_lit (Some (neg q)) la in
          et a (et b (et c3 c2)) in
        add tabl f !nvar;
        nvar := !nvar - 1 ;
        Some q,clause

(* Algorithme de Tseitin (1970), transforme une formule en FNC de manière efficace *)
(* Utilise une Hashtabl pour éviter d'avoir vraiment plein de variables, à appeler avec une table vite à l'initialisation puis la réutiliser *)
let tseitin ((nvar,t) as n : tseitinD) (f : formule) =
  let (l,f') = tseitinPlaisted n f in
  let l' = match l with
      | None -> failwith "Erreur : tseitin -> literal null"
      | Some x -> Lit x in 
  let fnc = et l' f' in
  let l'' =
    match l with
    | None -> failwith "Erreur : tseitin -> literal null"
    | Some Neg x -> x
    | Some Var x -> x in
  add t f l'';
  nvar := !nvar - 1 ;
  fnc
