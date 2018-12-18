(* Code extrait de:
   SAT-MICRO: petit mais costaud !
   par Sylvain Conchon, Johannes Kanig, Stéphane Lescuyer

   MODIFIÉ
*)

module type VARIABLES = sig
  type t
  val compare : t -> t -> int
end

module Make (V : VARIABLES) = struct

  type literal = bool * V.t

  module L = struct
    type t = literal
    let compare (b1,v1) (b2,v2) =
      let r = compare b1 b2 in
      if r = 0 then compare v1 v2
      else r

    let mk_not (b,v) = (not b, v)
  end

  module S = Set.Make(L)

  module Ml = Map.Make(L)

  module Mi = Map.Make(struct type t = int let compare = compare end)

  exception Unsat
  exception Sat of S.t

  type t = { gamma : S.t ; cl2 : L.t list list ; delta : L.t list list }

  let addFront update k v m =
    let maybe y =
      match y with
      | None -> Some [v]
      | Some xs -> Some (v :: xs) in
    update k maybe m

  (* Créer le graphe des implications https://en.wikipedia.org/wiki/Implication_graph *)
  let mk_implication_graph =
    let aux acc l = (* TODO Use tuples *)
      match l with
      | [a;b] ->
          addFront Ml.update (L.mk_not b) a (addFront Ml.update (L.mk_not a) b acc)
      | _ -> assert false in
    List.fold_left aux Ml.empty

  (* Transpose un graph: TODO améliorer *)
  let transpose g =
    let rec add_all k v acc =
      match v with
      | [] ->
         if Ml.mem k acc
         then acc
         else Ml.add k [] acc
      | x::xs -> add_all k xs (addFront Ml.update x k acc) in
    Ml.fold add_all g Ml.empty

  let maybe n f x =
    match x with
    | None -> n
    | Some x -> f x

  (* Renvoi la liste des sommets triés par ordre décroissant de date de fin de traitement par un PP *)
  let ppc_dt g =
    let res = ref Ml.empty in
    let rec aux k v ((already,date) as a )=
      if S.mem k already
      then a
      else
        begin
          let already = S.add k already in
          let alreadyD = List.fold_left
            (fun ((acc,d) as a) v ->
              maybe (if not (S.mem v already) then begin res := Ml.add v date !res; (S.add v acc, date+1) end else a) (fun x -> aux v x a) (Ml.find_opt v g)) (already,date) v in
          res := Ml.add k date !res ;
          alreadyD
        end
    in
    let _ = Ml.fold aux g (S.empty,0) in
    List.sort
      (fun x y -> compare (snd x) (snd y))
      (Seq.fold_left (fun acc x -> x::acc) [] (Ml.to_seq !res))

  (* Un PP suivant un ordre particulier *)
  let ppc_final order g =
    let res = ref Mi.empty in
    let rec aux count already k v =
      let f already k =
        res := addFront Mi.update count k !res ;
        S.add k already
      in
      if S.mem k already
      then already
      else
        let al = f already k in
        List.fold_left (fun acc v -> maybe (if not(S.mem v acc) then f acc v else acc) (aux count acc v) (Ml.find_opt v g)) al v
    in
    let rec ppc_order count already order =
      match order with
      | [] -> ()
      | (x,_)::xs ->
         if S.mem x already
         then ppc_order count already xs
         else
           begin
             let al = aux count already x (Ml.find x g) in
             ppc_order (count+1) al xs
           end
    in
    ppc_order 0 S.empty order;
    !res

  (* Algo de Kosaraju pour calculer les CFC *)
  let kosaraju_scc g =
    let order = ppc_dt g in
    let g = transpose g in
    ppc_final order g

  (* Vérifie que les CFC sont "valables", ie qu'un l'itéral n'est pas dans la même CFC que sont opposé *)
  let verif ls = List.for_all (fun x -> not (List.mem (L.mk_not x) ls)) ls
  let verify = Mi.for_all (fun _ v -> verif v)

    (* Assigne les variables selon les CFC https://en.wikipedia.org/wiki/2-satisfiability#Strongly_connected_components *)
  let mkAssign m gamma =
    let aux _ v gamma =
      match v with
      | [] -> gamma
      | x::_ ->
         if S.mem (L.mk_not x) gamma
         then gamma
         else List.fold_left (fun acc x -> S.add x acc) gamma v
    in
    Mi.fold aux m gamma

  (* SAT *)
  let rec assume env f =
    if S.mem f env.gamma then env
    else bcp { env with gamma = S.add f env.gamma}

  and bcp env =
    let aux env l f =
      try
        let l =
          List.filter
            (fun f ->
              if S.mem f env.gamma then raise Exit;
              not (S.mem (L.mk_not f) env.gamma)
            ) l
        in f l
      with Exit -> env in
    let start =
      List.fold_left
        (fun env l ->
          aux env l
            (fun l ->
              match l with
              | [] -> raise Unsat (* conflict *)
              | [f] -> assume env f
              | _ -> { env with cl2 = l :: env.cl2 } (* Ne peut pas être plus qu'une 2-clause *)
            ))
        {env with cl2 = []}
        env.cl2
    in
    List.fold_left
      (fun env l ->
        aux env l
          (fun l ->
            match l with
            | [] -> raise Unsat (* conflict *)
            | [f] -> assume env f
            | [_;_] -> { env with cl2 = l :: env.cl2 }
            | _ -> { env with delta = l :: env.delta }
          ))
      { start with delta = [] }
      env.delta

  let rec unsat env = try
      (* 3 clauses OU PLUS *)
      match env.delta with
      | [] ->
         let g = mk_implication_graph env.cl2 in
         let cfc = kosaraju_scc g in
         let assign = mkAssign cfc env.gamma in
         if verify cfc
         then raise (Sat assign)
         else raise Unsat
      | ([_] | []) :: _ -> assert false
      | (a :: xs) :: ys ->
         begin
           try
             unsat (assume {env with delta = ys} a)
           with Unsat -> ()
         end ;
         let nenv =
           match xs with
           | [_;_] -> {env with cl2 = xs::env.cl2}
           | _ -> {env with delta = xs::ys } in
        unsat (assume nenv (L.mk_not a))
    with Unsat -> ()

  let solve delta = try
      print_endline "SAT";
      print_endline (string_of_int (List.length delta));
      let b = bcp { gamma = S.empty ; cl2 = [];  delta } in
      print_endline (string_of_int (List.length b.delta));
      let c2,c3p = List.partition (fun x -> List.length x = 2) b.delta in
      let b =
        {b with
          cl2 = b.cl2 @ c2;
          delta = c3p
        } in
      unsat b ;
      None
    with
    | Sat g -> Some (S.elements g)
    | Unsat -> None

end
