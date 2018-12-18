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

  (* Transpose un graph *)
  let transpose g =
    let rec add_all k v acc =
      match v with
      | [] -> Ml.add k [] acc
      | x::xs -> add_all k xs (addFront Ml.update x k acc) in
    Ml.fold add_all g Ml.empty

  let maybe n f x =
    match x with
    | None -> n
    | Some x -> f x

  (* Renvoit la liste des sommets triés par ordre décroissant de date de fin de traitement par un PP *)
  let ppc_dt g =
    let res = ref Ml.empty in
    let date = ref 0 in
    let rec aux k v =
      let f k =
        res := Ml.add k !date !res ;
        date := !date + 1
      in
      if Ml.mem k !res
      then ()
      else
        begin
          f k ;
          List.iter
            (fun v ->
              maybe (f v) (aux v) (Ml.find_opt v g)) v;
        end
    in
    Ml.iter aux g;
    List.sort
      (fun x y -> compare (snd x) (snd y))
      (Seq.fold_left (fun acc x -> x::acc) [] (Ml.to_seq !res))

  (* Un PP suivant un ordre particulier *)
  let ppc_final order g =
    let res = ref Mi.empty in
    let already = ref S.empty in
    let count = ref 0 in
    let rec aux k v =
      let f k =
        already := S.add k !already;
        res := addFront Mi.update !count k !res
      in
      if S.mem k !already
      then ()
      else
        begin
          f k;
          List.iter (fun v -> maybe (f v) (aux v) (Ml.find_opt v g)) v
        end
    in
    let rec ppc_order order =
      match order with
      | [] -> ()
      | (x,_)::xs ->
         if S.mem x !already
         then ppc_order xs
         else
           begin
             aux x (Ml.find x g) ;
             count := !count + 1
           end
    in
    ppc_order order;
    !res

  (* Algo de Kosaraju pour calculer les CFC *)
  let kosaraju_scc g =
    let order = ppc_dt g in
    let g = transpose g in
    ppc_final order g

  (* Vérifie que les CFC sont "valables", ie qu'un l'itéral n'est pas dans la même CFC que sont opposé *)
  let verify m =
    let verif ls = List.for_all (fun x -> not (List.mem (L.mk_not x) ls)) ls in
    Mi.fold (fun _ v acc -> verif v && acc) m true

  (* SAT *)
  let rec assume env f =
    if S.mem f env.gamma then env
    else bcp { env with gamma = S.add f env.gamma}

  and bcp env =
    (* TODO facto + décider dans quel ordre: start doit travailler sur les 2 cl ou sur delta ? *)
    let start =
      List.fold_left
        (fun env l ->
          try
            let l =
              List.filter
                (fun f ->
                  if S.mem f env.gamma then raise Exit;
                  not (S.mem (L.mk_not f) env.gamma)
                ) l
            in
            match l with
            | [] -> raise Unsat (* conflict *)
            | [f] -> assume env f
            | _ -> { env with cl2 = l :: env.cl2 } (* Ne peut pas être plus qu'une 2-clause *)
          with Exit -> env)
        {env with cl2 = []}
        env.cl2
    in
    List.fold_left
      (fun env l ->
        try
          let l = List.filter
              (fun f ->
                 if S.mem f env.gamma then raise Exit;
                 not (S.mem (L.mk_not f) env.gamma)
              ) l
          in
          match l with
          | [] -> raise Unsat (* conflict *)
          | [f] -> assume env f
          | [_;_] -> { env with cl2 = l :: env.cl2 }
          | _ -> { env with delta = l :: env.delta }
        with Exit -> env)
      { start with delta = [] }
      env.delta

  let rec unsat env = try
      (* 3 clauses *)
      match env.delta with
      | [] ->
         let g = mk_implication_graph env.cl2 in
         if verify (kosaraju_scc g)
         then raise (Sat S.empty)
         else raise Unsat
      | ([_] | []) :: _ -> assert false
      | (a :: xs) :: ys ->
         begin
           try
             unsat (assume {env with delta = ys} a)
           with Unsat -> ()
         end ;
        unsat (assume {env with cl2 = xs::env.cl2} (L.mk_not a))
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
      print_endline "cc";
      unsat b ;
      None
    with
    | Sat g -> Some (S.elements g)
    | Unsat -> None

end
