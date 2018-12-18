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
    let add_all k v acc =
      let acc =
        if Ml.mem k acc
        then acc
        else Ml.add k [] acc in
      match v with
      | [] -> acc
      | x::xs -> List.fold_left (fun acc x -> addFront Ml.update x k acc) (addFront Ml.update x k acc) xs in
    Ml.fold add_all g Ml.empty

  (* Renvoi la liste des sommets triés par ordre décroissant de date de fin de traitement par un PP *)
  let ppc_dt g =
    let rec aux k vs ((res,already,date) as a )=
      if S.mem k already
      then a
      else
        let (res,already,date) =
          List.fold_left
            (fun ((res, already,date) as a) v ->
              match Ml.find_opt v g with
              | None ->
                 if S.mem v already
                 then a
                 else (Ml.add v date res, S.add v already,date+1)
              | Some x -> aux v x a (* Test de présence fait dans l'appel récursif *)
            ) (res,S.add k already,date) vs in
          (Ml.add k date res,already,date+1)
    in
    let (a,_,_) = Ml.fold aux g (Ml.empty,S.empty,0) in
    List.sort
      (fun x y -> compare (snd y) (snd x))
      (Seq.fold_left (fun acc x -> x::acc) [] (Ml.to_seq a))

  (* Un PP suivant un ordre particulier *)
  let ppc_final order g =
    let rec aux ((already,(res : literal list)) as a) k v =
      if S.mem k already
      then a
      else
        List.fold_left
          (fun ((already,res) as a) v ->
            match Ml.find_opt v g with
            | None ->
               if S.mem v already
               then a
               else (S.add v already, v:: res)
            | Some x -> aux a v x
          ) (S.add k already,k::res) v
    in
    let rec ppc_order ((already,(res : literal list list)) as a) order =
      match order with
      | [] -> res
      | (x,_)::xs ->
         if S.mem x already
         then ppc_order a xs
         else
           let (already,acc) = aux (already,[]) x (Ml.find x g) in
           ppc_order (already,acc::res) xs
    in
    ppc_order (S.empty,[]) order

  (* Algo de Kosaraju pour calculer les CFC *)
  let kosaraju_scc g =
    let order = ppc_dt g in
    let g = transpose g in
    ppc_final order g

  (* Vérifie que les CFC sont "valables", ie qu'un l'itéral n'est pas dans la même CFC que sont opposé *)
  let verif ls = List.for_all (fun x -> not (List.mem (L.mk_not x) ls)) ls
  let verify = List.for_all (fun v -> verif v)

    (* Assigne les variables selon les CFC https://en.wikipedia.org/wiki/2-satisfiability#Strongly_connected_components *)
  let mkAssign m gamma =
    let aux acc v =
      match v with
      | [] -> gamma
      | x::_ ->
         if S.mem (L.mk_not x) gamma
         then gamma
         else List.fold_left (fun acc x -> S.add x acc) gamma v
    in
    List.fold_left aux gamma (List.rev m) (* TODO remove rev *)

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
         if verify cfc
         then raise (Sat (mkAssign cfc env.gamma))
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
