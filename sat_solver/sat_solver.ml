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

  exception Unsat
  exception Sat of S.t

  type t = { gamma : S.t ; cl2 : L.t list list ; delta : L.t list list }

  let addFront update k v m =
    let maybe y =
      match y with
      | None -> Some [v]
      | Some xs -> Some (v :: xs) in
    update k maybe m

  let mk_if_not_here k m =
    let f z =
      match z with
      | None -> Some []
      | Some _ -> z in
    Ml.update k f m

  (* Créer le graphe des implications https://en.wikipedia.org/wiki/Implication_graph *)
  let mk_implication_graph =
    let aux acc l = (* TODO Use tuples *)
      match l with
      | [a;b] ->
         let acc = mk_if_not_here b (mk_if_not_here a acc) in
         addFront Ml.update (L.mk_not b) a (addFront Ml.update (L.mk_not a) b acc)
      | _ -> assert false in
    List.fold_left aux Ml.empty

  (* Transpose un graph: TODO améliorer ? *)
  let transpose g =
    let add_all k v acc =
      let acc = mk_if_not_here k acc in
      match v with
      | [] -> acc
      | x::xs ->
         List.fold_left (fun acc x -> addFront Ml.update x k acc) (addFront Ml.update x k acc) xs in
    Ml.fold add_all g Ml.empty

  (* Renvoi la liste des sommets triés par ordre décroissant de date de fin de traitement par un PP *)
  let ppc_dt g =
    let rec aux k vs ((res, already) as a )=
      if S.mem k already
      then a
      else
        let (res,already) =
          List.fold_left
            (fun ((res, already) as a) v ->
              try
                aux v (Ml.find v g) a
              with
              Not_found ->
                 if S.mem v already
                 then a
                 else (v::res,S.add v already)
            ) (res,S.add k already) vs in
          (k::res,already)
    in
    fst (Ml.fold aux g ([],S.empty))

  (* Un PP suivant un ordre particulier *)
  let ppc_final order g =
    let test_and_add v s =
      if S.mem (L.mk_not v) s
      then raise Unsat
      else S.add v s in
    let rec aux ((already,res) as a) k vs =
      if S.mem k already
      then a
      else
        List.fold_left
          (fun ((already,res) as a) v ->
              try
                aux a v (Ml.find v g)
              with
                Not_found ->
                if S.mem v already
                then a
                else (S.add v already, test_and_add v res)
          ) (S.add k already,test_and_add k res) vs
    in
    let rec ppc_order ((already,res) as a) order =
      match order with
      | [] -> res
      | x::xs ->
         if S.mem x already
         then ppc_order a xs
         else
           let (already,acc) = aux (already,S.empty) x (Ml.find x g) in
           ppc_order (already,acc::res) xs
    in
    ppc_order (S.empty,[]) order

  (* Algo de Kosaraju pour calculer les CFC *)
  (* Les CFC sont normalement déjà triées topologiquement *)
  let kosaraju_scc g =
    ppc_final (ppc_dt g) (transpose g)

    (* Assigne les variables selon les CFC https://en.wikipedia.org/wiki/2-satisfiability#Strongly_connected_components *)
  let mkAssign m gamma =
    let aux gamma v =
      try
        if S.mem (L.mk_not (S.choose v)) gamma
        then gamma
        else S.union gamma v
      with
        Not_found -> gamma
    in
    List.fold_left aux gamma m

  let filter_rev p =
    List.fold_left (fun acc x -> if p x then x::acc else acc) []

  (* SAT *)
  let rec assume env f =
    if S.mem f env.gamma then env
    else bcp { env with gamma = S.add f env.gamma}

  and bcp env =
    let start =
      List.fold_left
        (fun env l ->
          try
            let l =
              filter_rev
                (fun f ->
                  if S.mem f env.gamma then raise Exit;
                  not (S.mem (L.mk_not f) env.gamma)
                ) l
            in
            match l with
            | [] -> raise Unsat (* conflict *)
            | [f] -> assume env f
            | _ -> { env with cl2 = l :: env.cl2 } (* Ne peut pas être plus qu'une 2-clause *)
          with Exit -> env )
        {env with cl2 = []; delta = []}
        env.cl2
    in
    List.fold_left
      (fun env l ->
        try
          let l =
            filter_rev
              (fun f ->
                if S.mem f env.gamma then raise Exit;
                not (S.mem (L.mk_not f) env.gamma)
              ) l
          in
          match l with
          | [] -> raise Unsat (* conflict *)
          | [f] -> assume env f
          | [_;_]-> {env with cl2 = l :: env.cl2}
          | _ -> {env with delta = l :: env.delta}
        with Exit -> env )
      start
      env.delta

  let rec unsat env = try
      (* 3 clauses OU PLUS *)
      match env.delta with
      | [] -> 
         print_endline (string_of_int (List.length env.cl2));
         let g = mk_implication_graph env.cl2 in
         let cfc = kosaraju_scc g in
         let assign = mkAssign cfc env.gamma in (* Va vérifier si les CFC sont correctes *)
         raise (Sat assign)
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
