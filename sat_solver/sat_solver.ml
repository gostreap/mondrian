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

  (* TODO: on n'a pas forcément besoin de cl2 *)
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
      | [a;b] -> (* TODO change this, it can certainly made a better way *)
         let acc =
           if Ml.mem a acc
           then acc
           else Ml.add a [] acc in
         let acc =
           if Ml.mem b acc
           then acc
           else Ml.add b [] acc in
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
      | x::xs ->
         List.fold_left (fun acc x -> addFront Ml.update x k acc) (addFront Ml.update x k acc) xs in
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
      (fun x y -> compare (snd y) (snd x)) (* TODO insérer dans la liste triée durant le fold *)
      (Seq.fold_left (fun acc x -> x::acc) [] (Ml.to_seq a))

  (* Un PP suivant un ordre particulier *)
  let ppc_final order g =
    let test_and_add v s=
      if S.mem (L.mk_not v) s
      then raise Unsat
      else S.add v s in
    let rec aux ((already,res) as a) k vs =
      if S.mem k already
      then a
      else
        List.fold_left
          (fun ((already,res) as a) v ->
            match Ml.find_opt v g with
            | None ->
               if S.mem v already
               then a
               else (S.add v already, test_and_add v res)
            | Some x -> aux a v x
          ) (S.add k already,test_and_add k res) vs
    in
    let rec ppc_order ((already,res) as a) order =
      match order with
      | [] -> res
      | (x,_)::xs ->
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
    let order = ppc_dt g in
    let g = transpose g in
    ppc_final order g

    (* Assigne les variables selon les CFC https://en.wikipedia.org/wiki/2-satisfiability#Strongly_connected_components *)
  let mkAssign m gamma =
    let aux gamma v =
      match S.choose_opt v with
      | None -> gamma
      | Some x ->
         if S.mem (L.mk_not x) gamma
         then gamma
         else S.union gamma v
    in
    List.fold_left aux gamma m

  let filter_rev_l p = (* la taille de la liste est un by-product *)
    List.fold_left (fun ((a,acc) as e) x -> if p x then (a && (List.length acc < 2),x::acc) else e) (true,[])

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
        {env with cl2 = []}
        env.cl2
    in
    List.fold_left
      (fun env l ->
        try
          let b,l =
            filter_rev_l
              (fun f ->
                if S.mem f env.gamma then raise Exit;
                not (S.mem (L.mk_not f) env.gamma)
              ) l
          in
          match l with
          | [] -> raise Unsat (* conflict *)
          | [f] -> assume env f
          | _ ->
             if b
             then { env with cl2 = l :: env.cl2 } 
             else {env with delta = l :: env.delta}
        with Exit -> env )
      { start with delta = [] }
      env.delta

  let rec unsat env = try
      (* 3 clauses OU PLUS *)
      match env.delta with
      | [] ->
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
