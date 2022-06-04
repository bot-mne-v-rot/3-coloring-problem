module Graph = struct
  type 'v edge = 'v * 'v
  type 'v t = { vertices : 'v list; edges : 'v edge list }
end

module Color = struct
  type t = R | G | B
  type coloring = t list
end

module CNF = struct
  type 'v var = Var of 'v * Color.t
  type 'v literal = Lit of 'v var | NLit of 'v var
  type 'v clause = 'v literal list
  type 'v t = 'v clause list
  type 'v solution = 'v literal list
  type 'v solver = equal:('v -> 'v -> bool) -> 'v t -> 'v solution option

  let lit v c = Lit (Var (v, c))
  let nlit v c = NLit (Var (v, c))
end

(* Clauses for condition: each vertex has at least one color *)
let vertex_has_color (g : 'v Graph.t) : 'v CNF.t =
  let f v =
    let lit c = CNF.(Lit (Var (v, c))) in
    [ lit Color.R; lit Color.G; lit Color.B ]
  in
  List.map f Graph.(g.vertices)

(* Clauses for condition: no more than one color is assigned to each vertex *)
let vertices_color_uniqueness (g : 'v Graph.t) : 'v CNF.t =
  let open CNF in
  let open Color in
  let f (v : 'v) =
    let diff_cols c1 c2 = [ nlit v c1; nlit v c2 ] in
    [ diff_cols R B; diff_cols B G; diff_cols R G ]
  in
  List.flatten (List.map f Graph.(g.vertices))

(* Clauses for condition: adjacent vertices have different colors *)
let adj_vertices_have_diff_colors (g : 'v Graph.t) : 'v CNF.t =
  let open CNF in
  let open Color in
  let f ((u, v) : 'v Graph.edge) =
    let diff_cols c1 c2 = [ nlit u c1; nlit v c2 ] in
    [ diff_cols R G; diff_cols B G; diff_cols R B ]
  in
  List.flatten (List.map f Graph.(g.edges))

let ( @ ) = List.append

let generate_cnf (g : 'v Graph.t) : 'v CNF.t =
  (vertex_has_color g @ vertices_color_uniqueness g)
  @ adj_vertices_have_diff_colors g

module List = struct
  include List

  let map_opt (f : 'a -> 'b option) (l : 'a list) : 'b list option =
    let h e xs = Option.map (fun x -> cons x xs) (f e) in
    let g r e = Option.bind r (h e) in
    List.fold_left g (Some []) l
end

let recover_answer ~(equal : 'v -> 'v -> bool) (g : 'v Graph.t)
    (sol : 'v CNF.solution option) : Color.coloring option =
  let get_coloring (sol : 'v CNF.solution) =
    let f (v : 'v) : Color.t option =
      let true_var : 'v CNF.literal -> Color.t option = function
        | CNF.Lit (CNF.Var (v', c)) -> if equal v v' then Some c else None
        | _ -> None
      in
      List.find_map true_var sol
    in
    let map_opt : ('v -> Color.t option) -> 'v list -> Color.t list option =
      (List.map_opt [@coq_type_annotation])
    in
    (map_opt [@coq_type_annotation]) (f [@coq_type_annotation])
      (Graph.(g.vertices) [@coq_type_annotation])
  in
  Option.bind sol get_coloring

let solve ~(equal : 'v -> 'v -> bool) (sat_sol : 'v CNF.solver) (g : 'v Graph.t)
    : Color.coloring option =
  recover_answer g ~equal (sat_sol ~equal (generate_cnf g))
