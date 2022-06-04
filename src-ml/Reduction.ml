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
  type 'v solver = 'v t -> 'v solution option
end

(* Clauses for condition: each vertex has at least one color *)
let vertex_has_color (g : 'v Graph.t) : 'v CNF.t =
  let rec helper (vs : 'v list) =
    match vs with
    | [] -> []
    | v :: xs ->
        let lit c = CNF.Lit (CNF.Var (v, c)) in
        let cl = [ lit Color.R; lit Color.G; lit Color.B ] in
        cl :: helper xs
  in
  helper Graph.(g.vertices)

(* Clauses for condition: no more than one color is assigned to each vertex *)
let vertices_color_uniqueness (g : 'v Graph.t) : 'v CNF.t =
  let rec helper (vs : 'v list) =
    match vs with
    | [] -> []
    | v :: xs ->
        let nlit c = CNF.NLit (CNF.Var (v, c)) in
        let diff_cols c1 c2 = [ nlit c1; nlit c2 ] in
        let cl =
          [
            diff_cols Color.R Color.B;
            diff_cols Color.B Color.G;
            diff_cols Color.R Color.G;
          ]
        in
        cl @ helper xs
  in
  helper Graph.(g.vertices)

(* Clauses for condition: adjacent vertices have different colors *)
let adj_vertices_have_diff_colors (g : 'v Graph.t) : 'v CNF.t =
  let rec helper (edges : 'v Graph.edge list) =
    match edges with
    | [] -> []
    | e :: xs ->
        let u = fst e in
        let v = snd e in
        let nlit v c = CNF.Lit (CNF.Var (v, c)) in
        let cl1 = [ nlit u Color.R; nlit v Color.G ] in
        let cl2 = [ nlit u Color.G; nlit v Color.B ] in
        let cl3 = [ nlit u Color.B; nlit v Color.R ] in
        [ cl1; cl2; cl3 ] @ helper xs
  in
  helper Graph.(g.edges)

let generate_cnf (g : 'v Graph.t) : 'v CNF.t =
  vertex_has_color g
  @ vertices_color_uniqueness g
  @ adj_vertices_have_diff_colors g

let recover_answer (g : 'v Graph.t) (sol : 'v CNF.solution option) :
    Color.coloring option =
  match (g, sol) with
  | _, None -> None
  | graph, Some xs ->
      let rec helper (vs : 'v list) =
        match vs with
        | [] -> []
        | v :: xs ->
            let el =
              match v with
              | CNF.NLit v -> []
              | CNF.Lit (CNF.Var (v, c)) -> [ c ]
            in
            el @ helper xs
      in
      Some (helper Graph.(g.vertices))

let solve (sat_sol : 'v CNF.solver) (g : 'v Graph.t) : Color.coloring option =
  recover_answer g @@ sat_sol @@ generate_cnf g
