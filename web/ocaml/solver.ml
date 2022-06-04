open Src.Reduction

let rec remove_dups equal l =
  let elem x xs = List.exists (fun y -> equal x y) xs in
  match l with
  | x :: xs ->
      if elem x xs then remove_dups equal xs else x :: remove_dups equal xs
  | [] -> []

let vars_eq equal (CNF.Var (v1, _)) (CNF.Var (v2, _)) = equal v1 v2

let cnf_vars equal (cnf : 'v CNF.t) =
  let f = function CNF.Lit v -> v | CNF.NLit v -> v in
  remove_dups (vars_eq equal) @@ List.map f @@ List.flatten cnf

let eval_var equal (values : ('v CNF.var * bool) list) (var : 'v CNF.var) =
  let f (v, _) = vars_eq equal var v in
  snd @@ Stdlib.List.find f values

let eval_lit equal values (lit : 'v CNF.literal) =
  match lit with
  | CNF.Lit var -> eval_var equal values var
  | CNF.NLit var -> not @@ eval_var equal values var

let eval_clause equal values (cl : 'v CNF.clause) =
  List.for_all (eval_lit equal values) cl

let eval_cnf equal values (cnf : 'v CNF.t) =
  List.for_all (eval_clause equal values) cnf

let rec solve_sat' equal cnf vars values =
  let opt_or a b = Option.bind a (fun _ -> b) in
  match vars with
  | v :: vars' ->
      let sub f = solve_sat' equal cnf vars' ((v, f) :: values) in
      opt_or (sub true) (sub false)
  | [] -> if eval_cnf equal values cnf then Some values else None

let solve_sat ~equal cnf =
  let values_opt = solve_sat' equal cnf (cnf_vars equal cnf) [] in
  let f (v, b) = if b then CNF.Lit v else CNF.NLit v in
  Option.map (List.map f) values_opt
