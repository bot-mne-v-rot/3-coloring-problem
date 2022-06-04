open Js_of_ocaml

let _ =
  Js.export "solver"
    (object%js
       method solve g = Src.Reduction.solve ~equal:( = ) Solver.solve_sat g
    end)
