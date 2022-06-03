type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree

let rec sum tree =
  match tree with Leaf n -> n | Node (tree1, tree2) -> sum tree1 + sum tree2
