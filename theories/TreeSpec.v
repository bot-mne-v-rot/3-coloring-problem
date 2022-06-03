From tree Require Import Tree.

Theorem tree_simple: forall (a : Set),
    Leaf (a := a) = Leaf (a := a).
Proof.
    reflexivity.
Qed.


