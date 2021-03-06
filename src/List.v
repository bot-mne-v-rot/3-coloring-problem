(** File generated by coq-of-ocaml *)
Require Import CoqOfOCaml.CoqOfOCaml.
Require Import CoqOfOCaml.Settings.

Definition t (a : Set) : Set := list a.

Fixpoint length_aux {A : Set} (len : int) (function_parameter : list A) : int :=
  match function_parameter with
  | [] => len
  | cons _ l_value => length_aux (Z.add len 1) l_value
  end.

Definition length {A : Set} (l_value : list A) : int := length_aux 0 l_value.

Definition cons_value {A : Set} (a_value : A) (l_value : list A) : list A :=
  cons a_value l_value.

Definition nth_opt {A : Set} (l_value : list A) (n_value : int) : option A :=
  if CoqOfOCaml.Stdlib.lt n_value 0 then
    None
  else
    let fix nth_aux {B : Set} (l_value : list B) (n_value : int) : option B :=
      match l_value with
      | [] => None
      | cons a_value l_value =>
        if equiv_decb n_value 0 then
          Some a_value
        else
          nth_aux l_value (Z.sub n_value 1)
      end in
    nth_aux l_value n_value.

Definition append {A : Set} : list A -> list A -> list A :=
  CoqOfOCaml.Stdlib.app.

Fixpoint rev_append {A : Set} (l1 : list A) (l2 : list A) : list A :=
  match l1 with
  | [] => l2
  | cons a_value l_value => rev_append l_value (cons a_value l2)
  end.

Definition rev {A : Set} (l_value : list A) : list A := rev_append l_value nil.

Fixpoint flatten {A : Set} (function_parameter : list (list A)) : list A :=
  match function_parameter with
  | [] => nil
  | cons l_value r_value => CoqOfOCaml.Stdlib.app l_value (flatten r_value)
  end.

Definition concat {A : Set} : list (list A) -> list A := flatten.

Fixpoint map {a b : Set} (f_value : a -> b) (function_parameter : list a)
  : list b :=
  match function_parameter with
  | [] => nil
  | cons a_value l_value =>
    let r_value := f_value a_value in
    cons r_value (map f_value l_value)
  end.

Definition mapi {a b : Set} (f_value : int -> a -> b) (l_value : list a)
  : list b :=
  let fix aux (i_value : int) (function_parameter : list a) : list b :=
    match function_parameter with
    | [] => nil
    | cons a_value l_value =>
      let r_value := f_value i_value a_value in
      cons r_value (aux (Z.add i_value 1) l_value)
    end in
  aux 0 l_value.

Definition rev_map {A B : Set} (f_value : A -> B) (l_value : list A) : list B :=
  let fix rmap_f (accu : list B) (function_parameter : list A) : list B :=
    match function_parameter with
    | [] => accu
    | cons a_value l_value => rmap_f (cons (f_value a_value) accu) l_value
    end in
  rmap_f nil l_value.

Fixpoint iter {A : Set} (f_value : A -> unit) (function_parameter : list A)
  : unit :=
  match function_parameter with
  | [] => tt
  | cons a_value l_value =>
    let '_ := f_value a_value in
    iter f_value l_value
  end.

Definition iteri {A : Set} (f_value : int -> A -> unit) (l_value : list A)
  : unit :=
  let fix aux (i_value : int) (function_parameter : list A) : unit :=
    match function_parameter with
    | [] => tt
    | cons a_value l_value =>
      let '_ := f_value i_value a_value in
      aux (Z.add i_value 1) l_value
    end in
  aux 0 l_value.

Fixpoint fold_left {A B : Set}
  (f_value : A -> B -> A) (accu : A) (l_value : list B) : A :=
  match l_value with
  | [] => accu
  | cons a_value l_value => fold_left f_value (f_value accu a_value) l_value
  end.

Fixpoint fold_right {A B : Set}
  (f_value : A -> B -> B) (l_value : list A) (accu : B) : B :=
  match l_value with
  | [] => accu
  | cons a_value l_value => f_value a_value (fold_right f_value l_value accu)
  end.

Fixpoint for_all {A : Set} (p_value : A -> bool) (function_parameter : list A)
  : bool :=
  match function_parameter with
  | [] => true
  | cons a_value l_value => andb (p_value a_value) (for_all p_value l_value)
  end.

Fixpoint _exists {A : Set} (p_value : A -> bool) (function_parameter : list A)
  : bool :=
  match function_parameter with
  | [] => false
  | cons a_value l_value => orb (p_value a_value) (_exists p_value l_value)
  end.

Fixpoint find_opt {A : Set} (p_value : A -> bool) (function_parameter : list A)
  : option A :=
  match function_parameter with
  | [] => None
  | cons x_value l_value =>
    if p_value x_value then
      Some x_value
    else
      find_opt p_value l_value
  end.

Fixpoint find_map {A B : Set}
  (f_value : A -> option B) (function_parameter : list A) : option B :=
  match function_parameter with
  | [] => None
  | cons x_value l_value =>
    match f_value x_value with
    | (Some _) as result_value => result_value
    | None => find_map f_value l_value
    end
  end.

Definition find_all {A : Set} (p_value : A -> bool) : list A -> list A :=
  let fix find (accu : list A) (function_parameter : list A) : list A :=
    match function_parameter with
    | [] => rev accu
    | cons x_value l_value =>
      if p_value x_value then
        find (cons x_value accu) l_value
      else
        find accu l_value
    end in
  find nil.

Definition filter {A : Set} : (A -> bool) -> list A -> list A := find_all.

Definition filteri {A : Set} (p_value : int -> A -> bool) (l_value : list A)
  : list A :=
  let fix aux (i_value : int) (acc : list A) (function_parameter : list A)
    : list A :=
    match function_parameter with
    | [] => rev acc
    | cons x_value l_value =>
      aux (Z.add i_value 1)
        (if p_value i_value x_value then
          cons x_value acc
        else
          acc) l_value
    end in
  aux 0 nil l_value.

Definition filter_map {A B : Set} (f_value : A -> option B)
  : list A -> list B :=
  let fix aux (accu : list B) (function_parameter : list A) : list B :=
    match function_parameter with
    | [] => rev accu
    | cons x_value l_value =>
      match f_value x_value with
      | None => aux accu l_value
      | Some v_value => aux (cons v_value accu) l_value
      end
    end in
  aux nil.

Definition concat_map {a b : Set} (f_value : a -> list b) (l_value : list a)
  : list b :=
  let fix aux (acc : list b) (function_parameter : list a) : list b :=
    match function_parameter with
    | [] => rev acc
    | cons x_value l_value =>
      let xs := f_value x_value in
      aux (rev_append xs acc) l_value
    end in
  aux nil l_value.

Definition fold_left_map {a b c : Set}
  (f_value : a -> b -> a * c) (accu : a) (l_value : list b) : a * list c :=
  let fix aux (accu : a) (l_accu : list c) (function_parameter : list b)
    : a * list c :=
    match function_parameter with
    | [] => (accu, (rev l_accu))
    | cons x_value l_value =>
      let '(accu, x_value) := f_value accu x_value in
      aux accu (cons x_value l_accu) l_value
    end in
  aux accu nil l_value.

Definition partition {A : Set} (p_value : A -> bool) (l_value : list A)
  : list A * list A :=
  let fix part (yes : list A) (no : list A) (function_parameter : list A)
    : list A * list A :=
    match function_parameter with
    | [] => ((rev yes), (rev no))
    | cons x_value l_value =>
      if p_value x_value then
        part (cons x_value yes) no l_value
      else
        part yes (cons x_value no) l_value
    end in
  part nil nil l_value.

Fixpoint split {a b : Set} (l_value : list (a * b)) : list a * list b :=
  match l_value with
  | [] => (nil, nil)
  | cons (x_value, y_value) l_value =>
    let '(rx, ry) := split l_value in
    ((cons x_value rx), (cons y_value ry))
  end.

Fixpoint combine {A B : Set} (l1 : list A) (l2 : list B) : list (A * B) :=
  match (l1, l2) with
  | (cons a1 l1, cons a2 l2) => cons (a1, a2) (combine l1 l2)
  | (_, _) => nil
  end.

Fixpoint compare_lengths {A B : Set} (l1 : list A) (l2 : list B) : int :=
  match (l1, l2) with
  | ([], []) => 0
  | ([], _) => (-1)
  | (_, []) => 1
  | (cons _ l1, cons _ l2) => compare_lengths l1 l2
  end.

Fixpoint compare_length_with {A : Set} (l_value : list A) (n_value : int)
  : int :=
  match l_value with
  | [] =>
    if equiv_decb n_value 0 then
      0
    else
      if CoqOfOCaml.Stdlib.gt n_value 0 then
        (-1)
      else
        1
  | cons _ l_value =>
    if CoqOfOCaml.Stdlib.le n_value 0 then
      1
    else
      compare_length_with l_value (Z.sub n_value 1)
  end.

Fixpoint equal {A B : Set}
  (eq_value : A -> B -> bool) (l1 : list A) (l2 : list B) : bool :=
  match (l1, l2) with
  | ([], []) => true
  | (([], cons _ _) | (cons _ _, [])) => false
  | (cons a1 l1, cons a2 l2) => andb (eq_value a1 a2) (equal eq_value l1 l2)
  end.

Fixpoint compare {A B : Set} (cmp : A -> B -> int) (l1 : list A) (l2 : list B)
  : int :=
  match (l1, l2) with
  | ([], []) => 0
  | ([], cons _ _) => (-1)
  | (cons _ _, []) => 1
  | (cons a1 l1, cons a2 l2) =>
    let c_value := cmp a1 a2 in
    if nequiv_decb c_value 0 then
      c_value
    else
      compare cmp l1 l2
  end.
