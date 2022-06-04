(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* An alias for the type of lists. *)
type 'a t = 'a list

(* List operations *)

let rec length_aux len = function [] -> len | _ :: l -> length_aux (len + 1) l
let length l = length_aux 0 l
let cons a l = a :: l

let nth_opt l n =
  if n < 0 then None
  else
    let rec nth_aux l n =
      match l with
      | [] -> None
      | a :: l -> if n = 0 then Some a else nth_aux l (n - 1)
    in
    nth_aux l n

let append = ( @ )

let rec rev_append l1 l2 =
  match l1 with [] -> l2 | a :: l -> rev_append l (a :: l2)

let rev l = rev_append l []
let rec flatten = function [] -> [] | l :: r -> l @ flatten r
let concat = flatten

let rec map (f : 'a -> 'b) : 'a list -> 'b list = function
  | [] -> []
  | a :: l ->
      let r = f a in
      r :: map f l

let mapi (f : int -> 'a -> 'b) (l : 'a list) =
  let rec aux (i : int) : 'a list -> 'b list = function
    | [] -> []
    | a :: l ->
        let r = f i a in
        r :: aux (i + 1) l
  in
  aux 0 l

let rev_map f l =
  let rec rmap_f accu = function
    | [] -> accu
    | a :: l -> rmap_f (f a :: accu) l
  in
  rmap_f [] l

let rec iter f = function
  | [] -> ()
  | a :: l ->
      f a;
      iter f l

let iteri f l =
  let rec aux i = function
    | [] -> ()
    | a :: l ->
        f i a;
        aux (i + 1) l
  in
  aux 0 l

let rec fold_left f accu l =
  match l with [] -> accu | a :: l -> fold_left f (f accu a) l

let rec fold_right f l accu =
  match l with [] -> accu | a :: l -> f a (fold_right f l accu)

let rec for_all p = function [] -> true | a :: l -> p a && for_all p l
let rec exists p = function [] -> false | a :: l -> p a || exists p l

let rec find_opt p = function
  | [] -> None
  | x :: l -> if p x then Some x else find_opt p l

let rec find_map f = function
  | [] -> None
  | x :: l -> (
      match f x with Some _ as result -> result | None -> find_map f l)

let find_all p =
  let rec find accu = function
    | [] -> rev accu
    | x :: l -> if p x then find (x :: accu) l else find accu l
  in
  find []

let filter = find_all

let filteri p l =
  let rec aux i acc = function
    | [] -> rev acc
    | x :: l -> aux (i + 1) (if p i x then x :: acc else acc) l
  in
  aux 0 [] l

let filter_map f =
  let rec aux accu = function
    | [] -> rev accu
    | x :: l -> (
        match f x with None -> aux accu l | Some v -> aux (v :: accu) l)
  in
  aux []

let concat_map (f : 'a -> 'b list) (l : 'a list) : 'b list =
  let rec aux (acc : 'b list) = function
    | [] -> rev acc
    | x :: l ->
        let xs = f x in
        aux (rev_append xs acc) l
  in
  aux [] l

let fold_left_map (f : 'a -> 'b -> 'a * 'c) (accu : 'a) (l : 'b list) :
    'a * 'c list =
  let rec aux accu l_accu = function
    | [] -> (accu, rev l_accu)
    | x :: l ->
        let accu, x = f accu x in
        aux accu (x :: l_accu) l
  in
  aux accu [] l

let partition p l =
  let rec part yes no = function
    | [] -> (rev yes, rev no)
    | x :: l -> if p x then part (x :: yes) no l else part yes (x :: no) l
  in
  part [] [] l

let rec split (l : ('a * 'b) list) : 'a list * 'b list =
  match l with
  | [] -> ([], [])
  | (x, y) :: l ->
      let rx, ry = split l in
      (x :: rx, y :: ry)

let rec combine l1 l2 =
  match (l1, l2) with
  | a1 :: l1, a2 :: l2 -> (a1, a2) :: combine l1 l2
  | _, _ -> []

(** sorting *)

let rec compare_lengths l1 l2 =
  match (l1, l2) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | _ :: l1, _ :: l2 -> compare_lengths l1 l2

let rec compare_length_with l n =
  match l with
  | [] -> if n = 0 then 0 else if n > 0 then -1 else 1
  | _ :: l -> if n <= 0 then 1 else compare_length_with l (n - 1)

(** {1 Comparison} *)

(* Note: we are *not* shortcutting the list by using
   [List.compare_lengths] first; this may be slower on long lists
   immediately start with distinct elements. It is also incorrect for
   [compare] below, and it is better (principle of least surprise) to
   use the same approach for both functions. *)
let rec equal eq l1 l2 =
  match (l1, l2) with
  | [], [] -> true
  | [], _ :: _ | _ :: _, [] -> false
  | a1 :: l1, a2 :: l2 -> eq a1 a2 && equal eq l1 l2

let rec compare cmp l1 l2 =
  match (l1, l2) with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | a1 :: l1, a2 :: l2 ->
      let c = cmp a1 a2 in
      if c <> 0 then c else compare cmp l1 l2
