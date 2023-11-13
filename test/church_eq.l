(* church encoding of booleans *)
let true = \xT.\yT.xT in
let false = \xF.\yF.yF in

(* fixpoint combinator *)
let fix = \fY.(\xY.fY (xY xY)) (\xY.fY (xY xY)) in

(* church encoding of arithmetic *)
let plus = \p1.\p2.\pf.\x.p1 pf (p2 pf x) in

let mult = \m1.\m2.\mf.m2 (m1 mf) in

let pred = \n.\rf.\rx.
  n (\g.\h.h (g rf)) (\ignored.rx) (\id.id) in

let sub = \s1.\s2.s2 pred s1 in

(* church encoding of naturals *)
let 0 = \s.\z.z in
let S = \n.\s.\x.s (n s x) in

let 1 = S 0 in
let 2 = S 1 in
let 3 = S 2 in
let 4 = S 3 in

(* church encoding of conditionals *)
let if0 = \z.z (\zx.false) true in

let church_eq = fix \church_eq.\e1.\e2.
  if0 e1
    (if0 e2 true false)
    (if0 e2 false (church_eq (pred e1) (pred e2))) in

(* multiplication distributes over addition *)
church_eq
  (mult 2 (plus 1 3))
  (plus (mult 2 1) (mult 2 3))

