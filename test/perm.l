let false=\a.\b.b in
let true=\a.\b.a in
let cons=\a.\b.\x.x a b in
let Y=\f.(\x.f (x x))(\x.f (x x)) in
let foldr=\f.\init.Y(\self.\list.list (\lhs.\rhs.\init.f lhs (self rhs)) init) in
let concat=\a.\b.foldr cons b a in
let rcons=\a.\b.concat a (cons b false) in
let null=\list.list (\a.\b.\i.false) true in
let inc=\n.\f.\x.f (n f x) in
let pow=\a.\b.b a in
let 2=(inc \a.a) in
let 3=inc 2 in
let 10=inc (pow 3 2) in
let inject=Y \self.\init.\a.\block.
  a (\lhs.\rhs.\empty.
    concat (block init a) (self (rcons init lhs) rhs block)
  ) false in
let perm=Y \self.\pre.\s.
  null s (rcons pre 10) (
  inject false s \left.\right.
    self (rcons pre (right true)) (concat left (right false))) in
let nil = \a.\b.b in
perm false (cons 10 (cons 10 (cons 10 nil)))

