let a = \s.\z.s(s(s(s(s(s(s(s z))))))) in
let p = \s.\z.s(s z) in
let mul = \m.\n.\f.(m (n f)) in
(* a ^ p *)
let power = \a.\p.\f.p a f in
power a p

