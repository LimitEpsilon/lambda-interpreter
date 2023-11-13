type lexp = Var of string | Lam of string * lexp | App of lexp * lexp
