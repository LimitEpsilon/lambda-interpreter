open Lambda

module Pp = struct
  let rec pp exp =
    match exp with
    | Var s -> print_string s
    | Lam (s, e) ->
        print_string "\\";
        print_string (s ^ ".");
        pp e;
        print_string ""
    | App (e1, e2) ->
        print_string "(";
        pp e1;
        print_string ") (";
        pp e2;
        print_string ")"
end
