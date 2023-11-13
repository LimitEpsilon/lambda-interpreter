open Lambda

module Evaluator = struct
  module LEnv = Map.Make (struct
    type t = string

    let compare = compare
  end)

  type lbl = int

  let num_of_lbls = ref 0

  type lblexp = LVar of string | LLam of string * lbl | LApp of lbl * lbl

  let label_table : (lbl, lblexp) Hashtbl.t = Hashtbl.create 10

  let new_lbl () =
    let lbl = !num_of_lbls in
    incr num_of_lbls;
    lbl

  let rec label (e : lexp) =
    let lbl = new_lbl () in
    let lblexp =
      match e with
      | Var x -> LVar x
      | Lam (x, e) ->
        let e = label e in
        LLam (x, e)
      | App (e1, e2) ->
        let e1 = label e1 in
        let e2 = label e2 in
        LApp (e1, e2)
    in
    Hashtbl.add label_table lbl lblexp;
    lbl

  type t = A of t LEnv.t * lbl

  type ctx = Free of (f -> s) | Value of (lexp -> state)
  and f = (lexp, string * lbl * t LEnv.t) result
  and s = t * ctx
  and state = Halt of lexp | Continue of s

  let[@tail_mod_cons] rec reduce (A (env, exp), ctx) =
    match Hashtbl.find label_table exp with
    | LVar x -> (
      match LEnv.find x env with
      | exception Not_found -> (
        match ctx with
        | Free ctx -> reduce (ctx (Ok (Var x)))
        | Value ctx -> (
          match ctx (Var x) with Halt e -> e | Continue k -> reduce k))
      | found -> reduce (found, ctx))
    | LLam (x, e) -> (
      match ctx with
      | Free ctx -> reduce (ctx (Error (x, e, env)))
      | Value ctx ->
        reduce (A (LEnv.remove x env, e), Value (fun e -> ctx (Lam (x, e)))))
    | LApp (e1, e2) ->
      let ok_case =
        match ctx with
        | Free ctx -> fun e1 e2 -> Continue (ctx (Ok (App (e1, e2))))
        | Value ctx -> fun e1 e2 -> ctx (App (e1, e2))
      in
      reduce
        ( A (env, e1),
          Free
            (function
            | Error (x, e, env1) ->
              (A (LEnv.update x (fun _ -> Some (A (env, e2))) env1, e), ctx)
            | Ok e1 -> (A (env, e2), Value (ok_case e1))) )

  let reduce_lexp e =
    let my_lbl = label e in
    let exp = reduce (A (LEnv.empty, my_lbl), Value (fun e -> Halt e)) in
    exp
end

module Printer = struct
  open Evaluator

  let rec repeat times x =
    if times <= 0 then ()
    else (
      print_string x;
      repeat (times - 1) x)

  let rec print_aux (depth : int) (A (env, exp) : t) =
    repeat depth " ";
    print_int exp;
    print_string " under: {";
    LEnv.iter
      (fun x v ->
        print_newline ();
        repeat depth " ";
        print_string x;
        print_string "->";
        print_aux (depth + 1) v)
      env;
    print_string "}"

  let print exp =
    print_string "Labels:\n";
    Hashtbl.iter
      (fun i exp ->
        print_int i;
        print_string ": ";
        (match exp with
        | LVar x -> print_string x
        | LLam (x, e) ->
          print_string "\\";
          print_string x;
          print_string ".";
          print_int e
        | LApp (e1, e2) ->
          print_string "@ (";
          print_int e1;
          print_string ", ";
          print_int e2;
          print_string ")");
        print_newline ())
      label_table;
    print_string "Expr:\n";
    print_aux 0 exp;
    print_newline ()

  let rec finite_reduce leftover_steps (A (env, exp), ctx) =
    if leftover_steps <= 0 then
      let temp_var = Var ("[" ^ string_of_int exp ^ "]") in
      match ctx with
      | Value ctx -> (
        match ctx temp_var with
        | Halt e -> Pp.Pp.pp e
        | Continue k -> finite_reduce leftover_steps k)
      | Free ctx -> finite_reduce leftover_steps (ctx (Ok temp_var))
    else
      match Hashtbl.find label_table exp with
      | LVar x -> (
        match LEnv.find x env with
        | exception Not_found -> (
          match ctx with
          | Free ctx -> finite_reduce (leftover_steps - 1) (ctx (Ok (Var x)))
          | Value ctx -> (
            match ctx (Var x) with
            | Halt e -> Pp.Pp.pp e
            | Continue k -> finite_reduce (leftover_steps - 1) k))
        | found -> finite_reduce (leftover_steps - 1) (found, ctx))
      | LLam (x, e) -> (
        match ctx with
        | Free ctx ->
          finite_reduce (leftover_steps - 1) (ctx (Error (x, e, env)))
        | Value ctx ->
          finite_reduce (leftover_steps - 1)
            (A (LEnv.remove x env, e), Value (fun e -> ctx (Lam (x, e)))))
      | LApp (e1, e2) ->
        let ok_case =
          match ctx with
          | Free ctx -> fun e1 e2 -> Continue (ctx (Ok (App (e1, e2))))
          | Value ctx -> fun e1 e2 -> ctx (App (e1, e2))
        in
        finite_reduce (leftover_steps - 1)
          ( A (env, e1),
            Free
              (function
              | Error (x, e, env1) ->
                (A (LEnv.update x (fun _ -> Some (A (env, e2))) env1, e), ctx)
              | Ok e1 -> (A (env, e2), Value (ok_case e1))) )

  let finite_step steps pgm =
    let () = print_endline "=========\nInput pgm\n=========" in
    let () =
      Pp.Pp.pp pgm;
      print_newline ()
    in
    let exp = label pgm in
    let () = print_endline "===========\nPartial pgm\n===========" in
    finite_reduce steps (A (LEnv.empty, exp), Value (fun e -> Halt e));
    print_newline ()
end
