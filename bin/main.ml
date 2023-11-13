open Lambdatools
open Evaluate
open Pp

let main () =
  let step = ref 0 in
  let finite = ref false in
  let src = ref "" in
  let _ =
    Arg.parse
      [
        ( "-step",
          Arg.Int
            (fun i ->
              finite := true;
              step := i),
          "print the partial program after <n> steps" );
      ]
      (fun x -> src := x)
      ("Usage: " ^ Filename.basename Sys.argv.(0) ^ " [-step] [n] [file]")
  in
  let lexbuf =
    Lexing.from_channel (if !src = "" then stdin else open_in !src)
  in
  let pgm = Parser.program Lexer.start lexbuf in

  if !finite then Printer.finite_step !step pgm
  else (
    print_string "=============\n";
    print_string "input program\n";
    print_string "=============\n";
    Pp.pp pgm;
    print_string "\n\n\n==============\n";
    print_string "output program\n";
    print_string "==============\n";
    Pp.pp (Evaluator.reduce_lexp pgm);
    print_string "\n")

let _ = main ()
