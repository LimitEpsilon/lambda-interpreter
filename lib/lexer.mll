{
 open Parser
 exception Eof
 exception LexicalError
 let comment_depth = ref 0
 let keyword_tbl = Hashtbl.create 2
 let _ = List.iter (fun (keyword, tok) -> Hashtbl.add keyword_tbl keyword tok)
        [
        ("let", LET);
        ("in", IN);
        ]
}

let blank = [' ' '\n' '\t' '\r']+
let alpha = ['a'-'z' 'A'-'Z']
let allowed = alpha | ['0'-'9' '_']
let id = allowed allowed*

rule start =
 parse blank { start lexbuf }
     | "(*" { comment_depth :=1;
              comment lexbuf;
              start lexbuf }
     | id { let id = Lexing.lexeme lexbuf
            in try Hashtbl.find keyword_tbl id
               with _ -> ID id
            }
     | "\\" { LAMBDA }
     | "=" { EQUAL }
     | "." { DOT }
     | "(" { LP }
     | ")" { RP }
     | eof { EOF }
     | _ { raise LexicalError}

and comment = parse
     "(*" {comment_depth := !comment_depth+1; comment lexbuf}
   | "*)" {comment_depth := !comment_depth-1;
           if !comment_depth > 0 then comment lexbuf }
   | eof {raise Eof}
   | _   {comment lexbuf}
