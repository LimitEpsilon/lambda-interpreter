%token LAMBDA DOT
%token <string> ID
%token IN
%token EQUAL
%token LET
%token LP RP
%token EOF
%right LP ID LAMBDA LET

%start program
%type <Lambda.lexp> program

%%

program: exp EOF { $1 }

exp :
	| exp atom { Lambda.App ($1, $2) }
        | atom { $1 }
	;

atom :
        | ID { Lambda.Var ($1) }
	| LP exp RP { $2 }
	| LAMBDA ID DOT exp %prec LP{ Lambda.Lam ($2, $4) }
	| LET ID EQUAL exp IN exp %prec LP{ Lambda.App (Lambda.Lam ($2, $6), $4) }
        ;
%%
