%{ type expr = X | Y | Int of int | Add of expr * expr | Mul of expr * expr | Pow of expr * expr  %}

%token <int> INT
%token X_VAR Y_VAR PLUS MINUS TIMES POW OPEN CLOSE EOF

%start expr1
%type <expr> expr1

%left PLUS MINUS
%left TIMES DIVIDE
%right POW

%%

expr:
INT            { Int $1 }
| X_VAR          { X }
| Y_VAR        { Y }
| OPEN expr CLOSE { $2 }
| expr POW INT  { Pow($1, Int $3) }
| expr TIMES expr { Mul($1, $3) }
| expr PLUS expr  { Add($1, $3) };

expr1:
	| expr EOF { $1 };
