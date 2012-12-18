rule lex = parse
  | [' ' '\n' '\t'] { lex lexbuf }
  | 'x' { X_VAR }
  | 'y' { Y_VAR }
  | ['0'-'9']+ as s { INT (int_of_string s)} 
  | '+'             { PLUS}
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '('             { OPEN }
  | ')'             { CLOSE }
  | '^'             { POW   }
  | eof             { EOF }
