

exception myexception of string * int * int ;
val lexedList : (string *int * int) list ref = ref [];

 val row = ref 1;
 val col = ref 1;
 
 fun inc (x : int ref, value : int) = x := (value + (!x));
 
structure Tokens = Tokens

 type pos = int
 type svalue = Tokens.svalue
 type ('a,'b) token = ('a,'b) Tokens.token
 type lexresult = (svalue,pos) token
	
 val pos = ref 0 

 
 val eof = fn () => Tokens.EOF (!row, !col)
 val error = fn (e,l:int, _) => TextIO.output(TextIO.stdOut, "line " ^ (Int.toString l) ^ ": " ^ e)
	
 fun revfold _ nil b = b
 | revfold f (hd::tl) b = revfold f tl (f(hd,b))
	
		
	
%% 

%header (functor CalcLexFun (structure Tokens: Calc_TOKENS));

alpha = [A-Za-z];
numeric = [0-9];



ws = [\ \t];

%%




\n => (inc (row,1) ; col := 1; inc (pos,1) ; lex());
\r\n => (inc (row,1) ; col := 1; inc (pos,1) ; lex());

{ws}+	=> ( inc (col, size (yytext)) ; lex());

"("	=> (lexedList := ("LPAREN \"(\"" , (!row) , (!col)) :: (!lexedList) ; inc (col,1); Tokens.LPAREN(!row,!col));
")" => (lexedList := ("RPAREN \")\"" , (!row) , (!col)) :: (!lexedList) ; inc (col,1); Tokens.RPAREN(!row,!col));
"->" => (lexedList := ("SINGLEARROW \"->\"" , (!row) , (!col)) :: (!lexedList)   ; inc (col,2);  Tokens.SINGLEARROW(!row,!col));
"=>" => (lexedList := ("DOUBLEARROW \"=>\"" , (!row) , (!col)) :: (!lexedList)   ; inc (col,2);  Tokens.DOUBLEARROW(!row,!col));
";"	=> (lexedList := ("TERM \";\"" , (!row) , (!col)) :: (!lexedList)   ; inc (col,1);  Tokens.TERM(!row,!col));
"="	=> (lexedList := ("ASSIGN \"=\"" , (!row) , (!col)) :: (!lexedList)   ; inc (col,1);  Tokens.ASSIGN(!row,!col));
":" => (lexedList := ("COLON \":\"" , (!row) , (!col)) :: (!lexedList)   ; inc (col,1);  Tokens.COLON(!row,!col));

{alpha}+ => (

if (yytext = "if") then (lexedList := ("IF \"if\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.IF (!row,!col))
else if (yytext = "then") then (lexedList := ("THEN \"then\"", (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;  Tokens.THEN (!row,!col))
else if (yytext = "else") then (lexedList := ("ELSE \"else\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;  Tokens.ELSE (!row,!col))
else if (yytext = "fi") then (lexedList := ("FI \"fi\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;  Tokens.FI (!row,!col))

else if (yytext = "IMPLIES") then (lexedList := ("IMPLIES \"IMPLIES\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.IMPLIES (!row,!col))
else if (yytext = "EQUALS") then (lexedList := ("EQUALS \"EQUALS\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;  Tokens.EQUALS (!row,!col))
else if (yytext = "XOR") then (lexedList := ("XOR \"XOR\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;  Tokens.XOR (!row,!col))
else if (yytext = "AND") then (lexedList := ("AND \"AND\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.AND (!row,!col))
else if (yytext = "OR") then (lexedList := ("OR \"OR\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.OR (!row,!col))
else if (yytext = "NOT") then (lexedList := ("NOT \"NOT\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.NOT (!row,!col))

else if (yytext = "TRUE" orelse yytext = "FALSE") then (lexedList := ("BOOLCONST \""^ yytext ^"\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;  Tokens.BOOLCONST (yytext,!row,!col))

else if (yytext = "PLUS") then (lexedList := ("PLUS \"PLUS\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.PLUS (!row,!col))
else if (yytext = "MINUS") then (lexedList := ("MINUS \"MINUS\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.MINUS (!row,!col))
else if (yytext = "TIMES") then (lexedList := ("TIMES \"TIMES\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.TIMES (!row,!col))
else if (yytext = "NEGATE") then (lexedList := ("NEGATE \"NEGATE\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.NEGATE (!row,!col))
else if (yytext = "LESSTHAN") then (lexedList := ("LESSTHAN \"LESSTHAN\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.LESSTHAN (!row,!col))
else if (yytext = "GREATERTHAN") then (lexedList := ("GREATERTHAN \"GREATERTHAN\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.GREATERTHAN (!row,!col))

else if (yytext = "let") then (lexedList := ("LET \"let\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.LET (!row,!col))
else if (yytext = "in") then (lexedList := ("IN \"in\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.IN (!row,!col))
else if (yytext = "end") then (lexedList := ("END \"end\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.END (!row,!col))

else if (yytext = "int") then (lexedList := ("TYPE \""^yytext^"\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.INTBASETYPE (!row,!col))
else if (yytext = "boolean") then (lexedList := ("TYPE \""^yytext^"\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.BOOLBASETYPE (!row,!col))

else if (yytext = "fn") then (lexedList := ("FN \"fn\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.FN (!row,!col))
else if (yytext = "fun") then (lexedList := ("FUN \"fun\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.FUN (!row,!col))

else (lexedList := ("ID \""^ yytext ^"\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;  Tokens.ID (yytext,!row,!col))
);






{numeric}+ => (lexedList := ("INTCONST \"" ^ yytext ^ "\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;
				 Tokens.INTCONST (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),!row,!col));

({alpha}({alpha} | {numeric})*) => (lexedList := ("ID \""^ yytext ^"\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;  Tokens.ID (yytext,!row,!col));

.	=> (let val prerow = !row
	     	val precol = !col 
	    in (
	    row := 1;
	    col := 1;
	    raise myexception (yytext,prerow,precol)
	    )
	    end);













