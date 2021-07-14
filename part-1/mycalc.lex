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
ws = [\ \t];



%%

\n	=> (inc (row,1) ; col := 1; inc (pos,1) ; lex());
{ws}+	=> ( inc (col, size (yytext)) ; lex());

"("	=> (lexedList := ("LPAREN \"(\"" , (!row) , (!col)) :: (!lexedList) ; inc (col,1); Tokens.LPAREN(!row,!col));
")" 	=> (lexedList := ("RPAREN \")\"" , (!row) , (!col)) :: (!lexedList) ; inc (col,1); Tokens.RPAREN(!row,!col));
";"	=> (lexedList := ("TERM \";\"" , (!row) , (!col)) :: (!lexedList)   ; inc (col,1);  Tokens.TERM(!row,!col));

{alpha}+ => (
	 if (yytext = "IF") then (lexedList := ("IF \"IF\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.IF (!row,!col))
	 else if (yytext = "THEN") then (lexedList := ("THEN \"THEN\"", (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;  Tokens.THEN (!row,!col))
	 else if (yytext = "ELSE") then (lexedList := ("ELSE \"ELSE\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;  Tokens.ELSE (!row,!col))
	 else if (yytext = "IMPLIES") then (lexedList := ("IMPLIES \"IMPLIES\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.IMPLIES (!row,!col))
	 else if (yytext = "EQUALS") then (lexedList := ("EQUALS \"EQUALS\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;  Tokens.EQUALS (!row,!col))
	 else if (yytext = "XOR") then (lexedList := ("XOR \"XOR\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;  Tokens.XOR (!row,!col))
	 else if (yytext = "AND") then (lexedList := ("AND \"AND\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.AND (!row,!col))
	 else if (yytext = "OR") then (lexedList := ("OR \"OR\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.OR (!row,!col))
	 else if (yytext = "NOT") then (lexedList := ("NOT \"NOT\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ; Tokens.NOT (!row,!col))
	 else if (yytext = "TRUE" orelse yytext = "FALSE") then (lexedList := ("CONST \""^ yytext ^"\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;  Tokens.CONST (yytext,!row,!col))
	 else (lexedList := ("ID \""^ yytext ^"\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;  Tokens.ID (yytext,!row,!col)));

.	=> (let val prerow = !row
	     	val precol = !col 
	    in (
	    row := 1;
	    col := 1;
	    raise myexception (yytext,prerow,precol)
	    )
	    end);
















