functor CalcLexFun (structure Tokens: Calc_TOKENS)=
   struct
    structure UserDeclarations =
      struct


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
	
		
	
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\017\021\003\003\019\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\017\003\003\003\003\003\003\003\016\015\003\003\003\013\003\003\
\\011\011\011\011\011\011\011\011\011\011\010\009\003\007\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003"
),
 (4, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (6, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (7, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\008\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (11, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (17, 
"\000\000\000\000\000\000\000\000\000\018\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\018\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (19, 
"\000\000\000\000\000\000\000\000\000\000\020\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 35)], trans = 0},
{fin = [(N 26),(N 33),(N 35)], trans = 4},
{fin = [(N 26),(N 33)], trans = 4},
{fin = [(N 33)], trans = 6},
{fin = [(N 21),(N 35)], trans = 7},
{fin = [(N 17)], trans = 0},
{fin = [(N 19),(N 35)], trans = 0},
{fin = [(N 23),(N 35)], trans = 0},
{fin = [(N 29),(N 35)], trans = 11},
{fin = [(N 29)], trans = 11},
{fin = [(N 35)], trans = 13},
{fin = [(N 14)], trans = 0},
{fin = [(N 11),(N 35)], trans = 0},
{fin = [(N 9),(N 35)], trans = 0},
{fin = [(N 7),(N 35)], trans = 17},
{fin = [(N 7)], trans = 17},
{fin = [(N 35)], trans = 19},
{fin = [(N 4)], trans = 0},
{fin = [(N 1)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => (inc (row,1) ; col := 1; inc (pos,1) ; lex())
| 11 => (lexedList := ("RPAREN \")\"" , (!row) , (!col)) :: (!lexedList) ; inc (col,1); Tokens.RPAREN(!row,!col))
| 14 => (lexedList := ("SINGLEARROW \"->\"" , (!row) , (!col)) :: (!lexedList)   ; inc (col,2);  Tokens.SINGLEARROW(!row,!col))
| 17 => (lexedList := ("DOUBLEARROW \"=>\"" , (!row) , (!col)) :: (!lexedList)   ; inc (col,2);  Tokens.DOUBLEARROW(!row,!col))
| 19 => (lexedList := ("TERM \";\"" , (!row) , (!col)) :: (!lexedList)   ; inc (col,1);  Tokens.TERM(!row,!col))
| 21 => (lexedList := ("ASSIGN \"=\"" , (!row) , (!col)) :: (!lexedList)   ; inc (col,1);  Tokens.ASSIGN(!row,!col))
| 23 => (lexedList := ("COLON \":\"" , (!row) , (!col)) :: (!lexedList)   ; inc (col,1);  Tokens.COLON(!row,!col))
| 26 => let val yytext=yymktext() in 

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
 end
| 29 => let val yytext=yymktext() in lexedList := ("INTCONST \"" ^ yytext ^ "\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;
				 Tokens.INTCONST (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),!row,!col) end
| 33 => let val yytext=yymktext() in lexedList := ("ID \""^ yytext ^"\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;  Tokens.ID (yytext,!row,!col) end
| 35 => let val yytext=yymktext() in let val prerow = !row
	     	val precol = !col 
	    in (
	    row := 1;
	    col := 1;
	    raise myexception (yytext,prerow,precol)
	    )
	    end end
| 4 => (inc (row,1) ; col := 1; inc (pos,1) ; lex())
| 7 => let val yytext=yymktext() in  inc (col, size (yytext)) ; lex() end
| 9 => (lexedList := ("LPAREN \"(\"" , (!row) , (!col)) :: (!lexedList) ; inc (col,1); Tokens.LPAREN(!row,!col))
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
