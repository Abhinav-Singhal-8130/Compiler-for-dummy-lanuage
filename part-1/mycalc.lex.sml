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
"\003\003\003\003\003\003\003\003\003\009\011\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\009\003\003\003\003\003\003\003\008\007\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\006\003\003\003\003\
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
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\010\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
{fin = [(N 15)], trans = 0},
{fin = [(N 13),(N 15)], trans = 4},
{fin = [(N 13)], trans = 4},
{fin = [(N 10),(N 15)], trans = 0},
{fin = [(N 8),(N 15)], trans = 0},
{fin = [(N 6),(N 15)], trans = 0},
{fin = [(N 4),(N 15)], trans = 9},
{fin = [(N 4)], trans = 9},
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
| 10 => (lexedList := ("TERM \";\"" , (!row) , (!col)) :: (!lexedList)   ; inc (col,1);  Tokens.TERM(!row,!col))
| 13 => let val yytext=yymktext() in 
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
	 else (lexedList := ("ID \""^ yytext ^"\"" , (!row) , (!col)) :: (!lexedList) ; inc(col, size (yytext)) ;  Tokens.ID (yytext,!row,!col)) end
| 15 => let val yytext=yymktext() in let val prerow = !row
	     	val precol = !col 
	    in (
	    row := 1;
	    col := 1;
	    raise myexception (yytext,prerow,precol)
	    )
	    end end
| 4 => let val yytext=yymktext() in  inc (col, size (yytext)) ; lex() end
| 6 => (lexedList := ("LPAREN \"(\"" , (!row) , (!col)) :: (!lexedList) ; inc (col,1); Tokens.LPAREN(!row,!col))
| 8 => (lexedList := ("RPAREN \")\"" , (!row) , (!col)) :: (!lexedList) ; inc (col,1); Tokens.RPAREN(!row,!col))
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
