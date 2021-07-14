functor CalcLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Calc_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* User Declarations *)





end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\054\000\007\000\026\000\008\000\025\000\009\000\024\000\
\\010\000\023\000\012\000\022\000\016\000\020\000\017\000\019\000\
\\018\000\018\000\020\000\017\000\021\000\016\000\000\000\
\\001\000\001\000\070\000\030\000\069\000\000\000\
\\001\000\001\000\076\000\030\000\069\000\000\000\
\\001\000\001\000\077\000\030\000\069\000\000\000\
\\001\000\002\000\014\000\003\000\013\000\011\000\012\000\019\000\011\000\
\\022\000\010\000\025\000\009\000\026\000\029\000\027\000\007\000\000\000\
\\001\000\002\000\014\000\003\000\013\000\011\000\012\000\019\000\011\000\
\\022\000\010\000\025\000\009\000\026\000\029\000\027\000\007\000\
\\033\000\051\000\000\000\
\\001\000\002\000\049\000\000\000\
\\001\000\002\000\056\000\000\000\
\\001\000\002\000\066\000\028\000\065\000\029\000\064\000\000\000\
\\001\000\004\000\053\000\007\000\026\000\008\000\025\000\009\000\024\000\
\\010\000\023\000\012\000\022\000\016\000\020\000\017\000\019\000\
\\018\000\018\000\020\000\017\000\021\000\016\000\000\000\
\\001\000\005\000\062\000\007\000\026\000\008\000\025\000\009\000\024\000\
\\010\000\023\000\012\000\022\000\016\000\020\000\017\000\019\000\
\\018\000\018\000\020\000\017\000\021\000\016\000\000\000\
\\001\000\006\000\073\000\007\000\026\000\008\000\025\000\009\000\024\000\
\\010\000\023\000\012\000\022\000\016\000\020\000\017\000\019\000\
\\018\000\018\000\020\000\017\000\021\000\016\000\000\000\
\\001\000\007\000\026\000\008\000\025\000\009\000\024\000\010\000\023\000\
\\012\000\022\000\016\000\020\000\017\000\019\000\018\000\018\000\
\\020\000\017\000\021\000\016\000\024\000\061\000\000\000\
\\001\000\013\000\030\000\000\000\
\\001\000\015\000\000\000\000\000\
\\001\000\023\000\052\000\000\000\
\\001\000\026\000\027\000\000\000\
\\001\000\026\000\032\000\034\000\006\000\000\000\
\\001\000\026\000\055\000\000\000\
\\001\000\026\000\060\000\000\000\
\\001\000\030\000\069\000\031\000\080\000\000\000\
\\001\000\030\000\069\000\031\000\083\000\000\000\
\\001\000\032\000\059\000\000\000\
\\001\000\032\000\067\000\000\000\
\\001\000\032\000\075\000\000\000\
\\001\000\032\000\079\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\007\000\026\000\008\000\025\000\009\000\024\000\010\000\023\000\
\\012\000\022\000\014\000\021\000\016\000\020\000\017\000\019\000\
\\018\000\018\000\020\000\017\000\021\000\016\000\000\000\
\\089\000\000\000\
\\090\000\014\000\015\000\000\000\
\\091\000\002\000\014\000\003\000\013\000\011\000\012\000\019\000\011\000\
\\022\000\010\000\025\000\009\000\026\000\008\000\027\000\007\000\
\\034\000\006\000\000\000\
\\092\000\007\000\026\000\008\000\025\000\009\000\024\000\010\000\023\000\
\\012\000\022\000\016\000\020\000\017\000\019\000\018\000\018\000\
\\020\000\017\000\021\000\016\000\000\000\
\\093\000\007\000\026\000\008\000\025\000\009\000\024\000\010\000\023\000\
\\012\000\022\000\016\000\020\000\017\000\019\000\018\000\018\000\
\\020\000\017\000\021\000\016\000\000\000\
\\094\000\007\000\026\000\008\000\025\000\009\000\024\000\010\000\023\000\
\\012\000\022\000\016\000\020\000\017\000\019\000\018\000\018\000\
\\020\000\017\000\021\000\016\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\007\000\026\000\008\000\025\000\009\000\024\000\010\000\023\000\
\\012\000\022\000\016\000\020\000\017\000\019\000\018\000\018\000\
\\020\000\017\000\021\000\016\000\000\000\
\\101\000\016\000\020\000\017\000\019\000\018\000\018\000\020\000\017\000\
\\021\000\016\000\000\000\
\\102\000\016\000\020\000\017\000\019\000\018\000\018\000\020\000\017\000\
\\021\000\016\000\000\000\
\\103\000\016\000\020\000\017\000\019\000\018\000\018\000\020\000\017\000\
\\021\000\016\000\000\000\
\\104\000\016\000\020\000\017\000\019\000\018\000\018\000\020\000\017\000\
\\021\000\016\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\018\000\018\000\000\000\
\\108\000\018\000\018\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\016\000\020\000\017\000\019\000\018\000\018\000\000\000\
\\112\000\016\000\020\000\017\000\019\000\018\000\018\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\002\000\014\000\003\000\013\000\011\000\012\000\013\000\030\000\
\\019\000\011\000\022\000\010\000\025\000\009\000\026\000\029\000\
\\027\000\007\000\000\000\
\\117\000\002\000\014\000\003\000\013\000\011\000\012\000\019\000\011\000\
\\022\000\010\000\025\000\009\000\026\000\029\000\027\000\007\000\000\000\
\"
val actionRowNumbers =
"\031\000\030\000\028\000\026\000\
\\016\000\054\000\057\000\055\000\
\\017\000\004\000\004\000\004\000\
\\004\000\031\000\004\000\004\000\
\\004\000\004\000\004\000\031\000\
\\004\000\004\000\004\000\004\000\
\\004\000\006\000\056\000\058\000\
\\005\000\015\000\013\000\050\000\
\\045\000\009\000\000\000\029\000\
\\052\000\051\000\049\000\048\000\
\\047\000\027\000\044\000\042\000\
\\041\000\043\000\040\000\018\000\
\\032\000\007\000\004\000\004\000\
\\046\000\022\000\019\000\012\000\
\\010\000\008\000\023\000\053\000\
\\004\000\001\000\036\000\035\000\
\\008\000\008\000\011\000\008\000\
\\024\000\002\000\003\000\039\000\
\\038\000\008\000\037\000\025\000\
\\020\000\008\000\004\000\021\000\
\\034\000\004\000\033\000\014\000"
val gotoT =
"\
\\001\000\083\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\026\000\000\000\
\\000\000\
\\004\000\029\000\000\000\
\\003\000\031\000\000\000\
\\003\000\032\000\000\000\
\\003\000\033\000\000\000\
\\003\000\034\000\000\000\
\\002\000\035\000\003\000\002\000\004\000\001\000\000\000\
\\003\000\036\000\000\000\
\\003\000\037\000\000\000\
\\003\000\038\000\000\000\
\\003\000\039\000\000\000\
\\003\000\040\000\000\000\
\\002\000\041\000\003\000\002\000\004\000\001\000\000\000\
\\003\000\042\000\000\000\
\\003\000\043\000\000\000\
\\003\000\044\000\000\000\
\\003\000\045\000\000\000\
\\003\000\046\000\000\000\
\\000\000\
\\000\000\
\\003\000\026\000\000\000\
\\003\000\048\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\055\000\000\000\
\\003\000\056\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\061\000\000\000\
\\000\000\
\\000\000\
\\003\000\066\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\069\000\000\000\
\\005\000\070\000\000\000\
\\000\000\
\\005\000\072\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\076\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\079\000\000\000\
\\003\000\080\000\000\000\
\\000\000\
\\000\000\
\\003\000\082\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 84
val numrules = 32
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | BOOLCONST of unit ->  (string) | ID of unit ->  (string)
 | INTCONST of unit ->  (int) | mtype of unit ->  (PT.mytype)
 | declaration of unit ->  (PT.mydeclaration)
 | formula of unit ->  (PT.myformula)
 | statement of unit ->  (PT.mystatement)
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 14) => true | _ => false
val showTerminal =
fn (T 0) => "RPAREN"
  | (T 1) => "LPAREN"
  | (T 2) => "IF"
  | (T 3) => "THEN"
  | (T 4) => "ELSE"
  | (T 5) => "FI"
  | (T 6) => "IMPLIES"
  | (T 7) => "XOR"
  | (T 8) => "AND"
  | (T 9) => "OR"
  | (T 10) => "NOT"
  | (T 11) => "EQUALS"
  | (T 12) => "ASSIGN"
  | (T 13) => "TERM"
  | (T 14) => "EOF"
  | (T 15) => "PLUS"
  | (T 16) => "MINUS"
  | (T 17) => "TIMES"
  | (T 18) => "NEGATE"
  | (T 19) => "LESSTHAN"
  | (T 20) => "GREATERTHAN"
  | (T 21) => "LET"
  | (T 22) => "IN"
  | (T 23) => "END"
  | (T 24) => "INTCONST"
  | (T 25) => "ID"
  | (T 26) => "BOOLCONST"
  | (T 27) => "BOOLBASETYPE"
  | (T 28) => "INTBASETYPE"
  | (T 29) => "SINGLEARROW"
  | (T 30) => "DOUBLEARROW"
  | (T 31) => "COLON"
  | (T 32) => "FN"
  | (T 33) => "FUN"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 
2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.statement statement1, statement1left, 
statement1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  (statement as statement1) = statement1 ()
 in (
PT.statementToString (statement) ; 
		PT.evaluationOutput := PT.statementToAST (statement, []);
		(*case (PT.evalStatement( statement, [])) of 
		 PT.intVal(i) => print ("this is int output of statement: " ^ Int.toString (i) ^ "\n")
		| PT.boolVal (b) =>  print ("this is bool output of statement: " ^ Bool.toString(b) ^ "\n");*)
		  PT.STARTToString ()
)
end; ()))
 in ( LrTable.NT 0, ( result, statement1left, statement1right), 
rest671)
end
|  ( 1, ( ( _, ( MlyValue.statement statement1, _, statement1right))
 :: _ :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: 
rest671)) => let val  result = MlyValue.statement (fn _ => let val  
formula1 = formula1 ()
 val  statement1 = statement1 ()
 in (PT.fts(formula1,statement1) )
end)
 in ( LrTable.NT 1, ( result, formula1left, statement1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.formula formula1, formula1left, 
formula1right)) :: rest671)) => let val  result = MlyValue.statement
 (fn _ => let val  formula1 = formula1 ()
 in (PT.onlyFormula (formula1) )
end)
 in ( LrTable.NT 1, ( result, formula1left, formula1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.statement statement1, _, statement1right))
 :: _ :: ( _, ( MlyValue.declaration declaration1, declaration1left, _
)) :: rest671)) => let val  result = MlyValue.statement (fn _ => let
 val  declaration1 = declaration1 ()
 val  statement1 = statement1 ()
 in (PT.dts (declaration1,statement1))
end)
 in ( LrTable.NT 1, ( result, declaration1left, statement1right), 
rest671)
end
|  ( 4, ( ( _, ( MlyValue.declaration declaration1, declaration1left, 
declaration1right)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  declaration1 = declaration1 ()
 in (PT.onlyDeclaration (declaration1))
end)
 in ( LrTable.NT 1, ( result, declaration1left, declaration1right), 
rest671)
end
|  ( 5, ( rest671)) => let val  result = MlyValue.statement (fn _ => (
PT.snull))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 6, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: _ ::
 ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result
 = MlyValue.declaration (fn _ => let val  ID1 = ID1 ()
 val  formula1 = formula1 ()
 in (PT.varDecl (ID1, formula1))
end)
 in ( LrTable.NT 3, ( result, ID1left, formula1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: _ ::
 ( _, ( MlyValue.mtype mtype2, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.mtype mtype1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _))
 :: _ :: _ :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671))
 => let val  result = MlyValue.declaration (fn _ => let val  ID1 = ID1
 ()
 val  ID2 = ID2 ()
 val  mtype1 = mtype1 ()
 val  mtype2 = mtype2 ()
 val  formula1 = formula1 ()
 in (PT.funDeclLambda (ID1,ID2,mtype1,mtype2,formula1) )
end)
 in ( LrTable.NT 3, ( result, ID1left, formula1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: _ ::
 ( _, ( MlyValue.mtype mtype2, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.mtype mtype1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _))
 :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUN1left, _)) :: 
rest671)) => let val  result = MlyValue.declaration (fn _ => let val  
ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  mtype1 = mtype1 ()
 val  mtype2 = mtype2 ()
 val  formula1 = formula1 ()
 in (PT.funDeclHigher (ID1, ID2, mtype1, mtype2, formula1) )
end)
 in ( LrTable.NT 3, ( result, FUN1left, formula1right), rest671)
end
|  ( 9, ( ( _, ( _, BOOLBASETYPE1left, BOOLBASETYPE1right)) :: rest671
)) => let val  result = MlyValue.mtype (fn _ => (PT.atomicBool))
 in ( LrTable.NT 4, ( result, BOOLBASETYPE1left, BOOLBASETYPE1right), 
rest671)
end
|  ( 10, ( ( _, ( _, INTBASETYPE1left, INTBASETYPE1right)) :: rest671)
) => let val  result = MlyValue.mtype (fn _ => (PT.atomicInt))
 in ( LrTable.NT 4, ( result, INTBASETYPE1left, INTBASETYPE1right), 
rest671)
end
|  ( 11, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.mtype mtype1
, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.mtype (fn _ => let val  mtype1 = mtype1 ()
 in (PT.withParenType(mtype1))
end)
 in ( LrTable.NT 4, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.mtype mtype2, _, mtype2right)) :: _ :: ( _,
 ( MlyValue.mtype mtype1, mtype1left, _)) :: rest671)) => let val  
result = MlyValue.mtype (fn _ => let val  mtype1 = mtype1 ()
 val  mtype2 = mtype2 ()
 in (PT.compoundType (mtype1, mtype2))
end)
 in ( LrTable.NT 4, ( result, mtype1left, mtype2right), rest671)
end
|  ( 13, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.formula formula3
, _, _)) :: _ :: ( _, ( MlyValue.formula formula2, _, _)) :: _ :: ( _,
 ( MlyValue.formula formula1, _, _)) :: ( _, ( _, IF1left, _)) :: 
rest671)) => let val  result = MlyValue.formula (fn _ => let val  
formula1 = formula1 ()
 val  formula2 = formula2 ()
 val  formula3 = formula3 ()
 in ( PT.ite(formula1,formula2,formula3) )
end)
 in ( LrTable.NT 2, ( result, IF1left, FI1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (PT.withBoolBinop(formula1,PT.myimplies,formula2) )
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 15, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (PT.withBoolBinop(formula1,PT.myand,formula2) )
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 16, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (PT.withBoolBinop(formula1,PT.myor,formula2) )
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 17, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in ( PT.withBoolBinop(formula1,PT.myxor,formula2))
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 18, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in ( PT.withBoolBinop(formula1,PT.myequals,formula2))
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 19, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: ( _
, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  formula1 = formula1 ()
 in ( PT.withnot (formula1) )
end)
 in ( LrTable.NT 2, ( result, NOT1left, formula1right), rest671)
end
|  ( 20, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.formula (fn _ => let val  formula1 = formula1
 ()
 in ( PT.withparen (formula1))
end)
 in ( LrTable.NT 2, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (PT.withIntBinop (formula1, PT.myplus , formula2))
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 22, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (PT.withIntBinop (formula1, PT.myminus , formula2))
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 23, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (PT.withIntBinop (formula1, PT.mytimes , formula2))
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 24, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: ( _
, ( _, NEGATE1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  formula1 = formula1 ()
 in (PT.withnegate (formula1))
end)
 in ( LrTable.NT 2, ( result, NEGATE1left, formula1right), rest671)

end
|  ( 25, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (PT.lt (formula1,formula2))
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (PT.gt (formula1, formula2))
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 27, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: _ :: ( _, ( MlyValue.declaration declaration1, _,
 _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  declaration1 = declaration1 ()
 val  formula1 = formula1 ()
 in (PT.withLet (declaration1,formula1))
end)
 in ( LrTable.NT 2, ( result, LET1left, END1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.BOOLCONST BOOLCONST1, BOOLCONST1left, 
BOOLCONST1right)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  BOOLCONST1 = BOOLCONST1 ()
 in (PT.boolConst (BOOLCONST1))
end)
 in ( LrTable.NT 2, ( result, BOOLCONST1left, BOOLCONST1right), 
rest671)
end
|  ( 29, ( ( _, ( MlyValue.INTCONST INTCONST1, INTCONST1left, 
INTCONST1right)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  INTCONST1 = INTCONST1 ()
 in (PT.intConst (INTCONST1))
end)
 in ( LrTable.NT 2, ( result, INTCONST1left, INTCONST1right), rest671)

end
|  ( 30, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: ( _
, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  ID1 = ID1 ()
 val  formula1 = formula1 ()
 in (PT.funApplication (ID1,formula1))
end)
 in ( LrTable.NT 2, ( result, ID1left, formula1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.formula (fn _ => let val  ID1 = ID1 ()
 in ( PT.identifier (ID1) )
end)
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Calc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun INTCONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.INTCONST (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun BOOLCONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.BOOLCONST (fn () => i),p1,p2))
fun BOOLBASETYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun INTBASETYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun SINGLEARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun DOUBLEARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
end
end
