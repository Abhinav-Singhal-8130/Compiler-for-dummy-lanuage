(* User Declarations *)




%%
(* grammar declarations *)

(* required declarations *)
%name Calc

%term 
	RPAREN | LPAREN 
	| IF | THEN | ELSE | IMPLIES | EQUALS 
	| XOR | AND | OR | NOT 
	| ID of string 
	| CONST of string
	| TERM
	| EOF 
	
%nonterm 
	START 
	| program
	| statement of PT.mystatement 
	| formula of PT.myformula
		
%pos int 

(* optional declarations *) 
%eop EOF 
%noshift EOF 

%right IF ELSE THEN
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT


%start START
(* %start START *)
%verbose 


%%

START: program (PT.STARTToString ())

program:  statement (PT.programToString())
	
	
statement: formula TERM statement (PT.statementToString( (PT.fts(formula1,statement1)) :PT.mystatement ); PT.fts(formula1,statement1) )
	| (PT.snull)
	
	
formula: 
	IF formula THEN formula ELSE formula ( PT.ite(formula1,formula2,formula3) )
	| formula IMPLIES formula (PT.withbinop(formula1,PT.myimplies,formula2) )
	| formula AND formula (PT.withbinop(formula1,PT.myand,formula2) )
	| formula OR formula (PT.withbinop(formula1,PT.myor,formula2) )
	| formula XOR formula ( PT.withbinop(formula1,PT.myxor,formula2))
	| formula EQUALS formula ( PT.withbinop(formula1,PT.myequals,formula2))
	| NOT formula ( PT.withnot (formula1) )
	| LPAREN formula RPAREN ( PT.withparen (formula1) )
	| CONST (PT.const(CONST1))
	| ID ( PT.id (ID1) ) 

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	(*
program: statement ()

		
statement: formula TERM statement ()
	| ()
	
	
formula: 
	IF formula THEN formula ELSE formula ()
	| formula IMPLIES formula (PT.formulaToString ()
	| formula AND formula (PT.formulaToString ()
	| formula OR formula (PT.formulaToString ()
	| formula XOR formula (PT.formulaToString ()
	| formula EQUALS formula (PT.formulaToString ()
	| NOT formula ()
	| LPAREN formula RPAREN ()
	| CONST (PT.formulaToString ()
	| ID () 
	*)









