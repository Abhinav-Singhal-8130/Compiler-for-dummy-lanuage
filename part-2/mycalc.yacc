(* User Declarations *)




%%
(* grammar declarations *)

(* required declarations *)
%name Calc

%term 
	RPAREN | LPAREN 
	| IF | THEN | ELSE | FI
	| IMPLIES | XOR | AND | OR | NOT | EQUALS | ASSIGN
	
	| TERM
	| EOF 
	
	| PLUS | MINUS | TIMES | NEGATE 
	| LESSTHAN | GREATERTHAN
	
	| LET | IN | END 
	
	| INTCONST of int
	| ID of string 
	| BOOLCONST of string
	
	| BOOLBASETYPE | INTBASETYPE 
	
	| SINGLEARROW | DOUBLEARROW | COLON
	| FN | FUN 
	
	
%nonterm 
	START 
	| statement of PT.mystatement 
	| formula of PT.myformula
	| declaration of PT.mydeclaration
	| mtype of PT.mytype
	
		
%pos int 

(* optional declarations *) 
%eop EOF 
%noshift EOF 


(* PRECEDENCE RULES *)

%nonassoc LET IN END
%right ASSIGN
%right IF ELSE THEN
%right IMPLIES
%left AND OR XOR EQUALS

%left LESSTHAN GREATERTHAN
%left PLUS MINUS
%left TIMES
%right NOT NEGATE
%right ID
%left SINGLEARROW


%start START
(* %start START *)
%verbose 


%%

START: statement (PT.statementToString (statement) ; 
		PT.evaluationOutput := PT.statementToAST (statement, []);
		(*case (PT.evalStatement( statement, [])) of 
		 PT.intVal(i) => print ("this is int output of statement: " ^ Int.toString (i) ^ "\n")
		| PT.boolVal (b) =>  print ("this is bool output of statement: " ^ Bool.toString(b) ^ "\n");*)
		  PT.STARTToString ())

statement: formula TERM statement (PT.fts(formula1,statement1) )
	| formula (PT.onlyFormula (formula1) )
	| declaration TERM statement (PT.dts (declaration1,statement1))
	| declaration (PT.onlyDeclaration (declaration1)) 
	| (PT.snull)
		
declaration: ID ASSIGN formula (PT.varDecl (ID1, formula1))
	| ID ASSIGN FN LPAREN ID COLON mtype RPAREN COLON mtype DOUBLEARROW formula (PT.funDeclLambda (ID1,ID2,mtype1,mtype2,formula1) )
	| FUN ID LPAREN ID COLON mtype RPAREN COLON mtype DOUBLEARROW formula (PT.funDeclHigher (ID1, ID2, mtype1, mtype2, formula1) )

mtype: BOOLBASETYPE (PT.atomicBool)
	| INTBASETYPE (PT.atomicInt)
	| LPAREN mtype RPAREN (PT.withParenType(mtype1))
	| mtype SINGLEARROW mtype (PT.compoundType (mtype1, mtype2))
	

formula: 
	IF formula THEN formula ELSE formula FI ( PT.ite(formula1,formula2,formula3) )
	
	| formula IMPLIES formula (PT.withBoolBinop(formula1,PT.myimplies,formula2) )
	| formula AND formula (PT.withBoolBinop(formula1,PT.myand,formula2) )
	| formula OR formula (PT.withBoolBinop(formula1,PT.myor,formula2) )
	| formula XOR formula ( PT.withBoolBinop(formula1,PT.myxor,formula2))
	| formula EQUALS formula ( PT.withBoolBinop(formula1,PT.myequals,formula2))
	| NOT formula ( PT.withnot (formula1) )
	
	| LPAREN formula RPAREN ( PT.withparen (formula1))
	
	| formula PLUS formula (PT.withIntBinop (formula1, PT.myplus , formula2))
	| formula MINUS formula (PT.withIntBinop (formula1, PT.myminus , formula2))
	| formula TIMES formula (PT.withIntBinop (formula1, PT.mytimes , formula2))
	| NEGATE formula (PT.withnegate (formula1))
	| formula LESSTHAN formula (PT.lt (formula1,formula2))
	| formula GREATERTHAN formula (PT.gt (formula1, formula2))
	
	| LET declaration IN formula END (PT.withLet (declaration1,formula1))
	| BOOLCONST (PT.boolConst (BOOLCONST1))
	| INTCONST (PT.intConst (INTCONST1))
	| ID formula (PT.funApplication (ID1,formula1))
	| ID ( PT.identifier (ID1) ) 
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

