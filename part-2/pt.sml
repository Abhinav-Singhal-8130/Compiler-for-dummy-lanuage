structure PT = 
struct 


exception brokenTypes of string; (*Fail "Error in evaluation!"*)
exception typeIncorrect of string;
exception notABool of string;
exception scopeError of string;
exception functionEvaluationError of string;
exception unknownError of string;

(*------------------x----------------------------x---------DEFINITIONS------------------x-----------------------------------x-------------------------*)

val evaluationOutput :string ref = ref "";

datatype mytype = atomicInt
				| atomicBool
				| compoundType of (mytype * mytype)
				| withParenType of (mytype)
				

datatype mystatement = fts of (myformula * mystatement)
			| onlyFormula of myformula
			| dts of (mydeclaration * mystatement)
			| onlyDeclaration of mydeclaration
			| snull 
			
and myformula = 	ite of (myformula * myformula * myformula) 
			| identifier of string 							
			| intConst of int                           
			| boolConst of string                         
			
			| withBoolBinop of  myformula * boolBinop * myformula 
			| withnot of myformula                        
			| withparen of myformula                      
			| withLet of mydeclaration * myformula 		   
			| withnegate of myformula                          
			| withIntBinop of myformula * intBinop * myformula 
			| funApplication of  string * myformula
			| lt of (myformula * myformula)
			| gt of (myformula * myformula)
			
			
			
			


and boolBinop = myimplies | myequals | myxor | myand | myor 
and intBinop = myplus | myminus | mytimes
and value = 
	intVal of int 
	| boolVal of bool
	| funVal of (string * mytype * mytype * myformula) (*argumentName : domainType : rangeType : expression *)
	| funcArg  (*its value is unknown at the time of declaration*)
	| nullValue 
	
and mydeclaration = varDecl of (string * myformula) 
				| funDeclHigher of (string * string * mytype * mytype * myformula) (*funName : inputArgName : domainType : rangeType : expression*)
				| funDeclLambda of (string * string * mytype * mytype * myformula) 
				
				
				
fun typesAreSame (t1 :mytype, t2 :mytype) :bool = (
	 case (t1,t2) of 
		(atomicInt,atomicInt) => true
		| (atomicBool,atomicBool) => true
		| (withParenType (t),_) => typesAreSame (t,t2) 
		| (_,withParenType (tt)) => typesAreSame (t1,tt)
		| (compoundType(t11,t12), compoundType (t21,t22)) => typesAreSame (t11,t21) andalso typesAreSame (t12, t22)
		| _ => false
)

fun typeToString (t : mytype) :string = (
	case t of 
		atomicInt => "int"
		| atomicBool => "boolean"
		| withParenType (t1) => "(" ^ typeToString (t1) ^ ")"
		| compoundType (t1,t2) =>   typeToString (t1) ^ "->" ^  typeToString (t2) 
)
		
(*------------------x----------------------------x-------EVALUATION AND GENERATING ABSTRACT SYNTAX TREE--------------------x-----------------------------------x-------------------------*)
	type environment = (string * value) list 

fun envAdd (var :string, v:value, env:environment) =
    (var,v)::env

fun envLookup (var :string, env:environment) =
    case List.find(fn (x, _) => x = var) env of
	SOME (x, v)   => v
	| NONE => raise scopeError ("SCOPE ERROR::Could not find :" ^ var^ ": in current environment::TERMINATED\n")
	
fun envPop (nil) = raise scopeError ("SCOPE ERROR:: Cannot pop from empty environment::TERMINATED\n")
| envPop (h::nil) = nil
| envPop (h::t) = t


fun evalFormula (f : myformula , env : environment) : value = (
	case f of 
		intConst (i) => intVal (i)
		| boolConst (b) => (
			case b of 
				"TRUE" => boolVal(true)
				| "FALSE" => boolVal(false)
				| _ => raise notABool ("EVALUATION ERROR::Not a bool::This error should not be reported::TERMINATED\n")
		)
		| identifier (name) => envLookup (name,env)
		| withparen (f1) => evalFormula (f1,env)
		| withnot (f1) => (
			case evalFormula (f1,env) of 
				boolVal (b) => if (b) then (boolVal (false)) else (boolVal(true))
				| _ => raise typeIncorrect ("TYPE ERROR::Type Mismatch in not formula production rule::Terminated\n")
		)
		| withnegate (f1) => ( 
			case evalFormula (f1,env) of 
				intVal (i1) => (intVal (~i1))
				| _ => raise typeIncorrect ("TYPE ERROR::Type Mismatch in negate formula production rule::Terminated\n")
		)
		| ite (f1, f2, f3) => (	
			case (evalFormula (f1,env),evalFormula (f2,env),evalFormula (f3,env)) of 
				(boolVal (b), intVal (i2), intVal (i3)) => if (b) then intVal (i2) else intVal (i3)
				| (boolVal (b), boolVal (b2), boolVal (b3)) => if (b) then boolVal (b2) else (boolVal (b3))
				| (boolVal (b), funVal (str1,at1,at2,mf2), funVal (str2,at3,at4,mf3)) => if (b) then funVal (str1,at1,at2,mf2) else funVal (str2,at3,at4,mf3)
				| _ => raise typeIncorrect ("TYPE ERROR::Type Mismatch in if then else fi condition::Terminated\n")
		)
		| lt (f1,f2) => (
			case (evalFormula (f1,env),evalFormula (f2,env)) of 
			(intVal (i1), intVal (i2)) => if (i1 < i2) then boolVal (true) else boolVal (false)
			| _ =>  raise typeIncorrect ("TYPE ERROR::Type Mismatch in formula LESSTHAN formula condition::Terminated\n")
		)
		| gt (f1,f2) => (	
			case (evalFormula (f1,env),evalFormula (f2,env)) of 
			(intVal (i1), intVal (i2)) => if (i1 > i2) then boolVal (true) else boolVal (false)
			| _ =>  raise typeIncorrect ("TYPE ERROR::Type Mismatch in formula GREATERTHAN formula condition::Terminated\n")
		)
		| withBoolBinop (f1, bop, f2) => (			
			myData.evalList := "{ BoolBinop" :: !(myData.evalList);
			case (bop,evalFormula (f1,env),evalFormula (f2,env)) of 
				(myimplies, boolVal(b1), boolVal(b2)) => (case (b1,b2) of 
																(true,true) =>  (boolVal (true))
																| (true,false) => (boolVal (false))
																| (false,true) => (boolVal (true))
																| (false, false) => (boolVal (true)))
				| (myor, boolVal(b1), boolVal(b2)) => (case (b1,b2) of 
																(true,true) => (boolVal (true))
																| (true,false) => (boolVal (true))
																| (false,true) => (boolVal (true))
																| (false, false) => (boolVal (false)))
				| (myxor, boolVal(b1), boolVal(b2)) => (case (b1,b2) of 
																(true,true) => ( boolVal (false))
																| (true,false) => (boolVal (true))
																| (false,true) => (boolVal (true))
																| (false, false) => (boolVal (false)))
				| (myand, boolVal(b1), boolVal(b2)) => (case (b1,b2) of 
																(true,true) => (boolVal (true))
																| (true,false) => ( boolVal (false))
																| (false,true) => ( boolVal (false))
																| (false, false) => ( boolVal (false)))
				| (myequals, boolVal(b1), boolVal(b2)) => (case (b1,b2) of 
																(true,true) => ( boolVal (true))
																| (true,false) => (boolVal (false))
																| (false,true) => (boolVal (false))
																| (false, false) => (boolVal (true)))
				| (myequals, intVal(i1), intVal(i2)) => (if (i1 = i2) then (boolVal (true)) else (boolVal (false)))
				| _ => raise typeIncorrect ("TYPE ERROR::Type Mismatch in formula boolBinop formula::Terminated\n")
		)
		| withIntBinop (f1, intbop, f2) => (
			case (intbop,evalFormula (f1,env),evalFormula (f2,env)) of 
				(myplus, intVal (i1), intVal (i2)) => (intVal (i1+i2))
				| (myminus, intVal (i1), intVal (i2)) => (intVal (i1-i2))
				| (mytimes, intVal (i1), intVal (i2)) => (intVal (i1 * i2))
				| _ => raise typeIncorrect ("TYPE ERROR::Type Mismatch in formula intBinop formula::Terminated\n") 	
		)
		| withLet (dec, f1) => (
			case dec of 
				varDecl(tempVar,tempFormula) => evalFormula (f1, envAdd (tempVar, evalFormula (tempFormula,env), env)  )	
				| funDeclLambda (funName,argName,domain,range,funcFormula) => evalFormula (f1, envAdd (funName,funVal (argName,domain,range,funcFormula), env))
				| funDeclHigher (funName,argName,domain,range,funcFormula) => evalFormula (f1, envAdd (funName,funVal (argName,domain,range,funcFormula), env)) 
		)
		| funApplication (funName, argFormula) => (
			case (envLookup (funName,env) , evalFormula (argFormula,env)) of 
				(funVal (appArgName,atomicInt,appRangeType,appExpFormula) ,intVal (i2)) => evalFormula (appExpFormula, envAdd (appArgName, intVal (i2), env))
				| (funVal (appArgName,atomicBool,appRangeType,appExpFormula), boolVal (b2)) => evalFormula (appExpFormula, envAdd (appArgName, boolVal (b2), env))
				| ( funVal (appArgName, appDomainType,appRangeType,appExpFormula) , funVal (argName, domainType, rangeType, expressionFormula)) => (
					if (typesAreSame (appDomainType,compoundType (domainType,rangeType))) then evalFormula (appExpFormula, envAdd (appArgName,funVal (argName,domainType,rangeType,expressionFormula),env)) 
					else raise typeIncorrect ("TYPE ERROR::Type Mismatch in higher order function::Terminated\n")	
				)
				| _ => raise functionEvaluationError ("EVALUATION ERROR::some problem in evaluating functions111::TERMINATED\n") 
		)
)

fun evalStatement (s : mystatement, env : environment) : value = (
	case s of 
		fts (f1,s1) => (
			evalFormula (f1,env);
			evalStatement (s1,env)
		)
		| onlyFormula (f1) => evalFormula (f1,env)
		| dts (dec,s1) => (
			case dec of 
			varDecl (varName, expressionFormula) => evalStatement (s1, envAdd (varName,evalFormula (expressionFormula, env) ,env)  )
			| funDeclLambda (funName, inputArgName, domainType, rangeType, expressionFormula) => evalStatement (s1, envAdd (funName,funVal (inputArgName,domainType,rangeType,expressionFormula),env))
			| funDeclHigher (funName, inputArgName, domainType, rangeType, expressionFormula) => evalStatement (s1, envAdd (funName,funVal (inputArgName,domainType,rangeType,expressionFormula),env))
			
		)
		| onlyDeclaration (dec) => (
			case dec of 
			varDecl (varName, expressionFormula) => env = envAdd (varName,evalFormula (expressionFormula, env),env)
			| funDeclLambda (funName, inputArgName, domainType, rangeType, expressionFormula) => env = envAdd (funName,funVal (inputArgName,domainType,rangeType,expressionFormula), env)
			| funDeclHigher (funName, inputArgName, domainType, rangeType, expressionFormula) => env = envAdd (funName,funVal (inputArgName,domainType,rangeType,expressionFormula), env) ;
			nullValue 
		)
		| snull => nullValue
)

fun getFuncName (f :myformula) :string = (
	case f of 
	identifier (name) => name
	| _ => raise unknownError ("f is not of identifier type")
)

fun funToString (env :environment,argumentName :string,domainType :mytype, rangeType :mytype, expression : myformula) :string = (
	"{FUN |\"" ^ argumentName ^ "\"| domain: " ^ typeToString (domainType) ^ "| range: " ^ typeToString (rangeType) ^ "| " ^ formulaToAST (expression,envAdd (argumentName,funcArg,env))  ^ " end}"   
)




and valueToAST (v : value, env :environment) :string = (
	case v of 
	intVal (i) => Int.toString (i)
	| boolVal (b) => Bool.toString (b)
	| funVal (argumentName, domainType, rangeType, expression) => funToString (env, argumentName, domainType, rangeType, expression) (*argumentName : domainType : rangeType : expression *)
	| funcArg => "FunArg"
	| nullValue => "nullValue"
)

and formulaToAST (f :myformula, env :environment) :string = (
	case f of 
	intConst (i) => Int.toString(i)
	| boolConst (b) => if (b = "TRUE") then "true" else "false"
	| identifier (name) =>  name ^ " = " ^ valueToAST (envLookup (name,env), env)
	| withparen (f1) => formulaToAST (f1,env)
	| withnot (f1) => (
		case evalFormula (f1,env) of 
				boolVal (b) => if (b) then ("{ not| " ^ formulaToAST (f1,env) ^ " |not = false}") else ("{ not| " ^ formulaToAST (f1,env) ^ " |not = true}")
				| funcArg => "{ not| " ^ formulaToAST (f1,env) ^ " |not }"
				| _ => raise typeIncorrect ("TYPE ERROR::Type Mismatch in not formula production rule::Terminated\n")
	)
	| withnegate (f1) => ( 
		case evalFormula (f1,env) of 
			intVal (i1) => "{ negate| " ^  formulaToAST (f1,env) ^ " |negate = " ^ Int.toString (~i1) ^ "}"
			| funcArg => "{ negate| " ^  formulaToAST (f1,env) ^ " |negate}"
			| _ => raise typeIncorrect ("TYPE ERROR::Type Mismatch in negate formula production rule::Terminated\n")
	)
	| ite (f1, f2, f3) => (		
			case (evalFormula (f1,env),evalFormula (f2,env),evalFormula (f3,env)) of 
			
				(boolVal (b), intVal (i2), intVal (i3)) => if (b) then ( "{ ite = true| " ^ formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | " ^ formulaToAST (f3,env) ^ " | ite = " ^ Int.toString (i2) ^ "}") 
																else ( "{ ite = false| " ^ formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | " ^ formulaToAST (f3,env) ^ " | ite = " ^ Int.toString (i3) ^ "}")
				| (boolVal (b), boolVal (b2), boolVal (b3)) => if (b) then ("{ ite = true| " ^ formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | " ^ formulaToAST (f3,env) ^ " | ite = " ^ Bool.toString (b2) ^ "}") 
																	else  ("{ ite = false| " ^ formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | " ^ formulaToAST (f3,env) ^ " | ite = " ^ Bool.toString (b3) ^ "}")
				| (boolVal (b), funVal (str1,at1,at2,mf2), funVal (str2,at3,at4,mf3)) => if (b) then ("{ ite = true| "^formulaToAST(f1,env)^" | "^getFuncName (f2)^" | "^getFuncName(f3)^" | ite = " ^ getFuncName (f2)^"}") 
																								else ("{ ite = false| "^formulaToAST(f1,env)^" | "^getFuncName(f2)^" | "^getFuncName(f3)^" | ite = " ^ getFuncName (f3)^"}")
				
				| (funcArg,_,_) =>  "{ ite | " ^ formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | " ^ formulaToAST (f3,env) ^ " | ite }"
				| (_,funcArg,_) => "{ ite | " ^ formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | " ^ formulaToAST (f3,env) ^ " | ite }"
				| (_,_,funcArg) => "{ ite | " ^ formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | " ^ formulaToAST (f3,env) ^ " | ite }"
				| _ => raise typeIncorrect ("TYPE ERROR::Type Mismatch in if then else fi condition::Terminated\n")	
	)
	
	| lt (f1,f2) => (
			myData.evalList := "{ lt" :: !(myData.evalList);
			case (evalFormula (f1,env),evalFormula (f2,env)) of 
			(intVal (i1), intVal (i2)) => if (i1 < i2) then ( "{ lt| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | lt = true}")
														else ("{ lt| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | lt = false}")
			| (funcArg, _) => "{ lt| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | lt }"
			| (_,funcArg) => "{ lt| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | lt }"
			| _ =>  raise typeIncorrect ("TYPE ERROR::Type Mismatch in formula LESSTHAN formula condition::Terminated\n")
	)
		
	| gt (f1,f2) => (
		case (evalFormula (f1,env),evalFormula (f2,env)) of 
		(intVal (i1), intVal (i2)) => if (i1 > i2) then ("{ gt| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | gt = true}") 
													else ("{ gt| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | gt = false}")
													
		| (funcArg, _) => "{ gt| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | gt }"
		| (_,funcArg) => "{ gt| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | gt }"
		| _ =>  raise typeIncorrect ("TYPE ERROR::Type Mismatch in formula GREATERTHAN formula condition::Terminated\n")
	)
	| withBoolBinop (f1, bop, f2) => (			
			myData.evalList := "{ BoolBinop" :: !(myData.evalList);
			case (bop,evalFormula (f1,env),evalFormula (f2,env)) of 
				(myimplies, boolVal(b1), boolVal(b2)) => (case (b1,b2) of 
																(true,true) =>  ("{ implies| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | implies = true}")
																| (true,false) => ("{ implies| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | implies = false}")
																| (false,true) => ("{ implies| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | implies = true}")
																| (false, false) => ("{ implies| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | implies = true}"))
				| (myimplies, funcArg, _) => "{ implies| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | implies}"
				| (myimplies,_,funcArg) => "{ implies| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | implies}"
				
				| (myor, boolVal(b1), boolVal(b2)) => (case (b1,b2) of 
																(true,true) => ("{ or| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | or = true}")
																| (true,false) =>  ("{ or| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | or = true}")
																| (false,true) =>  ("{ or| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | or = true}")
																| (false, false) =>  ("{ or| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | or = false}"))
				| (myor, funcArg, _) => "{ or| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | or}"
				| (myor,_,funcArg) => "{ or| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | or}"
				
				| (myxor, boolVal(b1), boolVal(b2)) => (case (b1,b2) of 
																(true,true) => ("{ xor| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | xor = false}")
																| (true,false) => ("{ xor| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | xor = true}")
																| (false,true) => ("{ xor| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | xor = true}")
																| (false, false) => ("{ xor| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | xor = false}"))
				| (myxor, funcArg, _) => "{ xor| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | xor}"
				| (myxor,_,funcArg) => "{ xor| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | xor}"												
				
				| (myand, boolVal(b1), boolVal(b2)) => (case (b1,b2) of 
																(true,true) => ("{ and| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | and = true}")
																| (true,false) => ("{ and| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | and = false}")
																| (false,true) => ("{ and| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | and = false}")
																| (false, false) => ("{ and| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | and = false}"))
				| (myand, funcArg, _) => "{ and| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | and}"
				| (myand,_,funcArg) => "{ and| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | and}"
				| (myequals, boolVal(b1), boolVal(b2)) => (case (b1,b2) of 
																(true,true) => ("{ equals| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | equals = true}")
																| (true,false) => ("{ equals| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | equals = false}")
																| (false,true) => ("{ equals| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | equals = false}")
																| (false, false) => ("{ equals| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | equals = true}"))
				| (myequals, intVal(i1), intVal(i2)) => (if (i1 = i2) then ("{ equals| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | equals = true}") 
																	else ("{ equals| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | equals = false}"))												
				| (myequals, funcArg, _) => "{ equals| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | equals}"
				| (myequals,_,funcArg) => "{ equals| " ^  formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | equals}"													
				
				| _ => raise typeIncorrect ("TYPE ERROR::Type Mismatch in formula boolBinop formula::Terminated\n")	
		)
		| withIntBinop (f1, intbop, f2) => (
			case (intbop,evalFormula (f1,env),evalFormula (f2,env)) of 
				(myplus, intVal (i1), intVal (i2)) => ("{ plus| " ^ formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | plus = " ^ Int.toString (i1 + i2)  ^ "}")
				| (myplus, funcArg, _) => "{ plus| " ^ formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | plus }"
				| (myplus, _,funcArg) => "{ plus| " ^ formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | plus }"
				
				| (myminus, intVal (i1), intVal (i2)) => ("{ minus| " ^ formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | minus = " ^ Int.toString (i1 - i2)  ^ "}")
				| (myminus, funcArg, _) => "{ minus| " ^ formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | minus }"
				| (myminus, _,funcArg) => "{ minus| " ^ formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | minus }"
				
				| (mytimes, intVal (i1), intVal (i2)) => ("{ times| " ^ formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | times = " ^ Int.toString (i1 * i2)  ^ "}")
				| (mytimes, funcArg, _) => "{ times| " ^ formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | times }"
				| (mytimes, _,funcArg) => "{ times| " ^ formulaToAST (f1,env) ^ " | " ^ formulaToAST (f2,env) ^ " | times }"
				
				| _ => raise typeIncorrect ("TYPE ERROR::Type Mismatch in formula intBinop formula::Terminated\n") 	
		)
		| withLet (dec, f1) => (
			case dec of 
				varDecl(tempVar,tempFormula) 							   => "{ let| "^decToAST(dec,env)^" | in | "^formulaToAST(f1,envAdd (tempVar, evalFormula (tempFormula,env), env)) ^ "| end}"
				| funDeclLambda (funName,argName,domain,range,funcFormula) => "{ let| "^decToAST(dec,env)^" | in | "^formulaToAST(f1,envAdd (funName,funVal (argName,domain,range,funcFormula), env)) ^ "| end}"
				| funDeclHigher (funName,argName,domain,range,funcFormula) => "{ let| "^decToAST(dec,env)^" | in | "^formulaToAST(f1,envAdd (funName,funVal (argName,domain,range,funcFormula), env)) ^ "| end}"
		)
		| funApplication (funName, argFormula) => (
			case (envLookup (funName,env) , evalFormula (argFormula,env)) of 
				(funVal (appArgName,atomicInt,appRangeType,appExpFormula) ,intVal (i2)) => 					
					"{ funApp| " ^ funName ^ " | " ^ formulaToAST (appExpFormula, envAdd (appArgName, intVal (i2), env)) ^ 
					" | funApp = " ^ valueToAST (evalFormula (appExpFormula, envAdd (appArgName, intVal (i2), env)), envAdd (appArgName, intVal (i2), env) ) ^ "}"
				
				| (funVal (appArgName,atomicBool,appRangeType,appExpFormula), boolVal (b2)) => 
					"{ funApp| " ^ funName ^ " | " ^ formulaToAST (appExpFormula, envAdd (appArgName, boolVal (b2), env)) ^ 
					" | funApp = " ^ valueToAST (evalFormula (appExpFormula,envAdd(appArgName, boolVal (b2), env)),envAdd(appArgName, boolVal (b2), env) ) ^ "}"
				
				| ( funVal (appArgName, appDomainType,appRangeType,appExpFormula) , funVal (argName, domainType, rangeType, expressionFormula)) => (
					if (typesAreSame (appDomainType,compoundType (domainType,rangeType))) then  
							"{ funApp| " ^ funName ^ " | " ^ formulaToAST (appExpFormula, envAdd (appArgName,funVal (argName,domainType,rangeType,expressionFormula),env)) ^ 
							" | funApp = " ^ valueToAST (evalFormula (appExpFormula, envAdd (appArgName,funVal (argName,domainType,rangeType,expressionFormula),env)), envAdd (appArgName,funVal (argName,domainType,rangeType,expressionFormula),env)) ^ "}"		
			
					else raise typeIncorrect ("TYPE ERROR::Type Mismatch in higher order function::Terminated\n")
					
				)
				
				| (funcArg,funcArg) => "{ funApp| " ^ funName ^ " = FunArg | " ^ "FunArg" ^ "}"
				| (funcArg,_) => "{ funApp| " ^ funName ^ " = FunArg | " ^ formulaToAST (argFormula, env) ^ "}"
				| (_,funcArg) => "{ funApp| " ^ funName ^ " | FunArg " ^ "}" 
				| _ => raise functionEvaluationError ("EVALUATION ERROR::some problem in evaluating functions222::TERMINATED\n") 
		)
)

and decToAST(dec :mydeclaration ,env : environment) :string = (
	case dec of 
		varDecl (varName, f) =>  "{ varDecl | " ^ varName ^ " = " ^ formulaToAST (f,env) ^ " |varDecl end}"
		| funDeclLambda (funName,argName,domain,range,funcFormula) => "{ lambdaFuncDecl | " ^ funName ^ " = " ^ funToString (env,argName,domain,range,funcFormula) ^ " |lambdaFunDecl end}"
		| funDeclHigher (funName,argName,domain,range,funcFormula) => "{ HigherFuncDecl | " ^ funName ^ " = " ^ funToString (env,argName,domain,range,funcFormula) ^ " |higherFunDecl end}" 	
)

and statementToAST (s : mystatement, env :environment) :string = (
	case s of 
		fts (f1,s1) => "{ statement | " ^ formulaToAST (f1, env) ^ " |statement end}" ^ "\n\n" ^ statementToAST (s1,env) 
		| onlyFormula (f1) => "{ statement | " ^ formulaToAST (f1, env) ^ " |statement end}"
		| dts (dec,s1) => ( 
			case dec of 
			varDecl (varName, expressionFormula) => "{ statement | " ^ decToAST (dec, env) ^ " |statement end}" ^ "\n\n" ^ statementToAST (s1,envAdd (varName,evalFormula (expressionFormula, env) ,env))
			| funDeclLambda (funName, inputArgName, domainType, rangeType, expressionFormula) => "{ statement | " ^ decToAST (dec, env) ^ " |statement end}" ^ "\n\n" ^ 
																								statementToAST (s1,envAdd (funName,funVal (inputArgName,domainType,rangeType,expressionFormula),env))
			| funDeclHigher (funName, inputArgName, domainType, rangeType, expressionFormula) => "{ statement | " ^ decToAST (dec, env) ^ " |statement end}" ^ "\n\n" ^ 
																								statementToAST (s1,envAdd (funName,funVal (inputArgName,domainType,rangeType,expressionFormula),env))
		)
		| onlyDeclaration (dec) => (
			case dec of 
			varDecl (varName, expressionFormula) => "{ statement | " ^ decToAST (dec, env) ^ " |statement end}" ^ "\n\n"
			| funDeclLambda (funName, inputArgName, domainType, rangeType, expressionFormula) => "{ statement | " ^ decToAST (dec, env) ^ " |statement end}" ^ "\n\n"
			| funDeclHigher (funName, inputArgName, domainType, rangeType, expressionFormula) => "{ statement | " ^ decToAST (dec, env) ^ " |statement end}" ^ "\n\n"
		)
		| snull => ""

)

		

(*------------------x----------------------------x----GENERATING PARSE TREE-----------------------x-----------------------------------x-------------------------*)




fun STARTToString () = myData.parsedList := "START => statement" :: !(myData.parsedList) 

fun formulaToString (input : myformula) = (
	case input of 
		ite (f1,f2,f3) => (
			myData.parsedList := "IF if" :: !(myData.parsedList);
			formulaToString (f1);
			myData.parsedList := "THEN then" :: !(myData.parsedList);
			formulaToString (f2);
			myData.parsedList := "ELSE else" :: !(myData.parsedList);
			formulaToString (f3);
			myData.parsedList := "FI fi" :: !(myData.parsedList);
			myData.parsedList := "formula => IF formula THEN formula ELSE formula FI" :: !(myData.parsedList) 
		)
		
		| identifier (e) => (
			myData.parsedList := "ID " ^ e :: !(myData.parsedList);
			myData.parsedList := "formula => ID" :: !(myData.parsedList)
		)
		
		| intConst (e) => (
			myData.parsedList := "INTCONST " ^ Int.toString (e)  :: !(myData.parsedList);
			myData.parsedList := "formula => INTCONST" :: !(myData.parsedList)
		)
		| boolConst (e) => (
			myData.parsedList := "BOOLCONST " ^ e  :: !(myData.parsedList);
			myData.parsedList := "formula => BOOLCONST" :: !(myData.parsedList)
		)
		
		| lt (f1,f2) => (
			formulaToString (f1);
			myData.parsedList := "LESSTHAN LESSTHAN" :: !(myData.parsedList);
			formulaToString (f2);
			myData.parsedList := "formula => formula LESSTHAN formula" :: !(myData.parsedList) 
		)
		
		| gt (f1,f2) => (
			formulaToString (f1);
			myData.parsedList := "GREATERTHAN GREATERTHAN" :: !(myData.parsedList);
			formulaToString (f2);
			myData.parsedList := "formula => formula GREATERTHAN formula" :: !(myData.parsedList) 
		)
		
		| withBoolBinop (f1,bo,f2) => (
			formulaToString (f1);
			case bo of 
				myimplies => myData.parsedList := "IMPLIES IMPLIES" :: !(myData.parsedList)
				| myequals => myData.parsedList := "EQUALS EQUALS" :: !(myData.parsedList)
				| myxor => myData.parsedList := "XOR XOR" :: !(myData.parsedList)
				| myand => myData.parsedList := "AND AND" :: !(myData.parsedList)
				| myor => myData.parsedList := "OR OR" :: !(myData.parsedList) ;
			formulaToString (f2);
			case bo of 
				myimplies => myData.parsedList :=  "formula => formula IMPLIES formula" :: !(myData.parsedList)
				| myequals => myData.parsedList :=  "formula => formula EQUALS formula" :: !(myData.parsedList)
				| myxor => myData.parsedList :=  "formula => formula XOR formula" :: !(myData.parsedList)
				| myand => myData.parsedList :=  "formula => formula AND formula" :: !(myData.parsedList)
				| myor => myData.parsedList :=  "formula => formula OR formula" :: !(myData.parsedList) 
			
		)
		
		| withnot (f1) => (
				myData.parsedList := "NOT NOT" :: !(myData.parsedList);
				formulaToString (f1);
				myData.parsedList := "formula => NOT formula" :: !(myData.parsedList)
		)
		
		| withparen (f1) => (
				myData.parsedList := "LPAREN (" :: !(myData.parsedList);
				formulaToString (f1);
				myData.parsedList := "RPAREN )" :: !(myData.parsedList);
				myData.parsedList := "formula => LPAREN formula RPAREN" :: !(myData.parsedList) 
		)
		
		| withnegate (f1) => (
				myData.parsedList := "NEGATE NEGATE" :: !(myData.parsedList);
				formulaToString (f1);
				myData.parsedList := "formula => NEGATE formula" :: !(myData.parsedList) 
		)
		
		| withLet (d1,f1) => (
				myData.parsedList := "LET let" :: !(myData.parsedList);
				declarationToString (d1);
				myData.parsedList := "IN in" :: !(myData.parsedList);
				formulaToString (f1);
				myData.parsedList := "END end" :: !(myData.parsedList);
				myData.parsedList := "formula => LET declaration IN formula END" :: !(myData.parsedList) 
		)
		
		| withIntBinop (f1, ibo, f2) => (
				formulaToString (f1);
				case ibo of 
					myplus 		=> myData.parsedList := "PLUS PLUS" :: !(myData.parsedList)
					| myminus 	=> myData.parsedList := "MINUS MINUS" :: !(myData.parsedList)
					| mytimes 	=> myData.parsedList := "TIMES TIMES" :: !(myData.parsedList) ;
				formulaToString (f2);
				case ibo of 
					myplus 		=> myData.parsedList :=  "formula => formula PLUS formula" :: !(myData.parsedList)
					| myminus 	=> myData.parsedList :=  "formula => formula MINUS formula" :: !(myData.parsedList)
					| mytimes 	=> myData.parsedList :=  "formula => formula TIMES formula" :: !(myData.parsedList)
				
		)
		
		| funApplication (funName, f1) => (
				myData.parsedList := "ID " ^ funName :: !(myData.parsedList);
				formulaToString (f1);
				myData.parsedList := "formula => ID formula" :: !(myData.parsedList)
		)
)

and typeToString (t : mytype) = (
	case t of 
		atomicInt => (
			myData.parsedList := "INTBASETYPE int" :: !(myData.parsedList);
			myData.parsedList :=  "type => INTBASETYPE" :: !(myData.parsedList)
			
		)
		| atomicBool => (
			myData.parsedList := "BOOLBASETYPE boolean" :: !(myData.parsedList);
			myData.parsedList :=  "type => BOOLBASETYPE" :: !(myData.parsedList)
		)
		| compoundType (t1,t2) => (
			typeToString (t1);
			myData.parsedList := "SINGLEARROW ->" :: !(myData.parsedList);
			typeToString (t2);
			myData.parsedList :=  "type => type SINGLEARROW type" :: !(myData.parsedList)
		)
		| withParenType (t1) => (
			myData.parsedList := "LPAREN (" :: !(myData.parsedList);
			typeToString (t1);
			myData.parsedList := "RPAREN )" :: !(myData.parsedList);
			myData.parsedList := "type => LPAREN type RPAREN" :: !(myData.parsedList) 
		)
)

and declarationToString (d : mydeclaration) = (
	case d of 
		varDecl (varName, f1) => (
			myData.parsedList := "ID " ^ varName :: !(myData.parsedList);
			myData.parsedList := "ASSIGN =" :: !(myData.parsedList);
			formulaToString (f1);
			myData.parsedList :=  "declaration => ID ASSIGN formula" :: !(myData.parsedList)
		)
		| funDeclLambda (funName, inputArgName, domainType, rangeType, f1) => (
			myData.parsedList := "ID " ^ funName :: !(myData.parsedList);
			myData.parsedList := "ASSIGN =" :: !(myData.parsedList);
			myData.parsedList := "FN fn" :: !(myData.parsedList);
			myData.parsedList := "LPAREN (" :: !(myData.parsedList);
			myData.parsedList := "ID " ^ inputArgName :: !(myData.parsedList);
			myData.parsedList := "COLON :" :: !(myData.parsedList);
			typeToString (domainType);
			myData.parsedList := "RPAREN )" :: !(myData.parsedList);
			myData.parsedList := "COLON :" :: !(myData.parsedList);
			typeToString (rangeType);
			myData.parsedList := "DOUBLEARROW =>" :: !(myData.parsedList);
			formulaToString (f1);
			myData.parsedList :=  "declaration => ID ASSIGN FN LPAREN ID COLON type RPAREN COLON type DOUBLEARROW formula" :: !(myData.parsedList)
		)
		| funDeclHigher (funName, inputArgName, domainType, rangeType, f1) => (
			myData.parsedList := "FUN fun" :: !(myData.parsedList);
			myData.parsedList := "ID " ^ funName :: !(myData.parsedList);
			myData.parsedList := "LPAREN (" :: !(myData.parsedList);
			myData.parsedList := "ID " ^ inputArgName :: !(myData.parsedList);
			myData.parsedList := "COLON :" :: !(myData.parsedList);
			typeToString (domainType);
			myData.parsedList := "RPAREN )" :: !(myData.parsedList);
			myData.parsedList := "COLON :" :: !(myData.parsedList);
			typeToString (rangeType);
			myData.parsedList := "DOUBLEARROW =>" :: !(myData.parsedList);
			formulaToString (f1);
			myData.parsedList :=  "declaration => FUN ID LPAREN ID COLON type RPAREN COLON type DOUBLEARROW formula" :: !(myData.parsedList)
		)
)

and statementToString (input :mystatement) = (
	case input of 
		fts (f,s) => (
			formulaToString (f);
			myData.parsedList := "TERM ;" :: !(myData.parsedList);
			statementToString(s);
			myData.parsedList :="statement => formula TERM statement" :: !(myData.parsedList)  
		)
		| onlyFormula (f1) => (
			formulaToString (f1);
			myData.parsedList :="statement => formula" :: !(myData.parsedList) 
		)
		| dts (d1, s1) => (
			declarationToString (d1);
			myData.parsedList := "TERM ;" :: !(myData.parsedList);
			statementToString(s1);
			myData.parsedList :="statement => declaration TERM statement" :: !(myData.parsedList) 	
		)
		| onlyDeclaration (d1) => (
			declarationToString (d1);
			myData.parsedList :="statement => declaration" :: !(myData.parsedList) 	
		)
		
		| snull => myData.parsedList :="statement => <epsilon>" :: !(myData.parsedList)
)

end 
