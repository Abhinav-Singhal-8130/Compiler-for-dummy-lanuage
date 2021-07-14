structure PT = 
struct 

datatype mystatement = fts of (myformula * mystatement)
			| snull 
			
and myformula = 	ite of (myformula * myformula * myformula)
			|sfnull | id of string | const of string 
			| withbinop of  myformula * binop * myformula
			| withnot of myformula
			| withparen of myformula


and binop = myimplies | myequals | myxor | myand | myor 




fun STARTToString () = myData.parsedList := "START => program" :: !(myData.parsedList) 
fun programToString () = myData.parsedList :=  "program => statement" :: !(myData.parsedList) 


fun formulaToString (input : myformula) = (
	case input of 
	ite (f1,f2,f3) => (
	myData.parsedList := "IF IF" :: !(myData.parsedList);
	formulaToString (f1);
	myData.parsedList := "THEN THEN" :: !(myData.parsedList);
	formulaToString (f2);
	myData.parsedList := "ELSE ELSE" :: !(myData.parsedList);
	formulaToString (f3);
	myData.parsedList := "formula => IF formula THEN formula ELSE formula" :: !(myData.parsedList) 
	)
	
	| sfnull => (
		myData.parsedList := "formula => <epsilon>" :: !(myData.parsedList) 
	)
	| id (e) => (
		myData.parsedList := "ID " ^ e :: !(myData.parsedList);
		myData.parsedList := "formula => ID" :: !(myData.parsedList)
		
	)
	| const (e) => (
		myData.parsedList := "CONST " ^ e  :: !(myData.parsedList);
		myData.parsedList := "formula => CONST" :: !(myData.parsedList)
	)
	| withbinop (f1,bo,f2) => (
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
	
	
	
)

fun statementToString (input :mystatement) = (
	case input of 
	fts (f,s) => (
	formulaToString (f);
	
	myData.parsedList := "TERM ;" :: !(myData.parsedList);
	statementToString(s);
	myData.parsedList :="statement => formula TERM statement" :: !(myData.parsedList)  
	)
	
	| snull => myData.parsedList :="statement => <epsilon>" :: !(myData.parsedList)
)

end 
