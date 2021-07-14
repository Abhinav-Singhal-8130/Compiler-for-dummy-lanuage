structure CalcLrVals = CalcLrValsFun(structure Token = LrParser.Token)
structure CalcLex = CalcLexFun(structure Tokens = CalcLrVals.Tokens)

structure CalcParser=
  Join(structure ParserData = CalcLrVals.ParserData
       structure Lex= CalcLex
       structure LrParser=LrParser)
    
val isParseError : bool ref = ref false;
    
	
	
	
(* 			functions used for printing				*)   
fun printStringListHelper (nil) = ()
| printStringListHelper (hd::nil : (string * int * int) list) = (print (#1 (hd)))
| printStringListHelper (hd::tl : (string * int * int) list) = (print ( (#1 (hd)) ^ ","); printStringListHelper (tl));


fun printStringList (someStringList : (string * int * int) list ref) = (
  let val x = List.rev (!someStringList) in (
    print ("[");
    printStringListHelper (x);
    print ("]\n")
  )
  end
);
   
fun printParsedListHelper (nil) = ()
| printParsedListHelper (hd::nil : string list) = (print(hd))
| printParsedListHelper (hd::tl : string list) = (print (hd ^ ",") ; printParsedListHelper(tl)) ;

fun printParsedList (someStringList : string list ref) = (
  let val x = List.rev (!someStringList) in (
  print ("[");
    printParsedListHelper (x);
    print ("]\n")
  )
  end
); 
   
(*				print helper functions end				*)
   
   
   
fun fileToLexer infilename = 
    let 
    	val ins = TextIO.openIn infilename
    	val str :string = TextIO.inputAll ins 
 	val done = ref false
 	val lexer = CalcParser.makeLexer (fn _ => if (!done) then "" else (done := true; str))
    in 
    	lexer
    end 
   

fun invoke lexstream =
    let 
    
    fun print_error (s,pos:int, mcol :int) = ( (*let val x = CalcParser.lex () in () end ;*) if ((!isParseError) = false) then (printStringList (CalcLex.UserDeclarations.lexedList);
    						isParseError := true;
						TextIO.output(TextIO.stdOut,"Syntax Error:" ^ (Int.toString pos) ^ ":" ^ (Int.toString mcol) ^ ":" ^ s ^ "\n")) else ())   
    
    in (
     CalcParser.parse(0,lexstream,print_error,())
     )
    end


fun parse (lexer) =  
    let 
	val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = CalcParser.Stream.get lexer
    in
    	if (CalcParser.sameToken(nextToken, dummyEOF)) then (result)
    	else (
    	    TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n");
    	    result
    	)
    end
   


fun takeFile (x :string) = let val mylexer = fileToLexer (x) in (
			CalcLex.UserDeclarations.row := 1 ;
			CalcLex.UserDeclarations.col := 1 ;
			CalcLex.UserDeclarations.lexedList := [];
			isParseError := false;
			myData.parsedList := [];
			parse (mylexer) ; 
			if ((!isParseError) = false ) then (printStringList (CalcLex.UserDeclarations.lexedList); printParsedList (myData.parsedList)) else ();
			isParseError := false ) end
			
	handle CalcLex.UserDeclarations.myexception( myerrorString :string, myline :int, mycol :int) => print("Unknown Token:"^Int.toString(myline)^":"^Int.toString(mycol)^":"^ myerrorString ^"\n")
	| ParseError => ();
	
	
