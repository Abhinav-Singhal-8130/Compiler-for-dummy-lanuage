Unzip this folder and then open terminal and go to this directory.

Then execute the following commands:

ml-yacc mycalc.yacc
ml-lex mycalc.lex
sml myloader.sml


now the programme is ready to take input file. call the following function:

takeFile "inputfile";

where inputfile is the name of the file to be taken as input which should be in this same folder itself.
output should be shown after calling the function.

( I was unable to produce executable for sml on windows. we need to call the function manually in interactive mode. )
