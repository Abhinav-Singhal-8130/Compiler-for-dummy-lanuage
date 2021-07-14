
### COL226 - A2

Unzip this folder and then open terminal and go to this directory.

Then execute the following commands:

`ml-lex mycalc.lex`
`ml-yacc mycalc.yacc`
`sml myloader.sml`


Now the programme is ready to take input file. call the following function:

`takeFile "inputfile";`

where inputfile is the name of the file to be taken as input which should be in this same folder itself.
output (lexer output, parser output and type checked evaluated Abstract Syntax Tree) should be shown after calling the function.

