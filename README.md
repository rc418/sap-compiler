This is a compiler for a simple language for the Ben Eater version of the SAP-1 computer.

## EBNF
```
program ::= declaration, {declaration}, statement, {statement}, "HALT"
declaration ::= "LET" ident "=" number nl
statement ::= "OUTPUT" (ident) nl
    | "LABEL" ident nl
    | "GOTO" ident nl
    | ident "=" expression nl
expression ::= term {( "-" | "+" ) term}
term ::= number | ident
number ::= digit_excluding_zero, {digit} | "0"
ident ::= lower_char, {lower_char | digit}
digit_excluding_zero ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
digit ::= 0 | digit_excluding_zero
lower_char ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
nl ::= "\n", {"\n"}
```
# Example program
```
LET a = 13
LET b = 42
a = a - b
OUTPUT a
HALT
```
# Limitations
## Limit on size of numbers
- when defining a variable you can assign an 8 bit number max value 255 min value 0
- any other place in code only a 4 bit number is allowed max value 15 min value 0
## Looping
Currently, only infinite loops are possible as there are no conditional statements.
## Output
Currently, it only compiles as far as assembly code later it will go down to machine code.


# Reference
This work is based on http://enear.github.io/2016/03/31/parser-combinators/