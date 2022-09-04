# MiniLisp
An interpreter of MiniLisp.

## Language and Environment

* Lex and Yacc
* Windows 10
* GnuWin32
* bison 4.2.1
* flex 2.5.4


## Feature

* Syntax Validation
  
* Print
  * print-num
  * print-bool
* Numerical Operations
  * Plus
  * Minus
  * Multiply
  * Divide
  * Modulus
  * Greater
  * Smaller
  * Equal
  
* Logical Operations
  * And
  * Or
  * Not
  
* if Expression
* Variable Definition
* Function
* Named Function

## Compilation and Execution
1. Generate *.tab.c and *.tab.h file by using .y file.
2. Genetate *.yy.c file by using .l file.
3. Link the .c file that was generated by Lex and Yacc.
