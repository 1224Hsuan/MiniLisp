%{
  #include "final_108502550.tab.h"
  #include <string.h>
  #include <stdio.h>
  #include <stdlib.h>
  
  void yyerror(const char* message);
%}
%option noyywrap

separator		"\t"|"\n"|"\r"|" "
letter     		[a-z]
digit      	  	[0-9]

%%
define	                         		{return define;}

if                              		{return IF;}

print-num                       		{return printnum;}
print-bool                      		{return printbool;}

mod                            			{return mod;}
and                             		{return and;}
or                           			{return or;}
not                           	 		{return not;}

"+"|"-"|"*"|"/"|">"|"<"|"="|"("|")" 	{return yytext[0];}

{separator}								{;}
lambda                  	            {return lambda;}
fun                                         {return fun;}

0|[1-9]{digit}*|-[1-9]{digit}*  		{
											yylval.ival = atoi(yytext);
											return number;
										}
{letter}({letter}|{digit}|"-")*    		{
											char *tmp = (char*)malloc(yyleng);
											for(int i=0;i<yyleng;i++) tmp[i] = yytext[i];
											yylval.str = tmp;
											return id;
										}
#t|#f                           		{
											if(yytext[1] == 'f')  yylval.ival = 0;
											else yylval.ival = 1;
											return bool_val;
										}

.                                      	{yyerror("error\n");}

%%