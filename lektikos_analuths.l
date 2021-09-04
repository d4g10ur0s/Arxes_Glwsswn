%{

  #include <stdio.h>
  #include <stdlib.h>
  #include <unistd.h>
  #include <math.h>
  #include <string.h>
  //#include "termatika.h"
  #include "syntaktikos_analuths.tab.h"

  int lineno=1; // Program's line number
  int line_init=-1; // For multiline comments & strings
  extern int error_count ;

  void yyerror(char *message, int type);
  void token_print(int token_id);
%}


%option noyywrap
%option yylineno

LETTER                          [a-zA-Z]{1}
DIGIT                           [0-9]
NUM                             ({DIGIT})*
ID                              ([a-zA-Z]|[a-zA-Z]+[0-9]+|[0-9]+[a-zA-Z]+)*
WHITESPACE                      " "
NEWLINE                         [ \n]|[ \r\n]
ALPHANUM                        \"({LETTER}|{DIGIT}|{WHITESPACE})*\"

%%

"PROGRAM"                                               {token_print(PROGRAM); return PROGRAM;}
"FUNCTION"                                              {token_print(FUNCTION); return FUNCTION;}
"ENDFUNCTION"                                           {token_print(ENDFUNCTION); return ENDFUNCTION;}
"VARS"                                                  {token_print(VARS); return VARS;}
"INTEGER"                                               {token_print(INTEGER); return INTEGER;}
"CHAR"                                                  {token_print(CHAR); return CHAR;}
"STARTMAIN"                                             {token_print(STARTMAIN); return STARTMAIN;}
"ENDMAIN"                                               {token_print(ENDMAIN); return ENDMAIN;}
"WHILE"                                                 {token_print(WHILE); return WHILE;}
"ENDWHILE"                                              {token_print(ENDWHILE); return ENDWHILE;}
"FOR"                                                   {token_print(FOR); return FOR;}
"ENDFOR"                                                {token_print(ENDFOR); return ENDFOR;}
"IF"                                                    {token_print(IF); return IF;}
"THEN"                                                  {token_print(THEN); return THEN;}
"ELSEIF"                                                {token_print(ELSEIF); return ELSEIF;}
"ELSE"                                                  {token_print(ELSE); return ELSE;}
"ENDIF"                                                 {token_print(ENDIF); return ENDIF;}
"SWITCH"                                                {token_print(SWITCH); return SWITCH;}
"CASE"                                                  {token_print(CASE); return CASE;}
"DEFAULT"                                               {token_print(DEFAULT); return DEFAULT;}
"ENDSWITCH"                                             {token_print(ENDSWITCH); return ENDSWITCH;}
"BREAK"                                                 {token_print(BREAK); return BREAK;}
"TO"                                                    {token_print(TO); return TO;}
"STEP"                                                  {token_print(STEP); return STEP;}
"RETURN"                                                {token_print(RETURN); return RETURN;}
"PRINT"                                                 {token_print(PRINT); return PRINT;}

  /* telestes */

"OR"                {token_print(OR_OP); return OR_OP;}
"AND"                                                   {token_print(AND_OP); return AND_OP;}
"=="                                                    {token_print(EQ_OP); return EQ_OP;}
"!="                                                    {token_print(INEQ_OP); return INEQ_OP;}
">="|"<="|">"|"<"                                       {token_print(SUG_OP); return SUG_OP;}
"+"                                                     {token_print(PLUS_OP); return PLUS_OP;}
"-"                                                     {token_print(MINUS_OP); return MINUS_OP;}
"*"                                                     {token_print(MUL_OP); return MUL_OP;}
"/"                                                     {token_print(DIV_OP); return DIV_OP;}
"!"                                                     {token_print(NOT_OP); return NOT_OP;}
":"                                                     {token_print(ANW_KATW); return ANW_KATW;}
"("                                                     {token_print(L_PAREN); return L_PAREN;}
")"                                                     {token_print(R_PAREN); return R_PAREN;}
";"                                                     {token_print(SEMICOLON); return SEMICOLON;}
","                                                     {token_print(COMMA); return COMMA;}
"="                                                     {token_print(ASSIGN); return ASSIGN;}
"["                                                     {token_print(L_BRACK); return L_BRACK;}
"]"                                                     {token_print(R_BRACK); return R_BRACK;}
"{"                                                     {token_print(L_BRACE); return L_BRACE;}
"}"                                                     {token_print(R_BRACE); return R_BRACE;}

  /* ta dika mas */

{LETTER}                                                {token_print(LETTER); return LETTER;}
{DIGIT}                                                 {token_print(DIGIT); return DIGIT;}
{NUM}                                                   {token_print(NUM); return NUM;}
{ID}                                                    {token_print(ID); return ID;}
{ALPHANUM}                                              {token_print(ALPHANUM); return ALPHANUM;}

<<EOF>>                                                 {token_print(EOF); return 0;}
{WHITESPACE}                                            {token_print(WHITESPACE); return WHITESPACE;}
{NEWLINE}                                               {token_print(NEWLINE); lineno++; return NEWLINE;}
.                                                       {yyerror("Illegal character", 0); yyless(1);}



%%

/* end main */
/*
void yyerror(char *message, int type)
{
    error_count++;
		printf("-> ERROR at line %d caused by %s: %s\n", lineno, yytext, message);
}
*/
/* emfanish termatikwn */

void token_print(int token_id){
        printf("TOKEN FOUND: %s | TOKEN NUMBER: %d | LINE: %d\n", yytext, token_id, lineno);
}
