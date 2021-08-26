%{

#include "termatika.h"
  #include <stdio.h>
  #include <stdlib.h>
  #include <unistd.h>
  #include <math.h>
  #include <string.h>

  int lineno=1; // Program's line number
  int line_init=-1; // For multiline comments & strings
  int error_count = 0;

  extern int yylval;

  void token_print(int token_id);
%}


%option noyywrap
%option case-insensitive
%option yylineno

LETTER                          [a-zA-Z]
DIGIT                           [0-9]
NUM                             {DIGIT}+
NAME                            {LETTER}+
ALPHANUM                        ({LETTER}|{DIGIT})
WHITESPACE                      [ /t]
NEWLINE                         [ /n]
ID                            ({LETTER}+ {DIGIT}*)

%%

"PROGRAM"                                               {token_print(PROGRAM); return PROGRAM;}
"FUNCTION"                                              {token_print(FUNCTION); return FUNCTION;}
"ENDFUNCTION"                                           {token_print(ENDFUNCTION); return ENDFUNCTION;}
"VARS"                                                  {token_print(VARS); return VARS;}
"INTEGER"                                               {token_print(INTEGER); return INTEGER;}
"CHAR"                                                  {token_print(CHAR); return CHAR;}
"START_MAIN"                                            {token_print(START_MAIN); return START_MAIN;}
"END_MAIN"                                              {token_print(END_MAIN); return END_MAIN;}
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

  /* telestes */

"OR"                {token_print(OR_OP); return OR_OP;}
"AND"                                                   {token_print(AND_OP); return AND_OP;}
"=="                                                    {token_print(EQ_OP); return EQ_OP;}
"!="                                                    {token_print(INEQ_OP); return INEQ_OP;}
">=" | "<=" | ">" | "<"                                 {token_print(SUG_OP); return SUG_OP;}
"+"                                                     {token_print(PLUS_OP); return PLUS_OP;}
"-"                                                     {token_print(MINUS_OP); return MINUS_OP;}
"*"                                                     {token_print(MUL_OP); return MUL_OP;}
"/"                                                     {token_print(DIV_OP); return DIV_OP;}
"!"                                                     {token_print(NOT_OP); return NOT_OP;}

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
{NAME}                                                  {token_print(NAME); return M_NAME;}
{ALPHANUM}                                              {token_print(NAME); return M_NAME;}

<<EOF>>                                                 {token_print(EOF); return 0;}
{WHITESPACE}                                            {}
NEWLINE                                                 {line_num++;}
.                                                       {yyerror("Illegal character", 0); yyless(1);}



%%

/* main */

int main(int argc, char *argv[]){
    int token;

    if(argc > 1){
        yyin = fopen(argv[1], "r");
        //an to arxeio den yparxei error
        if (yyin == NULL){
            perror ("Error opening file"); return -1;
        }
    }
    //lektikh analysh
    do{
		token = yylex();
    }while(token != T_EOF);
    //apari8mhsh errors
    if(error_count > 0){
        printf("Failed to analyze the program due to %d errors\n", error_count);
    }else{
        printf("Program analyzed successfully\n");
    }
    //kleisimo programmatos
    fclose(yyin);
    yyterminate();
}

/* end main */

/* emfanish termatikwn */

void token_print(int token_id){
    if(FLEX_DEBUG && SHOW_TOKENS){
        printf("TOKEN FOUND: %s | TOKEN NUMBER: %d | LINE: %d\n", yytext, token_id, lineno);
    }
}
