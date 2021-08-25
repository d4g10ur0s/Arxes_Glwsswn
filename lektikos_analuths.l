%{

  #include <stdio.h>
  #include <stdlib.h>
  #include "ergasia_lex.h"


  int line_num = 1;
%}

%option noyywrap
%option case-sensitive
%option yylineno

  /* oi orismoi mas */
WHITESPACE          [ \t]

%%
 /* termatika symvola */
"PROGRAM"     {token_print(PROGRAM); return PROGRAM;}
"FUNCTION"    {token_print(FUNCTION); return FUNCTION;}
"ENDFUNCTION" {token_print(ENDFUNCTION); return ENDFUNCTION;}
"VARS"        {token_print(VARS); return VARS;}
"INTEGER"     {token_print(INTEGER); return INTEGER;}
"CHAR"        {token_print(CHAR); return CHAR;}
"START_MAIN"  {token_print(START_MAIN); return START_MAIN;}
"END_MAIN"    {token_print(END_MAIN); return END_MAIN;}
"WHILE"       {token_print(WHILE); return WHILE};}
"ENDWHILE"    {token_print(ENDWHILE); return ENDWHILE;}
"FOR"         {token_print(FOR); return FOR;}
"ENDFOR"      {token_print(ENDFOR); return ENDFOR;}
"IF"          {token_print(IF); return IF;}
"THEN"        {token_print(THEN); return THEN;}
"ELSEIF"      {token_print(ELSEIF); return ELSEIF;}
"ELSE"        {token_print(ELSE); return ELSE;}
"ENDIF"       {token_print(ENDIF); return ENDIF;}
"SWITCH"      {token_print(SWITCH); return SWITCH;}
"CASE"        {token_print(CASE); return CASE;}
"DEFAULT"     {token_print(DEFAULT); return DEFAULT;}
"ENDSWITCH"   {token_print(ENDSWITCH); return ENDSWITCH;}
"BREAK"       {token_print(BREAK); return BREAK;}
"TO"          {token_print(TO); return TO;}
"STEP"        {token_print(STEP); return STEP;}
"RETURN"                                                {token_print(RETURN); return RETURN;}

  /* telestes */

"OR"                                                    {token_print(OR_OP); return OR_OP;}
"AND"                                                   {token_print(AND_OP); return AND_OP;}
"=="                                                    {token_print(EQ_OP); return EQ_OP;}
"!="                                                    {token_print(INEQ_OP); return INEQ_OP;}
">=" | "<=" | ">" | "<"                                 {token_print(SUG_OP); return SUG_OP;}
"+"                                                     {token_print(PLUS_OP); return PLUS_OP;}
"-"                                                     {token_print(MINUS_OP); return MINUS_OP;}
"*"                                                     {token_print(MUL_OP); return MUL_OP;}
"/"                                                     {token_print(DIV_OP); return DIV_OP;}
"!"                                                     {token_print(NOT_OP); return NOT_OP;}
 /* alla symvola */
"("                                                     {token_print(L_PAREN); return L_PAREN;}
")"                                                     {token_print(R_PAREN); return R_PAREN;}
";"                                                     {token_print(SEMICOLON); return SEMICOLON;}
","                                                     {token_print(COMMA); return COMMA;}
"="                                                     {token_print(ASSIGN); return ASSIGN;}
"["                                                     {token_print(L_BRACK); return L_BRACK;}
"]"                                                     {token_print(R_BRACK); return R_BRACK;}
"{"                                                     {token_print(L_BRACE); return L_BRACE;}
"}"                                                     {token_print(R_BRACE); return R_BRACE;}

<<EOF>>       {token_print(EOF); return 0;}
{WHITESPACE}  {}
\n            {line_num++;}
.             {yyerror("Illegal character", 0); yyless(1);}


%%

/* ====[USER FUNCTIONS BLOCK]==== */
/* ====[END USER FUNCTIONS BLOCK]==== */

/* main */

int main(int argc, char *argv[]){
    int token;

    if(argc > 1){
        yyin = fopen(argv[1], "r");
        if (yyin == NULL){
            perror ("Error opening file"); return -1;
        }
    }

    do{
		token = yylex();
    }while(token != T_EOF);

    if(error_count > 0){
        printf("Failed to analyze the program due to %d errors\n", error_count);
    }else{
        printf("Program analyzed successfully\n");
    }
    fclose(yyin);
    yyterminate();
}
/* end main */

void yyerror(char *message, int type)
{
    error_count++;

    if(type==0){
		printf("-> ERROR at line %d caused by %s: %s\n", lineno, yytext, message);
    }else if(type==1){
		*str_buf_ptr = '\0'; // String or Comment Error. Cleanup old chars stored in buffer.
		printf("-> ERROR at line %d near \"%s\": %s\n", lineno, str_buf, message);
	}
    if(MAX_ERRORS <= 0) return;
    if(error_count == MAX_ERRORS){
        printf("Max errors detected\n");
        exit(-1);
    }
}

void token_print(int token_id){
    if(FLEX_DEBUG && SHOW_TOKENS){
        printf("TOKEN FOUND: %s | TOKEN NUMBER: %d | LINE: %d\n", yytext, token_id, lineno);
    }
}

void comment_print(){
    if(FLEX_DEBUG && SHOW_COMMENTS){
        int line = line_init > 0 ? lineno-line_init : lineno;
        printf("COMMENT FOUND: %s | TOKEN NUMBER: %d | LINE: %d\n", str_buf, T_STRING, line);
    }
}
