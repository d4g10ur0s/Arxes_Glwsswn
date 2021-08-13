%{

  #include <stdio.h>
  #include <stdlib.h>
  #include "ergasia_lex.h"


  int line_num = 1;
%}

%option noyywrap
%option case-sensitive
%option yylino

%%

LETTER   [a-zA-Z]
DIGIT    [0-9]
ALPHANUM ({LETTER}|{DIGIT})
NAME     *{LETTER}{ALPHANUM}*

/* terminal symbols */
"PROGRAM"     {return PROGRAM;}
"FUNCTION"    {return FUNCTION;}
"ENDFUNCTION" {return ENDFUNCTION;}
"VARS"        {return VARS;}
"INTEGER"     {return INTEGER;}
"CHAR"        {return CHAR;}
"START_MAIN"  {return START_MAIN;}
"ENDMAIN"     {return ENDMAIN;}
"WHILE"       {return WHILE};}
"ENDWHILE"    {return ENDWHILE;}
"FOR"         {return FOR;}
"ENDFOR"      {return ENDFOR;}
"IF"          {return IF;}
"THEN"        {return THEN;}
"ELSEIF"      {return ELSEIF;}
"ELSE"        {return ELSE;}
"ENDIF"       {return ENDIF;}
"SWITCH"      {return SWITCH;}
"CASE"        {return CASE;}
"DEFAULT"     {return DEFAULT;}
"ENDSWITCH"   {return ENDSWITCH;}
"BREAK"       {return BREAK;}
"TO"          {return TO;}
"STEP"        {return STEP;}
"RETURN"      {return RETURN;}



/* non terminal symbols */

{NAME}        {return M_NAME;}

\"{NAME}\"    {return KEIMENO;}

<<EOF>>       {return 0;}
{WHITESPACE}  {}
\n            {line_num++;}
.             {yyerror("Illegal character", 0);yyless(1);}

%%

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
/*end main*/
