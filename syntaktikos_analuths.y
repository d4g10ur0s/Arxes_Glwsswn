%{
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>


/** Extern from Flex **/
extern int lineno;
extern int yylineno;

extern int yylex();
extern char *yytext;
extern FILE *yyin;
extern FILE *yyout;

/** Bison specific variables **/
int error_count = 0;
int flag_err_type = 0; // 0: Token Error (YYTEXT) || 1: String Error (STRBUF)

/** Bison specific functions **/
void yyerror(const char *message);
int main(int argc, char *argv[]);
%}


%error-verbose

%union{
    int intval;
    char *strval;
}

/** TA THS GLWSSAS **/
%token  PROGRAM             "program"
%token  FUNCTION            "function"
%token  ENDFUNCTION         "endfunction"
%token  VARS                "vars"
%token  INTEGER             "integer constant"
%token  CHAR                "char"
%token  STARTMAIN           "start_main"
%token  ENDMAIN             "end_main"
%token  WHILE               "while"
%token  ENDWHILE            "endwhile"
%token  FOR                 "for"
%token  ENDFOR              "endfor"
%token  IF                  "if"
%token  THEN                "then"
%token  ELSEIF              "elseif"
%token  ELSE                "else"
%token  ENDIF               "endif"
%token  SWITCH              "switch"
%token  CASE                "case"
%token  DEFAULT             "default"
%token  ENDSWITCH           "endswitch"
%token  CONTINUE            "continue"
%token  BREAK               "break"
%token  TO                  "to"
%token  STEP                "step"
%token  RETURN              "return"
%token  PRINT               "print"

/** TA DIKA MAS **/
%token  ANW_KATW            ":"
%token  WHITESPACE          "tab"
%token  NEWLINE             "newline"
%token  LETTER              "[a-zA-Z]"
%token  DIGIT               "[0-9]"
%token  NUM                 "({DIGIT})*"
%token  NAME                "{LETTER}+"
%token  ALPHANUM            "({LETTER}|{DIGIT}|{WHITESPACE})"
%token  ID                  "([a-zA-Z]+|[a-zA-Z]+[0-9]+|[0-9]+[a-zA-Z]+)*"

/** TELESTES **/
%token  OR_OP               "||"
%token  AND_OP              "&&"
%token  EQ_OP               "=="
%token  INEQ_OP             "!="
%token  SUG_OP              "<= ,>=, >, <"
%token  NOT_OP              "!"
%token  MINUS_OP            "-"
%token  PLUS_OP             "+"
%token  MUL_OP              "*"
%token  DIV_OP              "/"
%token  L_PAREN             "("
%token  R_PAREN             ")"
%token  SEMICOLON           ";"
%token  COMMA               ","
%token  ASSIGN              "="
%token  L_BRACK             "["
%token  R_BRACK             "]"
%token  L_BRACE             "{"
%token  R_BRACE             "}"

/** END OF FILE **/
%token  T_EOF 0             "end of file"

%left COMMA
%right ASSIGN
%left OR_OP
%left AND_OP
%left EQU_OP INEQ_OP
%left ADD_OP
%left MUL_OP
%left NOT_OP
%left L_PAREN R_PAREN L_BRACK R_BRACK

%nonassoc LOWER_THAN_ELSE
%nonassoc T_ELSE

%%
program:                                              PROGRAM optional_space_or_newline ID optional_space_or_newline
                                                      multiple_func_declaration optional_space_or_newline
                                                      main_program
                                                    ;
optional_space_or_newline:                            optional_space_or_newline NEWLINE
                                                    | optional_space_or_newline WHITESPACE 
                                                    |
                                                    ;


multiple_func_declaration:                            func_declaration optional_space_or_newline
                                                    | multiple_func_declaration func_declaration
                                                    |
                                                    ;
func_declaration:                                     FUNCTION optional_space_or_newline full_par_func_header optional_space_or_newline function_body return_statement optional_space_or_newline ENDFUNCTION optional_space_or_newline
                                                    ;
function_body:                                         var_declaration optional_space_or_newline command_list
                                                    |  command_list
                                                    ;
return_statement:                                     RETURN optional_space_or_newline to_ret optional_space_or_newline SEMICOLON
                                                    ;
to_ret:                                               variable
                                                    | constant
                                                    |
                                                    ;
full_par_func_header:                                 ID optional_space_or_newline L_PAREN optional_space_or_newline parameter_list optional_space_or_newline R_PAREN
                                                    ;
parameter_list:                                       parameter_list optional_space_or_newline COMMA optional_space_or_newline typename optional_space_or_newline variable optional_space_or_newline
                                                    | typename optional_space_or_newline variable optional_space_or_newline
                                                    ;
typename:                                             CHAR
                                                    | INTEGER
                                                    ;
var_declaration:                                      VARS optional_space_or_newline typename optional_space_or_newline variables optional_space_or_newline SEMICOLON
                                                    | var_declaration optional_space_or_newline VARS optional_space_or_newline typename optional_space_or_newline variables optional_space_or_newline SEMICOLON
                                                    ;
variable:                                             ID optional_space_or_newline
                                                    | ID optional_space_or_newline L_BRACK optional_space_or_newline NUM optional_space_or_newline R_BRACK optional_space_or_newline
                                                    ;
variables:                                            variable
                                                    | variables optional_space_or_newline COMMA optional_space_or_newline variable optional_space_or_newline
                                                    |
                                                    ;
command_list:                                         command_list optional_space_or_newline command optional_space_or_newline
                                                    | command optional_space_or_newline
                                                    |
                                                    ;
command:                                              assignment
                                                    | if_statement
                                                    | while_statement
                                                    | for_statement
                                                    | switch_statement
                                                    | CONTINUE SEMICOLON
                                                    | BREAK SEMICOLON
                                                    | SEMICOLON
                                                    | print_statement
                                                    ;
assignment:                                           variable optional_space_or_newline ASSIGN optional_space_or_newline expression optional_space_or_newline SEMICOLON optional_space_or_newline
                                                    ;
expression:                                           expression optional_space_or_newline MINUS_OP optional_space_or_newline expression
                                                    | expression optional_space_or_newline PLUS_OP optional_space_or_newline expression
                                                    | expression optional_space_or_newline MUL_OP optional_space_or_newline expression
                                                    | expression optional_space_or_newline DIV_OP optional_space_or_newline expression
                                                    | variable optional_space_or_newline
                                                    | constant
                                                    | L_PAREN optional_space_or_newline expression optional_space_or_newline R_PAREN optional_space_or_newline
                                                    |
                                                    ;
logic_expression:                                     expression optional_space_or_newline OR_OP optional_space_or_newline expression
                                                    | expression optional_space_or_newline AND_OP optional_space_or_newline expression
                                                    | expression optional_space_or_newline EQU_OP EQU_OP optional_space_or_newline expression
                                                    | expression optional_space_or_newline INEQ_OP optional_space_or_newline expression
                                                    | expression optional_space_or_newline SUG_OP optional_space_or_newline expression
                                                    | NOT_OP optional_space_or_newline expression
                                                    | i_constant
                                                    |
                                                    ;
constant:                                             i_constant
                                                    | c_constant
                                                    ;
i_constant:                                           NUM
                                                    | DIGIT
                                                    ;
c_constant:                                           ALPHANUM
                                                    ;
if_statement:                                         IF optional_space_or_newline L_PAREN optional_space_or_newline logic_expression optional_space_or_newline R_PAREN optional_space_or_newline THEN
                                                      optional_space_or_newline command_list
                                                      optional_space_or_newline if_tail
                                                    ;
if_tail:                                              ELSEIF optional_space_or_newline L_PAREN optional_space_or_newline logic_expression optional_space_or_newline R_PAREN optional_space_or_newline
                                                      command_list optional_space_or_newline
                                                      if_tail
                                                    | ELSE
                                                      optional_space_or_newline command_list
                                                      optional_space_or_newline ENDIF
                                                    ;
while_statement:                                      WHILE optional_space_or_newline L_PAREN optional_space_or_newline logic_expression optional_space_or_newline R_PAREN
                                                      optional_space_or_newline command_list
                                                      optional_space_or_newline ENDWHILE
                                                    ;
for_statement:                                        FOR optional_space_or_newline counter ANW_KATW ASSIGN optional_space_or_newline i_constant optional_space_or_newline TO optional_space_or_newline i_constant optional_space_or_newline STEP optional_space_or_newline i_constant optional_space_or_newline
                                                      command_list optional_space_or_newline
                                                      ENDFOR
                                                    ;
counter:                                              ID
                                                    ;
switch_statement:                                     SWITCH optional_space_or_newline L_PAREN optional_space_or_newline logic_expression optional_space_or_newline R_PAREN optional_space_or_newline switch_tail
                                                    ;
switch_tail:                                          case_statement
                                                      optional_space_or_newline command_list
                                                      optional_space_or_newline switch_tail
                                                    | DEFAULT optional_space_or_newline ANW_KATW optional_space_or_newline
                                                      command_list
                                                      optional_space_or_newline
                                                      ENDSWITCH
                                                    | ENDSWITCH
                                                    ;
case_statement:                                       CASE optional_space_or_newline L_PAREN logic_expression optional_space_or_newline R_PAREN optional_space_or_newline ANW_KATW
                                                    ;
main_program:                                         STARTMAIN optional_space_or_newline
                                                      main_content optional_space_or_newline
                                                      ENDMAIN optional_space_or_newline
                                                    ;
main_content:                                         command_list
                                                    | var_declaration optional_space_or_newline command_list optional_space_or_newline
                                                    ;
print_statement:                                      PRINT optional_space_or_newline L_PAREN optional_space_or_newline ALPHANUM optional_space_or_newline R_PAREN optional_space_or_newline SEMICOLON
                                                    ;

%%

int main(int argc, char *argv[]){

    if(argc > 1){
        yyin = fopen(argv[1], "r");
        if (yyin == NULL){
            perror ("Error opening file"); return -1;
        }
    }

    yyparse();

    fclose(yyin);

    return 0;
}


void yyerror(const char *message) {
    fprintf(stderr, "\nline %d :%s \n",yylineno, message);
}
