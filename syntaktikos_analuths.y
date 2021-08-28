%{
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

/** Extern from Flex **/
extern int lineno;

extern int yylex();
extern char *yytext;
extern FILE *yyin;
extern FILE *yyout;

extern void yyterminate();


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
%token <strval> PROGRAM             "program"
%token <strval> FUNCTION            "function"
%token <strval> ENDFUNCTION         "endfunction"
%token <strval> VARS                "vars"
%token <intval> INTEGER             "integer constant"
%token <strval> CHAR                "char"
%token <strval> STARTMAIN           "start_main"
%token <strval> ENDMAIN             "end_main"
%token <strval> WHILE               "while"
%token <strval> ENDWHILE            "endwhile"
%token <strval> FOR                 "for"
%token <strval> ENDFOR              "endfor"
%token <strval> IF                  "if"
%token <strval> THEN                "then"
%token <strval> ELSEIF              "elseif"
%token <strval> ELSE                "else"
%token <strval> ENDIF               "endif"
%token <strval> SWITCH              "switch"
%token <strval> CASE                "case"
%token <strval> DEFAULT             "default"
%token <strval> ENDSWITCH           "endswitch"
%token <strval> CONTINUE            "continue"
%token <strval> BREAK               "break"
%token <strval> TO                  "to"
%token <strval> STEP                "step"
%token <strval> RETURN              "return"
%token <strval> PRINT               "print"

/** TA DIKA MAS **/
%token <strval> LETTER              "[a-zA-Z]"
%token <intval> DIGIT               "[0-9]"
%token <intval> NUM                 "{DIGIT}+"
%token <strval> NAME                "{LETTER}+"
%token <strval> ALPHANUM            "({LETTER}|{DIGIT}|{WHITESPACE})"
%token <strval> ID                  "({LETTER}+{DIGIT}*)"
%token <strval> WHITESPACE          "[ /t]"
%token <strval> NEWLINE             "[ /n]"

/** TELESTES **/
%token <strval> OR_OP               "||"
%token <strval> AND_OP              "&&"
%token <strval> EQ_OP               "=="
%token <strval> INEQ_OP             "!="
%token <strval> SUG_OP              "<= ,>=, >, <"
%token <strval> NOT_OP              "!"
%token <strval> MINUS_OP            "-"
%token <strval> PLUS_OP             "+"
%token <strval> MUL_OP              "*"
%token <strval> DIV_OP              "/"
%token <strval> L_PAREN             "("
%token <strval> R_PAREN             ")"
%token <strval> SEMICOLON           ";"
%token <strval> COMMA               ","
%token <strval> ASSIGN              "="
%token <strval> L_BRACK             "["
%token <strval> R_BRACK             "]"
%token <strval> L_BRACE             "{"
%token <strval> R_BRACE             "}"

/** END OF FILE **/
%token <strval> T_EOF 0             "end of file"


%type <strval> program typename var_declaration vars_declaration variable main_program main_content
%type <strval> multiple_func_declaration func_declaration full_par_func_header parameter_list
%type <strval> command_list command assignment expression logic_expression
%type <strval> if_statement if_tail switch_statement switch_tail case_statement print_statement
%type <strval> while_statement for_statement counter constant
%type <strval> comments comment_end

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

%start program

%%
program:                                              PROGRAM ID NEWLINE multiple_func_declaration main_program
                                                    ;

multiple_func_declaration:                            func_declaration
                                                    | multiple_func_declaration NEWLINE func_declaration
                                                    | {}
                                                    ;
func_declaration:                                     FUNCTION full_par_func_header NEWLINE var_declaration command_list return_statement ENDFUNCTION
                                                    ;
return_statement:                                     RETURN to_ret SEMICOLON
                                                    ;
to_ret:                                               ID
                                                    | variable
                                                    | constant
                                                    ;
full_par_func_header:                                 ID L_PAREN parameter_list R_PAREN
                                                    ;
parameter_list:                                       parameter_list COMMA typename ID
                                                    | typename ID
                                                    | {}
                                                    ;
typename:                                             CHAR
                                                    | INTEGER
                                                    ;
var_declaration:                                      VARS typename ID
                                                    | VARS typename ID L_BRACK NUM R_BRACK
                                                    | {}
                                                    ;
vars_declaration:                                     vars_declaration COMMA var_declaration SEMICOLON
                                                    | var_declaration SEMICOLON
                                                    | {}
                                                    ;
command_list:                                         command_list NEWLINE command
                                                    | command
                                                    | {}
                                                    ;
command:                                              assignment
                                                    | if_statement
                                                    | while_statement
                                                    | for_statement
                                                    | switch_statement
                                                    | return_statement
                                                    | CONTINUE SEMICOLON
                                                    | BREAK SEMICOLON
                                                    | SEMICOLON
                                                    | print_statement
                                                    ;
assignment:                                           variable ASSIGN expression SEMICOLON
                                                    ;
expression:                                           expression MINUS_OP expression
                                                    | expression PLUS_OP expression {$$ = $1 + $3}
                                                    | expression MUL_OP expression
                                                    | expression DIV_OP expression
                                                    | logic_expression
                                                    | variable
                                                    | constant
                                                    | L_PAREN expression R_PAREN
                                                    | {}
                                                    ;
logic_expression:                                     expression OR_OP expression
                                                    | expression AND_OP expression
                                                    | expression EQU_OP EQU_OP expression
                                                    | expression INEQ_OP expression
                                                    | expression SUG_OP expression
                                                    | NOT_OP expression
                                                    ;
constant:                                             i_constant
                                                    | c_constant
                                                    ;
i_constant:                                           INTEGER
                                                    ;
c_constant:                                           CHAR
                                                    ;
variable:                                             typename ID
                                                    ;
if_statement:                                         IF L_PAREN logic_expression R_PAREN THEN
                                                      NEWLINE command_list
                                                      NEWLINE if_tail
                                                    ;
if_tail:                                              ELSEIF L_PAREN logic_expression R_PAREN NEWLINE
                                                      command_list NEWLINE
                                                      if_tail
                                                    | ELSE NEWLINE
                                                      command_list
                                                      NEWLINE ENDIF
                                                    ;
while_statement:                                      WHILE L_PAREN logic_expression R_PAREN
                                                      NEWLINE command_list
                                                      NEWLINE ENDWHILE
                                                    ;
for_statement:                                        FOR counter ':'ASSIGN NUM TO NUM STEP NUM NEWLINE
                                                      command_list
                                                      NEWLINE ENDFOR
                                                    ;
counter:                                              ID
                                                    ;
switch_statement:                                     SWITCH L_PAREN expression R_PAREN NEWLINE switch_tail
                                                    ;
switch_tail:                                          case_statement
                                                      NEWLINE command_list
                                                      NEWLINE switch_tail
                                                    | DEFAULT':' NEWLINE
                                                      command_list
                                                      ENDSWITCH
                                                    | ENDSWITCH
                                                    ;
case_statement:                                       CASE L_PAREN expression R_PAREN':'
                                                    ;
comments:                                             '%'ALPHANUM comment_end
                                                    ;
comment_end:                                          ALPHANUM
                                                    ;
main_program:                                         STARTMAIN NEWLINE
                                                      main_content NEWLINE
                                                      ENDMAIN
                                                    ;
main_content:                                         command_list
                                                    | vars_declaration NEWLINE
                                                      command_list
                                                    ;
print_statement:                                      PRINT L_PAREN '\"' ALPHANUM '\"' R_PAREN SEMICOLON
                                                    ;

%%

int main(int argc, char *argv[]){

    if(!(hashtbl = hashtbl_create(10, NULL))) {
        fprintf(stderr, "ERROR: hashtbl_create() failed!\n");
        exit(EXIT_FAILURE);
    }

    if(argc > 1){
        yyin = fopen(argv[1], "r");
        if (yyin == NULL){
            perror ("Error opening file"); return -1;
        }
    }

    yyparse();

    hashtbl_get(hashtbl, scope); // Retrieve the last table (Scope 0);
    hashtbl_destroy(hashtbl);
    fclose(yyin);


    if(error_count > 0){
        printf("Syntax Analysis failed due to %d errors\n", error_count);
    }else{
        printf("Syntax Analysis completed successfully.\n");
    }
    return 0;
}


void yyerror(const char *message)
{
    error_count++;

    if(flag_err_type==0){
        printf("-> ERROR at line %d caused by %s : %s\n", lineno, yytext, message);
    }else if(flag_err_type==1){
        *str_buf_ptr = '\0'; // String or Comment Error. Cleanup old chars stored in buffer.
        printf("-> ERROR at line %d near \"%s\": %s\n", lineno, str_buf, message);
    }
    flag_err_type = 0; // Reset flag_err_type to default.
    if(MAX_ERRORS <= 0) return;
    if(error_count == MAX_ERRORS){
        printf("Max errors (%d) detected. ABORTING...\n", MAX_ERRORS);
        exit(-1);
    }
}
