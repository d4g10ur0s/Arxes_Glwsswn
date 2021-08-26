%{
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

/** Extern from Flex **/
extern int lineno, line_init;

extern char str_buf[MAX_STR_CONST];
extern char* str_buf_ptr;

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
%token <strval> START_MAIN          "start_main"
%token <strval> END_MAIN            "end_main"
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
%token <strval> BREAK               "break"
%token <strval> TO                  "to"
%token <strval> STEP                "step"
%token <strval> RETURN              "return"

/** TA DIKA MAS **/
%token <strval> LETTER              "[a-zA-Z]"
%token <strval> DIGIT               "[0-9]"
%token <strval> NUM                 "{DIGIT}+"
%token <strval> NAME                "{LETTER}+"
%token <strval> ALPHANUM            "({LETTER}|{DIGIT})"
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
%token <strval> COMMA               ","

/** END OF FILE **/
%token <strval> T_EOF 0             "end of file"


%type <strval> program multiple_func_declaration  global_declaration global_declarations typename standard_type listspec dims
%type <strval> id_list initializer init_value expression variable general_expression assignment expression_list listexpression
%type <strval> init_values var_declaration
%type <strval> parameter_types
%type <strval> init_variabledefs init_variabledef func_declaration full_func_declaration
%type <strval> full_par_func_header parameter_list decl_statements declarations decltype
%type <strval> statements statement expression_statement if_statement if_tail while_statement for_statement switch_statement switch_tail decl_cases
%type <strval> casestatements casestatement single_casestatement return_statement in_list in_item out_list out_item comp_statement main_function main_header
%type <strval> constant

%left COMMA
%right ASSIGN
%left OR_OP
%left AND_OP
%left EQU_OP INEQ_OP
%left ADD_OP
%left MUL_OP
%left NOT_OP
%left T_LPAREN T_RPAREN T_LBRACK T_RBRACK

%nonassoc LOWER_THAN_ELSE
%nonassoc T_ELSE

%start program

%%
program:                                              PROGRAM ID NEWLINE multiple_func_declaration main_program
                                                    ;

multiple_func_declaration:                            func_declaration
                                                    | multiple_func_declaration NEWLINE func_declaration
                                                    | %empty {}
                                                    ;
func_declaration:                                     FUNCTION full_par_func_header NEWLINE decl_statements command_list RETURN to_ret END_FUNCTION
                                                    ;
full_par_func_header:                                 ID L_PAREN parameter_list R_PAREN
                                                    ;
parameter_list:                                       parameter_list COMMA typename ID
                                                    | typename ID
                                                    | %empty {}
                                                    ;
typename:                                             CHAR
                                                    | INTEGER
                                                    ;
var_declaration:                                      VARS typename ID
                                                    | VARS typename ID L_BRACK NUM R_BRACK
                                                    | %empty {}
                                                    ;
vars_declaration:                                     vars_declaration COMMA var_declaration SEMICOLON
                                                    | var_declaration SEMICOLON
                                                    | %empty {}
                                                    ;
command_list:                                         command_list NEWLINE command
                                                    | command
                                                    | %empty {}
                                                    ;
command:                                              assign_statement
                                                    | if_statement
                                                    | while_statement
                                                    | for_statement
                                                    | switch_statement
                                                    | return_statement
                                                    | comp_statement
                                                    | CONTINUE SEMICOLON
                                                    | BREAK SEMICOLON
                                                    | SEMICOLON
                                                    | print_statement
                                                    ;
assignment:                                           variable ASSIGN expression
                                                    ;
expression:                                           expression OR_OP expression
                                                    | expression AND_OP expression
                                                    | expression EQU_OP expression
                                                    | expression INEQU_OP expression
                                                    | expression SUG_OP expression
                                                    | expression MINUS_OP expression
                                                    | expression PLUS_OP expression {$$ = $1 + $3}
                                                    | expression MUL_OP expression
                                                    | expression DIV_OP expression
                                                    | NOT_OP expression
                                                    | variable
                                                    | constant
                                                    | L_PAREN expression R_PAREN
                                                    | %empty {}
                                                    ;

variable:                                             typename ID
                                                    ;

/**exoun elegx8ei**/

global_declarations:      global_declarations global_declaration
                        | %empty {}
                        ;
global_declaration:     global_var_declaration
                        | func_declaration
                        ;
dims:                     dims dim
                        | %empty {}
                        ;
dim:                      T_LBRACK T_ICONST T_RBRACK
                        | T_LBRACK T_RBRACK
                        ;
id_list:                  id_list COMMA M_NAME initializer
                        | M_NAME
                          initializer
                        ;
initializer:              ASSIGN init_value
                        | %empty {}
init_value:               expression
                        | L_BRACE init_values R_BRACE
                        ;
init_values:              init_values COMMA init_value
                        | init_value
                        ;
fields:                   fields field
                        | field
                        ;
field:                    var_declaration
                        ;
func_header_start:        typename listspec M_NAME
                        ;
parameter_types:          parameter_types COMMA typename pass_list_dims
                        | typename pass_list_dims
                        ;
pass_list_dims:         listspec dims
                        ;
init_variabledefs:        init_variabledefs COMMA init_variabledef
                        | init_variabledef
                        ;
init_variabledef:         variabledef initializer
                        ;

decl_statements:          declarations statements
                        | declarations
                        | statements
                        | %empty {}
                        ;
declarations:             declarations decltype typename variabledefs SEMICOLON
                        | decltype typename variabledefs SEMICOLON
                        ;
expression_statement:     general_expression SEMICOLON
                        ;
if_statement:             IF L_PAREN
                          general_expression R_PAREN statement
                          if_tail
                        ;
if_tail:                  ELSE
                          statement
                        | %empty %prec LOWER_THAN_ELSE
                        ;
while_statement:          WHILE L_PAREN
                          general_expression R_PAREN statement
                        ;
for_statement:            FOR L_PAREN
                            optexpr SEMICOLON optexpr SEMICOLON optexpr R_PAREN statement
                        ;
optexpr:                  general_expression
                        | %empty {}
                        ;
switch_statement:         SWITCH L_PAREN
                          general_expression R_PAREN switch_tail
                        ;
switch_tail:              L_BRACE decl_cases R_BRACE
                        | single_casestatement
                        ;
decl_cases:               declarations casestatements
                        | declarations
                        | casestatements
                        | %empty {}
                        ;
casestatements:           casestatements casestatement
                        | casestatement
                        ;
casestatement:            CASE constant casestatement
                        | CASE constant statements
                        | DEFAULT COLON statements
                        ;
single_casestatement:     CASE constant single_casestatement
                        | CASE constant statement
                        ;
return_statement:         RETURN optexpr SEMICOLON
                        ;
comp_statement:           L_BRACE decl_statements R_BRACE
                        ;
main_function:            main_header
                            L_BRACE decl_statements R_BRACE
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
