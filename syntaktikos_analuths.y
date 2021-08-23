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
int error_count=0;
int flag_err_type=0; // 0: Token Error (YYTEXT) || 1: String Error (STRBUF)
int scope=0;

HASHTBL *hashtbl;

/** Bison specific functions **/
void yyerror(const char *message);

/** TODO: Write why split T_ICONST/T_FCONST **/
/** TODO: constant Union with type and val **/
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
%token <strval> M_NAME              "m_name"
%token <strval> KEIMENO             "keimeno"

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


%type <strval> program global_declaration global_declarations typename standard_type listspec dims
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
program:                  global_declarations main_function
                        ;
global_declarations:      global_declarations global_declaration
                        | %empty {}
                        ;
global_declaration:     global_var_declaration
                        | func_declaration
                        ;
typename:                 standard_type
                        | M_NAME
                        ;
standard_type:            T_CHAR
                        | T_INT
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
expression:               expression OR_OP expression
                        | expression AND_OP expression
                        | expression EQU_OP expression
                        | expression INEQU_OP expression
                        | expression SUG_OP expression
                        | expression MINUS_OP expression
                        | expression PLUS_OP expression
                        | expression MUL_OP expression
                        | expression DIV_OP expression
                        | NOT_OP expression
                        /**
                        | T_INCDEC variable
                        | variable T_INCDEC
                        **/
                        | variable
                        | variable L_PAREN expression_list R_PAREN
                        | T_LENGTH L_PAREN general_expression R_PAREN
                        | constant
                        | L_PAREN general_expression R_PAREN
                        | L_PAREN standard_type R_PAREN
                        | listexpression
                        ;
variable:                 variable L_BRACK general_expression R_BRACK
                        | decltype M_NAME
                        ;
general_expression:       general_expression T_COMMA general_expression
                        | assignment
                        ;
assignment:               variable ASSIGN assignment
                        | expression
                        ;
expression_list:          general_expression
                        | %empty {}
                        ;
listexpression:           L_BRACK expression_list R_BRACK
                        ;
init_values:              init_values COMMA init_value
                        | init_value
                        ;
var_declaration:          typename variabledefs SEMICOLON
                        ;
variabledefs:             variabledefs COMMA variabledef
                        | variabledef
                        ;
variabledef:              listspec M_NAME dims/****/
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
func_declaration:       full_func_declaration
                        ;
full_func_declaration:    full_par_func_header L_BRACE decl_statements R_BRACE
                        ;
full_par_func_header:     func_header_start L_PAREN parameter_list R_PAREN
                        ;
parameter_list:           parameter_list COMMA typename pass_variabledef
                        | typename pass_variabledef
                        ;
pass_variabledef:         variabledef
                        | M_NAME
                        ;
decl_statements:          declarations statements
                        | declarations
                        | statements
                        | %empty {}
                        ;
declarations:             declarations decltype typename variabledefs SEMICOLON
                        | decltype typename variabledefs SEMICOLON
                        ;
statements:               statements statement
                        | statement
                        ;
statement:                expression_statement
                        | if_statement
                        | while_statement
                        | for_statement
                        | switch_statement
                        | return_statement
                        | comp_statement
                        | BREAK SEMICOLON
                        | SEMICOLON
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
