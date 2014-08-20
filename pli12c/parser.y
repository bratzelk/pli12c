%{
/*
** vim: ts=4 sw=4 expandtab
*/

/*
**	Grammar for PLI12 programs.
*/

#include	<stdio.h>
#include	<ctype.h>
#include	"pli12c.h"
#include	"missing.h"

//include this for common macros.
#include	"std.h"

//include the symbol table functions
#include	"symbol.h"


extern	char	*pli12yytext;

extern	void	pli12yyerror(const char *s);

%}

%union
{
	char			*Ustr;
	int				Uint;
	bool			Ubool;
	float			Ureal;
	
	Funcs  			Ufuncs;
	Func   			Ufunc;
	Decls  			Udeclarations;
	Decl   			Udeclaration;
	Stmts  			Ustatements;
	Stmt   			Ustatement;
	Exprs  			Uexpressions;
	Expr			Uexpression;
	Params 			Uparams;
	Param  			Uparam;	
	Type   			Utype;
	Const			Uconst;
	
	int				lineno;
}

%token	<Ustr>	IDENTIFIER
%token	INTEGER
%token	REAL
%token	BOOL
%token	STRING

%token	<Uint>	INTEGER_CONST
%token	<Ureal>	REAL_CONST
%token	<Ustr>	STRING_CONST
%token	<Ubool>	BOOL_CONST

//FUNCTION is type int because it contains the line number it appears on.
%token 	<lineno>	FUNCTION
%token 	FUNCTION_BEGIN
%token 	FUNCTION_END
%token 	RETURNS
%token 	<lineno>	DECLARE
%token 	INITIALIZE
%token	TO

%token 	<lineno>	READ
%token 	<lineno>	WRITE

%token 	<lineno>	ASSIGNMENT_OP
%token	<lineno>	RETURN
%token	<lineno>	WHILE
%token	END_WHILE
%token	DO
%token	<lineno>	IF
%token	END_IF
%token	THEN
%token	ELSE
%token	OR
%token	AND
%token	NOT

%token 	PLUS
%token 	MINUS
%token 	MULTI
%token 	DIV

%token 	GE
%token 	LE
%token 	EQ
%token 	NE
%token  GT
%token 	LT



%token 	LEFT_PAREN
%token 	RIGHT_PAREN
%token 	<lineno>	COLON
%token 	COMMA
%token 	SEMI_COLON

%token	GARBAGE


/* Types of non-terminal symbols */

%type <Ufunc>			function
%type <Ufuncs>			functions
%type <Udeclaration>	declaration
%type <Udeclarations>	declarations
%type <Ustatement>		statement
%type <Ustatements>		statements
%type <Uparam>			parameter
%type <Uparams>			parameters
%type <Uexpression>		expression
%type <Uexpressions>	expressions

%type <Utype>			type
%type <Uconst>			const


/* Precedence (for maths tokens) */

%left OR
%left AND
%left NOT
%nonassoc GE LE EQ NE GT LT
%left PLUS MINUS
%left MULTI DIV
%left UNARY_MINUS



%start		file

%%

/* This is for you to fill in */

file	:	functions
			{
				parsed_prog = $1;
			}
		|	
			{
				parsed_prog = NULL;
			}
	;

functions	:	function functions
				{
					$$ = make_functions($1, $2);
					function_count++;
				}
			|	function
				{
					$$ = make_functions($1, NULL);
					function_count++;
				}	
	;

function	:	FUNCTION IDENTIFIER LEFT_PAREN parameters RIGHT_PAREN RETURNS type
				FUNCTION_BEGIN declarations statements FUNCTION_END
				{
					$$ = make_function($2, $4, $9, $10, $7, FALSE, $1);
				}
				|
				FUNCTION IDENTIFIER LEFT_PAREN RIGHT_PAREN RETURNS type
				FUNCTION_BEGIN declarations statements FUNCTION_END
				{
					//function with no parameters.
					$$ = make_function($2, NULL, $8, $9, $6, FALSE, $1);
				}
	;
	
parameters	:	parameter COMMA parameters
				{
					$$ = make_parameters($1, $3);
				}
				| parameter
				{
					$$ = make_parameters($1, NULL);
				}
	;
	
parameter	:	IDENTIFIER COLON type
				{
					$$ = make_parameter($1, $3, $2);
				}
	;
	
declarations	:	declaration declarations
					{
						$$ = make_declarations($1, $2);
					}
				//need this to stop an error if a function has no declarations
				|
					{
						$$ = NULL;
					}
	;
	
declaration	:	DECLARE IDENTIFIER type SEMI_COLON
				{
					$$ = make_declaration($2, $3, NULL, FALSE, $1);
				}
			|	DECLARE IDENTIFIER type INITIALIZE TO const SEMI_COLON
				{
					$$ = make_declaration($2, $3, $6, TRUE, $1);
				}
	;
	
statements	: 	statement statements
				{
					$$ = make_statements($1, $2);
				}
			//need this to stop an error if a function has no statements (even though it should have one)
			|
				{
					$$ = NULL;
				}
	;
	
statement	:	RETURN expression SEMI_COLON
				{
					$$ = make_statement(S_RETURN, NULL, $2, NULL, NULL, $1);
				}
			|	IDENTIFIER ASSIGNMENT_OP expression SEMI_COLON
				{
					$$ = make_statement(S_ASSIGNMENT, $1, $3, NULL, NULL, $2);
				}
			|	READ IDENTIFIER SEMI_COLON
				{
					$$ = make_statement(S_READ, $2, NULL, NULL, NULL, $1);
				}
			|	WRITE expression SEMI_COLON
				{
					$$ = make_statement(S_WRITE, NULL, $2, NULL, NULL, $1);
				}
			|	IF expression THEN statements END_IF
				{
					$$ = make_statement(S_IF, NULL, $2, $4, NULL, $1);
				}
			|	IF expression THEN statements ELSE statements END_IF
				{
					$$ = make_statement(S_IF_ELSE, NULL, $2, $4, $6, $1);
				}
			|	WHILE expression DO statements END_WHILE
				{
					$$ = make_statement(S_WHILE, NULL, $2, $4, NULL, $1);
				}
	;	

expressions	:	expression COMMA expressions
				{
					$$ = make_expressions($1, $3);
				}
			|	expression
				{
					$$ = make_expressions($1, NULL);
				}

	;
	
expression	:	const
				{
					//printf("%d\n",pli12yylinenum);
					$$ = make_expression(E_CONSTANT, NULL, $1, NULL, NULL, NULL, NULL, NULL, pli12yylinenum);
				}
			|	IDENTIFIER
				{
					//printf("%s\n",$1);
					$$ = make_expression(E_IDENTIFIER, $1, NULL, NULL, NULL, NULL, NULL, NULL, pli12yylinenum);
				}
			|	LEFT_PAREN expression RIGHT_PAREN
				{
					//printf("Brackets\n");
					$$ = $2;
				}	
			|	IDENTIFIER LEFT_PAREN expressions RIGHT_PAREN
				{
					$$ = make_expression(E_FUNCTION, $1, NULL, NULL, NULL, NULL, NULL, $3, pli12yylinenum);
				}
			
			//binary operators
			|	expression PLUS expression
				{
					$$ = make_expression(E_BINOP, NULL, NULL, BINOP_ADD, NULL, $1, $3, NULL, pli12yylinenum);
				}
			|	expression MINUS expression
				{
					$$ = make_expression(E_BINOP, NULL, NULL, BINOP_SUB, NULL, $1, $3, NULL, pli12yylinenum);
				}
			|	expression DIV expression
				{
					$$ = make_expression(E_BINOP, NULL, NULL, BINOP_DIV, NULL, $1, $3, NULL, pli12yylinenum);
				}
			|	expression MULTI expression
				{
					$$ = make_expression(E_BINOP, NULL, NULL, BINOP_MUL, NULL, $1, $3, NULL, pli12yylinenum);
				}
			|	expression OR expression
				{
					$$ = make_expression(E_BINOP, NULL, NULL, BINOP_OR, NULL, $1, $3, NULL, pli12yylinenum);
				}
			|	expression AND expression
				{
					$$ = make_expression(E_BINOP, NULL, NULL, BINOP_AND, NULL, $1, $3, NULL, pli12yylinenum);
				}
			|	expression EQ expression
				{
					$$ = make_expression(E_BINOP, NULL, NULL, BINOP_EQ, NULL, $1, $3, NULL, pli12yylinenum);
				}
			|	expression NE expression
				{
					$$ = make_expression(E_BINOP, NULL, NULL, BINOP_NE, NULL, $1, $3, NULL, pli12yylinenum);
				}
			|	expression LT expression
				{
					$$ = make_expression(E_BINOP, NULL, NULL, BINOP_LT, NULL, $1, $3, NULL, pli12yylinenum);
				}
			|	expression LE expression
				{
					$$ = make_expression(E_BINOP, NULL, NULL, BINOP_LE, NULL, $1, $3, NULL, pli12yylinenum);
				}
			|	expression GE expression
				{
					$$ = make_expression(E_BINOP, NULL, NULL, BINOP_GE, NULL, $1, $3, NULL, pli12yylinenum);
				}
			|	expression GT expression
				{
					$$ = make_expression(E_BINOP, NULL, NULL, BINOP_GT, NULL, $1, $3, NULL, pli12yylinenum);
				}
				
			//unary operators
			|	MINUS expression %prec UNARY_MINUS
				{
					$$ = make_expression(E_UNOP, NULL, NULL, NULL, UNOP_UMINUS, $2, NULL, NULL, pli12yylinenum);
				}
			|	NOT expression
				{
					$$ = make_expression(E_UNOP, NULL, NULL, NULL, UNOP_NOT, $2, NULL, NULL, pli12yylinenum);
				}
	;

type	:	INTEGER
			{
				$$ = T_INT;
			}
		|	BOOL
			{
				$$ = T_BOOL;
			}
		|	REAL
			{
				$$ = T_REAL;
			}
		|	STRING
			{
				$$ = T_STRING;
			}	
	;
	
const	:	INTEGER_CONST
			{
				$$ = make_const(T_INT, $1, 0, NULL, NULL);
			}
		|	REAL_CONST
			{
				$$ = make_const(T_REAL, NULL, $1, NULL, NULL);
			}
		|	BOOL_CONST
			{
				$$ = make_const(T_BOOL, NULL, 0, $1, NULL);
			}
		|	STRING_CONST
			{
				$$ = make_const(T_STRING, NULL, 0, NULL, $1);
			}	
	;
	

	

	
%%

void pli12yyerror(const char *s)
{
	char		buf[80];

	if (pli12yychar <= 0) {
		sprintf(buf, "premature EOF");
		pli12yylinenum--;
	} else if (pli12yytext[0] == '\n' || pli12yytext[0] == '\f') {
		sprintf(buf, "%s at end of line", s);
	} else if (isprint(pli12yytext[0])) {
		sprintf(buf, "%s at symbol `%s'", s, pli12yytext);
	} else {
		sprintf(buf, "%s at \\%o", s, pli12yytext[0]);
    }

	printf("%s, %d: %s\n", pli12yyfile, pli12yylinenum, buf);
}

