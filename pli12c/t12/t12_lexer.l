%{
/*
**	Scanner for the T12 simulator.
*/

#define	__USE_SVID
#define	__USE_POSIX
#define	__USE_XOPEN_EXTENDED
#define	_GNU_SOURCE
#define	__EXTENSIONS__

#undef	_STRING_H

#include	<stdio.h>
#include	<string.h>
#include	"std.h"
#include	"t12.h"
#include	"t12_parser.h"
#include	"missing.h"

#undef	t12yywrap
#define	YY_NO_UNPUT

extern	void	t12yyinit(void);
extern	int	t12yywrap(void);

const char	*t12yyfile = "no input file";
int		t12yylinenum = 0;

extern	int	t12yyget_lineno(void);
extern	FILE	*t12yyget_in(void);
extern	FILE	*t12yyget_out(void);

/*
**	extern	int		t12yyget_leng(void);
*/

extern	char	*t12yyget_text(void);
extern	void	t12yyset_lineno(int);
extern	void	t12yyset_in(FILE *);
extern	void	t12yyset_out(FILE *);
extern	int	t12yyget_debug(void);
extern	void	t12yyset_debug(int);
extern	int	t12yylex_destroy(void);

%}

alpha	[a-zA-Z_]
digit	[0-9]
alnum	[a-zA-Z_0-9]
sp	[ \t]
nl	[\n\f]
nonl	[^\n\f]

%%

"push_stack_frame"	{ return INSTR_PUSH_STACK_FRAME;	}
"pop_stack_frame"	{ return INSTR_POP_STACK_FRAME;		}
"load"			{ return INSTR_LOAD;			}
"store"			{ return INSTR_STORE;			}

"int_const"		{ return INSTR_INT_CONST;		}
"real_const"		{ return INSTR_REAL_CONST;		}
"bool_const"		{ return INSTR_BOOL_CONST;		}
"string_const"		{ return INSTR_STRING_CONST;		}

"add_int"		{ return INSTR_ADD_INT;			}
"add_real"		{ return INSTR_ADD_REAL;		}
"sub_int"		{ return INSTR_SUB_INT;			}
"sub_real"		{ return INSTR_SUB_REAL;		}
"mul_int"		{ return INSTR_MUL_INT;			}
"mul_real"		{ return INSTR_MUL_REAL;		}
"div_int"		{ return INSTR_DIV_INT;			}
"div_real"		{ return INSTR_DIV_REAL;		}

"cmp_eq_int"		{ return INSTR_CMP_EQ_INT;		}
"cmp_ne_int"		{ return INSTR_CMP_NE_INT;		}
"cmp_gt_int"		{ return INSTR_CMP_GT_INT;		}
"cmp_ge_int"		{ return INSTR_CMP_GE_INT;		}
"cmp_lt_int"		{ return INSTR_CMP_LT_INT;		}
"cmp_le_int"		{ return INSTR_CMP_LE_INT;		}
"cmp_eq_real"		{ return INSTR_CMP_EQ_REAL;		}
"cmp_ne_real"		{ return INSTR_CMP_NE_REAL;		}
"cmp_gt_real"		{ return INSTR_CMP_GT_REAL;		}
"cmp_ge_real"		{ return INSTR_CMP_GE_REAL;		}
"cmp_lt_real"		{ return INSTR_CMP_LT_REAL;		}
"cmp_le_real"		{ return INSTR_CMP_LE_REAL;		}
"cmp_eq_bool"		{ return INSTR_CMP_EQ_BOOL;		}
"cmp_ne_bool"		{ return INSTR_CMP_NE_BOOL;		}
"cmp_gt_bool"		{ return INSTR_CMP_GT_BOOL;		}
"cmp_ge_bool"		{ return INSTR_CMP_GE_BOOL;		}
"cmp_lt_bool"		{ return INSTR_CMP_LT_BOOL;		}
"cmp_le_bool"		{ return INSTR_CMP_LE_BOOL;		}
"cmp_eq_string"		{ return INSTR_CMP_EQ_STRING;		}
"cmp_ne_string"		{ return INSTR_CMP_NE_STRING;		}
"cmp_gt_string"		{ return INSTR_CMP_GT_STRING;		}
"cmp_ge_string"		{ return INSTR_CMP_GE_STRING;		}
"cmp_lt_string"		{ return INSTR_CMP_LT_STRING;		}
"cmp_le_string"		{ return INSTR_CMP_LE_STRING;		}

"and"			{ return INSTR_AND;			}
"or"			{ return INSTR_OR;			}
"not"			{ return INSTR_NOT;			}

"branch_uncond"		{ return INSTR_BRANCH_UNCOND;		}
"branch_on_true"	{ return INSTR_BRANCH_ON_TRUE;		}
"branch_on_false"	{ return INSTR_BRANCH_ON_FALSE;		}

"call"			{ return INSTR_CALL;			}
"call_builtin"		{ return INSTR_CALL_BUILTIN;		}
"return"		{ return INSTR_RETURN;			}

"int_to_real"		{ return INSTR_INT_TO_REAL;		}

"move"			{ return INSTR_MOVE;			}

"debug_reg"		{ return INSTR_DEBUG_REG;		}
"debug_slot"		{ return INSTR_DEBUG_SLOT;		}
"debug_stack"		{ return INSTR_DEBUG_STACK;		}

"halt"			{ return INSTR_HALT;			}

"read_int"		{ return FUNC_READ_INT;			}
"read_real"		{ return FUNC_READ_REAL;		}
"read_bool"		{ return FUNC_READ_BOOL;		}
"read_string"		{ return FUNC_READ_STRING;		}

"print_int"		{ return FUNC_PRINT_INT;		}
"print_real"		{ return FUNC_PRINT_REAL;		}
"print_bool"		{ return FUNC_PRINT_BOOL;		}
"print_string"		{ return FUNC_PRINT_STRING;		}

"string_concat"		{ return FUNC_STRING_CONCAT;		}
"string_length"		{ return FUNC_STRING_LENGTH;		}
"substring"		{ return FUNC_SUBSTRING;		}

"sqrt"			{ return FUNC_SQRT;			}
"trunc"			{ return FUNC_TRUNC;			}
"round"			{ return FUNC_ROUND;			}

","			{ return COMMA;				}
":"			{ return COLON;				}

"#"{nonl}*{nl}		{ t12yylinenum++;		}

{digit}+		{
				t12yylval.Uint = atoi(yytext);
				return NAT_CONST;
			}

{digit}+"."{digit}+	{
				t12yylval.Ureal = atof(yytext);
				return REAL_CONST;
			}

"true"			{
				t12yylval.Ubool = 1;
				return BOOL_CONST;
			}

"false"			{
				t12yylval.Ubool = 0;
				return BOOL_CONST;
			}

"\""([^\\\"]|\\.)*"\""	{
				t12yylval.Ustr = strdup(yytext);
				return STRING_CONST;
			}

"r"{digit}+		{
				t12yylval.Uint = atoi(yytext+1);
				return TOKEN_REG;
			}

{alpha}{alnum}*		{
				t12yylval.Ustr = strdup(yytext);
				return TOKEN_ID;
			}

{nl}			{ t12yylinenum++;		}
{sp}			{}

{nonl} 			{ return GARBAGE;	}

%%

void t12yyinit(void)
{
}

int t12yywrap(void)
{
	return 1;
}
