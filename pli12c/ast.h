/*
** vim: ts=4 sw=4 expandtab
*/
/*
** This module defines an abstract syntax tree for PLI12 programs.
*/

#ifndef AST_H
#define AST_H

#include    "std.h"
#include    "t12.h"

typedef struct s_const  *Const;
typedef struct s_expr   *Expr;
typedef struct s_decl   *Decl;
typedef struct s_stmt   *Stmt;
typedef struct s_param  *Param;
typedef struct s_func   *Func;

typedef struct s_types  *Types;
typedef struct s_exprs  *Exprs;
typedef struct s_decls  *Decls;
typedef struct s_stmts  *Stmts;
typedef struct s_params *Params;
typedef struct s_funcs  *Funcs;

//global to count the number of functions in the program.
extern	int			function_count;

typedef enum {
	T_INT, T_REAL, T_BOOL, T_STRING, T_UNKNOWN
} Type;

//List of all types of statements
typedef enum {
    S_ASSIGNMENT, S_READ, S_WRITE, S_IF, S_IF_ELSE, S_WHILE, S_RETURN
} StatementType;

//List of all types of expressions
typedef enum {
    E_IDENTIFIER, E_CONSTANT, E_BINOP, E_UNOP, E_FUNCTION
} ExpressionType;

typedef enum {
    BINOP_OR, BINOP_AND,
    BINOP_EQ, BINOP_NE, BINOP_LT, BINOP_LE, BINOP_GT, BINOP_GE,
    BINOP_ADD, BINOP_SUB, BINOP_MUL, BINOP_DIV
} BinOp;

typedef enum {
    UNOP_NOT, UNOP_UMINUS, UNOP_INT_TO_REAL
} UnOp;

typedef enum {
    BUILTIN, USER_DEFINED
} Status;

//A constant can be 1 of 4 types.
//It has 1 value, corresponding to its respective type.
struct s_const {

	Type	type;
	
	//All possible values
	int		int_value;
	float	real_value;
	bool	bool_value;
	char	*string_value;
};

struct s_expr {

	//The expression type is used to determine which other fields are required
	ExpressionType	e_type;
	
	char			*identifier;
	Const			constant;
	BinOp			binary_operator;
	UnOp			unary_operator;
	Expr			primary_expression;
	Expr			secondary_expression;
	Exprs			expression_list;

    int             place; //store the place or slot num

	int 			lineno;

	Type 			type;	//the soon to be evaluated type of the expression

	Opcode			opcode; //stored by the semantic analyser to make codegen easier (mostly used for function expressions)
};

struct s_decl {

	char	*identifier;
	Type 	type;
	Const	value;
	bool	is_initialized;		//flag to check if variable was initialized or not
								//This is because NULL could be equal to 0.

	int 	lineno;
};

//keep information for any statement type
struct s_stmt {
	
	//The statement type is used to determine which other fields are required
	StatementType	s_type;
	
	char			*identifier;
	Expr			expression;
	Stmts			primary_statement_list; //used for IF/WHILE
	Stmts			secondary_statement_list; //used for ELSE
	
	int 			lineno;

	Type 			type;	//the soon to be evaluated type of the statement
};

struct s_param {
	char	*identifier;
	Type	type;

	int 	lineno;
};

struct s_func {
	char	*identifier;	//function name
	Params	parameters;
	Decls	declarations;
	Stmts	statements;
	Type	type;			//return type
	Status	status;

	int 	lineno;

	int 	num_vars;
	int 	num_tmps;
};

struct s_types {
    Type    t_first;
    Types   t_rest;
};

struct s_exprs {
    Expr    e_first;
    Exprs   e_rest;
};

struct s_decls {
    Decl    d_first;
    Decls   d_rest;
};

struct s_stmts {
    Stmt    s_first;
    Stmts   s_rest;
};

struct s_params {
    Param   p_first;
    Params  p_rest;
};

struct s_funcs {
    Func    f_first;
    Funcs   f_rest;
};



//Haven't written these functions yet...
extern  Expr    make_binop(BinOp binop, int lineno, Expr e1, Expr e2);
extern  Expr    make_unop(UnOp unop, int lineno, Expr e1);

extern  Expr    convert_int_to_real(Expr expr);


//makes the data structures for each of the types.
extern  Funcs   make_functions(Func, Funcs);
extern	Func	make_function(char *, Params , Decls , Stmts , Type, bool, int);
extern	Params	make_parameters(Param, Params);
extern	Param	make_parameter(char *, Type, int);
extern	Decls	make_declarations(Decl, Decls);
extern	Decl	make_declaration(char *, Type, Const, bool, int);
extern	Stmts	make_statements(Stmt, Stmts);
extern	Stmt	make_statement(StatementType, char *, Expr, Stmts, Stmts, int);
extern	Exprs	make_expressions(Expr, Exprs);
extern	Expr	make_expression(ExpressionType, char *,	Const, BinOp, UnOp, Expr, Expr, Exprs, int);
extern	Const	make_const(Type, int, float, bool, char *);


#endif  /* AST_H */
