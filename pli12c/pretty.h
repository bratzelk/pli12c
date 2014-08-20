/*
** This is the header file for the PLI12 prettyprinter.
*/

#ifndef	PRETTY_H
#define	PRETTY_H

#include <stdio.h>
#include "ast.h"

#define	INDENT_AMOUNT 4

void	pretty_prog(FILE *fp, Funcs funcs);

void	print_function(struct s_func);
void	print_type(Type);
void	print_parameters(Params);
void	print_declarations(Decls);
void	print_const(Const);
void	print_bool(bool);
void	print_statements(Stmts, int);
void	print_expression(Expr);
void	print_binop(BinOp);
void	print_unop(UnOp);
void	print_expressions(Exprs);
void	print_identation(int);


#endif	/* PRETTY_H */
