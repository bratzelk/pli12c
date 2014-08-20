
#include    "ast.h"

//include these for common functions such as allocate() and checked_malloc()
#include	"pli12c.h"

#include    "pretty.h"

//this counts the number of functions in the current program.
int				function_count = 0;

Funcs
make_functions(Func function, Funcs functions)
{
	Funcs current_functions;
	
	current_functions = allocate(struct s_funcs);
	current_functions->f_first = function;
	current_functions->f_rest = functions;
	
	return current_functions;
}

Func
make_function(char *identifier, Params parameters, 
	Decls declarations, Stmts statements, Type type, bool is_builtin, int lineno)
{
	Func current_function;
	
	current_function = allocate(struct s_func);
	current_function->identifier = identifier;
	current_function->parameters = parameters;
	current_function->declarations = declarations;
	current_function->statements = statements;
	current_function->type = type;
	current_function->lineno = lineno;

	current_function->num_vars = 0;
	current_function->num_tmps = 0;

	current_function->status = (is_builtin) ? BUILTIN : USER_DEFINED;
	
	return current_function;
}


Params
make_parameters(Param parameter, Params parameters)
{
	Params current_parameters;
	
	current_parameters = allocate(struct s_params);
	current_parameters->p_first = parameter;
	current_parameters->p_rest = parameters;
	
	return current_parameters;
}


Param
make_parameter(char *identifier, Type type, int lineno)
{
	Param current_parameter;
	
	current_parameter = allocate(struct s_param);
	current_parameter->identifier = identifier;
	current_parameter->type = type;
	current_parameter->lineno = lineno;
	
	return current_parameter;
}

Decls
make_declarations(Decl declaration, Decls declarations)
{
	Decls current_declarations;
	
	current_declarations= allocate(struct s_decls);	
	current_declarations->d_first = declaration;
	current_declarations->d_rest = declarations;
	
	return current_declarations;
}

Decl
make_declaration(char *identifier, Type type, Const init_constant, bool is_initialized, int lineno)
{
	Decl current_declaration;
	
	current_declaration = allocate(struct s_decl);
	current_declaration->identifier = identifier;
	current_declaration->type = type;
	current_declaration->value = init_constant;
	current_declaration->is_initialized = is_initialized;

	current_declaration->lineno = lineno;
	
	return current_declaration;
}

Stmts
make_statements(Stmt statement, Stmts statements)
{
	Stmts current_statements;
	
	current_statements = allocate(struct s_stmts);
	current_statements->s_first = statement;
	current_statements->s_rest = statements;
	
	return current_statements;
}

Stmt
make_statement(StatementType type, char *identifier,
	Expr expression, Stmts statement_list_1,
	Stmts statement_list_2, int lineno)
{
	Stmt current_statement;
	
	current_statement = allocate(struct s_stmt);
	current_statement->s_type = type;
	current_statement->lineno = lineno;
	current_statement->type = T_UNKNOWN;
	
	//should probably remove this and simply set everything (including redundant parts)
	switch(type)
	{
		case S_RETURN:
			current_statement->expression = expression;
			break;
		case S_ASSIGNMENT:
			current_statement->expression = expression;
			current_statement->identifier = identifier;
			break;
		case S_READ:
			current_statement->identifier = identifier;
			break;
		case S_WRITE:
			current_statement->expression = expression;
			break;
		case S_IF:
			current_statement->expression = expression;
			current_statement->primary_statement_list = statement_list_1;
			break;
		case S_IF_ELSE:
			current_statement->expression = expression;
			current_statement->primary_statement_list = statement_list_1;
			current_statement->secondary_statement_list = statement_list_2;
			break;
		case S_WHILE:
			current_statement->expression = expression;
			current_statement->primary_statement_list = statement_list_1;
			break;
	}

	return current_statement;
}

Exprs
make_expressions(Expr expression, Exprs expressions)
{
	Exprs current_expressions;
	
	current_expressions = allocate(struct s_exprs);
	current_expressions->e_first = expression;
	current_expressions->e_rest = expressions;
	
	return current_expressions;
}

Expr
make_expression(ExpressionType type, char *identifier,
	Const constant, BinOp binary_operator, UnOp unary_operator,
	Expr expression_1, Expr expression_2, Exprs expression_list, int lineno)
{
	Expr current_expression;
	
	current_expression = allocate(struct s_expr);
	current_expression->e_type = type;
	current_expression->identifier = identifier;
	current_expression->constant = constant;
	current_expression->binary_operator = binary_operator;
	current_expression->unary_operator = unary_operator;
	current_expression->primary_expression = expression_1;
	current_expression->secondary_expression = expression_2;
	current_expression->expression_list = expression_list;

	current_expression->lineno = lineno;
	current_expression->type = T_UNKNOWN;
	
	return current_expression;
}


Const
make_const(Type type, int int_value, float real_value, bool bool_value, char *string_value)
{
	Const current_const;
	
	current_const= allocate(struct s_const);
	current_const->type = type;

	current_const->int_value = int_value;
	current_const->real_value = real_value;
	current_const->bool_value = bool_value;
	current_const->string_value = string_value;
	
	return current_const;
}

Expr
convert_int_to_real(Expr int_expr)
{
	//create our new expression (unop)
	Expr real_expr = make_expression(E_UNOP, NULL, NULL, NULL, UNOP_INT_TO_REAL, int_expr, NULL, NULL, int_expr->lineno);

	real_expr->type = T_REAL;
	
	return real_expr;
}

