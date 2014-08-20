/*
** vim: ts=4 sw=4 expandtab
*/
/*
** This module contains the semantic analyzer for PLI12 programs.
*/

#include    "ast.h"
#include    "analyze.h"



void analyze_parameters(Params);
void analyze_declarations(Decls);
void analyze_statements(Stmts);
void analyze_expression(Expr);
void analyze_comparision(Expr);
void analyze_mathematical(Expr);

const char *  convert_binop(BinOp operator);
int num_params(Params);
int compare_types(Type, Type);
bool compare_function_arguments(Types , Exprs , ParamCompareResult *, int);
bool compare_argument_number(Types , Exprs , int *, int *);
void get_opcode(Expr);
const char * convert_type(Type);
bool is_mathematical_operator(BinOp);


#define	CONVERT 			(2)
#define MAX_ERROR_STRING    (512)

//store error messages temporarily 
static	char	error_string[MAX_ERROR_STRING];
bool main_exists = FALSE;
Funcs current_func;
int variable_count = 0;


Funcs
analyze_prog(Funcs prog_funcs)
{

	//Do a pass through the AST, adding functions to the symbol table as we go
	//also, look for the main function
	current_func = prog_funcs;
	while(current_func != NULL)
	{
		//if the function name already exist, cause error
		//otherwise it id added to the symbol table
		if(!add_user_function(current_func->f_first->identifier, current_func->f_first->type, convert_params_to_types(current_func->f_first->parameters)))
		{
			sprintf(error_string, "function '%s' redefined",current_func->f_first->identifier);
			record_error(current_func->f_first->lineno, error_string);
		}
		//do a separate check for the main() function being added by the user
		if(!main_exists && streq(current_func->f_first->identifier, "main"))
		{
			main_exists = TRUE;
			//now let's check the return type
			if(current_func->f_first->type != T_INT)
			{
				sprintf(error_string, "return value of main has type %s; expected int",convert_type(current_func->f_first->type));
				record_error(current_func->f_first->lineno, error_string);
			}
			//and check the parameters
			if(num_params(current_func->f_first->parameters) > 0)
			{
				sprintf(error_string, "main has arguments");
				record_error(current_func->f_first->lineno, error_string);
			}

		}

		//next function
		current_func = current_func->f_rest;
	}
	//check if the main function was found
	if(!main_exists)
	{
		sprintf(error_string, "There is no function named 'main'");
		record_error(0, error_string);	
	}

	//now let's do another loop of the tree to analyse everything else
	//doing only one pass was causing an error if you referred to a function which wasn't declared yet!
	current_func = prog_funcs;
	while(current_func != NULL)
	{
		//now analyze the parameters, declarations and statements
		analyze_parameters(current_func->f_first->parameters);
		analyze_declarations(current_func->f_first->declarations);
		analyze_statements(current_func->f_first->statements);

		//save the number of variables the function has
		current_func->f_first->num_vars = num_variables() - variable_count;
		variable_count = num_variables();

		//next function
		current_func = current_func->f_rest;
	}


	//printf("%d varibles in prog.\n",num_variables());

    /* This is only a placeholder. */
    return prog_funcs;
}

//add the variables to the symbol table
void
analyze_parameters(Params parameters)
{
	Params current_params = parameters;
    int slot_num = 0;
	while(current_params != NULL && current_params->p_first != NULL)
	{
		//try to add each parameter to the symbol table
		if(!add_variable(current_params->p_first->identifier, current_params->p_first->type, TRUE, current_func->f_first->identifier, slot_num))
		{
			sprintf(error_string, "variable '%s' redefined",current_params->p_first->identifier);
			record_error(current_params->p_first->lineno, error_string);
		}
        slot_num++;

		current_params = current_params->p_rest;
	}
}

//add the variables to the symbol table
void
analyze_declarations(Decls declarations)
{
	Decls current_decls = declarations;
    int slot_num = 0;
	while(current_decls != NULL && current_decls->d_first != NULL)
	{
		//try to add each variable to the symbol table
		if(!add_variable(current_decls->d_first->identifier, current_decls->d_first->type, current_decls->d_first->is_initialized, current_func->f_first->identifier, slot_num))
		{
			sprintf(error_string, "variable '%s' redefined",current_decls->d_first->identifier);
			record_error(current_decls->d_first->lineno, error_string);
		}

		//now check if they are being initialised to the correct type
		if(current_decls->d_first->is_initialized)
		{
			if (compare_types(current_decls->d_first->type, current_decls->d_first->value->type) != TRUE)
			{
				sprintf(error_string, "type mismatch in initialization of '%s': assigning %s to %s",current_decls->d_first->identifier,convert_type(current_decls->d_first->value->type),convert_type(current_decls->d_first->type));
				record_error(current_decls->d_first->lineno, error_string);
			}
		}

        slot_num++;

		current_decls = current_decls->d_rest;
	}

}

//look for a return type matching the function declaration.
void
analyze_statements(Stmts statements)
{
	Stmts current_stmts = statements;
	Stmt curr_stmt;
	Type var_type = T_UNKNOWN;
	while(current_stmts != NULL && current_stmts->s_first != NULL)
	{
		curr_stmt = current_stmts->s_first;
		switch (curr_stmt->s_type)
		{
			case S_ASSIGNMENT:
				//first, check to see if variable exists
				if(!lookup_variable(curr_stmt->identifier,current_func->f_first->identifier, &var_type))
				{
					sprintf(error_string, "assignment to undefined variable '%s'",curr_stmt->identifier);
					record_error(curr_stmt->lineno, error_string);
				}
				else
				{
					//we know the type of the statement to be the LHS variable type.
					curr_stmt->type = var_type;
					//printf("LHS Type: %s - lineno: %d\n",convert_type(curr_stmt->type),curr_stmt->lineno);
					//now we need to evaluate the type of the expression on the RHS
					analyze_expression(curr_stmt->expression);
					//printf("RHS Type: %s - lineno: %d\n",convert_type(curr_stmt->expression->type),curr_stmt->expression->lineno);
					//compare types of RHS and LHS
					switch (compare_types(curr_stmt->type, curr_stmt->expression->type))
					{
						case TRUE:
							break;
						case FALSE:
							//suppress errors such as a type mismatch due to a unknown function call (since it's a superfluous error)
							if(curr_stmt->expression->type != T_UNKNOWN)
							{
								sprintf(error_string, "type mismatch in assignment to '%s': assigning %s to %s",curr_stmt->identifier,convert_type(curr_stmt->expression->type), convert_type(curr_stmt->type));
								record_error(curr_stmt->lineno, error_string);
							}
							break;
						case CONVERT:
							//if the LHS (in this case) is an int, we have an error.
							//if the RHS is an int, we can convert it
							if(curr_stmt->expression->type != T_INT)
							{
								sprintf(error_string, "type mismatch in assignment to '%s': assigning %s to %s",curr_stmt->identifier,convert_type(curr_stmt->expression->type), convert_type(curr_stmt->type));
								record_error(curr_stmt->lineno, error_string);
							}
							else
							{
								curr_stmt->expression = convert_int_to_real(curr_stmt->expression);
							}
							break;
					}
				}
				
				break;
			case S_READ:
				//first, check to see that we are reading into a valid variable
				if(!lookup_variable(curr_stmt->identifier,current_func->f_first->identifier, &var_type))
				{
					sprintf(error_string, "read into undefined variable '%s'",curr_stmt->identifier);
					record_error(curr_stmt->lineno, error_string);
				}
				else
				{
					//otherwise, set the type of the statement
					curr_stmt->type = var_type;
				}

				break;
			case S_WRITE:
				//set the type of the statement to the type of the expression.
				analyze_expression(curr_stmt->expression);
				curr_stmt->type = curr_stmt->expression->type;
				break;
			case S_IF:
				analyze_expression(curr_stmt->expression);
				analyze_statements(curr_stmt->primary_statement_list);

				//make sure the expression is boolean
				if(curr_stmt->expression->type != T_BOOL)
				{
					sprintf(error_string, "condition of if-then-else is %s; should be bool",convert_type(curr_stmt->expression->type));
					record_error(curr_stmt->expression->lineno, error_string);
				}
				else
				{
					//otherwise, set the statement type.
					curr_stmt->type = curr_stmt->expression->type;
				}

				break;
			case S_IF_ELSE:
				analyze_expression(curr_stmt->expression);
				analyze_statements(curr_stmt->primary_statement_list);
				analyze_statements(curr_stmt->secondary_statement_list);
				
				//make sure the expression is boolean
				if(curr_stmt->expression->type != T_BOOL)
				{
					sprintf(error_string, "condition of if-then-else is %s; should be bool",convert_type(curr_stmt->expression->type));
					record_error(curr_stmt->expression->lineno, error_string);
				}
				else
				{
					//otherwise, set the statement type.
					curr_stmt->type = curr_stmt->expression->type;
				}

				break;
			case S_WHILE:
				analyze_expression(curr_stmt->expression);
				analyze_statements(curr_stmt->primary_statement_list);

				//make sure the expression is boolean
				if(curr_stmt->expression->type != T_BOOL)
				{
					sprintf(error_string, "condition of while loop is %s; should be bool",convert_type(curr_stmt->expression->type));
					record_error(curr_stmt->expression->lineno, error_string);
				}
				else
				{
					//otherwise, set the statement type.
					curr_stmt->type = curr_stmt->expression->type;
				}
				break;
			case S_RETURN:
				analyze_expression(curr_stmt->expression);
				curr_stmt->type = curr_stmt->expression->type;
				//check the actual return type matches the expected return type, otherwise add an implicit conversion if possible
                if(curr_stmt->type == T_INT && current_func->f_first->type == T_REAL)
                {
                    curr_stmt->expression = convert_int_to_real(curr_stmt->expression);
                }
                //we don't actually do this conversion
                // else if(curr_stmt->type == T_REAL && current_func->f_first->type == T_INT)
                // {
                //     curr_stmt->expression = convert_real_to_int(curr_stmt->expression);
                // }
				else if(curr_stmt->type != current_func->f_first->type)
				{
					sprintf(error_string, "type mismatch in return statement; actual %s, expected %s",convert_type(curr_stmt->expression->type),convert_type(current_func->f_first->type));
					record_error(curr_stmt->expression->lineno, error_string);
				}
				break;
		}

		//next statement
		current_stmts = current_stmts->s_rest;
	}
}

void
analyze_expression(Expr expression)
{
	//used to store information about looked-up variables and functions
	Type var_type = T_UNKNOWN;
	Type return_type = T_UNKNOWN;
	Types arg_types = allocate(struct s_types);
	int expected_param_count, given_param_count;
	ParamCompareResult param_result = allocate(struct s_pcr);

	switch (expression->e_type)
	{
		case E_IDENTIFIER:
			//check if variable exists and is in scope
			if(!lookup_variable(expression->identifier,current_func->f_first->identifier, &var_type))
			{
				sprintf(error_string, "reference to undefined variable '%s'",expression->identifier);
				record_error(expression->lineno, error_string);
			}
			//otherwise we need to work out the type of the variable
			else
			{
				expression->type = var_type;
			}

			break;
		case E_CONSTANT:
			expression->type = expression->constant->type;
			break;
		case E_BINOP:
			//check what type of binary operators we are using
			//check for or/and
			if(expression->binary_operator == BINOP_OR || expression->binary_operator == BINOP_AND)
			{
				//make sure the left and right are both boolean
				analyze_expression(expression->primary_expression);
				analyze_expression(expression->secondary_expression);
				if(expression->primary_expression->type != T_BOOL)
				{
					sprintf(error_string, "left operand of '%s' has type %s: expected bool",convert_binop(expression->binary_operator), convert_type(expression->primary_expression->type));
					record_error(expression->lineno, error_string);
				}
				if(expression->secondary_expression->type != T_BOOL)
				{
					sprintf(error_string, "right operand of '%s' has type %s: expected bool",convert_binop(expression->binary_operator), convert_type(expression->secondary_expression->type));
					record_error(expression->lineno, error_string);
				}
				//set the expression type
				expression->type = T_BOOL;
			}
			//check for mathematical operators
			else if(expression->binary_operator == BINOP_ADD || expression->binary_operator == BINOP_SUB || expression->binary_operator == BINOP_MUL || expression->binary_operator == BINOP_DIV)
			{
				analyze_mathematical(expression);
			}
			//check for comparision operators
			else if(expression->binary_operator == BINOP_EQ || expression->binary_operator == BINOP_NE || expression->binary_operator == BINOP_LT || expression->binary_operator == BINOP_LE || expression->binary_operator == BINOP_GT || expression->binary_operator == BINOP_GE)
			{
				analyze_comparision(expression);
			}
			else
			{
				printf("Unknown binary operator!\n");
			}
			//store the opcode to be used later
			get_opcode(expression);

			break;
		case E_UNOP:
			analyze_expression(expression->primary_expression);
			//check if the unary operator has been used on the correct type(s)
			if(expression->unary_operator == UNOP_NOT)
			{
				//check that we are operating on a boolean type!
				if(expression->primary_expression->type != T_BOOL)
				{
					sprintf(error_string, "operand of 'not' has type %s: expected bool", convert_type(expression->primary_expression->type));
					record_error(expression->lineno, error_string);
				}
				//set the expression type
				expression->type = T_BOOL;
			}
			else if(expression->unary_operator == UNOP_UMINUS)
			{
				analyze_expression(expression->primary_expression);
				//check that we are operating on an int or real
				if(expression->primary_expression->type != T_INT && expression->primary_expression->type != T_REAL)
				{
					sprintf(error_string, "operand of '-' has type %s: expected int or real", convert_type(expression->primary_expression->type));
					record_error(expression->lineno, error_string);
				}
				else
				{
					//modify the unary expression, change it to a binary one, i.e. -6 -> 0 - 6
					expression->e_type = E_BINOP;
					expression->binary_operator = BINOP_SUB;
					expression->secondary_expression = expression->primary_expression;
					
					//make the zero constant
					Expr expr_lhs_zero = allocate(struct s_expr);
					expr_lhs_zero->e_type = E_CONSTANT;
					Const zero_const = allocate(struct s_const);

					//make it the correct type (either int or real)
					zero_const->type = expression->secondary_expression->type;
					zero_const->int_value = 0;
					zero_const->real_value = 0.0;

					//add the constant to the new expression
					expr_lhs_zero->constant = zero_const;

					//add the zero constant expression to the lhs of the main expression
					expression->primary_expression = expr_lhs_zero;

					//set the expression type
					expression->type = expression->primary_expression->type;
					
				}
			}
			else
			{
				printf("error!\n Unknown unary unary_operator.\n");
			}
			break;
		case E_FUNCTION:
			//check that function exists.
			if(!lookup_function(expression->identifier, &return_type, &arg_types))
			{
				sprintf(error_string, "call to undefined function '%s'",expression->identifier);
				record_error(expression->lineno, error_string);
			}
			//check that the number of arguments match
			else if(!compare_argument_number(arg_types, expression->expression_list, &expected_param_count, &given_param_count))
			{
				sprintf(error_string, "wrong number of arguments in call to '%s': actual %d, expected %d",expression->identifier, given_param_count, expected_param_count);
				record_error(expression->lineno, error_string);
			}
			//now check that types of the given arguments match the expected arguments
			else if(!compare_function_arguments(arg_types, expression->expression_list, &param_result, 1))
			{
				sprintf(error_string, "type mismatch in argument %d of call to '%s': actual %s, expected %s",param_result->param_number, expression->identifier, convert_type(param_result->actual_param), convert_type(param_result->expected_param));
				record_error(expression->lineno, error_string);
			}
            //otherwise there are no problems
			else
			{
                //then set the expression type to the return type.
				expression->type = return_type;
                //we can also store the op-code (which tells us if it is a builtin or userdefined function call)
                if(get_function_status(expression->identifier) == BUILTIN)
                {
                    expression->opcode = OP_CALL_BUILTIN;
                }
                else
                {
                    expression->opcode = OP_CALL;
                }
                
			}
			
			break;
	}
}

void
analyze_mathematical(Expr expression)
{
	analyze_expression(expression->primary_expression);
	analyze_expression(expression->secondary_expression);

	//make sure both operands are either ints, reals or one of each
	if(expression->primary_expression->type != T_INT && expression->primary_expression->type != T_REAL && expression->primary_expression->type != T_UNKNOWN)
	{
		sprintf(error_string, "left operand of '%s' has type %s: expected int or real",convert_binop(expression->binary_operator), convert_type(expression->primary_expression->type));
		record_error(expression->lineno, error_string);
		//expression->type = T_REAL;
	}
	else if(expression->secondary_expression->type != T_INT && expression->secondary_expression->type != T_REAL && expression->primary_expression->type != T_UNKNOWN)
	{
		sprintf(error_string, "right operand of '%s' has type %s: expected int or real",convert_binop(expression->binary_operator), convert_type(expression->secondary_expression->type));
		record_error(expression->lineno, error_string);
		//expression->type = T_REAL;
	}
	//check if we need to convert the left
	else if(expression->primary_expression->type == T_INT && expression->secondary_expression->type == T_REAL)
	{
		//WE NEED TO CONVERT THE LEFT
		expression->primary_expression = convert_int_to_real(expression->primary_expression);

		//set the expression type
		expression->type = T_REAL;
	}
	//check if we need to convert the right
	else if(expression->primary_expression->type == T_REAL && expression->secondary_expression->type == T_INT)
	{
		//WE NEED TO CONVERT THE RIGHT
		expression->secondary_expression = convert_int_to_real(expression->secondary_expression);

		//set the expression type
		expression->type = T_REAL;
	}
	//otherwise they are both the same
	else
	{
		//set the expression type
		expression->type = expression->primary_expression->type;
	}
}

void
analyze_comparision(Expr expression)
{
	analyze_expression(expression->primary_expression);
	analyze_expression(expression->secondary_expression);

    //is_not_exclusively_mathematical_operator
    bool not_mathematical = !is_mathematical_operator(expression->binary_operator);

    //Check that operands are the same type
    if(expression->primary_expression->type == expression->primary_expression->type && not_mathematical)
    {
        //this is good, do nothing.
    }
	//make sure both operands are either ints, reals or one of each
	else if(expression->primary_expression->type != T_INT && expression->primary_expression->type != T_REAL)
	{
		sprintf(error_string, "left operand of '%s' has type %s: expected int or real",convert_binop(expression->binary_operator), convert_type(expression->primary_expression->type));
		record_error(expression->lineno, error_string);
	}
	else if(expression->secondary_expression->type != T_INT && expression->secondary_expression->type != T_REAL)
	{
		sprintf(error_string, "right operand of '%s' has type %s: expected int or real",convert_binop(expression->binary_operator), convert_type(expression->secondary_expression->type));
		record_error(expression->lineno, error_string);
	}
	//check if we need to convert the left
	else if(expression->primary_expression->type == T_INT && expression->secondary_expression->type == T_REAL)
	{
		//WE NEED TO CONVERT THE LEFT
		expression->primary_expression = convert_int_to_real(expression->primary_expression);
	}
	//check if we need to convert the right
	else if(expression->primary_expression->type == T_REAL && expression->secondary_expression->type == T_INT)
	{
		//WE NEED TO CONVERT THE RIGHT
		expression->secondary_expression = convert_int_to_real(expression->secondary_expression);
	}

	//set the expression type
	expression->type = T_BOOL;
}

//this compares function arguments (expected and given) and returns true if the types are the same.
//if it returns false, it returns details of the error through param_result_ptr
bool
compare_function_arguments(Types arg_types, Exprs expressions, ParamCompareResult *param_result_ptr, int arg_num)
{
	//we already know that the number of arguments are equal

	//no arguments to compare
	if(expressions == NULL || expressions->e_first == NULL || arg_types == NULL)
	{
		return TRUE;
	}
	//get the expression type
	analyze_expression(expressions->e_first);

	//we must have an error if the types don't match
	if(compare_types(arg_types->t_first, expressions->e_first->type) != TRUE)
	{
		//set up the data struct to return
		(*param_result_ptr)->failed = TRUE;
		(*param_result_ptr)->param_number = arg_num;
		(*param_result_ptr)->expected_param = arg_types->t_first;
		(*param_result_ptr)->actual_param = expressions->e_first->type;
		return FALSE;
	}
	//otherwise recurse with the rest of the args
	else
	{
		return compare_function_arguments(arg_types->t_rest, expressions->e_rest, param_result_ptr, ++arg_num);
	}
	
}

//returns true if they are the same
//compares the number of arguments in a function call to actual function
bool
compare_argument_number(Types arg_types, Exprs expressions, int *expected_param_count, int *given_param_count)
{
	*expected_param_count = 0;
	*given_param_count = 0;

	//count the number of args in the actual function
	while(arg_types != NULL && arg_types->t_first != T_UNKNOWN)
	{
		(*expected_param_count)++;
		arg_types = arg_types->t_rest;
	}
	//count the number of args in the function call
	while(expressions != NULL && expressions->e_first != NULL)
	{
		(*given_param_count)++;
		expressions = expressions->e_rest;
	}
	//printf("Given: %d, actual: %d\n", *given_param_count, *expected_param_count);
	if((*expected_param_count) == (*given_param_count))
	{
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}


//simply convert the type enum to a readable form.
//very similar to the pretty printer version but returns a string rather than output
const char *
convert_type(Type type)
{
	switch(type)
	{
		case T_INT:
			return "int";
			break;
		case T_REAL:
			return "real";
			break;
		case T_BOOL:
			return "bool";
			break;
		case T_STRING:
			return "string";
			break;
		//stop the compiler warning.
		case T_UNKNOWN:
			return "Undefined";
			break;
	}
	return "ERRROR!";
}

const char *
convert_binop(BinOp operator)
{
	
	switch(operator)
	{
		case BINOP_OR:
			return("or");
			break;
		case BINOP_AND:
			return("and");
			break;
		case BINOP_EQ:
			return("=");
			break;
		case BINOP_NE:
			return("!=");
			break;
        case BINOP_GE:
            return(">=");
            break;
        case BINOP_LE:
            return("<=");
            break;
		case BINOP_LT:
			return("<");
			break;
		case BINOP_GT:
			return(">");
			break;
		case BINOP_ADD:
			return("+");
			break;
		case BINOP_SUB:
			return("-");
			break;
		case BINOP_MUL:
			return("*");
			break;
		case BINOP_DIV:
			return("/");
			break;
	}
	return "Error!";

}

//looks at two types and returns true if they are the same
//it returns false if they aren't the same,
//except in the case where we have a real and an int.
int
compare_types(Type type1, Type type2)
{
	if(type1 != type2)
	{
		if( (type1 == T_REAL && type2 == T_INT) || (type1 == T_INT && type2 == T_REAL) )
		{
			return CONVERT;
		}
		return FALSE;
	}
	return TRUE;
}

//counts and returns the number of parameters in the list
int
num_params(Params params)
{
	int count = 0;
	Params current_params = params;

	while(current_params != NULL && current_params->p_first != NULL)
	{
		count++;
		current_params = current_params->p_rest;
	}
	return count;
}


//Get the actual op_code for a particular binary operator
void
get_opcode(Expr expression)
{
	switch(expression->binary_operator)
	{
		case BINOP_OR:
			expression->opcode = OP_OR;
			break;
		case BINOP_AND:
			expression->opcode = OP_AND;
			break;
		case BINOP_EQ:
			expression->opcode = OP_CMP_EQ;
			break;
		case BINOP_NE:
			expression->opcode = OP_CMP_NE;
			break;
		case BINOP_LT:
			expression->opcode = OP_CMP_LT;
			break;
		case BINOP_LE:
			expression->opcode = OP_CMP_LE;
			break;
		case BINOP_GT:
			expression->opcode = OP_CMP_GT;
			break;
		case BINOP_GE:
			expression->opcode = OP_CMP_GE;
			break;
		case BINOP_ADD:
			if(expression->type == T_INT || expression->type == T_REAL)
			{
				expression->opcode = OP_ADD;
			}
			else
			{
				//this would be an error, but I have already found it earlier
			}
			break;
		case BINOP_SUB:
			if(expression->type == T_INT || expression->type == T_REAL)
			{
				expression->opcode = OP_SUB;
			}
			else
			{
				//this would be an error, but I have already found it earlier
			}
			break;
		case BINOP_MUL:
			if(expression->type == T_INT || expression->type == T_REAL)
			{
				expression->opcode = OP_MUL;
			}
			else
			{
				//this would be an error, but I have already found it earlier
			}
			break;
		case BINOP_DIV:
			if(expression->type == T_INT || expression->type == T_REAL)
			{
				expression->opcode = OP_DIV;
			}
			else
			{
				//this would be an error, but I have already found it earlier
			}
			break;
	}
}

//Tells us if an operator can only be used mathematically (i.e. for INTS and REALS)
bool
is_mathematical_operator(BinOp binary_operator)
{
    switch(binary_operator)
    {
        case BINOP_ADD:
            return TRUE;
            break;
        case BINOP_SUB:
            return TRUE;
            break;
        case BINOP_MUL:
            return TRUE;
            break;
        case BINOP_DIV:
            return TRUE;
            break;
        default:
            return FALSE;
            break;
    }

    return FALSE;
}

