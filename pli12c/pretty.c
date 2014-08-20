/*
** vim: ts=4 sw=4 expandtab
*/
/*
** This module contains the prettyprinter for PLI12 programs.
*/

#include    <stdio.h>
#include    "ast.h"
#include    "pretty.h"

//included for qsort
#include    <stdlib.h>
//for strcmp()
#include	<string.h>


static	FILE	*output_file; // output file pointer

//This was giving me a warning when it was in the header file.
static int compare_functions(const void *, const void *);

void
pretty_prog(FILE *fp, Funcs prog_funcs)
{
    output_file = fp;
    
	//allocate an array of functions big enough to hold the number of functions we have
	struct s_func function_array[function_count];
    
    Func current_function;
    Funcs remaining_functions = prog_funcs;
    
    //printf("Number of functions: %d\n", function_count);
    
    //add all the functions to an array
    int function_number = 0;
    while (remaining_functions != NULL)
    {
        current_function = remaining_functions->f_first;
    	remaining_functions = remaining_functions->f_rest;
    	
    	function_array[function_number] = *current_function;

    	function_number++;
    }
    
    //sort the functions in the array
    qsort(function_array, function_count, sizeof(struct s_func), compare_functions);
    
    //for each function in sorted array
    for (int i=0;i<function_count;i++)
    {
    	//print a new line before each function (except the first one)
    	if(i>0)
    	{
    		fprintf(output_file,"\n");
    	}	
    	 print_function(function_array[i]);
    	 //printf("name: %s\n", function_array[i].identifier);
    }
}

void
print_function(struct s_func function)
{
	fprintf(output_file,"function %s", function.identifier);
	fprintf(output_file,"(");
	print_parameters(function.parameters);
	fprintf(output_file,")");
	fprintf(output_file," returns ");
	print_type(function.type);
	fprintf(output_file,"\n");
	
	fprintf(output_file,"begin");
	fprintf(output_file,"\n");
	
	print_declarations(function.declarations);
	
	//print a new line
	fprintf(output_file,"\n");
	
	print_statements(function.statements, INDENT_AMOUNT);
	
	fprintf(output_file,"end");
	
	//print a new line
	fprintf(output_file,"\n");
}

void
print_parameters(Params parameters)
{
	Param current_parameter;
	Params remaining_parameters = parameters;
	
	while (remaining_parameters != NULL)
	{
		current_parameter = remaining_parameters->p_first;
		remaining_parameters = remaining_parameters->p_rest;
		
		fprintf(output_file,"%s: ",current_parameter->identifier);
		print_type(current_parameter->type);
		if (remaining_parameters != NULL)	//print the comma (unless there are no more items to print)
		{
			fprintf(output_file,", ");
		}		
	}
}

void
print_declarations(Decls declarations)
{
		Decl current_declaration;
		Decls remaining_declarations = declarations;
		
		while (remaining_declarations != NULL)
		{
			current_declaration = remaining_declarations->d_first;
			remaining_declarations = remaining_declarations->d_rest;
			
			print_identation(INDENT_AMOUNT);
			
			fprintf(output_file,"declare %s ",current_declaration->identifier);
			print_type(current_declaration->type);
			
			if(current_declaration->is_initialized)
			{
				fprintf(output_file," initialize to ");
				print_const(current_declaration->value);
			}
			
			fprintf(output_file,";\n");
		}
}


void
print_statements(Stmts statements, int indent_num)
{
		Stmt current_statement;
		Stmts remaining_statements = statements;
		
		while (remaining_statements != NULL)
		{
			current_statement = remaining_statements->s_first;
			remaining_statements = remaining_statements->s_rest;
			
			print_identation(indent_num);
			
			//print out each statement
			switch(current_statement->s_type)
			{
				case S_ASSIGNMENT:
					fprintf(output_file,"%s := ",current_statement->identifier);
					print_expression(current_statement->expression);
					fprintf(output_file,";");
					break;
				case S_READ:
					fprintf(output_file,"read %s",current_statement->identifier);
					fprintf(output_file,";");
					break;
				case S_WRITE:
					fprintf(output_file,"write ");
					print_expression(current_statement->expression);
					fprintf(output_file,";");
					break;
				case S_IF:
					fprintf(output_file,"if ");
					print_expression(current_statement->expression);
					fprintf(output_file," then\n");
					print_statements(current_statement->primary_statement_list, indent_num + INDENT_AMOUNT);
					print_identation(indent_num);
					fprintf(output_file,"endif");
					break;
				case S_IF_ELSE:
					fprintf(output_file,"if ");
					print_expression(current_statement->expression);
					fprintf(output_file," then\n");
					print_statements(current_statement->primary_statement_list, indent_num + INDENT_AMOUNT);
					print_identation(indent_num);
					fprintf(output_file,"else\n");
					print_statements(current_statement->secondary_statement_list, indent_num + INDENT_AMOUNT);
					print_identation(indent_num);
					fprintf(output_file,"endif");
					break;
				case S_WHILE:
					fprintf(output_file,"while ");
					print_expression(current_statement->expression);
					fprintf(output_file," do\n");
					print_statements(current_statement->primary_statement_list, indent_num + INDENT_AMOUNT);
					print_identation(indent_num);
					fprintf(output_file,"endwhile");
					break;
				case S_RETURN:
					fprintf(output_file,"return ");
					print_expression(current_statement->expression);
					fprintf(output_file,";");
					break;
			}
			fprintf(output_file,"\n");

		}
}

void
print_expressions(Exprs expressions)
{
		Expr current_expression;
		Exprs remaining_expressions = expressions;
		
		while (remaining_expressions != NULL)
		{
			current_expression = remaining_expressions->e_first;
			remaining_expressions = remaining_expressions->e_rest;
			
			print_expression(current_expression);
			
			if (remaining_expressions != NULL)	//print the comma (unless there are no more items to print)
			{
				fprintf(output_file,", ");
			}
		}

}

void
print_expression(Expr expression)
{
	switch(expression->e_type)
	{
		case E_IDENTIFIER:
			fprintf(output_file,"%s",expression->identifier);
			break;
		case E_CONSTANT:
			print_const(expression->constant);
			break;
		case E_BINOP:
			//print brackets around this expression (if it isn't a constant or variable)
			if(expression->primary_expression->e_type != E_CONSTANT && expression->primary_expression->e_type != E_IDENTIFIER)
			{
				fprintf(output_file,"(");
				print_expression(expression->primary_expression);
				fprintf(output_file,")");
			}
			else
			{
				print_expression(expression->primary_expression);
			}
			print_binop(expression->binary_operator);
			//print brackets around this expression (if it isn't a constant or variable)
			if(expression->secondary_expression->e_type != E_CONSTANT && expression->secondary_expression->e_type != E_IDENTIFIER)
			{
				fprintf(output_file,"(");
				print_expression(expression->secondary_expression);
				fprintf(output_file,")");
			}
			else
			{
				print_expression(expression->secondary_expression);
			}
			break;
		case E_UNOP:
			print_unop(expression->unary_operator);
			//print brackets around this expression (if it isn't a constant or variable)
			if(expression->primary_expression->e_type != E_CONSTANT && expression->primary_expression->e_type != E_IDENTIFIER)
			{
				fprintf(output_file,"(");
				print_expression(expression->primary_expression);
				fprintf(output_file,")");
			}
			else
			{
				print_expression(expression->primary_expression);
			}
			break;
		case E_FUNCTION:
			fprintf(output_file,"%s",expression->identifier);
			fprintf(output_file,"(");
			print_expressions(expression->expression_list);
			fprintf(output_file,")");
			break;
	}
}

void
print_type(Type type)
{
	switch(type)
	{
		case T_INT:
			fprintf(output_file,"int");
			break;
		case T_REAL:
			fprintf(output_file,"real");
			break;
		case T_BOOL:
			fprintf(output_file,"bool");
			break;
		case T_STRING:
			fprintf(output_file,"string");
			break;
		//stop the compiler warning.
		case T_UNKNOWN:
			printf("ERROR!, Unknown type!\n");
			break;
	}
}

void
print_const(Const constant)
{
	switch(constant->type)
	{
		case T_INT:
			fprintf(output_file,"%d",constant->int_value);
			break;
		case T_REAL:
			fprintf(output_file,"%f",constant->real_value);
			break;
		case T_BOOL:
			print_bool(constant->bool_value);
			break;
		case T_STRING:
			fprintf(output_file,"%s",constant->string_value);
			break;
		//stop the compiler warning.
		case T_UNKNOWN:
			printf("ERROR!, Unknown type!\n");
			break;
	}
}

void
print_bool(bool value)
{
	fprintf(output_file,(value) ? "true" : "false");
}

void
print_binop(BinOp operator)
{
	fprintf(output_file," ");
	
	switch(operator)
	{
		case BINOP_OR:
			fprintf(output_file,"or");
			break;
		case BINOP_AND:
			fprintf(output_file,"and");
			break;
		case BINOP_EQ:
			fprintf(output_file,"=");
			break;
		case BINOP_NE:
			fprintf(output_file,"!=");
			break;
		case BINOP_LT:
			fprintf(output_file,"<");
			break;
		case BINOP_LE:
			fprintf(output_file,"<=");
			break;
		case BINOP_GT:
			fprintf(output_file,">");
			break;
		case BINOP_GE:
			fprintf(output_file,">=");
			break;
		case BINOP_ADD:
			fprintf(output_file,"+");
			break;
		case BINOP_SUB:
			fprintf(output_file,"-");
			break;
		case BINOP_MUL:
			fprintf(output_file,"*");
			break;
		case BINOP_DIV:
			fprintf(output_file,"/");
			break;
	}
	
	fprintf(output_file," ");

}

void
print_unop(UnOp operator)
{
	//fprintf(output_file," ");

	switch(operator)
	{
		case UNOP_NOT:
			fprintf(output_file,"not");
			break;
		case UNOP_UMINUS:
			fprintf(output_file,"-");
			break;
		case UNOP_INT_TO_REAL:
			fprintf(output_file,"NOT DONE YET!!!!");
			break;
	}
	
	fprintf(output_file," ");
}

void
print_identation(int spaces)
{
	for(int i=0;i<spaces;i++)
	{
		fprintf(output_file," ");
	}
}

//this is used to compare functions (for the sorting of functions)
static int
compare_functions(const void *a, const void *b)
{
	const struct s_func   *af;
    const struct s_func   *bf;

    af = (const struct s_func *) a;
    bf = (const struct s_func *) b;

	//printf("name1: %s\n", (af->identifier));
	//printf("name2: %s\n", (bf->identifier));
	
	return ((strcmp(af->identifier,bf->identifier) > 0) ? 1 : -1);
}

