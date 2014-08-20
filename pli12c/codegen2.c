/*
** vim: ts=4 sw=4 expandtab
*/
/*
** This module contains the code generator for PLI12 programs.
*/

#include <string.h>
#include <assert.h>

#include    "ast.h"
#include    "analyze.h"
#include    "t12.h"
#include    "codegen.h"
#include    "symbol.h"
#include    "pretty.h"
#include    "std.h"
#include    "pli12c.h"


Funcs translate_function(Funcs);

Code translate_params(Params);
Code translate_decls(Decls);
Code translate_stmts(Stmts);
Code translate_expr(Expr, int);
Code translate_function_call(Expr, int);
Code translate_unop(Expr);
Code translate_binop(Expr, int);
Code translate_const(Const);

Code make_call(const char *);
Code make_comment(const char *);
Code make_label(const char *, bool);
Code make_end_label(const char *);

Code store_regs_up_to(int, int);
Code load_regs_up_to(int , int);

static Code make_pop(int);
static Code make_push(int);

const char * get_type(Type);

void topntail(char *);


#define	MAX_LABELNAME	(1024) //string buffer for labels
#define	MAX_REGISTERS	(1024)

int frame_count = 0; //holds the number of variables + temporaries for each function (as we process it)
int current_slot_num = 0; //tells us which frame we are currently using

int next_label = 0;

static	char	comment_string[MAX_LABELNAME];

//Store all of the translated code here
Code code;

Funcs remaining_funcs;
bool function_has_returned = FALSE;

Code
translate_prog(Funcs prog)
{

	//call the main function to begin the program.
	code = make_call("main");

	//add the halt instruction.
	Instr halt_instr = allocate(struct s_instr);
	halt_instr->opcode = OP_HALT;
	Code halt_code = instr_to_code(halt_instr);
	code = seq(code, halt_code);


	//Loop through the remaining functions in the AST and translate each of them until there are none remaining.
	remaining_funcs = prog;
	while(remaining_funcs != NULL)
	{
        remaining_funcs = translate_function(remaining_funcs);
	}
    return code;
}

Funcs
translate_function(Funcs remaining_functions)
{
    //reset the frame count for the current function (N + M)
    frame_count = remaining_functions->f_first->num_vars + remaining_functions->f_first->num_tmps;

    //reset the current frame/register number for this function
    current_slot_num = 0;

    //add function label
    code = seq(code, make_label(remaining_functions->f_first->identifier, TRUE));

    //push the stack frames
    code = seq(code, make_push(frame_count));

    //deal with the function declarations and statements
    code = seq(code, translate_params(remaining_functions->f_first->parameters));
    code = seq(code, translate_decls(remaining_functions->f_first->declarations));
    code = seq(code, translate_stmts(remaining_functions->f_first->statements));
    
    //end function label
    code = seq(code, make_end_label(remaining_functions->f_first->identifier));
    
    //pop the stack frames
    code = seq(code, make_pop(frame_count));

    //return from the function
    Instr instr = allocate(struct s_instr);
    instr->opcode = OP_RETURN;
    code = seq(code, instr_to_code(instr));

    //next function
    return remaining_functions->f_rest;
}

Code
translate_params(Params params)
{
	Code return_code = NULL;
	Instr param_instr;
	int register_num;

	//Add params on to the stack
	while(params != NULL && params->p_first != NULL)
	{
		//add a comment, telling us which parameter is stored where
		char *tmp_comment = checked_malloc(MAX_LABELNAME * sizeof(char));
		sprintf(comment_string, "argument %s is in stack slot %d",params->p_first->identifier, current_slot_num);
		strcpy(tmp_comment, comment_string);
		return_code = seq(return_code, make_comment(tmp_comment));

		//We need to remember which stack slot the variable is in, so we store it by doing this
		set_slot_num(params->p_first->identifier, remaining_funcs->f_first->identifier, current_slot_num);

		//the store instruction
		register_num = 0;
		param_instr = allocate(struct s_instr);
		param_instr->opcode = OP_STORE;
		param_instr->int_const = current_slot_num;
		param_instr->rs1 = register_num;

		//increment the slot number
		current_slot_num++;

		//add the instruction
		return_code = seq(return_code, instr_to_code(param_instr));

		//next param
		params = params->p_rest;
	}

	return return_code;
}

Code
translate_decls(Decls decls)
{
	Code return_code = NULL;
	Instr decl_instr;
	int current_register;

	//loop through all the declarations
	while(decls != NULL && decls->d_first != NULL)
	{
		
		//add a comment, telling us which variable is stored in which slot
		char *tmp_comment = checked_malloc(MAX_LABELNAME * sizeof(char));
		sprintf(comment_string, "variable %s is in stack slot %d",decls->d_first->identifier, current_slot_num);
		strcpy(tmp_comment, comment_string);
		return_code = seq(return_code, make_comment(tmp_comment));


		//check if the variable is being initialized!
		if(decls->d_first->is_initialized)
		{
			//create the correct constant for the declaration
			return_code = seq(return_code, translate_const(decls->d_first->value));

			//new instruction
			decl_instr = allocate(struct s_instr);
			
			decl_instr->opcode = OP_STORE;
			decl_instr->int_const = current_slot_num;
			current_register = current_slot_num;
			decl_instr->rs1 = current_register;

			//add the instruction
			return_code = seq(return_code, instr_to_code(decl_instr));
		}

        //now we need to store the variable in the correct stack slot
        set_slot_num(decls->d_first->identifier, remaining_funcs->f_first->identifier, current_slot_num);

		//increment the slot number
		current_slot_num++;

		

		//next declaration
		decls = decls->d_rest;
	}

	return return_code;
}

Code
translate_stmts(Stmts stmts)
{
	Code return_code = NULL;
	Instr stmt_instr;
	Type var_type = T_UNKNOWN;
	char *new_label;
	int start_label, end_label;

    //Todo remove this.
    int current_register = 0;

	//loop through all the statements
	while(stmts != NULL && stmts->s_first != NULL)
	{
		//check which statement type we are dealing with
		switch(stmts->s_first->s_type)
		{
			case S_ASSIGNMENT:

				//add a comment
				return_code = seq(return_code, make_comment("assignment"));

				//process the expression on the RHS
				return_code = seq(return_code, translate_expr(stmts->s_first->expression, 0));

				//assign it to the LHS
				stmt_instr = allocate(struct s_instr);
				stmt_instr->opcode = OP_STORE;
				stmt_instr->int_const = get_slot_num(stmts->s_first->identifier, remaining_funcs->f_first->identifier);//set the slot number to that of the variable on the RHS
				current_register = 0;
				stmt_instr->rs1 = current_register; //use the register that translate_expr found
				return_code = seq(return_code, instr_to_code(stmt_instr));

				break;
			case S_READ:

				//add a comment
				return_code = seq(return_code, make_comment("read"));

				//get the type of the variable we are reading in to (sets var_type pointer)
				lookup_variable(stmts->s_first->identifier, remaining_funcs->f_first->identifier, &var_type);

				stmt_instr = allocate(struct s_instr);
				stmt_instr->opcode = OP_CALL_BUILTIN;

				//call the correct builtin
				switch(var_type)
				{
					case T_INT:
						stmt_instr->string_const = "read_int";
						break;
					case T_REAL:
						stmt_instr->string_const = "read_real";
						break;
					case T_BOOL:
						stmt_instr->string_const = "read_bool";
						break;
					case T_STRING:
						stmt_instr->string_const = "read_string";
						break;
					case T_UNKNOWN:
						//do nothing, stops compiler warning
						break;
				}
				return_code = seq(return_code, instr_to_code(stmt_instr));

				//now we have read in the input to r0, we need to store it in the correct stack slot
				stmt_instr = allocate(struct s_instr);
				stmt_instr->opcode = OP_STORE;
				stmt_instr->int_const = get_slot_num(stmts->s_first->identifier, remaining_funcs->f_first->identifier);
				
				current_register = 0;
				stmt_instr->rs1 = current_register;
				return_code = seq(return_code, instr_to_code(stmt_instr));

				break;

			case S_WRITE:

				//add a comment
				return_code = seq(return_code, make_comment("write"));

				//process the expression
				return_code = seq(return_code, translate_expr(stmts->s_first->expression, 0));

				//call the write function
				stmt_instr = allocate(struct s_instr);
				stmt_instr->opcode = OP_CALL_BUILTIN;
				//work out which built in to call - depending on the type of the expression
				switch(stmts->s_first->expression->type)
				{
					case T_INT:
						stmt_instr->string_const = "print_int";
						break;
					case T_REAL:
						stmt_instr->string_const = "print_real";
						break;
					case T_BOOL:
						stmt_instr->string_const = "print_bool";
						break;
					case T_STRING:
						stmt_instr->string_const = "print_string";
						break;
					case T_UNKNOWN:
						//do nothing, stops compiler warning
						break;
				}
				return_code = seq(return_code, instr_to_code(stmt_instr));
				//we don't need the register anymore
				current_register = 0;

				break;
			case S_IF:
				//get our label ready
				end_label = next_label++;

				//add a comment
				return_code = seq(return_code, make_comment("if"));

				//process the expression
				return_code = seq(return_code, translate_expr(stmts->s_first->expression, 0));

				//jump to the end label if we don't get a true
				stmt_instr = allocate(struct s_instr);
				stmt_instr->opcode = OP_BRANCH_ON_FALSE;

				stmt_instr->rs1 = 0; //save it into register 0

				new_label = checked_malloc(MAX_LABELNAME * sizeof(char));
				sprintf(new_label, "label%d", end_label);
				stmt_instr->string_const = new_label;
				return_code = seq(return_code, instr_to_code(stmt_instr));

				//now we can run our statements inside the if
				return_code = seq(return_code, translate_stmts(stmts->s_first->primary_statement_list));

				//now add our end label
				new_label = checked_malloc(MAX_LABELNAME * sizeof(char));
				sprintf(new_label, "label%d", end_label);
				return_code = seq(return_code, make_label(new_label, FALSE));

				break;
			case S_IF_ELSE:

				//get our labels ready
				start_label = next_label++;
				end_label = next_label++;

				//add a comment
				return_code = seq(return_code, make_comment("if"));

				//process the expression
				return_code = seq(return_code, translate_expr(stmts->s_first->expression, 0));

				//jump to the end label if we don't get a true
				stmt_instr = allocate(struct s_instr);
				stmt_instr->opcode = OP_BRANCH_ON_FALSE;

				stmt_instr->rs1 = 0; //save it into register 0

				new_label = checked_malloc(MAX_LABELNAME * sizeof(char));
				sprintf(new_label, "label%d", start_label);
				stmt_instr->string_const = new_label;
				return_code = seq(return_code, instr_to_code(stmt_instr));

				//now we can run our statements inside the if
				return_code = seq(return_code, translate_stmts(stmts->s_first->primary_statement_list));

				//now we branch to the end
				stmt_instr = allocate(struct s_instr);
				stmt_instr->opcode = OP_BRANCH_UNCOND;
				new_label = checked_malloc(MAX_LABELNAME * sizeof(char));
				sprintf(new_label, "label%d", end_label);
				stmt_instr->string_const = new_label;
				return_code = seq(return_code, instr_to_code(stmt_instr));

				//add in the label for this section of the if statement
				new_label = checked_malloc(MAX_LABELNAME * sizeof(char));
				sprintf(new_label, "label%d", start_label);
				return_code = seq(return_code, make_label(new_label, FALSE));

				//now we process the else part of the if
				return_code = seq(return_code, translate_stmts(stmts->s_first->secondary_statement_list));

				//now add our end label
				new_label = checked_malloc(MAX_LABELNAME * sizeof(char));
				sprintf(new_label, "label%d", end_label);
				return_code = seq(return_code, make_label(new_label, FALSE));

				break;
			case S_WHILE:

				//get our labels ready
				start_label = next_label++;
				end_label = next_label++;

				//add a comment
				return_code = seq(return_code, make_comment("while"));

				//make the start label
				new_label = checked_malloc(MAX_LABELNAME * sizeof(char));
				sprintf(new_label, "label%d", start_label);
				return_code = seq(return_code, make_label(new_label, FALSE));

				//process the expression
				return_code = seq(return_code, translate_expr(stmts->s_first->expression, 0));

				//skip the inside of the while loop if we don't meet the condition...
				stmt_instr = allocate(struct s_instr);
				stmt_instr->opcode = OP_BRANCH_ON_FALSE;

				stmt_instr->rs1 = 0;

				new_label = checked_malloc(MAX_LABELNAME * sizeof(char));
				sprintf(new_label, "label%d", end_label);
				stmt_instr->string_const = new_label;
				return_code = seq(return_code, instr_to_code(stmt_instr));

				//now do all the statements inside the loop
				return_code = seq(return_code, translate_stmts(stmts->s_first->primary_statement_list));

				//now we jump back to the top of the while loop
				stmt_instr = allocate(struct s_instr);
				stmt_instr->opcode = OP_BRANCH_UNCOND;
				new_label = checked_malloc(MAX_LABELNAME * sizeof(char));
				sprintf(new_label, "label%d", start_label);
				stmt_instr->string_const = new_label;
				return_code = seq(return_code, instr_to_code(stmt_instr));

				//now add our end label
				new_label = checked_malloc(MAX_LABELNAME * sizeof(char));
				sprintf(new_label, "label%d", end_label);
				return_code = seq(return_code, make_label(new_label, FALSE));

				break;
			case S_RETURN:
				//add a comment
				return_code = seq(return_code, make_comment("return"));
				//process the expression
				return_code = seq(return_code, translate_expr(stmts->s_first->expression, 0));

                //add the return label
                stmt_instr = allocate(struct s_instr);
                stmt_instr->opcode = OP_BRANCH_UNCOND;
                new_label = checked_malloc(MAX_LABELNAME * sizeof(char));
                sprintf(new_label, "endfunc_%s", stmts->s_first->identifier); //TODO fix last part
                stmt_instr->string_const = new_label;
                return_code = seq(return_code, instr_to_code(stmt_instr));

				break;
		}

		//next statement
		stmts = stmts->s_rest;
	}
	
	return return_code;
}

Code
translate_expr(Expr expr, int place)
{
	Code return_code = NULL;
	Instr instr;

	switch(expr->e_type)
	{
		case E_IDENTIFIER:
			instr = allocate(struct s_instr);
			instr->opcode = OP_LOAD;

			instr->rd = place; //TODO check this?

			instr->int_const = get_slot_num(expr->identifier, remaining_funcs->f_first->identifier); //get the slot number
			return_code = seq(return_code, instr_to_code(instr));

			break;
		case E_CONSTANT:
			return_code = seq(return_code, translate_const(expr->constant));
			break;
		case E_BINOP:
			return_code = seq(return_code, translate_binop(expr, place));
			break;
		case E_UNOP:
			return_code = seq(return_code, translate_unop(expr));
			break;
		case E_FUNCTION:
			return_code = seq(return_code, translate_function_call(expr, place));
			break;

	}
	
	return return_code;
}


//THIS ISNT FINISHED!! - TODO
Code
translate_function_call(Expr expr, int place)
{
	Code return_code = NULL;


    //TODO - Calculate the temporaries correctly
    int num_func_vars = 0;

    //update num temporaries here
    if (place > num_func_vars)
    {
        //f->num_temps = place;
        num_func_vars = place;
    }

    //store all of the registers up to the current place
    return_code = seq(return_code, store_regs_up_to(place, num_func_vars));

    //evaluate function arguments
    while(expr->expression_list != NULL && expr->expression_list->e_first != NULL)
    {
        return_code = seq(return_code, translate_expr(expr->expression_list->e_first, 0));
        expr->expression_list = expr->expression_list->e_rest;
    }

	//call the function
    Instr instr = allocate(struct s_instr);
    instr->opcode = expr->opcode;

    //add 'func_' if it isn't a built-in function
    char *function_str = checked_malloc(MAX_LABELNAME * sizeof(char));
    strcpy(function_str, "func_");
    strcat(function_str, expr->identifier);
    instr->string_const = function_str;
    //instr->string_const = expr->identifier; //TODO remove for builtins

    return_code = seq(return_code, instr_to_code(instr));


    //TODO - Calculate variables correctly
    return_code = seq(return_code, load_regs_up_to(place, num_func_vars));


	//return the r0 pointer (return value of the function)

	return return_code;
}

Code
translate_unop(Expr expr)
{
	Code return_code = NULL;
	Instr instr = allocate(struct s_instr);
	int current_register;

	//we need to first process the operand
	return_code = seq(return_code, translate_expr(expr->primary_expression, 0));

	switch(expr->unary_operator)
	{
		case UNOP_NOT:
			instr->opcode = OP_NOT;
			break;
		case UNOP_INT_TO_REAL:
			instr->opcode = OP_INT_TO_REAL;
			break;
		case UNOP_UMINUS:
			//this should no longer exist, since we changed it to a binary operator
			printf("Error, unary operator doesn't exist!\n");
			break;
	}

	current_register = 0;

	instr->rd = current_register;
	instr->rs1 = current_register;

	return_code = seq(return_code, instr_to_code(instr));
	return return_code;
}

Code
translate_binop(Expr expr, int place)
{
	Code return_code = NULL;
	Instr instr = allocate(struct s_instr);

	instr->opcode = expr->opcode;
	instr->suffix = get_type(expr->primary_expression->type);

	//translate both operands of the operator by recursing
	return_code = seq(return_code, translate_expr(expr->primary_expression, place));
	return_code = seq(return_code, translate_expr(expr->secondary_expression, place+1));

    int left_reg = place;
	int right_reg = place+1;
	

	instr->rd = left_reg;
	instr->rs1 = left_reg;
	instr->rs2 = right_reg;


	return_code = seq(return_code, instr_to_code(instr));

	return return_code;
}

Code
translate_const(Const constant)
{
	Code return_code = NULL;
	Instr instr = allocate(struct s_instr);
	int reg_num;

	switch (constant->type)
	{
		case T_INT:
			instr->opcode = OP_INT_CONST;
			instr->int_const = constant->int_value;
			break;
		case T_REAL:
			instr->opcode = OP_REAL_CONST;
			instr->real_const = constant->real_value;
			break;
		case T_BOOL:
			instr->opcode = OP_BOOL_CONST;
			instr->bool_const = constant->bool_value;
			break;
		case T_STRING:
			instr->opcode = OP_STRING_CONST;
			//trim the string (I had an extra " at each end of the string)
			topntail(constant->string_value);
			instr->string_const = constant->string_value;
			break;
		case T_UNKNOWN:
			//suppress warning for unhandled switch case
			break;
	}

	//set the register
	reg_num = 0;
	instr->rd = reg_num;


	//create the constant
	return_code = seq(return_code, instr_to_code(instr));

	return return_code;
}

Code
make_call(const char *funcname)
{
	//append func_ to the name of each function
	Instr instr = allocate(struct s_instr);
	instr->opcode = OP_CALL;

	//add func_ to the function name
	char *function_str = checked_malloc(MAX_LABELNAME * sizeof(char));
	strcpy(function_str, "func_");
	strcat(function_str, funcname);
	instr->string_const = function_str;

	return instr_to_code(instr);
}

Code
make_comment(const char *comment)
{
	Instr instr = allocate(struct s_instr);
	instr->opcode = OP_COMMENT;
	instr->string_const = comment;
	return instr_to_code(instr);
}

Code
make_label(const char *labelname, bool is_function)
{
	Instr instr = allocate(struct s_instr);
	instr->opcode = OP_LABEL;
	if(is_function)
	{	
		//add func_ to the function name
		char *function_str = checked_malloc(MAX_LABELNAME * sizeof(char));
		strcpy(function_str, "func_");
		strcat(function_str, labelname);

		instr->string_const = function_str;
	}
	else
	{
		instr->string_const = labelname;
	}
	return instr_to_code(instr);
}


Code
make_end_label(const char *labelname)
{
	Instr instr = allocate(struct s_instr);
	instr->opcode = OP_LABEL;

	//add func_ to the function name
	char *function_str = checked_malloc(MAX_LABELNAME * sizeof(char));
	strcpy(function_str, "endfunc_");
	strcat(function_str, labelname);

	instr->string_const = function_str;

	return instr_to_code(instr);
}

static Code
make_push(int frame_size)
{
	Instr instr = allocate(struct s_instr);
	instr->opcode = OP_PUSH_STACK_FRAME;
	instr->int_const = frame_size; 
	return instr_to_code(instr);
}

static Code
make_pop(int frame_size)
{
	Instr instr = allocate(struct s_instr);
	instr->opcode = OP_POP_STACK_FRAME;
	instr->int_const = frame_size;
	return instr_to_code(instr);
}



Code
store_regs_up_to(int current_register, int num_func_vars)
{

    Code return_code = NULL;

    char *comment;
    comment = checked_malloc(MAX_LABELNAME * sizeof(char));
    sprintf(comment, "store_regs_up_to %d", current_register);

    return_code = seq(return_code, make_comment(comment));

    //store registers up to current_register
    for(int i=0; i<current_register; i++)
    {
        Instr instr = allocate(struct s_instr);
        instr->opcode = OP_STORE;
        instr->rs1 = i;
        instr->int_const = num_func_vars+i;

        return_code = seq(return_code,instr_to_code(instr)); 
    }

    return return_code;
}

Code
load_regs_up_to(int current_register, int num_func_vars)
{

    Code return_code = NULL;

    char *comment;
    comment = checked_malloc(MAX_LABELNAME * sizeof(char));
    sprintf(comment, "load_regs_up_to_and_copy %d", current_register);

    return_code = seq(return_code, make_comment(comment));


    //copy function return value in r0 to current_register
    if (current_register > 0)
    {
        Instr instr = allocate(struct s_instr);
        instr->opcode = OP_MOVE;
        instr->rd = current_register;
        instr->rs1 = 0;

        return_code = seq(return_code,instr_to_code(instr)); 
    }

    //load registers up to current_register
    for(int i=0; i<current_register; i++)
    {
        Instr instr = allocate(struct s_instr);
        instr->opcode = OP_LOAD;
        instr->rd = i;
        instr->int_const = num_func_vars+i;

        return_code = seq(return_code,instr_to_code(instr)); 
    }

    char *end_comment;
    end_comment = checked_malloc(MAX_LABELNAME * sizeof(char));
    sprintf(end_comment, "end");

    return_code = seq(return_code, make_comment(end_comment));


    return return_code;
}



const char *
get_type(Type type)
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


//This function trims the extra " I had at the start and end of a string
//It was shamelessly stolen from: http://stackoverflow.com/questions/1726298/strip-first-and-last-character-from-c-string
void
topntail(char *str)
{
    size_t len = strlen(str);
    assert(len >= 2); // or whatever you want to do with short strings
    memmove(str, str+1, len-2);
    str[len-2] = 0;
}
