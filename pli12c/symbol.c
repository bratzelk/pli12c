#include "symbol.h"

//Keep track of all the functions in the program
Sym_funcs	symbol_table;

//Keep track of all the variables in the program
Sym_vars	variable_table;


/*
** These look after the list of known functions.
*/

void
init_with_builtin_functions(void)
{
	//set up the symbol table
	symbol_table = allocate(struct s_sym_funcs);

	symbol_table->s_first = NULL;
	symbol_table->s_rest = NULL;

	init_variables();
	//add in the builtin functions
	add_builtin_functions();

	//printf("number of user functions: %d\n",num_functions());

	return;
}


//Returns the status of a function (i.e. BUILTIN or USER_DEFINED)
Status
get_function_status(const char *name)
{
    Sym_funcs current_function = symbol_table;

    while(current_function != NULL && current_function->s_first != NULL)
    {
        if(streq(current_function->s_first->identifier,name))
        {
            return current_function->s_first->status;
        }
        //next function
        current_function = current_function->s_rest;
    }
    //Error, function name not found
    return 0;
}

bool
add_function(const char *name, Type return_type,
			Types arg_types, Status status)
{
	Sym_funcs current_symbol = symbol_table;

	//first we need to check if the function name is already used
	if(lookup_function(name, &return_type, &arg_types))
	{
		//printf("Function already exists: %s\n",name);
		return FALSE;
	}
	//otherwise we can continue adding the function to the symbol table

	//find the next free slot
	while(current_symbol->s_first != NULL)
	{
		current_symbol = current_symbol->s_rest;
	}
	//set up a new free slot (for the next function)
	Sym_funcs next_symbol = allocate(struct s_sym_funcs);
	next_symbol->s_first = NULL;
	next_symbol->s_rest = NULL;
	current_symbol->s_rest = next_symbol;

	//add in the function
	Sym_func new_function = allocate(struct s_sym_func);
	new_function->identifier = name;
	new_function->return_type = return_type;
	new_function->arg_types = arg_types;
	new_function->status = status;

	current_symbol->s_first = new_function;

	return TRUE;
}

Types
convert_params_to_types(Params parameters)
{
	Types types, current_types;
	types = allocate(struct s_types);
	types->t_first = T_UNKNOWN;
	types->t_rest = NULL;
	current_types = types;

	while(parameters != NULL && parameters->p_first != NULL)
	{
		
		//add in the parameter
		current_types->t_first = parameters->p_first->type;

		//set up space for the next one
		current_types->t_rest = allocate(struct s_types);
		current_types->t_rest->t_first = T_UNKNOWN;
		current_types->t_rest->t_rest = NULL;
		current_types = current_types->t_rest;

		parameters = parameters->p_rest;
	}

	return types;
}


bool
add_user_function(const char *name, Type return_type,
			Types arg_types)
{
	return add_function(name, return_type, arg_types, USER_DEFINED);
}


//this function returns true if the function already exists
//currently only checks the function name
bool
lookup_function(const char *name, Type *return_type_ptr,
			Types *arg_types_ptr)
{
	Sym_funcs current_function = symbol_table;

	while(current_function != NULL && current_function->s_first != NULL)
	{
		if(streq(current_function->s_first->identifier,name))
		{
			//set the return type through the pointer
			if(return_type_ptr != NULL)
			{
				*return_type_ptr = current_function->s_first->return_type;
			}
			//set the arg types through the pointer
			if(arg_types_ptr != NULL)
			{
				*arg_types_ptr = current_function->s_first->arg_types;
			}
			return TRUE;
		}
		//next function
		current_function = current_function->s_rest;
	}
	return FALSE;
}

//returns the number of USER_DEFINED functions in the program
//used for testing and debugging
int
num_functions(void)
{
	int count = 0;
	Sym_funcs current_funcs = symbol_table;

	while(current_funcs != NULL && current_funcs->s_first != NULL)
	{
		count++;
		current_funcs = current_funcs->s_rest;
	}
	//simply subtract the number of builtin functions
	return count - NUMBER_OF_BUILTINS;
}

//this returns a single new Types structure.
//they can then be linked together manually if more than 1 is needed
Types
make_types(Type type)
{
	Types new_types = allocate(struct s_types);
	new_types->t_first = type;
	new_types->t_rest = NULL;

	return new_types;
}

//This simply adds each of the builtin functions to the symbol table.
bool
add_builtin_functions()
{
	const char *name;
	Type return_type;
	Types arg_types;
	Status status = BUILTIN;

	//string_concat(string, string) returns string;
	name = "string_concat";
	return_type = T_STRING;
	//add two string arguments
	arg_types = make_types(T_STRING);
	arg_types->t_rest = make_types(T_STRING);
	add_function(name, return_type,arg_types, status);

	//string_length(string) returns int;
	name = "string_length";
	return_type = T_INT;
	arg_types = make_types(T_STRING);
	add_function(name, return_type,arg_types, status);

	//substring(string, int, int) returns string;
	name = "substring";
	return_type = T_STRING;
	arg_types = make_types(T_STRING);
	arg_types->t_rest = make_types(T_INT);
	arg_types->t_rest->t_rest = make_types(T_INT);
	add_function(name, return_type,arg_types, status);

	//sqrt(real) returns real;
	name = "sqrt";
	return_type = T_REAL;
	arg_types = make_types(T_REAL);
	add_function(name, return_type,arg_types, status);

	//round(real) returns int;
	name = "round";
	return_type = T_INT;
	arg_types = make_types(T_REAL);
	add_function(name, return_type,arg_types, status);

	//trunc(real) returns int;
	name = "trunc";
	return_type = T_INT;
	arg_types = make_types(T_REAL);
	add_function(name, return_type,arg_types, status);

	return TRUE;
}

/*
** These look after the list of known variables in a function,
** including parameters.
*/

void
init_variables(void)
{
	variable_table = allocate(struct s_sym_vars);

	variable_table->v_first = NULL;
	variable_table->v_rest = NULL;

	return;
}

//returns false if a variable of the same name already existed.
bool
add_variable(const char *name, Type type, bool is_init, const char *function_name, int slot_num)
{
	Sym_vars current_variable = variable_table;

	//first, check if the variable already exists
	if(lookup_variable(name,function_name, NULL))
	{
		return FALSE;
	}

	//find the next free slot
	while(current_variable->v_first != NULL)
	{
		current_variable = current_variable->v_rest;
	}
	//set up the next slot
	Sym_vars next_variable = allocate(struct s_sym_vars);
	next_variable->v_first = NULL;
	next_variable->v_rest = NULL;
	current_variable->v_rest = next_variable;

	//add the variable to the structure
	Sym_var new_var = allocate(struct s_sym_var);
	new_var->type = type;
	new_var->identifier = name;
	new_var->is_init = is_init;
	new_var->function_identifier = function_name;

	new_var->slot_num = slot_num;

	current_variable->v_first = new_var;
	//printf("Variable added!\n");
	//printf("Variable added: %s\n", current_variable->v_first->identifier);


	return TRUE;

}

//return true if the variable exists.
//the variable type will returned through the pointer
bool
lookup_variable(const char *name, const char *function_name, Type *variable_type_ptr)
{
	Sym_vars current_variable = variable_table;

	while(current_variable != NULL && current_variable->v_first != NULL)
	{
		//printf("type: %d\n", *type_ptr);

		//if we matched a variable with the same name
		if(streq(current_variable->v_first->identifier,name))
		{
			//now check if it's in the same function (scope)
			if(streq(current_variable->v_first->function_identifier,function_name))
			{
				//set the type in the pointer
				if(variable_type_ptr != NULL)
				{
					//printf("returning the type!\n");
					*variable_type_ptr = current_variable->v_first->type;
				}
				return TRUE;
			}	
		}
		current_variable = current_variable->v_rest;
	}

	return FALSE;

}

//return the variable type
//This has been made redundant now since lookup_variable returns the type through the pointer.
Type
lookup_variable_type(const char *name, const char *function_name)
{
	Sym_vars current_variable = variable_table;

	while(current_variable != NULL && current_variable->v_first != NULL)
	{
		//printf("type: %d\n", *type_ptr);

		//if we matched a variable with the same name
		if(streq(current_variable->v_first->identifier,name))
		{
			//now check if it's in the same function (scope)
			if(streq(current_variable->v_first->function_identifier,function_name))
			{
				return current_variable->v_first->type;
			}	
		}
		current_variable = current_variable->v_rest;
	}

	printf("Error, variable type lookup failed!\n");
	return -1;

}


//returns the slot number of the variable in a given function
int
get_slot_num(const char *name, const char *function_name)
{
	Sym_vars current_variable = variable_table;

	while(current_variable != NULL && current_variable->v_first != NULL)
	{
		//if we matched a variable with the same name
		if(streq(current_variable->v_first->identifier,name))
		{
			//now check if it's in the same function (scope)
			if(streq(current_variable->v_first->function_identifier,function_name))
			{
				return current_variable->v_first->slot_num;
			}	
		}
		current_variable = current_variable->v_rest;
	}

	printf("Error, variable could not be found!\n");
	return 0;

}

void
set_slot_num(const char *name, const char *function_name, int slot_num)
{
	Sym_vars current_variable = variable_table;

	while(current_variable != NULL && current_variable->v_first != NULL)
	{
		//if we matched a variable with the same name
		if(streq(current_variable->v_first->identifier,name))
		{
			//now check if it's in the same function (scope)
			if(streq(current_variable->v_first->function_identifier,function_name))
			{
				current_variable->v_first->slot_num = slot_num;
				return;
				
			}	
		}
		current_variable = current_variable->v_rest;
	}

	printf("Error, variable could not be found!\n");
}

int
num_variables(void)
{
	int count = 0;
	Sym_vars current_variable = variable_table;

	while(current_variable != NULL && current_variable->v_first != NULL)
	{
		count++;
		current_variable = current_variable->v_rest;
	}
	//printf("counted: %d\n", count);
	return count;
}





