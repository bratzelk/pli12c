/*
** This is the header file for the symbol table module of the PLI12 compiler.
**
*/

#ifndef	SYMBOL_H
#define	SYMBOL_H

#include "std.h"
#include "ast.h"

//include these for common functions such as allocate() and checked_malloc()
#include	<stdio.h>
#include	<string.h>
#include	"pli12c.h"


//this is mainly used for testing to correctly compute the number of user_defined functions
#define NUMBER_OF_BUILTINS (6)


typedef struct s_sym_func  *Sym_func;
typedef struct s_sym_funcs  *Sym_funcs;

typedef struct s_sym_var  *Sym_var;
typedef struct s_sym_vars  *Sym_vars;


//a function
struct s_sym_func {
	const char	*identifier;	//function name
	Types	arg_types;
	Type	return_type;
	Status	status;

	//Sym_vars variables;		//variables in the function
};

//a variable
struct s_sym_var {
	const char	*identifier;		//variable name
	Type	type;

	bool	is_init;				//has the variable been initialised?

	int 	slot_num;				//the slot number (in the function below)
	
	const char	*function_identifier;	//function name (that the variable belongs to)
};

//This is the symbol table
struct s_sym_funcs {
    Sym_func    s_first;
    Sym_funcs   s_rest;
};

struct s_sym_vars {
    Sym_var    v_first;
    Sym_vars   v_rest;
};


/*
** These look after the list of known functions.
*/

extern	void	init_with_builtin_functions(void);
extern Status   get_function_status(const char *);
extern	bool	add_user_function(const char *, Type ,Types );
extern	bool	lookup_function(const char *, Type *,Types *);

extern bool	add_function(const char *, Type ,Types , Status );
extern bool add_builtin_functions(void);
extern Types make_types(Type);
extern int num_functions(void);

extern Types convert_params_to_types(Params);

/*
** These look after the list of known variables in a function,
** including parameters.
*/

extern	void	init_variables(void);
extern	bool	add_variable(const char *, Type , bool , const char *, int);
extern	bool	lookup_variable(const char *, const char *, Type *);
extern  int 	get_slot_num(const char *, const char *);
extern  void 	set_slot_num(const char *, const char *, int);
extern	int		num_variables(void);
extern Type 	lookup_variable_type(const char *, const char *);

#endif	/* SYMBOL_H */
