/*
** This is the main module of the PLI12 compiler. This header file declares
** the facilities this module makes available to the rest of the compiler.
*/

#ifndef	PLI12C_H
#define	PLI12C_H

#include	"std.h"
#include	"ast.h"

/*
** parsed_prog holds the AST after parsing.
**
** pli12yyfile is the name of the file given to the PLI12 parser, while
** pli12yylinenum is the number of the current line in that file.
*/

#ifndef NULL
    #define NULL ((void*)0)
#endif

extern	Funcs		parsed_prog;

extern	const char	*pli12yyfile;
extern	int		pli12yylinenum;

/*
** Like malloc, but never returns NULL (it aborts the program instead
** when it runs out of memory).
*/

extern	void		*checked_malloc(int size);

/*
** Like strdup, but never returns NULL (it aborts the program instead
** when it runs out of memory).
*/

extern	char		*checked_strdup(const char *s);

/*
** Records an error message for later reporting. The reason for not reporting
** each error as it is detected is that this way, the error messages can be
** sorted by line number. This is convenient for the users of real compilers.
** In this project, it is useful because it eliminates one source of spurious
** discrepancies between actual and expected error output files.
*/

extern  void        	record_error(int linenum, const char *msg);

/*
** If your compiler encounters a violation of its own invariants (e.g. you
** discover a semantic error during code generation, even though compilation
** is not supposed to proceed to code generation in the presence of semantic
** errors), call this function to abort the program.
*/

extern  void        	report_internal_error_and_exit(const char *msg);

#endif	/* PLI12C_H */
