/*
** This module defines an intermediate representation for T12 programs.
**
*/

#ifndef	T12_H
#define	T12_H

#include "std.h"
#include <stdio.h>

typedef enum {
	OP_COMMENT,
	OP_LABEL,

	OP_PUSH_STACK_FRAME,
	OP_POP_STACK_FRAME,
	OP_LOAD,
	OP_STORE,

	OP_INT_CONST,
	OP_REAL_CONST,
	OP_BOOL_CONST,
	OP_STRING_CONST,

	OP_ADD,
	OP_SUB,
	OP_MUL,
	OP_DIV,

	OP_CMP_EQ,
	OP_CMP_NE,
	OP_CMP_GT,
	OP_CMP_GE,
	OP_CMP_LT,
	OP_CMP_LE,

	OP_AND,
	OP_OR,
	OP_NOT,

	OP_BRANCH_UNCOND,
	OP_BRANCH_ON_TRUE,
	OP_BRANCH_ON_FALSE,
	OP_CALL,
	OP_CALL_BUILTIN,
	OP_RETURN,

	OP_INT_TO_REAL,

	OP_MOVE,

	OP_HALT
} Opcode;

/*
** You will need to look at the code in t12.c to see which fields of this
** structure are meaningful for each kind of opcode.
*/

struct s_instr {
	Opcode		opcode;
	int		rd;
	int		rs1;
	int		rs2;
	int		int_const;
	float		real_const;
	bool		bool_const;
	const char	*string_const;
	const char	*suffix;
};

typedef struct s_instr	*Instr;

/*
** Code represents a sequence of zero or more instructions.
** You can convert an Instr to Code with instr_to_code, and you can create
** a sequence of two Codes with seq.
**
** NULL is perfectly acceptable a value for Code; it represents a sequence
** of zero instructions.
*/

typedef struct s_code	*Code;

struct s_code {
    bool    is_leaf;
    Instr   instr;      /* meaningful if is_leaf */
    Code    code1;      /* meaningful if !is_leaf */
    Code    code2;      /* meaningful if !is_leaf */
};

extern	Code	instr_to_code(Instr instr);
extern	Code	seq(Code code1, Code code2);

/*
** Write out the given instruction sequence to the given file.
*/

extern	void	write_code(FILE *fp, Code code);

#endif	/* T12_H */
