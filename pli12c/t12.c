/*
** vim: ts=4 sw=4 expandtab
*/
/*
** This module contains the code to convert the T12 intermediate representation
** into T12 assembly code.
**
*/

#define __USE_ISOC99

#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>
#include    "std.h"
#include    "pli12c.h"
#include    "t12.h"

/*****************************************************************************/



extern  void    write_instr(FILE *fp, Instr instr);

/*****************************************************************************/

Code
instr_to_code(Instr instr)
{
    Code    code;

    code = allocate(struct s_code);
    code->is_leaf = TRUE;
    code->instr = instr;
    return code;
}

Code
seq(Code code1, Code code2)
{
    Code    code;

    if (code1 == NULL) {
        return code2;
    } else if (code2 == NULL) {
        return code1;
    }

    code = allocate(struct s_code);
    code->is_leaf = FALSE;
    code->code1 = code1;
    code->code2 = code2;
    return code;
}

void
write_code(FILE *fp, Code code)
{
    if (code == NULL) {
        return;
    }

    if (code->is_leaf) {
        write_instr(fp, code->instr);
    } else {
        write_code(fp, code->code1);
        write_code(fp, code->code2);
    }
}

void
write_instr(FILE *fp, Instr instr)
{
    switch (instr->opcode) {
        case OP_COMMENT:
            fprintf(fp, "# %s\n", instr->string_const);
            break;

        case OP_LABEL:
            fprintf(fp, "%s:\n", instr->string_const);
            break;

        case OP_PUSH_STACK_FRAME:
            fprintf(fp, "\tpush_stack_frame %d\n", instr->int_const);
            break;

        case OP_POP_STACK_FRAME:
            fprintf(fp, "\tpop_stack_frame %d\n", instr->int_const);
            break;

        case OP_LOAD:
            fprintf(fp, "\tload r%d, %d\n", instr->rd, instr->int_const);
            break;

        case OP_STORE:
            fprintf(fp, "\tstore %d, r%d\n", instr->int_const, instr->rs1);
            break;

        case OP_INT_CONST:
            fprintf(fp, "\tint_const r%d, %d\n", instr->rd, instr->int_const);
            break;

        case OP_REAL_CONST:
            fprintf(fp, "\treal_const r%d, %f\n", instr->rd, instr->real_const);
            break;

        case OP_BOOL_CONST:
            fprintf(fp, "\tbool_const r%d, %s\n", instr->rd,
                instr->bool_const ? "true" : "false");
            break;

        case OP_STRING_CONST:
            fprintf(fp, "\tstring_const r%d, \"%s\"\n",
                instr->rd, instr->string_const);
            break;

        case OP_ADD:
            fprintf(fp, "\tadd_%s r%d, r%d, r%d\n",
                instr->suffix, instr->rd, instr->rs1, instr->rs2);
            break;

        case OP_SUB:
            fprintf(fp, "\tsub_%s r%d, r%d, r%d\n",
                instr->suffix, instr->rd, instr->rs1, instr->rs2);
            break;

        case OP_MUL:
            fprintf(fp, "\tmul_%s r%d, r%d, r%d\n",
                instr->suffix, instr->rd, instr->rs1, instr->rs2);
            break;

        case OP_DIV:
            fprintf(fp, "\tdiv_%s r%d, r%d, r%d\n",
                instr->suffix, instr->rd, instr->rs1, instr->rs2);
            break;

        case OP_CMP_EQ:
            fprintf(fp, "\tcmp_eq_%s r%d, r%d, r%d\n",
                instr->suffix, instr->rd, instr->rs1, instr->rs2);
            break;

        case OP_CMP_NE:
            fprintf(fp, "\tcmp_ne_%s r%d, r%d, r%d\n",
                instr->suffix, instr->rd, instr->rs1, instr->rs2);
            break;

        case OP_CMP_LT:
            fprintf(fp, "\tcmp_lt_%s r%d, r%d, r%d\n",
                instr->suffix, instr->rd, instr->rs1, instr->rs2);
            break;

        case OP_CMP_LE:
            fprintf(fp, "\tcmp_le_%s r%d, r%d, r%d\n",
                instr->suffix, instr->rd, instr->rs1, instr->rs2);
            break;

        case OP_CMP_GT:
            fprintf(fp, "\tcmp_gt_%s r%d, r%d, r%d\n",
                instr->suffix, instr->rd, instr->rs1, instr->rs2);
            break;

        case OP_CMP_GE:
            fprintf(fp, "\tcmp_ge_%s r%d, r%d, r%d\n",
                instr->suffix, instr->rd, instr->rs1, instr->rs2);
            break;

        case OP_AND:
            fprintf(fp, "\tand r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);
            break;

        case OP_OR:
            fprintf(fp, "\tor r%d, r%d, r%d\n",
                instr->rd, instr->rs1, instr->rs2);
            break;

        case OP_NOT:
            fprintf(fp, "\tnot r%d, r%d\n", instr->rd, instr->rs1);
            break;

        case OP_BRANCH_UNCOND:
            fprintf(fp, "\tbranch_uncond %s\n", instr->string_const);
            break;

        case OP_BRANCH_ON_TRUE:
            fprintf(fp, "\tbranch_on_true r%d, %s\n",
                instr->rs1, instr->string_const);
            break;

        case OP_BRANCH_ON_FALSE:
            fprintf(fp, "\tbranch_on_false r%d, %s\n",
                instr->rs1, instr->string_const);
            break;

        case OP_INT_TO_REAL:
            fprintf(fp, "\tint_to_real r%d, r%d\n", instr->rd, instr->rs1);
            break;

        case OP_MOVE:
            fprintf(fp, "\tmove r%d, r%d\n", instr->rd, instr->rs1);
            break;

        case OP_CALL:
            fprintf(fp, "\tcall %s\n", instr->string_const);
            break;

        case OP_CALL_BUILTIN:
            fprintf(fp, "\tcall_builtin %s\n", instr->string_const);
            break;

        case OP_RETURN:
            fprintf(fp, "\treturn\n");
            break;

        case OP_HALT:
            fprintf(fp, "\thalt\n");
            break;
    }
}
