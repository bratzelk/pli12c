/*
** vim: ts=4 sw=4 expandtab
*/
/*
** This module contains the top level of the PLI12 compiler.
**
*/

#define __USE_ISOC99
#define __USE_XOPEN_EXTENDED

#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>
#include    "std.h"
#include    "pli12c_getopt.h"
#include    "ast.h"
#include    "t12.h"
#include    "pli12c.h"
#include    "pretty.h"
#include    "analyze.h"
#include    "codegen.h"
#include    "symbol.h"
#include    "missing.h"

extern  char    *strdup(const char *);

/*****************************************************************************/

const char      *pli12yyfile = "no input file";
int             pli12yylinenum = 0;

const char      *progname;

Funcs           parsed_prog = NULL;

typedef struct {
    int         error_linenum;
    int         error_seqnum;
    const char  *error_msg;
} ErrorRecord;

#define MAX_PROG_ERRORS    1000

static  int         num_prog_errors = 0;
static  ErrorRecord prog_errors[MAX_PROG_ERRORS];

/*****************************************************************************/

static  void        usage(void);
static  FILE        *checked_fopen(const char *filename, const char *mode);

static  void        report_all_errors(void);
static  int         compare_errors(const void *a, const void *b);
static  void        report_perror_and_exit(const char *msg);

/*****************************************************************************/

int
main(int argc, char **argv)
{
    const char  *in_filename;
    const char  *out_filename;
    int         opt;
    FILE        *fp;
    bool        pretty_print_only;

    progname = argv[0];
    pretty_print_only = FALSE;

    while ((opt = pli12c_getopt(argc, argv, "p")) != EOF) {
        switch (opt) {
            case 'p':
                pretty_print_only = TRUE;
                break;

            default:
                usage();
        }
    }

    if (pli12c_optind + 2 != argc) {
        usage();
    }

    in_filename = argv[pli12c_optind];
    out_filename = argv[pli12c_optind + 1];

    pli12yyin = fopen(in_filename, "r");
    pli12yyfile = in_filename;
    pli12yylinenum = 1;

    init_with_builtin_functions();

    if (pli12yyin == NULL) {
        report_perror_and_exit(in_filename);
        exit(EXIT_FAILURE);
    }

    if (pli12yyparse() != 0) {
        /* The error message will already have been printed. */
        exit(EXIT_FAILURE);
    }

    if (pretty_print_only) {
        fp = checked_fopen(out_filename, "w");
        pretty_prog(fp, parsed_prog);
    } else {
        Funcs   prog;

        prog = analyze_prog(parsed_prog);

        if (num_prog_errors > 0) {
            report_all_errors();
        } else {
            Code    code;

            code = translate_prog(prog);
            fp = checked_fopen(out_filename, "w");
            write_code(fp, code);
        }
    }

    return num_prog_errors > 0 ? 1 : 0;
}

static void
usage(void)
{
    printf("usage: pli12c [-p] pli12_filename output_filename\n");
}

/*****************************************************************************/

static FILE *
checked_fopen(const char *filename, const char *mode)
{
    FILE    *fp;

    fp = fopen(filename, mode);
    if (fp == NULL) {
        report_perror_and_exit(progname);
    }

    return fp;
}

void *
checked_malloc(int num_bytes)
{
    void    *addr;

    addr = malloc((size_t) num_bytes);
    if (addr == NULL) {
        report_internal_error_and_exit("out of memory");
    }

    return addr;
}

char *
checked_strdup(const char *s)
{
    char    *t;

    t = (char *) malloc((size_t) (strlen(s) + 1));
    if (t == NULL) {
        report_internal_error_and_exit("out of memory");
    }

    strcpy(t, s);
    return t;
}

/*****************************************************************************/

void
record_error(int linenum, const char *msg)
{
    if (num_prog_errors >= MAX_PROG_ERRORS) {
        report_all_errors();
        fprintf(stderr, "too many errors; exiting\n");
        exit(EXIT_FAILURE);
    }

    prog_errors[num_prog_errors].error_linenum = linenum;
    prog_errors[num_prog_errors].error_seqnum = num_prog_errors;
    prog_errors[num_prog_errors].error_msg = checked_strdup(msg);
    num_prog_errors++;
}

static void
report_all_errors(void)
{
    int i;
    int linenum;

    qsort(prog_errors, num_prog_errors, sizeof(ErrorRecord), compare_errors);

    for (i = 0; i < num_prog_errors; i++) {
        linenum = prog_errors[i].error_linenum;
        if (linenum > 0) {
            fprintf(stderr, "%s, %d: %s\n",
                pli12yyfile, linenum, prog_errors[i].error_msg);
        } else {
            fprintf(stderr, "%s: %s\n",
                pli12yyfile, prog_errors[i].error_msg);
        }
    }
}

static int
compare_errors(const void *a, const void *b)
{
    const ErrorRecord   *ae;
    const ErrorRecord   *be;
    int                 linenum_diff;

    ae = (const ErrorRecord *) a;
    be = (const ErrorRecord *) b;

    linenum_diff = ae->error_linenum - be->error_linenum;
    if (linenum_diff != 0) {
        return linenum_diff;
    }

    return ae->error_seqnum - be->error_seqnum;
}

static void
report_perror_and_exit(const char *msg)
{
    perror(msg);
    exit(EXIT_FAILURE);
}

void
report_internal_error_and_exit(const char *msg)
{
    fprintf(stderr, "pli12c internal error: %s\n", msg);
    exit(EXIT_FAILURE);
}

/*****************************************************************************/
