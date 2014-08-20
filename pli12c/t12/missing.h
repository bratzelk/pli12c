/*
** bison doesn't put these into t12_parser.h.
*/

extern	int		t12yyparse(void);
extern	const char	*t12yyfile;
extern	int		t12yylinenum;

/*
** flex doesn't put this into t12_lexer.h (it doesn't even create t12_lexer.h).
*/

extern	int		t12yylex(void);
extern	FILE		*t12yyin;

/*
** stdio.h defines this only with some options, not with others.
*/

extern	int		fileno(FILE *);
