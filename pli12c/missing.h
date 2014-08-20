#ifndef	MISSING_H
#define	MISSING_H

/*
** bison doesn't put these into pli12_parser.h.
*/

extern	int		pli12yyparse(void);
extern	const char	*pli12yyfile;
extern	int		pli12yylinenum;
extern	int		pli12yydebug;

/*
** flex doesn't put this into pli12_lexer.h
** (it doesn't even create pli12_lexer.h).
*/

extern	int		pli12yylex(void);
extern	FILE		*pli12yyin;

/*
** stdio.h defines this only with some options, not with others.
*/

extern	int		fileno(FILE *);

#endif	/* MISSING_H */
