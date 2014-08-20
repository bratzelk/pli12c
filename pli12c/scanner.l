%{
/*
** vim: ts=4 sw=4 expandtab
*/
/*
**  Scanner for PLI12.
*/

#define __USE_SVID
#define __USE_POSIX
#define __USE_XOPEN_EXTENDED
#define _GNU_SOURCE
#define __EXTENSIONS__

#include    <stdio.h>
#include    <string.h>
#include    "std.h"
#include    "pli12c.h"
#include    "parser.h"
#include    "missing.h"

/* Declarations flex should add but doesn't. 
extern  int     pli12yyget_lineno(void);
extern  FILE    *pli12yyget_in(void);
extern  FILE    *pli12yyget_out(void);
extern  int     pli12yyget_leng(void);
extern  char    *pli12yyget_text(void);
extern  void    pli12yyset_lineno(int);
extern  void    pli12yyset_in(FILE *);
extern  void    pli12yyset_out(FILE *);
extern  int     pli12yyget_debug(void);
extern  void    pli12yyset_debug(int);
extern  int     pli12yylex_destroy(void);
*/


#undef  pli12yywrap
#define YY_NO_UNPUT

extern  void    pli12yyinit(void);
extern  int     pli12yywrap(void);

%}


alpha	[a-zA-Z_]
digit	[0-9]
alnum	[a-zA-Z_0-9]
sp		[ \t]
nl		[\n\f]
nonl	[^\n\f]

%%



"int"	   					return INTEGER;
"real"	   					return REAL;
"bool"	   					return BOOL;
"string"					return STRING;

"function" 					{
								//return the line number the token appears on
								pli12yylval.lineno = pli12yylinenum;
								return FUNCTION;
							}
"returns"  					return RETURNS;
"begin"	   					return FUNCTION_BEGIN;
"end" 	   					return FUNCTION_END;

"declare"					{
								//return the line number the token appears on
								pli12yylval.lineno = pli12yylinenum;
								return DECLARE;
							}
"initialize"				return INITIALIZE;
"to"						return TO;


"return"					{
								//return the line number the token appears on
								pli12yylval.lineno = pli12yylinenum;
								return RETURN;
							}

"read"						{
								//return the line number the token appears on
								pli12yylval.lineno = pli12yylinenum;
								return READ;
							}
"write"						{
								//return the line number the token appears on
								pli12yylval.lineno = pli12yylinenum;
								return WRITE;
							}
"while"						{
								//return the line number the token appears on
								pli12yylval.lineno = pli12yylinenum;
								return WHILE; 
							} 
"endwhile"					return END_WHILE;
"if"						{
								//return the line number the token appears on
								pli12yylval.lineno = pli12yylinenum;
								return IF; 
							}
"endif"						return END_IF;
"else"						return ELSE;
"do"						return DO;
"then"						return THEN;
":="						{
								//return the line number the token appears on
								pli12yylval.lineno = pli12yylinenum;
								return ASSIGNMENT_OP;
							}

"or"						return OR;  
"and"						return AND;  
"not"						return NOT;

":"							{
								pli12yylval.lineno = pli12yylinenum;
								return COLON;
							}	
"("							return LEFT_PAREN; 			
")"							return RIGHT_PAREN; 						
";"							return SEMI_COLON; 					
","							return COMMA; 


"+"							return PLUS;
"-"							return MINUS;
"*"							return MULTI;
"/"							return DIV;


">="                        return GE;  
"<="                        return LE; 

"<"							return LT;
">"							return GT;
"="							return EQ;  
"!="						return NE; 


"true"					{
							pli12yylval.Ubool = TRUE;
							return BOOL_CONST;
						}

"false"					{
							pli12yylval.Ubool = FALSE;
							return BOOL_CONST;
						}

{alpha}{alnum}*			{
							pli12yylval.Ustr = strdup(yytext);
							return IDENTIFIER;
						}
						
"#"{nonl}*{nl}			{ 
							pli12yylinenum++;
						}

{digit}+"."{digit}+		{
							pli12yylval.Ureal = atof(yytext);
							return REAL_CONST;
						}
						
{digit}+				{
							pli12yylval.Uint = atoi(yytext);
							return INTEGER_CONST;
						}
						

"\""([^\\\"]|\\.)*"\""	{
							pli12yylval.Ustr = strdup(yytext);
							return STRING_CONST;
						}						

{nl}					{ pli12yylinenum++;	}
{sp}					{}
{nonl} 					{ return GARBAGE;	}

%%



void
pli12yyinit(void)
{
}

int
pli12yywrap(void)
{
    return 1;
}
