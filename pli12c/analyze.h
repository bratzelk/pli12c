/*
** This is the header file for the PLI12 semantic analyzer.
*/

#ifndef	ANALYZE_H
#define	ANALYZE_H

#include "ast.h"

#include "symbol.h"

typedef struct s_pcr  *ParamCompareResult;

//stores the result of a comparison between actual and expected parameters in a function call.
struct s_pcr {

	bool 	failed;
	int		param_number;
	Type 	expected_param;
	Type 	actual_param;
};



extern	Funcs	analyze_prog(Funcs funcs);

#endif	/* ANALYZE_H */
