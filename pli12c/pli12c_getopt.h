#undef __GNU_LIBRARY__
#define __GNU_LIBRARY__
/* Declarations for pli12c_getopt.
   Copyright (C) 1989,90,91,92,93,94,96,97 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your pli12c_option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#ifndef P2_GETOPT_H
#define P2_GETOPT_H 1

#ifdef	__cplusplus
extern "C" {
#endif

/* For communication from `pli12c_getopt' to the caller.
   When `pli12c_getopt' finds an pli12c_option that takes an argument,
   the argument value is returned here.
   Also, when `ordering' is RETURN_IN_ORDER,
   each non-pli12c_option ARGV-element is returned here.  */

extern char *pli12c_optarg;

/* Index in ARGV of the next element to be scanned.
   This is used for communication to and from the caller
   and for communication between successive calls to `pli12c_getopt'.

   On entry to `pli12c_getopt', zero means this is the first call; initialize.

   When `pli12c_getopt' returns -1, this is the index of the first of the
   non-pli12c_option elements that the caller should itself scan.

   Otherwise, `pli12c_optind' communicates from one call to the next
   how much of ARGV has been scanned so far.  */

extern int pli12c_optind;

/* Callers store zero here to inhibit the error message `pli12c_getopt' prints
   for unrecognized pli12c_options.  */

extern int pli12c_opterr;

/* Set to an pli12c_option character which was unrecognized.  */

extern int pli12c_optopt;

/* Describe the long-named pli12c_options requested by the application.
   The LONG_OPTIONS argument to pli12c_getopt_long or pli12c_getopt_long_only is a vector
   of `struct pli12c_option' terminated by an element containing a name which is
   zero.

   The field `has_arg' is:
   pli12c_no_argument		(or 0) if the pli12c_option does not take an argument,
   pli12c_required_argument	(or 1) if the pli12c_option requires an argument,
   pli12c_optional_argument 	(or 2) if the pli12c_option takes an pli12c_optional argument.

   If the field `flag' is not NULL, it points to a variable that is set
   to the value given in the field `val' when the pli12c_option is found, but
   left unchanged if the pli12c_option is not found.

   To have a long-named pli12c_option do something other than set an `int' to
   a compiled-in constant, such as set a value from `pli12c_optarg', set the
   pli12c_option's `flag' field to zero and its `val' field to a nonzero
   value (the equivalent single-letter pli12c_option character, if there is
   one).  For long pli12c_options that have a zero `flag' field, `pli12c_getopt'
   returns the contents of the `val' field.  */

struct pli12c_option
{
#if defined (__STDC__) && __STDC__
  const char *name;
#else
  char *name;
#endif
  /* has_arg can't be an enum because some compilers complain about
     type mismatches in all the code that assumes it is an int.  */
  int has_arg;
  int *flag;
  int val;
};

/* Names for the values of the `has_arg' field of `struct pli12c_option'.  */

#define	pli12c_no_argument		0
#define pli12c_required_argument	1
#define pli12c_optional_argument	2

#if defined (__STDC__) && __STDC__
#ifdef __GNU_LIBRARY__
/* Many other libraries have conflicting prototypes for pli12c_getopt, with
   differences in the consts, in stdlib.h.  To avoid compilation
   errors, only prototype pli12c_getopt for the GNU C library.  */
extern int pli12c_getopt (int argc, char *const *argv, const char *shortopts);
#else /* not __GNU_LIBRARY__ */
extern int pli12c_getopt ();
#endif /* __GNU_LIBRARY__ */
extern int pli12c_getopt_long (int argc, char *const *argv, const char *shortopts,
		        const struct pli12c_option *longopts, int *longind);
extern int pli12c_getopt_long_only (int argc, char *const *argv,
			     const char *shortopts,
		             const struct pli12c_option *longopts, int *longind);

/* Internal only.  Users should not call this directly.  */
extern int pli12c__getopt_internal (int argc, char *const *argv,
			     const char *shortopts,
		             const struct pli12c_option *longopts, int *longind,
			     int long_only);
#else /* not __STDC__ */
extern int pli12c_getopt ();
extern int pli12c_getopt_long ();
extern int pli12c_getopt_long_only ();

extern int pli12c__getopt_internal ();
#endif /* __STDC__ */

#ifdef	__cplusplus
}
#endif

#endif /* P2_GETOPT_H */
