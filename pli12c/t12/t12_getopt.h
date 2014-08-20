#undef __GNU_LIBRARY__
#define __GNU_LIBRARY__
/* Declarations for t12_getopt.
   Copyright (C) 1989,90,91,92,93,94,96,97 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your t12_option) any later version.

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

/* For communication from `t12_getopt' to the caller.
   When `t12_getopt' finds an t12_option that takes an argument,
   the argument value is returned here.
   Also, when `ordering' is RETURN_IN_ORDER,
   each non-t12_option ARGV-element is returned here.  */

extern char *t12_optarg;

/* Index in ARGV of the next element to be scanned.
   This is used for communication to and from the caller
   and for communication between successive calls to `t12_getopt'.

   On entry to `t12_getopt', zero means this is the first call; initialize.

   When `t12_getopt' returns -1, this is the index of the first of the
   non-t12_option elements that the caller should itself scan.

   Otherwise, `t12_optind' communicates from one call to the next
   how much of ARGV has been scanned so far.  */

extern int t12_optind;

/* Callers store zero here to inhibit the error message `t12_getopt' prints
   for unrecognized t12_options.  */

extern int t12_opterr;

/* Set to an t12_option character which was unrecognized.  */

extern int t12_optopt;

/* Describe the long-named t12_options requested by the application.
   The LONG_OPTIONS argument to t12_getopt_long or t12_getopt_long_only is a vector
   of `struct t12_option' terminated by an element containing a name which is
   zero.

   The field `has_arg' is:
   t12_no_argument		(or 0) if the t12_option does not take an argument,
   t12_required_argument	(or 1) if the t12_option requires an argument,
   t12_optional_argument 	(or 2) if the t12_option takes an t12_optional argument.

   If the field `flag' is not NULL, it points to a variable that is set
   to the value given in the field `val' when the t12_option is found, but
   left unchanged if the t12_option is not found.

   To have a long-named t12_option do something other than set an `int' to
   a compiled-in constant, such as set a value from `t12_optarg', set the
   t12_option's `flag' field to zero and its `val' field to a nonzero
   value (the equivalent single-letter t12_option character, if there is
   one).  For long t12_options that have a zero `flag' field, `t12_getopt'
   returns the contents of the `val' field.  */

struct t12_option
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

/* Names for the values of the `has_arg' field of `struct t12_option'.  */

#define	t12_no_argument		0
#define t12_required_argument	1
#define t12_optional_argument	2

#if defined (__STDC__) && __STDC__
#ifdef __GNU_LIBRARY__
/* Many other libraries have conflicting prototypes for t12_getopt, with
   differences in the consts, in stdlib.h.  To avoid compilation
   errors, only prototype t12_getopt for the GNU C library.  */
extern int t12_getopt (int argc, char *const *argv, const char *shortopts);
#else /* not __GNU_LIBRARY__ */
extern int t12_getopt ();
#endif /* __GNU_LIBRARY__ */
extern int t12_getopt_long (int argc, char *const *argv, const char *shortopts,
		        const struct t12_option *longopts, int *longind);
extern int t12_getopt_long_only (int argc, char *const *argv,
			     const char *shortopts,
		             const struct t12_option *longopts, int *longind);

/* Internal only.  Users should not call this directly.  */
extern int t12__getopt_internal (int argc, char *const *argv,
			     const char *shortopts,
		             const struct t12_option *longopts, int *longind,
			     int long_only);
#else /* not __STDC__ */
extern int t12_getopt ();
extern int t12_getopt_long ();
extern int t12_getopt_long_only ();

extern int t12__getopt_internal ();
#endif /* __STDC__ */

#ifdef	__cplusplus
}
#endif

#endif /* P2_GETOPT_H */
