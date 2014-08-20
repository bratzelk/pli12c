/* t12_getopt_long and t12_getopt_long_only entry points for GNU t12_getopt.
   Copyright (C) 1987,88,89,90,91,92,93,94,96,97 Free Software Foundation, Inc.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "t12_getopt.h"

#if !defined (__STDC__) || !__STDC__
/* This is a separate conditional since some stdc systems
   reject `defined (const)'.  */
#ifndef const
#define const
#endif
#endif

#include <stdio.h>

/* Comment out all this code if we are using the GNU C Library, and are not
   actually compiling the library itself.  This code is part of the GNU C
   Library, but also included in many other GNU distributions.  Compiling
   and linking in this code is a waste when using the GNU C library
   (especially if it is a shared library).  Rather than having every GNU
   program understand `configure --with-gnu-libc' and omit the object files,
   it is simpler to just do this in the source for each such file.  */

#define GETOPT_INTERFACE_VERSION 2
#if !defined (_LIBC) && defined (__GLIBC__) && __GLIBC__ >= 2
#include <gnu-versions.h>
#if _GNU_GETOPT_INTERFACE_VERSION == GETOPT_INTERFACE_VERSION
#define ELIDE_CODE
#endif
#endif

#ifndef XXXELIDE_CODEXXX


/* This needs to come after some library #include
   to get __GNU_LIBRARY__ defined.  */
#ifdef __GNU_LIBRARY__
#include <stdlib.h>
#endif

#ifndef	NULL
#define NULL 0
#endif

int
t12_getopt_long (argc, argv, t12_options, long_options, opt_index)
     int argc;
     char *const *argv;
     const char *t12_options;
     const struct t12_option *long_options;
     int *opt_index;
{
  return t12__getopt_internal (argc, argv, t12_options, long_options, opt_index, 0);
}

/* Like t12_getopt_long, but '-' as well as '--' can indicate a long t12_option.
   If an t12_option that starts with '-' (not '--') doesn't match a long t12_option,
   but does match a short t12_option, it is parsed as a short t12_option
   instead.  */

int
t12_getopt_long_only (argc, argv, t12_options, long_options, opt_index)
     int argc;
     char *const *argv;
     const char *t12_options;
     const struct t12_option *long_options;
     int *opt_index;
{
  return t12__getopt_internal (argc, argv, t12_options, long_options, opt_index, 1);
}


#endif	/* Not ELIDE_CODE.  */

#ifdef TEST

#include <stdio.h>

int
main (argc, argv)
     int argc;
     char **argv;
{
  int c;
  int digit_optind = 0;

  while (1)
    {
      int this_option_optind = t12_optind ? t12_optind : 1;
      int t12_option_index = 0;
      static struct t12_option long_options[] =
      {
	{"add", 1, 0, 0},
	{"append", 0, 0, 0},
	{"delete", 1, 0, 0},
	{"verbose", 0, 0, 0},
	{"create", 0, 0, 0},
	{"file", 1, 0, 0},
	{0, 0, 0, 0}
      };

      c = t12_getopt_long (argc, argv, "abc:d:0123456789",
		       long_options, &t12_option_index);
      if (c == -1)
	break;

      switch (c)
	{
	case 0:
	  printf ("t12_option %s", long_options[t12_option_index].name);
	  if (t12_optarg)
	    printf (" with arg %s", t12_optarg);
	  printf ("\n");
	  break;

	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  if (digit_optind != 0 && digit_optind != this_option_optind)
	    printf ("digits occur in two different argv-elements.\n");
	  digit_optind = this_option_optind;
	  printf ("t12_option %c\n", c);
	  break;

	case 'a':
	  printf ("t12_option a\n");
	  break;

	case 'b':
	  printf ("t12_option b\n");
	  break;

	case 'c':
	  printf ("t12_option c with value `%s'\n", t12_optarg);
	  break;

	case 'd':
	  printf ("t12_option d with value `%s'\n", t12_optarg);
	  break;

	case '?':
	  break;

	default:
	  printf ("?? t12_getopt returned character code 0%o ??\n", c);
	}
    }

  if (t12_optind < argc)
    {
      printf ("non-t12_option ARGV-elements: ");
      while (t12_optind < argc)
	printf ("%s ", argv[t12_optind++]);
      printf ("\n");
    }

  exit (0);
}

#endif /* TEST */
