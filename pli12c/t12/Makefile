# Makefile for the t12 simulator.

HDR =	t12.h t12_getopt.h missing.h std.h missing.h
SRC = 	t12.c t12_getopt.c t12_getopt1.c t12_parser.c t12_lexer.c
OBJ = 	t12.o t12_getopt.o t12_getopt1.o t12_parser.o t12_lexer.o
SSRC = 	t12.c

CC =	 	gnuc
# We need -ffloat-store to help ensure consistent results for floating point
# operations on different architectures and with different optimization levels.
CFLAGS =	-ffloat-store -g -O0
LINKFLAGS =	-g
SPLINT_OPTS =	-realcompare -dependenttrans -temptrans -immediatetrans \
		-mustfreefresh -globstate -exportlocal 

.PHONY: all
all:		t12 tags

t12:		$(OBJ)
		$(CC) $(CFLAGS) $(LINKFLAGS) -o t12 $(OBJ) -lm

t12_parser.c t12_parser.h:	t12_parser.y
		bison -p t12yy -d t12_parser.y -o t12_parser.c

t12_lexer.c:	t12_lexer.l t12_parser.h
		flex -s -t -Pt12yy t12_lexer.l > t12_lexer.c

splint.out:	$(SSRC) $(HDR) Makefile
		splint $(SPLINT_OPTS) $(SSRC) > splint.out

tags:		$(SRC)
		ctags $(SRC)

clean:
		-/bin/rm $(OBJ) t12_parser.c t12_parser.h t12_lexer.c \
			splint.out > /dev/null 2>&1

realclean:	clean
		-/bin/rm t12 tags > /dev/null 2>&1

$(OBJ):		$(HDR)
