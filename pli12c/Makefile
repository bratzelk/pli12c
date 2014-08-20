HDR =		pli12c.h t12.h pli12c_getopt.h missing.h std.h \
		ast.h symbol.h pretty.h analyze.h codegen.h
SRC =		pli12c.c t12.c pli12c_getopt.c pli12c_getopt1.c \
		parser.c scanner.c ast.c symbol.c pretty.c analyze.c codegen.c
OBJ =		pli12c.o t12.o pli12c_getopt.o pli12c_getopt1.o \
		parser.o scanner.o ast.o symbol.o pretty.o analyze.o codegen.o
SSRC =		pli12c.c t12.c \
		ast.c symbol.c pretty.c analyze.c codegen.c

CC =		gnuc
# We need -ffloat-store to help ensure consistent results for floating point
# operations on different architectures and with different optimization levels.
CFLAGS =	-ffloat-store
LINKFLAGS =
# CFLAGS =	-ffloat-store -g -O0 -fprofile-arcs -ftest-coverage
# LINKFLAGS =	-g -fprofile-arcs
SPLINT_OPTS =	-mustfreefresh -mustfreeonly -temptrans \
		-predboolint +charindex +charint

.PHONY: all
all:		pli12c tags

pli12c:		$(OBJ)
		$(CC) $(CFLAGS) $(LINKFLAGS) -o pli12c $(OBJ) -lm

parser.c parser.h: parser.y
		/usr/local/Cellar/bison27/2.7.1/bin/bison -p pli12yy --debug -v -d parser.y -o parser.c

scanner.c:	scanner.l parser.h
		flex -s -t -Ppli12yy scanner.l > scanner.c 

splint.out:	$(SSRC) $(HDR) Makefile
		splint $(SPLINT_OPTS) $(SSRC) > splint.out

tags:		$(SRC)
		ctags -w $(SRC)

clean:
		-/bin/rm $(OBJ) parser.c parser.h parser.output scanner.c \
			> /dev/null 2>&1

realclean:	clean
		-/bin/rm pli12c splint.out tags > /dev/null 2>&1

$(OBJ):		$(HDR)

