# GNU Makefile

# To compile FORTRAN codes. Since many OLD Fortran codes will NOT be run
# properly if compiled by using f77 --- the default GNU Fortran Compiler,
# I prefer to using f2c + gcc.

# By default, all source codes are kept under ./src, all objective files
# under ./obj

# All source files are suffixed by .f, objective files by .o and the
# executive code by .x

#============================================================================
# The meaning of these options are:
#   -A    Produce ANSI C.  Default is old-style C
#   -w8   Suppress warnings when COMMON or EQUIVALENCE
#         forces  odd-word alignment of doubles.
#   -r8   Promote REAL to DOUBLE PRECISION, COMPLEX to
#         DOUBLE  COMPLEX
#   -w    Suppress  all  warning messages, or, if the 
#         option is `-w66', just Fortran 66 compatibility 
#         warnings
#
# -Nn802  It is a very important option without which the
# ******  answer is NOT correct and maybe produces a core.
#         However, I still do NOT know the exact meaning of 
#                          ***********     *************
#         such option until NOW..
#
#============================================================================
DIR := $(shell pwd | sed 's/^.*\///')
EXE := $(DIR).x

FCOMP = f2c
CCOMP = gcc

DIRS := src
DIRO := obj

#=================
vpath %.f $(DIRS)
vpath %.c $(DIRO)
vpath %.o $(DIRO)

OBJS := $(patsubst $(DIRS)/%.f,$(DIRO)/%.o,$(wildcard $(DIRS)/*.f))

# The following paths are for FreeBSD
incl  = -I/usr/local/include
lib   = -L/usr/local/lib -lf2c -lm

CFLAGS = -Wall
F2COPT = -Aw8 -r8 -w66 -Nn802

FTOC = $(FCOMP) $(F2COPT)
CTOO = $(CCOMP) $(CFLAGS) $(incl) -c
OTOX = $(CCOMP) $(lib) -o

#=================
all : $(EXE)

$(EXE) : $(OBJS)
	@echo $(DIR)
	$(OTOX) $@ $(OBJS)
	-rm ../$(EXE)
	cd .. ; ln -s $(DIR)/$(EXE) $(EXE); cd $(DIR)

$(DIRO)/%.o : %.c
	cd $(DIRO); $(CTOO) $< ; cd ..

%.c : %.f
	cd $(DIRO); $(FTOC) ../$< ; cd ..

#=================
.PHONY : cleanall clean cleanc

clean :
	-rm $(DIRO)/*.o

cleanc : clean
	-rm $(DIRO)/*.c

cleanall : clean cleanc
	-rm $(EXE) ../$(EXE)
#============================================================================
