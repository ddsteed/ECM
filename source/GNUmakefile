# GNU makefile

# To compile all the source codes under all subdirectories 
#============================================================================
DIRS := $(shell ls -F | grep /$ | sed 's/\///')

MAKE = gmake

all : 
	@for dir in $(DIRS); do $(MAKE) -C $$dir; done

#=================
.PHONY : cleanall clean cleanc

clean :
	@for dir in $(DIRS); do $(MAKE) -C $$dir clean; done

cleanc : clean
	@for dir in $(DIRS); do $(MAKE) -C $$dir cleanc; done

cleanall : clean cleanc
	@for dir in $(DIRS); do $(MAKE) -C $$dir cleanall; done
#============================================================================
