#
# $Id:
#
# make all test
# and printout errors

all : clean allts alltf allto
	grep -n -i fails log

# returns the error code 
# of the command line
# in file retcode

ifdef DJGPP
getreturncode : 
	redir -e $(FILE).log -o $(FILE).log getret $(COMMAND)
else
getreturncode : 
	getret $(COMMAND) !> $(FILE).log !2>$(FILE).log
endif
	

RETCODE=$(wildcard retcode*)

# retcode should be between 0 and 255
# 256 is for halt
# 512+doserror if doserror<>0
ifdef RESFILE
RETVAL=$(shell cat $(RESFILE))
else
ifdef RETCODE
RETVAL=$(shell cat retcode)
else
RETVAL=1024
endif
endif

printretcode:
	echo Return code of $(FILE) is $(RETVAL)

ifeq ($(RETVAL),0)
testsuccess:
	echo Test for $(FILE) success (compiles)
	echo Test for $(FILE) success (compiles) >>log
else
testsuccess:
	echo Test for $(FILE) fails (does not compile) error $(RETVAL)
	echo Test for $(FILE) fails (does not compile) error $(RETVAL)>>log
	echo $(FILE) >> faillist
endif
	
ifneq ($(RETVAL),0)
testfail:
	echo Test for $(FILE) success (does not compile) error $(RETVAL)
	echo Test for $(FILE) success (does not compile) error $(RETVAL)>> log
else
testfail:
	echo Test for $(FILE) fails (does compile and should not)
	echo Test for $(FILE) fails (does compile and should not) >> log
	echo $(FILE) >> faillist
endif
	
ifndef PP
PP=ppc386
endif

ifndef OPT
OPT=
endif

ifdef FILE
OPTFILE=$(wildcard $(FILE).opt)
endif

ifdef OPTFILE
override OPT+=$(OPTFILE)
endif

ifndef FILE
FILE=ts00001.pp
endif

testone :
	make getreturncode 'COMMAND=$(PP) $(OPT) $(FILE).pp'
	make printretcode 'FILE=$(FILE)'

%.res : %.pp
	make testone 'FILE=$*'
	cat retcode > $*.res
	make testsuccess 'FILE=$*' 'RESFILE=$*.res'

%.ref : %.pp
	make testone 'FILE=$*'
	cat retcode > $*.ref
	make testfail 'FILE=$*' 'RESFILE=$*.ref'

allts : $(patsubst %.pp,%.res,$(wildcard ts*.pp))

alltf : $(patsubst %.pp,%.ref,$(wildcard tf*.pp))

allto : $(patsubst %.pp,%.res,$(wildcard to*.pp))

clean :
	-rm *.re* *.o *.ppu *.exe log faillist

# $Log:
