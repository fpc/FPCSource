#
# $Id$
#
# make all test
# and printout errors

all : clean allts alltf allto alltest
	grep -n -i fails log

# returns the error code 
# of the command line
# in file retcode

ifdef DJGPP

EXEEXT=.exe

getreturncode : 
	redir -e $(FILE).log -o $(FILE).log getret $(COMMAND)
	cp retcode $(FILE).$(RESEXT)
else

EXEEXT=
getreturncode : 
	getret $(COMMAND) !> $(FILE).log !2>$(FILE).log
	cp retcode $(FILE).$(RESEXT)
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
	
ifdef FILE
ifneq ($wildcard $(FILE).exc),)
EXERETVAL:=$(shell cat $(FILE).exc)
else
EXERETVAL=-1
endif
else
EXERETVAL=-2
endif

ifeq ($(EXERETVAL),0)
testexecsuccess:
	echo Test for exec $(FILE) success (runs without error)
	echo Test for $(FILE) success (runs without error) >>log
else
testexecsuccess:
	echo Test for exec $(FILE) fails exec error $(RETVAL)
	echo Test for exec $(FILE) fails exec error $(RETVAL)>>log
	echo $(FILE) >> faillist
endif
	
ifneq ($(wildcard $(FILE)$(EXEEXT)),)
testexec:
	redir -e $(FILE).elg -o$(FILE).elg getret $(FILE)$(EXEEXT)
	cp retcode $(FILE).exc
	make testexecsuccess 'FILE=$(FILE)' 
else
testexec:
	echo No exefile $(FILE)$(EXEEXT)
	make testexecsuccess 'FILE=$(FILE)' 
	true
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
else
export PP
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
	make getreturncode 'COMMAND=$(PP) $(OPT) $(FILE).pp' 'RESEXT=$(RESEXT)'
	make printretcode 'FILE=$(FILE)'

%.res : %.pp
	make testone 'FILE=$*' 'RESEXT=res'
	make testsuccess 'FILE=$*' 'RESFILE=$*.res'

%.ref : %.pp
	make testone 'FILE=$*' 'RESEXT=ref'
	make testfail 'FILE=$*' 'RESFILE=$*.ref'

# exec log files
# creates two files
# *.elg log file
# *.exc exicode of program
%.elg : %.res
	make testexec 'FILE=$*'
	
allts : $(patsubst %.pp,%.res,$(wildcard ts*.pp))

alltest : $(patsubst %.pp,%.res,$(wildcard test*.pp))

alltf : $(patsubst %.pp,%.ref,$(wildcard tf*.pp))

allto : $(patsubst %.pp,%.res,$(wildcard to*.pp))

allexec: $(patsubst %.pp,%.elg,$(wildcard test*.pp)) $(patsubst %.pp,%.elg,$(wildcard ts*.pp)) 

clean :
	-rm *.re* *.o *.ppu ts*.exe tf*.exe log faillist

# $Log$
# Revision 1.6  1998-10-22 14:35:40  pierre
#   + added allexec tests if executables compiled
#     don't return with an error code
#   * some changes in test files for dos
#
# Revision 1.5  1998/10/21 16:24:16  pierre
#  + tests to check if filename exists
#
# Revision 1.4  1998/10/21 12:14:30  pierre
#  * stupid error removing getret.exe each time
#
# Revision 1.3  1998/10/21 12:12:09  pierre
# Log inserted
#

