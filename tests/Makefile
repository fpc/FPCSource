#
# $Id$
#
# make all test
# and printout errors

all : info

tests : clean all_compilations

cont_tests : all_compilations

ifdef DJGPP

EXEEXT=.exe

getreturncode : 
	redir -ea $(FILE).log -oa $(FILE).log getret $(COMMAND)
	cp retcode $(FILE).$(RESEXT)

else

EXEEXT=
getreturncode : 
	getret $(COMMAND) > $(FILE).log 2>$(FILE).log
	cp retcode $(FILE).$(RESEXT)
	@echo "Return code of $(FILE) is $(cat retcode)"
endif


# retcode should be between 0 and 255
# 256 is for halt
# 512+doserror if doserror<>0
# 1024 RESFILE does not exist
# 2048 RESFILE is not set
ifndef RESFILE
RETVAL=2048
else
ifeq ($(wildcard $(RESFILE)*),$(RESFILE))
RETVAL=$(shell cat $(RESFILE))
else
RETVAL=1024
endif
endif

ifeq ($(RETVAL),0)
testsuccess:
	@echo "Test for $(FILE) success (compiles)"
	@echo "Test for $(FILE) success (compiles)" >>log
else
testsuccess:
	@echo "Test for $(FILE) fails (does not compile) error $(RETVAL)"
	@echo "Test for $(FILE) fails (does not compile) error $(RETVAL)" >>log
	@echo $(FILE) >> ts_fail
	@echo $(FILE) >> faillist
endif
	
ifdef EXCFILE
ifeq ($(wildcard $(EXCFILE)*),$(EXCFILE))
EXERETVAL:=$(shell cat $(EXCFILE))
else
EXERETVAL=$(EXCFILE) does not exist
endif
else
EXERETVAL=No EXCFILE variable defined
endif

ifeq ($(EXERETVAL),0)
testexecsuccess:
	@echo "Test for exec $(FILE) success (runs without error)"
	@echo "Test for $(FILE) success (runs without error)" >>log
else
testexecsuccess:
	@echo "Test for exec $(FILE) fails exec error $(EXERETVAL)"
	@echo "Test for exec $(FILE) fails exec error $(EXERETVAL)" >>log
endif
	
ifeq ($(wildcard $(FILE)$(EXEEXT)),$(FILE)$(EXEEXT))
testexec:
	@echo "Testing $(FILE)$(EXEEXT)"
ifdef NOREDIR
	getret $(FILE)$(EXEEXT)
else
ifdef DJGPP
	redir -e $(FILE).elg -o $(FILE).elg getret $(FILE)$(EXEEXT)
else
	getret $(FILE)$(EXEEXT) > $(FILE).elg 2>$(FILE).elg
endif
endif
	cp -f retcode $(FILE).exc
	$(MAKE) testexecsuccess 'FILE=$(FILE)' 'EXCFILE=$(FILE).exc'
else
testexec:
	@echo "No exefile $(FILE)$(EXEEXT)"
	@echo $(FILE) >> faillist
endif

test_exc :
	@echo $(wildcard $(FILE).exc*)
	@echo xx$(wildcard $(EXCFILE)*)xx xx$(EXCFILE)xx
	cat $(FILE).exc
	
ifneq ($(RETVAL),0)
testfail:
	@echo "Test for $(FILE) success (does not compile) error $(RETVAL)"
	@echo "Test for $(FILE) success (does not compile) error $(RETVAL)" >> log
else
testfail:
	@echo "Test for $(FILE) fails (does compile and should not)"
	@echo "Test for $(FILE) fails (does compile and should not)" >> log
	@echo $(FILE) >> tf_fail
	@echo $(FILE) >> faillist
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
	$(MAKE) getreturncode 'COMMAND=$(PP) $(OPT) $(FILE).pp' 'RESEXT=$(RESEXT)' 'FILE=$(FILE)'

%.res : %.pp
	$(MAKE) testone 'FILE=$*' 'RESEXT=res'
	$(MAKE) testsuccess 'FILE=$*' 'RESFILE=$*.res'

%.ref : %.pp
	$(MAKE) testone 'FILE=$*' 'RESEXT=ref'
	$(MAKE) testfail 'FILE=$*' 'RESFILE=$*.ref'

# exec log files
# creates two files
# *.elg log file
# *.exc exicode of program
%.elg : %.res
	$(MAKE) testexec 'FILE=$*'

%.eli : %.res
	$(MAKE) testexec 'FILE=$*' 'NOREDIR=YES'

allts : $(patsubst %.pp,%.res,$(wildcard ts*.pp))

alltbs : $(patsubst %.pp,%.res,$(wildcard tbs*.pp))

alltest : $(patsubst %.pp,%.res,$(wildcard test*.pp))

alltesi : $(patsubst %.pp,%.res,$(wildcard tesi*.pp))

alltis : $(patsubst %.pp,%.res,$(wildcard tis*.pp))

alltf : $(patsubst %.pp,%.ref,$(wildcard tf*.pp))

alltbf : $(patsubst %.pp,%.ref,$(wildcard tbf*.pp))

allto : $(patsubst %.pp,%.res,$(wildcard to*.pp))

ifndef TS_FAIL_LIST
ifeq ($(wildcard ts_fail*),ts_fail)
TS_FAIL_LIST=$(shell cat ts_fail)
export TS_FAIL_LIST
endif
endif

ifndef TF_FAIL_LIST
ifeq ($(wildcard tf_fail*),tf_fail)
TF_FAIL_LIST=$(shell cat tf_fail)
export TF_FAIL_LIST
endif
endif

clean_fail :
	-rm -f $(addsuffix .res,$(TS_FAIL_LIST)) 
	-rm -f $(addsuffix .ref,$(TF_FAIL_LIST)) 
	-rm log 

again : clean_fail $(addsuffix .res,$(TS_FAIL_LIST)) $(addsuffix .ref,$(TF_FAIL_LIST)) 
	grep fails log

all_compilations : allts alltbs alltf alltbf allto alltest alltesi alltis
	grep fails log

allexec : alltsexec alltbsexec alltestexec
	grep "fails exec" log

alltestexec: $(patsubst %.pp,%.elg,$(wildcard test*.pp)) 

# these test are interactive
# no redirection !!!
alltesiexec: $(patsubst %.pp,%.eli,$(wildcard test*.pp)) 

alltsexec: $(patsubst %.pp,%.elg,$(wildcard ts*.pp)) 

alltbsexec: $(patsubst %.pp,%.elg,$(wildcard tbs*.pp)) 

alltisexec: $(patsubst %.pp,%.eli,$(wildcard tis*.pp)) 

clean :
	-rm -f *.re* *.o *.ppu *.log *.elg *.exc t*.exe log faillist ts_fail tf_fail
	-rm -f ppas.sh ppas.bat retcode
ifndef DJGPP
	-rm -f $(patsubst %.pp,%,$(wildcard *.pp))
endif

info :
	@echo This Makefile allows to test the compiler
	@echo compilation of 'ts*.pp' should succeed
	@echo compilation of 'tf*.pp' should fail
	@echo compilation of 'test*.pp' should succeed
	@echo 'to*.pp' files should also compile
	@echo simply run \'make tests\' to test all compilation
	@echo run \'make allexec\' to test also if the executables
	@echo created behave like the should
	@echo run \'make tesiexec\' to test executables 
	@echo that require interactive mode

#
# $Log$
# Revision 1.6  1999-10-13 12:42:09  pierre
#  * small fixes for win32
#
# Revision 1.5  1999/09/19 11:48:18  peter
#   * remove ppas and retcode
#
# Revision 1.4  1999/09/19 11:23:06  peter
#   * clean also the executables under linux
#
# Revision 1.3  1999/06/01 13:27:27  peter
#   * updates for linux
#
# Revision 1.2  1999/06/01 00:06:14  peter
#   * linux fixes
#
# Revision 1.1  1999/01/25 20:23:04  peter
#   * linux updates
#
# Revision 1.12  1999/01/19 18:01:43  pierre
#  local change removed
#
# Revision 1.11  1999/01/19 17:34:01  pierre
#  several modifications
#
# Revision 1.10  1999/01/15 17:41:58  pierre
#  + new bugs converted
#
# Revision 1.9  1998/11/10 11:13:07  pierre
#  * more tests
#
# Revision 1.8  1998/10/28 09:52:26  pierre
#  * see readme.txt
#
# Revision 1.7  1998/10/22 16:41:11  pierre
#   * added two small tests
#     iocheck inside iocheck
#     enums inside objects
#
# Revision 1.6  1998/10/22 14:35:40  pierre
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
