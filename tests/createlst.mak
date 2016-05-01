FILES=$(sort $(wildcard $(addsuffix /t*.pp,$(TESTDIRS))))
$(foreach filename,$(FILES),$(info $(filename)))

all: ;
