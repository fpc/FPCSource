#
# Makefile to make the internationalization files for the resstr example
# Add a 2 letter code when adding a language to DEMOLANGUAGES
#

DEMOLANGUAGES=fr nl de pb

#
# No need to add after this line.
#

OBJECTS=$(addprefix restest.,$(DEMOLANGUAGES))
MOOBJECTS=$(addsuffix .mo,$(OBJECTS))

.SUFFIXES: .mo .po

%.mo: %.po
	msgfmt -o $@ $?

all: $(MOOBJECTS)
