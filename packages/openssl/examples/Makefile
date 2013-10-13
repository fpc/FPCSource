PP=fpc
ARGS=-O2 -XX -Xs
DELP=delp

all:
	$(PP) $(ARGS) -Fu../src test1.pas
	$(PP) $(ARGS) -Fu../src genkeypair.lpr

clean:
	$(DELP) .