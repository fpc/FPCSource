SRCDIR = ..
FPC = fpc
FPCFLAGS = -Mdelphi -Sh -Cr -Co -FUlib -Fu$(SRCDIR) -Fu$(SRCDIR)/lib/x86_64-linux

DEMOS = demo_basics demo_attributes demo_classes demo_converters \
        demo_collections demo_dynamic demo_formatting demo_populate \
        regexapidemo

all: lib $(DEMOS)

lib:
	mkdir -p lib

regexapidemo: regexapidemo.pp lib
	$(FPC) $(FPCFLAGS) $<

demo_%: demo_%.pp lib
	$(FPC) $(FPCFLAGS) $<

run: all
	@echo ""; echo "============================="
	@echo "Running all demos..."; echo "============================="
	@for d in $(DEMOS); do \
		echo ""; echo ">>> $$d <<<"; echo ""; \
		./$$d; \
		echo ""; \
	done

clean:
	rm -f $(DEMOS) lib/*.o lib/*.ppu

.PHONY: all run clean
