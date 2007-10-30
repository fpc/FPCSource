This utility is used to re-create the complete
Free Pascal makefiles, depending on the rules
specified in the Makefile.fpc files.

Some tips:
- To regenerate the makefiles for all supported
  targets, simply do : fpcmake -Tall
- To regenerate the complete makefile tree
  for all targets (under UNIX): 
    fpcmake -Tall -w `find . -name Makefile.fpc`
