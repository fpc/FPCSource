This utility is used to re-create the complete
Free Pascal makefiles, depending on the rules
specified in the Makefile.fpc files.

Some tips:
- The rtl and packages contain a script that regenerates the makefiles. 
  targets, simply do (in a unix-like environment) 
  cd rtl
  ./regenmakefiles.sh
  cd ../packages
  ./regenmakefiles.sh

  To avoid svn conflicts for each locally changed Makefile
instead of using the current date for fpcmake executable,
revision.inc file is automatically updated to contain
the lastest date and revision of the files that do matter for
the Makefile generation.
  Each time any of these files "fpc*.pp fpcmake.ini Makefile.fpc"
is modified, revision.inc content is modified.
  This modified content should also be committed, to have correct
revision.inc content even on systems that do not have
svnversion binary or on exported trees.
PM 2013-04-26