#
# Here we set some variables, needed by all OSes.
#

# System unit include files. These are composed from header and
# implementation files.

SYSNAMES=systemh heaph mathh filerec textrec system real2str sstrings innr file typefile version
SYSINCNAMES=$(addsuffix .inc,$(SYSNAMES))

# Other files...
#astrings.pp
#complex.pp
#cpne.pp
#lstrings.pp
