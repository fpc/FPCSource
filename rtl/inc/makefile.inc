#
# Here we set some variables, needed by all OSes.
#
# System unit include files. These are composed from header and
# implementation files.

SYSNAMES=systemh heaph mathh filerec textrec system real2str sstrings innr \
         file typefile text rtti heap astrings objpas objpash except int64 \
         generic dynarr varianth variant

SYSINCNAMES=$(addsuffix .inc,$(SYSNAMES))

# Other unit names which can be used for all systems
#
UNITNAMES=os_types
#UNITNAMES=getops
#UNITPPNAMES=$(addsuffix .pp,$(UNITNAMES))

# Other files...
#astrings.pp
#complex.pp
#cpne.pp
#lstrings.pp
