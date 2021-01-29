unit buildrtl;

  interface

    uses
      si_prc,
      athreads, dos, sysutils,

      ctypes, strings,
      rtlconsts, sysconst, math, types,

      exeinfo,
{$ifdef cpui386}
      lineinfo,
{$endif}

      typinfo, sortbase, fgl, classes,
      charset, character, getopts,
      fpwidestring, fpintres,
      softfpu, sfpux80, ufloatx80, sfpu128, ufloat128;

  implementation

end.
