unit buildrtl;

  interface

    uses
      si_prc,
      sysutils,

      ctypes, strings,
      rtlconsts, sysconst, math, types,
      typinfo, sortbase, fgl, classes,
      charset, character, getopts,
      fpwidestring
{$IFNDEF FPUNONE}
      ,softfpu, sfpux80, ufloatx80, sfpu128, ufloat128
{$ENDIF}
      ;

  implementation

end.
