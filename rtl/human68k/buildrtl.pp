unit buildrtl;

  interface

    uses
{$ifdef cpum68k}
      si_prc,
{$endif}
      sysutils, dos,

      ctypes, strings,
      rtlconsts, sysconst, math, types,
      typinfo, sortbase, fgl, classes,
      charset, character, getopts,
      fpwidestring,
      softfpu, sfpux80, ufloatx80, sfpu128, ufloat128;

  implementation

end.
