unit buildrtl;

  interface

    uses
{$ifdef cpum68k}
      si_prc,
{$endif}
      athreads, dos, sysutils,

      ctypes, strings,
      rtlconsts, sysconst, math, types,
      typinfo, fgl, classes,
      charset, character, getopts;

  implementation

end.
