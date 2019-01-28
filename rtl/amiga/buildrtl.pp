unit buildrtl;

  interface

    uses
      si_prc,
      athreads, dos, sysutils,

{$ifdef cpupowerpc}
      exeinfo, lineinfo,
{$endif}

      ctypes, strings,
      rtlconsts, sysconst, math, types,
      typinfo, fgl, classes,
      charset, character, getopts;

  implementation

end.
