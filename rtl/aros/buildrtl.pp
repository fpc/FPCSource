unit buildrtl;

  interface

    uses
      si_prc,
      athreads, dos, sysutils,
      softfpu, sfpux80, ufloatx80, sfpu128, ufloat128,
      ctypes, strings,
      rtlconsts, sysconst, math, types,
      exeinfo,
{$ifdef cpui386}
      cpu, mmx, lineinfo,
{$endif}
{$ifdef cpux86_64}
      cpu,
{$endif}
      typinfo, fgl, classes,
      charset, character, getopts,
      fpintres;

  implementation

end.
