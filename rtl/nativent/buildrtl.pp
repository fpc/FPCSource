unit buildrtl;

  interface

    uses
      ndk, ndkutils, ddk,
      ctypes, strings,
      matrix,
      rtlconsts, sysconst, sysutils, math, types,
      strutils, dateutils, varutils, variants, typinfo, fgl, classes,
      convutils, stdconvs,
{$ifdef cpui386}
      mmx, cpu,
{$endif}
{$ifdef cpux86_64}
      cpu,
{$endif}
      charset, cpall, ucomplex, getopts,
      fmtbcd;

  implementation

end.
