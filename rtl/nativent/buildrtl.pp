unit buildrtl;

  interface

    uses
      ndk, ndkutils, ddk,
      ctypes, strings,
      rtlconsts, sysconst, sysutils, math, types,
      strutils, typinfo, fgl, classes,
{$ifdef cpui386}
      mmx, cpu,
{$endif}
{$ifdef cpux86_64}
      cpu,
{$endif}
      charset, cpall, getopts;

  implementation

end.
