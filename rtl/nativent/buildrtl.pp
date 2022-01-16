unit buildrtl;

  interface

    uses
      ndk, ndkutils, ddk,
      ctypes, strings,
      dos,
      rtlconsts, sysconst, sysutils, math, types,
      typinfo, fgl, classes,
{$ifdef cpui386}
      mmx, cpu,
{$endif}
{$ifdef cpux86_64}
      cpu,
{$endif}
      charset, cpall, getopts,
      character, fpwidestring, unicodedata, unicodenumtable,
      softfpu, sfpux80, ufloatx80, sfpu128, ufloat128;

  implementation

end.
