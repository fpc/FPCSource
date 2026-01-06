unit buildrtl;

  interface

    uses
      ndk, ndkutils, ddk,
      ctypes, strings,
      dos,
      rtlconsts, sysconst, sysutils, math, types,
      typinfo, fgl, classes,
{$ifdef cpui386}
      mmx, cpu, intrinsics,
{$endif}
{$ifdef cpux86_64}
      cpu, intrinsics,
{$endif}
      charset, cpall, getopts,
      character, fpwidestring, unicodedata,
      softfpu, sfpux80, ufloatx80, sfpu128, ufloat128;

  implementation

end.
