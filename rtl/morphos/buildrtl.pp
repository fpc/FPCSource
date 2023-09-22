unit buildrtl;

  interface
{$IFDEF FPC_DOTTEDUNITS}
    uses
      si_prc,
      AmigaApi.AThreads, TP.DOS, System.SysUtils,

      System.ExeInfo, System.LineInfo,

      System.CTypes, System.Strings,
      System.RtlConsts, System.SysConst, System.Math, System.Types,
      System.TypInfo, System.SortBase, System.FGL, System.Classes,
      System.CharSet, System.Character, System.GetOpts,
      System.FPWideString, fpintres, System.CodePages.All,
      System.SoftFPU, System.SoftFpuX80, System.SoftFpu128, System.UFloatX80, System.UFloat128;
{$ELSE FPC_DOTTEDUNITS}
    uses
      si_prc,
      athreads, dos, sysutils,

      exeinfo, lineinfo,

      ctypes, strings,
      rtlconsts, sysconst, math, types,
      typinfo, sortbase, fgl, classes,
      charset, character, getopts,
      fpwidestring, fpintres,
      softfpu, sfpux80, ufloatx80, sfpu128, ufloat128;
{$ENDIF FPC_DOTTEDUNITS}
  implementation

end.
