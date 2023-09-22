unit buildrtl;

  interface
{$IFDEF FPC_DOTTEDUNITS}
    uses
      si_prc,
      AmigaApi.AThreads, TP.DOS, System.SysUtils,

      System.ExeInfo,

      System.CTypes, System.Strings,
      System.RtlConsts, System.SysConst, System.Math, System.Types,

{$ifdef cpui386}
      System.CPU, System.CPU.MMX, System.LineInfo,
{$endif}
{$ifdef cpux86_64}
      System.CPU,
{$endif}

      System.TypInfo, System.SortBase, System.FGL, System.Classes,
      System.CharSet, System.Character, System.GetOpts,
      System.FPWideString, fpintres, System.CodePages.All,
      System.SoftFPU, System.SoftFpuX80, System.SoftFpu128, System.UFloatX80, System.UFloat128;
{$ELSE FPC_DOTTEDUNITS}
    uses
      si_prc,
      athreads, dos, sysutils,

      ctypes, strings,
      rtlconsts, sysconst, math, types,

      exeinfo,
{$ifdef cpui386}
      cpu, mmx, lineinfo,
{$endif}
{$ifdef cpux86_64}
      cpu,
{$endif}

      typinfo, sortbase, fgl, classes,
      charset, character, getopts,
      fpwidestring, fpintres,
      softfpu, sfpux80, ufloatx80, sfpu128, ufloat128;
{$ENDIF FPC_DOTTEDUNITS}
  implementation

end.
