unit buildrtl;

  interface

{$IFDEF FPC_DOTTEDUNITS}
    uses
      sysinitpas, sysinitcyg, sysinitgprof,
      extpas, heaptrc,
      System.CTypes, System.Strings,
      WinApi.Windows,  initc,  System.CMem, System.DynLibs, WinApi.Signals,
      TP.DOS, WinApi.Messages,
      System.RtlConsts, System.SysConst, System.SysUtils, System.Math, System.Types,
      System.TypInfo, System.FGL, System.Classes,    
      System.CPU, System.CPU.MMX, System.CharSet, System.Character, System.GetOpts,
      System.FPWideString,
      WinApi.WinSysUt, WinApi.ShareMem, fpintres, WinApi.WinDirs,
      System.SoftFPU, System.SoftFpuX80, System.SoftFpu128, System.UFloatX80, System.UFloat128;
{$ELSE FPC_DOTTEDUNITS}
  uses
      sysinitpas, sysinitcyg, sysinitgprof,
      extpas, heaptrc,
      ctypes, strings,
      windows,  initc, cmem, dynlibs, signals,
      dos, messages,
      rtlconsts, sysconst, sysutils, math, types,
      typinfo, fgl, classes,
      cpu, mmx, charset, character, getopts,
      fpwidestring,
      winsysut, sharemem, fpintres, windirs,
      softfpu, sfpux80, sfpu128, ufloatx80, ufloat128;
{$ENDIF FPC_DOTTEDUNITS}
  implementation

end.
