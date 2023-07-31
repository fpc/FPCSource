unit buildrtl;

  interface

{$IFDEF FPC_DOTTEDUNITS}
    uses
      sysinitpas, sysinitcyg, sysinitgprof,
      extpas,
      System.CTypes, System.Strings,
      WinApi.Windows,  initc,  System.CMem, System.DynLibs, signals,
      TP.DOS, WinApi.Messages,
      System.RtlConsts, System.SysConst, System.SysUtils, System.Math, System.Types,
      System.TypInfo, System.FGL, System.Classes,    
      System.Cpu, mmx, System.CharSet, System.Character, System.GetOpts,
      System.fpwidestring,
      System.Winsysut,  WinApi.ShareMem, fpintres, WinApi.WinDirs,
      System.SoftFPU, System.SoftFpuX80, System.SoftFpu128, System.UFloatX80, System.UFloat128;
{$ELSE FPC_DOTTEDUNITS}
  uses
      sysinitpas, sysinitcyg, sysinitgprof,
      extpas,
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
