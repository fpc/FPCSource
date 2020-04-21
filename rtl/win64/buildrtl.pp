unit buildrtl;

  interface

    uses
      sysinit,
      extpas,
      ctypes, strings,
      heaptrc,
      windows,  cmem, dynlibs,
      dos, messages,
      rtlconsts, sysconst, sysutils, math, types,
      typinfo, fgl, classes,
      charset, character, getopts,
      fpwidestring,
      sharemem, exeinfo, fpintres, windirs,
      {$ifndef CPUAARCH64}cpu, signals,{$endif} sfpux80;

  implementation

end.
