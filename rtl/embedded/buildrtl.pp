{ This unit is only used to edit the rtl with lazarus }
unit buildrtl;

  interface

    uses
      system, unixtype, ctypes, baseunix, strings, objpas, macpas, syscall, unixutil, heapmgr,
      fpintres, heaptrc, lineinfo, lnfodwrf,
      termio, unix, linux, initc, cmem, mmx,
      crt, printer, linuxvcs,
      sysutils, typinfo, math, matrix, varutils,
      charset, ucomplex, getopts,
      errors, sockets, gpm, ipc, serial, terminfo, dl, dynlibs,
      video, mouse, keyboard, variants, types, dateutils, sysconst, fmtbcd,
      cthreads, classes, fgl, convutils, stdconvs, strutils, rtlconsts, dos, objects, cwstring, fpcylix, clocale,
      exeinfo,
{$ifdef CPUARM}
      stellaris
{$endif CPUARM}
{$ifdef CPUAVR}
      atmega128
{$endif CPUAVR}
      ;

  implementation

end.
