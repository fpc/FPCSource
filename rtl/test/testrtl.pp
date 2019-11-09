{$h+}
program testrtl;

uses
{$IFDEF unix} cthreads, {$ENDIF}
  punit, sysutils, utsysutils,
  utstrtotime, utstrcmp, utastrcmp, utwstrcmp, utuplow, utunifile,
  utstrtobool, utscanf, utrwsync, utformat, utfloattostr, utfilename,
  utffirst, utfile, utfexpand,utexpfncase,utextractquote, utexec,
  utbytesof, utdirex, utencoding, utencodingerr, utsyshelpers,
  utstringhelp, utfattr, utenv,utdfexp,utfsearch, utverify,
  utstrcopy, utstrings1, utstringbuild, utustringbuild, uttypinfo, utclasses ;


begin
  SetTimeHook(@Now);
  if IsExecInvocation then
    Halt(Ord(Not TestExecInvocation))
  else
    RunAllSysTests;
end.

