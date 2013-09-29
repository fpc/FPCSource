{ %norun }
{ %needlibrary }
{ %target=darwin,linux,freebsd,solaris,beos,haiku,aix,android }

{$mode delphi}

{$ifdef darwin}
{$PIC+}
{$endif darwin}

{$ifdef CPUX86_64}
{$ifndef WINDOWS}
{$PIC+}
{$endif WINDOWS}
{$endif CPUX86_64}

library tw12704a;

uses
  SysUtils;

procedure initsignals;
var
  p: pointer;
  i: longint;
begin
  // check that none of the handlers have been yet by the library's init code
  for i:=RTL_SIGINT to RTL_SIGLAST do
    if (InquireSignal(i) <> ssNotHooked) then
      halt(1);

  // hook standard signals
  HookSignal(RTL_SIGDEFAULT);
  for i:=RTL_SIGINT to RTL_SIGLAST do
    case i of
      RTL_SIGINT,
      RTL_SIGQUIT:
        if (InquireSignal(i) <> ssNotHooked) then
          halt(2);
      RTL_SIGFPE,
      RTL_SIGSEGV,
      RTL_SIGILL,
      RTL_SIGBUS:
        if (InquireSignal(i) <> ssHooked) then
          halt(3);
      else
        halt(4);
    end;

  // unhook sigill
  UnHookSignal(RTL_SIGILL);
  for i:=RTL_SIGINT to RTL_SIGLAST do
    case i of
      RTL_SIGINT,
      RTL_SIGILL,
      RTL_SIGQUIT:
        if (InquireSignal(i) <> ssNotHooked) then
          halt(5);
      RTL_SIGFPE,
      RTL_SIGSEGV,
      RTL_SIGBUS:
        if (InquireSignal(i) <> ssHooked) then
          halt(6);
    end;

  // check whether installed signal handler actually works
(*
  try
    p:=nil;
    longint(p^):=1;
  except
  end;
*)
end;


procedure testsignals; cdecl;
var
  i: longint;
begin
  // called from program -> it has overridden our signal handlers
  // when this routine is called, it will have unhooked sigbus, so
  // that one should still belong to us
  // we previously unhooked sigill, so that one should still be
  // unhooked as far as we are concerned
  for i:=RTL_SIGINT to RTL_SIGLAST do
    case i of
      RTL_SIGINT,
      RTL_SIGILL,
      RTL_SIGQUIT:
        if (InquireSignal(i) <> ssNothooked) then
          halt(7);
      RTL_SIGFPE,
      RTL_SIGSEGV:
        if (InquireSignal(i) <> ssOverridden) then
          halt(8);
      RTL_SIGBUS:
        if (InquireSignal(i) <> ssHooked) then
          halt(9);
    end;
end;

exports
  testsignals;

begin
  initsignals;
end.
