{ %TARGET=win32 }

{ When an operating system exception that has an exception object reaches the
  default handler with no ExceptProc installed, ErrorBase is the frame of the
  routine that faulted, as it is for one without an object. It used to be read
  from the exception record's fifth parameter, which only an exception raised by
  the run time library fills in, so the runtime error's backtrace stopped after
  its first line. The exit procedure checks ErrorBase and ends the program with
  exit code 0 when it is right. }
program tb0736;

{$mode objfpc}

uses
  sysutils;

var
  ErrBase: pointer; external name 'FPC_ERRORBASE';
  faultframe: pointer;
  oldexit: codepointer;

procedure check;
begin
  ExitProc:=oldexit;
  if ErrBase=faultframe then
    begin
      ErrorAddr:=nil;
      ExitCode:=0;
    end
  else
    begin
      writeln('ErrorBase $',hexstr(ErrBase),', the faulting frame $',hexstr(faultframe));
      ExitCode:=1;
    end;
end;

procedure fault;
var
  p: pinteger;
begin
  faultframe:=get_frame;
  p:=nil;
  p^:=1;
end;

begin
  oldexit:=ExitProc;
  ExitProc:=@check;
  ExceptProc:=nil;
  fault;
  writeln('the access violation was not reported');
  halt(2);
end.
