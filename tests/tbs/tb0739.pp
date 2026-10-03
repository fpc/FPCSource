{ %TARGET=win32 }

{ The companion of tb0736: an exception raised by the program, reaching the
  default handler with no ExceptProc installed, still gets the frame it was
  raised from as ErrorBase -- the frame fpc_RaiseException passes in the
  exception record. The exit procedure checks ErrorBase and ends the program
  with exit code 0 when it is right. }
program tb0739;

{$mode objfpc}

uses
  sysutils;

var
  ErrBase: pointer; external name 'FPC_ERRORBASE';
  raiseframe: pointer;
  oldexit: codepointer;

procedure check;
begin
  ExitProc:=oldexit;
  if (ExitCode=217) and (ErrBase=raiseframe) then
    begin
      ErrorAddr:=nil;
      ExitCode:=0;
    end
  else
    begin
      writeln('exit code ',ExitCode,', ErrorBase $',hexstr(ErrBase),', the raising frame $',hexstr(raiseframe));
      ExitCode:=1;
    end;
end;

procedure doraise;
begin
  raiseframe:=get_frame;
  raise Exception.Create('not handled');
end;

begin
  oldexit:=ExitProc;
  ExitProc:=@check;
  ExceptProc:=nil;
  doraise;
  writeln('the exception was not reported');
  halt(2);
end.
