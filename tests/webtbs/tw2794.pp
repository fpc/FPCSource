{ Source provided for Free Pascal Bug Report 2794 }
{ Submitted by "Johannes Berg" on  2003-11-17 }
{ e-mail: bugs@johannes.sipsolutions.de }
program test;
{$ifdef fpc}
  {$THREADING ON}
  {$MODE OBJFPC}
{$endif}

uses
  {$ifdef fpc}{$ifdef unix}CThreads,{$endif}{$endif}
  Classes;

type
  TMyThread = class(TThread)
    procedure Execute;override;
  end;

threadvar
  x: ansistring;

procedure TMyThread.Execute;
begin
  x := 'asdf';
  x := x + x;
  writeln(x);
end;

begin
  TMyThread.Create(false).Free;
end.
