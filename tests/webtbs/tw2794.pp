{ Source provided for Free Pascal Bug Report 2794 }
{ Submitted by "Johannes Berg" on  2003-11-17 }
{ e-mail: bugs@johannes.sipsolutions.de }
program test;
{$THREADING ON}
{$MODE OBJFPC}

uses
  CThreads,Classes;

type
  TMyThread = class(TThread)
    procedure Execute;
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
