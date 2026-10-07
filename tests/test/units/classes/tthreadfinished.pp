{ TThread.Finished becomes True only after DoTerminate has run, issue #31267 }
program tthreadfinished;

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  cthreads,
{$endif}
  SysUtils, Classes;

type
  TFinishThread = class(TThread)
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    // True once DoTerminate has completed
    TerminateDone: Boolean;
  end;

procedure TFinishThread.Execute;

begin
end;


procedure TFinishThread.DoTerminate;

begin
  Sleep(200);
  inherited DoTerminate;
  TerminateDone:=True;
  WriteBarrier;
end;


var
  lThread: TFinishThread;
  lWaited: Integer;

begin
  lThread:=TFinishThread.Create(False);
  lWaited:=0;
  while not lThread.Finished do
    begin
    Sleep(1);
    Inc(lWaited);
    if lWaited>10000 then
      halt(1);
    end;
  ReadBarrier;
  if not lThread.TerminateDone then
    halt(2);
  lThread.Free;
  writeln('ok');
end.
