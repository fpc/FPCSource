library timerdemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, wasm.timer.shared, wasm.timer.api, wasm.logger.api, wasm.timer.objects, wasm.http.api;

Type

  { TTestTimer }

  TTestTimer = class(TObject)
     FTimer1 : TWasmTimer;
     FTimer2 : TTimer;
     Fcount : Integer;
     Procedure Run;
  private
    procedure DoTimerTick(Sender: TObject);
  end;

{ TTestTimer }

procedure TTestTimer.Run;
begin
  Writeln('Creating timers');
  FTimer1:=TWasmTimer.Create(1000,@DotimerTick,Self);
  FTimer2:=TTimer.Create(Nil);
  FTimer2.Interval:=3000;
  FTimer2.OnTimer:=@DoTimerTick;
  FTimer2.Enabled:=True;
end;

procedure TTestTimer.DoTimerTick(Sender: TObject);
begin
  Inc(FCount);
  Writeln('Timer tick ',FCount,':  sender: ',Sender.ClassName);
  if FCount>=33 then
    begin
    Writeln('Stopping timers');
    FreeAndNil(FTimer1);
    FreeAndNil(FTimer2);
    end;
end;

begin
  With TTestTimer.Create do
    Run;
end.

