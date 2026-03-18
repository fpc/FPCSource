program PromiseSimpleThreadExample;

{ 
  Example: TFPPromise (simple, parameterless resolve)

 Simulates async work using threads:
   Step 1 — sleeps 500ms in a background thread, then resolves
   Step 2 — synchronous step, resolves immediately

{$mode objfpc}
{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, FPPromise;

var
  Done: Boolean = False;

procedure Main;
begin
  WriteLn('=== TFPPromise (simple thread) example ===');
  WriteLn;

  TFPPromise.New(
    procedure(AResolve: TSimpleProc; AReject: TRejectProc; AComplete: TSimpleProc)
    begin
      WriteLn('[Step 1] Starting background work ...');
      TThread.CreateAnonymousThread(
        procedure
        begin
          Sleep(500);
          WriteLn('[Step 1] Background work done');
          AResolve();
        end
      ).Start;
    end
  )
  .ThenDo(
    procedure(AResolve: TSimpleProc; AReject: TRejectProc)
    begin
      WriteLn('[Step 2] Synchronous step');
      AResolve();
    end
  )
  .Catch(
    procedure(AError: string)
    begin
      WriteLn('[ERROR] ', AError);
    end
  )
  .FinallyDo(
    procedure
    begin
      WriteLn;
      WriteLn('[Done] Promise settled.');
      Done := True;
    end
  )
  .Run();
end;

begin
  Main;
  while not Done do
    Sleep(50);
end.
