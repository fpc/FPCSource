program PromiseValueThreadExample;

{
   Example: TFPValuePromise<string> (value-carrying resolve)

   Simulates async work using threads, passing a string through the chain:
   Step 1 — produces "Hello" in a background thread
   Step 2 — receives "Hello", appends " World", resolves with "Hello World"
   Step 3 — receives "Hello World", formats final message
}

{$mode objfpc}
{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, FPPromise;

type
  TStringPromise = specialize TFPValuePromise<string>;

var
  Done: Boolean = False;

procedure Main;
begin
  WriteLn('=== TFPValuePromise<string> (thread) example ===');
  WriteLn;

  TStringPromise.New(
    // Step 1: produce a value in a background thread
    procedure(AResolve: TStringPromise.TResolveProc; AReject: TRejectProc; AComplete: TSimpleProc)
    begin
      WriteLn('[Step 1] Starting background work ...');
      TThread.CreateAnonymousThread(
        procedure
        begin
          Sleep(500);
          WriteLn('[Step 1] Resolving with "Hello"');
          AResolve('Hello');
        end
      ).Start;
    end
  )
  .ThenDo(
    // Step 2: receives "Hello", appends " World"
    procedure(AValue: string; AResolve: TStringPromise.TResolveProc; AReject: TRejectProc; AComplete: TSimpleProc)
    begin
      WriteLn('[Step 2] Received: "', AValue, '"');
      AResolve(AValue + ' World');
    end
  )
  .ThenDo(
    // Step 3: receives "Hello World", formats final output
    procedure(AValue: string; AResolve: TStringPromise.TResolveProc; AReject: TRejectProc; AComplete: TSimpleProc)
    begin
      WriteLn('[Step 3] Received: "', AValue, '"');
      AResolve('Final result: "' + AValue + '"');
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
