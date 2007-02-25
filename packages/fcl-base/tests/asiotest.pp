
// AsyncIO test  by Sebastian Guenther, sg@freepascal.org
// This file is in the public domain

{$ifndef unix}
  {$fatal This test is only for Unix platforms}
{$endif}

{$MODE objfpc}
program asiotest;
uses SysUtils, Classes, Crt, FPAsync;

type

  TASIOTest = class
  protected
    FManager: Teventloop;
    Input: THandleStream;
    MyTimer : Pointer;
    procedure InputAvailable(UserData: TObject);
    procedure TimerHandler(UserData: TObject);
  public
    constructor Create(AManager: Teventloop);
    destructor Destroy; override;
  end;



procedure TASIOTest.InputAvailable(UserData: TObject);
var
  b: Byte;
begin
  b := Input.ReadByte;
  Write('Input available: ');
  if b >= 32 then
    WriteLn('"', Chr(b), '"')
  else
    WriteLn('#', b);

  case b of
    Ord('q'): FManager.Break;
    Ord('t'): begin
                FManager.RemoveTimerNotify(MyTimer);
                writeln('Timer stopped');
              end;
  end;
end;

procedure TASIOTest.TimerHandler(UserData: TObject);
begin
  writeln('Timer');
end;

constructor TASIOTest.Create(AManager: Teventloop);
begin
  inherited Create;
  FManager := AManager;
  Input := THandleStream.Create(StdInputHandle);
  AManager.SetIONotify(Input.Handle, @InputAvailable, nil);
  MyTimer:=AManager.AddTimerNotify(1000,true,@TimerHandler,nil);
end;

destructor TASIOTest.Destroy;
begin
  Input.Free;
end;


var
  AsyncIOManager: Teventloop;
  app: TASIOTest;

begin
  WriteLn('Exit with "q", use "t" to stop the timer handler');

  AsyncIOManager := Teventloop.Create;
  app := TASIOTest.Create(AsyncIOManager);

  AsyncIOManager.Run;

  app.Free;
  AsyncIOManager.Free;
end.
