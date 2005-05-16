// $Id: asiotest.pp,v 1.4 2005/02/14 17:13:18 peter Exp $

// AsyncIO test  by Sebastian Guenther, sg@freepascal.org
// This file is in the public domain

{$MODE objfpc}
program asiotest;
uses SysUtils, Classes, Crt, AsyncIO;

type

  TASIOTest = class
  protected
    FManager: TAsyncIOManager;
    Input: THandleStream;
    procedure InputAvailable(UserData: TObject);
    procedure Timeout(UserData: TObject);
  public
    constructor Create(AManager: TAsyncIOManager);
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
    Ord('q'): FManager.BreakRun;
    Ord('t'): FManager.ClearTimeoutHandler;
  end;
end;

procedure TASIOTest.Timeout(UserData: TObject);
begin
  WriteLn('Timeout');
end;

constructor TASIOTest.Create(AManager: TAsyncIOManager);
begin
  inherited Create;
  FManager := AManager;
  Input := THandleStream.Create(StdInputHandle);
  AManager.SetReadHandler(Input.Handle, @InputAvailable, nil);
  AManager.SetTimeoutHandler(@Timeout, nil);
  AManager.Timeout := 1000;
end;

destructor TASIOTest.Destroy;
begin
  Input.Free;
end;


var
  AsyncIOManager: TAsyncIOManager;
  app: TASIOTest;

begin
  WriteLn('Exit with "q", use "t" to stop the timeout handler');

  AsyncIOManager := TAsyncIOManager.Create;
  app := TASIOTest.Create(AsyncIOManager);

  AsyncIOManager.Run;

  app.Free;
  AsyncIOManager.Free;
end.


{
  $Log: asiotest.pp,v $
  Revision 1.4  2005/02/14 17:13:18  peter
    * truncate log

}
