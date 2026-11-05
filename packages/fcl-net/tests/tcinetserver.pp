{ Tests TInetServer construction failures and the handler passed to the string constructor. }
unit tcinetserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, ssockets;

type

  { TTestInetServerCreate }

  TTestInetServerCreate = class(TTestCase)
  private
    procedure CheckCreateFails(const aHost: string);
  published
    procedure TestCreateUnknownHost;
    procedure TestCreateEmptyHost;
    procedure TestCreateFailureKeepsStdIn;
    procedure TestCreatePassesHandler;
  end;

implementation

{$IFDEF UNIX}
uses
  BaseUnix;
{$ENDIF}

type
  TInetServerAccess = class(TInetServer);


// True if file descriptor 0 is open
function StdInOpen: Boolean;

{$IFDEF UNIX}
var
  lStat: Stat;
{$ENDIF}

begin
{$IFDEF UNIX}
  Result:=fpFStat(0, lStat)=0;
{$ELSE}
  Result:=True;
{$ENDIF}
end;


// Open /dev/null as file descriptor 0 when it is closed
procedure EnsureStdInOpen;

begin
{$IFDEF UNIX}
  if not StdInOpen then
    fpOpen('/dev/null', O_RDONLY);
{$ENDIF}
end;


{ TTestInetServerCreate }

procedure TTestInetServerCreate.CheckCreateFails(const aHost: string);

var
  lClass: string;

begin
  lClass:='';
  try
    TInetServer.Create(aHost, 0).Free;
  except
    on E: Exception do
      lClass:=E.ClassName;
  end;
  AssertEquals('Create('''+aHost+''') raises only EUnsupportedAddress', 'EUnsupportedAddress', lClass);
end;


procedure TTestInetServerCreate.TestCreateUnknownHost;

begin
  CheckCreateFails('foo');
end;


procedure TTestInetServerCreate.TestCreateEmptyHost;

begin
  CheckCreateFails('');
end;


procedure TTestInetServerCreate.TestCreateFailureKeepsStdIn;

begin
  EnsureStdInOpen;
  AssertTrue('file descriptor 0 is open before Create', StdInOpen);
  CheckCreateFails('foo');
  AssertTrue('a failed Create leaves file descriptor 0 open', StdInOpen);
end;


procedure TTestInetServerCreate.TestCreatePassesHandler;

var
  lHandler: TSocketHandler;
  lServer: TInetServer;

begin
  lHandler:=TSocketHandler.Create;
  lServer:=TInetServer.Create('127.0.0.1', 0, lHandler);
  try
    AssertSame('the string constructor uses the given handler', lHandler, TInetServerAccess(lServer).Handler);
  finally
    lServer.Free;
  end;
end;


initialization
  RegisterTest(TTestInetServerCreate);
end.
