program thandlesconnect;

{ Tests TSocketHandler.HandlesConnect: a handler which establishes the
  connection itself. Nothing may listen on 127.0.0.1:1. }

{$mode objfpc}{$H+}

uses
{$IFDEF TEST_DOTTEDUNITS}
  System.SysUtils, System.Net.Ssockets;
{$ELSE}
  SysUtils, ssockets;
{$ENDIF}

type
  TOwnConnectHandler = class(TSocketHandler)
  public
    Calls : Integer;
    Succeed : Boolean;
    ConnectHost : String;
    ConnectPort : Word;
    function HandlesConnect: Boolean; override;
    function Connect: Boolean; override;
  end;

function TOwnConnectHandler.HandlesConnect: Boolean;
begin
  Result:=True;
end;

function TOwnConnectHandler.Connect: Boolean;
begin
  Inc(Calls);
  ConnectHost:=(Socket as TInetSocket).NetworkAddress.Address;
  ConnectPort:=(Socket as TInetSocket).Port;
  Result:=Succeed;
end;

procedure Check(Condition: Boolean; const Msg: String);
begin
  if not Condition then
    begin
    Writeln('FAIL: ',Msg);
    Halt(1);
    end;
end;

function ConnectFails(S: TInetSocket): Boolean;
begin
  Result:=False;
  try
    S.Connect;
  except
    on E: ESocketError do
      Result:=E.Code=seConnectFailed;
  end;
end;

procedure TestHandlerConnects;
var
  H : TOwnConnectHandler;
  S : TInetSocket;
begin
  H:=TOwnConnectHandler.Create;
  H.Succeed:=True;
  S:=TInetSocket.Create('127.0.0.1',1,H);
  try
    // Only succeeds if the socket handle itself is not connected.
    S.Connect;
    Check(H.Calls=1,'Connect called '+IntToStr(H.Calls)+' times');
    Check(H.ConnectHost='127.0.0.1','Host '+H.ConnectHost);
    Check(H.ConnectPort=1,'Port '+IntToStr(H.ConnectPort));
  finally
    S.Free;
  end;
end;

procedure TestHandlerFails;
var
  H : TOwnConnectHandler;
  S : TInetSocket;
begin
  H:=TOwnConnectHandler.Create;
  H.Succeed:=False;
  S:=TInetSocket.Create('127.0.0.1',1,H);
  try
    Check(ConnectFails(S),'Failing handler: expected seConnectFailed');
    Check(H.Calls=1,'Connect called '+IntToStr(H.Calls)+' times');
  finally
    S.Free;
  end;
end;

procedure TestDefaultHandler;
var
  S : TInetSocket;
begin
  // A nil handler would make the constructor connect immediately.
  S:=TInetSocket.Create('127.0.0.1',1,TSocketHandler.Create);
  try
    Check(ConnectFails(S),'Default handler: expected seConnectFailed');
  finally
    S.Free;
  end;
end;

begin
  TestHandlerConnects;
  TestHandlerFails;
  TestDefaultHandler;
  Writeln('PASS');
end.
