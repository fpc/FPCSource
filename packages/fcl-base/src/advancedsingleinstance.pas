{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2015 by Ondrej Pokorny

    Unit implementing Single Instance functionality.

    The order of message processing is not deterministic (if there are more
    pending messages, the server won't process them in the order they have
    been sent to the server.
    SendRequest and PostRequest+PeekResponse sequences from 1 client are
    blocking and processed in correct order.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit AdvancedSingleInstance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AdvancedIPC, singleinstance;

type

  TSingleInstanceReceivedCustomMessage = procedure(Sender: TBaseSingleInstance; MsgID: Integer; MsgType: Integer; MsgData: TStream) of object;

  TAdvancedSingleInstance = class(TBaseSingleInstance)
  private
    FGlobal: Boolean;
    FID: string;
    FServer: TIPCServer;
    FClient: TIPCClient;
    FOnServerReceivedCustomRequest: TSingleInstanceReceivedCustomMessage;
    procedure SetGlobal(const aGlobal: Boolean);
    procedure SetID(const aID: string);
  protected
    procedure DoServerReceivedCustomRequest(const aMsgID: Integer; const aMsgType: Integer; const aStream: TStream);
    function GetIsClient: Boolean; override;
    function GetIsServer: Boolean; override;
    function GetStartResult: TSingleInstanceStart; override;
  public
    constructor Create(aOwner: TComponent); override;
  public
    function Start: TSingleInstanceStart; override;
    procedure Stop; override;
    procedure ServerCheckMessages; override;
    procedure ClientPostParams; override;
  public
    function ClientPostCustomRequest(const aMsgType: Integer; const aStream: TStream): Integer;
    function ClientSendCustomRequest(const aMsgType: Integer; const aStream: TStream): Boolean; overload;
    function ClientSendCustomRequest(const aMsgType: Integer; const aStream: TStream; out outRequestID: Integer): Boolean; overload;
    procedure ServerPostCustomResponse(const aRequestID: Integer; const aMsgType: Integer; const aStream: TStream);
    function ClientPeekCustomResponse(const aStream: TStream; out outMsgType: Integer): Boolean;
  public
    property ID: string read FID write SetID;
    property Global: Boolean read FGlobal write SetGlobal;

    property OnServerReceivedCustomRequest: TSingleInstanceReceivedCustomMessage read FOnServerReceivedCustomRequest write FOnServerReceivedCustomRequest;
  end;

implementation

Resourcestring
  SErrSetSingleInstanceIDStarted = 'You cannot change the single instance ID when it''s been started.';
  SErrSetSingleInstanceGlobalStarted = 'You cannot change the single instance global property when it''s been started.';
  SErrStartSingleInstanceStarted = 'You cannot start single instance when it''s been already started.';
  SErrSingleInstanceStartResultNotAvailable = 'Single instance hasn''t been started yet.';
  SErrSingleInstanceNotClient = 'Current instance is not a client.';
  SErrSingleInstanceNotServer = 'Current instance is not a server.';

Const
  MSGTYPE_CHECK = -1;
  MSGTYPE_CHECKRESPONSE = -2;
  MSGTYPE_PARAMS = -3;
  MSGTYPE_WAITFORINSTANCES = -4;

{ TAdvancedSingleInstance }

constructor TAdvancedSingleInstance.Create(aOwner: TComponent);
var
  xID: RawByteString;
  I: Integer;
begin
  inherited Create(aOwner);

  xID := 'SI_'+ExtractFileName(ParamStr(0));
  for I := 1 to Length(xID) do
    case xID[I] of
      'a'..'z', 'A'..'Z', '0'..'9', '_': begin end;
    else
      xID[I] := '_';
    end;
  ID := xID;
end;

function TAdvancedSingleInstance.ClientPeekCustomResponse(
  const aStream: TStream; out outMsgType: Integer): Boolean;
begin
  if not Assigned(FClient) then
    raise ESingleInstance.Create(SErrSingleInstanceNotClient);

  Result := FClient.PeekResponse(aStream, outMsgType, TimeOutMessages);
end;

function TAdvancedSingleInstance.ClientPostCustomRequest(
  const aMsgType: Integer; const aStream: TStream): Integer;
begin
  if not Assigned(FClient) then
    raise ESingleInstance.Create(SErrSingleInstanceNotClient);

  Result := FClient.PostRequest(aMsgType, aStream);
end;

procedure TAdvancedSingleInstance.ClientPostParams;
var
  xSL: TStringList;
  xStringStream: TStringStream;
  I: Integer;
begin
  if not Assigned(FClient) then
    raise ESingleInstance.Create(SErrSingleInstanceNotClient);

  xSL := TStringList.Create;
  try
    for I := 0 to ParamCount do
      xSL.Add(ParamStr(I));

    xStringStream := TStringStream.Create(xSL.DelimitedText);
    try
      xStringStream.Position := 0;
      FClient.PostRequest(MSGTYPE_PARAMS, xStringStream);
    finally
      xStringStream.Free;
    end;
  finally
    xSL.Free;
  end;
end;

function TAdvancedSingleInstance.ClientSendCustomRequest(
  const aMsgType: Integer; const aStream: TStream): Boolean;
begin
  if not Assigned(FClient) then
    raise ESingleInstance.Create(SErrSingleInstanceNotClient);

  Result := FClient.SendRequest(aMsgType, aStream, TimeOutMessages);
end;

function TAdvancedSingleInstance.ClientSendCustomRequest(
  const aMsgType: Integer; const aStream: TStream; out
  outRequestID: Integer): Boolean;
begin
  if not Assigned(FClient) then
    raise ESingleInstance.Create(SErrSingleInstanceNotClient);

  Result := FClient.SendRequest(aMsgType, aStream, TimeOutMessages, outRequestID);
end;

procedure TAdvancedSingleInstance.DoServerReceivedCustomRequest(
  const aMsgID: Integer; const aMsgType: Integer; const aStream: TStream);
begin
  if Assigned(FOnServerReceivedCustomRequest) then
    FOnServerReceivedCustomRequest(Self, aMsgID, aMsgType, aStream);
end;

function TAdvancedSingleInstance.GetIsClient: Boolean;
begin
  Result := Assigned(FClient);
end;

function TAdvancedSingleInstance.GetIsServer: Boolean;
begin
  Result := Assigned(FServer);
end;

function TAdvancedSingleInstance.GetStartResult: TSingleInstanceStart;
begin
  if not(Assigned(FServer) or Assigned(FClient)) then
    raise ESingleInstance.Create(SErrSingleInstanceStartResultNotAvailable);

  Result := inherited GetStartResult;
end;

procedure TAdvancedSingleInstance.ServerCheckMessages;
var
  xMsgID: Integer;
  xMsgType: Integer;
  xStream: TStream;
  xStringStream: TStringStream;
begin
  if not Assigned(FServer) then
    raise ESingleInstance.Create(SErrSingleInstanceNotServer);

  if not FServer.PeekRequest(xMsgID, xMsgType) then
    Exit;

  case xMsgType of
    MSGTYPE_CHECK:
    begin
      FServer.DeleteRequest(xMsgID);
      FServer.PostResponse(xMsgID, MSGTYPE_CHECKRESPONSE, nil);
    end;
    MSGTYPE_PARAMS:
    begin
      xStringStream := TStringStream.Create('');
      try
        FServer.ReadRequest(xMsgID, xStringStream);
        DoServerReceivedParams(xStringStream.DataString);
      finally
        xStringStream.Free;
      end;
    end;
    MSGTYPE_WAITFORINSTANCES:
      FServer.DeleteRequest(xMsgID);
  else
    xStream := TMemoryStream.Create;
    try
      FServer.ReadRequest(xMsgID, xStream);
      DoServerReceivedCustomRequest(xMsgID, xMsgType, xStream);
    finally
      xStream.Free;
    end;
  end;
end;

procedure TAdvancedSingleInstance.ServerPostCustomResponse(
  const aRequestID: Integer; const aMsgType: Integer;
  const aStream: TStream);
begin
  if not Assigned(FServer) then
    raise ESingleInstance.Create(SErrSingleInstanceNotServer);

  FServer.PostResponse(aRequestID, aMsgType, aStream);
end;

procedure TAdvancedSingleInstance.SetGlobal(const aGlobal: Boolean);
begin
  if FGlobal = aGlobal then Exit;
  if Assigned(FServer) or Assigned(FClient) then
    raise ESingleInstance.Create(SErrSetSingleInstanceGlobalStarted);
  FGlobal := aGlobal;
end;

procedure TAdvancedSingleInstance.SetID(const aID: string);
begin
  if FID = aID then Exit;
  if Assigned(FServer) or Assigned(FClient) then
    raise ESingleInstance.Create(SErrSetSingleInstanceIDStarted);
  FID := aID;
end;

function TAdvancedSingleInstance.Start: TSingleInstanceStart;
  {$IFNDEF MSWINDOWS}
  procedure UnixWorkaround(var bServerStarted: Boolean);
  var
    xWaitRequestID, xLastCount, xNewCount: Integer;
    xClient: TIPCClient;
  begin
    //file locking workaround for UNIX systems -> the server can be started twice if 2 processes are started in parallel
    //wait some time to see other clients
    FServer.StopServer(False);
    xClient := TIPCClient.Create(Self);
    try
      xClient.ServerID := FID;
      xClient.Global := FGlobal;
      xWaitRequestID := xClient.PostRequest(MSGTYPE_WAITFORINSTANCES, nil);
      xLastCount := -1;
      xNewCount := FServer.GetPendingRequestCount;
      while xLastCount <> xNewCount do
      begin
        xLastCount := xNewCount;
        Sleep(TimeOutWaitForInstances);
        xNewCount := FServer.GetPendingRequestCount;
      end;
    finally
      FreeAndNil(xClient);
    end;

    //find highest client that will be the server
    if FServer.FindHighestPendingRequestId = xWaitRequestID then
    begin
      bServerStarted := FServer.StartServer(False);
    end else
    begin
      //something went wrong, there are not-deleted waiting requests
      //use random sleep as workaround and try to restart the server
      Randomize;
      Sleep(Random(($3F+PtrInt(GetProcessID)) and $3F));//limit to $3F (63)
      bServerStarted := FServer.StartServer(False) and (FServer.GetPendingRequestCount > 0);
    end;
  end;
  {$ENDIF}
var
  xStream: TStream;
  xMsgType: Integer;
  xServerStarted: Boolean;
begin
  if Assigned(FServer) or Assigned(FClient) then
    raise ESingleInstance.Create(SErrStartSingleInstanceStarted);

  FServer := TIPCServer.Create(Self);
  FServer.ServerID := FID;
  FServer.Global := FGlobal;
  xServerStarted := FServer.StartServer(False);
  if xServerStarted then
  begin//this is single instance -> be server
    Result := siServer;
    {$IFNDEF MSWINDOWS}
    UnixWorkaround(xServerStarted);
    {$ENDIF}
  end;
  if not xServerStarted then
  begin//instance found -> be client
    FreeAndNil(FServer);
    FClient := TIPCClient.Create(Self);
    FClient.ServerID := FID;
    FClient.Global := FGlobal;
    FClient.PostRequest(MSGTYPE_CHECK, nil);
    xStream := TMemoryStream.Create;
    try
      if FClient.PeekResponse(xStream, xMsgType, TimeOutMessages) then
        Result := siClient
      else
        Result := siNotResponding;
    finally
      xStream.Free;
    end;
  end;
  SetStartResult(Result);
end;

procedure TAdvancedSingleInstance.Stop;
begin
  FreeAndNil(FServer);
  FreeAndNil(FClient);
end;

initialization
  DefaultSingleInstanceClass:=TAdvancedSingleInstance;

end.

