unit singleinstance;

{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2015 by Ondrej Pokorny

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, Classes, advancedipc;

type

  TBaseSingleInstance = class;

  //siServer: No other instance is running. The server is started.
  //siClient: There is another instance running. This instance is used as client.
  //siNotResponding: There is another instance running but it doesn't respond.
  TSingleInstanceStart = (siServer, siClient, siNotResponding);
  TSingleInstanceParams = procedure(Sender: TBaseSingleInstance; Params: TStringList) of object;
  TSingleInstanceReceivedCustomMessage = procedure(Sender: TBaseSingleInstance; MsgID: Integer; MsgType: TMessageType; MsgData: TStream) of object;
  TBaseSingleInstance = class(TComponent)
  private
    FGlobal: Boolean;
    FID: string;
    FServer: TIPCServer;
    FClient: TIPCClient;
    FStartResult: TSingleInstanceStart;
    FTimeOutMessages: Integer;
    FTimeOutWaitForInstances: Integer;
    FOnServerReceivedCustomRequest: TSingleInstanceReceivedCustomMessage;
    FOnServerReceivedParams: TSingleInstanceParams;
    function GetIsClient: Boolean;
    function GetIsServer: Boolean;
    function GetStartResult: TSingleInstanceStart;
    procedure SetGlobal(const aGlobal: Boolean);
    procedure SetID(const aID: string);
    procedure DoServerReceivedParams(const aParamsDelimitedText: string);
    procedure DoServerReceivedCustomRequest(const aMsgID: Integer; const aMsgType: TMessageType; const aStream: TStream);
  protected
    //call Start when you want to start single instance checking
    function Start: TSingleInstanceStart;
    //stop single instance server or client
    procedure Stop;

    procedure ServerCheckMessages;
    procedure ClientPostParams;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    function ClientPostCustomRequest(const aMsgType: TMessageType; const aStream: TStream): Integer;
    function ClientSendCustomRequest(const aMsgType: TMessageType; const aStream: TStream): Boolean; overload;
    function ClientSendCustomRequest(const aMsgType: TMessageType; const aStream: TStream; out outRequestID: Integer): Boolean; overload;
    procedure ServerPostCustomResponse(const aRequestID: Integer; const aMsgType: TMessageType; const aStream: TStream);
    function ClientPeekCustomResponse(const aStream: TStream; out outMsgType: TMessageType): Boolean;
  public
    property ID: string read FID write SetID;
    property Global: Boolean read FGlobal write SetGlobal;
    property TimeOutMessages: Integer read FTimeOutMessages write FTimeOutMessages;
    property TimeOutWaitForInstances: Integer read FTimeOutWaitForInstances write FTimeOutWaitForInstances;
    property OnServerReceivedParams: TSingleInstanceParams read FOnServerReceivedParams write FOnServerReceivedParams;
    property OnServerReceivedCustomRequest: TSingleInstanceReceivedCustomMessage read FOnServerReceivedCustomRequest write FOnServerReceivedCustomRequest;
  public
    property StartResult: TSingleInstanceStart read GetStartResult;
    property IsServer: Boolean read GetIsServer;
    property IsClient: Boolean read GetIsClient;
  end;

  TSingleInstance = class(TBaseSingleInstance)
  public
    function Start: TSingleInstanceStart;
    procedure Stop;

    procedure ServerCheckMessages;
    procedure ClientPostParams;
  end;

  ESingleInstance = class(Exception);

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

{ TSingleInstance }

procedure TSingleInstance.ClientPostParams;
begin
  inherited ClientPostParams;
end;

procedure TSingleInstance.ServerCheckMessages;
begin
  inherited ServerCheckMessages;
end;

function TSingleInstance.Start: TSingleInstanceStart;
begin
  Result := inherited Start;
end;

procedure TSingleInstance.Stop;
begin
  inherited Stop;
end;

{ TBaseSingleInstance }

function TBaseSingleInstance.ClientPeekCustomResponse(const aStream: TStream; out
  outMsgType: TMessageType): Boolean;
begin
  if not Assigned(FClient) then
    raise ESingleInstance.Create(SErrSingleInstanceNotClient);

  Result := FClient.PeekResponse(aStream, outMsgType, FTimeOutMessages);
end;

function TBaseSingleInstance.ClientPostCustomRequest(const aMsgType: TMessageType;
  const aStream: TStream): Integer;
begin
  if not Assigned(FClient) then
    raise ESingleInstance.Create(SErrSingleInstanceNotClient);

  Result := FClient.PostRequest(aMsgType, aStream);
end;

procedure TBaseSingleInstance.ClientPostParams;
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

function TBaseSingleInstance.ClientSendCustomRequest(
  const aMsgType: TMessageType; const aStream: TStream): Boolean;
begin
  if not Assigned(FClient) then
    raise ESingleInstance.Create(SErrSingleInstanceNotClient);

  Result := FClient.SendRequest(aMsgType, aStream, FTimeOutMessages);
end;

function TBaseSingleInstance.ClientSendCustomRequest(const aMsgType: TMessageType;
  const aStream: TStream; out outRequestID: Integer): Boolean;
begin
  if not Assigned(FClient) then
    raise ESingleInstance.Create(SErrSingleInstanceNotClient);

  Result := FClient.SendRequest(aMsgType, aStream, FTimeOutMessages, outRequestID);
end;

constructor TBaseSingleInstance.Create(aOwner: TComponent);
var
  xID: RawByteString;
  I: Integer;
begin
  inherited Create(aOwner);

  FTimeOutMessages := 1000;
  FTimeOutWaitForInstances := 100;

  xID := 'SI_'+ExtractFileName(ParamStr(0));
  for I := 1 to Length(xID) do
    case xID[I] of
      'a'..'z', 'A'..'Z', '0'..'9', '_': begin end;
    else
      xID[I] := '_';
    end;
  ID := xID;
end;

destructor TBaseSingleInstance.Destroy;
begin
  Stop;

  inherited Destroy;
end;

procedure TBaseSingleInstance.DoServerReceivedCustomRequest(
  const aMsgID: Integer; const aMsgType: TMessageType; const aStream: TStream);
begin
  if Assigned(FOnServerReceivedCustomRequest) then
    FOnServerReceivedCustomRequest(Self, aMsgID, aMsgType, aStream);
end;

procedure TBaseSingleInstance.DoServerReceivedParams(
  const aParamsDelimitedText: string);
var
  xSL: TStringList;
begin
  if not Assigned(FOnServerReceivedParams) then
    Exit;

  xSL := TStringList.Create;
  try
    xSL.DelimitedText := aParamsDelimitedText;
    FOnServerReceivedParams(Self, xSL);
  finally
    xSL.Free;
  end;
end;

function TBaseSingleInstance.GetIsClient: Boolean;
begin
  Result := Assigned(FClient);
end;

function TBaseSingleInstance.GetIsServer: Boolean;
begin
  Result := Assigned(FServer);
end;

function TBaseSingleInstance.GetStartResult: TSingleInstanceStart;
begin
  if not(Assigned(FServer) or Assigned(FClient)) then
    raise ESingleInstance.Create(SErrSingleInstanceStartResultNotAvailable);

  Result := FStartResult;
end;

procedure TBaseSingleInstance.ServerCheckMessages;
var
  xMsgID: Integer;
  xMsgType: TMessageType;
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

procedure TBaseSingleInstance.ServerPostCustomResponse(
  const aRequestID: Integer; const aMsgType: TMessageType;
  const aStream: TStream);
begin
  if not Assigned(FServer) then
    raise ESingleInstance.Create(SErrSingleInstanceNotServer);

  FServer.PostResponse(aRequestID, aMsgType, aStream);
end;

procedure TBaseSingleInstance.SetGlobal(const aGlobal: Boolean);
begin
  if FGlobal = aGlobal then Exit;
  if Assigned(FServer) or Assigned(FClient) then
    raise ESingleInstance.Create(SErrSetSingleInstanceGlobalStarted);
  FGlobal := aGlobal;
end;

procedure TBaseSingleInstance.SetID(const aID: string);
begin
  if FID = aID then Exit;
  if Assigned(FServer) or Assigned(FClient) then
    raise ESingleInstance.Create(SErrSetSingleInstanceIDStarted);
  FID := aID;
end;

procedure TBaseSingleInstance.Stop;
begin
  FreeAndNil(FServer);
  FreeAndNil(FClient);
end;

function TBaseSingleInstance.Start: TSingleInstanceStart;
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
        Sleep(FTimeOutWaitForInstances);
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
      Sleep(Random(($3F+PtrInt(GetCurrentThreadId)) and $3F));//limit to $3F (63)
      bServerStarted := FServer.StartServer(False) and (FServer.GetPendingRequestCount > 0);
    end;
  end;
  {$ENDIF}
var
  xStream: TStream;
  xMsgType: TMessageType;
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
      if FClient.PeekResponse(xStream, xMsgType, FTimeOutMessages) then
        Result := siClient
      else
        Result := siNotResponding;
    finally
      xStream.Free;
    end;
  end;
  FStartResult := Result;
end;

end.

