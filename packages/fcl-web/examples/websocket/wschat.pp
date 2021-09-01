{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2021 - by the Free Pascal development team

    Simple websocket chat server implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wschat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcustwsserver, fpwebsocket, syncobjs, fpjson;

Type

  { TWebsocketChat }
  TChatLogEvent = procedure (Sender : TObject; Const Msg : String) of object;

  TWebsocketChat = Class(TComponent)
  Private
    FLock : TCriticalSection;
    FMap : TStringList;
    FOnLog: TChatLogEvent;
    FSrv: TCustomWSServer;
    procedure SetServer(AValue: TCustomWSServer);
  Protected
    Procedure DoLog(Const Msg : String); overload;
    Procedure DoLog(Const Fmt : String; Args : Array of const); overload;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetConnectionFromUser(aFrom: String): TWSConnection; virtual;
    procedure MapConnection(aFrom: String; aConn: TWSConnection); virtual;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    procedure DoControlReceived(Sender: TObject; aType: TFrameType; const aData: TBytes); virtual;
    procedure DoDisconnect(Sender: TObject); virtual;
    procedure DoMessageReceived(Sender: TObject; const aMessage: TWSMessage); virtual;
    Property WebsocketServer : TCustomWSServer Read FSrv Write SetServer;
    Property OnLog : TChatLogEvent Read FOnLog Write FOnLog;
  end;

implementation

Constructor TWebsocketChat.Create(aOwner : TComponent);

begin
  Inherited;
  FMap:=TStringList.Create;
  FLock:=TCriticalSection.Create;
end;

destructor TWebsocketChat.Destroy;
begin
  FreeAndNil(FLock);
  FreeAndNil(FMap);
  inherited Destroy;
end;

procedure TWebsocketChat.DoMessageReceived(Sender: TObject; const aMessage: TWSMessage);

Var
  S,From,Recip : String;
  D : TJSONData;
  Msg : TJSONObject absolute D;
  SenderConn,RecipConn : TWSConnection;

begin
  SenderConn:=Sender as TWSConnection;
  RecipConn:=Nil;
  S:=aMessage.AsString;
  DoLog('Received message: '+S);
  try
    D:=GetJSON(S);
    try
      if Not (D is TJSONOBject) then
        Raise EJSON.Create('Not an object: '+S);
      From:=Msg.Get('from','');
      if From<>'' then
        MapConnection(From,SenderConn);
      Recip:=Msg.Get('to','');
    finally
      FreeAndNil(D)
    end;
  except
    DoLog('Message is not JSON, echoing as JSON');
    S:='{ "msg": "You sent: '+StringReplace(S,'"','\"',[rfReplaceAll])+'" }';
    RecipConn:=SenderConn;
  end;
  if (Recip<>'') then
    begin
    RecipConn:=GetConnectionFromUser(Recip);
    if RecipConn=Nil then
      exit;
    end;
  if Assigned(RecipConn) then
    RecipConn.Send(S)
  else
    FSRv.BroadcastMessage(S);
end;

procedure TWebsocketChat.DoControlReceived(Sender: TObject; aType: TFrameType; const aData: TBytes);

Var
  aReason : String;
  aCode : Integer;

begin
  Case aType of
  ftClose:
    begin
    aCode:=TWSConnection(Sender).GetCloseData(aData,aReason);
    DoLog('Close code %d received with reason: %s',[aCode,aReason]);
    end;
  ftPing:
    begin
    DoLog('Ping received');
    end;
  ftPong:
    begin
    DoLog('Pong received');
    end;
  else
    DoLog('Unknown control code: %d',[Ord(aType)]);
  end;
end;

procedure TWebsocketChat.DoDisconnect(Sender: TObject);

Var
  Conn : TWSConnection;
  Found : Boolean;
  I : Integer;
  aID,N,V : String;


begin
  Conn:=(Sender as TWSConnection);
  aID:=Conn.ConnectionID;
  DoLog('Connection '+aID+' disappeared');
  FLock.Enter;
  try
    Found:=False;
    I:=FMap.Count-1;
    While (I>=0) and not Found do
      begin
      FMap.GetNameValue(I,N,V);
      Found:=SameText(V,aID);
      if Found then
        FMap.Delete(I);
      Dec(I);
      end;
  finally
    Flock.Leave;
  end;
end;

Function TWebsocketChat.GetConnectionFromUser(aFrom : String): TWSConnection;

Var
  aID : String;

begin
  FLock.Enter;
  try
    aID:=FMap.Values[aFrom];
  finally
    FLock.Leave;
  end;
  Result:=FSrv.Connections.FindConnectionById(aID);
end;

procedure TWebsocketChat.MapConnection(aFrom : String; aConn : TWSConnection);

begin
  // We could also store the connection object directly in the objects array,
  // but this way we demonstrate the ConnectionID and FindConnectionByID
  Flock.Enter;
  try
    FMap.Values[aFrom]:=aConn.ConnectionID;
  finally
    FLock.Leave;
  end;
end;

procedure TWebsocketChat.SetServer(AValue: TCustomWSServer);
begin
  if FSrv=AValue then Exit;
  if Assigned(FSRV) then
    FSRV.RemoveFreeNotification(Self);
  FSrv:=AValue;
  if Assigned(FSRV) then
    FSRV.FreeNotification(Self);
end;

procedure TWebsocketChat.DoLog(const Msg: String);
begin
  If Assigned(FonLog) then
    FOnLog(Self,Msg);
end;

procedure TWebsocketChat.DoLog(const Fmt: String; Args: array of const);
begin
  DoLog(Format(Fmt,Args));
end;

procedure TWebsocketChat.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (aComponent=FSrv) then
    FSrv:=Nil;
end;


end.

