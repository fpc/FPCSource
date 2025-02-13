{
    This file is part of the Free Component Library

    Webassembly Websocket - Simple objects around the low-level API
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wasm.websocket.objects;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, wasm.websocket.api, wasm.websocket.shared;
  {$ELSE}
  Classes, SysUtils, wasm.websocket.api, wasm.websocket.shared;
  {$ENDIF}

Type
  EWasmWebsocket = Class(Exception);
  TWasmWebSocketManager = Class;
  TWasmWebsocket = Class;

  TWasmWebSocketManagerClass = Class of TWasmWebSocketManager;
  TWasmWebSocketClass = Class of TWasmWebsocket;

  TWasmWebsocketErrorEvent = procedure(Sender : TObject) of object;
  TWasmWebsocketMessageEvent = procedure(Sender : TObject; const IsString : Boolean; aPayload : TBytes) of object;
  TWasmWebsocketOpenEvent = procedure(Sender : TObject) of object;
  TWasmWebsocketCloseEvent = procedure(Sender : TObject; aCode : Integer; const aReason : string; aClean : Boolean) of object;

  { TWasmWebsocket }

  TWasmWebsocket = class(TComponent)
  private
    class var _NextID : TWasmWebSocketID;
    class function GetNextWebsocketID : TWasmWebSocketID;
  private
    FKeepDataAlive: Boolean;
    FOnClose: TWasmWebsocketCloseEvent;
    FOnError: TWasmWebsocketErrorEvent;
    FOnMessage: TWasmWebsocketMessageEvent;
    FOnOpen: TWasmWebsocketOpenEvent;
    FProtocols: String;
    FURL: String;
    FWebSocketID: TWasmWebSocketID;
    FClosed : Boolean;
    FData : Array of TBytes;
    FDataCount : Integer;
    procedure DoSendMessage(aBytes: TBytes; aType: longint);
    procedure SetKeepDataAlive(AValue: Boolean);
  Protected
    procedure CheckWebsocketRes(aResult: TWasmWebsocketResult; const aMsg: String; aLogOnly: Boolean=false);
    Procedure DoOpen(const aURL : String; const aProtocols : String); virtual;
    Procedure DoClose(aCode : Longint; aReason: UTF8String; aRaiseError : Boolean); virtual;
    // Called from host
    Procedure HandleError; virtual;
    procedure HandleOpen; virtual;
    procedure HandleMessage(aType : Longint; aMessage : TBytes); virtual;
    procedure HandleClose(aCode : Longint; aReason : string; aIsClean : Boolean); virtual;
    // Data management
    procedure ReleaseAllData;
    Procedure KeepData(const aData : TBytes);
  Public
    Constructor create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure Open(const aURL : String; const aProtocols : String);
    Procedure Close(aCode : Longint; aReason: UTF8String);
    Procedure SendMessage(aBytes : TBytes);
    Procedure SendMessage(const aString : String);
    function ReleaseData(const aData : TBytes) : boolean;
    Property WebSocketID : TWasmWebSocketID Read FWebSocketID;
    Property OnError : TWasmWebsocketErrorEvent Read FOnError Write FOnError;
    Property OnMessage : TWasmWebsocketMessageEvent Read FOnMessage Write FOnMessage;
    Property OnClose : TWasmWebsocketCloseEvent Read FOnClose Write FOnClose;
    Property OnOpen : TWasmWebsocketOpenEvent Read FOnOpen Write FOnOpen;
    Property URL : String Read FURL;
    Property Protocols : String Read FProtocols;
    Property KeepDataAlive : Boolean Read FKeepDataAlive Write SetKeepDataAlive;
  end;

  { TWasmWebSocketManager }
  TWasmWebSocketManager = class(TObject)
  private
    class var _Instance : TWasmWebSocketManager;
    class function GetInstance: TWasmWebSocketManager; static;
  private
    Flist : TFPList; // Todo: change to thread list.
  protected
    class procedure HandleReleasePackageCallBack(aWebsocketID: TWasmWebSocketID; aUserData: Pointer; aPacket: Pointer; var Result: boolean); static;
    class procedure HandleCloseCallBack(aWebSocketID: TWasmWebSocketID; aUserData: Pointer; aCode: Longint; const aReason: String; aClean: Boolean); static;
    class procedure HandleErrorCallBack(aWebSocketID: TWasmWebSocketID; aUserData: Pointer); static;
    class procedure HandleMessageCallBack(aWebSocketID: TWasmWebSocketID; aUserData: Pointer; aMessageType: TWasmWebSocketMessageType;  aMessage: TBytes); static;
    class procedure HandleOpenCallBack(aWebSocketID: TWasmWebSocketID; aUserData: Pointer); static;
    function HandleReleasePacket(aSocket : TWasmWebSocket; aPacket : Pointer) : Boolean; virtual;
    procedure HandleClose(aSocket : TWasmWebSocket; aCode: Longint; const aReason: String; aClean: Boolean); virtual;
    procedure HandleError(aSocket:  TWasmWebSocket); virtual;
    procedure HandleMessage(aSocket: TWasmWebSocket; aMessageType: TWasmWebSocketMessageType;  aMessage: TBytes); virtual;
    procedure HandleOpen(aSocket: TWasmWebSocket); virtual;

    procedure RegisterWebSocket(aWebSocket : TWasmWebSocket);
    procedure UnRegisterWebSocket(aWebSocket : TWasmWebSocket);
    function IsValidWebSocket(aWebSocketID: TWasmWebSocketID; aUserData: Pointer) : Boolean;
  Public
    class constructor init;
    constructor create; virtual;
    destructor destroy; override;
    class var DefaultInstanceType : TWasmWebSocketManagerClass;
    Class Property Instance : TWasmWebSocketManager Read GetInstance;
  end;

implementation



{ TWasmWebsocket }

class procedure TWasmWebSocketManager.HandleReleasePackageCallBack(aWebsocketID: TWasmWebSocketID; aUserData: Pointer; aPacket: Pointer; var Result: boolean);
begin
  If Instance.IsValidWebSocket(aWebSocketID,aUserData) then
    Result:=Instance.HandleReleasePacket(TWasmWebSocket(aUserData),aPacket);
end;

constructor TWasmWebsocket.create(aOwner : TComponent);

begin
  Inherited;
  TWasmWebSocketManager.Instance.RegisterWebSocket(Self);
  FClosed:=False
end;

procedure TWasmWebsocket.DoClose(aCode: Longint; aReason: UTF8String; aRaiseError: Boolean);

var
  Res : TWasmWebsocketResult;

begin
  if FWebSocketID=0 then
    exit;
  Res:=__wasm_websocket_close(FWebSocketID,aCode,PByte(PAnsiChar(aReason)),Length(aReason));
  CheckWebsocketRes(Res,'close',not aRaiseError);
end;

procedure TWasmWebsocket.HandleError;
begin
  if assigned(FonError) then
    FOnError(Self);
end;

procedure TWasmWebsocket.HandleOpen;
begin
  if assigned(FonOpen) then
    FOnOpen(Self);
end;

procedure TWasmWebsocket.HandleMessage(aType: Longint; aMessage: TBytes);
begin
  if assigned(FOnMessage) then
    FOnMessage(Self,aType=WASMWS_MESSAGE_TYPE_TEXT,aMessage);
end;

procedure TWasmWebsocket.HandleClose(aCode: Longint; aReason: string; aIsClean: Boolean);
begin
  FClosed:=True;
  if assigned(FonClose) then
    FOnClose(Self,aCode,aReason,aIsClean);
end;

procedure TWasmWebsocket.DoOpen(const aURL: String; const aProtocols: String);

var
  lURL,lProtocols : UTF8String;

begin
  FURL:=aURL;
  FProtocols:=aProtocols;
  lURL:=UTF8Encode(aURL);
  lProtocols:=UTF8Encode(aProtocols);
  if __wasm_websocket_allocate(PByte(lURL),Length(lURL),PByte(lProtocols),Length(lProtocols),Self,FWebSocketID)<>WASMWS_RESULT_SUCCESS then
    Raise EWasmWebsocket.CreateFmt('Failed to allocate websocket for URL %s',[aURL]);
end;

destructor TWasmWebsocket.Destroy;

var
  Res : TWasmWebsocketResult;

begin
  if not FClosed then
    DoClose(0,'',False);
  res:=__wasm_websocket_deallocate(FWebSocketID);
  CheckWebsocketRes(Res,'Deallocating websocket',True);
  FWebSocketID:=0;
  TWasmWebSocketManager.Instance.UnRegisterWebSocket(Self);
  ReleaseAllData;
  inherited Destroy;
end;

procedure TWasmWebsocket.Open(const aURL: String; const aProtocols: String);
begin
  FWebSocketID:=GetNextWebsocketID;
  DoOpen(aURL,aProtocols);
end;

procedure TWasmWebsocket.Close(aCode: Longint; aReason: UTF8String);

begin
  DoClose(aCode,aReason,True);
  FClosed:=True;
end;

procedure TWasmWebsocket.CheckWebsocketRes(aResult : TWasmWebsocketResult; const aMsg :String; aLogOnly : Boolean = false);

var
  Err : String;

begin
  if aResult=WASMWS_RESULT_SUCCESS then
    Exit;
  Err:=Format('Websocket %d (URL: %s) got error %d: %s',[FWebSocketID,FURL,aResult,aMsg]);
  __wasmwebsocket_log(wllError,Err);
  if not aLogOnly then
    Raise EWasmWebsocket.Create(Err);
end;

class function TWasmWebsocket.GetNextWebsocketID: TWasmWebSocketID;
begin
  Result:=InterlockedIncrement(_NextID);
end;

procedure TWasmWebsocket.DoSendMessage(aBytes: TBytes; aType : longint);

const
  aTypes : Array[Boolean] of string = ('binary','text');

var
  Res : TWasmWebsocketResult;
  DataLen : Longint;

begin
  DataLen:=Length(aBytes);
  if DataLen=0 then
    exit;
  Res:=__wasm_websocket_send(FWebsocketID,PByte(aBytes),DataLen,aType);
  if (Res=WASMWS_RESULT_SUCCESS) and KeepDataAlive then
    KeepData(aBytes);
  CheckWebsocketRes(Res,'Failed to send '+aTypes[aType=WASMWS_MESSAGE_TYPE_TEXT]+' data on websocket');
end;

procedure TWasmWebsocket.SetKeepDataAlive(AValue: Boolean);
begin
  if FKeepDataAlive=AValue then Exit;
  FKeepDataAlive:=AValue;
  if not FKeepDataAlive then
    ReleaseAllData;
end;

procedure TWasmWebsocket.ReleaseAllData;

begin
  SetLength(FData,0);
end;

procedure TWasmWebsocket.KeepData(const aData: TBytes);
var
  lLen : Integer;
begin
  lLen:=Length(FData);
  if (FDataCount=lLen) then
    SetLength(FData,lLen+10);
  FData[FDataCount]:=aData;
  Inc(FDataCount);
end;

function TWasmWebsocket.ReleaseData(const aData: TBytes): boolean;
var
  lIdx : Integer;
begin
  Result:=False;
  lIdx:=FDataCount-1;
  While (lIdx>=0) and (FData[lIdx]<>aData) do
    Dec(lIdx);
  if (lIdx<0) then
    exit;
  if lIdx<FDataCount-1 then
    FData[lIdx]:=FData[FDataCount-1];
  FData[FDataCount-1]:=Nil;
  Dec(FDataCount);
  Result:=True;
end;

procedure TWasmWebsocket.SendMessage(aBytes: TBytes);

begin
  DoSendMessage(aBytes,WASMWS_MESSAGE_TYPE_BINARY);
end;

procedure TWasmWebsocket.SendMessage(const aString: String);

var
  Buf : TBytes;

begin
  if Length(aString)=0 then
    exit;
  {$IF SIZEOF(CHAR)=1}
  Buf:=TEncoding.UTF8.GetAnsiBytes(aString);
  {$ELSE}
  Buf:=TEncoding.UTF8.GetBytes(aString);
  {$ENDIF}
  DoSendMessage(Buf,WASMWS_MESSAGE_TYPE_TEXT);
end;

{ TWasmWebSocketManager }

class function TWasmWebSocketManager.GetInstance: TWasmWebSocketManager; static;

var
  C : TWasmWebSocketManagerClass;

begin
  if _instance=nil then
    begin
    C:=DefaultInstanceType;
    if C=Nil then C:=TWasmWebSocketManager;
    _instance:=C.Create;
    end;
  Result:=_Instance;
end;

procedure TWasmWebSocketManager.RegisterWebSocket(aWebSocket: TWasmWebSocket);
begin
  Flist.Add(aWebSocket);
end;

procedure TWasmWebSocketManager.UnRegisterWebSocket(aWebSocket: TWasmWebSocket);
begin
  Flist.Remove(aWebSocket);
end;

function TWasmWebSocketManager.IsValidWebSocket(aWebSocketID: TWasmWebSocketID; aUserData: Pointer): Boolean;
begin
  Result:=FList.IndexOf(aUserData)<>-1;
  If Result then
    Result:=TWasmWebSocket(aUserData).WebSocketID=aWebSocketID;
  if not Result then
    begin
    __wasmwebsocket_log(wllError,'Invalid websocket received: %d [%p]',[aWebsocketID,aUserData]);
    end;
end;

class procedure TWasmWebSocketManager.HandleErrorCallBack(aWebSocketID: TWasmWebSocketID; aUserData: Pointer);
begin
  If Instance.IsValidWebSocket(aWebSocketID,aUserData) then
    Instance.HandleError(TWasmWebSocket(aUserData));
end;

class procedure TWasmWebSocketManager.HandleMessageCallBack(aWebSocketID: TWasmWebSocketID; aUserData: Pointer;
  aMessageType: TWasmWebSocketMessageType; aMessage: TBytes);
begin
  If Instance.IsValidWebSocket(aWebSocketID,aUserData) then
    Instance.HandleMessage(TWasmWebSocket(aUserData),aMessageType,aMessage);
end;

class procedure TWasmWebSocketManager.HandleCloseCallBack(aWebSocketID: TWasmWebSocketID; aUserData: Pointer; aCode: Longint;
  const aReason: String; aClean: Boolean);
begin
  If Instance.IsValidWebSocket(aWebSocketID,aUserData) then
    Instance.HandleClose(TWasmWebSocket(aUserData),aCode,aReason,aClean);
end;

class procedure TWasmWebSocketManager.HandleOpenCallBack(aWebSocketID: TWasmWebSocketID; aUserData: Pointer);
begin
  If Instance.IsValidWebSocket(aWebSocketID,aUserData) then
    Instance.HandleOpen(TWasmWebSocket(aUserData));
end;

function TWasmWebSocketManager.HandleReleasePacket(aSocket: TWasmWebSocket; aPacket: Pointer): Boolean;
begin
  aSocket.ReleaseData(TBytes(aPacket));
end;

procedure TWasmWebSocketManager.HandleClose(aSocket: TWasmWebSocket; aCode: Longint; const aReason: String; aClean: Boolean);
begin
  aSocket.HandleClose(aCode,aReason,aClean);
end;

procedure TWasmWebSocketManager.HandleError(aSocket: TWasmWebSocket);
begin
  aSocket.HandleError;
end;

procedure TWasmWebSocketManager.HandleMessage(aSocket: TWasmWebSocket; aMessageType: TWasmWebSocketMessageType; aMessage: TBytes);
begin
  aSocket.HandleMessage(aMessageType,aMessage);
end;

procedure TWasmWebSocketManager.HandleOpen(aSocket: TWasmWebSocket);
begin
  aSocket.HandleOpen;
end;


class constructor TWasmWebSocketManager.init;
begin
  WebSocketErrorCallback:=@HandleErrorCallBack;
  WebSocketMessageCallback:=@HandleMessageCallBack;
  WebSocketCloseCallback:=@HandleCloseCallBack;
  WebSocketOpenCallback:=@HandleOpenCallBack;
  WebSocketReleasePackageCallBack:=@HandleReleasePackageCallBack;
end;

constructor TWasmWebSocketManager.create;
begin
  Flist:=TFPList.Create;
end;

destructor TWasmWebSocketManager.destroy;
begin
  FreeAndNil(Flist);
  inherited destroy;
end;

end.

