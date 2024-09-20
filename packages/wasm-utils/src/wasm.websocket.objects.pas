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
    FOnClose: TWasmWebsocketCloseEvent;
    FOnError: TWasmWebsocketErrorEvent;
    FOnMessage: TWasmWebsocketMessageEvent;
    FOnOpen: TWasmWebsocketOpenEvent;
    FProtocols: String;
    FURL: String;
    FWebSocketID: TWasmWebSocketID;
    FClosed : Boolean;
    procedure DoSendMessage(aBytes: TBytes; aType: longint);
  Protected
    procedure CheckWebsocketRes(aResult: TWasmWebsocketResult; const aMsg: String; aLogOnly: Boolean=false);
    Procedure DoOpen(const aURL : String; const aProtocols : String); virtual;
    Procedure DoClose(aCode : Longint; aReason: UTF8String; aRaiseError : Boolean); virtual;
    // Called from host
    Procedure HandleError; virtual;
    procedure HandleOpen; virtual;
    procedure HandleMessage(aType : Longint; aMessage : TBytes); virtual;
    procedure HandleClose(aCode : Longint; aReason : string; aIsClean : Boolean); virtual;

  Public
    Constructor create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure Open(const aURL : String; const aProtocols : String);
    Procedure Close(aCode : Longint; aReason: UTF8String);
    Procedure SendMessage(aBytes : TBytes);
    Procedure SendMessage(const aString : String);
    Property WebSocketID : TWasmWebSocketID Read FWebSocketID;
    Property OnError : TWasmWebsocketErrorEvent Read FOnError Write FOnError;
    Property OnMessage : TWasmWebsocketMessageEvent Read FOnMessage Write FOnMessage;
    Property OnClose : TWasmWebsocketCloseEvent Read FOnClose Write FOnClose;
    Property OnOpen : TWasmWebsocketOpenEvent Read FOnOpen Write FOnOpen;
    Property URL : String Read FURL;
    Property Protocols : String Read FProtocols;
  end;

  { TWasmWebSocketManager }
  TWasmWebSocketManager = class(TObject)
  private
    class var _Instance : TWasmWebSocketManager;
    class function GetInstance: TWasmWebSocketManager; static;
  private
    Flist : TFPList; // Todo: change to thread list.
  protected
    class procedure HandleClose(aWebSocketID: TWasmWebSocketID; aUserData: Pointer; aCode: Longint; const aReason: String; aClean: Boolean); static;
    class procedure HandleError(aWebSocketID: TWasmWebSocketID; aUserData: Pointer); static;
    class procedure HandleMessage(aWebSocketID: TWasmWebSocketID; aUserData: Pointer; aMessageType: TWasmWebSocketMessageType;  aMessage: TBytes); static;
    class procedure HandleOpen(aWebSocketID: TWasmWebSocketID; aUserData: Pointer); static;
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
  if __wasm_websocket_allocate(PByte(lURL),Length(lURL),PByte(lProtocols),Length(lProtocols),Self,@FWebSocketID)<>WASMWS_RESULT_SUCCESS then
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
  inherited Destroy;
end;


procedure TWasmWebsocket.Open(const aURL: String; const aProtocols: String);
begin
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
  CheckWebsocketRes(Res,'Failed to send '+aTypes[aType=WASMWS_MESSAGE_TYPE_TEXT]+' data on websocket');
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
    _instance:=TWasmWebSocketManager.Create;
    end;
  Result:=_instance;
end;


procedure TWasmWebSocketManager.RegisterWebSocket(aWebSocket: TWasmWebSocket);
begin
  Writeln(Format('adding websocket [%p]',[Pointer(aWebSocket)]));
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
    __wasmwebsocket_log(wllError,'Invalid websocket received: %d [%p]',[aWebsocketID,aUserData]);
end;


class procedure TWasmWebSocketManager.HandleError(aWebSocketID : TWasmWebSocketID; aUserData : Pointer);
begin
  If Instance.IsValidWebSocket(aWebSocketID,aUserData) then
    TWasmWebSocket(aUserData).HandleError;
end;


class procedure TWasmWebSocketManager.HandleMessage(aWebSocketID : TWasmWebSocketID; aUserData : Pointer; aMessageType : TWasmWebSocketMessageType; aMessage : TBytes);
begin
  If Instance.IsValidWebSocket(aWebSocketID,aUserData) then
    TWasmWebSocket(aUserData).HandleMessage(aMessageType,aMessage);
end;


class procedure TWasmWebSocketManager.HandleClose(aWebSocketID : TWasmWebSocketID; aUserData : Pointer; aCode: Longint; const aReason : String; aClean : Boolean);
begin
  If Instance.IsValidWebSocket(aWebSocketID,aUserData) then
    TWasmWebSocket(aUserData).HandleClose(aCode,aReason,aClean)
end;


class procedure TWasmWebSocketManager.HandleOpen(aWebSocketID : TWasmWebSocketID; aUserData : Pointer);
begin
  If Instance.IsValidWebSocket(aWebSocketID,aUserData) then
    TWasmWebSocket(aUserData).HandleOpen;
end;

class constructor TWasmWebSocketManager.init;
begin
  WebSocketErrorCallback:=@HandleError;
  WebSocketMessageCallback:=@HandleMessage;
  WebSocketCloseCallback:=@HandleClose;
  WebSocketOpenCallback:=@HandleOpen;
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

