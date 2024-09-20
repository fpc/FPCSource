{
    This file is part of the Free Component Library

    Webassembly Websocket API - imported functions and structures.
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit wasm.websocket.api;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils,
  {$ELSE}
  sysutils,
  {$ENDIF}
  wasm.logger.api,
  wasm.websocket.shared;

Type
  TWasmWebSocketLogLevel = TWasmLogLevel;
  TWasmWebSocketLogLevels = set of TWasmWebsocketLogLevel;

const
  wllTrace    = wasm.logger.api.wllTrace;
  wllDebug    = wasm.logger.api.wllDebug;
  wllInfo     = wasm.logger.api.wllInfo;
  wllWarning  = wasm.logger.api.wllWarning;
  wllError    = wasm.logger.api.wllError;
  wllCritical = wasm.logger.api.wllCritical;

function __wasm_websocket_allocate(
    aURL : PByte;
    aUrlLen : Longint;
    aProtocols : PByte;
    aProtocolLen : Longint;
    aUserData : Pointer;
    aWebsocketID : PWasmWebSocketID) : TWasmWebsocketResult; external websocketExportName name websocketFN_Allocate;

function __wasm_websocket_close(
    aWebsocketID : TWasmWebSocketID;
    aCode : Longint;
    aReason : PByte;
    aReasonLen : Longint) : TWasmWebsocketResult; external websocketExportName name websocketFN_Close;

function __wasm_websocket_send(
    aWebsocketID : TWasmWebSocketID;
    aData : PByte;
    aDataLen : Longint;
    aType : Longint
    ) : TWasmWebsocketResult; external websocketExportName name websocketFN_Send;

function __wasm_websocket_deallocate(
    aWebsocketID : TWasmWebSocketID) : TWasmWebsocketResult; external websocketExportName name websocketFN_DeAllocate;


Type
  TWasmWebsocketErrorCallback = procedure(aWebSocketID : TWasmWebSocketID; aUserData : Pointer);
  TWasmWebsocketMessageCallback = procedure(aWebSocketID : TWasmWebSocketID; aUserData : Pointer; aMessageType : TWasmWebSocketMessageType; aMessage : TBytes);
  TWasmWebsocketCloseCallback = procedure(aWebSocketID : TWasmWebSocketID; aUserData : Pointer; aCode: Longint; const aReason : String; aClean : Boolean);
  TWasmWebsocketOpenCallback = procedure(aWebSocketID : TWasmWebSocketID; aUserData : Pointer);
  TWasmWebsocketLogHook = procedure (Level : TWasmWebSocketLogLevel; const Msg : string) of object;

// Callee is responsible for freeing incoming buffers
Function __wasm_websocket_allocate_buffer(aWebsocketID : TWasmWebSocketID; aUserData : Pointer; aBufferLen : Longint) : Pointer;
Function __wasm_websocket_on_error (aWebsocketID : TWasmWebSocketID; aUserData : Pointer) : TWebsocketCallBackResult;
Function __wasm_websocket_on_message (aWebsocketID : TWasmWebSocketID; aUserData : Pointer; aMessageType : TWasmWebSocketMessageType; aMessage : Pointer; aMessageLen : Integer) : TWebsocketCallBackResult;
Function __wasm_websocket_on_open (aWebsocketID : TWasmWebSocketID; aUserData : Pointer) : TWebsocketCallBackResult;
Function __wasm_websocket_on_close (aWebsocketID : TWasmWebSocketID; aUserData : Pointer; aCode: Longint; aReason : PByte; aReasonLen : Longint; aClean : Longint) : TWebsocketCallBackResult;


procedure __wasmwebsocket_log(level : TWasmLogLevel; const Msg : String);
procedure __wasmwebsocket_log(level : TWasmLogLevel; const Fmt : String; const Args : Array of const);

var
  WebSocketLogEnabled : Boolean;
  WebSocketErrorCallback : TWasmWebsocketErrorCallback;
  WebSocketMessageCallback : TWasmWebsocketMessageCallback;
  WebSocketCloseCallback : TWasmWebsocketCloseCallback;
  WebSocketOpenCallback : TWasmWebsocketOpenCallback;

implementation

procedure __wasmwebsocket_log(level : TWasmWebSocketLogLevel; const Msg : String);

begin
  if not WebSocketLogEnabled then
    exit;
  __wasm_log(level,'websocket',msg);
end;

procedure __wasmwebsocket_log(level : TWasmWebSocketLogLevel; const Fmt : String; const Args : Array of const);

begin
  if not WebSocketLogEnabled then
    exit;
  __wasm_log(level,'websocket',Fmt,Args);
end;


Function __wasm_websocket_allocate_buffer(aWebsocketID : TWasmWebSocketID; aUserData : Pointer; aBufferLen : Longint) : Pointer;

begin
    // Silence compiler warning
  if (aWebSocketID=0) or (aUserData=Nil) then ;
  Result:=GetMem(aBufferLen);
end;

procedure LogError(const aOperation : String; aError : Exception);

begin
  __wasmwebsocket_log(wllError,SafeFormat('Error %s during %s callback: %s',[aError.ClassName,aOperation,aError.Message]));
end;

Function __wasm_websocket_on_error (aWebsocketID : TWasmWebSocketID; aUserData : Pointer) : TWebsocketCallBackResult;

begin
  if not assigned(WebSocketErrorCallback) then
    Exit(WASMWS_CALLBACK_NOHANDLER);
  try
    WebsocketErrorCallBack(aWebsocketID,aUserData);
    Result:=WASMWS_CALLBACK_SUCCESS;
  except
    On E : exception do
      begin
      LogError('error',E);
      Result:=WASMWS_CALLBACK_ERROR;
      end;
  end;
end;

Function __wasm_websocket_on_message (aWebsocketID : TWasmWebSocketID; aUserData : Pointer; aMessageType : TWasmWebSocketMessageType; aMessage : Pointer; aMessageLen : Integer) : TWebsocketCallBackResult;

var
  Buf : TBytes;

begin
  Buf:=[];
  try
    if not assigned(WebSocketMessageCallback) then
      Exit(WASMWS_CALLBACK_NOHANDLER);
    try
      SetLength(Buf,aMessageLen);
      if aMessageLen>0 then
        Move(aMessage^,Buf[0],aMessageLen);
      WebsocketMessageCallBack(aWebsocketID,aUserData,aMessageType,Buf);
      Result:=WASMWS_CALLBACK_SUCCESS;
    except
      On E : exception do
        begin
        LogError('message',E);
        Result:=WASMWS_CALLBACK_ERROR;
        end;
    end;
  finally
    FreeMem(aMessage);
  end;
end;

Function __wasm_websocket_on_open (aWebsocketID : TWasmWebSocketID; aUserData : Pointer) : TWebsocketCallBackResult;

begin
  if not assigned(WebSocketOpenCallback) then
    Exit(WASMWS_CALLBACK_NOHANDLER);
  try
    WebsocketOpenCallBack(aWebsocketID,aUserData);
    Result:=WASMWS_CALLBACK_SUCCESS;
  except
    On E : exception do
      begin
      LogError('message',E);
      Result:=WASMWS_CALLBACK_ERROR;
      end;
  end;
end;

Function __wasm_websocket_on_close (aWebsocketID : TWasmWebSocketID; aUserData : Pointer; aCode: Longint; aReason : PByte; aReasonLen : Longint; aClean : Longint) : TWebsocketCallBackResult;

var
  lReason : String;
  Buf : TBytes;
  lClean : Boolean;

begin
  Buf:=[];
  try
    if not assigned(WebSocketCloseCallback) then
      Exit(WASMWS_CALLBACK_NOHANDLER);
    try
      lClean:=(aClean=0);
      SetLength(Buf,aReasonLen);
      Move(aReason^,Buf[0],aReasonLen);
      {$IF SIZEOF(CHAR)=1}
      lReason:=TEncoding.UTF8.GetAnsiString(Buf);
      {$ELSE}
      lReason:=TEncoding.UTF8.GetString(Buf);
      {$ENDIF}
      WebsocketCloseCallBack(aWebsocketID,aUserData,aCode,lReason,lClean);
      Result:=WASMWS_CALLBACK_SUCCESS;
    except
      On E : exception do
        begin
        LogError('message',E);
        Result:=WASMWS_CALLBACK_ERROR;
        end;
    end;
  finally
    FreeMem(aReason);
  end;
end;

exports
  __wasm_websocket_allocate_buffer,
  __wasm_websocket_on_error,
  __wasm_websocket_on_message,
  __wasm_websocket_on_open,
  __wasm_websocket_on_close;


end.

