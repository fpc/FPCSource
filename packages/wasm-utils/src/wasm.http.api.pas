{
    This file is part of the Free Component Library

    Webassembly HTTP API - imported functions and structures.
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit wasm.http.api;

{$mode ObjFPC}{$H+}

interface

uses wasm.http.shared, wasm.logger.api;

Type
  TWasmHTTPLogLevel = TWasmLogLevel;
  TWasmHTTPLogLevels = TWasmLogLevels;

const
  hllTrace    = wllTrace;
  hllDebug    = wllDebug;
  hllInfo     = wllInfo;
  hllWarning  = wllWarning;
  hllError    = wllError;
  hllCritical = wllCritical;

Type
  TWasmString = record
    Data : PAnsiChar;
    Len : Longint;
  end;
  PWasmString = ^TWasmString;

  TWasmBuffer = record
    Data : PByte;
    Len : Longint;
  end;
  PWasmBuffer = ^TWasmBuffer;

  TWasmHTTPApiRequest = Record
    Url : TWasmString;
    Method : TWasmString;
    HeaderCount : Longint;
    Headers : PWasmString;
    Body : TWasmBuffer;
    Integrity : TWasmString;
    Redirect : Longint;
    Cache : Longint;
    KeepAlive : Longint;
    Mode : Longint;
    Priority : Longint;
    Referrer : TWasmString;
    ReferrerPolicy : TWasmString;
    AbortSignal : Longint;
    Credentials: Longint;
  end;
  PWasmHTTPAPIRequest = ^TWasmHTTPApiRequest;

  TWasmHTTPResponseEvent    = procedure(aRequestID : Longint; aUserData : Pointer; aStatus : TWasmHTTPResponseStatus; var Deallocate : Boolean) of object;
  TWasmHTTPResponseCallback = procedure(aRequestID : Longint; aUserData : Pointer; aStatus : TWasmHTTPResponseStatus; var Deallocate : Boolean);
  TWasmHTTPLogHook = TWasmLogHook;

function __wasmhttp_request_allocate(aRequest : PWasmHTTPAPIRequest; aUserData : Pointer; aRequestID : PWasmHTTPRequestID) : TWasmHTTPResult; external httpExportName name httpFN_RequestAllocate;
function __wasmhttp_request_execute(aRequestID : TWasmHTTPRequestID) : TWasmHTTPResult; external httpExportName name httpFN_RequestExecute;
function __wasmhttp_request_deallocate(aRequestID : TWasmHTTPRequestID) : TWasmHTTPResult; external httpExportName name httpFN_RequestDeAllocate;
function __wasmhttp_request_abort(aRequestID : TWasmHTTPRequestID) : TWasmHTTPResult; external httpExportName name httpFN_RequestAbort;

function __wasmhttp_response_get_status(aRequestID : TWasmHTTPRequestID; aStatus : PLongint) : TWasmHTTPResult; external httpExportName name httpFN_ResponseGetStatus;
function __wasmhttp_response_get_statustext(aRequestID : TWasmHTTPRequestID; aStatusText : PByte; aMaxHeaderTextLen : PLongint) : TWasmHTTPResult; external httpExportName name httpFN_ResponseGetStatusText;
function __wasmhttp_response_get_headercount(aRequestID : TWasmHTTPRequestID; var aHeaderCount : Longint) : TWasmHTTPResult; external httpExportName name httpFN_ResponseGetHeaderCount;
function __wasmhttp_response_get_headername(aRequestID : TWasmHTTPRequestID; aHeaderIdx: Longint; aHeader : PByte; aMaxHeaderLen : PLongint) : TWasmHTTPResult; external httpExportName name httpFN_ResponseGetHeaderName;
function __wasmhttp_response_get_header(aRequestID : TWasmHTTPRequestID; aHeaderName: PByte; aHeaderLen : Longint; aHeader : PByte; aMaxHeaderLen : PLongint) : TWasmHTTPResult; external httpExportName name httpFN_ResponseGetHeader;
function __wasmhttp_response_get_body(aRequestID : TWasmHTTPRequestID; aBody : PByte; MaxBodyLen : PLongint) : TWasmHTTPResult; external httpExportName name httpFN_ResponseGetBody;

function __wasmhttp_response_callback(aRequestID : TWasmHTTPRequestID; aUserData : Pointer; aStatus : TWasmHTTPResponseStatus) : TWasmHTTPResponseResult;


procedure __wasmhttp_log(level : TWasmHTTPLogLevel; const Msg : String);
procedure __wasmhttp_log(level : TWasmHTTPLogLevel; const Fmt : String; const Args : Array of const);

var
  OnWasmHTTPResponse : TWasmHTTPResponseEvent;
  WasmHTTPResponseCallback : TWasmHTTPResponseCallback;
  EnableWasmHTTPLog : Boolean;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils;
{$ELSE}
uses sysutils;
{$ENDIF}

procedure __wasmhttp_log(level : TWasmHTTPLogLevel; const Msg : String);

begin
  if not EnableWasmHTTPLog then
    exit;
  __wasm_log(level,'HTTP',Msg);
end;

procedure __wasmhttp_log(level : TWasmHTTPLogLevel; const Fmt : String; const Args : Array of const);

begin
  if not EnableWasmHTTPLog then
    exit;
  __wasm_log(level,'HTTP',Fmt,Args);
end;

function __wasmhttp_response_callback(aRequestID : TWasmHTTPRequestID; aUserData : Pointer; aStatus : TWasmHTTPResponseStatus) : TWasmHTTPResponseResult;

var
  B : Boolean;

begin
  B:=True;
  try
    if Assigned(OnWasmHTTPResponse) then
      OnWasmHTTPResponse(aRequestID,aUSerData,aStatus,B)
    else if Assigned(WasmHTTPResponseCallback) then
      WasmHTTPResponseCallback(aRequestID,aUSerData,aStatus,B);
    if B then
      Result:=WASMHTTP_RESPONSE_DEALLOCATE
    else
      Result:=WASMHTTP_RESPONSE_SUCCESS;
  except
    on E : exception do
      begin
      __wasmhttp_log(hllError,'Exception %s during response callback: %s',[E.ClassName,E.Message]);
      Result:=WASMHTTP_RESPONSE_ERROR;
      end;
  end;
end;

exports __wasmhttp_response_callback;

end.

