{
    This file is part of the Free Component Library

    Webassembly Websocket API - Definitions shared with host implementation.
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wasm.websocket.shared;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils;
  {$ELSE}
  sysutils;
  {$ENDIF}

Type
  TWasmWebsocketResult = longint;
  TWasmWebsocketID = longint;
  TWasmWebSocketMessageType = Longint;
  TWebsocketCallBackResult = Longint;

  {$IFNDEF PAS2JS}
  PWasmWebSocketID = ^TWasmWebsocketID;
  {$ELSE}
  TWasmPointer = longint;

  PByte = TWasmPointer;
  PWasmWebSocketID = TWasmPointer;
  {$endif}

Const
  WASMWS_RESULT_SUCCESS   = 0;
  WASMWS_RESULT_ERROR     = -1;
  WASMWS_RESULT_NO_URL    = -2;
  WASMWS_RESULT_INVALIDID = -3;
  WASMWS_RESULT_FAILEDLOCK  = -4;
  WASMWS_RESULT_INVALIDSIZE = -5;
  WASMWS_RESULT_NOSHAREDMEM = -6;
  WASMWS_RESULT_DUPLICATEID = -7;

  WASMWS_CALLBACK_SUCCESS   = 0;
  WASMWS_CALLBACK_NOHANDLER = -1;
  WASMWS_CALLBACK_ERROR     = -2;

  WASMWS_MESSAGE_TYPE_TEXT   = 0;
  WASMWS_MESSAGE_TYPE_BINARY = 1;

const
  websocketExportName  = 'websocket';
  websocketFN_Allocate = 'allocate_websocket';
  websocketFN_DeAllocate = 'deallocate_websocket';
  websocketFN_close = 'close_websocket';
  websocketFN_send = 'send_websocket';


implementation

end.

