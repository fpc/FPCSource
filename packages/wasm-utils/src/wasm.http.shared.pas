{
    This file is part of the Free Component Library

    Webassembly HTTP API - shared constants 
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit wasm.http.shared;

{$mode ObjFPC}{$H+}

interface

Const
  WASMHTTP_CACHE_DEFAULT        = 0;
  WASMHTTP_CACHE_NO_STORE       = 1;
  WASMHTTP_CACHE_RELOAD         = 2;
  WASMHTTP_CACHE_NO_CACHE       = 3;
  WASMHTTP_CACHE_FORCE_CACHE    = 4;
  WASMHTTP_CACHE_ONLY_IF_CACHED = 5;

  WASMHTTP_CREDENTIALS_SAME_ORIGIN = 0;
  WASMHTTP_CREDENTIALS_OMIT        = 1;
  WASMHTTP_CREDENTIALS_INCLUDE     = 2;

  WASMHTTP_MODE_CORS        = 0;
  WASMHTTP_MODE_SAME_ORIGIN = 1;
  WASMHTTP_MODE_NO_CORS     = 2;
  WASMHTTP_MODE_NAVIGATE    = 3;
  WASMHTTP_MODE_WEBSOCKET   = 4;

  WASMHTTP_PRIORITY_AUTO    = 0;
  WASMHTTP_PRIORITY_LOW     = 1;
  WASMHTTP_PRIORITY_HIGH    = 2;

  WASMHTTP_REDIRECT_FOLLOW  = 0;
  WASMHTTP_REDIRECT_ERROR   = 1;
  WASMHTTP_REDIRECT_MANUAL  = 2;

  WASMHTTP_ABORTSIGNAL_NO  = 0;
  WASMHTTP_ABORTSIGNAL_YES = 1;

  WASMHTTP_RESULT_SUCCESS         = 0;
  WASMHTTP_RESULT_ERROR           = -1;
  WASMHTTP_RESULT_NO_URL          = -2;
  WASMHTTP_RESULT_INVALIDID       = -3;
  WASMHTTP_RESULT_INPROGRESS      = -4;
  WASMHTTP_RESULT_INSUFFICIENTMEM = -5;

  WASMHTTP_RESPONSE_SUCCESS    = 0;
  WASMHTTP_RESPONSE_DEALLOCATE = 1;
  WASMHTTP_RESPONSE_ERROR      = -1;

Type
  TWasmHTTPRequestID = Longint;
  {$IFDEF PAS2JS}
  TWasmPointer = Longint;
  PWasmHTTPRequestID = TWasmPointer;
  {$ELSE}
  PWasmHTTPRequestID = ^TWasmHTTPRequestID;
  {$ENDIF}

  TWasmHTTPResult = Longint;
  TWasmHTTPResponseResult = Longint;
  TWasmHTTPResponseStatus = Longint;
  TWasmHTTPResponseInfoType = Longint;

const
  httpExportName = 'http';
  httpFN_RequestAllocate = 'request_allocate';
  httpFN_RequestExecute  = 'request_execute';
  httpFN_RequestDeAllocate = 'request_deallocate';
  httpFN_RequestAbort = 'request_abort';
  httpFN_ResponseGetStatus = 'response_get_status';
  httpFN_ResponseGetStatusText = 'response_get_status_text';
  httpFN_ResponseGetHeaderName = 'response_get_header_name';
  httpFN_ResponseGetHeaderCount = 'response_get_header_count';
  httpFN_ResponseGetHeader = 'response_get_header';
  httpFN_ResponseGetBody = 'response_get_body';
  httpFN_ResponseCallback = '__wasmhttp_response_callback';

implementation

end.

