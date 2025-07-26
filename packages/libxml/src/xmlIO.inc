(*
 * Summary: interface for the I/O interfaces used by the parser
 * Description: interface for the I/O interfaces used by the parser
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

(*
 * Those are the functions and datatypes for the parser input
 * I/O structures.
 *)

{$IFDEF POINTER}
  xmlParserInputBufferPtr = ^xmlParserInputBuffer;
  xmlOutputBufferPtr = ^xmlOutputBuffer;
{$ENDIF}

{$IFDEF TYPE}
(**
 * xmlInputMatchCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Input API to detect if the current handler
 * can provide input fonctionnalities for this resource.
 *
 * Returns 1 if yes and 0 if another Input module should be used
 *)
  xmlInputMatchCallback = function(filename: PAnsiChar): cint; EXTDECL;

(**
 * xmlInputOpenCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Input API to open the resource
 *
 * Returns an Input context or NULL in case or error
 *)
  xmlInputOpenCallback = function(filename: PAnsiChar): pointer; EXTDECL;

(**
 * xmlInputReadCallback:
 * @context:  an Input context
 * @buffer:  the buffer to store data read
 * @len:  the length of the buffer in bytes
 *
 * Callback used in the I/O Input API to read the resource
 *
 * Returns the number of bytes read or -1 in case of error
 *)
  xmlInputReadCallback = function(context: pointer; buffer: PAnsiChar; len: cint): cint; EXTDECL;

(**
 * xmlInputCloseCallback:
 * @context:  an Input context
 *
 * Callback used in the I/O Input API to close the resource
 *
 * Returns 0 or -1 in case of error
 *)
  xmlInputCloseCallback = function(context: pointer): cint; EXTDECL;

{$IFDEF LIBXML_OUTPUT_ENABLED}
(*
 * Those are the functions and datatypes for the library output
 * I/O structures.
 *)

(**
 * xmlOutputMatchCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Output API to detect if the current handler
 * can provide output fonctionnalities for this resource.
 *
 * Returns 1 if yes and 0 if another Output module should be used
 *)
  xmlOutputMatchCallback = function(filename: PAnsiChar): cint; EXTDECL;

(**
 * xmlOutputOpenCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Output API to open the resource
 *
 * Returns an Output context or NULL in case or error
 *)
  xmlOutputOpenCallback = function(filename: PAnsiChar): pointer; EXTDECL;

(**
 * xmlOutputWriteCallback:
 * @context:  an Output context
 * @buffer:  the buffer of data to write
 * @len:  the length of the buffer in bytes
 *
 * Callback used in the I/O Output API to write to the resource
 *
 * Returns the number of bytes written or -1 in case of error
 *)
  xmlOutputWriteCallback = function(context: pointer; buffer: PAnsiChar; len: cint): cint; EXTDECL;

(**
 * xmlOutputCloseCallback:
 * @context:  an Output context
 *
 * Callback used in the I/O Output API to close the resource
 *
 * Returns 0 or -1 in case of error
 *)
  xmlOutputCloseCallback = function(context: pointer): cint; EXTDECL;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

  xmlParserInputBuffer = record
    context       : pointer;
    readcallback  : xmlInputReadCallback;
    closecallback : xmlInputCloseCallback;
    encoder       : xmlCharEncodingHandlerPtr; (* I18N conversions to UTF-8 *)
    buffer        : xmlBufferPtr; (* Local buffer encoded in UTF-8 *)
    raw           : xmlBufferPtr; (* if encoder != NULL buffer for raw input *)
    compressed    : cint; (* -1=unknown, 0=not compressed, 1=compressed *)
    error         : cint;
    rawconsumed   : culong; (* amount consumed from raw *)
  end;

{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlOutputBuffer = record
    context       : pointer;
    writecallback : xmlOutputWriteCallback;
    closecallback : xmlOutputCloseCallback;
    encoder       : xmlCharEncodingHandlerPtr; (* I18N conversions to UTF-8 *)
    buffer        : xmlBufferPtr; (* Local buffer encoded in UTF-8 or ISOLatin *)
    conv          : xmlBufferPtr; (* if encoder != NULL buffer for output *)
    written       : cint; (* total number of byte written *)
    error         : cint;
  end;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Interfaces for input
 *)
procedure xmlCleanupInputCallbacks; EXTDECL; external xml2lib;
function xmlPopInputCallbacks: cint; EXTDECL; external xml2lib;
procedure xmlRegisterDefaultInputCallbacks; EXTDECL; external xml2lib;
function xmlAllocParserInputBuffer(enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL; external xml2lib;
function xmlParserInputBufferCreateFilename(URI: PAnsiChar; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL; external xml2lib;
function xmlParserInputBufferCreateFile(fp: PFILE; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL; external xml2lib;
function xmlParserInputBufferCreateFd(fd: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL; external xml2lib;
function xmlParserInputBufferCreateMem(mem: PAnsiChar; size: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL; external xml2lib;
function xmlParserInputBufferCreateStatic(mem: PAnsiChar; size: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL; external xml2lib;

function xmlParserInputBufferCreateIO(ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL; external xml2lib;
function xmlParserInputBufferRead(_in: xmlParserInputBufferPtr; len: cint): cint; EXTDECL; external xml2lib;
function xmlParserInputBufferGrow(_in: xmlParserInputBufferPtr; len: cint): cint; EXTDECL; external xml2lib;
function xmlParserInputBufferPush(_in: xmlParserInputBufferPtr; len: cint; buf: PAnsiChar): cint; EXTDECL; external xml2lib;
procedure xmlFreeParserInputBuffer(_in: xmlParserInputBufferPtr); EXTDECL; external xml2lib;
function xmlParserGetDirectory(filename: PAnsiChar): PAnsiChar; EXTDECL; external xml2lib;
function xmlRegisterInputCallbacks(matchFunc: xmlInputMatchCallback; openFunc: xmlInputOpenCallback; readFunc: xmlInputReadCallback; closeFunc: xmlInputCloseCallback): cint; EXTDECL; external xml2lib;
function __xmlParserInputBufferCreateFilename(URI: PAnsiChar; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL; external xml2lib;

{$IFDEF LIBXML_OUTPUT_ENABLED}
(*
 * Interfaces for output
 *)
procedure xmlCleanupOutputCallbacks; EXTDECL; external xml2lib;
procedure xmlRegisterDefaultOutputCallbacks; EXTDECL; external xml2lib;
function xmlAllocOutputBuffer(encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; EXTDECL; external xml2lib;

function xmlOutputBufferCreateFilename(URI: PAnsiChar; encoder: xmlCharEncodingHandlerPtr; compression: cint): xmlOutputBufferPtr; EXTDECL; external xml2lib;
function xmlOutputBufferCreateFile(fp: PFILE; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; EXTDECL; external xml2lib;
function xmlOutputBufferCreateBuffer(buffer: xmlBufferPtr; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; EXTDECL; external xml2lib;
function xmlOutputBufferCreateFd(fd: cint; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; EXTDECL; external xml2lib;
function xmlOutputBufferCreateIO(iowrite: xmlOutputWriteCallback; ioclose: xmlOutputCloseCallback; ioctx: pointer; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; EXTDECL; external xml2lib;
function xmlOutputBufferGetContent(_out: xmlOutputBufferPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlOutputBufferGetSize(_out: xmlOutputBufferPtr): csize_t; EXTDECL; external xml2lib;
function xmlOutputBufferWrite(_out: xmlOutputBufferPtr; len: cint; buf: PAnsiChar): cint; EXTDECL; external xml2lib;
function xmlOutputBufferWriteString(_out: xmlOutputBufferPtr; str: PAnsiChar): cint; EXTDECL; external xml2lib;
function xmlOutputBufferWriteEscape(_out: xmlOutputBufferPtr; str: xmlCharPtr; escaping: xmlCharEncodingOutputFunc): cint; EXTDECL; external xml2lib;
function xmlOutputBufferFlush(_out: xmlOutputBufferPtr): cint; EXTDECL; external xml2lib;
function xmlOutputBufferClose(_out: xmlOutputBufferPtr): cint; EXTDECL; external xml2lib;
function xmlRegisterOutputCallbacks(matchFunc: xmlOutputMatchCallback; openFunc: xmlOutputOpenCallback; writeFunc: xmlOutputWriteCallback; closeFunc: xmlOutputCloseCallback): cint; EXTDECL; external xml2lib;
function __xmlOutputBufferCreateFilename(URI: PAnsiChar; encoder: xmlCharEncodingHandlerPtr; compression: cint): xmlOutputBufferPtr; EXTDECL; external xml2lib;

{$IFDEF LIBXML_HTTP_ENABLED}
(*  This function only exists if HTTP support built into the library  *)
procedure xmlRegisterHTTPPostCallbacks; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_HTTP_ENABLED *)
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

function xmlCheckHTTPInput(ctxt: xmlParserCtxtPtr; ret: xmlParserInputPtr): xmlParserInputPtr; EXTDECL; external xml2lib;

(*
 * A predefined entity loader disabling network accesses
 *)
function xmlNoNetExternalEntityLoader(URL: PAnsiChar; ID: PAnsiChar; ctxt: xmlParserCtxtPtr): xmlParserInputPtr; EXTDECL; external xml2lib;

(*
 * Check xmlCanonicPath in uri.h for a better alternative.
 *)
function xmlCheckFilename(path: PAnsiChar): cint; EXTDECL; external xml2lib;

(**
 * Default 'file://' protocol callbacks
 *)
function xmlFileMatch(filename: PAnsiChar): cint; EXTDECL; external xml2lib;
function xmlFileOpen(filename: PAnsiChar): pointer; EXTDECL; external xml2lib;
function xmlFileRead(context: pointer; buffer: PAnsiChar; len: cint): cint; EXTDECL; external xml2lib;
function xmlFileClose(context: pointer): cint; EXTDECL; external xml2lib;

(**
 * Default 'http://' protocol callbacks
 *)
{$IFDEF LIBXML_HTTP_ENABLED}
function xmlIOHTTPMatch(filename: PAnsiChar): cint; EXTDECL; external xml2lib;
function xmlIOHTTPOpen(filename: PAnsiChar): pointer; EXTDECL; external xml2lib;
{$IFDEF LIBXML_OUTPUT_ENABLED}
function xmlIOHTTPOpenW(post_uri: PAnsiChar; compression: cint): pointer; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
function xmlIOHTTPRead(context: pointer; buffer: PAnsiChar; len: cint): cint; EXTDECL; external xml2lib;
function xmlIOHTTPClose(context: pointer): cint; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_HTTP_ENABLED *)

(**
 * Default 'ftp://' protocol callbacks
 *)
{$IFDEF LIBXML_FTP_ENABLED}
function xmlIOFTPMatch(filename: PAnsiChar): cint; EXTDECL; external xml2lib;
function xmlIOFTPOpen(filename: PAnsiChar): pointer; EXTDECL; external xml2lib;
function xmlIOFTPRead(context: pointer; buffer: PAnsiChar; len: cint): cint; EXTDECL; external xml2lib;
function xmlIOFTPClose(context: pointer): cint; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_FTP_ENABLED *)
{$ENDIF}

{$IFDEF FUNCTIONVAR}
(*
 * Interfaces for input
 *)
  xmlCleanupInputCallbacks: procedure; EXTDECL;
  xmlPopInputCallbacks: function: cint; EXTDECL;
  xmlRegisterDefaultInputCallbacks: procedure; EXTDECL;
  xmlAllocParserInputBuffer: function(enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL;
  xmlParserInputBufferCreateFilename: function(URI: PAnsiChar; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL;
  xmlParserInputBufferCreateFile: function(fp: PFILE; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL;
  xmlParserInputBufferCreateFd: function(fd: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL;
  xmlParserInputBufferCreateMem: function(mem: PAnsiChar; size: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL;
  xmlParserInputBufferCreateStatic: function(mem: PAnsiChar; size: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL;

  xmlParserInputBufferCreateIO: function(ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL;
  xmlParserInputBufferRead: function(_in: xmlParserInputBufferPtr; len: cint): cint; EXTDECL;
  xmlParserInputBufferGrow: function(_in: xmlParserInputBufferPtr; len: cint): cint; EXTDECL;
  xmlParserInputBufferPush: function(_in: xmlParserInputBufferPtr; len: cint; buf: PAnsiChar): cint; EXTDECL;
  xmlFreeParserInputBuffer: procedure(_in: xmlParserInputBufferPtr); EXTDECL;
  xmlParserGetDirectory: function(filename: PAnsiChar): PAnsiChar; EXTDECL;
  xmlRegisterInputCallbacks: function(matchFunc: xmlInputMatchCallback; openFunc: xmlInputOpenCallback; readFunc: xmlInputReadCallback; closeFunc: xmlInputCloseCallback): cint; EXTDECL;
  __xmlParserInputBufferCreateFilename: function(URI: PAnsiChar; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL;

{$IFDEF LIBXML_OUTPUT_ENABLED}
(*
 * Interfaces for output
 *)
  xmlCleanupOutputCallbacks: procedure; EXTDECL;
  xmlRegisterDefaultOutputCallbacks: procedure; EXTDECL;
  xmlAllocOutputBuffer: function(encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; EXTDECL;

  xmlOutputBufferCreateFilename: function(URI: PAnsiChar; encoder: xmlCharEncodingHandlerPtr; compression: cint): xmlOutputBufferPtr; EXTDECL;
  xmlOutputBufferCreateFile: function(fp: PFILE; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; EXTDECL;
  xmlOutputBufferCreateBuffer: function(buffer: xmlBufferPtr; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; EXTDECL;
  xmlOutputBufferCreateFd: function(fd: cint; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; EXTDECL;
  xmlOutputBufferCreateIO: function(iowrite: xmlOutputWriteCallback; ioclose: xmlOutputCloseCallback; ioctx: pointer; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; EXTDECL;
  xmlOutputBufferGetContent: function(_out: xmlOutputBufferPtr): xmlCharPtr; EXTDECL;
  xmlOutputBufferGetSize: function(_out: xmlOutputBufferPtr): csize_t; EXTDECL;
  xmlOutputBufferWrite: function(_out: xmlOutputBufferPtr; len: cint; buf: PAnsiChar): cint; EXTDECL;
  xmlOutputBufferWriteString: function(_out: xmlOutputBufferPtr; str: PAnsiChar): cint; EXTDECL;
  xmlOutputBufferWriteEscape: function(_out: xmlOutputBufferPtr; str: xmlCharPtr; escaping: xmlCharEncodingOutputFunc): cint; EXTDECL;
  xmlOutputBufferFlush: function(_out: xmlOutputBufferPtr): cint; EXTDECL;
  xmlOutputBufferClose: function(_out: xmlOutputBufferPtr): cint; EXTDECL;
  xmlRegisterOutputCallbacks: function(matchFunc: xmlOutputMatchCallback; openFunc: xmlOutputOpenCallback; writeFunc: xmlOutputWriteCallback; closeFunc: xmlOutputCloseCallback): cint; EXTDECL;
  __xmlOutputBufferCreateFilename: function(URI: PAnsiChar; encoder: xmlCharEncodingHandlerPtr; compression: cint): xmlOutputBufferPtr; EXTDECL;

{$IFDEF LIBXML_HTTP_ENABLED}
(*  This function only exists if HTTP support built into the library  *)
  xmlRegisterHTTPPostCallbacks: procedure; EXTDECL;
{$ENDIF} (* LIBXML_HTTP_ENABLED *)
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

  xmlCheckHTTPInput: function(ctxt: xmlParserCtxtPtr; ret: xmlParserInputPtr): xmlParserInputPtr; EXTDECL;

(*
 * A predefined entity loader disabling network accesses
 *)
  xmlNoNetExternalEntityLoader: function(URL: PAnsiChar; ID: PAnsiChar; ctxt: xmlParserCtxtPtr): xmlParserInputPtr; EXTDECL;

(*
 * Check xmlCanonicPath in uri.h for a better alternative.
 *)
  xmlCheckFilename: function(path: PAnsiChar): cint; EXTDECL;

(**
 * Default 'file://' protocol callbacks
 *)
  xmlFileMatch: function(filename: PAnsiChar): cint; EXTDECL;
  xmlFileOpen: function(filename: PAnsiChar): pointer; EXTDECL;
  xmlFileRead: function(context: pointer; buffer: PAnsiChar; len: cint): cint; EXTDECL;
  xmlFileClose: function(context: pointer): cint; EXTDECL;

(**
 * Default 'http://' protocol callbacks
 *)
{$IFDEF LIBXML_HTTP_ENABLED}
  xmlIOHTTPMatch: function(filename: PAnsiChar): cint; EXTDECL;
  xmlIOHTTPOpen: function(filename: PAnsiChar): pointer; EXTDECL;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlIOHTTPOpenW: function(post_uri: PAnsiChar; compression: cint): pointer; EXTDECL;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  xmlIOHTTPRead: function(context: pointer; buffer: PAnsiChar; len: cint): cint; EXTDECL;
  xmlIOHTTPClose: function(context: pointer): cint; EXTDECL;
{$ENDIF} (* LIBXML_HTTP_ENABLED *)

(**
 * Default 'ftp://' protocol callbacks
 *)
{$IFDEF LIBXML_FTP_ENABLED}
  xmlIOFTPMatch: function(filename: PAnsiChar): cint; EXTDECL;
  xmlIOFTPOpen: function(filename: PAnsiChar): pointer; EXTDECL;
  xmlIOFTPRead: function(context: pointer; buffer: PAnsiChar; len: cint): cint; EXTDECL;
  xmlIOFTPClose: function(context: pointer): cint; EXTDECL;
{$ENDIF} (* LIBXML_FTP_ENABLED *)
{$ENDIF}
