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
  xmlInputMatchCallback = function(filename: pchar): cint; EXTDECL;

(**
 * xmlInputOpenCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Input API to open the resource
 *
 * Returns an Input context or NULL in case or error
 *)
  xmlInputOpenCallback = function(filename: pchar): pointer; EXTDECL;

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
  xmlInputReadCallback = function(context: pointer; buffer: pchar; len: cint): cint; EXTDECL;

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
  xmlOutputMatchCallback = function(filename: pchar): cint; EXTDECL;

(**
 * xmlOutputOpenCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Output API to open the resource
 *
 * Returns an Output context or NULL in case or error
 *)
  xmlOutputOpenCallback = function(filename: pchar): pointer; EXTDECL;

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
  xmlOutputWriteCallback = function(context: pointer; buffer: pchar; len: cint): cint; EXTDECL;

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
function xmlParserInputBufferCreateFilename(URI: pchar; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL; external xml2lib;
function xmlParserInputBufferCreateFile(fp: PFILE; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL; external xml2lib;
function xmlParserInputBufferCreateFd(fd: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL; external xml2lib;
function xmlParserInputBufferCreateMem(mem: pchar; size: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL; external xml2lib;
function xmlParserInputBufferCreateStatic(mem: pchar; size: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL; external xml2lib;

function xmlParserInputBufferCreateIO(ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL; external xml2lib;
function xmlParserInputBufferRead(_in: xmlParserInputBufferPtr; len: cint): cint; EXTDECL; external xml2lib;
function xmlParserInputBufferGrow(_in: xmlParserInputBufferPtr; len: cint): cint; EXTDECL; external xml2lib;
function xmlParserInputBufferPush(_in: xmlParserInputBufferPtr; len: cint; buf: pchar): cint; EXTDECL; external xml2lib;
procedure xmlFreeParserInputBuffer(_in: xmlParserInputBufferPtr); EXTDECL; external xml2lib;
function xmlParserGetDirectory(filename: pchar): pchar; EXTDECL; external xml2lib;
function xmlRegisterInputCallbacks(matchFunc: xmlInputMatchCallback; openFunc: xmlInputOpenCallback; readFunc: xmlInputReadCallback; closeFunc: xmlInputCloseCallback): cint; EXTDECL; external xml2lib;
function __xmlParserInputBufferCreateFilename(URI: pchar; enc: xmlCharEncoding): xmlParserInputBufferPtr; EXTDECL; external xml2lib;

{$IFDEF LIBXML_OUTPUT_ENABLED}
(*
 * Interfaces for output
 *)
procedure xmlCleanupOutputCallbacks; EXTDECL; external xml2lib;
procedure xmlRegisterDefaultOutputCallbacks; EXTDECL; external xml2lib;
function xmlAllocOutputBuffer(encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; EXTDECL; external xml2lib;

function xmlOutputBufferCreateFilename(URI: pchar; encoder: xmlCharEncodingHandlerPtr; compression: cint): xmlOutputBufferPtr; EXTDECL; external xml2lib;
function xmlOutputBufferCreateFile(fp: PFILE; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; EXTDECL; external xml2lib;
function xmlOutputBufferCreateBuffer(buffer: xmlBufferPtr; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; EXTDECL; external xml2lib;
function xmlOutputBufferCreateFd(fd: cint; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; EXTDECL; external xml2lib;
function xmlOutputBufferCreateIO(iowrite: xmlOutputWriteCallback; ioclose: xmlOutputCloseCallback; ioctx: pointer; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; EXTDECL; external xml2lib;
function xmlOutputBufferWrite(_out: xmlOutputBufferPtr; len: cint; buf: pchar): cint; EXTDECL; external xml2lib;
function xmlOutputBufferWriteString(_out: xmlOutputBufferPtr; str: pchar): cint; EXTDECL; external xml2lib;
function xmlOutputBufferWriteEscape(_out: xmlOutputBufferPtr; str: xmlCharPtr; escaping: xmlCharEncodingOutputFunc): cint; EXTDECL; external xml2lib;
function xmlOutputBufferFlush(_out: xmlOutputBufferPtr): cint; EXTDECL; external xml2lib;
function xmlOutputBufferClose(_out: xmlOutputBufferPtr): cint; EXTDECL; external xml2lib;
function xmlRegisterOutputCallbacks(matchFunc: xmlOutputMatchCallback; openFunc: xmlOutputOpenCallback; writeFunc: xmlOutputWriteCallback; closeFunc: xmlOutputCloseCallback): cint; EXTDECL; external xml2lib;
function __xmlOutputBufferCreateFilename(URI: pchar; encoder: xmlCharEncodingHandlerPtr; compression: cint): xmlOutputBufferPtr; EXTDECL; external xml2lib;

{$IFDEF LIBXML_HTTP_ENABLED}
(*  This function only exists if HTTP support built into the library  *)
procedure xmlRegisterHTTPPostCallbacks; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_HTTP_ENABLED *)
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

function xmlCheckHTTPInput(ctxt: xmlParserCtxtPtr; ret: xmlParserInputPtr): xmlParserInputPtr; EXTDECL; external xml2lib;

(*
 * A predefined entity loader disabling network accesses
 *)
function xmlNoNetExternalEntityLoader(URL: pchar; ID: pchar; ctxt: xmlParserCtxtPtr): xmlParserInputPtr; EXTDECL; external xml2lib;

(*
 * Check xmlCanonicPath in uri.h for a better alternative.
 *)
function xmlCheckFilename(path: pchar): cint; EXTDECL; external xml2lib;

(**
 * Default 'file://' protocol callbacks
 *)
function xmlFileMatch(filename: pchar): cint; EXTDECL; external xml2lib;
function xmlFileOpen(filename: pchar): pointer; EXTDECL; external xml2lib;
function xmlFileRead(context: pointer; buffer: pchar; len: cint): cint; EXTDECL; external xml2lib;
function xmlFileClose(context: pointer): cint; EXTDECL; external xml2lib;

(**
 * Default 'http://' protocol callbacks
 *)
{$IFDEF LIBXML_HTTP_ENABLED}
function xmlIOHTTPMatch(filename: pchar): cint; EXTDECL; external xml2lib;
function xmlIOHTTPOpen(filename: pchar): pointer; EXTDECL; external xml2lib;
{$IFDEF LIBXML_OUTPUT_ENABLED}
function xmlIOHTTPOpenW(post_uri: pchar; compression: cint): pointer; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
function xmlIOHTTPRead(context: pointer; buffer: pchar; len: cint): cint; EXTDECL; external xml2lib;
function xmlIOHTTPClose(context: pointer): cint; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_HTTP_ENABLED *)

(**
 * Default 'ftp://' protocol callbacks
 *)
{$IFDEF LIBXML_FTP_ENABLED}
function xmlIOFTPMatch(filename: pchar): cint; EXTDECL; external xml2lib;
function xmlIOFTPOpen(filename: pchar): pointer; EXTDECL; external xml2lib;
function xmlIOFTPRead(context: pointer; buffer: pchar; len: cint): cint; EXTDECL; external xml2lib;
function xmlIOFTPClose(context: pointer): cint; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_FTP_ENABLED *)
{$ENDIF}
