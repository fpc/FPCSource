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
  xmlInputMatchCallback = function(filename: pchar): cint; XMLCALL;

(**
 * xmlInputOpenCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Input API to open the resource
 *
 * Returns an Input context or NULL in case or error
 *)
  xmlInputOpenCallback = function(filename: pchar): pointer; XMLCALL;

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
  xmlInputReadCallback = function(context: pointer; buffer: pchar; len: cint): cint; XMLCALL;

(**
 * xmlInputCloseCallback:
 * @context:  an Input context
 *
 * Callback used in the I/O Input API to close the resource
 *
 * Returns 0 or -1 in case of error
 *)
  xmlInputCloseCallback = function(context: pointer): cint; XMLCALL;

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
  xmlOutputMatchCallback = function(filename: pchar): cint; XMLCALL;

(**
 * xmlOutputOpenCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Output API to open the resource
 *
 * Returns an Output context or NULL in case or error
 *)
  xmlOutputOpenCallback = function(filename: pchar): pointer; XMLCALL;

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
  xmlOutputWriteCallback = function(context: pointer; buffer: pchar; len: cint): cint; XMLCALL;

(**
 * xmlOutputCloseCallback:
 * @context:  an Output context
 *
 * Callback used in the I/O Output API to close the resource
 *
 * Returns 0 or -1 in case of error
 *)
  xmlOutputCloseCallback = function(context: pointer): cint; XMLCALL;
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
procedure xmlCleanupInputCallbacks; XMLCALL; XMLPUBFUN;
function xmlPopInputCallbacks: cint; XMLCALL; XMLPUBFUN;
procedure xmlRegisterDefaultInputCallbacks; XMLCALL; XMLPUBFUN;
function xmlAllocParserInputBuffer(enc: xmlCharEncoding): xmlParserInputBufferPtr; XMLCALL; XMLPUBFUN;
function xmlParserInputBufferCreateFilename(URI: pchar; enc: xmlCharEncoding): xmlParserInputBufferPtr; XMLCALL; XMLPUBFUN;
//function xmlParserInputBufferCreateFile(file: FILE; enc: xmlCharEncoding): xmlParserInputBufferPtr; XMLCALL; XMLPUBFUN;
function xmlParserInputBufferCreateFd(fd: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; XMLCALL; XMLPUBFUN;
function xmlParserInputBufferCreateMem(mem: pchar; size: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; XMLCALL; XMLPUBFUN;
function xmlParserInputBufferCreateStatic(mem: pchar; size: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; XMLCALL; XMLPUBFUN;

function xmlParserInputBufferCreateIO(ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; enc: xmlCharEncoding): xmlParserInputBufferPtr; XMLCALL; XMLPUBFUN;
function xmlParserInputBufferRead(_in: xmlParserInputBufferPtr; len: cint): cint; XMLCALL; XMLPUBFUN;
function xmlParserInputBufferGrow(_in: xmlParserInputBufferPtr; len: cint): cint; XMLCALL; XMLPUBFUN;
function xmlParserInputBufferPush(_in: xmlParserInputBufferPtr; len: cint; buf: pchar): cint; XMLCALL; XMLPUBFUN;
procedure xmlFreeParserInputBuffer(_in: xmlParserInputBufferPtr); XMLCALL; XMLPUBFUN;
function xmlParserGetDirectory(filename: pchar): pchar; XMLCALL; XMLPUBFUN;
function xmlRegisterInputCallbacks(matchFunc: xmlInputMatchCallback; openFunc: xmlInputOpenCallback; readFunc: xmlInputReadCallback; closeFunc: xmlInputCloseCallback): cint; XMLCALL; XMLPUBFUN;
function __xmlParserInputBufferCreateFilename(URI: pchar; enc: xmlCharEncoding): xmlParserInputBufferPtr; XMLCALL; XMLPUBFUN;

{$IFDEF LIBXML_OUTPUT_ENABLED}
(*
 * Interfaces for output
 *)
procedure xmlCleanupOutputCallbacks; XMLCALL; XMLPUBFUN;
procedure xmlRegisterDefaultOutputCallbacks; XMLCALL; XMLPUBFUN;
function xmlAllocOutputBuffer(encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; XMLCALL; XMLPUBFUN;

function xmlOutputBufferCreateFilename(URI: pchar; encoder: xmlCharEncodingHandlerPtr; compression: cint): xmlOutputBufferPtr; XMLCALL; XMLPUBFUN;
//function xmlOutputBufferCreateFile(file: FILE; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; XMLCALL; XMLPUBFUN;
function xmlOutputBufferCreateBuffer(buffer: xmlBufferPtr; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; XMLCALL; XMLPUBFUN;
function xmlOutputBufferCreateFd(fd: cint; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; XMLCALL; XMLPUBFUN;
function xmlOutputBufferCreateIO(iowrite: xmlOutputWriteCallback; ioclose: xmlOutputCloseCallback; ioctx: pointer; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; XMLCALL; XMLPUBFUN;
function xmlOutputBufferWrite(_out: xmlOutputBufferPtr; len: cint; buf: pchar): cint; XMLCALL; XMLPUBFUN;
function xmlOutputBufferWriteString(_out: xmlOutputBufferPtr; str: pchar): cint; XMLCALL; XMLPUBFUN;
function xmlOutputBufferWriteEscape(_out: xmlOutputBufferPtr; str: xmlCharPtr; escaping: xmlCharEncodingOutputFunc): cint; XMLCALL; XMLPUBFUN;
function xmlOutputBufferFlush(_out: xmlOutputBufferPtr): cint; XMLCALL; XMLPUBFUN;
function xmlOutputBufferClose(_out: xmlOutputBufferPtr): cint; XMLCALL; XMLPUBFUN;
function xmlRegisterOutputCallbacks(matchFunc: xmlOutputMatchCallback; openFunc: xmlOutputOpenCallback; writeFunc: xmlOutputWriteCallback; closeFunc: xmlOutputCloseCallback): cint; XMLCALL; XMLPUBFUN;
function __xmlOutputBufferCreateFilename(URI: pchar; encoder: xmlCharEncodingHandlerPtr; compression: cint): xmlOutputBufferPtr; XMLCALL; XMLPUBFUN;

{$IFDEF LIBXML_HTTP_ENABLED}
(*  This function only exists if HTTP support built into the library  *)
procedure xmlRegisterHTTPPostCallbacks; XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_HTTP_ENABLED *)
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

function xmlCheckHTTPInput(ctxt: xmlParserCtxtPtr; ret: xmlParserInputPtr): xmlParserInputPtr; XMLCALL; XMLPUBFUN;

(*
 * A predefined entity loader disabling network accesses
 *)
function xmlNoNetExternalEntityLoader(URL: pchar; ID: pchar; ctxt: xmlParserCtxtPtr): xmlParserInputPtr; XMLCALL; XMLPUBFUN;

(*
 * xmlNormalizeWindowsPath is obsolete, don't use it.
 * Check xmlCanonicPath in uri.h for a better alternative.
 *)
function xmlNormalizeWindowsPath(path: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlCheckFilename(path: pchar): cint; XMLCALL; XMLPUBFUN;

(**
 * Default 'file://' protocol callbacks
 *)
function xmlFileMatch(filename: pchar): cint; XMLCALL; XMLPUBFUN;
function xmlFileOpen(filename: pchar): pointer; XMLCALL; XMLPUBFUN;
function xmlFileRead(context: pointer; buffer: pchar; len: cint): cint; XMLCALL; XMLPUBFUN;
function xmlFileClose(context: pointer): cint; XMLCALL; XMLPUBFUN;

(**
 * Default 'http://' protocol callbacks
 *)
{$IFDEF LIBXML_HTTP_ENABLED}
function xmlIOHTTPMatch(filename: pchar): cint; XMLCALL; XMLPUBFUN;
function xmlIOHTTPOpen(filename: pchar): pointer; XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_OUTPUT_ENABLED}
function xmlIOHTTPOpenW(post_uri: pchar; compression: cint): pointer; XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
function xmlIOHTTPRead(context: pointer; buffer: pchar; len: cint): cint; XMLCALL; XMLPUBFUN;
function xmlIOHTTPClose(context: pointer): cint; XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_HTTP_ENABLED *)

(**
 * Default 'ftp://' protocol callbacks
 *)
{$IFDEF LIBXML_FTP_ENABLED}
function xmlIOFTPMatch(filename: pchar): cint; XMLCALL; XMLPUBFUN;
function xmlIOFTPOpen(filename: pchar): pointer; XMLCALL; XMLPUBFUN;
function xmlIOFTPRead(context: pointer; buffer: pchar; len: cint): cint; XMLCALL; XMLPUBFUN;
function xmlIOFTPClose(context: pointer): cint; XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_FTP_ENABLED *)
{$ENDIF}
