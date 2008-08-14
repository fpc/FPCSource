(*
 * Summary: minimal HTTP implementation
 * Description: minimal HTTP implementation allowing to fetch resources
 *              like external subset.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)
 
{$IFDEF LIBXML_HTTP_ENABLED}

{$IFDEF FUNCTION}
procedure xmlNanoHTTPInit; XMLCALL; XMLPUBFUN;
procedure xmlNanoHTTPCleanup; XMLCALL; XMLPUBFUN;
procedure xmlNanoHTTPScanProxy(URL: pchar); XMLCALL; XMLPUBFUN;
function xmlNanoHTTPCleanup(URL, filename: pchar; contentType: ppchar): cint; XMLCALL; XMLPUBFUN;
function xmlNanoHTTPMethod(URL, method, input: pchar; contentType: ppchar; headers: pchar; ilen: cint): pointer; XMLCALL; XMLPUBFUN;
function xmlNanoHTTPMethodRedir(URL, method, input: pchar; contentType, redir: ppchar; headers: pchar; ilen: cint): pointer; XMLCALL; XMLPUBFUN;
function xmlNanoHTTPOpen(URL: pchar; contentType: ppchar): pointer; XMLCALL; XMLPUBFUN;
function xmlNanoHTTPOpenRedir(URL: pchar; contentType, redir: ppchar): pointer; XMLCALL; XMLPUBFUN;
function xmlNanoHTTPReturnCode(ctx: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlNanoHTTPAuthHeader(ctx: pointer): pchar; XMLCALL; XMLPUBFUN;
function xmlNanoHTTPRedir(ctx: pointer): pchar; XMLCALL; XMLPUBFUN;
function xmlNanoHTTPContentLength(ctx: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlNanoHTTPEncoding(ctx: pointer): pchar; XMLCALL; XMLPUBFUN;
function xmlNanoHTTPMimeType(ctx: pointer): pchar; XMLCALL; XMLPUBFUN;
function xmlNanoHTTPRead(ctx: pointer; dest: pointer; len: cint): cint; XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_OUTPUT_ENABLED}
function xmlNanoHTTPSave(ctx: pointer; filename: pchar): cint; XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
procedure xmlNanoHTTPClose(ctx: pointer); XMLCALL; XMLPUBFUN;
{$ENDIF}

{$ENDIF} (* LIBXML_HTTP_ENABLED *)
