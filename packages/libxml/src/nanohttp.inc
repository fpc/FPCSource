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
procedure xmlNanoHTTPInit; EXTDECL; external xml2lib;
procedure xmlNanoHTTPCleanup; EXTDECL; external xml2lib;
procedure xmlNanoHTTPScanProxy(URL: pchar); EXTDECL; external xml2lib;
function xmlNanoHTTPCleanup(URL, filename: pchar; contentType: ppchar): cint; EXTDECL; external xml2lib;
function xmlNanoHTTPMethod(URL, method, input: pchar; contentType: ppchar; headers: pchar; ilen: cint): pointer; EXTDECL; external xml2lib;
function xmlNanoHTTPMethodRedir(URL, method, input: pchar; contentType, redir: ppchar; headers: pchar; ilen: cint): pointer; EXTDECL; external xml2lib;
function xmlNanoHTTPOpen(URL: pchar; contentType: ppchar): pointer; EXTDECL; external xml2lib;
function xmlNanoHTTPOpenRedir(URL: pchar; contentType, redir: ppchar): pointer; EXTDECL; external xml2lib;
function xmlNanoHTTPReturnCode(ctx: pointer): cint; EXTDECL; external xml2lib;
function xmlNanoHTTPAuthHeader(ctx: pointer): pchar; EXTDECL; external xml2lib;
function xmlNanoHTTPRedir(ctx: pointer): pchar; EXTDECL; external xml2lib;
function xmlNanoHTTPContentLength(ctx: pointer): cint; EXTDECL; external xml2lib;
function xmlNanoHTTPEncoding(ctx: pointer): pchar; EXTDECL; external xml2lib;
function xmlNanoHTTPMimeType(ctx: pointer): pchar; EXTDECL; external xml2lib;
function xmlNanoHTTPRead(ctx: pointer; dest: pointer; len: cint): cint; EXTDECL; external xml2lib;
{$IFDEF LIBXML_OUTPUT_ENABLED}
function xmlNanoHTTPSave(ctx: pointer; filename: pchar): cint; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
procedure xmlNanoHTTPClose(ctx: pointer); EXTDECL; external xml2lib;
{$ENDIF}

{$ENDIF} (* LIBXML_HTTP_ENABLED *)
