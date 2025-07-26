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
procedure xmlNanoHTTPScanProxy(URL: PAnsiChar); EXTDECL; external xml2lib;
function xmlNanoHTTPFetch(URL, filename: PAnsiChar; contentType: PPAnsiChar): cint; EXTDECL; external xml2lib;
function xmlNanoHTTPMethod(URL, method, input: PAnsiChar; contentType: PPAnsiChar; headers: PAnsiChar; ilen: cint): pointer; EXTDECL; external xml2lib;
function xmlNanoHTTPMethodRedir(URL, method, input: PAnsiChar; contentType, redir: PPAnsiChar; headers: PAnsiChar; ilen: cint): pointer; EXTDECL; external xml2lib;
function xmlNanoHTTPOpen(URL: PAnsiChar; contentType: PPAnsiChar): pointer; EXTDECL; external xml2lib;
function xmlNanoHTTPOpenRedir(URL: PAnsiChar; contentType, redir: PPAnsiChar): pointer; EXTDECL; external xml2lib;
function xmlNanoHTTPReturnCode(ctx: pointer): cint; EXTDECL; external xml2lib;
function xmlNanoHTTPAuthHeader(ctx: pointer): PAnsiChar; EXTDECL; external xml2lib;
function xmlNanoHTTPRedir(ctx: pointer): PAnsiChar; EXTDECL; external xml2lib;
function xmlNanoHTTPContentLength(ctx: pointer): cint; EXTDECL; external xml2lib;
function xmlNanoHTTPEncoding(ctx: pointer): PAnsiChar; EXTDECL; external xml2lib;
function xmlNanoHTTPMimeType(ctx: pointer): PAnsiChar; EXTDECL; external xml2lib;
function xmlNanoHTTPRead(ctx: pointer; dest: pointer; len: cint): cint; EXTDECL; external xml2lib;
{$IFDEF LIBXML_OUTPUT_ENABLED}
function xmlNanoHTTPSave(ctx: pointer; filename: PAnsiChar): cint; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
procedure xmlNanoHTTPClose(ctx: pointer); EXTDECL; external xml2lib;
{$ENDIF}

{$IFDEF FUNCTIONVAR}
  xmlNanoHTTPInit: procedure; EXTDECL;
  xmlNanoHTTPCleanup: procedure; EXTDECL;
  xmlNanoHTTPScanProxy: procedure(URL: PAnsiChar); EXTDECL;
  xmlNanoHTTPFetch: function(URL, filename: PAnsiChar; contentType: PPAnsiChar): cint; EXTDECL;
  xmlNanoHTTPMethod: function(URL, method, input: PAnsiChar; contentType: PPAnsiChar; headers: PAnsiChar; ilen: cint): pointer; EXTDECL;
  xmlNanoHTTPMethodRedir: function(URL, method, input: PAnsiChar; contentType, redir: PPAnsiChar; headers: PAnsiChar; ilen: cint): pointer; EXTDECL;
  xmlNanoHTTPOpen: function(URL: PAnsiChar; contentType: PPAnsiChar): pointer; EXTDECL;
  xmlNanoHTTPOpenRedir: function(URL: PAnsiChar; contentType, redir: PPAnsiChar): pointer; EXTDECL;
  xmlNanoHTTPReturnCode: function(ctx: pointer): cint; EXTDECL;
  xmlNanoHTTPAuthHeader: function(ctx: pointer): PAnsiChar; EXTDECL;
  xmlNanoHTTPRedir: function(ctx: pointer): PAnsiChar; EXTDECL;
  xmlNanoHTTPContentLength: function(ctx: pointer): cint; EXTDECL;
  xmlNanoHTTPEncoding: function(ctx: pointer): PAnsiChar; EXTDECL;
  xmlNanoHTTPMimeType: function(ctx: pointer): PAnsiChar; EXTDECL;
  xmlNanoHTTPRead: function(ctx: pointer; dest: pointer; len: cint): cint; EXTDECL;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlNanoHTTPSave: function(ctx: pointer; filename: PAnsiChar): cint; EXTDECL;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  xmlNanoHTTPClose: procedure(ctx: pointer); EXTDECL;
{$ENDIF}

{$ENDIF} (* LIBXML_HTTP_ENABLED *)
