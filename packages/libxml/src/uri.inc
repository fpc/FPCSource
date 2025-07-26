(**
 * Summary: library of generic URI related routines
 * Description: library of generic URI related routines
 *              Implements RFC 2396
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF POINTER}
  xmlURIPtr = ^xmlURI;
{$ENDIF}

{$IFDEF TYPE}
(**
 * xmlURI:
 *
 * A parsed URI reference. This is a struct containing the various fields
 * as described in RFC 2396 but separated for further processing.
 *
 * Note: query is a deprecated field which is incorrectly unescaped.
 * query_raw takes precedence over query if the former is set.
 * See: http://mail.gnome.org/archives/xml/2007-April/thread.html#00127
 *)
  xmlUri = record
    scheme    : PAnsiChar;	(* the URI scheme *)
    opaque    : PAnsiChar;	(* opaque part *)
    authority : PAnsiChar;	(* the authority part *)
    server    : PAnsiChar;	(* the server part *)
    user      : PAnsiChar;		(* the user part *)
    port      : cint;		(* the port number *)
    path      : PAnsiChar;		(* the path string *)
    query     : PAnsiChar;	(* the query string (deprecated - use with caution) *)
    fragment  : PAnsiChar;	(* the fragment identifier *)
    cleanup   : cint;	(* parsing potentially unclean URI *)
    query_raw : PAnsiChar;	(* the query string (as it appears in the URI) *)
  end;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * This function is in tree.h:
 * xmlChar *	xmlNodeGetBase	(xmlDocPtr doc,
 *                               xmlNodePtr cur);
 *)
function xmlCreateURI(): xmlURIPtr; EXTDECL; external xml2lib;
function xmlBuildURI(URI: xmlCharPtr; base: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlBuildRelativeURI(URI: xmlCharPtr; base: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlParseURI(str: PAnsiChar): xmlURIPtr; EXTDECL; external xml2lib;
function xmlParseURIRaw(str: PAnsiChar; raw: cint): xmlURIPtr; EXTDECL; external xml2lib;
function xmlParseURIReference(uri: xmlURIPtr; str: PAnsiChar): cint; EXTDECL; external xml2lib;
function xmlSaveUri(uri: xmlURIPtr): PAnsiChar; EXTDECL; external xml2lib;
procedure xmlPrintURI(stream: PFILE; uri: xmlURIPtr); EXTDECL; external xml2lib;
function xmlURIEscapeStr(str: xmlCharPtr; list: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlURIUnescapeString(str: PAnsiChar; len: cint; target: PAnsiChar): xmlCharPtr; EXTDECL; external xml2lib;
function xmlNormalizeURIPath(path: PAnsiChar): cint; EXTDECL; external xml2lib;
function xmlURIEscape(str: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
procedure xmlFreeURI(uri: xmlURIPtr); EXTDECL; external xml2lib;
function xmlCanonicPath(path: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlPathToURI(path: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
{$ENDIF}

{$IFDEF FUNCTIONVAR}
(*
 * This function is in tree.h:
 * xmlChar *	xmlNodeGetBase	(xmlDocPtr doc,
 *                               xmlNodePtr cur);
 *)
  xmlCreateURI: function(): xmlURIPtr; EXTDECL;
  xmlBuildURI: function(URI: xmlCharPtr; base: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlBuildRelativeURI: function(URI: xmlCharPtr; base: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlParseURI: function(str: PAnsiChar): xmlURIPtr; EXTDECL;
  xmlParseURIRaw: function(str: PAnsiChar; raw: cint): xmlURIPtr; EXTDECL;
  xmlParseURIReference: function(uri: xmlURIPtr; str: PAnsiChar): cint; EXTDECL;
  xmlSaveUri: function(uri: xmlURIPtr): PAnsiChar; EXTDECL;
  xmlPrintURI: procedure(stream: PFILE; uri: xmlURIPtr); EXTDECL;
  xmlURIEscapeStr: function(str: xmlCharPtr; list: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlURIUnescapeString: function(str: PAnsiChar; len: cint; target: PAnsiChar): xmlCharPtr; EXTDECL;
  xmlNormalizeURIPath: function(path: PAnsiChar): cint; EXTDECL;
  xmlURIEscape: function(str: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlFreeURI: procedure(uri: xmlURIPtr); EXTDECL;
  xmlCanonicPath: function(path: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlPathToURI: function(path: xmlCharPtr): xmlCharPtr; EXTDECL;
{$ENDIF}
