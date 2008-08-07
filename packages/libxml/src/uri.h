(**
 * Summary: library of generic URI related routines
 * Description: library of generic URI related routines
 *              Implements RFC 2396
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

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
    scheme    : pchar;	(* the URI scheme *)
    opaque    : pchar;	(* opaque part *)
    authority : pchar;	(* the authority part *)
    server    : pchar;	(* the server part *)
    user      : pchar;		(* the user part *)
    port      : cint;		(* the port number *)
    path      : pchar;		(* the path string *)
    query     : pchar;	(* the query string (deprecated - use with caution) *)
    fragment  : pchar;	(* the fragment identifier *)
    cleanup   : cint;	(* parsing potentially unclean URI *)
    query_raw : pchar;	(* the query string (as it appears in the URI) *)
  end;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * This function is in tree.h:
 * xmlChar *	xmlNodeGetBase	(xmlDocPtr doc,
 *                               xmlNodePtr cur);
 *)
function xmlCreateURI(): xmlURIPtr; cdecl; external;
function xmlBuildURI(URI: xmlCharPtr; base: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlBuildRelativeURI(URI: xmlCharPtr; base: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlParseURI(str: pchar): xmlURIPtr; cdecl; external;
function xmlParseURI(str: pchar; raw: cint): xmlURIPtr; cdecl; external;
function xmlParseURIReference(uri: xmlURIPtr; str: pchar): cint; cdecl; external;
function xmlSaveUri(uri: xmlURIPtr): pchar; cdecl; external;
//procedure xmlPrintURI(stream: FILE; uri: xmlURIPtr); cdecl; external;
function xmlURIEscapeStr(str: xmlCharPtr; list: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlURIUnescapeString(str: pchar; len: cint; target: pchar): xmlCharPtr; cdecl; external;
function xmlNormalizeURIPath(path: pchar): cint; cdecl; external;
function xmlURIEscape(str: xmlCharPtr): xmlCharPtr; cdecl; external;
procedure xmlPrintURI(uri: xmlURIPtr); cdecl; external;
function xmlCanonicPath(path: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlPathToURI(path: xmlCharPtr): xmlCharPtr; cdecl; external;
{$ENDIF}