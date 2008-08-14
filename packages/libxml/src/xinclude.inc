(*
 * Summary: implementation of XInclude
 * Description: API to handle XInclude processing,
 * implements the
 * World Wide Web Consortium Last Call Working Draft 10 November 2003
 * http://www.w3.org/TR/2003/WD-xinclude-20031110
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_XINCLUDE_ENABLED}

{$IFDEF CONST}
(**
 * XINCLUDE_NS:
 *
 * Macro defining the Xinclude namespace: http://www.w3.org/2003/XInclude
 *)
  XINCLUDE_NS: xmlCharPtr = 'http://www.w3.org/2003/XInclude';
(**
 * XINCLUDE_OLD_NS:
 *
 * Macro defining the draft Xinclude namespace: http://www.w3.org/2001/XInclude
 *)
  XINCLUDE_OLD_NS: xmlCharPtr = 'http://www.w3.org/2001/XInclude';
(**
 * XINCLUDE_NODE:
 *
 * Macro defining "include"
 *)
  XINCLUDE_NODE: xmlCharPtr = 'include';
(**
 * XINCLUDE_FALLBACK:
 *
 * Macro defining "fallback"
 *)
  XINCLUDE_FALLBACK: xmlCharPtr = 'fallback';
(**
 * XINCLUDE_HREF:
 *
 * Macro defining "href"
 *)
  XINCLUDE_HREF: xmlCharPtr = 'href';
(**
 * XINCLUDE_PARSE:
 *
 * Macro defining "parse"
 *)
  XINCLUDE_PARSE: xmlCharPtr = 'parse';
(**
 * XINCLUDE_PARSE_XML:
 *
 * Macro defining "xml"
 *)
  XINCLUDE_PARSE_XML: xmlCharPtr = 'xml';
(**
 * XINCLUDE_PARSE_TEXT:
 *
 * Macro defining "text"
 *)
  XINCLUDE_PARSE_TEXT: xmlCharPtr = 'text';
(**
 * XINCLUDE_PARSE_ENCODING:
 *
 * Macro defining "encoding"
 *)
  XINCLUDE_PARSE_ENCODING: xmlCharPtr = 'encoding';
(**
 * XINCLUDE_PARSE_XPOINTER:
 *
 * Macro defining "xpointer"
 *)
  XINCLUDE_PARSE_XPOINTER: xmlCharPtr = 'xpointer';
{$ENDIF}

{$IFDEF POINTER}
  xmlXIncludeCtxtPtr = ^xmlXIncludeCtxt;
{$ENDIF}

{$IFDEF TYPE}
  xmlXIncludeCtxt = record end;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * standalone processing
 *)
function xmlXIncludeProcess(doc: xmlDocPtr): cint; XMLCALL; XMLPUBFUN;
function xmlXIncludeProcessFlags(doc: xmlDocPtr; flags: cint): cint; XMLCALL; XMLPUBFUN;
function xmlXIncludeProcessFlagsData(doc: xmlDocPtr; flags: cint; data: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlXIncludeProcessTree(tree: xmlNodePtr): cint; XMLCALL; XMLPUBFUN;
function xmlXIncludeProcessTreeFlags(tree: xmlNodePtr; flags: cint): cint; XMLCALL; XMLPUBFUN;

(*
 * contextual processing
 *)
function xmlXIncludeNewContext(doc: xmlDocPtr): xmlXIncludeCtxtPtr; XMLCALL; XMLPUBFUN;
function xmlXIncludeProcessFlags(ctxt: xmlXIncludeCtxtPtr; flags: cint): cint; XMLCALL; XMLPUBFUN;
procedure xmlXIncludeProcess(ctxt: xmlXIncludeCtxtPtr); XMLCALL; XMLPUBFUN;
function xmlXIncludeProcessNode(ctxt: xmlXIncludeCtxtPtr; tree: xmlNodePtr): cint; XMLCALL; XMLPUBFUN;
{$ENDIF}

{$ENDIF} (* LIBXML_XINCLUDE_ENABLED *)
