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
  __XINCLUDE_NS: String = 'http://www.w3.org/2003/XInclude';
  XINCLUDE_NS: xmlCharPtr = @__XINCLUDE_NS[1];
(**
 * XINCLUDE_OLD_NS:
 *
 * Macro defining the draft Xinclude namespace: http://www.w3.org/2001/XInclude
 *)
  __XINCLUDE_OLD_NS: String = 'http://www.w3.org/2001/XInclude';
  XINCLUDE_OLD_NS: xmlCharPtr = @__XINCLUDE_OLD_NS[1];
(**
 * XINCLUDE_NODE:
 *
 * Macro defining "include"
 *)
  __XINCLUDE_NODE: String = 'include';
  XINCLUDE_NODE: xmlCharPtr = @__XINCLUDE_NODE[1];
(**
 * XINCLUDE_FALLBACK:
 *
 * Macro defining "fallback"
 *)
  __XINCLUDE_FALLBACK: String = 'fallback';
  XINCLUDE_FALLBACK: xmlCharPtr = @__XINCLUDE_FALLBACK[1];
(**
 * XINCLUDE_HREF:
 *
 * Macro defining "href"
 *)
  __XINCLUDE_HREF: String = 'href';
  XINCLUDE_HREF: xmlCharPtr = @__XINCLUDE_HREF[1];
(**
 * XINCLUDE_PARSE:
 *
 * Macro defining "parse"
 *)
  __XINCLUDE_PARSE: String = 'parse';
  XINCLUDE_PARSE: xmlCharPtr = @__XINCLUDE_PARSE[1];
(**
 * XINCLUDE_PARSE_XML:
 *
 * Macro defining "xml"
 *)
  __XINCLUDE_PARSE_XML: String = 'xml';
  XINCLUDE_PARSE_XML: xmlCharPtr = @__XINCLUDE_PARSE_XML[1];
(**
 * XINCLUDE_PARSE_TEXT:
 *
 * Macro defining "text"
 *)
  __XINCLUDE_PARSE_TEXT: String = 'text';
  XINCLUDE_PARSE_TEXT: xmlCharPtr = @__XINCLUDE_PARSE_TEXT[1];
(**
 * XINCLUDE_PARSE_ENCODING:
 *
 * Macro defining "encoding"
 *)
  __XINCLUDE_PARSE_ENCODING: String = 'encoding';
  XINCLUDE_PARSE_ENCODING: xmlCharPtr = @__XINCLUDE_PARSE_ENCODING[1];
(**
 * XINCLUDE_PARSE_XPOINTER:
 *
 * Macro defining "xpointer"
 *)
  __XINCLUDE_PARSE_XPOINTER: String = 'xpointer';
  XINCLUDE_PARSE_XPOINTER: xmlCharPtr = @__XINCLUDE_PARSE_XPOINTER[1];
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
