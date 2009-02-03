(*
 * Summary: interface for an HTML 4.0 non-verifying parser
 * Description: this module implements an HTML 4.0 non-verifying parser
 *              with API compatible with the XML parser ones. It should
 *              be able to parse "real world" HTML, even if severely
 *              broken from a specification point of view.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_HTML_ENABLED}

{$IFDEF POINTER}
  htmlElemDescPtr = ^htmlElemDesc;
  htmlEntityDescPtr = ^htmlEntityDesc;
{$ENDIF}

{$IFDEF TYPE}
(*
 * Most of the back-end structures from XML and HTML are shared.
 *)
  htmlParserCtxt = xmlParserCtxt;
  htmlParserCtxtPtr = xmlParserCtxtPtr;
  htmlParserNodeInfo = xmlParserNodeInfo;
  htmlSAXHandler = xmlSAXHandler;
  htmlSAXHandlerPtr = xmlSAXHandlerPtr;
  htmlParserInput = xmlParserInput;
  htmlParserInputPtr = xmlParserInputPtr;
  htmlDocPtr = xmlDocPtr;
  htmlNodePtr = xmlNodePtr;

(*
 * Internal description of an HTML element, representing HTML 4.01
 * and XHTML 1.0 (which share the same structure).
 *)
  htmlElemDesc = record
    name        : pchar;	(* The tag name *)
    startTag    : char;      (* Whether the start tag can be implied *)
    endTag      : char;        (* Whether the end tag can be implied *)
    saveEndTag  : char;    (* Whether the end tag should be saved *)
    empty       : char;         (* Is this an empty element ? *)
    depr        : char;          (* Is this a deprecated element ? *)
    dtd         : char;           (* 1: only in Loose DTD, 2: only Frameset one *)
    isinline    : char;      (* is this a block 0 or inline 1 element *)
    desc        : pchar;   (* the description *)

(* NRK Jan.2003
 * New fields encapsulating HTML structure
 *
 * Bugs:
 *	This is a very limited representation.  It fails to tell us when
 *	an element *requires* subelements (we only have whether they're
 *	allowed or not), and it doesn't tell us where CDATA and PCDATA
 *	are allowed.  Some element relationships are not fully represented:
 *	these are flagged with the word MODIFIER
 *)
    subelts       : ppchar;		(* allowed sub-elements of this element *)
    defaultsubelt : pchar;	(* subelement for suggested auto-repair
					   if necessary or NULL *)
    attrs_opt     : ppchar;		(* Optional Attributes *)
    attrs_depr    : ppchar;		(* Additional deprecated attributes *)
    attrs_req     : ppchar;		(* Required attributes *)
  end;

(*
 * Internal description of an HTML entity.
 *)
  htmlEntityDesc = record
    value : cuint;	(* the UNICODE value for the character *)
    name  : pchar;	(* The entity name *)
    desc  : pchar;   (* the description *)
  end;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * There is only few public functions.
 *)
function htmlTagLookup(tag: xmlCharPtr): htmlElemDescPtr; EXTDECL; external xml2lib;
function htmlEntityLookup(tag: xmlCharPtr): htmlEntityDescPtr; EXTDECL; external xml2lib;
function htmlEntityValueLookup(value: cuint): htmlEntityDescPtr; EXTDECL; external xml2lib;

function htmlIsAutoClosed(doc: htmlDocPtr; elem: htmlNodePtr): cint; EXTDECL; external xml2lib;
function htmlAutoCloseTag(doc: htmlDocPtr; name: xmlCharPtr; elem: htmlNodePtr): cint; EXTDECL; external xml2lib;
function htmlParseEntityRef(ctxt: htmlParserCtxtPtr; str: xmlCharPtrPtr): htmlEntityDescPtr; EXTDECL; external xml2lib;

function htmlParseCharRef(ctxt: htmlParserCtxtPtr): cint; EXTDECL; external xml2lib;
function htmlParseElement(ctxt: htmlParserCtxtPtr): cint; EXTDECL; external xml2lib;
function htmlNewParserCtxt: htmlParserCtxtPtr; EXTDECL; external xml2lib;
function htmlCreateMemoryParserCtxt(buffer: pchar; size: cint): htmlParserCtxtPtr; EXTDECL; external xml2lib;

function htmlParseDocument(doc: htmlDocPtr; elem: htmlNodePtr): cint; EXTDECL; external xml2lib;
function htmlSAXParseDoc(cur: xmlCharPtr; encoding: pchar; sax: htmlSAXHandlerPtr; userdata: pointer): htmlDocPtr; EXTDECL; external xml2lib;
function htmlParseDoc(cur: xmlCharPtr; encoding: pchar): htmlDocPtr; EXTDECL; external xml2lib;
function htmlSAXParseFile(filename, encoding: pchar; sax: htmlSAXHandlerPtr; userdata: pointer): htmlDocPtr; EXTDECL; external xml2lib;
function htmlParseFile(filename, encoding: pchar): htmlDocPtr; EXTDECL; external xml2lib;
function UTF8ToHtml(_out: pointer; outlen: cint; _in: pointer; inlen: cint): cint; EXTDECL; external xml2lib;
function htmlEncodeEntities(_out: pointer; outlen: cint; _in: pointer; inlen, quoteChar: cint): cint; EXTDECL; external xml2lib;
function htmlIsScriptAttribute(name: xmlCharPtr): cint; EXTDECL; external xml2lib;
function htmlHandleOmittedElem(val: cint): cint; EXTDECL; external xml2lib;

{$IFDEF LIBXML_PUSH_ENABLED}
(**
 * Interfaces for the Push mode.
 *)
function htmlCreatePushParserCtxt(sax: htmlSAXHandlerPtr; userdata: pointer; chunk: pchar; size: cint; filename: pchar; enc: xmlCharEncoding): htmlParserCtxtPtr; EXTDECL; external xml2lib;
function htmlParseChunk(ctxt: htmlParserCtxtPtr; chunk: pchar; size, terminate: cint): htmlParserCtxtPtr; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_PUSH_ENABLED *)
procedure htmlFreeParserCtxt(ctxt: htmlParserCtxtPtr); EXTDECL; external xml2lib;
{$ENDIF}

{$IFDEF TYPE}
  htmlParserOption = type cint;
{$ENDIF}
{$IFDEF CONST}
(*
 * New set of simpler/more flexible APIs
 *)
(**
 * xmlParserOption:
 *
 * This is the set of XML parser options that can be passed down
 * to the xmlReadDoc() and similar calls.
 *)
  HTML_PARSE_RECOVER  = 1 shl 0; (* Relaxed parsing *)
  HTML_PARSE_NOERROR	= 1 shl 5;	(* suppress error reports *)
  HTML_PARSE_NOWARNING= 1 shl 6;	(* suppress warning reports *)
  HTML_PARSE_PEDANTIC	= 1 shl 7;	(* pedantic error reporting *)
  HTML_PARSE_NOBLANKS	= 1 shl 8;	(* remove blank nodes *)
  HTML_PARSE_NONET	= 1 shl 11;(* Forbid network access *)
  HTML_PARSE_COMPACT  = 1 shl 16; (* compact small text nodes *)
{$ENDIF}

{$IFDEF FUNCTION}
procedure htmlCtxtReset(ctxt: htmlParserCtxtPtr); EXTDECL; external xml2lib;
function htmlParseChunk(ctxt: htmlParserCtxtPtr; options: cint): cint; EXTDECL; external xml2lib;
function htmlReadDoc(cur: xmlCharPtr; URL, encoding: pchar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlReadFile(URL, encoding: pchar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlReadMemory(buffer: pchar; size: cint; URL, encoding: pchar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlReadFd(fd: cint; URL, encoding: pchar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlReadIO(ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; URL, encoding: pchar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlCtxtReadDoc(ctxt: xmlParserCtxtPtr; cur: xmlCharPtr; URL, encoding: pchar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlCtxtReadFile(ctxt: xmlParserCtxtPtr; filename, encoding: pchar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlCtxtReadMemory(ctxt: xmlParserCtxtPtr; buffer: pchar; size: cint; URL, encoding: pchar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlCtxtReadFd(ctxt: xmlParserCtxtPtr; fd: cint; URL, encoding: pchar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlCtxtReadIO(ctxt: xmlParserCtxtPtr; ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; URL, encoding: pchar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
{$ENDIF}

{$IFDEF TYPE}
  htmlStatus = type cint;
{$ENDIF}
{$IFDEF CONST}
(* NRK/Jan2003: further knowledge of HTML structure
 *)
  HTML_NA = $0;		(* something we don't check at all *)
  HTML_INVALID = $1;
  HTML_DEPRECATED = $2;
  HTML_VALID = $4;
  HTML_REQUIRED = $c; (* VALID bit set so ( & HTML_VALID ) is TRUE *)
{$ENDIF}

{$IFDEF FUNCTION}
(* Using htmlElemDesc rather than name here, to emphasise the fact
   that otherwise there's a lookup overhead
*)
function htmlAttrAllowed(desc: htmlElemDescPtr; str: xmlCharPtr; val: cint): htmlStatus; EXTDECL; external xml2lib;
function htmlElementAllowedHere(desc: htmlElemDescPtr; str: xmlCharPtr): cint; EXTDECL; external xml2lib;
function htmlAttrAllowed(desc1, desc2: htmlElemDescPtr): htmlStatus; EXTDECL; external xml2lib;
function htmlNodeStatus(node: htmlNodePtr; val: cint): htmlStatus; EXTDECL; external xml2lib;

(**
 * htmlDefaultSubelement:
 * @elt: HTML element
 *
 * Returns the default subelement for this element
 *)
function htmlDefaultSubelement(elt: htmlElemDescPtr): pchar;

(**
 * htmlElementAllowedHereDesc:
 * @parent: HTML parent element
 * @elt: HTML element
 *
 * Checks whether an HTML element description may be a
 * direct child of the specified element.
 *
 * Returns 1 if allowed; 0 otherwise.
 *)
function htmlElementAllowedHereDesc(parent: htmlElemDescPtr; elt: htmlElemDescPtr): cint;

(**
 * htmlRequiredAttrs:
 * @elt: HTML element
 *
 * Returns the attributes required for the specified element.
 *)
function htmlRequiredAttrs(elt: htmlElemDescPtr): ppchar;
{$ENDIF}

{$ENDIF} (* LIBXML_HTML_ENABLED *)

