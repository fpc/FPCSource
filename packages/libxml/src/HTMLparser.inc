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
    name        : PAnsiChar;	(* The tag name *)
    startTag    : AnsiChar;      (* Whether the start tag can be implied *)
    endTag      : AnsiChar;        (* Whether the end tag can be implied *)
    saveEndTag  : AnsiChar;    (* Whether the end tag should be saved *)
    empty       : AnsiChar;         (* Is this an empty element ? *)
    depr        : AnsiChar;          (* Is this a deprecated element ? *)
    dtd         : AnsiChar;           (* 1: only in Loose DTD, 2: only Frameset one *)
    isinline    : AnsiChar;      (* is this a block 0 or inline 1 element *)
    desc        : PAnsiChar;   (* the description *)

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
    subelts       : PPAnsiChar;		(* allowed sub-elements of this element *)
    defaultsubelt : PAnsiChar;	(* subelement for suggested auto-repair
					   if necessary or NULL *)
    attrs_opt     : PPAnsiChar;		(* Optional Attributes *)
    attrs_depr    : PPAnsiChar;		(* Additional deprecated attributes *)
    attrs_req     : PPAnsiChar;		(* Required attributes *)
  end;

(*
 * Internal description of an HTML entity.
 *)
  htmlEntityDesc = record
    value : cuint;	(* the UNICODE value for the character *)
    name  : PAnsiChar;	(* The entity name *)
    desc  : PAnsiChar;   (* the description *)
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
function htmlCreateMemoryParserCtxt(buffer: PAnsiChar; size: cint): htmlParserCtxtPtr; EXTDECL; external xml2lib;

function htmlParseDocument(doc: htmlDocPtr; elem: htmlNodePtr): cint; EXTDECL; external xml2lib;
function htmlSAXParseDoc(cur: xmlCharPtr; encoding: PAnsiChar; sax: htmlSAXHandlerPtr; userdata: pointer): htmlDocPtr; EXTDECL; external xml2lib;
function htmlParseDoc(cur: xmlCharPtr; encoding: PAnsiChar): htmlDocPtr; EXTDECL; external xml2lib;
function htmlSAXParseFile(filename, encoding: PAnsiChar; sax: htmlSAXHandlerPtr; userdata: pointer): htmlDocPtr; EXTDECL; external xml2lib;
function htmlParseFile(filename, encoding: PAnsiChar): htmlDocPtr; EXTDECL; external xml2lib;
function UTF8ToHtml(_out: pointer; outlen: cint; _in: pointer; inlen: cint): cint; EXTDECL; external xml2lib;
function htmlEncodeEntities(_out: pointer; outlen: cint; _in: pointer; inlen, quoteChar: cint): cint; EXTDECL; external xml2lib;
function htmlIsScriptAttribute(name: xmlCharPtr): cint; EXTDECL; external xml2lib;
function htmlHandleOmittedElem(val: cint): cint; EXTDECL; external xml2lib;

{$IFDEF LIBXML_PUSH_ENABLED}
(**
 * Interfaces for the Push mode.
 *)
function htmlCreatePushParserCtxt(sax: htmlSAXHandlerPtr; userdata: pointer; chunk: PAnsiChar; size: cint; filename: PAnsiChar; enc: xmlCharEncoding): htmlParserCtxtPtr; EXTDECL; external xml2lib;
function htmlParseChunk(ctxt: htmlParserCtxtPtr; chunk: PAnsiChar; size, terminate: cint): htmlParserCtxtPtr; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_PUSH_ENABLED *)
procedure htmlFreeParserCtxt(ctxt: htmlParserCtxtPtr); EXTDECL; external xml2lib;
{$ENDIF}

{$IFDEF FUNCTIONVAR}
(*
 * There is only few public functions.
 *)
  htmlTagLookup: function(tag: xmlCharPtr): htmlElemDescPtr; EXTDECL;
  htmlEntityLookup: function(tag: xmlCharPtr): htmlEntityDescPtr; EXTDECL;
  htmlEntityValueLookup: function(value: cuint): htmlEntityDescPtr; EXTDECL;

  htmlIsAutoClosed: function(doc: htmlDocPtr; elem: htmlNodePtr): cint; EXTDECL;
  htmlAutoCloseTag: function(doc: htmlDocPtr; name: xmlCharPtr; elem: htmlNodePtr): cint; EXTDECL;
  htmlParseEntityRef: function(ctxt: htmlParserCtxtPtr; str: xmlCharPtrPtr): htmlEntityDescPtr; EXTDECL;

  htmlParseCharRef: function(ctxt: htmlParserCtxtPtr): cint; EXTDECL;
  htmlParseElement: function(ctxt: htmlParserCtxtPtr): cint; EXTDECL;
  htmlNewParserCtxt: function: htmlParserCtxtPtr; EXTDECL;
  htmlCreateMemoryParserCtxt: function(buffer: PAnsiChar; size: cint): htmlParserCtxtPtr; EXTDECL;

  htmlParseDocument: function(doc: htmlDocPtr; elem: htmlNodePtr): cint; EXTDECL;
  htmlSAXParseDoc: function(cur: xmlCharPtr; encoding: PAnsiChar; sax: htmlSAXHandlerPtr; userdata: pointer): htmlDocPtr; EXTDECL;
  htmlParseDoc: function(cur: xmlCharPtr; encoding: PAnsiChar): htmlDocPtr; EXTDECL;
  htmlSAXParseFile: function(filename, encoding: PAnsiChar; sax: htmlSAXHandlerPtr; userdata: pointer): htmlDocPtr; EXTDECL;
  htmlParseFile: function(filename, encoding: PAnsiChar): htmlDocPtr; EXTDECL;
  UTF8ToHtml: function(_out: pointer; outlen: cint; _in: pointer; inlen: cint): cint; EXTDECL;
  htmlEncodeEntities: function(_out: pointer; outlen: cint; _in: pointer; inlen, quoteChar: cint): cint; EXTDECL;
  htmlIsScriptAttribute: function(name: xmlCharPtr): cint; EXTDECL;
  htmlHandleOmittedElem: function(val: cint): cint; EXTDECL;

{$IFDEF LIBXML_PUSH_ENABLED}
(**
 * Interfaces for the Push mode.
 *)
  htmlCreatePushParserCtxt: function(sax: htmlSAXHandlerPtr; userdata: pointer; chunk: PAnsiChar; size: cint; filename: PAnsiChar; enc: xmlCharEncoding): htmlParserCtxtPtr; EXTDECL;
  htmlParseChunk: function(ctxt: htmlParserCtxtPtr; chunk: PAnsiChar; size, terminate: cint): htmlParserCtxtPtr; EXTDECL;
{$ENDIF} (* LIBXML_PUSH_ENABLED *)
  htmlFreeParserCtxt: procedure(ctxt: htmlParserCtxtPtr); EXTDECL;
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
  HTML_PARSE_RECOVER    = (1 shl 0); (* Relaxed parsing *)
  HTML_PARSE_NODEFDTD   = (1 shl 2); (* do not default a doctype if not found *)
  HTML_PARSE_NOERROR    = (1 shl 5); (* suppress error reports *)
  HTML_PARSE_NOWARNING  = (1 shl 6); (* suppress warning reports *)
  HTML_PARSE_PEDANTIC	= (1 shl 7); (* pedantic error reporting *)
  HTML_PARSE_NOBLANKS	= (1 shl 8); (* remove blank nodes *)
  HTML_PARSE_NONET	= (1 shl 11);(* Forbid network access *)
  HTML_PARSE_NOIMPLIED  = (1 shl 13);(* Do not add implied html/body... elements *)
  HTML_PARSE_COMPACT    = (1 shl 16);(* compact small text nodes *)
  HTML_PARSE_IGNORE_ENC = (1 shl 21);(* ignore internal document encoding hint *)
{$ENDIF}

{$IFDEF FUNCTION}
procedure htmlCtxtReset(ctxt: htmlParserCtxtPtr); EXTDECL; external xml2lib;
function htmlCtxtUseOptions(ctxt: htmlParserCtxtPtr; options: cint): cint; EXTDECL; external xml2lib;
function htmlReadDoc(cur: xmlCharPtr; URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlReadFile(URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlReadMemory(buffer: PAnsiChar; size: cint; URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlReadFd(fd: cint; URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlReadIO(ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlCtxtReadDoc(ctxt: xmlParserCtxtPtr; cur: xmlCharPtr; URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlCtxtReadFile(ctxt: xmlParserCtxtPtr; filename, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlCtxtReadMemory(ctxt: xmlParserCtxtPtr; buffer: PAnsiChar; size: cint; URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlCtxtReadFd(ctxt: xmlParserCtxtPtr; fd: cint; URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
function htmlCtxtReadIO(ctxt: xmlParserCtxtPtr; ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL; external xml2lib;
{$ENDIF}

{$IFDEF FUNCTIONVAR}
  htmlCtxtReset: procedure(ctxt: htmlParserCtxtPtr); EXTDECL;
  htmlCtxtUseOptions: function(ctxt: htmlParserCtxtPtr; options: cint): cint; EXTDECL;
  htmlReadDoc: function(cur: xmlCharPtr; URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL;
  htmlReadFile: function(URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL;
  htmlReadMemory: function(buffer: PAnsiChar; size: cint; URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL;
  htmlReadFd: function(fd: cint; URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL;
  htmlReadIO: function(ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL;
  htmlCtxtReadDoc: function(ctxt: xmlParserCtxtPtr; cur: xmlCharPtr; URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL;
  htmlCtxtReadFile: function(ctxt: xmlParserCtxtPtr; filename, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL;
  htmlCtxtReadMemory: function(ctxt: xmlParserCtxtPtr; buffer: PAnsiChar; size: cint; URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL;
  htmlCtxtReadFd: function(ctxt: xmlParserCtxtPtr; fd: cint; URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL;
  htmlCtxtReadIO: function(ctxt: xmlParserCtxtPtr; ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; URL, encoding: PAnsiChar; options: cint): htmlDocPtr; EXTDECL;
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
function htmlElementStatusHere(desc1, desc2: htmlElemDescPtr): htmlStatus; EXTDECL; external xml2lib;
function htmlNodeStatus(node: htmlNodePtr; val: cint): htmlStatus; EXTDECL; external xml2lib;
{$ENDIF}

{$IFDEF FUNCTIONVAR}
(* Using htmlElemDesc rather than name here, to emphasise the fact
   that otherwise there's a lookup overhead
*)
  htmlAttrAllowed: function(desc: htmlElemDescPtr; str: xmlCharPtr; val: cint): htmlStatus; EXTDECL;
  htmlElementAllowedHere: function(desc: htmlElemDescPtr; str: xmlCharPtr): cint; EXTDECL;
  htmlElementStatusHere: function(desc1, desc2: htmlElemDescPtr): htmlStatus; EXTDECL;
  htmlNodeStatus: function(node: htmlNodePtr; val: cint): htmlStatus; EXTDECL;
{$ENDIF}

{$IFDEF MACRO}
(**
 * htmlDefaultSubelement:
 * @elt: HTML element
 *
 * Returns the default subelement for this element
 *)
function htmlDefaultSubelement(elt: htmlElemDescPtr): PAnsiChar;

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
function htmlRequiredAttrs(elt: htmlElemDescPtr): PPAnsiChar;
{$ENDIF}

{$ENDIF} (* LIBXML_HTML_ENABLED *)

