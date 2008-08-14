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

{$IFDEF FUNCTION_}
(*
 * There is only few public functions.
 *)
XMLPUBFUN htmlElemDesc * XMLCALL
			htmlTagLookup	(xmlChar *tag);
XMLPUBFUN htmlEntityDesc * XMLCALL
			htmlEntityLookup(xmlChar *name);
XMLPUBFUN htmlEntityDesc * XMLCALL
			htmlEntityValueLookup(unsigned int value);

XMLPUBFUN int XMLCALL
			htmlIsAutoClosed(htmlDocPtr doc,
					 htmlNodePtr elem);
XMLPUBFUN int XMLCALL
			htmlAutoCloseTag(htmlDocPtr doc,
					 xmlChar *name,
					 htmlNodePtr elem);
XMLPUBFUN htmlEntityDesc * XMLCALL
			htmlParseEntityRef(htmlParserCtxtPtr ctxt,
					 xmlChar **str);
XMLPUBFUN int XMLCALL
			htmlParseCharRef(htmlParserCtxtPtr ctxt);
XMLPUBFUN void XMLCALL
			htmlParseElement(htmlParserCtxtPtr ctxt);

XMLPUBFUN htmlParserCtxtPtr XMLCALL
			htmlNewParserCtxt(void);

XMLPUBFUN htmlParserCtxtPtr XMLCALL
			htmlCreateMemoryParserCtxt(char *buffer,
						   int size);

XMLPUBFUN int XMLCALL
			htmlParseDocument(htmlParserCtxtPtr ctxt);
XMLPUBFUN htmlDocPtr XMLCALL
			htmlSAXParseDoc	(xmlChar *cur,
					 char *encoding,
					 htmlSAXHandlerPtr sax,
					 void *userData);
XMLPUBFUN htmlDocPtr XMLCALL
			htmlParseDoc	(xmlChar *cur,
					 char *encoding);
XMLPUBFUN htmlDocPtr XMLCALL
			htmlSAXParseFile(char *filename,
					 char *encoding,
					 htmlSAXHandlerPtr sax,
					 void *userData);
XMLPUBFUN htmlDocPtr XMLCALL
			htmlParseFile	(char *filename,
					 char *encoding);
XMLPUBFUN int XMLCALL
			UTF8ToHtml	(unsigned char *out,
					 int *outlen,
					 unsigned char *in,
					 int *inlen);
XMLPUBFUN int XMLCALL
			htmlEncodeEntities(unsigned char *out,
					 int *outlen,
					 unsigned char *in,
					 int *inlen, int quoteChar);
XMLPUBFUN int XMLCALL
			htmlIsScriptAttribute(xmlChar *name);
XMLPUBFUN int XMLCALL
			htmlHandleOmittedElem(int val);

{$IFDEF LIBXML_PUSH_ENABLED}
(**
 * Interfaces for the Push mode.
 *)
XMLPUBFUN htmlParserCtxtPtr XMLCALL
			htmlCreatePushParserCtxt(htmlSAXHandlerPtr sax,
						 void *user_data,
						 char *chunk,
						 int size,
						 char *filename,
						 xmlCharEncoding enc);
XMLPUBFUN int XMLCALL
			htmlParseChunk		(htmlParserCtxtPtr ctxt,
						 char *chunk,
						 int size,
						 int terminate);
{$ENDIF} (* LIBXML_PUSH_ENABLED *)

XMLPUBFUN void XMLCALL
			htmlFreeParserCtxt	(htmlParserCtxtPtr ctxt);
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

{$IFDEF FUNCTION_}
XMLPUBFUN void XMLCALL
		htmlCtxtReset		(htmlParserCtxtPtr ctxt);
XMLPUBFUN int XMLCALL
		htmlCtxtUseOptions	(htmlParserCtxtPtr ctxt,
					 int options);
XMLPUBFUN htmlDocPtr XMLCALL
		htmlReadDoc		(xmlChar *cur,
					 char *URL,
					 char *encoding,
					 int options);
XMLPUBFUN htmlDocPtr XMLCALL
		htmlReadFile		(char *URL,
					 char *encoding,
					 int options);
XMLPUBFUN htmlDocPtr XMLCALL
		htmlReadMemory		(char *buffer,
					 int size,
					 char *URL,
					 char *encoding,
					 int options);
XMLPUBFUN htmlDocPtr XMLCALL
		htmlReadFd		(int fd,
					 char *URL,
					 char *encoding,
					 int options);
XMLPUBFUN htmlDocPtr XMLCALL
		htmlReadIO		(xmlInputReadCallback ioread,
					 xmlInputCloseCallback ioclose,
					 void *ioctx,
					 char *URL,
					 char *encoding,
					 int options);
XMLPUBFUN htmlDocPtr XMLCALL
		htmlCtxtReadDoc		(xmlParserCtxtPtr ctxt,
					 xmlChar *cur,
					 char *URL,
					 char *encoding,
					 int options);
XMLPUBFUN htmlDocPtr XMLCALL
		htmlCtxtReadFile		(xmlParserCtxtPtr ctxt,
					 char *filename,
					 char *encoding,
					 int options);
XMLPUBFUN htmlDocPtr XMLCALL
		htmlCtxtReadMemory		(xmlParserCtxtPtr ctxt,
					 char *buffer,
					 int size,
					 char *URL,
					 char *encoding,
					 int options);
XMLPUBFUN htmlDocPtr XMLCALL
		htmlCtxtReadFd		(xmlParserCtxtPtr ctxt,
					 int fd,
					 char *URL,
					 char *encoding,
					 int options);
XMLPUBFUN htmlDocPtr XMLCALL
		htmlCtxtReadIO		(xmlParserCtxtPtr ctxt,
					 xmlInputReadCallback ioread,
					 xmlInputCloseCallback ioclose,
					 void *ioctx,
					 char *URL,
					 char *encoding,
					 int options);
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
function htmlAttrAllowed(desc: htmlElemDescPtr; str: xmlCharPtr; val: cint): htmlStatus; XMLCALL; XMLPUBFUN;
function htmlElementAllowedHere(desc: htmlElemDescPtr; str: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function htmlAttrAllowed(desc1, desc2: htmlElemDescPtr): htmlStatus; XMLCALL; XMLPUBFUN;
function htmlNodeStatus(node: htmlNodePtr; val: cint): htmlStatus; XMLCALL; XMLPUBFUN;

(**
 * htmlDefaultSubelement:
 * @elt: HTML element
 *
 * Returns the default subelement for this element
 *)
//#define htmlDefaultSubelement(elt) elt->defaultsubelt

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
{#define htmlElementAllowedHereDesc(parent,elt) \
	htmlElementAllowedHere((parent), (elt)->name)}

(**
 * htmlRequiredAttrs:
 * @elt: HTML element
 *
 * Returns the attributes required for the specified element.
 *)
{#define htmlRequiredAttrs(elt) (elt)->attrs_req}
{$ENDIF}

{$ENDIF} (* LIBXML_HTML_ENABLED *)

