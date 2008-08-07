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

#ifndef __HTML_PARSER_H__
#define __HTML_PARSER_H__
#include <libxml/xmlversion.h>
#include <libxml/parser.h>

{ LIBXML_HTML_ENABLED

{ __cplusplus
extern "C" {
#endif

(*
 * Most of the back-end structures from XML and HTML are shared.
 *)
typedef xmlParserCtxt htmlParserCtxt;
typedef xmlParserCtxtPtr htmlParserCtxtPtr;
typedef xmlParserNodeInfo htmlParserNodeInfo;
typedef xmlSAXHandler htmlSAXHandler;
typedef xmlSAXHandlerPtr htmlSAXHandlerPtr;
typedef xmlParserInput htmlParserInput;
typedef xmlParserInputPtr htmlParserInputPtr;
typedef xmlDocPtr htmlDocPtr;
typedef xmlNodePtr htmlNodePtr;

(*
 * Internal description of an HTML element, representing HTML 4.01
 * and XHTML 1.0 (which share the same structure).
 *)
typedef struct _htmlElemDesc htmlElemDesc;
typedef htmlElemDesc *htmlElemDescPtr;
struct _htmlElemDesc {
    char *name;	(* The tag name *)
    char startTag;      (* Whether the start tag can be implied *)
    char endTag;        (* Whether the end tag can be implied *)
    char saveEndTag;    (* Whether the end tag should be saved *)
    char empty;         (* Is this an empty element ? *)
    char depr;          (* Is this a deprecated element ? *)
    char dtd;           (* 1: only in Loose DTD, 2: only Frameset one *)
    char isinline;      (* is this a block 0 or inline 1 element *)
    char *desc;   (* the description *)

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
    char** subelts;		(* allowed sub-elements of this element *)
    char* defaultsubelt;	(* subelement for suggested auto-repair
					   if necessary or NULL *)
    char** attrs_opt;		(* Optional Attributes *)
    char** attrs_depr;		(* Additional deprecated attributes *)
    char** attrs_req;		(* Required attributes *)
};

(*
 * Internal description of an HTML entity.
 *)
typedef struct _htmlEntityDesc htmlEntityDesc;
typedef htmlEntityDesc *htmlEntityDescPtr;
struct _htmlEntityDesc {
    unsigned int value;	(* the UNICODE value for the character *)
    char *name;	(* The entity name *)
    char *desc;   (* the description *)
};

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

{ LIBXML_PUSH_ENABLED
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
#endif (* LIBXML_PUSH_ENABLED *)

XMLPUBFUN void XMLCALL
			htmlFreeParserCtxt	(htmlParserCtxtPtr ctxt);

(*
 * New set of simpler/more flexible APIs
 *)
(**
 * xmlParserOption:
 *
 * This is the set of XML parser options that can be passed down
 * to the xmlReadDoc() and similar calls.
 *)
typedef enum {
    HTML_PARSE_RECOVER  = 1<<0, (* Relaxed parsing *)
    HTML_PARSE_NOERROR	= 1<<5,	(* suppress error reports *)
    HTML_PARSE_NOWARNING= 1<<6,	(* suppress warning reports *)
    HTML_PARSE_PEDANTIC	= 1<<7,	(* pedantic error reporting *)
    HTML_PARSE_NOBLANKS	= 1<<8,	(* remove blank nodes *)
    HTML_PARSE_NONET	= 1<<11,(* Forbid network access *)
    HTML_PARSE_COMPACT  = 1<<16 (* compact small text nodes *)
} htmlParserOption;

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

(* NRK/Jan2003: further knowledge of HTML structure
 *)
typedef enum {
  HTML_NA = 0 ,		(* something we don't check at all *)
  HTML_INVALID = 0x1 ,
  HTML_DEPRECATED = 0x2 ,
  HTML_VALID = 0x4 ,
  HTML_REQUIRED = 0xc (* VALID bit set so ( & HTML_VALID ) is TRUE *)
} htmlStatus ;

(* Using htmlElemDesc rather than name here, to emphasise the fact
   that otherwise there's a lookup overhead
*)
XMLPUBFUN htmlStatus XMLCALL htmlAttrAllowed(htmlElemDesc*, xmlChar*, int) ;
XMLPUBFUN int XMLCALL htmlElementAllowedHere(htmlElemDesc*, xmlChar*) ;
XMLPUBFUN htmlStatus XMLCALL htmlElementStatusHere(htmlElemDesc*, htmlElemDesc*) ;
XMLPUBFUN htmlStatus XMLCALL htmlNodeStatus(htmlNodePtr, int) ;
(**
 * htmlDefaultSubelement:
 * @elt: HTML element
 *
 * Returns the default subelement for this element
 *)
#define htmlDefaultSubelement(elt) elt->defaultsubelt
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
#define htmlElementAllowedHereDesc(parent,elt) \
	htmlElementAllowedHere((parent), (elt)->name)
(**
 * htmlRequiredAttrs:
 * @elt: HTML element
 *
 * Returns the attributes required for the specified element.
 *)
#define htmlRequiredAttrs(elt) (elt)->attrs_req


{ __cplusplus
}
#endif

#endif (* LIBXML_HTML_ENABLED *)
#endif (* __HTML_PARSER_H__ *)
