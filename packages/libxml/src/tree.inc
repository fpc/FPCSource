(*
 * Summary: interfaces for tree manipulation
 * Description: this module describes the structures found in an tree resulting
 *              from an XML or HTML parsing, as well as the API provided for
 *              various processing on that tree
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF POINTER}
  xmlBufferPtr = ^xmlBuffer;
  xmlNotationPtr = ^xmlNotation;
  xmlEnumerationPtr = ^xmlEnumeration;
  xmlAttributePtr = ^xmlAttribute;
  xmlElementContentPtr = ^xmlElementContent;
  xmlElementPtr = ^xmlElement;
  xmlNsPtr = ^xmlNs;
  xmlNsPtrPtr = ^xmlNsPtr;
  xmlNodePtr = ^xmlNode;
  xmlNodePtrPtr = ^xmlNodePtr;
  xmlDtdPtr = ^xmlDtd;
  xmlAttrPtr = ^xmlAttr;
  xmlIDPtr = ^xmlID;
  xmlRefPtr = ^xmlRef;
  xmlDocPtr = ^xmlDoc;
  xmlDOMWrapCtxtPtr = ^xmlDOMWrapCtxt;
  xmlBufferAllocationSchemePtr = ^xmlBufferAllocationScheme;
{$ENDIF}

(*
 * Some of the basic types pointer to structures:
 *)

{$IFDEF CONST}
(**
 * BASE_BUFFER_SIZE:
 *
 * default buffer size 4000.
 *)
  BASE_BUFFER_SIZE = 4096;

(**
 * LIBXML_NAMESPACE_DICT:
 *
 * Defines experimental behaviour:
 * 1) xmlNs gets an additional field @context (a xmlDoc)
 * 2) when creating a tree, xmlNs->href is stored in the dict of xmlDoc.
 *)
{.$DEFINE LIBXML_NAMESPACE_DICT}

(**
 * XML_XML_NAMESPACE:
 *
 * This is the namespace for the special xml: prefix predefined in the
 * XML Namespace specification.
 *)
  XML_XML_NAMESPACE: xmlCharPtr = 'http://www.w3.org/XML/1998/namespace';

(**
 * XML_XML_ID:
 *
 * This is the name for the special xml:id attribute
 *)
  XML_XML_ID: xmlCharPtr = 'xml:id';
{$ENDIF}

{$IFDEF TYPE}
(**
 * xmlBufferAllocationScheme:
 *
 * A buffer allocation scheme can be defined to either match exactly the
 * need or double it's allocated size each time it is found too small.
 *)
  xmlBufferAllocationScheme = (
    XML_BUFFER_ALLOC_DOUBLEIT,
    XML_BUFFER_ALLOC_EXACT,
    XML_BUFFER_ALLOC_IMMUTABLE
  );

(**
 * xmlBuffer:
 *
 * A buffer structure.
 *)
  xmlBuffer = record
    content : xmlCharPtr;   (* The buffer content UTF8 *)
    use     : cuint;      (* The buffer size used *)
    size    : cuint;      (* The buffer size *)
    alloc   : xmlBufferAllocationScheme; (* The realloc method *)
  end;

(*
 * The different element types carried by an XML tree.
 *
 * NOTE: This is synchronized with DOM Level1 values
 *       See http://www.w3.org/TR/REC-DOM-Level-1/
 *
 * Actually this had diverged a bit, and now XML_DOCUMENT_TYPE_NODE should
 * be deprecated to use an XML_DTD_NODE.
 *)
  xmlElementType = (
    XML_ELEMENT_NODE=		1,
    XML_ATTRIBUTE_NODE=		2,
    XML_TEXT_NODE=		3,
    XML_CDATA_SECTION_NODE=	4,
    XML_ENTITY_REF_NODE=	5,
    XML_ENTITY_NODE=		6,
    XML_PI_NODE=		7,
    XML_COMMENT_NODE=		8,
    XML_DOCUMENT_NODE=		9,
    XML_DOCUMENT_TYPE_NODE=	10,
    XML_DOCUMENT_FRAG_NODE=	11,
    XML_NOTATION_NODE=		12,
    XML_HTML_DOCUMENT_NODE=	13,
    XML_DTD_NODE=		14,
    XML_ELEMENT_DECL=		15,
    XML_ATTRIBUTE_DECL=		16,
    XML_ENTITY_DECL=		17,
    XML_NAMESPACE_DECL=		18,
    XML_XINCLUDE_START=		19,
    XML_XINCLUDE_END=		20
{$IFDEF LIBXML_DOCB_ENABLED}
   ,XML_DOCB_DOCUMENT_NODE=	21
{$ENDIF}
  );

(**
 * xmlNotation:
 *
 * A DTD Notation definition.
 *)
  xmlNotation = record
    name      : xmlCharPtr;          (* Notation name *)
    PublicID  : xmlCharPtr;  (* Public identifier, if any *)
    SystemID  : xmlCharPtr;  (* System identifier, if any *)
  end;

(**
 * xmlAttributeType:
 *
 * A DTD Attribute type definition.
 *)
  xmlAttributeType = (
    XML_ATTRIBUTE_CDATA = 1,
    XML_ATTRIBUTE_ID,
    XML_ATTRIBUTE_IDREF	,
    XML_ATTRIBUTE_IDREFS,
    XML_ATTRIBUTE_ENTITY,
    XML_ATTRIBUTE_ENTITIES,
    XML_ATTRIBUTE_NMTOKEN,
    XML_ATTRIBUTE_NMTOKENS,
    XML_ATTRIBUTE_ENUMERATION,
    XML_ATTRIBUTE_NOTATION
  );

(**
 * xmlAttributeDefault:
 *
 * A DTD Attribute default definition.
 *)
  xmlAttributeDefault = (
    XML_ATTRIBUTE_NONE = 1,
    XML_ATTRIBUTE_REQUIRED,
    XML_ATTRIBUTE_IMPLIED,
    XML_ATTRIBUTE_FIXED
  );

(**
 * xmlEnumeration:
 *
 * List structure used when there is an enumeration in DTDs.
 *)
  xmlEnumeration = record
    next: xmlEnumerationPtr; (* next one *)
    name: xmlCharPtr;
  end;

(**
 * xmlAttribute:
 *
 * An Attribute declaration in a DTD.
 *)
  xmlAttribute = record
    _private      : pointer;        (* application data *)
    _type         : xmlElementType;       (* XML_ATTRIBUTE_DECL, must be second ! *)
    name          : xmlCharPtr;	(* Attribute name *)
    children      : xmlNodePtr;	(* NULL *)
    last          : xmlNodePtr;	(* NULL *)
    parent        : xmlDtdPtr;	(* -> DTD *)
    next          : xmlNodePtr;	(* next sibling link  *)
    prev          : xmlNodePtr;	(* previous sibling link  *)
    doc           : xmlDocPtr;       (* the containing document *)

    nexth         : xmlAttributePtr;	(* next in hash table *)
    atype         : xmlAttributeType;	(* The attribute type *)
    def           : xmlAttributeDefault;	(* the default *)
    defaultValue  : xmlCharPtr;	(* or the default value *)
    tree          : xmlEnumerationPtr;       (* or the enumeration tree if any *)
    prefix        : xmlCharPtr;	(* the namespace prefix if any *)
    elem          : xmlCharPtr;	(* Element holding the attribute *)
  end;

(**
 * xmlElementContentType:
 *
 * Possible definitions of element content types.
 *)
  xmlElementContentType = (
    XML_ELEMENT_CONTENT_PCDATA = 1,
    XML_ELEMENT_CONTENT_ELEMENT,
    XML_ELEMENT_CONTENT_SEQ,
    XML_ELEMENT_CONTENT_OR
  );

(**
 * xmlElementContentOccur:
 *
 * Possible definitions of element content occurrences.
 *)
  xmlElementContentOccur = (
    XML_ELEMENT_CONTENT_ONCE = 1,
    XML_ELEMENT_CONTENT_OPT,
    XML_ELEMENT_CONTENT_MULT,
    XML_ELEMENT_CONTENT_PLUS
  );

(**
 * xmlElementContent:
 *
 * An XML Element content as stored after parsing an element definition
 * in a DTD.
 *)
  xmlElementContent = record
    _type   : xmlElementContentType;	(* PCDATA, ELEMENT, SEQ or OR *)
    ocur    : xmlElementContentOccur;	(* ONCE, OPT, MULT or PLUS *)
    name    : xmlCharPtr;	(* Element name *)
    c1      : xmlElementContentPtr;	(* first child *)
    c2      : xmlElementContentPtr;	(* second child *)
    parent  : xmlElementContentPtr;	(* parent *)
    prefix  : xmlCharPtr;	(* Namespace prefix *)
  end;

(**
 * xmlElementTypeVal:
 *
 * The different possibilities for an element content type.
 *)
  xmlElementTypeVal = (
    XML_ELEMENT_TYPE_UNDEFINED = 0,
    XML_ELEMENT_TYPE_EMPTY = 1,
    XML_ELEMENT_TYPE_ANY,
    XML_ELEMENT_TYPE_MIXED,
    XML_ELEMENT_TYPE_ELEMENT
  );


(**
 * xmlElement:
 *
 * An XML Element declaration from a DTD.
 *)
  xmlElement = record
    _private    : pointer;	        (* application data *)
    _type       : xmlElementType;       (* XML_ELEMENT_DECL, must be second ! *)
    name        : xmlCharPtr;	(* Element name *)
    children    : xmlNodePtr;	(* NULL *)
    last        : xmlNodePtr;	(* NULL *)
    parent      : xmlDtdPtr;	(* -> DTD *)
    next        : xmlNodePtr;	(* next sibling link  *)
    prev        : xmlNodePtr;	(* previous sibling link  *)
    doc         : xmlDocPtr;       (* the containing document *)

    etype       : xmlElementTypeVal;	(* The type *)
    content     : xmlElementContentPtr;	(* the allowed element content *)
    attributes  : xmlAttributePtr;	(* List of the declared attributes *)
    prefix      : xmlCharPtr;	(* the namespace prefix if any *)
{$IFDEF LIBXML_REGEXP_ENABLED}
    contModel   : xmlRegexpPtr;	(* the validating regexp *)
{$ELSE}
    contModel   : pointer;
{$ENDIF}
  end;

(**
 * XML_LOCAL_NAMESPACE:
 *
 * A namespace declaration node.
 *)
{$ENDIF}

{$IFDEF CONST}
  XML_LOCAL_NAMESPACE = XML_NAMESPACE_DECL;
{$ENDIF}

{$IFDEF TYPE}
  xmlNsType = xmlElementType;

(**
 * xmlNs:
 *
 * An XML namespace.
 * Note that prefix == NULL is valid, it defines the default namespace
 * within the subtree (until overridden).
 *
 * xmlNsType is unified with xmlElementType.
 *)
  xmlNs = record
    next      : xmlNsPtr;	(* next Ns link for this node  *)
    _type     : xmlNsType;	(* global or local *)
    href      : xmlCharPtr;	(* URL for the namespace *)
    prefix    : xmlCharPtr;	(* prefix for the namespace *)
    _private  : pointer;   (* application data *)
    context   : xmlDocPtr;		(* normally an xmlDoc *)
  end;

(**
 * xmlDtd:
 *
 * An XML DTD, as defined by <!DOCTYPE ... There is actually one for
 * the internal subset and for the external subset.
 *)
  xmlDtd = record
    _private    : pointer;	(* application data *)
    _type       : xmlElementType;       (* XML_DTD_NODE, must be second ! *)
    name        : xmlCharPtr;	(* Name of the DTD *)
    children    : xmlNodePtr;	(* the value of the property link *)
    last        : xmlNodePtr;	(* last child link *)
    parent      : xmlDocPtr;	(* child->parent link *)
    next        : xmlNodePtr;	(* next sibling link  *)
    prev        : xmlNodePtr;	(* previous sibling link  *)
    doc         : xmlDocPtr;	(* the containing document *)

    (* End of common part *)
    notations   : pointer;   (* Hash table for notations if any *)
    elements    : pointer;    (* Hash table for elements if any *)
    attributes  : pointer;  (* Hash table for attributes if any *)
    entities    : pointer;    (* Hash table for entities if any *)
    ExternalID  : xmlCharPtr;	(* External identifier for PUBLIC DTD *)
    SystemID    : xmlCharPtr;	(* URI for a SYSTEM or PUBLIC DTD *)
    pentities   : pointer;   (* Hash table for param entities if any *)
  end;

(**
 * xmlAttr:
 *
 * An attribute on an XML node.
 *)
  xmlAttr = record
    _private    : pointer;	(* application data *)
    _type       : xmlElementType;      (* XML_ATTRIBUTE_NODE, must be second ! *)
    name        : xmlCharPtr;      (* the name of the property *)
    children    : xmlNodePtr;	(* the value of the property *)
    last        : xmlNodePtr;	(* NULL *)
    parent      : xmlNodePtr;	(* child->parent link *)
    next        : xmlAttrPtr;	(* next sibling link  *)
    prev        : xmlAttrPtr;	(* previous sibling link  *)
    doc         : xmlDocPtr;	(* the containing document *)
    ns          : xmlNsPtr;        (* pointer to the associated namespace *)
    atype       : xmlAttributeType;     (* the attribute type if validating *)
    psvi        : pointer;	(* for type/PSVI informations *)
  end;

(**
 * xmlID:
 *
 * An XML ID instance.
 *)
  xmlID = record
    next    : xmlIDPtr;	(* next ID *)
    value   : xmlCharPtr;	(* The ID name *)
    attr    : xmlAttrPtr;	(* The attribute holding it *)
    name    : xmlCharPtr;	(* The attribute if attr is not available *)
    lineno  : cint;	(* The line number if attr is not available *)
    doc     : xmlDocPtr;	(* The document holding the ID *)
  end;

(**
 * xmlRef:
 *
 * An XML IDREF instance.
 *)
  xmlRef = record
    next    : xmlRefPtr;	(* next Ref *)
    value   : xmlCharPtr;	(* The Ref name *)
    attr    : xmlAttrPtr;	(* The attribute holding it *)
    name    : xmlCharPtr;	(* The attribute if attr is not available *)
    lineno  : cint;	(* The line number if attr is not available *)
  end;

(**
 * xmlNode:
 *
 * A node in an XML tree.
 *)
  xmlNode = record
    _private    : pointer;	(* application data *)
    _type       : xmlElementType;	(* type number, must be second ! *)
    name        : xmlCharPtr;      (* the name of the node, or the entity *)
    children    : xmlNodePtr;	(* parent->childs link *)
    last        : xmlNodePtr;	(* last child link *)
    parent      : xmlNodePtr;	(* child->parent link *)
    next        : xmlNodePtr;	(* next sibling link  *)
    prev        : xmlNodePtr;	(* previous sibling link  *)
    doc         : xmlDocPtr;	(* the containing document *)

    (* End of common part *)
    ns          : xmlNsPtr;        (* pointer to the associated namespace *)
    content     : xmlCharPtr;   (* the content *)
    properties  : xmlAttrPtr;(* properties list *)
    nsDef       : xmlNsPtr;     (* namespace definitions on this node *)
    psvi        : pointer;	(* for type/PSVI informations *)
    line        : cushort;	(* line number *)
    extra       : cushort;	(* extra data for XPath/XSLT *)
  end;
{$ENDIF}

{$IFDEF FUNCTION}
(**
 * XML_GET_CONTENT:
 *
 * Macro to extract the content pointer of a node.
 *)
function XML_GET_CONTENT(n: pointer): xmlCharPtr;

(**
 * XML_GET_LINE:
 *
 * Macro to extract the line number of an element node.
 *)
{#define XML_GET_LINE(n)						\
    (xmlGetLineNo(n))}
{$ENDIF}

{$IFDEF TYPE}
(**
 * xmlDoc:
 *
 * An XML document.
 *)
  xmlDoc = record
    _private    : pointer;	(* application data *)
    _type       : xmlElementType;       (* XML_DOCUMENT_NODE, must be second ! *)
    name        : pchar;	(* name/filename/URI of the document *)
    children    : xmlCharPtr; (* the document tree *)
    last        : xmlCharPtr;	(* last child link *)
    parent      : xmlCharPtr;	(* child->parent link *)
    next        : xmlCharPtr;	(* next sibling link  *)
    prev        : xmlCharPtr;	(* previous sibling link  *)
    doc         : xmlDocPtr;	(* autoreference to itself *)

    (* End of common part *)
    compression : cint; (* level of zlib compression *)
    standalone  : cint; (* standalone document (no external refs)
				     1 if standalone="yes"
				     0 if standalone="no"
				    -1 if there is no XML declaration
				    -2 if there is an XML declaration, but no
					standalone attribute was specified *)
    intSubset   : xmlDtdPtr;	(* the document internal subset *)
    extSubset   : xmlDtdPtr;	(* the document external subset *)
    oldNs       : xmlNsPtr;	(* Global namespace, the old way *)
    version     : xmlCharPtr;	(* the XML version string *)
    encoding    : xmlCharPtr;   (* external initial encoding, if any *)
    ids         : pointer;        (* Hash table for ID attributes if any *)
    refs        : pointer;       (* Hash table for IDREFs attributes if any *)
    URL         : xmlCharPtr;	(* The URI for that document *)
    charset     : cint;    (* encoding of the in-memory content
				   actually an xmlCharEncoding *)
    dict        : xmlDictPtr;      (* dict used to allocate names or NULL *)
    psvi        : pointer;	(* for type/PSVI informations *)
  end;


(**
 * xmlDOMWrapAcquireNsFunction:
 * @ctxt:  a DOM wrapper context
 * @node:  the context node (element or attribute)
 * @nsName:  the requested namespace name
 * @nsPrefix:  the requested namespace prefix
 *
 * A function called to acquire namespaces (xmlNs) from the wrapper.
 *
 * Returns an xmlNsPtr or NULL in case of an error.
 *)
  xmlDOMWrapAcquireNsFunction = function (ctxt: xmlDOMWrapCtxtPtr; node: xmlNodePtr; nsName, nsPrefix: xmlCharPtr): xmlNsPtr; XMLCALL;

(**
 * xmlDOMWrapCtxt:
 *
 * Context for DOM wrapper-operations.
 *)
  xmlDOMWrapCtxt = record
    _private: pointer;
    (*
    * The type of this context, just in case we need specialized
    * contexts in the future.
    *)
    _type: cint;
    (*
    * Internal namespace map used for various operations.
    *)
    namespaceMap: pointer;
    (*
    * Use this one to acquire an xmlNsPtr intended for node->ns.
    * (Note that this is not intended for elem->nsDef).
    *)
    getNsForNodeFunc: xmlDOMWrapAcquireNsFunction;
  end;

(**
 * xmlChildrenNode:
 *
 * Macro for compatibility naming layer with libxml1. Maps
 * to "children."
 *)
{#ifndef xmlChildrenNode
#define xmlChildrenNode children
#endif}

(**
 * xmlRootNode:
 *
 * Macro for compatibility naming layer with libxml1. Maps
 * to "children".
 *)
{#ifndef xmlRootNode
#define xmlRootNode children
#endif}
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Variables.
 *)
(*
 * Some helper functions
 *)
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_XPATH_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED) or
  defined(LIBXML_DEBUG_ENABLED) or defined (LIBXML_HTML_ENABLED) or defined(LIBXML_SAX1_ENABLED) or
  defined(LIBXML_HTML_ENABLED) or defined(LIBXML_WRITER_ENABLED) or defined(LIBXML_DOCB_ENABLED)}
function xmlValidateNCName(value: xmlCharPtr; space: cint): cint; XMLCALL; XMLPUBFUN;
{$ENDIF}

{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
function xmlValidateQName(value: xmlCharPtr; space: cint): cint; XMLCALL; XMLPUBFUN;
function xmlValidateName(value: xmlCharPtr; space: cint): cint; XMLCALL; XMLPUBFUN;
function xmlValidateNMToken(value: xmlCharPtr; space: cint): cint; XMLCALL; XMLPUBFUN;
{$ENDIF}

function xmlValidateQName(ncname, prefix, memory: xmlCharPtr; len: cint): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlSplitQName2(name: xmlCharPtr; var prefix: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlSplitQName3(name: xmlCharPtr; var prefix: xmlCharPtr; var len: cint): xmlCharPtr; XMLCALL; XMLPUBFUN;

(*
 * Handling Buffers.
 *)
procedure xmlSetBufferAllocationScheme(scheme: xmlBufferAllocationScheme); XMLCALL; XMLPUBFUN;
function xmlGetBufferAllocationScheme: xmlBufferAllocationScheme; XMLCALL; XMLPUBFUN;

function xmlBufferCreate: xmlBufferPtr; XMLCALL; XMLPUBFUN;
function xmlBufferCreateSize(size: size_t): xmlBufferPtr; XMLCALL; XMLPUBFUN;
function xmlBufferCreateStatic(mem: pointer; size: size_t): xmlBufferPtr; XMLCALL; XMLPUBFUN;
function xmlBufferResize(buf: xmlBufferPtr; size: cuint): cint; XMLCALL; XMLPUBFUN;
procedure xmlBufferFree(buf: xmlBufferPtr); XMLCALL; XMLPUBFUN;
procedure xmlBufferDump(fp: PFILE; buf: xmlBufferPtr); XMLCALL; XMLPUBFUN;
function xmlBufferAdd(buf: xmlBufferPtr; str: xmlCharPtr; len: cint): cint; XMLCALL; XMLPUBFUN;
function xmlBufferAddHead(buf: xmlBufferPtr; str: xmlCharPtr; len: cint): cint; XMLCALL; XMLPUBFUN;
function xmlBufferCat(buf: xmlBufferPtr; str: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlBufferCCat(buf: xmlBufferPtr; str: pchar): cint; XMLCALL; XMLPUBFUN;
function xmlBufferShrink(buf: xmlBufferPtr; len: cuint): cint; XMLCALL; XMLPUBFUN;
function xmlBufferGrow(buf: xmlBufferPtr; len: cuint): cint; XMLCALL; XMLPUBFUN;
procedure xmlBufferEmpty(buf: xmlBufferPtr); XMLCALL; XMLPUBFUN;
function xmlBufferContent(buf: xmlBufferPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
procedure xmlBufferSetAllocationScheme(buf: xmlBufferPtr; scheme: xmlBufferAllocationScheme); XMLCALL; XMLPUBFUN;
function xmlBufferLength(buf: xmlBufferPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * Creating/freeing new structures.
 *)
function xmlCreateIntSubset(doc: xmlDocPtr; name, ExternalID, SystemID: xmlCharPtr): xmlDtdPtr; XMLCALL; XMLPUBFUN;
function xmlNewDtd(doc: xmlDocPtr; name, ExternalID, SystemID: xmlCharPtr): xmlDtdPtr; XMLCALL; XMLPUBFUN;
function xmlGetIntSubset(doc: xmlDocPtr): xmlDtdPtr; XMLCALL; XMLPUBFUN;
procedure xmlFreeDtd(cur: xmlDtdPtr); XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_LEGACY_ENABLED}
function xmlNewGlobalNs(doc: xmlDocPtr; href, prefix: xmlCharPtr): xmlNsPtr; XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_LEGACY_ENABLED *)
function xmlNewNs(node: xmlNodePtr; href, prefix: xmlCharPtr): xmlNsPtr; XMLCALL; XMLPUBFUN;
procedure xmlFreeNs(cur: xmlNsPtr); XMLCALL; XMLPUBFUN;
procedure xmlFreeNsList(cur: xmlNsPtr); XMLCALL; XMLPUBFUN;
function xmlNewDoc(version: xmlCharPtr): xmlDocPtr; XMLCALL; XMLPUBFUN;
procedure xmlFreeDoc(cur: xmlDocPtr); XMLCALL; XMLPUBFUN;
function xmlNewDocProp(doc: xmlDocPtr; name, value: xmlCharPtr): xmlAttrPtr; XMLCALL; XMLPUBFUN;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_HTML_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
function xmlNewProp(node: xmlNodePtr; name, value: xmlCharPtr): xmlAttrPtr; XMLCALL; XMLPUBFUN;
{$ENDIF}
function xmlNewNsProp(node: xmlNodePtr; ns: xmlNsPtr; name, value: xmlCharPtr): xmlAttrPtr; XMLCALL; XMLPUBFUN;
function xmlNewNsPropEatName(node: xmlNodePtr; ns: xmlNsPtr; name, value: xmlCharPtr): xmlAttrPtr; XMLCALL; XMLPUBFUN;
procedure xmlFreePropList(cur: xmlAttrPtr); XMLCALL; XMLPUBFUN;
procedure xmlFreeProp(cur: xmlAttrPtr); XMLCALL; XMLPUBFUN;
function xmlCopyProp(target: xmlNodePtr; cur: xmlAttrPtr): xmlAttrPtr; XMLCALL; XMLPUBFUN;
function xmlCopyPropList(target: xmlNodePtr; cur: xmlAttrPtr): xmlAttrPtr; XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_TREE_ENABLED}
function xmlCopyDtd(dtd: xmlDtdPtr): xmlDtdPtr; XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
function xmlCopyDoc(doc: xmlDocPtr; recursive: cint): xmlDocPtr; XMLCALL; XMLPUBFUN;
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED) *)

{$IFDEF 0}
(*
 * Creating new nodes.
 *)
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewDocNode		(xmlDocPtr doc,
					 xmlNsPtr ns,
					 xmlChar *name,
					 xmlChar *content);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewDocNodeEatName	(xmlDocPtr doc,
					 xmlNsPtr ns,
					 xmlChar *name,
					 xmlChar *content);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewNode		(xmlNsPtr ns,
					 xmlChar *name);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewNodeEatName	(xmlNsPtr ns,
					 xmlChar *name);
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewChild		(xmlNodePtr parent,
					 xmlNsPtr ns,
					 xmlChar *name,
					 xmlChar *content);
{$ENDIF}
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewDocText		(xmlDocPtr doc,
					 xmlChar *content);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewText		(xmlChar *content);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewDocPI		(xmlDocPtr doc,
					 xmlChar *name,
					 xmlChar *content);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewPI		(xmlChar *name,
					 xmlChar *content);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewDocTextLen	(xmlDocPtr doc,
					 xmlChar *content,
					 int len);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewTextLen		(xmlChar *content,
					 int len);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewDocComment	(xmlDocPtr doc,
					 xmlChar *content);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewComment		(xmlChar *content);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewCDataBlock	(xmlDocPtr doc,
					 xmlChar *content,
					 int len);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewCharRef		(xmlDocPtr doc,
					 xmlChar *name);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewReference		(xmlDocPtr doc,
					 xmlChar *name);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlCopyNode		(xmlNodePtr node,
					 int recursive);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlDocCopyNode		(xmlNodePtr node,
					 xmlDocPtr doc,
					 int recursive);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlDocCopyNodeList	(xmlDocPtr doc,
					 xmlNodePtr node);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlCopyNodeList		(xmlNodePtr node);
{$IFDEF LIBXML_TREE_ENABLED}
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewTextChild		(xmlNodePtr parent,
					 xmlNsPtr ns,
					 xmlChar *name,
					 xmlChar *content);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewDocRawNode	(xmlDocPtr doc,
					 xmlNsPtr ns,
					 xmlChar *name,
					 xmlChar *content);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlNewDocFragment	(xmlDocPtr doc);
{$ENDIF} (* LIBXML_TREE_ENABLED *)

(*
 * Navigating.
 *)
XMLPUBFUN long XMLCALL
		xmlGetLineNo		(xmlNodePtr node);
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_DEBUG_ENABLED)}
XMLPUBFUN xmlChar * XMLCALL
		xmlGetNodePath		(xmlNodePtr node);
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_DEBUG_ENABLED) *)
XMLPUBFUN xmlNodePtr XMLCALL
		xmlDocGetRootElement	(xmlDocPtr doc);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlGetLastChild		(xmlNodePtr parent);
XMLPUBFUN int XMLCALL
		xmlNodeIsText		(xmlNodePtr node);
XMLPUBFUN int XMLCALL
		xmlIsBlankNode		(xmlNodePtr node);

(*
 * Changing the structure.
 *)
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_WRITER_ENABLED)}
XMLPUBFUN xmlNodePtr XMLCALL
		xmlDocSetRootElement	(xmlDocPtr doc,
					 xmlNodePtr root);
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_WRITER_ENABLED) *)
{$IFDEF LIBXML_TREE_ENABLED}
XMLPUBFUN void XMLCALL
		xmlNodeSetName		(xmlNodePtr cur,
					 xmlChar *name);
{$ENDIF} (* LIBXML_TREE_ENABLED *)
XMLPUBFUN xmlNodePtr XMLCALL
		xmlAddChild		(xmlNodePtr parent,
					 xmlNodePtr cur);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlAddChildList		(xmlNodePtr parent,
					 xmlNodePtr cur);
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_WRITER_ENABLED)}
XMLPUBFUN xmlNodePtr XMLCALL
		xmlReplaceNode		(xmlNodePtr old,
					 xmlNodePtr cur);
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_WRITER_ENABLED) *)
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_HTML_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
XMLPUBFUN xmlNodePtr XMLCALL
		xmlAddPrevSibling	(xmlNodePtr cur,
					 xmlNodePtr elem);
{$ENDIF} (* LIBXML_TREE_ENABLED || LIBXML_HTML_ENABLED || LIBXML_SCHEMAS_ENABLED *)
XMLPUBFUN xmlNodePtr XMLCALL
		xmlAddSibling		(xmlNodePtr cur,
					 xmlNodePtr elem);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlAddNextSibling	(xmlNodePtr cur,
					 xmlNodePtr elem);
XMLPUBFUN void XMLCALL
		xmlUnlinkNode		(xmlNodePtr cur);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlTextMerge		(xmlNodePtr first,
					 xmlNodePtr second);
XMLPUBFUN int XMLCALL
		xmlTextConcat		(xmlNodePtr node,
					 xmlChar *content,
					 int len);
XMLPUBFUN void XMLCALL
		xmlFreeNodeList		(xmlNodePtr cur);
XMLPUBFUN void XMLCALL
		xmlFreeNode		(xmlNodePtr cur);
XMLPUBFUN void XMLCALL
		xmlSetTreeDoc		(xmlNodePtr tree,
					 xmlDocPtr doc);
XMLPUBFUN void XMLCALL
		xmlSetListDoc		(xmlNodePtr list,
					 xmlDocPtr doc);
(*
 * Namespaces.
 *)
XMLPUBFUN xmlNsPtr XMLCALL
		xmlSearchNs		(xmlDocPtr doc,
					 xmlNodePtr node,
					 xmlChar *nameSpace);
XMLPUBFUN xmlNsPtr XMLCALL
		xmlSearchNsByHref	(xmlDocPtr doc,
					 xmlNodePtr node,
					 xmlChar *href);
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_XPATH_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
XMLPUBFUN xmlNsPtr * XMLCALL
		xmlGetNsList		(xmlDocPtr doc,
					 xmlNodePtr node);
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_XPATH_ENABLED) *)

XMLPUBFUN void XMLCALL
		xmlSetNs		(xmlNodePtr node,
					 xmlNsPtr ns);
XMLPUBFUN xmlNsPtr XMLCALL
		xmlCopyNamespace	(xmlNsPtr cur);
XMLPUBFUN xmlNsPtr XMLCALL
		xmlCopyNamespaceList	(xmlNsPtr cur);

(*
 * Changing the content.
 *)
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_XINCLUDE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED) or defined(LIBXML_HTML_ENABLED)}
XMLPUBFUN xmlAttrPtr XMLCALL
		xmlSetProp		(xmlNodePtr node,
					 xmlChar *name,
					 xmlChar *value);
XMLPUBFUN xmlAttrPtr XMLCALL
		xmlSetNsProp		(xmlNodePtr node,
					 xmlNsPtr ns,
					 xmlChar *name,
					 xmlChar *value);
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_XINCLUDE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED) || defined(LIBXML_HTML_ENABLED) *)
XMLPUBFUN xmlChar * XMLCALL
		xmlGetNoNsProp		(xmlNodePtr node,
					 xmlChar *name);
XMLPUBFUN xmlChar * XMLCALL
		xmlGetProp		(xmlNodePtr node,
					 xmlChar *name);
XMLPUBFUN xmlAttrPtr XMLCALL
		xmlHasProp		(xmlNodePtr node,
					 xmlChar *name);
XMLPUBFUN xmlAttrPtr XMLCALL
		xmlHasNsProp		(xmlNodePtr node,
					 xmlChar *name,
					 xmlChar *nameSpace);
XMLPUBFUN xmlChar * XMLCALL
		xmlGetNsProp		(xmlNodePtr node,
					 xmlChar *name,
					 xmlChar *nameSpace);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlStringGetNodeList	(xmlDocPtr doc,
					 xmlChar *value);
XMLPUBFUN xmlNodePtr XMLCALL
		xmlStringLenGetNodeList	(xmlDocPtr doc,
					 xmlChar *value,
					 int len);
XMLPUBFUN xmlChar * XMLCALL
		xmlNodeListGetString	(xmlDocPtr doc,
					 xmlNodePtr list,
					 int inLine);
{$IFDEF LIBXML_TREE_ENABLED}
XMLPUBFUN xmlChar * XMLCALL
		xmlNodeListGetRawString	(xmlDocPtr doc,
					 xmlNodePtr list,
					 int inLine);
{$ENDIF} (* LIBXML_TREE_ENABLED *)
XMLPUBFUN void XMLCALL
		xmlNodeSetContent	(xmlNodePtr cur,
					 xmlChar *content);
{$IFDEF LIBXML_TREE_ENABLED}
XMLPUBFUN void XMLCALL
		xmlNodeSetContentLen	(xmlNodePtr cur,
					 xmlChar *content,
					 int len);
{$ENDIF} (* LIBXML_TREE_ENABLED *)
XMLPUBFUN void XMLCALL
		xmlNodeAddContent	(xmlNodePtr cur,
					 xmlChar *content);
XMLPUBFUN void XMLCALL
		xmlNodeAddContentLen	(xmlNodePtr cur,
					 xmlChar *content,
					 int len);
XMLPUBFUN xmlChar * XMLCALL
		xmlNodeGetContent	(xmlNodePtr cur);
XMLPUBFUN int XMLCALL
		xmlNodeBufGetContent	(xmlBufferPtr buffer,
					 xmlNodePtr cur);
XMLPUBFUN xmlChar * XMLCALL
		xmlNodeGetLang		(xmlNodePtr cur);
XMLPUBFUN int XMLCALL
		xmlNodeGetSpacePreserve	(xmlNodePtr cur);
{$IFDEF LIBXML_TREE_ENABLED}
XMLPUBFUN void XMLCALL
		xmlNodeSetLang		(xmlNodePtr cur,
					 xmlChar *lang);
XMLPUBFUN void XMLCALL
		xmlNodeSetSpacePreserve (xmlNodePtr cur,
					 int val);
{$ENDIF} (* LIBXML_TREE_ENABLED *)
XMLPUBFUN xmlChar * XMLCALL
		xmlNodeGetBase		(xmlDocPtr doc,
					 xmlNodePtr cur);
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_XINCLUDE_ENABLED)}
XMLPUBFUN void XMLCALL
		xmlNodeSetBase		(xmlNodePtr cur,
					 xmlChar *uri);
{$ENDIF}

(*
 * Removing content.
 *)
XMLPUBFUN int XMLCALL
		xmlRemoveProp		(xmlAttrPtr cur);
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
XMLPUBFUN int XMLCALL
		xmlUnsetNsProp		(xmlNodePtr node,
					 xmlNsPtr ns,
					 xmlChar *name);
XMLPUBFUN int XMLCALL
		xmlUnsetProp		(xmlNodePtr node,
					 xmlChar *name);
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED) *)

(*
 * Internal, don't use.
 *)
XMLPUBFUN void XMLCALL
		xmlBufferWriteCHAR	(xmlBufferPtr buf,
					 xmlChar *string);
XMLPUBFUN void XMLCALL
		xmlBufferWriteChar	(xmlBufferPtr buf,
					 char *string);
XMLPUBFUN void XMLCALL
		xmlBufferWriteQuotedString(xmlBufferPtr buf,
					 xmlChar *string);

{$IFDEF LIBXML_OUTPUT_ENABLED}
XMLPUBFUN void xmlAttrSerializeTxtContent(xmlBufferPtr buf,
					 xmlDocPtr doc,
					 xmlAttrPtr attr,
					 xmlChar *string);
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

{$IFDEF LIBXML_TREE_ENABLED}
(*
 * Namespace handling.
 *)
XMLPUBFUN int XMLCALL
		xmlReconciliateNs	(xmlDocPtr doc,
					 xmlNodePtr tree);
{$ENDIF}

{$IFDEF LIBXML_OUTPUT_ENABLED}
(*
 * Saving.
 *)
XMLPUBFUN void XMLCALL
		xmlDocDumpFormatMemory	(xmlDocPtr cur,
					 xmlChar **mem,
					 int *size,
					 int format);
XMLPUBFUN void XMLCALL
		xmlDocDumpMemory	(xmlDocPtr cur,
					 xmlChar **mem,
					 int *size);
XMLPUBFUN void XMLCALL
		xmlDocDumpMemoryEnc	(xmlDocPtr out_doc,
					 xmlChar **doc_txt_ptr,
					 int * doc_txt_len,
					 char *txt_encoding);
XMLPUBFUN void XMLCALL
		xmlDocDumpFormatMemoryEnc(xmlDocPtr out_doc,
					 xmlChar **doc_txt_ptr,
					 int * doc_txt_len,
					 char *txt_encoding,
					 int format);
XMLPUBFUN int XMLCALL
		xmlDocFormatDump	(FILE *f,
					 xmlDocPtr cur,
					 int format);
XMLPUBFUN int XMLCALL
		xmlDocDump		(FILE *f,
					 xmlDocPtr cur);
XMLPUBFUN void XMLCALL
		xmlElemDump		(FILE *f,
					 xmlDocPtr doc,
					 xmlNodePtr cur);
XMLPUBFUN int XMLCALL
		xmlSaveFile		(char *filename,
					 xmlDocPtr cur);
XMLPUBFUN int XMLCALL
		xmlSaveFormatFile	(char *filename,
					 xmlDocPtr cur,
					 int format);
XMLPUBFUN int XMLCALL
		xmlNodeDump		(xmlBufferPtr buf,
					 xmlDocPtr doc,
					 xmlNodePtr cur,
					 int level,
					 int format);

XMLPUBFUN int XMLCALL
		xmlSaveFileTo		(xmlOutputBufferPtr buf,
					 xmlDocPtr cur,
					 char *encoding);
XMLPUBFUN int XMLCALL
		xmlSaveFormatFileTo     (xmlOutputBufferPtr buf,
					 xmlDocPtr cur,
				         char *encoding,
				         int format);
XMLPUBFUN void XMLCALL
		xmlNodeDumpOutput	(xmlOutputBufferPtr buf,
					 xmlDocPtr doc,
					 xmlNodePtr cur,
					 int level,
					 int format,
					 char *encoding);

XMLPUBFUN int XMLCALL
		xmlSaveFormatFileEnc    (char *filename,
					 xmlDocPtr cur,
					 char *encoding,
					 int format);

XMLPUBFUN int XMLCALL
		xmlSaveFileEnc		(char *filename,
					 xmlDocPtr cur,
					 char *encoding);

{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
(*
 * XHTML
 *)
XMLPUBFUN int XMLCALL
		xmlIsXHTML		(xmlChar *systemID,
					 xmlChar *publicID);

(*
 * Compression.
 *)
XMLPUBFUN int XMLCALL
		xmlGetDocCompressMode	(xmlDocPtr doc);
XMLPUBFUN void XMLCALL
		xmlSetDocCompressMode	(xmlDocPtr doc,
					 int mode);
XMLPUBFUN int XMLCALL
		xmlGetCompressMode	(void);
XMLPUBFUN void XMLCALL
		xmlSetCompressMode	(int mode);
{$ENDIF}

(*
* DOM-wrapper helper functions.
*)
function xmlDOMWrapNewCtxt: xmlDOMWrapCtxtPtr; XMLCALL; XMLPUBFUN;
procedure xmlDOMWrapNewCtxt(ctxt: xmlDOMWrapCtxtPtr); XMLCALL; XMLPUBFUN;
function xmlDOMWrapReconcileNamespaces(ctxt: xmlDOMWrapCtxtPtr; elem: xmlNodePtr; options: cint): cint; XMLCALL; XMLPUBFUN;
function xmlDOMWrapAdoptNode(ctxt: xmlDOMWrapCtxtPtr; sourceDoc: xmlDocPtr; node: xmlNodePtr; destDoc: xmlDocPtr; destParent: xmlNodePtr; options: cint): cint; XMLCALL; XMLPUBFUN;
function xmlDOMWrapRemoveNode(ctxt: xmlDOMWrapCtxtPtr; doc: xmlDocPtr; node: xmlNodePtr; options: cint): cint; XMLCALL; XMLPUBFUN;
function xmlDOMWrapCloneNode(ctxt: xmlDOMWrapCtxtPtr; sourceDoc: xmlDocPtr; node: xmlNodePtr; var clonedNode: xmlNodePtr; destDoc: xmlDocPtr; destParent: xmlNodePtr; deep, options: cint): cint; XMLCALL; XMLPUBFUN;
{$ENDIF}