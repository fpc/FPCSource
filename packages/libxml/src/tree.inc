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
 * LIBXML_XML_NAMESPACE:
 *
 * This is the namespace for the special xml: prefix predefined in the
 * XML Namespace specification.
 *)
  __LIBXML_XML_NAMESPACE: String = 'http://www.w3.org/XML/1998/namespace';
  LIBXML_XML_NAMESPACE: xmlCharPtr = @__LIBXML_XML_NAMESPACE[1];

(**
 * XML_XML_ID:
 *
 * This is the name for the special xml:id attribute
 *)
  __LIBXML_XML_ID: String = 'xml:id';
  LIBXML_XML_ID: xmlCharPtr = @__LIBXML_XML_ID[1];
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
  xmlDOMWrapAcquireNsFunction = function (ctxt: xmlDOMWrapCtxtPtr; node: xmlNodePtr; nsName, nsPrefix: xmlCharPtr): xmlNsPtr; EXTDECL;

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
function xmlValidateNCName(value: xmlCharPtr; space: cint): cint; EXTDECL; external xml2lib;
{$ENDIF}

{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
function xmlValidateQName(value: xmlCharPtr; space: cint): cint; EXTDECL; external xml2lib;
function xmlValidateName(value: xmlCharPtr; space: cint): cint; EXTDECL; external xml2lib;
function xmlValidateNMToken(value: xmlCharPtr; space: cint): cint; EXTDECL; external xml2lib;
{$ENDIF}

function xmlValidateQName(ncname, prefix, memory: xmlCharPtr; len: cint): xmlCharPtr; EXTDECL; external xml2lib;
function xmlSplitQName2(name: xmlCharPtr; var prefix: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlSplitQName3(name: xmlCharPtr; var prefix: xmlCharPtr; var len: cint): xmlCharPtr; EXTDECL; external xml2lib;

(*
 * Handling Buffers.
 *)
procedure xmlSetBufferAllocationScheme(scheme: xmlBufferAllocationScheme); EXTDECL; external xml2lib;
function xmlGetBufferAllocationScheme: xmlBufferAllocationScheme; EXTDECL; external xml2lib;

function xmlBufferCreate: xmlBufferPtr; EXTDECL; external xml2lib;
function xmlBufferCreateSize(size: csize_t): xmlBufferPtr; EXTDECL; external xml2lib;
function xmlBufferCreateStatic(mem: pointer; size: csize_t): xmlBufferPtr; EXTDECL; external xml2lib;
function xmlBufferResize(buf: xmlBufferPtr; size: cuint): cint; EXTDECL; external xml2lib;
procedure xmlBufferFree(buf: xmlBufferPtr); EXTDECL; external xml2lib;
procedure xmlBufferDump(fp: PFILE; buf: xmlBufferPtr); EXTDECL; external xml2lib;
function xmlBufferAdd(buf: xmlBufferPtr; str: xmlCharPtr; len: cint): cint; EXTDECL; external xml2lib;
function xmlBufferAddHead(buf: xmlBufferPtr; str: xmlCharPtr; len: cint): cint; EXTDECL; external xml2lib;
function xmlBufferCat(buf: xmlBufferPtr; str: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlBufferCCat(buf: xmlBufferPtr; str: pchar): cint; EXTDECL; external xml2lib;
function xmlBufferShrink(buf: xmlBufferPtr; len: cuint): cint; EXTDECL; external xml2lib;
function xmlBufferGrow(buf: xmlBufferPtr; len: cuint): cint; EXTDECL; external xml2lib;
procedure xmlBufferEmpty(buf: xmlBufferPtr); EXTDECL; external xml2lib;
function xmlBufferContent(buf: xmlBufferPtr): xmlCharPtr; EXTDECL; external xml2lib;
procedure xmlBufferSetAllocationScheme(buf: xmlBufferPtr; scheme: xmlBufferAllocationScheme); EXTDECL; external xml2lib;
function xmlBufferLength(buf: xmlBufferPtr): cint; EXTDECL; external xml2lib;

(*
 * Creating/freeing new structures.
 *)
function xmlCreateIntSubset(doc: xmlDocPtr; name, ExternalID, SystemID: xmlCharPtr): xmlDtdPtr; EXTDECL; external xml2lib;
function xmlNewDtd(doc: xmlDocPtr; name, ExternalID, SystemID: xmlCharPtr): xmlDtdPtr; EXTDECL; external xml2lib;
function xmlGetIntSubset(doc: xmlDocPtr): xmlDtdPtr; EXTDECL; external xml2lib;
procedure xmlFreeDtd(cur: xmlDtdPtr); EXTDECL; external xml2lib;
{$IFDEF LIBXML_LEGACY_ENABLED}
function xmlNewGlobalNs(doc: xmlDocPtr; href, prefix: xmlCharPtr): xmlNsPtr; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_LEGACY_ENABLED *)
function xmlNewNs(node: xmlNodePtr; href, prefix: xmlCharPtr): xmlNsPtr; EXTDECL; external xml2lib;
procedure xmlFreeNs(cur: xmlNsPtr); EXTDECL; external xml2lib;
procedure xmlFreeNsList(cur: xmlNsPtr); EXTDECL; external xml2lib;
function xmlNewDoc(version: xmlCharPtr): xmlDocPtr; EXTDECL; external xml2lib;
procedure xmlFreeDoc(cur: xmlDocPtr); EXTDECL; external xml2lib;
function xmlNewDocProp(doc: xmlDocPtr; name, value: xmlCharPtr): xmlAttrPtr; EXTDECL; external xml2lib;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_HTML_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
function xmlNewProp(node: xmlNodePtr; name, value: xmlCharPtr): xmlAttrPtr; EXTDECL; external xml2lib;
{$ENDIF}
function xmlNewNsProp(node: xmlNodePtr; ns: xmlNsPtr; name, value: xmlCharPtr): xmlAttrPtr; EXTDECL; external xml2lib;
function xmlNewNsPropEatName(node: xmlNodePtr; ns: xmlNsPtr; name, value: xmlCharPtr): xmlAttrPtr; EXTDECL; external xml2lib;
procedure xmlFreePropList(cur: xmlAttrPtr); EXTDECL; external xml2lib;
procedure xmlFreeProp(cur: xmlAttrPtr); EXTDECL; external xml2lib;
function xmlCopyProp(target: xmlNodePtr; cur: xmlAttrPtr): xmlAttrPtr; EXTDECL; external xml2lib;
function xmlCopyPropList(target: xmlNodePtr; cur: xmlAttrPtr): xmlAttrPtr; EXTDECL; external xml2lib;
{$IFDEF LIBXML_TREE_ENABLED}
function xmlCopyDtd(dtd: xmlDtdPtr): xmlDtdPtr; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
function xmlCopyDoc(doc: xmlDocPtr; recursive: cint): xmlDocPtr; EXTDECL; external xml2lib;
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED) *)

(*
 * Creating new nodes.
 *)
function xmlNewDocNode(doc: xmlDocPtr; ns: xmlNsPtr; name, content: xmlCharPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNewDocNodeEatName(doc: xmlDocPtr; ns: xmlNsPtr; name, content: xmlCharPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNewNode(ns: xmlNsPtr; name: xmlCharPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNewNodeEatName(ns: xmlNsPtr; name: xmlCharPtr): xmlNodePtr; EXTDECL; external xml2lib;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
function xmlNewChild(parent: xmlNodePtr; ns: xmlNsPtr; name, content: xmlCharPtr): xmlNodePtr; EXTDECL; external xml2lib;
{$ENDIF}

function xmlNewDocText(doc: xmlDocPtr; content: xmlCharPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNewText(content: xmlCharPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNewDocPI(doc: xmlDocPtr; name, content: xmlCharPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNewPI(name, content: xmlCharPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNewDocTextLen(doc: xmlDocPtr; content: xmlCharPtr; len: cint): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNewTextLen(content: xmlCharPtr; len: cint): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNewDocComment(doc: xmlDocPtr; content: xmlCharPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNewComment(content: xmlCharPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNewCDataBlock(doc: xmlDocPtr; content: xmlCharPtr; len: cint): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNewCharRef(doc: xmlDocPtr; name: xmlCharPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNewReference(doc: xmlDocPtr; name: xmlCharPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlCopyNode(node: xmlNodePtr; recursive: cint): xmlNodePtr; EXTDECL; external xml2lib;
function xmlDocCopyNode(node: xmlNodePtr; doc: xmlDocPtr; recursive: cint): xmlNodePtr; EXTDECL; external xml2lib;
function xmlDocCopyNodeList(doc: xmlDocPtr; node: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlCopyNodeList(node: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
{$IFDEF LIBXML_TREE_ENABLED}
function xmlNewTextChild(parent: xmlNodePtr; ns: xmlNsPtr; name, content: xmlCharPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNewDocRawNode(doc: xmlDocPtr; ns: xmlNsPtr; name, content: xmlCharPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNewDocFragment(doc: xmlDocPtr): xmlNodePtr; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_TREE_ENABLED *)

(*
 * Navigating.
 *)
function xmlGetLineNo(node: xmlNodePtr): clong; EXTDECL; external xml2lib;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_DEBUG_ENABLED)}
function xmlGetNodePath(node: xmlNodePtr): xmlCharPtr; EXTDECL; external xml2lib;
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_DEBUG_ENABLED) *)
function xmlDocGetRootElement(doc: xmlDocPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlGetLastChild(parent: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNodeIsText(node: xmlNodePtr): cint; EXTDECL; external xml2lib;
function xmlIsBlankNode(node: xmlNodePtr): cint; EXTDECL; external xml2lib;

(*
 * Changing the structure.
 *)
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_WRITER_ENABLED)}
function xmlDocSetRootElement(doc: xmlDocPtr; root: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_WRITER_ENABLED) *)
{$IFDEF LIBXML_TREE_ENABLED}
procedure xmlNodeSetName(cur: xmlNodePtr; name: xmlCharPtr); EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
function xmlAddChild(parent, cur: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlAddChildList(parent, cur: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_WRITER_ENABLED)}
function xmlReplaceNode(old, cur: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_WRITER_ENABLED) *)
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_HTML_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
function xmlAddPrevSibling(cur, elem: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_TREE_ENABLED || LIBXML_HTML_ENABLED || LIBXML_SCHEMAS_ENABLED *)
function xmlAddSibling(cur, elem: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlAddNextSibling(cur, elem: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
procedure xmlUnlinkNode(cur: xmlNodePtr); EXTDECL; external xml2lib;
function xmlTextMerge(first, second: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlTextConcat(node: xmlNodePtr; name: xmlCharPtr; len: cint): cint; EXTDECL; external xml2lib;
procedure xmlFreeNodeList(cur: xmlNodePtr); EXTDECL; external xml2lib;
procedure xmlFreeNode(cur: xmlNodePtr); EXTDECL; external xml2lib;
procedure xmlSetTreeDoc(tree: xmlNodePtr; doc: xmlDocPtr); EXTDECL; external xml2lib;
procedure xmlSetListDoc(list: xmlNodePtr; doc: xmlDocPtr); EXTDECL; external xml2lib;

(*
 * Namespaces.
 *)
function xmlSearchNs(doc: xmlDocPtr; node: xmlNodePtr; nameSpace: xmlCharPtr): xmlNsPtr; EXTDECL; external xml2lib;
function xmlSearchNsByHref(doc: xmlDocPtr; node: xmlNodePtr; href: xmlCharPtr): xmlNsPtr; EXTDECL; external xml2lib;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_XPATH_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
function xmlGetNsList(doc: xmlDocPtr; node: xmlNodePtr): xmlNsPtr; EXTDECL; external xml2lib;
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_XPATH_ENABLED) *)
procedure xmlSetNs(node: xmlNodePtr; ns: xmlNsPtr); EXTDECL; external xml2lib;
function xmlCopyNamespace(cur: xmlNsPtr): xmlNsPtr; EXTDECL; external xml2lib;
function xmlCopyNamespaceList(cur: xmlNsPtr): xmlNsPtr; EXTDECL; external xml2lib;

(*
 * Changing the content.
 *)
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_XINCLUDE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED) or defined(LIBXML_HTML_ENABLED)}
function xmlSetProp(node: xmlNodePtr; name, value: xmlCharPtr): xmlAttrPtr; EXTDECL; external xml2lib;
function xmlSetNsProp(node: xmlNodePtr; ns: xmlNsPtr; name, value: xmlCharPtr): xmlAttrPtr; EXTDECL; external xml2lib;
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_XINCLUDE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED) || defined(LIBXML_HTML_ENABLED) *)
function xmlGetNoNsProp(node: xmlNodePtr; name: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlGetProp(node: xmlNodePtr; name: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlHasProp(node: xmlNodePtr; name: xmlCharPtr): xmlAttrPtr; EXTDECL; external xml2lib;
function xmlHasNsProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlAttrPtr; EXTDECL; external xml2lib;
function xmlGetNsProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlStringGetNodeList(doc: xmlDocPtr; value: xmlCharPtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlStringLenGetNodeList(doc: xmlDocPtr; value: xmlCharPtr; len: cint): xmlNodePtr; EXTDECL; external xml2lib;
function xmlNodeListGetString(doc: xmlDocPtr; list: xmlNodePtr; _inLine: cint): xmlCharPtr; EXTDECL; external xml2lib;
{$IFDEF LIBXML_TREE_ENABLED}
function xmlNodeListGetRawString(doc: xmlDocPtr; list: xmlNodePtr; _inLine: cint): xmlCharPtr; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
procedure xmlNodeSetContent(node: xmlNodePtr; content: xmlCharPtr); EXTDECL; external xml2lib;
{$IFDEF LIBXML_TREE_ENABLED}
procedure xmlNodeSetContentLen(node: xmlNodePtr; content: xmlCharPtr; len: cint); EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
procedure xmlNodeAddContent(cur: xmlNodePtr; content: xmlCharPtr); EXTDECL; external xml2lib;
procedure xmlNodeAddContentLen(cur: xmlNodePtr; content: xmlCharPtr; len: cint); EXTDECL; external xml2lib;
function xmlNodeGetContent(cur: xmlNodePtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlNodeBufGetContent(buffer: xmlBufferPtr; cur: xmlNodePtr): cint; EXTDECL; external xml2lib;
function xmlNodeGetLang(cur: xmlNodePtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlNodeGetSpacePreserve(cur: xmlNodePtr): cint; EXTDECL; external xml2lib;
{$IFDEF LIBXML_TREE_ENABLED}
procedure xmlNodeSetLang(cur: xmlNodePtr; lang: xmlCharPtr); EXTDECL; external xml2lib;
procedure xmlNodeSetSpacePreserve(cur: xmlNodePtr; val: cint); EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
function xmlNodeGetBase(doc: xmlDocPtr; cur: xmlNodePtr): xmlCharPtr; EXTDECL; external xml2lib;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_XINCLUDE_ENABLED)}
procedure xmlNodeSetBase(cur: xmlNodePtr; uri: xmlCharPtr); EXTDECL; external xml2lib;
{$ENDIF}

(*
 * Removing content.
 *)
function xmlRemoveProp(cur: xmlAttrPtr): cint; EXTDECL; external xml2lib;
{$IF defined(LIBXML_TREE_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
function xmlUnsetNsProp(node: xmlNodePtr; ns: xmlNsPtr; name: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlUnsetProp(node: xmlNodePtr; name: xmlCharPtr): cint; EXTDECL; external xml2lib;
{$ENDIF} (* defined(LIBXML_TREE_ENABLED) || defined(LIBXML_SCHEMAS_ENABLED) *)

(*
 * Internal, don't use.
 *)
{external xml2lib void EXTDECL
		xmlBufferWriteCHAR	(xmlBufferPtr buf,
					 xmlChar *string);
external xml2lib void EXTDECL
		xmlBufferWriteChar	(xmlBufferPtr buf,
					 char *string);
external xml2lib void EXTDECL
		xmlBufferWriteQuotedString(xmlBufferPtr buf,
					 xmlChar *string);
}
{$IFDEF LIBXML_OUTPUT_ENABLED}
{external xml2lib void xmlAttrSerializeTxtContent(xmlBufferPtr buf,
					 xmlDocPtr doc,
					 xmlAttrPtr attr,
					 xmlChar *string);}
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

{$IFDEF LIBXML_TREE_ENABLED}
(*
 * Namespace handling.
 *)
function xmlReconciliateNs(doc: xmlDocPtr; tree: xmlNodePtr): cint; EXTDECL; external xml2lib;
{$ENDIF}

{$IFDEF LIBXML_OUTPUT_ENABLED}
(*
 * Saving.
 *)
procedure xmlDocDumpFormatMemory(cur: xmlDocPtr; var mem: xmlCharPtr; var size: cint; format: cint); EXTDECL; external xml2lib;
procedure xmlDocDumpMemory(cur: xmlDocPtr; var mem: xmlCharPtr; var size: cint); EXTDECL; external xml2lib;
procedure xmlDocDumpMemoryEnc(out_doc: xmlDocPtr; var doc_txt_ptr: xmlCharPtr; var doc_txt_len: cint; txt_encoding: pchar); EXTDECL; external xml2lib;
procedure xmlDocDumpFormatMemoryEnc(out_doc: xmlDocPtr; var doc_txt_ptr: xmlCharPtr; var doc_txt_len: cint; txt_encoding: pchar; format: cint); EXTDECL; external xml2lib;
function xmlDocFormatDump(f: PFILE; cur: xmlDocPtr; format: cint): cint; EXTDECL; external xml2lib;
function xmlDocDump(f: PFILE; cur: xmlDocPtr): cint; EXTDECL; external xml2lib;
function xmlElemDump(f: PFILE; doc: xmlDocPtr; cur: xmlNodePtr): cint; EXTDECL; external xml2lib;
function xmlSaveFormatFile(filename: pchar; cur: xmlDocPtr; format: cint): cint; EXTDECL; external xml2lib;
function xmlSaveFile(filename: pchar; cur: xmlDocPtr): cint; EXTDECL; external xml2lib;
function xmlNodeDump(buf: xmlBufferPtr; doc: xmlDocPtr; cur: xmlNodePtr; level, format: cint): cint; EXTDECL; external xml2lib;
function xmlSaveFileTo(buf: xmlOutputBufferPtr; cur: xmlDocPtr; encoding: pchar): cint; EXTDECL; external xml2lib;
function xmlSaveFormatFileTo(buf: xmlOutputBufferPtr; cur: xmlDocPtr; encoding: pchar; format: cint): cint; EXTDECL; external xml2lib;
procedure xmlNodeDumpOutput(buf: xmlBufferPtr; doc: xmlDocPtr; cur: xmlNodePtr; level, format: cint; encoding: pchar); EXTDECL; external xml2lib;
function xmlSaveFormatFileEnc(filename: pchar; cur: xmlDocPtr; encoding: pchar; format: cint): cint; EXTDECL; external xml2lib;
function xmlSaveFileEnc(filename: pchar; cur: xmlDocPtr; encoding: pchar): cint; EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

(*
 * XHTML
 *)
function xmlIsXHTML(systemID, publicID: xmlCharPtr): cint; EXTDECL; external xml2lib;

(*
 * Compression.
 *)
function xmlGetDocCompressMode(doc: xmlDocPtr): cint; EXTDECL; external xml2lib;
procedure xmlSetDocCompressMode(ctxt: xmlDOMWrapCtxtPtr; mode: cint); EXTDECL; external xml2lib;
function xmlGetCompressMode: cint; EXTDECL; external xml2lib;
procedure xmlSetCompressMode(mode: cint); EXTDECL; external xml2lib;

(*
* DOM-wrapper helper functions.
*)
function xmlDOMWrapNewCtxt: xmlDOMWrapCtxtPtr; EXTDECL; external xml2lib;
procedure xmlDOMWrapNewCtxt(ctxt: xmlDOMWrapCtxtPtr); EXTDECL; external xml2lib;
function xmlDOMWrapReconcileNamespaces(ctxt: xmlDOMWrapCtxtPtr; elem: xmlNodePtr; options: cint): cint; EXTDECL; external xml2lib;
function xmlDOMWrapAdoptNode(ctxt: xmlDOMWrapCtxtPtr; sourceDoc: xmlDocPtr; node: xmlNodePtr; destDoc: xmlDocPtr; destParent: xmlNodePtr; options: cint): cint; EXTDECL; external xml2lib;
function xmlDOMWrapRemoveNode(ctxt: xmlDOMWrapCtxtPtr; doc: xmlDocPtr; node: xmlNodePtr; options: cint): cint; EXTDECL; external xml2lib;
function xmlDOMWrapCloneNode(ctxt: xmlDOMWrapCtxtPtr; sourceDoc: xmlDocPtr; node: xmlNodePtr; var clonedNode: xmlNodePtr; destDoc: xmlDocPtr; destParent: xmlNodePtr; deep, options: cint): cint; EXTDECL; external xml2lib;
{$ENDIF}
