(*
 * Summary: The DTD validation
 * Description: API for the DTD handling and the validity checking
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF POINTER}
  xmlValidStatePtr = ^xmlValidState;
  xmlValidCtxtPtr = ^xmlValidCtxt;
  xmlNotationTablePtr = ^xmlNotationTable;
  xmlElementTablePtr = ^xmlElementTable;
  xmlAttributeTablePtr = ^xmlAttributeTable;
  xmlIDTablePtr = ^xmlIDTable;
  xmlRefTablePtr = ^xmlRefTable;
{$ENDIF}

{$IFDEF TYPE}
(*
 * Validation state added for non-determinist content model.
 *)
  xmlValidState = record end;

(**
 * xmlValidityErrorFunc:
 * @ctx:  usually an xmlValidCtxtPtr to a validity error context,
 *        but comes from ctxt->userData (which normally contains such
 *        a pointer); ctxt->userData can be changed by the user.
 * @msg:  the string to format *printf like vararg
 * @...:  remaining arguments to the format
 *
 * Callback called when a validity error is found. This is a message
 * oriented function similar to an *printf function.
 *)
  xmlValidityErrorFunc = procedure(ctx: pointer; msg: pchar); XMLCDECL; varargs;

(**
 * xmlValidityWarningFunc:
 * @ctx:  usually an xmlValidCtxtPtr to a validity error context,
 *        but comes from ctxt->userData (which normally contains such
 *        a pointer); ctxt->userData can be changed by the user.
 * @msg:  the string to format *printf like vararg
 * @...:  remaining arguments to the format
 *
 * Callback called when a validity warning is found. This is a message
 * oriented function similar to an *printf function.
 *)
  xmlValidityWarningFunc = procedure(ctx: pointer; msg: pchar); XMLCDECL; varargs;

(*
 * xmlValidCtxt:
 * An xmlValidCtxt is used for error reporting when validating.
 *)
  xmlValidCtxt = record
    userData      : pointer;			(* user specific data block *)
    error         : xmlValidityErrorFunc;		(* the callback in case of errors *)
    warning       : xmlValidityWarningFunc;	(* the callback in case of warning *)

    (* Node analysis stack used when validating within entities *)
    node          : xmlNodePtr;          (* Current parsed Node *)
    nodeNr        : cint;        (* Depth of the parsing stack *)
    nodeMax       : cint;       (* Max depth of the parsing stack *)
    nodeTab       : xmlNodePtrPtr;       (* array of nodes *)

    finishDtd     : cuint;       (* finished validating the Dtd ? *)
    doc           : xmlDocPtr;       (* the document *)
    valid         : cint;       (* temporary validity check result *)

    (* state state used for non-determinist content validation *)
    vstate        : xmlValidStatePtr;        (* current state *)
    vstateNr      : cint;      (* Depth of the validation stack *)
    vstateMax     : cint;     (* Max depth of the validation stack *)
    vstateTab     : xmlValidStatePtr;     (* array of validation states *)

{$IFDEF LIBXML_REGEXP_ENABLED}
    am            : xmlAutomataPtr;     (* the automata *)
    state         : xmlAutomataStatePtr;     (* used to build the automata *)
{$ELSE}
    am            : pointer;
    state         : pointer;
{$ENDIF}
  end;

(*
 * ALL notation declarations are stored in a table.
 * There is one table per DTD.
 *)
  xmlNotationTable = type xmlHashTable;

(*
 * ALL element declarations are stored in a table.
 * There is one table per DTD.
 *)
  xmlElementTable = type xmlHashTable;

(*
 * ALL attribute declarations are stored in a table.
 * There is one table per DTD.
 *)
  xmlAttributeTable = type xmlHashTable;

(*
 * ALL IDs attributes are stored in a table.
 * There is one table per document.
 *)
  xmlIDTable = type xmlHashTable;

(*
 * ALL Refs attributes are stored in a table.
 * There is one table per document.
 *)
  xmlRefTable = type xmlHashTable;

{$ENDIF}

{$IFDEF FUNCTION}
(* Notation *)
function xmlAddNotationDecl(ctxt: xmlValidCtxtPtr; dtd: xmlDtdPtr; name, PublicID, SystemID: xmlCharPtr): xmlNotationPtr; XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_TREE_ENABLED}
function xmlCopyNotationTable(table: xmlNotationTablePtr): xmlNotationTablePtr; XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
procedure xmlFreeNotationTable(table: xmlNotationTablePtr); XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_OUTPUT_ENABLED}
procedure xmlDumpNotationDecl(buf: xmlBufferPtr; nota: xmlNotationPtr); XMLCALL; XMLPUBFUN;
procedure xmlDumpNotationTable(buf: xmlBufferPtr; table: xmlNotationTablePtr); XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

(* Element Content *)
function xmlNewDocElementContent(doc: xmlDocPtr; name: xmlCharPtr; _type: xmlElementContentType): xmlElementContentPtr; XMLCALL; XMLPUBFUN;
function xmlCopyDocElementContent(doc: xmlDocPtr; content: xmlElementContentPtr): xmlElementContentPtr; XMLCALL; XMLPUBFUN;
procedure xmlFreeDocElementContent(doc: xmlDocPtr; cur: xmlElementContentPtr); XMLCALL; XMLPUBFUN;
procedure xmlSnprintfElementContent(buf: pchar; size: cint; content: xmlElementContentPtr; englob: cint); XMLCALL; XMLPUBFUN;

(* Element *)
function xmlAddElementDecl(ctxt: xmlValidCtxtPtr; dtd: xmlDtdPtr; name: xmlCharPtr; _type: xmlElementTypeVal; content: xmlElementContentPtr): xmlElementPtr; XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_TREE_ENABLED}
function xmlCopyElementTable(table: xmlElementTablePtr): xmlElementTablePtr; XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
procedure xmlFreeElementTable(table: xmlElementTablePtr); XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_OUTPUT_ENABLED}
procedure xmlDumpElementTable(buf: xmlBufferPtr; table: xmlElementTablePtr); XMLCALL; XMLPUBFUN;
procedure xmlDumpElementDecl(buf: xmlBufferPtr; elem: xmlElementPtr); XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

(* Enumeration *)
function xmlCreateEnumeration(name: xmlCharPtr): xmlEnumerationPtr; XMLCALL; XMLPUBFUN;
procedure xmlFreeEnumeration(cur: xmlEnumerationPtr); XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_TREE_ENABLED}
function xmlCopyEnumeration(cur: xmlEnumerationPtr): xmlEnumerationPtr; XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_TREE_ENABLED *)

(* Attribute *)
function xmlAddAttributeDecl(ctxt: xmlValidCtxtPtr; dtd: xmlDtdPtr; elem, name, ns: xmlCharPtr; _type: xmlAttributeType;
  def: xmlAttributeDefault; defaultValue: xmlCharPtr; tree: xmlEnumerationPtr): xmlAttributePtr; XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_TREE_ENABLED}
function xmlCopyAttributeTable(table: xmlAttributeTablePtr): xmlAttributeTablePtr; XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
procedure xmlFreeAttributeTable(table: xmlAttributeTablePtr); XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_OUTPUT_ENABLED}
procedure xmlDumpAttributeTable(buf: xmlBufferPtr; table: xmlAttributeTablePtr); XMLCALL; XMLPUBFUN;
procedure xmlDumpAttributeDecl(buf: xmlBufferPtr; attr: xmlAttributePtr); XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

(* IDs *)
function xmlAddID(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; value: xmlCharPtr; attr: xmlAttrPtr): xmlIDPtr; XMLCALL; XMLPUBFUN;
procedure xmlFreeIDTable(table: xmlIDTablePtr); XMLCALL; XMLPUBFUN;
function xmlGetID(doc: xmlDocPtr; ID: xmlCharPtr): xmlAttrPtr; XMLCALL; XMLPUBFUN;
function xmlIsID(doc: xmlDocPtr; elem: xmlNodePtr; attr: xmlAttrPtr): cint; XMLCALL; XMLPUBFUN;
function xmlRemoveID(doc: xmlDocPtr; attr: xmlAttrPtr): cint; XMLCALL; XMLPUBFUN;

(* IDREFs *)
function xmlAddRef(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; value: xmlCharPtr; attr: xmlAttrPtr): xmlRefPtr; XMLCALL; XMLPUBFUN;
procedure xmlFreeRefTable(table: xmlRefTablePtr); XMLCALL; XMLPUBFUN;
function xmlIsRef(doc: xmlDocPtr; elem: xmlNodePtr; attr: xmlAttrPtr): cint; XMLCALL; XMLPUBFUN;
function xmlRemoveRef(doc: xmlDocPtr; attr: xmlAttrPtr): cint; XMLCALL; XMLPUBFUN;
function xmlGetRefs(doc: xmlDocPtr; ID: xmlCharPtr): xmlListPtr; XMLCALL; XMLPUBFUN;

(**
 * The public function calls related to validity checking.
 *)
{$IFDEF LIBXML_VALID_ENABLED}
(* Allocate/Release Validation Contexts *)
function xmlNewValidCtxt: xmlValidCtxtPtr; XMLCALL; XMLPUBFUN;
procedure xmlFreeValidCtxt(table: xmlValidCtxtPtr); XMLCALL; XMLPUBFUN;
function xmlValidateRoot(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidateElementDecl(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlElementPtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidNormalizeAttributeValue(doc: xmlDocPtr; elem: xmlNodePtr; name, value: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlValidCtxtNormalizeAttributeValue(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr; name, value: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlValidateAttributeDecl(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; attr: xmlAttributePtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidateAttributeValue(_type: xmlAttributeType; value: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidateNotationDecl(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; nota: xmlNotationPtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidateDtd(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; dtd: xmlDtdPtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidateDtdFinal(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidateDocument(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidateElement(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidateOneElement(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidateOneAttribute(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr; attr: xmlAttrPtr; value: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidateOneNamespace(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr; prefix: xmlCharPtr; ns: xmlNsPtr; value: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidateDocumentFinal(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr): cint; XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_VALID_ENABLED *)

{$IF defined(LIBXML_VALID_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
function xmlValidateNotationUse(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; notationName: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_VALID_ENABLED or LIBXML_SCHEMAS_ENABLED *)

function xmlIsMixedElement(doc: xmlDocPtr; name: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlGetDtdAttrDesc(dtd: xmlDtdPtr; elem, name: xmlCharPtr): xmlAttributePtr; XMLCALL; XMLPUBFUN;
function xmlGetDtdQAttrDesc(dtd: xmlDtdPtr; elem, name, prefix: xmlCharPtr): xmlAttributePtr; XMLCALL; XMLPUBFUN;
function xmlGetDtdNotationDesc(dtd: xmlDtdPtr; name: xmlCharPtr): xmlNotationPtr; XMLCALL; XMLPUBFUN;
function xmlGetDtdQElementDesc(dtd: xmlDtdPtr; name, prefix: xmlCharPtr): xmlElementPtr; XMLCALL; XMLPUBFUN;
function xmlGetDtdElementDesc(dtd: xmlDtdPtr; name: xmlCharPtr): xmlElementPtr; XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_VALID_ENABLED}

function xmlGetDtdElementDesc(ctree: xmlElementContentPtr; var names: xmlCharPtr; var len: cint; max: cint): cint; XMLCALL; XMLPUBFUN;
function xmlValidGetValidElements(prev, next: xmlNodePtr; var names: xmlCharPtr; max: cint): cint; XMLCALL; XMLPUBFUN;
function xmlValidateNameValue(value: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidateNamesValue(value: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidateNmtokenValue(value: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidateNmtokensValue(value: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;


{$IFDEF LIBXML_REGEXP_ENABLED}
(*
 * Validation based on the regexp support
 *)
function xmlValidBuildContentModel(ctxt: xmlValidCtxtPtr; elem: xmlElementPtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidatePushElement(ctxt: xmlValidCtxtPtr; doc: xmlNodePtr; elem: xmlElementPtr; qname: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlValidatePushCData(ctxt: xmlValidCtxtPtr; data: xmlCharPtr; len: cint): cint; XMLCALL; XMLPUBFUN;
function xmlValidatePopElement(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr; qname: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_REGEXP_ENABLED *)
{$ENDIF} (* LIBXML_VALID_ENABLED *)
{$ENDIF}
