(*
 * Summary: The DTD validation
 * Description: API for the DTD handling and the validity checking
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

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
  xmlValidityErrorFunc = procedure(ctx: pointer; msg: pchar; args: array of const); cdecl;

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
  xmlValidityWarningFunc = procedure(ctx: pointer; msg: pchar; args: array of const); cdecl;

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
function xmlAddNotationDecl(ctxt: xmlValidCtxtPtr; dtd: xmlDtdPtr; name, PublicID, SystemID: xmlCharPtr): xmlNotationPtr; cdecl; external;
{$IFDEF LIBXML_TREE_ENABLED}
function xmlCopyNotationTable(table: xmlNotationTablePtr): xmlNotationTablePtr; cdecl; external;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
procedure xmlFreeNotationTable(table: xmlNotationTablePtr); cdecl; external;
{$IFDEF LIBXML_OUTPUT_ENABLED}
procedure xmlDumpNotationDecl(buf: xmlBufferPtr; nota: xmlNotationPtr); cdecl; external;
procedure xmlDumpNotationTable(buf: xmlBufferPtr; table: xmlNotationTablePtr); cdecl; external;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

(* Element Content *)
(* the non Doc version are being deprecated *)
function xmlNewElementContent(name: xmlCharPtr; _type: xmlElementContentType): xmlElementContentPtr; cdecl; external;
function xmlCopyElementContent(content: xmlElementContentPtr): xmlElementContentPtr; cdecl; external;
procedure xmlFreeElementContent(cur: xmlElementContentPtr); cdecl; external;

(* the new versions with doc argument *)
function xmlNewDocElementContent(doc: xmlDocPtr; name: xmlCharPtr; _type: xmlElementContentType): xmlElementContentPtr; cdecl; external;
function xmlCopyDocElementContent(doc: xmlDocPtr; content: xmlElementContentPtr): xmlElementContentPtr; cdecl; external;
procedure xmlFreeDocElementContent(doc: xmlDocPtr; cur: xmlElementContentPtr); cdecl; external;
procedure xmlSnprintfElementContent(buf: pchar; size: cint; content: xmlElementContentPtr; englob: cint); cdecl; external;
{$IFDEF LIBXML_OUTPUT_ENABLED}
(* DEPRECATED *)
{XMLPUBFUN void XMLCALL		     
		xmlSprintfElementContent(char *buf,
	                                 xmlElementContentPtr content,
					 int englob);}
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
(* DEPRECATED *)

(* Element *)
function xmlAddElementDecl(ctxt: xmlValidCtxtPtr; dtd: xmlDtdPtr; name: xmlCharPtr; _type: xmlElementTypeVal; content: xmlElementContentPtr): xmlElementPtr; cdecl; external;
{$IFDEF LIBXML_TREE_ENABLED}
function xmlCopyElementTable(table: xmlElementTablePtr): xmlElementTablePtr; cdecl; external;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
procedure xmlFreeElementTable(table: xmlElementTablePtr); cdecl; external;
{$IFDEF LIBXML_OUTPUT_ENABLED}
procedure xmlDumpElementTable(buf: xmlBufferPtr; table: xmlElementTablePtr); cdecl; external;
procedure xmlDumpElementDecl(buf: xmlBufferPtr; elem: xmlElementPtr); cdecl; external;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

(* Enumeration *)
function xmlCreateEnumeration(name: xmlCharPtr): xmlEnumerationPtr; cdecl; external;
procedure xmlFreeEnumeration(cur: xmlEnumerationPtr); cdecl; external;
{$IFDEF LIBXML_TREE_ENABLED}
function xmlCopyEnumeration(cur: xmlEnumerationPtr): xmlEnumerationPtr; cdecl; external;
{$ENDIF} (* LIBXML_TREE_ENABLED *)

(* Attribute *)
function xmlAddAttributeDecl(ctxt: xmlValidCtxtPtr; dtd: xmlDtdPtr; elem, name, ns: xmlCharPtr; _type: xmlAttributeType;
  def: xmlAttributeDefault; defaultValue: xmlCharPtr; tree: xmlEnumerationPtr): xmlAttributePtr; cdecl; external;
{$IFDEF LIBXML_TREE_ENABLED}
function xmlCopyAttributeTable(table: xmlAttributeTablePtr): xmlAttributeTablePtr; cdecl; external;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
procedure xmlFreeAttributeTable(table: xmlAttributeTablePtr); cdecl; external;
{$IFDEF LIBXML_OUTPUT_ENABLED}
procedure xmlDumpAttributeTable(buf: xmlBufferPtr; table: xmlAttributeTablePtr); cdecl; external;
procedure xmlDumpAttributeDecl(buf: xmlBufferPtr; attr: xmlAttributePtr); cdecl; external;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

(* IDs *)
function xmlAddID(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; value: xmlCharPtr; attr: xmlAttrPtr): xmlIDPtr; cdecl; external;
procedure xmlFreeIDTable(table: xmlIDTablePtr); cdecl; external;
function xmlGetID(doc: xmlDocPtr; ID: xmlCharPtr): xmlAttrPtr; cdecl; external;
function xmlIsID(doc: xmlDocPtr; elem: xmlNodePtr; attr: xmlAttrPtr): cint; cdecl; external;
function xmlRemoveID(doc: xmlDocPtr; attr: xmlAttrPtr): cint; cdecl; external;

(* IDREFs *)
function xmlAddRef(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; value: xmlCharPtr; attr: xmlAttrPtr): xmlRefPtr; cdecl; external;
procedure xmlFreeRefTable(table: xmlRefTablePtr); cdecl; external;
function xmlIsRef(doc: xmlDocPtr; elem: xmlNodePtr; attr: xmlAttrPtr): cint; cdecl; external;
function xmlRemoveRef(doc: xmlDocPtr; attr: xmlAttrPtr): cint; cdecl; external;
function xmlGetRefs(doc: xmlDocPtr; ID: xmlCharPtr): xmlListPtr; cdecl; external;

(**
 * The public function calls related to validity checking.
 *)
{$IFDEF LIBXML_VALID_ENABLED}
(* Allocate/Release Validation Contexts *)
function xmlNewValidCtxt: xmlValidCtxtPtr; cdecl; external;
procedure xmlFreeValidCtxt(table: xmlValidCtxtPtr); cdecl; external;
function xmlValidateRoot(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr): cint; cdecl; external;
function xmlValidateElementDecl(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlElementPtr): cint; cdecl; external;
function xmlValidNormalizeAttributeValue(doc: xmlDocPtr; elem: xmlNodePtr; name, value: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlValidCtxtNormalizeAttributeValue(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr; name, value: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlValidateAttributeDecl(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; attr: xmlAttributePtr): cint; cdecl; external;
function xmlValidateAttributeValue(_type: xmlAttributeType; value: xmlCharPtr): cint; cdecl; external;
function xmlValidateNotationDecl(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; nota: xmlNotationPtr): cint; cdecl; external;
function xmlValidateDtd(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; dtd: xmlDtdPtr): cint; cdecl; external;
function xmlValidateDtdFinal(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr): cint; cdecl; external;
function xmlValidateDocument(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr): cint; cdecl; external;
function xmlValidateElement(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr): cint; cdecl; external;
function xmlValidateOneElement(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr): cint; cdecl; external;
function xmlValidateOneAttribute(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr; attr: xmlAttrPtr; value: xmlCharPtr): cint; cdecl; external;
function xmlValidateOneNamespace(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr; prefix: xmlCharPtr; ns: xmlNsPtr; value: xmlCharPtr): cint; cdecl; external;
function xmlValidateDocumentFinal(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr): cint; cdecl; external;
{$ENDIF} (* LIBXML_VALID_ENABLED *)

{$IF defined(LIBXML_VALID_ENABLED) ordefined(LIBXML_SCHEMAS_ENABLED)}
function xmlValidateNotationUse(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; notationName: xmlCharPtr): cint; cdecl; external;
{$ENDIF} (* LIBXML_VALID_ENABLED or LIBXML_SCHEMAS_ENABLED *)

function xmlIsMixedElement(doc: xmlDocPtr; name: xmlCharPtr): cint; cdecl; external;
function xmlGetDtdAttrDesc(dtd: xmlDtdPtr; elem, name: xmlCharPtr): xmlAttributePtr; cdecl; external;
function xmlGetDtdQAttrDesc(dtd: xmlDtdPtr; elem, name, prefix: xmlCharPtr): xmlAttributePtr; cdecl; external;
function xmlGetDtdNotationDesc(dtd: xmlDtdPtr; name: xmlCharPtr): xmlNotationPtr; cdecl; external;
function xmlGetDtdQElementDesc(dtd: xmlDtdPtr; name, prefix: xmlCharPtr): xmlElementPtr; cdecl; external;
function xmlGetDtdElementDesc(dtd: xmlDtdPtr; name: xmlCharPtr): xmlElementPtr; cdecl; external;
{$IFDEF LIBXML_VALID_ENABLED}

function xmlGetDtdElementDesc(ctree: xmlElementContentPtr; var names: xmlCharPtr; var len: cint; max: cint): cint; cdecl; external;
function xmlValidGetValidElements(prev, next: xmlNodePtr; var names: xmlCharPtr; max: cint): cint; cdecl; external;
function xmlValidateNameValue(value: xmlCharPtr): cint; cdecl; external;
function xmlValidateNamesValue(value: xmlCharPtr): cint; cdecl; external;
function xmlValidateNmtokenValue(value: xmlCharPtr): cint; cdecl; external;
function xmlValidateNmtokensValue(value: xmlCharPtr): cint; cdecl; external;


{$IFDEF LIBXML_REGEXP_ENABLED}
(*
 * Validation based on the regexp support
 *)
function xmlValidBuildContentModel(ctxt: xmlValidCtxtPtr; elem: xmlElementPtr): cint; cdecl; external;
function xmlValidatePushElement(ctxt: xmlValidCtxtPtr; doc: xmlNodePtr; elem: xmlElementPtr; qname: xmlCharPtr): cint; cdecl; external;
function xmlValidatePushCData(ctxt: xmlValidCtxtPtr; data: xmlCharPtr; len: cint): cint; cdecl; external;
function xmlValidatePopElement(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr; qname: xmlCharPtr): cint; cdecl; external;
{$ENDIF} (* LIBXML_REGEXP_ENABLED *)
{$ENDIF} (* LIBXML_VALID_ENABLED *)
{$ENDIF}
