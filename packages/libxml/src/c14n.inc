(*
 * Summary: Provide Canonical XML and Exclusive XML Canonicalization
 * Description: the c14n modules provides a
 *
 * "Canonical XML" implementation
 * http://www.w3.org/TR/xml-c14n
 *
 * and an
 *
 * "Exclusive XML Canonicalization" implementation
 * http://www.w3.org/TR/xml-exc-c14n

 * Copy: See Copyright for the status of this software.
 *
 * Author: Aleksey Sanin <aleksey@aleksey.com>
 *)

{$IFDEF LIBXML_C14N_ENABLED}
{$IFDEF LIBXML_OUTPUT_ENABLED}

(*
 * XML Canonicazation
 * http://www.w3.org/TR/xml-c14n
 *
 * Exclusive XML Canonicazation
 * http://www.w3.org/TR/xml-exc-c14n
 *
 * Canonical form of an XML document could be created if and only if
 *  a) default attributes (if any) are added to all nodes
 *  b) all character and parsed entity references are resolved
 * In order to achive this in libxml2 the document MUST be loaded with
 * following global setings:
 *
 *    xmlLoadExtDtdDefaultValue = XML_DETECT_IDS | XML_COMPLETE_ATTRS;
 *    xmlSubstituteEntitiesDefault(1);
 *
 * or corresponding parser context setting:
 *    xmlParserCtxtPtr ctxt;
 *
 *    ...
 *    ctxt->loadsubset = XML_DETECT_IDS | XML_COMPLETE_ATTRS;
 *    ctxt->replaceEntities = 1;
 *    ...
 *)

{$IFDEF TYPE}
(**
 * This is the core C14N function
 *)

  xmlC14NIsVisibleCallback = function(user_data: pointer; node, parent: xmlNodePtr): cint; EXTDECL;
{$ENDIF}

{$IFDEF FUNCTION}
function xmlC14NDocSaveTo(doc: xmlDocPtr; nodes: xmlNodeSetPtr; exclusive: cint; inclusive_ns_prefixes: xmlCharPtrPtr;
  with_comments: cint; buf: xmlOutputBufferPtr): cint; EXTDECL; external xml2lib;

function xmlC14NDocDumpMemory(doc: xmlDocPtr; nodes: xmlNodeSetPtr; exclusive: cint; inclusive_ns_prefixes: xmlCharPtrPtr;
  with_comments: cint; doc_txt_ptr: xmlCharPtrPtr): cint; EXTDECL; external xml2lib;

function xmlC14NDocSave(doc: xmlDocPtr; nodes: xmlNodeSetPtr; exclusive: cint; inclusive_ns_prefixes: xmlCharPtrPtr;
  with_comments: cint; filename: pchar; compression: cint): cint; EXTDECL; external xml2lib;

function xmlC14NExecute(doc: xmlDocPtr; is_visible_callback: xmlC14NIsVisibleCallback; user_data: pointer;
  exclusive: cint; inclusive_ns_prefixes: xmlCharPtrPtr; with_comments: cint; buf: xmlOutputBufferPtr): cint; EXTDECL; external xml2lib;
{$ENDIF}

{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
{$ENDIF} (* LIBXML_C14N_ENABLED *)

