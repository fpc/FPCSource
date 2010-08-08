(*
 * Summary: incomplete XML Schemas structure implementation
 * Description: interface to the XML Schemas handling and schema validity
 *              checking, it is incomplete right now.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_SCHEMAS_ENABLED}

{$IFDEF POINTER}
  xmlSchemaPtr = ^xmlSchema;
  xmlSchemaParserCtxtPtr = ^xmlSchemaParserCtxt;
  xmlSchemaValidCtxtPtr = ^xmlSchemaValidCtxt;
  xmlSchemaSAXPlugPtr = ^xmlSchemaSAXPlugStruct;
{$ENDIF}

{$IFDEF TYPE}
(**
 * This error codes are obsolete; not used any more.
 *)
  xmlSchemaValidError = (
    XML_SCHEMAS_ERR_OK		= 0,
    XML_SCHEMAS_ERR_NOROOT	= 1,
    XML_SCHEMAS_ERR_UNDECLAREDELEM,
    XML_SCHEMAS_ERR_NOTTOPLEVEL,
    XML_SCHEMAS_ERR_MISSING,
    XML_SCHEMAS_ERR_WRONGELEM,
    XML_SCHEMAS_ERR_NOTYPE,
    XML_SCHEMAS_ERR_NOROLLBACK,
    XML_SCHEMAS_ERR_ISABSTRACT,
    XML_SCHEMAS_ERR_NOTEMPTY,
    XML_SCHEMAS_ERR_ELEMCONT,
    XML_SCHEMAS_ERR_HAVEDEFAULT,
    XML_SCHEMAS_ERR_NOTNILLABLE,
    XML_SCHEMAS_ERR_EXTRACONTENT,
    XML_SCHEMAS_ERR_INVALIDATTR,
    XML_SCHEMAS_ERR_INVALIDELEM,
    XML_SCHEMAS_ERR_NOTDETERMINIST,
    XML_SCHEMAS_ERR_CONSTRUCT,
    XML_SCHEMAS_ERR_INTERNAL,
    XML_SCHEMAS_ERR_NOTSIMPLE,
    XML_SCHEMAS_ERR_ATTRUNKNOWN,
    XML_SCHEMAS_ERR_ATTRINVALID,
    XML_SCHEMAS_ERR_VALUE,
    XML_SCHEMAS_ERR_FACET,
    XML_SCHEMAS_ERR_,
    XML_SCHEMAS_ERR_XXX
  );

(*
* ATTENTION: Change xmlSchemaSetValidOptions's check
* for invalid values, if adding to the validation
* options below.
*)
(**
 * xmlSchemaValidOption:
 *
 * This is the set of XML Schema validation options.
 *)
  xmlSchemaValidOption = type cint;
{$ENDIF}

{$IFDEF CONST}
    XML_SCHEMA_VAL_VC_I_CREATE			= 1 shl 0;
	(* Default/fixed: create an attribute node
	* or an element's text node on the instance.
	*)

    XML_SCHEMA_VAL_XSI_ASSEMBLE			= 1 shl 1;
(*
	* assemble schemata using
	* xsi:schemaLocation and
	* xsi:noNamespaceSchemaLocation
*)
{$ENDIF}

{$IFDEF TYPE}
(**
 * The schemas related types are kept internal
 *)
//  xmlSchema = record end;

(**
 * A schemas validation context
 *)
  xmlSchemaValidityErrorFunc = procedure(ctx: pointer; msg: pchar); cdecl; varargs;
  xmlSchemaValidityWarningFunc = procedure(ctx: pointer; msg: pchar); cdecl; varargs;

  xmlSchemaParserCtxt = record end;

  xmlSchemaValidCtxt = record end;

  xmlSchemaSAXPlugStruct = record end;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Interfaces for parsing.
 *)
function xmlSchemaNewParserCtxt(URL: pchar): xmlSchemaParserCtxtPtr; EXTDECL; external xml2lib;
function xmlSchemaNewMemParserCtxt(buffer: pchar; size: cint): xmlSchemaParserCtxtPtr; EXTDECL; external xml2lib;
function xmlSchemaNewDocParserCtxt(doc: xmlDocPtr): xmlSchemaParserCtxtPtr; EXTDECL; external xml2lib;
procedure xmlSchemaFreeParserCtxt(ctxt: xmlSchemaParserCtxtPtr); EXTDECL; external xml2lib;
procedure xmlSchemaSetParserErrors(ctxt: xmlSchemaParserCtxtPtr; err: xmlSchemaValidityErrorFunc; warn: xmlSchemaValidityWarningFunc; ctx: pointer); EXTDECL; external xml2lib;
procedure xmlSchemaSetParserStructuredErrors(ctxt: xmlSchemaParserCtxtPtr; serror: xmlStructuredErrorFunc; ctx: pointer); EXTDECL; external xml2lib;
function xmlSchemaSetParserErrors(ctxt: xmlSchemaParserCtxtPtr; var err: xmlSchemaValidityErrorFunc; var warn: xmlSchemaValidityWarningFunc; var ctx: pointer): cint; EXTDECL; external xml2lib;
function xmlSchemaIsValid(ctxt: xmlSchemaValidCtxtPtr): cint; EXTDECL; external xml2lib;
function xmlSchemaParse(ctxt: xmlSchemaParserCtxtPtr): xmlSchemaPtr; EXTDECL; external xml2lib;
procedure xmlSchemaFree(schema: xmlSchemaPtr); EXTDECL; external xml2lib;
{$IFDEF LIBXML_OUTPUT_ENABLED}
procedure xmlSchemaDump(output: PFILE; schema: xmlSchemaPtr); EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

(*
 * Interfaces for validating
 *)
procedure xmlSchemaSetValidErrors(ctxt: xmlSchemaParserCtxtPtr; err: xmlSchemaValidityErrorFunc; warn: xmlSchemaValidityWarningFunc; ctx: pointer); EXTDECL; external xml2lib;
procedure xmlSchemaSetValidStructuredErrors(ctxt: xmlSchemaParserCtxtPtr; serror: xmlStructuredErrorFunc; ctx: pointer); EXTDECL; external xml2lib;
function xmlSchemaGetValidErrors(ctxt: xmlSchemaParserCtxtPtr; var err: xmlSchemaValidityErrorFunc; var warn: xmlSchemaValidityWarningFunc; var ctx: pointer): cint; EXTDECL; external xml2lib;
function xmlSchemaSetValidOptions(ctxt: xmlSchemaValidCtxtPtr; options: cint): cint; EXTDECL; external xml2lib;
function xmlSchemaValidCtxtGetOptions(ctxt: xmlSchemaValidCtxtPtr): cint; EXTDECL; external xml2lib;
function xmlSchemaNewValidCtxt(schema: xmlSchemaPtr): xmlSchemaValidCtxtPtr; EXTDECL; external xml2lib;
procedure xmlSchemaFreeValidCtxt(ctxt: xmlSchemaValidCtxtPtr); EXTDECL; external xml2lib;
function xmlSchemaNewValidCtxt(ctxt: xmlSchemaValidCtxtPtr; instance: xmlDocPtr): cint; EXTDECL; external xml2lib;
function xmlSchemaValidateOneElement(ctxt: xmlSchemaValidCtxtPtr; elem: xmlNodePtr): cint; EXTDECL; external xml2lib;
function xmlSchemaValidateStream(ctxt: xmlSchemaValidCtxtPtr; input: xmlParserInputBufferPtr; enc: xmlCharEncoding; sax: xmlSAXHandlerPtr; user_data: pointer): cint; EXTDECL; external xml2lib;
function xmlSchemaValidateFile(ctxt: xmlSchemaValidCtxtPtr; filename: pchar; options: cint): cint; EXTDECL; external xml2lib;

(*
 * Interface to insert Schemas SAX velidation in a SAX stream
 *)
function xmlSchemaSAXPlug(ctxt: xmlSchemaValidCtxtPtr; var sax: xmlSchemaSAXPlugPtr; var user_data: pointer): xmlSchemaSAXPlugPtr; EXTDECL; external xml2lib;
function xmlSchemaSAXUnplug(plug: xmlSchemaSAXPlugPtr): cint; EXTDECL; external xml2lib;
{$ENDIF}

{$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)
