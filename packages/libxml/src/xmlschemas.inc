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
  xmlSchemaValidityErrorFunc = procedure(ctx: pointer; msg: pchar); XMLCDECL; varargs;
  xmlSchemaValidityWarningFunc = procedure(ctx: pointer; msg: pchar); XMLCDECL; varargs;

  xmlSchemaParserCtxt = record end;

  xmlSchemaValidCtxt = record end;

  xmlSchemaSAXPlugStruct = record end;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Interfaces for parsing.
 *)
function xmlSchemaNewParserCtxt(URL: pchar): xmlSchemaParserCtxtPtr; XMLCALL; XMLPUBFUN;
function xmlSchemaNewMemParserCtxt(buffer: pchar; size: cint): xmlSchemaParserCtxtPtr; XMLCALL; XMLPUBFUN;
function xmlSchemaNewDocParserCtxt(doc: xmlDocPtr): xmlSchemaParserCtxtPtr; XMLCALL; XMLPUBFUN;
procedure xmlSchemaFreeParserCtxt(ctxt: xmlSchemaParserCtxtPtr); XMLCALL; XMLPUBFUN;
procedure xmlSchemaSetParserErrors(ctxt: xmlSchemaParserCtxtPtr; err: xmlSchemaValidityErrorFunc; warn: xmlSchemaValidityWarningFunc; ctx: pointer); XMLCALL; XMLPUBFUN;
procedure xmlSchemaSetParserStructuredErrors(ctxt: xmlSchemaParserCtxtPtr; serror: xmlStructuredErrorFunc; ctx: pointer); XMLCALL; XMLPUBFUN;
function xmlSchemaSetParserErrors(ctxt: xmlSchemaParserCtxtPtr; var err: xmlSchemaValidityErrorFunc; var warn: xmlSchemaValidityWarningFunc; var ctx: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaIsValid(ctxt: xmlSchemaValidCtxtPtr): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaParse(ctxt: xmlSchemaParserCtxtPtr): xmlSchemaPtr; XMLCALL; XMLPUBFUN;
procedure xmlSchemaFree(schema: xmlSchemaPtr); XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_OUTPUT_ENABLED}
procedure xmlSchemaDump(output: PFILE; schema: xmlSchemaPtr); XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

(*
 * Interfaces for validating
 *)
procedure xmlSchemaSetValidErrors(ctxt: xmlSchemaParserCtxtPtr; err: xmlSchemaValidityErrorFunc; warn: xmlSchemaValidityWarningFunc; ctx: pointer); XMLCALL; XMLPUBFUN;
procedure xmlSchemaSetValidStructuredErrors(ctxt: xmlSchemaParserCtxtPtr; serror: xmlStructuredErrorFunc; ctx: pointer); XMLCALL; XMLPUBFUN;
function xmlSchemaGetValidErrors(ctxt: xmlSchemaParserCtxtPtr; var err: xmlSchemaValidityErrorFunc; var warn: xmlSchemaValidityWarningFunc; var ctx: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaSetValidOptions(ctxt: xmlSchemaValidCtxtPtr; options: cint): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaValidCtxtGetOptions(ctxt: xmlSchemaValidCtxtPtr): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaNewValidCtxt(schema: xmlSchemaPtr): xmlSchemaValidCtxtPtr; XMLCALL; XMLPUBFUN;
procedure xmlSchemaFreeValidCtxt(ctxt: xmlSchemaValidCtxtPtr); XMLCALL; XMLPUBFUN;
function xmlSchemaNewValidCtxt(ctxt: xmlSchemaValidCtxtPtr; instance: xmlDocPtr): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaValidateOneElement(ctxt: xmlSchemaValidCtxtPtr; elem: xmlNodePtr): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaValidateStream(ctxt: xmlSchemaValidCtxtPtr; input: xmlParserInputBufferPtr; enc: xmlCharEncoding; sax: xmlSAXHandlerPtr; user_data: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaValidateFile(ctxt: xmlSchemaValidCtxtPtr; filename: pchar; options: cint): cint; XMLCALL; XMLPUBFUN;

(*
 * Interface to insert Schemas SAX velidation in a SAX stream
 *)
function xmlSchemaSAXPlug(ctxt: xmlSchemaValidCtxtPtr; var sax: xmlSchemaSAXPlugPtr; var user_data: pointer): xmlSchemaSAXPlugPtr; XMLCALL; XMLPUBFUN;
function xmlSchemaSAXUnplug(plug: xmlSchemaSAXPlugPtr): cint; XMLCALL; XMLPUBFUN;
{$ENDIF}

{$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)
