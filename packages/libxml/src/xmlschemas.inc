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
  xmlSchemaValidityErrorFunc = procedure(ctx: pointer; msg: PAnsiChar); cdecl; varargs;
  xmlSchemaValidityWarningFunc = procedure(ctx: pointer; msg: PAnsiChar); cdecl; varargs;

  xmlSchemaParserCtxt = record end;

  xmlSchemaValidCtxt = record end;

  xmlSchemaSAXPlugStruct = record end;

(**
 * xmlSchemaValidityLocatorFunc:
 * @ctx: user provided context
 * @file: returned file information
 * @line: returned line information
 *
 * A schemas validation locator, a callback called by the validator.
 * This is used when file or node informations are not available
 * to find out what file and line number are affected
 *
 * Returns: 0 in case of success and -1 in case of error
 *)

  xmlSchemaValidityLocatorFunc = function(ctx: pointer; const _file: PPAnsiChar; line: pculong): cint; cdecl;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Interfaces for parsing.
 *)
function xmlSchemaNewParserCtxt(URL: PAnsiChar): xmlSchemaParserCtxtPtr; EXTDECL; external xml2lib;
function xmlSchemaNewMemParserCtxt(buffer: PAnsiChar; size: cint): xmlSchemaParserCtxtPtr; EXTDECL; external xml2lib;
function xmlSchemaNewDocParserCtxt(doc: xmlDocPtr): xmlSchemaParserCtxtPtr; EXTDECL; external xml2lib;
procedure xmlSchemaFreeParserCtxt(ctxt: xmlSchemaParserCtxtPtr); EXTDECL; external xml2lib;
procedure xmlSchemaSetParserErrors(ctxt: xmlSchemaParserCtxtPtr; err: xmlSchemaValidityErrorFunc; warn: xmlSchemaValidityWarningFunc; ctx: pointer); EXTDECL; external xml2lib;
procedure xmlSchemaSetParserStructuredErrors(ctxt: xmlSchemaParserCtxtPtr; serror: xmlStructuredErrorFunc; ctx: pointer); EXTDECL; external xml2lib;
function xmlSchemaGetParserErrors(ctxt: xmlSchemaParserCtxtPtr; var err: xmlSchemaValidityErrorFunc; var warn: xmlSchemaValidityWarningFunc; var ctx: pointer): cint; EXTDECL; external xml2lib;
function xmlSchemaIsValid(ctxt: xmlSchemaValidCtxtPtr): cint; EXTDECL; external xml2lib;
function xmlSchemaParse(ctxt: xmlSchemaParserCtxtPtr): xmlSchemaPtr; EXTDECL; external xml2lib;
procedure xmlSchemaFree(schema: xmlSchemaPtr); EXTDECL; external xml2lib;
{$IFDEF LIBXML_OUTPUT_ENABLED}
procedure xmlSchemaDump(output: PFILE; schema: xmlSchemaPtr); EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

(*
 * Interfaces for validating
 *)
procedure xmlSchemaSetValidErrors(ctxt: xmlSchemaValidCtxtPtr; err: xmlSchemaValidityErrorFunc; warn: xmlSchemaValidityWarningFunc; ctx: pointer); EXTDECL; external xml2lib;
procedure xmlSchemaSetValidStructuredErrors(ctxt: xmlSchemaValidCtxtPtr; serror: xmlStructuredErrorFunc; ctx: pointer); EXTDECL; external xml2lib;
function xmlSchemaGetValidErrors(ctxt: xmlSchemaParserCtxtPtr; var err: xmlSchemaValidityErrorFunc; var warn: xmlSchemaValidityWarningFunc; var ctx: pointer): cint; EXTDECL; external xml2lib;
function xmlSchemaSetValidOptions(ctxt: xmlSchemaValidCtxtPtr; options: cint): cint; EXTDECL; external xml2lib;
function xmlSchemaValidCtxtGetOptions(ctxt: xmlSchemaValidCtxtPtr): cint; EXTDECL; external xml2lib;
function xmlSchemaNewValidCtxt(schema: xmlSchemaPtr): xmlSchemaValidCtxtPtr; EXTDECL; external xml2lib;
function xmlSchemaValidCtxtGetParserCtxt(ctxt: xmlSchemaValidCtxtPtr): xmlParserCtxtPtr; EXTDECL; external xml2lib;
procedure xmlSchemaFreeValidCtxt(ctxt: xmlSchemaValidCtxtPtr); EXTDECL; external xml2lib;
function xmlSchemaValidateDoc(ctxt: xmlSchemaValidCtxtPtr; instance: xmlDocPtr): cint; EXTDECL; external xml2lib;
function xmlSchemaValidateOneElement(ctxt: xmlSchemaValidCtxtPtr; elem: xmlNodePtr): cint; EXTDECL; external xml2lib;
function xmlSchemaValidateStream(ctxt: xmlSchemaValidCtxtPtr; input: xmlParserInputBufferPtr; enc: xmlCharEncoding; sax: xmlSAXHandlerPtr; user_data: pointer): cint; EXTDECL; external xml2lib;
function xmlSchemaValidateFile(ctxt: xmlSchemaValidCtxtPtr; filename: PAnsiChar; options: cint): cint; EXTDECL; external xml2lib;
procedure xmlSchemaValidateSetFilename(vctxt: xmlSchemaValidCtxtPtr; const filename: PAnsiChar); EXTDECL; external xml2lib;
procedure xmlSchemaValidateSetLocator(vctxt: xmlSchemaValidCtxtPtr; f: xmlSchemaValidityLocatorFunc; ctxt: Pointer); EXTDECL; external xml2lib;

(*
 * Interface to insert Schemas SAX velidation in a SAX stream
 *)
function xmlSchemaSAXPlug(ctxt: xmlSchemaValidCtxtPtr; var sax: xmlSchemaSAXPlugPtr; var user_data: pointer): xmlSchemaSAXPlugPtr; EXTDECL; external xml2lib;
function xmlSchemaSAXUnplug(plug: xmlSchemaSAXPlugPtr): cint; EXTDECL; external xml2lib;
{$ENDIF}

{$IFDEF FUNCTIONVAR}
(*
 * Interfaces for parsing.
 *)
  xmlSchemaNewParserCtxt: function(URL: PAnsiChar): xmlSchemaParserCtxtPtr; EXTDECL;
  xmlSchemaNewMemParserCtxt: function(buffer: PAnsiChar; size: cint): xmlSchemaParserCtxtPtr; EXTDECL;
  xmlSchemaNewDocParserCtxt: function(doc: xmlDocPtr): xmlSchemaParserCtxtPtr; EXTDECL;
  xmlSchemaFreeParserCtxt: procedure(ctxt: xmlSchemaParserCtxtPtr); EXTDECL;
  xmlSchemaSetParserErrors: procedure(ctxt: xmlSchemaParserCtxtPtr; err: xmlSchemaValidityErrorFunc; warn: xmlSchemaValidityWarningFunc; ctx: pointer); EXTDECL;
  xmlSchemaSetParserStructuredErrors: procedure(ctxt: xmlSchemaParserCtxtPtr; serror: xmlStructuredErrorFunc; ctx: pointer); EXTDECL;
  xmlSchemaGetParserErrors: function(ctxt: xmlSchemaParserCtxtPtr; var err: xmlSchemaValidityErrorFunc; var warn: xmlSchemaValidityWarningFunc; var ctx: pointer): cint; EXTDECL;
  xmlSchemaIsValid: function(ctxt: xmlSchemaValidCtxtPtr): cint; EXTDECL;
  xmlSchemaParse: function(ctxt: xmlSchemaParserCtxtPtr): xmlSchemaPtr; EXTDECL;
  xmlSchemaFree: procedure(schema: xmlSchemaPtr); EXTDECL;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlSchemaDump: procedure(output: PFILE; schema: xmlSchemaPtr); EXTDECL;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

(*
 * Interfaces for validating
 *)
  xmlSchemaSetValidErrors: procedure(ctxt: xmlSchemaValidCtxtPtr; err: xmlSchemaValidityErrorFunc; warn: xmlSchemaValidityWarningFunc; ctx: pointer); EXTDECL;
  xmlSchemaSetValidStructuredErrors: procedure(ctxt: xmlSchemaValidCtxtPtr; serror: xmlStructuredErrorFunc; ctx: pointer); EXTDECL;
  xmlSchemaGetValidErrors: function(ctxt: xmlSchemaParserCtxtPtr; var err: xmlSchemaValidityErrorFunc; var warn: xmlSchemaValidityWarningFunc; var ctx: pointer): cint; EXTDECL;
  xmlSchemaSetValidOptions: function(ctxt: xmlSchemaValidCtxtPtr; options: cint): cint; EXTDECL;
  xmlSchemaValidCtxtGetOptions: function(ctxt: xmlSchemaValidCtxtPtr): cint; EXTDECL;
  xmlSchemaNewValidCtxt: function(schema: xmlSchemaPtr): xmlSchemaValidCtxtPtr; EXTDECL;
  xmlSchemaValidCtxtGetParserCtxt: function(ctxt: xmlSchemaValidCtxtPtr): xmlParserCtxtPtr; EXTDECL;
  xmlSchemaFreeValidCtxt: procedure(ctxt: xmlSchemaValidCtxtPtr); EXTDECL;
  xmlSchemaValidateDoc: function(ctxt: xmlSchemaValidCtxtPtr; instance: xmlDocPtr): cint; EXTDECL;
  xmlSchemaValidateOneElement: function(ctxt: xmlSchemaValidCtxtPtr; elem: xmlNodePtr): cint; EXTDECL;
  xmlSchemaValidateStream: function(ctxt: xmlSchemaValidCtxtPtr; input: xmlParserInputBufferPtr; enc: xmlCharEncoding; sax: xmlSAXHandlerPtr; user_data: pointer): cint; EXTDECL;
  xmlSchemaValidateFile: function(ctxt: xmlSchemaValidCtxtPtr; filename: PAnsiChar; options: cint): cint; EXTDECL;
  xmlSchemaValidateSetFilename: procedure(vctxt: xmlSchemaValidCtxtPtr; const filename: PAnsiChar); EXTDECL;
  xmlSchemaValidateSetLocator: procedure(vctxt: xmlSchemaValidCtxtPtr; f: xmlSchemaValidityLocatorFunc; ctxt: Pointer); EXTDECL;

(*
 * Interface to insert Schemas SAX velidation in a SAX stream
 *)
  xmlSchemaSAXPlug: function(ctxt: xmlSchemaValidCtxtPtr; var sax: xmlSchemaSAXPlugPtr; var user_data: pointer): xmlSchemaSAXPlugPtr; EXTDECL;
  xmlSchemaSAXUnplug: function(plug: xmlSchemaSAXPlugPtr): cint; EXTDECL;
{$ENDIF}

{$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)
