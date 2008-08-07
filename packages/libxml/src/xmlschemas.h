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
  xmlSchemaValidOption = (
    XML_SCHEMA_VAL_VC_I_CREATE			= 1 shl 0,
	(* Default/fixed: create an attribute node
	* or an element's text node on the instance.
	*)

    XML_SCHEMA_VAL_XSI_ASSEMBLE			= 1 shl 1
(*
	* assemble schemata using
	* xsi:schemaLocation and
	* xsi:noNamespaceSchemaLocation
*)
  );

(**
 * The schemas related types are kept internal
 *)
  xmlSchema = record
  end;

(**
 * A schemas validation context
 *)
  xmlSchemaValidityErrorFunc = procedure(ctx: pointer; msg: pchar; args: array of const); cdecl;
  xmlSchemaValidityWarningFunc = procedure(ctx: pointer; msg: pchar; args: array of const); cdecl;

  xmlSchemaParserCtxt = record
  end;

  xmlSchemaValidCtxt = record
  end;

  xmlSchemaSAXPlugStruct = record
  end;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Interfaces for parsing.
 *)
function xmlSchemaNewParserCtxt(URL: pchar): xmlSchemaParserCtxtPtr; cdecl; external;
function xmlSchemaNewMemParserCtxt(buffer: pchar; size: cint): xmlSchemaParserCtxtPtr; cdecl; external;
function xmlSchemaNewDocParserCtxt(doc: xmlDocPtr): xmlSchemaParserCtxtPtr; cdecl; external;
procedure xmlSchemaFreeParserCtxt(ctxt: xmlSchemaParserCtxtPtr); cdecl; external;
procedure xmlSchemaSetParserErrors(ctxt: xmlSchemaParserCtxtPtr; err: xmlSchemaValidityErrorFunc; warn: xmlSchemaValidityWarningFunc; ctx: pointer); cdecl; external;
procedure xmlSchemaSetParserStructuredErrors(ctxt: xmlSchemaParserCtxtPtr; serror: xmlStructuredErrorFunc; ctx: pointer); cdecl; external;
function xmlSchemaSetParserErrors(ctxt: xmlSchemaParserCtxtPtr; var err: xmlSchemaValidityErrorFunc; var warn: xmlSchemaValidityWarningFunc; var ctx: pointer): cint; cdecl; external;
function xmlSchemaIsValid(ctxt: xmlSchemaValidCtxtPtr): cint; cdecl; external;
function xmlSchemaParse(ctxt: xmlSchemaParserCtxtPtr): xmlSchemaPtr; cdecl; external;
procedure xmlSchemaFree(schema: xmlSchemaPtr); cdecl; external;
{$IFDEF LIBXML_OUTPUT_ENABLED}
{XMLPUBFUN void XMLCALL		
	    xmlSchemaDump		(FILE *output,
					 xmlSchemaPtr schema);}
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

(*
 * Interfaces for validating
 *)
procedure xmlSchemaSetValidErrors(ctxt: xmlSchemaParserCtxtPtr; err: xmlSchemaValidityErrorFunc; warn: xmlSchemaValidityWarningFunc; ctx: pointer); cdecl; external;
procedure xmlSchemaSetValidStructuredErrors(ctxt: xmlSchemaParserCtxtPtr; serror: xmlStructuredErrorFunc; ctx: pointer); cdecl; external;
function xmlSchemaGetValidErrors(ctxt: xmlSchemaParserCtxtPtr; var err: xmlSchemaValidityErrorFunc; var warn: xmlSchemaValidityWarningFunc; var ctx: pointer): cint; cdecl; external;
function xmlSchemaSetValidOptions(ctxt: xmlSchemaValidCtxtPtr; options: cint): cint; cdecl; external;
function xmlSchemaValidCtxtGetOptions(ctxt: xmlSchemaValidCtxtPtr): cint; cdecl; external;
function xmlSchemaNewValidCtxt(schema: xmlSchemaPtr): xmlSchemaValidCtxtPtr; cdecl; external;
procedure xmlSchemaFreeValidCtxt(ctxt: xmlSchemaValidCtxtPtr); cdecl; external;
function xmlSchemaNewValidCtxt(ctxt: xmlSchemaValidCtxtPtr; instance: xmlDocPtr): cint; cdecl; external;
function xmlSchemaValidateOneElement(ctxt: xmlSchemaValidCtxtPtr; elem: xmlNodePtr): cint; cdecl; external;
function xmlSchemaValidateStream(ctxt: xmlSchemaValidCtxtPtr; input: xmlParserInputBufferPtr; enc: xmlCharEncoding; sax: xmlSAXHandlerPtr; user_data: pointer): cint; cdecl; external;
function xmlSchemaValidateFile(ctxt: xmlSchemaValidCtxtPtr; filename: pchar; options: cint): cint; cdecl; external;

(*
 * Interface to insert Schemas SAX velidation in a SAX stream
 *)
function xmlSchemaSAXPlug(ctxt: xmlSchemaValidCtxtPtr; var sax: xmlSchemaSAXPlugPtr; var user_data: pointer): xmlSchemaSAXPlugPtr; cdecl; external;
function xmlSchemaSAXUnplug(plug: xmlSchemaSAXPlugPtr): cint; cdecl; external;
{$ENDIF}

{$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)
