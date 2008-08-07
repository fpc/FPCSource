(*
 * Summary: implementation of the Relax-NG validation
 * Description: implementation of the Relax-NG validation
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)


{$IFDEF LIBXML_SCHEMAS_ENABLED}

{$IFDEF TYPE}
  xmlRelaxNG = record
  end;

(**
 * A schemas validation context
 *)
  xmlRelaxNGValidityErrorFunc = procedure(ctx: pointer; msg: pchar; args: array of const); cdecl;
  xmlRelaxNGValidityWarningFunc = procedure(ctx: pointer; msg: pchar; args: array of const); cdecl;

  xmlRelaxNGParserCtxt = record
  end;

  xmlRelaxNGValidCtxt = record
  end;

(*
 * xmlRelaxNGValidErr:
 *
 * List of possible Relax NG validation errors
 *)
  xmlRelaxNGValidErr = (
    XML_RELAXNG_OK = 0,
    XML_RELAXNG_ERR_MEMORY,
    XML_RELAXNG_ERR_TYPE,
    XML_RELAXNG_ERR_TYPEVAL,
    XML_RELAXNG_ERR_DUPID,
    XML_RELAXNG_ERR_TYPECMP,
    XML_RELAXNG_ERR_NOSTATE,
    XML_RELAXNG_ERR_NODEFINE,
    XML_RELAXNG_ERR_LISTEXTRA,
    XML_RELAXNG_ERR_LISTEMPTY,
    XML_RELAXNG_ERR_INTERNODATA,
    XML_RELAXNG_ERR_INTERSEQ,
    XML_RELAXNG_ERR_INTEREXTRA,
    XML_RELAXNG_ERR_ELEMNAME,
    XML_RELAXNG_ERR_ATTRNAME,
    XML_RELAXNG_ERR_ELEMNONS,
    XML_RELAXNG_ERR_ATTRNONS,
    XML_RELAXNG_ERR_ELEMWRONGNS,
    XML_RELAXNG_ERR_ATTRWRONGNS,
    XML_RELAXNG_ERR_ELEMEXTRANS,
    XML_RELAXNG_ERR_ATTREXTRANS,
    XML_RELAXNG_ERR_ELEMNOTEMPTY,
    XML_RELAXNG_ERR_NOELEM,
    XML_RELAXNG_ERR_NOTELEM,
    XML_RELAXNG_ERR_ATTRVALID,
    XML_RELAXNG_ERR_CONTENTVALID,
    XML_RELAXNG_ERR_EXTRACONTENT,
    XML_RELAXNG_ERR_INVALIDATTR,
    XML_RELAXNG_ERR_DATAELEM,
    XML_RELAXNG_ERR_VALELEM,
    XML_RELAXNG_ERR_LISTELEM,
    XML_RELAXNG_ERR_DATATYPE,
    XML_RELAXNG_ERR_VALUE,
    XML_RELAXNG_ERR_LIST,
    XML_RELAXNG_ERR_NOGRAMMAR,
    XML_RELAXNG_ERR_EXTRADATA,
    XML_RELAXNG_ERR_LACKDATA,
    XML_RELAXNG_ERR_INTERNAL,
    XML_RELAXNG_ERR_ELEMWRONG,
    XML_RELAXNG_ERR_TEXTWRONG
  );

(*
 * xmlRelaxNGParserFlags:
 *
 * List of possible Relax NG Parser flags
 *)
  xmlRelaxNGParserFlag = (
    XML_RELAXNGP_NONE = 0,
    XML_RELAXNGP_FREE_DOC = 1,
    XML_RELAXNGP_CRNG = 2
  );

{$ENDIF}

{$IFDEF FUNCTION}
function xmlRelaxNGInitTypes: cint; cdecl; external;
function xmlRelaxNGCleanupTypes: cint; cdecl; external;

(*
 * Interfaces for parsing.
 *)
function xmlRelaxNGNewParserCtxt(URL: pchar): xmlRelaxNGParserCtxtPtr; cdecl; external;
function xmlRelaxNGNewMemParserCtxt(buffer: pchar; size: cint): xmlRelaxNGParserCtxtPtr; cdecl; external;
function xmlRelaxNGNewDocParserCtxt(doc: xmlDocPtr): xmlRelaxNGParserCtxtPtr; cdecl; external;
function xmlRelaxParserSetFlag(ctxt: xmlRelaxNGParserCtxtPtr; flag: cint; doc: xmlDocPtr): cint; cdecl; external;
procedure xmlRelaxNGFreeParserCtxt(ctxt: xmlRelaxNGParserCtxtPtr); cdecl; external;
procedure xmlRelaxNGSetParserErrors(ctxt: xmlRelaxNGParserCtxtPtr; err: xmlRelaxNGValidityErrorFunc; warn: xmlRelaxNGValidityWarningFunc; ctx: pointer); cdecl; external;
function xmlRelaxNGGetParserErrors(ctxt: xmlRelaxNGParserCtxtPtr; var err: xmlRelaxNGValidityErrorFunc; var warn: xmlRelaxNGValidityWarningFunc; var ctx: pointer): cint; cdecl; external;
procedure xmlRelaxNGSetParserStructuredErrors(ctxt: xmlRelaxNGParserCtxtPtr; serror: xmlStructuredErrorFunc; ctx: pointer); cdecl; external;
function xmlRelaxNGParse(ctxt: xmlRelaxNGParserCtxtPtr): xmlRelaxNGPtr; cdecl; external;
procedure xmlRelaxNGFree(schema: xmlRelaxNGPtr); cdecl; external;
{$IFDEF LIBXML_OUTPUT_ENABLED}
{XMLPUBFUN void XMLCALL
		    xmlRelaxNGDump		(FILE *output,
					 xmlRelaxNGPtr schema);
XMLPUBFUN void XMLCALL
		    xmlRelaxNGDumpTree	(FILE * output,
					 xmlRelaxNGPtr schema);}
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)

(*
 * Interfaces for validating
 *)
procedure xmlRelaxNGSetValidErrors(ctxt: xmlRelaxNGValidCtxtPtr; err: xmlRelaxNGValidityErrorFunc; warn: xmlRelaxNGValidityWarningFunc; ctx: pointer); cdecl; external;
function xmlRelaxNGGetValidErrors(ctxt: xmlRelaxNGValidCtxtPtr; var err: xmlRelaxNGValidityErrorFunc; var warn: xmlRelaxNGValidityWarningFunc; var ctx: pointer): cint; cdecl; external;
procedure xmlRelaxNGSetValidErrors(ctxt: xmlRelaxNGValidCtxtPtr; serror: xmlStructuredErrorFunc; ctx: pointer); cdecl; external;
function xmlRelaxNGNewValidCtxt(schema: xmlRelaxNGPtr): xmlRelaxNGValidCtxtPtr; cdecl; external;
procedure xmlRelaxNGFreeValidCtxt(ctxt: xmlRelaxNGValidCtxtPtr); cdecl; external;
function xmlRelaxNGValidateDoc(ctxt: xmlRelaxNGValidCtxtPtr; doc: xmlDocPtr): cint; cdecl; external;

(*
 * Interfaces for progressive validation when possible
 *)
function xmlRelaxNGValidatePushElement(ctxt: xmlRelaxNGValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr): cint; cdecl; external;
function xmlRelaxNGValidatePushCData(ctxt: xmlRelaxNGValidCtxtPtr; data: xmlCharPtr; len: cint): cint; cdecl; external;
function xmlRelaxNGValidatePopElement(ctxt: xmlRelaxNGValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr): cint; cdecl; external;
function xmlRelaxNGValidateFullElement(ctxt: xmlRelaxNGValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr): cint; cdecl; external;
{$ENDIF}
{$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)

