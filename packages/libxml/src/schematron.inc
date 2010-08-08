(*
 * Summary: XML Schemastron implementation
 * Description: interface to the XML Schematron validity checking.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_SCHEMATRON_ENABLED}

{$IFDEF CONST}
  XML_SCHEMATRON_OUT_QUIET = 1 shl 0;	(* quiet no report *)
  XML_SCHEMATRON_OUT_TEXT = 1 shl 1;	(* build a textual report *)
  XML_SCHEMATRON_OUT_XML = 1 shl 2;	(* output SVRL *)
  XML_SCHEMATRON_OUT_ERROR = 1 shl 3;  (* output via xmlStructuredErrorFunc *)
  XML_SCHEMATRON_OUT_FILE = 1 shl 8;	(* output to a file descriptor *)
  XML_SCHEMATRON_OUT_BUFFER = 1 shl 9;	(* output to a buffer *)
  XML_SCHEMATRON_OUT_IO = 1 shl 10;	(* output to I/O mechanism *)
{$ENDIF}

{$IFDEF POINTER}
  xmlSchematronPtr = ^xmlSchematron;
  xmlSchematronParserCtxtPtr = ^xmlSchematronParserCtxt;
  xmlSchematronValidCtxtPtr = ^xmlSchematronValidCtxt;
{$ENDIF}

{$IFDEF TYPE}
  xmlSchematronValidOptions = type cint;

(**
 * The schemas related types are kept internal
 *)
  xmlSchematron = record end;

(**
 * A schemas validation context
 *)
  xmlSchematronValidityErrorFunc = procedure(ctx: pointer; msg: pchar); cdecl; varargs;
  xmlSchematronValidityWarningFunc = procedure(ctx: pointer; msg: pchar); cdecl; varargs;

  xmlSchematronParserCtxt = record end;

  xmlSchematronValidCtxt = record end;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Interfaces for parsing.
 *)
function xmlSchematronNewParserCtxt(URL: pchar): xmlSchematronParserCtxtPtr; EXTDECL; external xml2lib;
function xmlSchematronNewMemParserCtxt(buffer: pchar; size: cint): xmlSchematronParserCtxtPtr; EXTDECL; external xml2lib;
function xmlSchematronNewDocParserCtxt(doc: xmlDocPtr): xmlSchematronParserCtxtPtr; EXTDECL; external xml2lib;
procedure xmlSchematronFreeParserCtxt(ctxt: xmlSchematronParserCtxtPtr); EXTDECL; external xml2lib;
(*****
external xml2lib void EXTDECL		
	    xmlSchematronSetParserErrors(xmlSchematronParserCtxtPtr ctxt,
					 xmlSchematronValidityErrorFunc err,
					 xmlSchematronValidityWarningFunc warn,
					 void *ctx);
external xml2lib int EXTDECL
		xmlSchematronGetParserErrors(xmlSchematronParserCtxtPtr ctxt,
					xmlSchematronValidityErrorFunc * err,
					xmlSchematronValidityWarningFunc * warn,
					void **ctx);
external xml2lib int EXTDECL
		xmlSchematronIsValid	(xmlSchematronValidCtxtPtr ctxt);
 *****)
function xmlSchematronParse(ctxt: xmlSchematronParserCtxtPtr): xmlSchematronPtr; EXTDECL; external xml2lib;
procedure xmlSchematronFree(schema: xmlSchematronPtr); EXTDECL; external xml2lib;

(*
 * Interfaces for validating
 *)
procedure xmlSchematronSetValidStructuredErrors(ctxt: xmlSchematronValidCtxtPtr; serror: xmlStructuredErrorFunc; ctx: pointer); EXTDECL; external xml2lib;
(******
external xml2lib void EXTDECL		
	    xmlSchematronSetValidErrors	(xmlSchematronValidCtxtPtr ctxt,
					 xmlSchematronValidityErrorFunc err,
					 xmlSchematronValidityWarningFunc warn,
					 void *ctx);
external xml2lib int EXTDECL
	    xmlSchematronGetValidErrors	(xmlSchematronValidCtxtPtr ctxt,
					 xmlSchematronValidityErrorFunc *err,
					 xmlSchematronValidityWarningFunc *warn,
					 void **ctx);
external xml2lib int EXTDECL
	    xmlSchematronSetValidOptions(xmlSchematronValidCtxtPtr ctxt,
					 int options);
external xml2lib int EXTDECL
	    xmlSchematronValidCtxtGetOptions(xmlSchematronValidCtxtPtr ctxt);
external xml2lib int EXTDECL
            xmlSchematronValidateOneElement (xmlSchematronValidCtxtPtr ctxt,
			                 xmlNodePtr elem);
 *******)
function xmlSchematronNewValidCtxt(schema: xmlSchematronPtr; options: cint): xmlSchematronValidCtxtPtr; EXTDECL; external xml2lib;
procedure xmlSchematronFreeValidCtxt(ctxt: xmlSchematronValidCtxtPtr); EXTDECL; external xml2lib;
function xmlSchematronValidateDoc(ctxt: xmlSchematronValidCtxtPtr; instance: xmlDocPtr): cint; EXTDECL; external xml2lib;

{$ENDIF}
{$ENDIF} (* LIBXML_SCHEMATRON_ENABLED *)
