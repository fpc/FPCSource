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
  xmlSchematronValidityErrorFunc = procedure(ctx: pointer; msg: pchar; args: array of const); XMLCDECL;
  xmlSchematronValidityWarningFunc = procedure(ctx: pointer; msg: pchar; args: array of const); XMLCDECL;

  xmlSchematronParserCtxt = record end;

  xmlSchematronValidCtxt = record end;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Interfaces for parsing.
 *)
function xmlSchematronNewParserCtxt(URL: pchar): xmlSchematronParserCtxtPtr; XMLCALL; XMLPUBFUN;
function xmlSchematronNewMemParserCtxt(buffer: pchar; size: cint): xmlSchematronParserCtxtPtr; XMLCALL; XMLPUBFUN;
function xmlSchematronNewDocParserCtxt(doc: xmlDocPtr): xmlSchematronParserCtxtPtr; XMLCALL; XMLPUBFUN;
procedure xmlSchematronFreeParserCtxt(ctxt: xmlSchematronParserCtxtPtr); XMLCALL; XMLPUBFUN;
(*****
XMLPUBFUN void XMLCALL		
	    xmlSchematronSetParserErrors(xmlSchematronParserCtxtPtr ctxt,
					 xmlSchematronValidityErrorFunc err,
					 xmlSchematronValidityWarningFunc warn,
					 void *ctx);
XMLPUBFUN int XMLCALL
		xmlSchematronGetParserErrors(xmlSchematronParserCtxtPtr ctxt,
					xmlSchematronValidityErrorFunc * err,
					xmlSchematronValidityWarningFunc * warn,
					void **ctx);
XMLPUBFUN int XMLCALL
		xmlSchematronIsValid	(xmlSchematronValidCtxtPtr ctxt);
 *****)
function xmlSchematronParse(ctxt: xmlSchematronParserCtxtPtr): xmlSchematronPtr; XMLCALL; XMLPUBFUN;
procedure xmlSchematronFree(schema: xmlSchematronPtr); XMLCALL; XMLPUBFUN;

(*
 * Interfaces for validating
 *)
procedure xmlSchematronSetValidStructuredErrors(ctxt: xmlSchematronValidCtxtPtr; serror: xmlStructuredErrorFunc; ctx: pointer); XMLCALL; XMLPUBFUN;
(******
XMLPUBFUN void XMLCALL		
	    xmlSchematronSetValidErrors	(xmlSchematronValidCtxtPtr ctxt,
					 xmlSchematronValidityErrorFunc err,
					 xmlSchematronValidityWarningFunc warn,
					 void *ctx);
XMLPUBFUN int XMLCALL
	    xmlSchematronGetValidErrors	(xmlSchematronValidCtxtPtr ctxt,
					 xmlSchematronValidityErrorFunc *err,
					 xmlSchematronValidityWarningFunc *warn,
					 void **ctx);
XMLPUBFUN int XMLCALL
	    xmlSchematronSetValidOptions(xmlSchematronValidCtxtPtr ctxt,
					 int options);
XMLPUBFUN int XMLCALL
	    xmlSchematronValidCtxtGetOptions(xmlSchematronValidCtxtPtr ctxt);
XMLPUBFUN int XMLCALL
            xmlSchematronValidateOneElement (xmlSchematronValidCtxtPtr ctxt,
			                 xmlNodePtr elem);
 *******)
function xmlSchematronNewValidCtxt(schema: xmlSchematronPtr; options: cint): xmlSchematronValidCtxtPtr; XMLCALL; XMLPUBFUN;
procedure xmlSchematronFreeValidCtxt(ctxt: xmlSchematronValidCtxtPtr); XMLCALL; XMLPUBFUN;
function xmlSchematronValidateDoc(ctxt: xmlSchematronValidCtxtPtr; instance: xmlDocPtr): cint; XMLCALL; XMLPUBFUN;

{$ENDIF}
{$ENDIF} (* LIBXML_SCHEMATRON_ENABLED *)
