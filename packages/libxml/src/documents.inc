(*
 * Summary: interface for the document handling
 * Description: implements document loading and cache (multiple
 *              document() reference for the same resources must
 *              be equal.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF FUNCTION}
function xsltNewDocument(ctxt: xsltTransformContextPtr; doc: xmlDocPtr): xsltDocumentPtr; EXTDECL; external xsltlib;
function xsltLoadDocument(ctxt: xsltTransformContextPtr; const URI: xmlCharPtr): xsltDocumentPtr; EXTDECL; external xsltlib;
function xsltFindDocument(ctxt: xsltTransformContextPtr; doc: xmlDocPtr): xsltDocumentPtr; EXTDECL; external xsltlib;
procedure xsltFreeDocuments(ctxt: xsltTransformContextPtr); EXTDECL; external xsltlib;
function xsltLoadStyleDocument(style: xsltStylesheetPtr; const URI: xmlCharPtr): xsltDocumentPtr; EXTDECL; external xsltlib;
function xsltNewStyleDocument(style: xsltStylesheetPtr; doc: xmlDocPtr): xsltDocumentPtr; EXTDECL; external xsltlib;
procedure xsltFreeStyleDocuments(style: xsltStylesheetPtr); EXTDECL; external xsltlib;
{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
  xsltNewDocument: function(ctxt: xsltTransformContextPtr; doc: xmlDocPtr): xsltDocumentPtr; EXTDECL;
  xsltLoadDocument: function(ctxt: xsltTransformContextPtr; const URI: xmlCharPtr): xsltDocumentPtr; EXTDECL;
  xsltFindDocument: function(ctxt: xsltTransformContextPtr; doc: xmlDocPtr): xsltDocumentPtr; EXTDECL;
  xsltFreeDocuments: procedure(ctxt: xsltTransformContextPtr); EXTDECL;
  xsltLoadStyleDocument: function(style: xsltStylesheetPtr; const URI: xmlCharPtr): xsltDocumentPtr; EXTDECL;
  xsltNewStyleDocument: function(style: xsltStylesheetPtr; doc: xmlDocPtr): xsltDocumentPtr; EXTDECL;
  xsltFreeStyleDocuments: procedure(style: xsltStylesheetPtr); EXTDECL;
{$ENDIF} {FUNCTIONVAR}

(*
 * Hooks for document loading
 *)

{$IFDEF TYPE}
(**
 * xsltLoadType:
 *
 * Enum defining the kind of loader requirement.
 *)
  xsltLoadType = (
    XSLT_LOAD_START = 0,          (* loading for a top stylesheet *)
    XSLT_LOAD_STYLESHEET,         (* loading for a stylesheet include/import *)
    XSLT_LOAD_DOCUMENT            (* loading document at transformation time *)
  );

(**
 * xsltDocLoaderFunc:
 * @URI: the URI of the document to load
 * @dict: the dictionary to use when parsing that document
 * @options: parsing options, a set of xmlParserOption
 * @ctxt: the context, either a stylesheet or a transformation context
 * @type: the xsltLoadType indicating the kind of loading required
 *
 * An xsltDocLoaderFunc is a signature for a function which can be
 * registered to load document not provided by the compilation or
 * transformation API themselve, for example when an xsl:import,
 * xsl:include is found at compilation time or when a document()
 * call is made at runtime.
 *
 * Returns the pointer to the document (which will be modified and
 * freed by the engine later), or NULL in case of error.
 *)
  xsltDocLoaderFunc = function(const URI: xmlCharPtr;
    dict: xmlDictPtr;
    options: cint;
    ctxt: Pointer;
    &type: xsltLoadType): xmlDocPtr; EXTDECL;
{$ENDIF} {TYPE}

{$IFDEF FUNCTION}
procedure xsltSetLoaderFunc(f: xsltDocLoaderFunc); EXTDECL; external xsltlib;
{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
  xsltSetLoaderFunc: procedure(f: xsltDocLoaderFunc); EXTDECL;
{$ENDIF} {FUNCTIONVAR}

{$IFDEF EXTVAR}
(* the loader may be needed by extension libraries so it is exported *)
var
  xsltDocDefaultLoader: xsltDocLoaderFunc;
{$ENDIF}
