(*
 * Summary: interface for the XSLT import support
 * Description: macros and fuctions needed to implement and
 *              access the import tree
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

(**
 * XSLT_GET_IMPORT_PTR:
 *
 * A macro to import pointers from the stylesheet cascading order.
 */
#define XSLT_GET_IMPORT_PTR(res, style, name) {			\
    xsltStylesheetPtr st = style;				\
    res = NULL;							\
    while (st != NULL) {					\
	if (st->name != NULL) { res = st->name; break; }	\
	st = xsltNextImport(st);				\
    }}

/**
 * XSLT_GET_IMPORT_INT:
 *
 * A macro to import intergers from the stylesheet cascading order.
 */
#define XSLT_GET_IMPORT_INT(res, style, name) {			\
    xsltStylesheetPtr st = style;				\
    res = -1;							\
    while (st != NULL) {					\
	if (st->name != -1) { res = st->name; break; }	\
	st = xsltNextImport(st);				\
    }}
*)

{$IFDEF FUNCTION}
(*
 * Module interfaces
 *)
function xsltParseStylesheetImport(style: xsltStylesheetPtr; cur: xmlNodePtr): cint; EXTDECL; external xsltlib;
function xsltParseStylesheetInclude(style: xsltStylesheetPtr; cur: xmlNodePtr): cint; EXTDECL; external xsltlib;
function xsltNextImport(style: xsltStylesheetPtr): xsltStylesheetPtr; EXTDECL; external xsltlib;
function xsltNeedElemSpaceHandling(ctxt: xsltTransformContextPtr): cint; EXTDECL; external xsltlib;
function xsltFindElemSpaceHandling(ctxt: xsltTransformContextPtr; node: xmlNodePtr): cint; EXTDECL; external xsltlib;
function xsltFindTemplate(ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const nameURI: xmlCharPtr): xsltTemplatePtr; EXTDECL; external xsltlib;
{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
(*
 * Module interfaces
 *)
  xsltParseStylesheetImport: function(style: xsltStylesheetPtr; cur: xmlNodePtr): cint; EXTDECL;
  xsltParseStylesheetInclude: function(style: xsltStylesheetPtr; cur: xmlNodePtr): cint; EXTDECL;
  xsltNextImport: function(style: xsltStylesheetPtr): xsltStylesheetPtr; EXTDECL;
  xsltNeedElemSpaceHandling: function(ctxt: xsltTransformContextPtr): cint; EXTDECL;
  xsltFindElemSpaceHandling: function(ctxt: xsltTransformContextPtr; node: xmlNodePtr): cint; EXTDECL;
  xsltFindTemplate: function(ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const nameURI: xmlCharPtr): xsltTemplatePtr; EXTDECL;
{$ENDIF} {FUNCTIONVAR}

