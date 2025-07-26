(*
 * Summary: interface for the variable matching and lookup.
 * Description: interface for the variable matching and lookup.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)


(**
 * XSLT_REGISTER_VARIABLE_LOOKUP:
 *
 * Registering macro, not general purpose at all but used in different modules.
 *)

{
#define XSLT_REGISTER_VARIABLE_LOOKUP(ctxt)			\
    xmlXPathRegisterVariableLookup((ctxt)->xpathCtxt,		\
	       xsltXPathVariableLookup,	(void *)(ctxt));	\
    xsltRegisterAllFunctions((ctxt)->xpathCtxt);		\
    xsltRegisterAllElement(ctxt);				\
    (ctxt)->xpathCtxt->extra = ctxt
}

(*
 * Flags for memory management of RVTs
 *)

{$IFDEF CONST}
  (**
   * XSLT_RVT_LOCAL:
   *
   * RVT is destroyed after the current instructions ends.
   *)
  XSLT_RVT_LOCAL = 1;

  (**
   * XSLT_RVT_FUNC_RESULT:
   *
   * RVT is part of results returned with func:result. The RVT won't be
   * destroyed after exiting a template and will be reset to XSLT_RVT_LOCAL or
   * XSLT_RVT_VARIABLE in the template that receives the return value.
   *)
  XSLT_RVT_FUNC_RESULT = 2;

  (**
   * XSLT_RVT_GLOBAL:
   *
   * RVT is part of a global variable.
   *)
  XSLT_RVT_GLOBAL = 3;
{$ENDIF} {CONST}

{$IFDEF FUNCTION}

(*
 * Interfaces for the variable module.
 *)

function xsltEvalGlobalVariables(ctxt: xsltTransformContextPtr): cint; EXTDECL; external xsltlib;
function xsltEvalUserParams(ctxt: xsltTransformContextPtr; const params: PPAnsiChar): cint; EXTDECL; external xsltlib;
function xsltQuoteUserParams(ctxt: xsltTransformContextPtr; const params: PPAnsiChar): cint; EXTDECL; external xsltlib;
function xsltEvalOneUserParam(ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const value: xmlCharPtr): cint; EXTDECL; external xsltlib;
function xsltQuoteOneUserParam(ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const value: xmlCharPtr): cint; EXTDECL; external xsltlib;
procedure xsltParseGlobalVariable(style: xsltStylesheetPtr; cur: xmlNodePtr); EXTDECL; external xsltlib;
procedure xsltParseGlobalParam(style: xsltStylesheetPtr; cur: xmlNodePtr); EXTDECL; external xsltlib;
procedure xsltParseStylesheetVariable(ctxt: xsltTransformContextPtr; cur: xmlNodePtr); EXTDECL; external xsltlib;
procedure xsltParseStylesheetParam(ctxt: xsltTransformContextPtr; cur: xmlNodePtr); EXTDECL; external xsltlib;
function xsltParseStylesheetCallerParam(ctxt: xsltTransformContextPtr; cur: xmlNodePtr): xsltStackElemPtr; EXTDECL; external xsltlib;
function xsltAddStackElemList(ctxt: xsltTransformContextPtr; elems: xsltStackElemPtr): cint; EXTDECL; external xsltlib;
procedure xsltFreeGlobalVariables(ctxt: xsltTransformContextPtr); EXTDECL; external xsltlib;
function xsltVariableLookup(ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const ns_uri: xmlCharPtr): xmlXPathObjectPtr; EXTDECL; external xsltlib;
function xsltXPathVariableLookup(ctxt: Pointer; const name: xmlCharPtr; const ns_uri: xmlCharPtr): xmlXPathObjectPtr; EXTDECL; external xsltlib;
{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}

(*
 * Interfaces for the variable module.
 *)

  xsltEvalGlobalVariables: function(ctxt: xsltTransformContextPtr): cint; EXTDECL;
  xsltEvalUserParams: function(ctxt: xsltTransformContextPtr; const params: PPAnsiChar): cint; EXTDECL;
  xsltQuoteUserParams: function(ctxt: xsltTransformContextPtr; const params: PPAnsiChar): cint; EXTDECL;
  xsltEvalOneUserParam: function(ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const value: xmlCharPtr): cint; EXTDECL;
  xsltQuoteOneUserParam: function(ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const value: xmlCharPtr): cint; EXTDECL;
  xsltParseGlobalVariable: procedure(style: xsltStylesheetPtr; cur: xmlNodePtr); EXTDECL;
  xsltParseGlobalParam: procedure(style: xsltStylesheetPtr; cur: xmlNodePtr); EXTDECL;
  xsltParseStylesheetVariable: procedure(ctxt: xsltTransformContextPtr; cur: xmlNodePtr); EXTDECL;
  xsltParseStylesheetParam: procedure(ctxt: xsltTransformContextPtr; cur: xmlNodePtr); EXTDECL;
  xsltParseStylesheetCallerParam: function(ctxt: xsltTransformContextPtr; cur: xmlNodePtr): xsltStackElemPtr; EXTDECL;
  xsltAddStackElemList: function(ctxt: xsltTransformContextPtr; elems: xsltStackElemPtr): cint; EXTDECL;
  xsltFreeGlobalVariables: procedure(ctxt: xsltTransformContextPtr); EXTDECL;
  xsltVariableLookup: function(ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const ns_uri: xmlCharPtr): xmlXPathObjectPtr; EXTDECL;
  xsltXPathVariableLookup: function(ctxt: Pointer; const name: xmlCharPtr; const ns_uri: xmlCharPtr): xmlXPathObjectPtr; EXTDECL;
{$ENDIF} {FUNCTIONVAR}


