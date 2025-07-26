(*
 * Summary: the XSLT engine transformation part.
 * Description: This module implements the bulk of the actual
 *              transformation processing. Most of the xsl: element
 *              constructs are implemented in this module.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF FUNCTION}
(**
 * XInclude default processing.
 *)
procedure xsltSetXIncludeDefault(xinclude: cint); EXTDECL; external xsltlib;
function xsltGetXIncludeDefault: cint; EXTDECL; external xsltlib;

(**
 * Export context to users.
 *)
function xsltNewTransformContext(style: xsltStylesheetPtr; doc: xmlDocPtr): xsltTransformContextPtr; EXTDECL; external xsltlib;
procedure xsltFreeTransformContext(ctxt: xsltTransformContextPtr); EXTDECL; external xsltlib;
function xsltApplyStylesheetUser(style: xsltStylesheetPtr; doc: xmlDocPtr; const params: PPAnsiChar; const output: PAnsiChar; profile: Pointer; userCtxt: xsltTransformContextPtr): xmlDocPtr; EXTDECL; external xsltlib;
procedure xsltProcessOneNode(ctxt: xsltTransformContextPtr; node: xmlNodePtr; params: xsltStackElemPtr); EXTDECL; external xsltlib;

(**
 * Private Interfaces.
 *)
procedure xsltApplyStripSpaces(ctxt: xsltTransformContextPtr; node: xmlNodePtr); EXTDECL; external xsltlib;
function xsltApplyStylesheet(style: xsltStylesheetPtr; doc: xmlDocPtr; const params: PPAnsiChar): xmlDocPtr; EXTDECL; external xsltlib;
function xsltProfileStylesheet(style: xsltStylesheetPtr; doc: xmlDocPtr; const params: PPAnsiChar; output: Pointer): xmlDocPtr; EXTDECL; external xsltlib;
function xsltRunStylesheet(style: xsltStylesheetPtr; doc: xmlDocPtr; const params: PPAnsiChar; const output: PAnsiChar; SAX: xmlSAXHandlerPtr; IObuf: xmlOutputBufferPtr): cint; EXTDECL; external xsltlib;
function xsltRunStylesheetUser(style: xsltStylesheetPtr; doc: xmlDocPtr; const params: PPAnsiChar; const output: PAnsiChar; SAX: xmlSAXHandlerPtr; IObuf: xmlOutputBufferPtr; profile: Pointer; userCtxt: xsltTransformContextPtr): cint; EXTDECL; external xsltlib;
procedure xsltApplyOneTemplate(ctxt: xsltTransformContextPtr; node: xmlNodePtr; list: xmlNodePtr; templ: xsltTemplatePtr; params: xsltStackElemPtr); EXTDECL; external xsltlib;
procedure xsltDocumentElem(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltSort(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltCopy(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltText(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltElement(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltComment(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltAttribute(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltProcessingInstruction(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltCopyOf(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltValueOf(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltNumber(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltApplyImports(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltCallTemplate(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltApplyTemplates(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltChoose(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltIf(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltForEach(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltRegisterAllElement(ctxt: xsltTransformContextPtr); EXTDECL; external xsltlib;
function xsltCopyTextString(ctxt: xsltTransformContextPtr; target: xmlNodePtr; const &string: xmlCharPtr; noescape: cint): xmlNodePtr; EXTDECL; external xsltlib;

(* Following 2 functions needed for libexslt/functions.c *)
procedure xsltLocalVariablePop(ctxt: xsltTransformContextPtr; limitNr: cint; level: cint); EXTDECL; external xsltlib;
function xsltLocalVariablePush(ctxt: xsltTransformContextPtr; variable: xsltStackElemPtr; level: cint): cint; EXTDECL; external xsltlib;

(*
 * Hook for the debugger if activated.
 *)
procedure xslHandleDebugger(cur: xmlNodePtr; node: xmlNodePtr; templ: xsltTemplatePtr; ctxt: xsltTransformContextPtr); EXTDECL; external xsltlib;

{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
(**
 * XInclude default processing.
 *)
  xsltSetXIncludeDefault: procedure(xinclude: cint); EXTDECL;
  xsltGetXIncludeDefault: function: cint; EXTDECL;

(**
 * Export context to users.
 *)
  xsltNewTransformContext: function(style: xsltStylesheetPtr; doc: xmlDocPtr): xsltTransformContextPtr; EXTDECL;
  xsltFreeTransformContext: procedure(ctxt: xsltTransformContextPtr); EXTDECL;
  xsltApplyStylesheetUser: function(style: xsltStylesheetPtr; doc: xmlDocPtr; const params: PPAnsiChar; const output: PAnsiChar; profile: Pointer; userCtxt: xsltTransformContextPtr): xmlDocPtr; EXTDECL;
  xsltProcessOneNode: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; params: xsltStackElemPtr); EXTDECL;

(**
 * Private Interfaces.
 *)
  xsltApplyStripSpaces: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr); EXTDECL;
  xsltApplyStylesheet: function(style: xsltStylesheetPtr; doc: xmlDocPtr; const params: PPAnsiChar): xmlDocPtr; EXTDECL;
  xsltProfileStylesheet: function(style: xsltStylesheetPtr; doc: xmlDocPtr; const params: PPAnsiChar; output: Pointer): xmlDocPtr; EXTDECL;
  xsltRunStylesheet: function(style: xsltStylesheetPtr; doc: xmlDocPtr; const params: PPAnsiChar; const output: PAnsiChar; SAX: xmlSAXHandlerPtr; IObuf: xmlOutputBufferPtr): cint; EXTDECL;
  xsltRunStylesheetUser: function(style: xsltStylesheetPtr; doc: xmlDocPtr; const params: PPAnsiChar; const output: PAnsiChar; SAX: xmlSAXHandlerPtr; IObuf: xmlOutputBufferPtr; profile: Pointer; userCtxt: xsltTransformContextPtr): cint; EXTDECL;
  xsltApplyOneTemplate: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; list: xmlNodePtr; templ: xsltTemplatePtr; params: xsltStackElemPtr); EXTDECL;
  xsltDocumentElem: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltSort: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltCopy: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltText: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltElement: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltComment: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltAttribute: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltProcessingInstruction: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltCopyOf: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltValueOf: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltNumber: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltApplyImports: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltCallTemplate: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltApplyTemplates: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltChoose: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltIf: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltForEach: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltRegisterAllElement: procedure(ctxt: xsltTransformContextPtr); EXTDECL;
  xsltCopyTextString: function(ctxt: xsltTransformContextPtr; target: xmlNodePtr; const &string: xmlCharPtr; noescape: cint): xmlNodePtr; EXTDECL;

(* Following 2 functions needed for libexslt/functions.c *)
  xsltLocalVariablePop: procedure(ctxt: xsltTransformContextPtr; limitNr: cint; level: cint); EXTDECL;
  xsltLocalVariablePush: function(ctxt: xsltTransformContextPtr; variable: xsltStackElemPtr; level: cint): cint; EXTDECL;

(*
 * Hook for the debugger if activated.
 *)
  xslHandleDebugger: procedure(cur: xmlNodePtr; node: xmlNodePtr; templ: xsltTemplatePtr; ctxt: xsltTransformContextPtr); EXTDECL;

{$ENDIF} {FUNCTIONVAR}
