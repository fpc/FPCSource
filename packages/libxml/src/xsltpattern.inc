(*
 * Summary: interface for the pattern matching used in template matches.
 * Description: the implementation of the lookup of the right template
 *              for a given node must be really fast in order to keep
 *              decent performances.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF TYPE}
(**
 * xsltCompMatch:
 *
 * Data structure used for the implementation of patterns.
 * It is kept private (in pattern.c).
 *)
  xsltCompMatch = record end;
  xsltCompMatchPtr = ^xsltCompMatch;

{$ENDIF}


{$IFDEF FUNCTION}
(*
 * Pattern related interfaces.
 *)

function xsltCompilePattern(const pattern: xmlCharPtr; doc: xmlDocPtr; node: xmlNodePtr; style: xsltStylesheetPtr; runtime: xsltTransformContextPtr): xsltCompMatchPtr; EXTDECL; external xsltlib;
procedure xsltFreeCompMatchList(comp: xsltCompMatchPtr); EXTDECL; external xsltlib;
function xsltTestCompMatchList(ctxt: xsltTransformContextPtr; node: xmlNodePtr; comp: xsltCompMatchPtr): cint; EXTDECL; external xsltlib;
procedure xsltCompMatchClearCache(ctxt: xsltTransformContextPtr; comp: xsltCompMatchPtr); EXTDECL; external xsltlib;
procedure xsltNormalizeCompSteps(payload: Pointer; data: Pointer; const name: xmlCharPtr); EXTDECL; external xsltlib;

(*
 * Template related interfaces.
 *)
function xsltAddTemplate(style: xsltStylesheetPtr; cur: xsltTemplatePtr; const mode: xmlCharPtr; const modeURI: xmlCharPtr): cint; EXTDECL; external xsltlib;
function xsltGetTemplate(ctxt: xsltTransformContextPtr; node: xmlNodePtr; style: xsltStylesheetPtr): xsltTemplatePtr; EXTDECL; external xsltlib;
procedure xsltFreeTemplateHashes(style: xsltStylesheetPtr); EXTDECL; external xsltlib;
procedure xsltCleanupTemplates(style: xsltStylesheetPtr); EXTDECL; external xsltlib;

{.$if 0
function xsltMatchPattern(ctxt: xsltTransformContextPtr; node: xmlNodePtr; const pattern: xmlCharPtr; ctxtdoc: xmlDocPtr; ctxtnode: xmlNodePtr): cint; EXTDECL; external xsltlib;
endif}

{$ENDIF}

{$IFDEF FUNCTIONVAR}
(*
 * Pattern related interfaces.
 *)

  xsltCompilePattern: function(const pattern: xmlCharPtr; doc: xmlDocPtr; node: xmlNodePtr; style: xsltStylesheetPtr; runtime: xsltTransformContextPtr): xsltCompMatchPtr; EXTDECL;
  xsltFreeCompMatchList: procedure(comp: xsltCompMatchPtr); EXTDECL;
  xsltTestCompMatchList: function(ctxt: xsltTransformContextPtr; node: xmlNodePtr; comp: xsltCompMatchPtr): cint; EXTDECL;
  xsltCompMatchClearCache: procedure(ctxt: xsltTransformContextPtr; comp: xsltCompMatchPtr); EXTDECL;
  xsltNormalizeCompSteps: procedure(payload: Pointer; data: Pointer; const name: xmlCharPtr); EXTDECL;

(*
 * Template related interfaces.
 *)
  xsltAddTemplate: function(style: xsltStylesheetPtr; cur: xsltTemplatePtr; const mode: xmlCharPtr; const modeURI: xmlCharPtr): cint; EXTDECL;
  xsltGetTemplate: function(ctxt: xsltTransformContextPtr; node: xmlNodePtr; style: xsltStylesheetPtr): xsltTemplatePtr; EXTDECL;
  xsltFreeTemplateHashes: procedure(style: xsltStylesheetPtr); EXTDECL;
  xsltCleanupTemplates: procedure(style: xsltStylesheetPtr); EXTDECL;

{.$if 0
  xsltMatchPattern: function(ctxt: xsltTransformContextPtr; node: xmlNodePtr; const pattern: xmlCharPtr; ctxtdoc: xmlDocPtr; ctxtnode: xmlNodePtr): cint; EXTDECL;
endif}

{$ENDIF}
