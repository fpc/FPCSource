(*
 * Summary: interface for the extension support
 * Description: This provide the API needed for simple and module
 *              extension support.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)


(**
 * Extension Modules API.
 *)

{$IFDEF FUNCTION}
(**
 * xsltInitGlobals:
 *
 * Initialize the global variables for extensions
 *
 *)

procedure xsltInitGlobals; EXTDECL; external xsltlib;

{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
(**
 * xsltInitGlobals:
 *
 * Initialize the global variables for extensions
 *
 *)

  xsltInitGlobals: procedure; EXTDECL;

{$ENDIF} {FUNCTIONVAR}

{$IFDEF TYPE}
  (**
   * xsltStyleExtInitFunction:
   * @ctxt:  an XSLT stylesheet
   * @URI:  the namespace URI for the extension
   *
   * A function called at initialization time of an XSLT extension module.
   *
   * Returns a pointer to the module specific data for this transformation.
   *)
  xsltStyleExtInitFunction = function(style: xsltStylesheetPtr; const URI: xmlCharPtr): Pointer; EXTDECL;

  (**
   * xsltStyleExtShutdownFunction:
   * @ctxt:  an XSLT stylesheet
   * @URI:  the namespace URI for the extension
   * @data:  the data associated to this module
   *
   * A function called at shutdown time of an XSLT extension module.
   *)
  xsltStyleExtShutdownFunction = procedure(style: xsltStylesheetPtr; const URI: xmlCharPtr; data: Pointer); EXTDECL;

  (**
   * xsltExtInitFunction:
   * @ctxt:  an XSLT transformation context
   * @URI:  the namespace URI for the extension
   *
   * A function called at initialization time of an XSLT extension module.
   *
   * Returns a pointer to the module specific data for this transformation.
   *)
  xsltExtInitFunction = function(ctxt: xsltTransformContextPtr; const URI: xmlCharPtr): Pointer; EXTDECL;

  (**
   * xsltExtShutdownFunction:
   * @ctxt:  an XSLT transformation context
   * @URI:  the namespace URI for the extension
   * @data:  the data associated to this module
   *
   * A function called at shutdown time of an XSLT extension module.
   *)
  xsltExtShutdownFunction = procedure(ctxt: xsltTransformContextPtr; const URI: xmlCharPtr; data: Pointer); EXTDECL;

{$ENDIF} {TYPE}

{$IFDEF FUNCTION}
function xsltRegisterExtModule(const URI: xmlCharPtr; initFunc: xsltExtInitFunction; shutdownFunc: xsltExtShutdownFunction): cint; EXTDECL; external xsltlib;
function xsltRegisterExtModuleFull(const URI: xmlCharPtr; initFunc: xsltExtInitFunction; shutdownFunc: xsltExtShutdownFunction; styleInitFunc: xsltStyleExtInitFunction; styleShutdownFunc: xsltStyleExtShutdownFunction): cint; EXTDECL; external xsltlib;
function xsltUnregisterExtModule(const URI: xmlCharPtr): cint; EXTDECL; external xsltlib;
function xsltGetExtData(ctxt: xsltTransformContextPtr; const URI: xmlCharPtr): Pointer; EXTDECL; external xsltlib;
function xsltStyleGetExtData(style: xsltStylesheetPtr; const URI: xmlCharPtr): Pointer; EXTDECL; external xsltlib;
{$IFDEF XSLT_REFACTORED}
function xsltStyleStylesheetLevelGetExtData(style: xsltStylesheetPtr; const URI: xmlCharPtr): Pointer; EXTDECL; external xsltlib;
{$ENDIF}
procedure xsltShutdownCtxtExts(ctxt: xsltTransformContextPtr); EXTDECL; external xsltlib;
procedure xsltShutdownExts(style: xsltStylesheetPtr); EXTDECL; external xsltlib;
function xsltXPathGetTransformContext(ctxt: xmlXPathParserContextPtr): xsltTransformContextPtr; EXTDECL; external xsltlib;

(*
 * extension functions
 *)
function xsltRegisterExtModuleFunction(const name: xmlCharPtr; const URI: xmlCharPtr; &function: xmlXPathFunction): cint; EXTDECL; external xsltlib;
function xsltExtModuleFunctionLookup(const name: xmlCharPtr; const URI: xmlCharPtr): xmlXPathFunction; EXTDECL; external xsltlib;
function xsltUnregisterExtModuleFunction(const name: xmlCharPtr; const URI: xmlCharPtr): cint; EXTDECL; external xsltlib;
{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
  xsltRegisterExtModule: function(const URI: xmlCharPtr; initFunc: xsltExtInitFunction; shutdownFunc: xsltExtShutdownFunction): cint; EXTDECL;
  xsltRegisterExtModuleFull: function(const URI: xmlCharPtr; initFunc: xsltExtInitFunction; shutdownFunc: xsltExtShutdownFunction; styleInitFunc: xsltStyleExtInitFunction; styleShutdownFunc: xsltStyleExtShutdownFunction): cint; EXTDECL;
  xsltUnregisterExtModule: function(const URI: xmlCharPtr): cint; EXTDECL;
  xsltGetExtData: function(ctxt: xsltTransformContextPtr; const URI: xmlCharPtr): Pointer; EXTDECL;
  xsltStyleGetExtData: function(style: xsltStylesheetPtr; const URI: xmlCharPtr): Pointer; EXTDECL;
{$IFDEF XSLT_REFACTORED}
  xsltStyleStylesheetLevelGetExtData: function(style: xsltStylesheetPtr; const URI: xmlCharPtr): Pointer; EXTDECL;
{$ENDIF}
  xsltShutdownCtxtExts: procedure(ctxt: xsltTransformContextPtr); EXTDECL;
  xsltShutdownExts: procedure(style: xsltStylesheetPtr); EXTDECL;
  xsltXPathGetTransformContext: function(ctxt: xmlXPathParserContextPtr): xsltTransformContextPtr; EXTDECL;

(*
 * extension functions
 *)
  xsltRegisterExtModuleFunction: function(const name: xmlCharPtr; const URI: xmlCharPtr; &function: xmlXPathFunction): cint; EXTDECL;
  xsltExtModuleFunctionLookup: function(const name: xmlCharPtr; const URI: xmlCharPtr): xmlXPathFunction; EXTDECL;
  xsltUnregisterExtModuleFunction: function(const name: xmlCharPtr; const URI: xmlCharPtr): cint; EXTDECL;
{$ENDIF} {FUNCTIONVAR}

{$IFDEF TYPE}
(*
 * extension elements
 *)
  xsltPreComputeFunction = function(style: xsltStylesheetPtr; inst: xmlNodePtr; &function: xsltTransformFunction): xsltElemPreCompPtr; EXTDECL;
{$ENDIF}

{$IFDEF FUNCTION}
function xsltNewElemPreComp(style: xsltStylesheetPtr; inst: xmlNodePtr; &function: xsltTransformFunction): xsltElemPreCompPtr; EXTDECL; external xsltlib;
procedure xsltInitElemPreComp(comp: xsltElemPreCompPtr; style: xsltStylesheetPtr; inst: xmlNodePtr; &function: xsltTransformFunction; freeFunc: xsltElemPreCompDeallocator); EXTDECL; external xsltlib;
function xsltRegisterExtModuleElement(const name: xmlCharPtr; const URI: xmlCharPtr; precomp: xsltPreComputeFunction; transform: xsltTransformFunction): cint; EXTDECL; external xsltlib;
function xsltExtElementLookup(ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const URI: xmlCharPtr): xsltTransformFunction; EXTDECL; external xsltlib;
function xsltExtModuleElementLookup(const name: xmlCharPtr; const URI: xmlCharPtr): xsltTransformFunction; EXTDECL; external xsltlib;
function xsltExtModuleElementPreComputeLookup(const name: xmlCharPtr; const URI: xmlCharPtr): xsltPreComputeFunction; EXTDECL; external xsltlib;
function xsltUnregisterExtModuleElement(const name: xmlCharPtr; const URI: xmlCharPtr): cint; EXTDECL; external xsltlib;
{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
  xsltNewElemPreComp: function(style: xsltStylesheetPtr; inst: xmlNodePtr; &function: xsltTransformFunction): xsltElemPreCompPtr; EXTDECL;
  xsltInitElemPreComp: procedure(comp: xsltElemPreCompPtr; style: xsltStylesheetPtr; inst: xmlNodePtr; &function: xsltTransformFunction; freeFunc: xsltElemPreCompDeallocator); EXTDECL;
  xsltRegisterExtModuleElement: function(const name: xmlCharPtr; const URI: xmlCharPtr; precomp: xsltPreComputeFunction; transform: xsltTransformFunction): cint; EXTDECL;
  xsltExtElementLookup: function(ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const URI: xmlCharPtr): xsltTransformFunction; EXTDECL;
  xsltExtModuleElementLookup: function(const name: xmlCharPtr; const URI: xmlCharPtr): xsltTransformFunction; EXTDECL;
  xsltExtModuleElementPreComputeLookup: function(const name: xmlCharPtr; const URI: xmlCharPtr): xsltPreComputeFunction; EXTDECL;
  xsltUnregisterExtModuleElement: function(const name: xmlCharPtr; const URI: xmlCharPtr): cint; EXTDECL;
{$ENDIF} {FUNCTIONVAR}

{$IFDEF TYPE}
(*
 * top-level elements
 *)
  xsltTopLevelFunction = procedure(style: xsltStylesheetPtr; inst: xmlNodePtr); EXTDECL;
{$ENDIF} {TYPE}

{$IFDEF FUNCTION}
function xsltRegisterExtModuleTopLevel(const name: xmlCharPtr; const URI: xmlCharPtr; &function: xsltTopLevelFunction): cint; EXTDECL; external xsltlib;
function xsltExtModuleTopLevelLookup(const name: xmlCharPtr; const URI: xmlCharPtr): xsltTopLevelFunction; EXTDECL; external xsltlib;
function xsltUnregisterExtModuleTopLevel(const name: xmlCharPtr; const URI: xmlCharPtr): cint; EXTDECL; external xsltlib;

(* These 2 functions are deprecated for use within modules. *)
function xsltRegisterExtFunction(ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const URI: xmlCharPtr; &function: xmlXPathFunction): cint; EXTDECL; external xsltlib;
function xsltRegisterExtElement(ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const URI: xmlCharPtr; &function: xsltTransformFunction): cint; EXTDECL; external xsltlib;

(*
 * Extension Prefix handling API.
 * Those are used by the XSLT (pre)processor.
 *)
function xsltRegisterExtPrefix(style: xsltStylesheetPtr; const prefix: xmlCharPtr; const URI: xmlCharPtr): cint; EXTDECL; external xsltlib;
function xsltCheckExtPrefix(style: xsltStylesheetPtr; const URI: xmlCharPtr): cint; EXTDECL; external xsltlib;
function xsltCheckExtURI(style: xsltStylesheetPtr; const URI: xmlCharPtr): cint; EXTDECL; external xsltlib;
function xsltInitCtxtExts(ctxt: xsltTransformContextPtr): cint; EXTDECL; external xsltlib;
procedure xsltFreeCtxtExts(ctxt: xsltTransformContextPtr); EXTDECL; external xsltlib;
procedure xsltFreeExts(style: xsltStylesheetPtr); EXTDECL; external xsltlib;
function xsltPreComputeExtModuleElement(style: xsltStylesheetPtr; inst: xmlNodePtr): xsltElemPreCompPtr; EXTDECL; external xsltlib;

(*
 * Extension Infos access.
 * Used by exslt initialisation
 *)
function xsltGetExtInfo(style: xsltStylesheetPtr; const URI: xmlCharPtr): xmlHashTablePtr; EXTDECL; external xsltlib;

(**
 * Test of the extension module API
 *)
procedure xsltRegisterTestModule; EXTDECL; external xsltlib;
procedure xsltDebugDumpExtensions(output: Pointer {FILE * }); EXTDECL; external xsltlib;
{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
  xsltRegisterExtModuleTopLevel: function(const name: xmlCharPtr; const URI: xmlCharPtr; &function: xsltTopLevelFunction): cint; EXTDECL;
  xsltExtModuleTopLevelLookup: function(const name: xmlCharPtr; const URI: xmlCharPtr): xsltTopLevelFunction; EXTDECL;
  xsltUnregisterExtModuleTopLevel: function(const name: xmlCharPtr; const URI: xmlCharPtr): cint; EXTDECL;

(* These 2 functions are deprecated for use within modules. *)
  xsltRegisterExtFunction: function(ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const URI: xmlCharPtr; &function: xmlXPathFunction): cint; EXTDECL;
  xsltRegisterExtElement: function(ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const URI: xmlCharPtr; &function: xsltTransformFunction): cint; EXTDECL;

(*
 * Extension Prefix handling API.
 * Those are used by the XSLT (pre)processor.
 *)
  xsltRegisterExtPrefix: function(style: xsltStylesheetPtr; const prefix: xmlCharPtr; const URI: xmlCharPtr): cint; EXTDECL;
  xsltCheckExtPrefix: function(style: xsltStylesheetPtr; const URI: xmlCharPtr): cint; EXTDECL;
  xsltCheckExtURI: function(style: xsltStylesheetPtr; const URI: xmlCharPtr): cint; EXTDECL;
  xsltInitCtxtExts: function(ctxt: xsltTransformContextPtr): cint; EXTDECL;
  xsltFreeCtxtExts: procedure(ctxt: xsltTransformContextPtr); EXTDECL;
  xsltFreeExts: procedure(style: xsltStylesheetPtr); EXTDECL;
  xsltPreComputeExtModuleElement: function(style: xsltStylesheetPtr; inst: xmlNodePtr): xsltElemPreCompPtr; EXTDECL;

(*
 * Extension Infos access.
 * Used by exslt initialisation
 *)
  xsltGetExtInfo: function(style: xsltStylesheetPtr; const URI: xmlCharPtr): xmlHashTablePtr; EXTDECL;

(**
 * Test of the extension module API
 *)
  xsltRegisterTestModule: procedure; EXTDECL;
  xsltDebugDumpExtensions: procedure(output: Pointer {FILE * }); EXTDECL;
{$ENDIF} {FUNCTIONVAR}

