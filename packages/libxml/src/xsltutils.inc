(*
 * Summary: set of utilities for the XSLT engine
 * Description: interfaces for the utilities module of the XSLT engine.
 *              things like message handling, profiling, and other
 *              generally useful routines.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{
/**
 * XSLT_TODO:
 *
 * Macro to flag unimplemented blocks.
 */
#define XSLT_TODO							\
    xsltGenericError(xsltGenericErrorContext,				\
	    "Unimplemented block at %s:%d\n",				\
            __FILE__, __LINE__);

/**
 * XSLT_STRANGE:
 *
 * Macro to flag that a problem was detected internally.
 */
#define XSLT_STRANGE							\
    xsltGenericError(xsltGenericErrorContext,				\
	    "Internal error at %s:%d\n",				\
            __FILE__, __LINE__);

/**
 * IS_XSLT_ELEM:
 *
 * Checks that the element pertains to XSLT namespace.
 */
#define IS_XSLT_ELEM(n)							\
    (((n) != NULL) && ((n)->type == XML_ELEMENT_NODE) &&                \
     ((n)->ns != NULL) && (xmlStrEqual((n)->ns->href, XSLT_NAMESPACE)))

/**
 * IS_XSLT_NAME:
 *
 * Checks the value of an element in XSLT namespace.
 */
#define IS_XSLT_NAME(n, val)						\
    (xmlStrEqual((n)->name, (const xmlChar *) (val)))

/**
 * IS_XSLT_REAL_NODE:
 *
 * Check that a node is a 'real' one: document, element, text or attribute.
 */
#define IS_XSLT_REAL_NODE(n)						\
    (((n) != NULL) &&							\
     (((n)->type == XML_ELEMENT_NODE) ||				\
      ((n)->type == XML_TEXT_NODE) ||					\
      ((n)->type == XML_CDATA_SECTION_NODE) ||				\
      ((n)->type == XML_ATTRIBUTE_NODE) ||				\
      ((n)->type == XML_DOCUMENT_NODE) ||				\
      ((n)->type == XML_HTML_DOCUMENT_NODE) ||				\
      ((n)->type == XML_COMMENT_NODE) ||				\
      ((n)->type == XML_PI_NODE)))
}

{$IFDEF FUNCTION}

(*
 * Our own version of namespaced attributes lookup.
 *)
function xsltGetNsProp(node: xmlNodePtr; const name: xmlCharPtr; const nameSpace: xmlCharPtr): xmlCharPtr; EXTDECL; external xsltlib;
function xsltGetCNsProp(style: xsltStylesheetPtr; node: xmlNodePtr; const name: xmlCharPtr; const nameSpace: xmlCharPtr): xmlCharPtr; EXTDECL; external xsltlib;
function xsltGetUTF8Char(const utf: PAnsiChar; len: pcint): cint; EXTDECL; external xsltlib;
{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}

(*
 * Our own version of namespaced attributes lookup.
 *)
  xsltGetNsProp: function(node: xmlNodePtr; const name: xmlCharPtr; const nameSpace: xmlCharPtr): xmlCharPtr; EXTDECL;
  xsltGetCNsProp: function(style: xsltStylesheetPtr; node: xmlNodePtr; const name: xmlCharPtr; const nameSpace: xmlCharPtr): xmlCharPtr; EXTDECL;
  xsltGetUTF8Char: function(const utf: PAnsiChar; len: pcint): cint; EXTDECL;
{$ENDIF} {FUNCTIONVAR}

{$IFDEF TYPE}
  (*
   * XSLT Debug Tracing Tracing Types
   *)
  xsltDebugTraceCodes = (
    XSLT_TRACE_ALL =            -1,
    XSLT_TRACE_NONE =           0,
    XSLT_TRACE_COPY_TEXT =      1 shl 0,
    XSLT_TRACE_PROCESS_NODE =   1 shl 1,
    XSLT_TRACE_APPLY_TEMPLATE = 1 shl 2,
    XSLT_TRACE_COPY =           1 shl 3,
    XSLT_TRACE_COMMENT =        1 shl 4,
    XSLT_TRACE_PI =             1 shl 5,
    XSLT_TRACE_COPY_OF =        1 shl 6,
    XSLT_TRACE_VALUE_OF =       1 shl 7,
    XSLT_TRACE_CALL_TEMPLATE =  1 shl 8,
    XSLT_TRACE_APPLY_TEMPLATES =1 shl 9,
    XSLT_TRACE_CHOOSE =         1 shl 10,
    XSLT_TRACE_IF =             1 shl 11,
    XSLT_TRACE_FOR_EACH =       1 shl 12,
    XSLT_TRACE_STRIP_SPACES =   1 shl 13,
    XSLT_TRACE_TEMPLATES =      1 shl 14,
    XSLT_TRACE_KEYS =           1 shl 15,
    XSLT_TRACE_VARIABLES =      1 shl 16
  );
{$ENDIF} {TYPE}

(**
 * XSLT_TRACE:
 *
 * Control the type of xsl debugtrace messages emitted.
 *)
{
#define XSLT_TRACE(ctxt,code,call)	\
	if (ctxt->traceCode && (*(ctxt->traceCode) & code)) \
	    call
}

{$IFDEF FUNCTION}
procedure xsltDebugSetDefaultTrace(val: xsltDebugTraceCodes); EXTDECL; external xsltlib;
function xsltDebugGetDefaultTrace: xsltDebugTraceCodes; EXTDECL; external xsltlib;
{$ENDIF}

{$IFDEF FUNCTIONVAR}
  xsltDebugSetDefaultTrace: procedure(val: xsltDebugTraceCodes); EXTDECL;
  xsltDebugGetDefaultTrace: function: xsltDebugTraceCodes; EXTDECL;
{$ENDIF}

{$IFDEF EXTVAR}
(*
 * XSLT specific error and debug reporting functions.
 *)
var
  xsltGenericError: xmlGenericErrorFunc; external xsltlib;
  xsltGenericErrorContext: Pointer; external xsltlib;
  xsltGenericDebug: xmlGenericErrorFunc; external xsltlib;
  xsltGenericDebugContext: Pointer; external xsltlib;
{$ENDIF}

{$IFDEF FUNCTION}
procedure xsltPrintErrorContext(ctxt: xsltTransformContextPtr; style: xsltStylesheetPtr; node: xmlNodePtr); EXTDECL; external xsltlib;
procedure xsltMessage(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr); EXTDECL; external xsltlib;
procedure xsltSetGenericErrorFunc(ctx: Pointer; handler: xmlGenericErrorFunc); EXTDECL; external xsltlib;
procedure xsltSetGenericDebugFunc(ctx: Pointer; handler: xmlGenericErrorFunc); EXTDECL; external xsltlib;
procedure xsltSetTransformErrorFunc(ctxt: xsltTransformContextPtr;  ctx: Pointer; handler: xmlGenericErrorFunc); EXTDECL; external xsltlib;
procedure xsltTransformError(ctxt: xsltTransformContextPtr; style: xsltStylesheetPtr; node: xmlNodePtr; const msg: PAnsiChar; params: array of const); EXTDECL; external xsltlib;
function xsltSetCtxtParseOptions(ctxt: xsltTransformContextPtr; options: cint): cint; EXTDECL; external xsltlib;

(*
 * Sorting.
 *)

procedure xsltDocumentSortFunction(list: xmlNodeSetPtr); EXTDECL; external xsltlib;
procedure xsltSetSortFunc(handler: xsltSortFunc); EXTDECL; external xsltlib;
procedure xsltSetCtxtSortFunc(ctxt: xsltTransformContextPtr; handler: xsltSortFunc); EXTDECL; external xsltlib;
procedure xsltDefaultSortFunction(ctxt: xsltTransformContextPtr; sorts: xmlNodePtrPtr; nbsorts: cint); EXTDECL; external xsltlib;
procedure xsltDoSortFunction(ctxt: xsltTransformContextPtr; sorts: xmlNodePtrPtr; nbsorts: cint); EXTDECL; external xsltlib;
function xsltComputeSortResult(ctxt: xsltTransformContextPtr; sort: xmlNodePtr): xmlXPathObjectPtrPtr; EXTDECL; external xsltlib;

(*
 * QNames handling.
 *)

function xsltSplitQName(dict: xmlDictPtr; const name: xmlCharPtr; const prefix: xmlCharPtrPtr): xmlCharPtr; EXTDECL; external xsltlib;
function xsltGetQNameURI(node: xmlNodePtr; name: xmlCharPtrPtr): xmlCharPtr; EXTDECL; external xsltlib;
function xsltGetQNameURI2(style: xsltStylesheetPtr; node: xmlNodePtr; const name: xmlCharPtrPtr): xmlCharPtr; EXTDECL; external xsltlib;

(*
 * Output, reuse libxml I/O buffers.
 *)
function xsltSaveResultTo(buf: xmlOutputBufferPtr; result: xmlDocPtr; style: xsltStylesheetPtr): cint; EXTDECL; external xsltlib;
function xsltSaveResultToFilename(const URI: PAnsiChar; result: xmlDocPtr; style: xsltStylesheetPtr; compression: cint): cint; EXTDECL; external xsltlib;
function xsltSaveResultToFile(&file: Pointer; result: xmlDocPtr; style: xsltStylesheetPtr): cint; EXTDECL; external xsltlib;
function xsltSaveResultToFd(fd: cint; result: xmlDocPtr; style: xsltStylesheetPtr): cint; EXTDECL; external xsltlib;
function xsltSaveResultToString(doc_txt_ptr: xmlCharPtrPtr; doc_txt_len: pcint; result: xmlDocPtr; style: xsltStylesheetPtr): cint; EXTDECL; external xsltlib;

(*
 * XPath interface
 *)
function xsltXPathCompile(style: xsltStylesheetPtr; const str: xmlCharPtr): xmlXPathCompExprPtr; EXTDECL; external xsltlib;
function xsltXPathCompileFlags(style: xsltStylesheetPtr; const str: xmlCharPtr; flags: cint): xmlXPathCompExprPtr; EXTDECL; external xsltlib;

(*
 * Profiling.
 *)
procedure xsltSaveProfiling(ctxt: xsltTransformContextPtr; output: Pointer); EXTDECL; external xsltlib;
function xsltGetProfileInformation(ctxt: xsltTransformContextPtr): xmlDocPtr; EXTDECL; external xsltlib;
function xsltTimestamp: clong; EXTDECL; external xsltlib;
procedure xsltCalibrateAdjust(delta: clong); EXTDECL; external xsltlib;

{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
  xsltPrintErrorContext: procedure(ctxt: xsltTransformContextPtr; style: xsltStylesheetPtr; node: xmlNodePtr); EXTDECL;
  xsltMessage: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr); EXTDECL;
  xsltSetGenericErrorFunc: procedure(ctx: Pointer; handler: xmlGenericErrorFunc); EXTDECL;
  xsltSetGenericDebugFunc: procedure(ctx: Pointer; handler: xmlGenericErrorFunc); EXTDECL;
  xsltSetTransformErrorFunc: procedure(ctxt: xsltTransformContextPtr;  ctx: Pointer; handler: xmlGenericErrorFunc); EXTDECL;
  xsltTransformError: procedure(ctxt: xsltTransformContextPtr; style: xsltStylesheetPtr; node: xmlNodePtr; const msg: PAnsiChar; params: array of const); EXTDECL;
  xsltSetCtxtParseOptions: function(ctxt: xsltTransformContextPtr; options: cint): cint; EXTDECL;

(*
 * Sorting.
 *)

  xsltDocumentSortFunction: procedure(list: xmlNodeSetPtr); EXTDECL;
  xsltSetSortFunc: procedure(handler: xsltSortFunc); EXTDECL;
  xsltSetCtxtSortFunc: procedure(ctxt: xsltTransformContextPtr; handler: xsltSortFunc); EXTDECL;
  xsltDefaultSortFunction: procedure(ctxt: xsltTransformContextPtr; sorts: xmlNodePtrPtr; nbsorts: cint); EXTDECL;
  xsltDoSortFunction: procedure(ctxt: xsltTransformContextPtr; sorts: xmlNodePtrPtr; nbsorts: cint); EXTDECL;
  xsltComputeSortResult: function(ctxt: xsltTransformContextPtr; sort: xmlNodePtr): xmlXPathObjectPtrPtr; EXTDECL;

(*
 * QNames handling.
 *)

  xsltSplitQName: function(dict: xmlDictPtr; const name: xmlCharPtr; const prefix: xmlCharPtrPtr): xmlCharPtr; EXTDECL;
  xsltGetQNameURI: function(node: xmlNodePtr; name: xmlCharPtrPtr): xmlCharPtr; EXTDECL;
  xsltGetQNameURI2: function(style: xsltStylesheetPtr; node: xmlNodePtr; const name: xmlCharPtrPtr): xmlCharPtr; EXTDECL;

(*
 * Output, reuse libxml I/O buffers.
 *)
  xsltSaveResultTo: function(buf: xmlOutputBufferPtr; result: xmlDocPtr; style: xsltStylesheetPtr): cint; EXTDECL;
  xsltSaveResultToFilename: function(const URI: PAnsiChar; result: xmlDocPtr; style: xsltStylesheetPtr; compression: cint): cint; EXTDECL;
  xsltSaveResultToFile: function(&file: Pointer; result: xmlDocPtr; style: xsltStylesheetPtr): cint; EXTDECL;
  xsltSaveResultToFd: function(fd: cint; result: xmlDocPtr; style: xsltStylesheetPtr): cint; EXTDECL;
  xsltSaveResultToString: function(doc_txt_ptr: xmlCharPtrPtr; doc_txt_len: pcint; result: xmlDocPtr; style: xsltStylesheetPtr): cint; EXTDECL;

(*
 * XPath interface
 *)
  xsltXPathCompile: function(style: xsltStylesheetPtr; const str: xmlCharPtr): xmlXPathCompExprPtr; EXTDECL;
  xsltXPathCompileFlags: function(style: xsltStylesheetPtr; const str: xmlCharPtr; flags: cint): xmlXPathCompExprPtr; EXTDECL;

(*
 * Profiling.
 *)
  xsltSaveProfiling: procedure(ctxt: xsltTransformContextPtr; output: Pointer); EXTDECL;
  xsltGetProfileInformation: function(ctxt: xsltTransformContextPtr): xmlDocPtr; EXTDECL;
  xsltTimestamp: function: clong; EXTDECL;
  xsltCalibrateAdjust: procedure(delta: clong); EXTDECL;

{$ENDIF} {FUNCTIONVAR}

{$IFDEF CONST}
  (**
   * XSLT_TIMESTAMP_TICS_PER_SEC:
   *
   * Sampling precision for profiling
   *)
  XSLT_TIMESTAMP_TICS_PER_SEC = clong(100000);
{$ENDIF} {CONST}

{$IFDEF TYPE}
(*
 * Hooks for the debugger.
 *)

  xsltDebugStatusCodes = (
    XSLT_DEBUG_NONE = 0, (* no debugging allowed *)
    XSLT_DEBUG_INIT,
    XSLT_DEBUG_STEP,
    XSLT_DEBUG_STEPOUT,
    XSLT_DEBUG_NEXT,
    XSLT_DEBUG_STOP,
    XSLT_DEBUG_CONT,
    XSLT_DEBUG_RUN,
    XSLT_DEBUG_RUN_RESTART,
    XSLT_DEBUG_QUIT
  );
{$ENDIF} {TYPE}

{$IFDEF EXTVAR}
var
  xslDebugStatus: cint; external xsltlib;
{$ENDIF} {EXTVAR}

{$IFDEF TYPE}
  xsltHandleDebuggerCallback = procedure(cur: xmlNodePtr; node: xmlNodePtr;
    templ: xsltTemplatePtr; ctxt: xsltTransformContextPtr); EXTDECL;
  xsltAddCallCallback = function(templ: xsltTemplatePtr; source: xmlNodePtr): cint; EXTDECL;
  xsltDropCallCallback = procedure; EXTDECL;
{$ENDIF} {TYPE}

{$IFDEF FUNCTION}
procedure xsltSetDebuggerStatus(value: cint); EXTDECL; external xsltlib;
function xsltGetDebuggerStatus: cint; EXTDECL; external xsltlib;
function xsltSetDebuggerCallbacks(no: cint; block: Pointer): cint; EXTDECL; external xsltlib;
function xslAddCall(templ: xsltTemplatePtr; source: xmlNodePtr): cint; EXTDECL; external xsltlib;
procedure xslDropCall; EXTDECL; external xsltlib;
{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
  xsltSetDebuggerStatus: procedure(value: cint); EXTDECL;
  xsltGetDebuggerStatus: function: cint; EXTDECL;
  xsltSetDebuggerCallbacks: function(no: cint; block: Pointer): cint; EXTDECL;
  xslAddCall: function(templ: xsltTemplatePtr; source: xmlNodePtr): cint; EXTDECL;
  xslDropCall: procedure; EXTDECL;
{$ENDIF} {FUNCTIONVAR}

