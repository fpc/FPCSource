(*
 * Summary: regular expressions handling
 * Description: basic API for libxml regular expressions handling used
 *              for XML Schemas and validation.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_REGEXP_ENABLED}

{$IFDEF POINTER}
  xmlExpNodePtr = ^xmlExpNode;
  xmlRegexpPtr = ^xmlRegexp;
  xmlRegExecCtxtPtr = ^xmlRegExecCtxt;
  xmlExpCtxtPtr = ^xmlExpCtxt;
{$ENDIF}


{$IFDEF TYPE}
(**
 * xmlRegexpPtr:
 *
 * A libxml regular expression, they can actually be far more complex
 * thank the POSIX regex expressions.
 *)
  xmlRegexp = record end;

(**
 * xmlRegExecCtxtPtr:
 *
 * A libxml progressive regular expression evaluation context
 *)
  xmlRegExecCtxt = record end;

(*
 * Callback function when doing a transition in the automata
 *)
  xmlRegExecCallbacks = procedure(exec: xmlRegExecCtxtPtr; token: xmlCharPtr; transdata, inputdata: pointer); EXTDECL;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * The POSIX like API
 *)
function xmlRegexpCompile(regexp: xmlCharPtr): xmlRegexpPtr; EXTDECL; external xml2lib;
procedure xmlRegFreeRegexp(regexp: xmlRegexpPtr); EXTDECL; external xml2lib;
function xmlRegexpExec(comp: xmlRegexpPtr; value: xmlCharPtr): cint; EXTDECL; external xml2lib;
procedure xmlRegexpPrint(output: PFILE; regexp: xmlRegexpPtr); EXTDECL; external xml2lib;
function xmlRegexpIsDeterminist(comp: xmlRegexpPtr): cint; EXTDECL; external xml2lib;

(*
 * The progressive API
 *)
function xmlRegNewExecCtxt(comp: xmlRegexpPtr; callback: xmlRegExecCallbacks; data: pointer): xmlRegExecCtxtPtr; EXTDECL; external xml2lib;
procedure xmlRegFreeExecCtxt(exec: xmlRegExecCtxtPtr); EXTDECL; external xml2lib;
function xmlRegExecPushString(exec: xmlRegExecCtxtPtr; value: xmlCharPtr; data: pointer): cint; EXTDECL; external xml2lib;
function xmlRegExecPushString2(exec: xmlRegExecCtxtPtr; value, value2: xmlCharPtr; data: pointer): cint; EXTDECL; external xml2lib;
function xmlRegExecNextValues(exec: xmlRegExecCtxtPtr; nbval, nbneg: pcint; values: xmlCharPtrPtr; terminal: pcint): cint; EXTDECL; external xml2lib;
function xmlRegExecErrInfo(exec: xmlRegExecCtxtPtr; _string: xmlCharPtrPtr; nbval, nbneg: pcint; values: xmlCharPtrPtr; terminal: pcint): cint; EXTDECL; external xml2lib;
{$ENDIF}

{$IFDEF LIBXML_EXPR_ENABLED}
(*
 * Formal regular expression handling
 * Its goal is to do some formal work on content models
 *)

{$IFDEF TYPE}
(* expressions are used within a context *)
  xmlExpCtxt = record end;

(* Expressions are trees but the tree is opaque *)
  xmlExpNode = record end;

  xmlExpNodeType = (
    XML_EXP_EMPTY = 0,
    XML_EXP_FORBID = 1,
    XML_EXP_ATOM = 2,
    XML_EXP_SEQ = 3,
    XML_EXP_OR = 4,
    XML_EXP_COUNT = 5
  );
{$ENDIF}

{$IFDEF FUNCTION}
procedure xmlRegFreeExecCtxt(ctxt: xmlExpCtxtPtr); EXTDECL; external xml2lib;
function xmlExpNewCtxt(maxNodes: cint; dict: xmlDictPtr): xmlExpCtxtPtr; EXTDECL; external xml2lib;
function xmlExpCtxtNbNodes(ctxt: xmlExpCtxtPtr): cint; EXTDECL; external xml2lib;
function xmlExpCtxtNbCons(ctxt: xmlExpCtxtPtr): cint; EXTDECL; external xml2lib;

(*
 * 2 core expressions shared by all for the empty language set
 * and for the set with just the empty token
 *)
{$IFNDEF NO_EXTERNAL_VARS}
var
  forbiddenExp: xmlExpNodePtr; cvar; external;
  emptyExp: xmlExpNodePtr; cvar; external;
{$ENDIF}

(*
 * Expressions are reference counted internally
 *)
procedure xmlExpFree(ctxt: xmlExpCtxtPtr; expr: xmlExpNodePtr); EXTDECL; external xml2lib;
procedure xmlExpRef(expr: xmlExpNodePtr); EXTDECL; external xml2lib;

(*
 * constructors can be either manual or from a string
 *)
function xmlExpParse(ctxt: xmlExpCtxtPtr; expr: pchar): xmlExpNodePtr; EXTDECL; external xml2lib;
function xmlExpNewAtom(ctxt: xmlExpCtxtPtr; name: xmlCharPtr; len: cint): xmlExpNodePtr; EXTDECL; external xml2lib;
function xmlExpNewOr(ctxt: xmlExpCtxtPtr; left, right: xmlExpNodePtr): xmlExpNodePtr; EXTDECL; external xml2lib;
function xmlExpNewSeq(ctxt: xmlExpCtxtPtr; left, right: xmlExpNodePtr): xmlExpNodePtr; EXTDECL; external xml2lib;
function xmlExpNewRange(ctxt: xmlExpCtxtPtr; subset: xmlExpNodePtr; min, max: cint): xmlExpNodePtr; EXTDECL; external xml2lib;

(*
 * The really interesting APIs
 *)
function xmlExpIsNillable(expr: xmlExpNodePtr): cint; EXTDECL; external xml2lib;
function xmlExpMaxToken(expr: xmlExpNodePtr): cint; EXTDECL; external xml2lib;
function xmlExpGetLanguage(ctxt: xmlExpCtxtPtr; expr: xmlExpNodePtr; langList: xmlCharPtrPtr; len: cint): cint; EXTDECL; external xml2lib;
function xmlExpGetStart(ctxt: xmlExpCtxtPtr; expr: xmlExpNodePtr; tokList: xmlCharPtrPtr; len: cint): cint; EXTDECL; external xml2lib;
function xmlExpStringDerive(ctxt: xmlExpCtxtPtr; expr: xmlExpNodePtr; str: xmlCharPtr; len: cint): xmlExpNodePtr; EXTDECL; external xml2lib;
function xmlExpExpDerive(ctxt: xmlExpCtxtPtr; expr, sub: xmlExpNodePtr): xmlExpNodePtr; EXTDECL; external xml2lib;
function xmlExpSubsume(ctxt: xmlExpCtxtPtr; expr, sub: xmlExpNodePtr): cint; EXTDECL; external xml2lib;
procedure xmlExpDump(buf: xmlBufferPtr; expr: xmlExpNodePtr); EXTDECL; external xml2lib;
{$ENDIF}
{$ENDIF} (* LIBXML_EXPR_ENABLED *)
{$ENDIF} (* LIBXML_REGEXP_ENABLED *)
