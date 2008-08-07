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
{$IFDEF TYPE}
(**
 * xmlRegexpPtr:
 *
 * A libxml regular expression, they can actually be far more complex
 * thank the POSIX regex expressions.
 *)
  xmlRegexp = record
  end;

(**
 * xmlRegExecCtxtPtr:
 *
 * A libxml progressive regular expression evaluation context
 *)
  xmlRegExecCtxt = record
  end;

(*
 * Callback function when doing a transition in the automata
 *)
  xmlRegExecCallbacks = procedure(exec: xmlRegExecCtxtPtr; token: xmlCharPtr; transdata, inputdata: pointer); cdecl;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * The POSIX like API
 *)
function xmlRegexpCompile(regexp: xmlCharPtr): xmlRegexpPtr; cdecl; external;
procedure xmlRegFreeRegexp(regexp: xmlRegexpPtr); cdecl; external;
function xmlRegexpExec(comp: xmlRegexpPtr; value: xmlCharPtr): cint; cdecl; external;
//procedure xmlRegexpPrint(output: FILE; regexp: xmlRegexpPtr); cdecl; external;
function xmlRegexpIsDeterminist(comp: xmlRegexpPtr): cint; cdecl; external;

(*
 * The progressive API
 *)
function xmlRegNewExecCtxt(comp: xmlRegexpPtr; callback: xmlRegExecCallbacks; data: pointer): xmlRegExecCtxtPtr; cdecl; external;
procedure xmlRegFreeExecCtxt(exec: xmlRegExecCtxtPtr); cdecl; external;
function xmlRegExecPushString(exec: xmlRegExecCtxtPtr; value: xmlCharPtr; data: pointer): cint; cdecl; external;
function xmlRegExecPushString2(exec: xmlRegExecCtxtPtr; value, value2: xmlCharPtr; data: pointer): cint; cdecl; external;
function xmlRegExecNextValues(exec: xmlRegExecCtxtPtr; nbval, nbneg: pcint; values: xmlCharPtrPtr; terminal: pcint): cint; cdecl; external;
function xmlRegExecErrInfo(exec: xmlRegExecCtxtPtr; _string: xmlCharPtrPtr; nbval, nbneg: pcint; values: xmlCharPtrPtr; terminal: pcint): cint; cdecl; external;
{$ENDIF}

{$IFDEF LIBXML_EXPR_ENABLED}
(*
 * Formal regular expression handling
 * Its goal is to do some formal work on content models
 *)

{$IFDEF TYPE}
(* expressions are used within a context *)
  xmlExpCtxt = record
  end;

(* Expressions are trees but the tree is opaque *)
  xmlExpNodePtr = ^xmlExpNode;
  xmlExpNode = record
  end;

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
procedure xmlRegFreeExecCtxt(ctxt: xmlExpCtxtPtr); cdecl; external;
function xmlExpNewCtxt(maxNodes: cint; dict: xmlDictPtr): xmlExpCtxtPtr; cdecl; external;
function xmlExpCtxtNbNodes(ctxt: xmlExpCtxtPtr): cint; cdecl; external;
function xmlExpCtxtNbCons(ctxt: xmlExpCtxtPtr): cint; cdecl; external;

(*
 * 2 core expressions shared by all for the empty language set
 * and for the set with just the empty token
 *)
var
  forbiddenExp: xmlExpNodePtr; cvar; external;
  emptyExp: xmlExpNodePtr; cvar; external;

(*
 * Expressions are reference counted internally
 *)
procedure xmlExpFree(ctxt: xmlExpCtxtPtr; expr: xmlExpNodePtr); cdecl; external;
procedure xmlExpRef(expr: xmlExpNodePtr); cdecl; external;

(*
 * constructors can be either manual or from a string
 *)
function xmlExpParse(ctxt: xmlExpCtxtPtr; expr: pchar): xmlExpNodePtr; cdecl; external;
function xmlExpNewAtom(ctxt: xmlExpCtxtPtr; name: xmlCharPtr; len: cint): xmlExpNodePtr; cdecl; external;
function xmlExpNewOr(ctxt: xmlExpCtxtPtr; left, right: xmlExpNodePtr): xmlExpNodePtr; cdecl; external;
function xmlExpNewSeq(ctxt: xmlExpCtxtPtr; left, right: xmlExpNodePtr): xmlExpNodePtr; cdecl; external;
function xmlExpNewRange(ctxt: xmlExpCtxtPtr; subset: xmlExpNodePtr; min, max: cint): xmlExpNodePtr; cdecl; external;

(*
 * The really interesting APIs
 *)
function xmlExpIsNillable(expr: xmlExpNodePtr): cint; cdecl; external;
function xmlExpMaxToken(expr: xmlExpNodePtr): cint; cdecl; external;
function xmlExpGetLanguage(ctxt: xmlExpCtxtPtr; expr: xmlExpNodePtr; langList: xmlCharPtrPtr; len: cint): cint; cdecl; external;
function xmlExpGetStart(ctxt: xmlExpCtxtPtr; expr: xmlExpNodePtr; tokList: xmlCharPtrPtr; len: cint): cint; cdecl; external;
function xmlExpStringDerive(ctxt: xmlExpCtxtPtr; expr: xmlExpNodePtr; str: xmlCharPtr; len: cint): xmlExpNodePtr; cdecl; external;
function xmlExpExpDerive(ctxt: xmlExpCtxtPtr; expr, sub: xmlExpNodePtr): xmlExpNodePtr; cdecl; external;
function xmlExpSubsume(ctxt: xmlExpCtxtPtr; expr, sub: xmlExpNodePtr): cint; cdecl; external;
procedure xmlExpDump(buf: xmlBufferPtr; expr: xmlExpNodePtr); cdecl; external;
{$ENDIF}
{$ENDIF} (* LIBXML_EXPR_ENABLED *)
{$ENDIF} (* LIBXML_REGEXP_ENABLED *)
