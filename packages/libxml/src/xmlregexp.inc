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
  xmlRegExecCallbacks = procedure(exec: xmlRegExecCtxtPtr; token: xmlCharPtr; transdata, inputdata: pointer); XMLCALL;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * The POSIX like API
 *)
function xmlRegexpCompile(regexp: xmlCharPtr): xmlRegexpPtr; XMLCALL; XMLPUBFUN;
procedure xmlRegFreeRegexp(regexp: xmlRegexpPtr); XMLCALL; XMLPUBFUN;
function xmlRegexpExec(comp: xmlRegexpPtr; value: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
procedure xmlRegexpPrint(output: PFILE; regexp: xmlRegexpPtr); XMLCALL; XMLPUBFUN;
function xmlRegexpIsDeterminist(comp: xmlRegexpPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * The progressive API
 *)
function xmlRegNewExecCtxt(comp: xmlRegexpPtr; callback: xmlRegExecCallbacks; data: pointer): xmlRegExecCtxtPtr; XMLCALL; XMLPUBFUN;
procedure xmlRegFreeExecCtxt(exec: xmlRegExecCtxtPtr); XMLCALL; XMLPUBFUN;
function xmlRegExecPushString(exec: xmlRegExecCtxtPtr; value: xmlCharPtr; data: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlRegExecPushString2(exec: xmlRegExecCtxtPtr; value, value2: xmlCharPtr; data: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlRegExecNextValues(exec: xmlRegExecCtxtPtr; nbval, nbneg: pcint; values: xmlCharPtrPtr; terminal: pcint): cint; XMLCALL; XMLPUBFUN;
function xmlRegExecErrInfo(exec: xmlRegExecCtxtPtr; _string: xmlCharPtrPtr; nbval, nbneg: pcint; values: xmlCharPtrPtr; terminal: pcint): cint; XMLCALL; XMLPUBFUN;
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
procedure xmlRegFreeExecCtxt(ctxt: xmlExpCtxtPtr); XMLCALL; XMLPUBFUN;
function xmlExpNewCtxt(maxNodes: cint; dict: xmlDictPtr): xmlExpCtxtPtr; XMLCALL; XMLPUBFUN;
function xmlExpCtxtNbNodes(ctxt: xmlExpCtxtPtr): cint; XMLCALL; XMLPUBFUN;
function xmlExpCtxtNbCons(ctxt: xmlExpCtxtPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * 2 core expressions shared by all for the empty language set
 * and for the set with just the empty token
 *)
var
  forbiddenExp: xmlExpNodePtr; XMLPUBVAR;
  emptyExp: xmlExpNodePtr; XMLPUBVAR;

(*
 * Expressions are reference counted internally
 *)
procedure xmlExpFree(ctxt: xmlExpCtxtPtr; expr: xmlExpNodePtr); XMLCALL; XMLPUBFUN;
procedure xmlExpRef(expr: xmlExpNodePtr); XMLCALL; XMLPUBFUN;

(*
 * constructors can be either manual or from a string
 *)
function xmlExpParse(ctxt: xmlExpCtxtPtr; expr: pchar): xmlExpNodePtr; XMLCALL; XMLPUBFUN;
function xmlExpNewAtom(ctxt: xmlExpCtxtPtr; name: xmlCharPtr; len: cint): xmlExpNodePtr; XMLCALL; XMLPUBFUN;
function xmlExpNewOr(ctxt: xmlExpCtxtPtr; left, right: xmlExpNodePtr): xmlExpNodePtr; XMLCALL; XMLPUBFUN;
function xmlExpNewSeq(ctxt: xmlExpCtxtPtr; left, right: xmlExpNodePtr): xmlExpNodePtr; XMLCALL; XMLPUBFUN;
function xmlExpNewRange(ctxt: xmlExpCtxtPtr; subset: xmlExpNodePtr; min, max: cint): xmlExpNodePtr; XMLCALL; XMLPUBFUN;

(*
 * The really interesting APIs
 *)
function xmlExpIsNillable(expr: xmlExpNodePtr): cint; XMLCALL; XMLPUBFUN;
function xmlExpMaxToken(expr: xmlExpNodePtr): cint; XMLCALL; XMLPUBFUN;
function xmlExpGetLanguage(ctxt: xmlExpCtxtPtr; expr: xmlExpNodePtr; langList: xmlCharPtrPtr; len: cint): cint; XMLCALL; XMLPUBFUN;
function xmlExpGetStart(ctxt: xmlExpCtxtPtr; expr: xmlExpNodePtr; tokList: xmlCharPtrPtr; len: cint): cint; XMLCALL; XMLPUBFUN;
function xmlExpStringDerive(ctxt: xmlExpCtxtPtr; expr: xmlExpNodePtr; str: xmlCharPtr; len: cint): xmlExpNodePtr; XMLCALL; XMLPUBFUN;
function xmlExpExpDerive(ctxt: xmlExpCtxtPtr; expr, sub: xmlExpNodePtr): xmlExpNodePtr; XMLCALL; XMLPUBFUN;
function xmlExpSubsume(ctxt: xmlExpCtxtPtr; expr, sub: xmlExpNodePtr): cint; XMLCALL; XMLPUBFUN;
procedure xmlExpDump(buf: xmlBufferPtr; expr: xmlExpNodePtr); XMLCALL; XMLPUBFUN;
{$ENDIF}
{$ENDIF} (* LIBXML_EXPR_ENABLED *)
{$ENDIF} (* LIBXML_REGEXP_ENABLED *)
