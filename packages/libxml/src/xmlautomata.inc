(*
 * Summary: API to build regexp automata
 * Description: the API to build regexp automata
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_REGEXP_ENABLED}
{$IFDEF LIBXML_AUTOMATA_ENABLED}

{$IFDEF TYPE}
(**
 * xmlAutomataPtr:
 *
 * A libxml automata description, It can be compiled into a regexp
 *)
  xmlAutomata = record end;

(**
 * xmlAutomataStatePtr:
 *
 * A state int the automata description,
 *)
  xmlAutomataState = record end;

{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Building API
 *)
function xmlNewAutomata: xmlAutomataPtr; XMLCALL; XMLPUBFUN;
procedure xmlFreeAutomata(am: xmlAutomataPtr); XMLCALL; XMLPUBFUN;
function xmlAutomataGetInitState(am: xmlAutomataPtr): xmlAutomataStatePtr; XMLCALL; XMLPUBFUN;
function xmlAutomataSetFinalState(am: xmlAutomataPtr; state: xmlAutomataStatePtr): cint; XMLCALL; XMLPUBFUN;
function xmlAutomataNewState(am: xmlAutomataPtr): xmlAutomataStatePtr; XMLCALL; XMLPUBFUN;
function xmlAutomataNewTransition(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token: xmlCharPtr; data: pointer): xmlAutomataStatePtr; XMLCALL; XMLPUBFUN;
function xmlAutomataNewTransition2(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token, token2: xmlCharPtr; data: pointer): xmlAutomataStatePtr; XMLCALL; XMLPUBFUN;
function xmlAutomataNewNegTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token, token2: xmlCharPtr; data: pointer): xmlAutomataStatePtr; XMLCALL; XMLPUBFUN;
function xmlAutomataNewCountTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token: xmlCharPtr; min, max: cint; data: pointer): xmlAutomataStatePtr; XMLCALL; XMLPUBFUN;
function xmlAutomataNewCountTrans2(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token, token2: xmlCharPtr; min, max: cint; data: pointer): xmlAutomataStatePtr; XMLCALL; XMLPUBFUN;
function xmlAutomataNewOnceTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token: xmlCharPtr; min, max: cint; data: pointer): xmlAutomataStatePtr; XMLCALL; XMLPUBFUN;
function xmlAutomataNewOnceTrans2(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token, token2: xmlCharPtr; min, max: cint; data: pointer): xmlAutomataStatePtr; XMLCALL; XMLPUBFUN;
function xmlAutomataNewAllTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; lax: cint): xmlAutomataStatePtr; XMLCALL; XMLPUBFUN;
function xmlAutomataNewEpsilon(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr): xmlAutomataStatePtr; XMLCALL; XMLPUBFUN;
function xmlAutomataNewCountedTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; counter: cint): xmlAutomataStatePtr; XMLCALL; XMLPUBFUN;
function xmlAutomataNewCounterTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; counter: cint): xmlAutomataStatePtr; XMLCALL; XMLPUBFUN;
function xmlAutomataNewCounter(am: xmlAutomataPtr; min, max: cint): cint; XMLCALL; XMLPUBFUN;
function xmlAutomataCompile(am: xmlAutomataPtr): xmlRegexpPtr; XMLCALL; XMLPUBFUN;
function xmlAutomataIsDeterminist(am: xmlAutomataPtr): cint; XMLCALL; XMLPUBFUN;
{$ENDIF}

{$ENDIF} (* LIBXML_AUTOMATA_ENABLED *)
{$ENDIF} (* LIBXML_REGEXP_ENABLED *)

