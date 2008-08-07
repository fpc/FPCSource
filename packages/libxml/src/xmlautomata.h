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
function xmlNewAutomata: xmlAutomataPtr; cdecl; external;
procedure xmlFreeAutomata(am: xmlAutomataPtr); cdecl; external;
function xmlAutomataGetInitState(am: xmlAutomataPtr): xmlAutomataStatePtr; cdecl; external;
function xmlAutomataSetFinalState(am: xmlAutomataPtr; state: xmlAutomataStatePtr): cint; cdecl; external;
function xmlAutomataNewState(am: xmlAutomataPtr): xmlAutomataStatePtr; cdecl; external;
function xmlAutomataNewTransition(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token: xmlCharPtr; data: pointer): xmlAutomataStatePtr; cdecl; external;
function xmlAutomataNewTransition2(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token, token2: xmlCharPtr; data: pointer): xmlAutomataStatePtr; cdecl; external;
function xmlAutomataNewNegTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token, token2: xmlCharPtr; data: pointer): xmlAutomataStatePtr; cdecl; external;
function xmlAutomataNewCountTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token: xmlCharPtr; min, max: cint; data: pointer): xmlAutomataStatePtr; cdecl; external;
function xmlAutomataNewCountTrans2(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token, token2: xmlCharPtr; min, max: cint; data: pointer): xmlAutomataStatePtr; cdecl; external;
function xmlAutomataNewOnceTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token: xmlCharPtr; min, max: cint; data: pointer): xmlAutomataStatePtr; cdecl; external;
function xmlAutomataNewOnceTrans2(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token, token2: xmlCharPtr; min, max: cint; data: pointer): xmlAutomataStatePtr; cdecl; external;
function xmlAutomataNewAllTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; lax: cint): xmlAutomataStatePtr; cdecl; external;
function xmlAutomataNewEpsilon(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr): xmlAutomataStatePtr; cdecl; external;
function xmlAutomataNewCountedTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; counter: cint): xmlAutomataStatePtr; cdecl; external;
function xmlAutomataNewCounterTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; counter: cint): xmlAutomataStatePtr; cdecl; external;
function xmlAutomataNewCounter(am: xmlAutomataPtr; min, max: cint): cint; cdecl; external;
function xmlAutomataCompile(am: xmlAutomataPtr): xmlRegexpPtr; cdecl; external;
function xmlAutomataIsDeterminist(am: xmlAutomataPtr): cint; cdecl; external;
{$ENDIF}

{$ENDIF} (* LIBXML_AUTOMATA_ENABLED *)
{$ENDIF} (* LIBXML_REGEXP_ENABLED *)

