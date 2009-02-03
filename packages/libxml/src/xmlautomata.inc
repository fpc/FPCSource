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

{$IFDEF POINTER}
  xmlAutomataPtr = ^xmlAutomata;
  xmlAutomataStatePtr = ^xmlAutomataState;
{$ENDIF}

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
function xmlNewAutomata: xmlAutomataPtr; EXTDECL; external xml2lib;
procedure xmlFreeAutomata(am: xmlAutomataPtr); EXTDECL; external xml2lib;
function xmlAutomataGetInitState(am: xmlAutomataPtr): xmlAutomataStatePtr; EXTDECL; external xml2lib;
function xmlAutomataSetFinalState(am: xmlAutomataPtr; state: xmlAutomataStatePtr): cint; EXTDECL; external xml2lib;
function xmlAutomataNewState(am: xmlAutomataPtr): xmlAutomataStatePtr; EXTDECL; external xml2lib;
function xmlAutomataNewTransition(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token: xmlCharPtr; data: pointer): xmlAutomataStatePtr; EXTDECL; external xml2lib;
function xmlAutomataNewTransition2(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token, token2: xmlCharPtr; data: pointer): xmlAutomataStatePtr; EXTDECL; external xml2lib;
function xmlAutomataNewNegTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token, token2: xmlCharPtr; data: pointer): xmlAutomataStatePtr; EXTDECL; external xml2lib;
function xmlAutomataNewCountTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token: xmlCharPtr; min, max: cint; data: pointer): xmlAutomataStatePtr; EXTDECL; external xml2lib;
function xmlAutomataNewCountTrans2(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token, token2: xmlCharPtr; min, max: cint; data: pointer): xmlAutomataStatePtr; EXTDECL; external xml2lib;
function xmlAutomataNewOnceTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token: xmlCharPtr; min, max: cint; data: pointer): xmlAutomataStatePtr; EXTDECL; external xml2lib;
function xmlAutomataNewOnceTrans2(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; token, token2: xmlCharPtr; min, max: cint; data: pointer): xmlAutomataStatePtr; EXTDECL; external xml2lib;
function xmlAutomataNewAllTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; lax: cint): xmlAutomataStatePtr; EXTDECL; external xml2lib;
function xmlAutomataNewEpsilon(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr): xmlAutomataStatePtr; EXTDECL; external xml2lib;
function xmlAutomataNewCountedTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; counter: cint): xmlAutomataStatePtr; EXTDECL; external xml2lib;
function xmlAutomataNewCounterTrans(am: xmlAutomataPtr; from, _to: xmlAutomataStatePtr; counter: cint): xmlAutomataStatePtr; EXTDECL; external xml2lib;
function xmlAutomataNewCounter(am: xmlAutomataPtr; min, max: cint): cint; EXTDECL; external xml2lib;
function xmlAutomataCompile(am: xmlAutomataPtr): xmlRegexpPtr; EXTDECL; external xml2lib;
function xmlAutomataIsDeterminist(am: xmlAutomataPtr): cint; EXTDECL; external xml2lib;
{$ENDIF}

{$ENDIF} (* LIBXML_AUTOMATA_ENABLED *)
{$ENDIF} (* LIBXML_REGEXP_ENABLED *)

