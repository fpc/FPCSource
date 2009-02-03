(*
 * Summary: string dictionnary
 * Description: dictionary of reusable strings, just used to avoid allocation
 *         and freeing operations.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

(*
 * The dictionnary
 *)
{$IFDEF POINTER}
  xmlDictPtr = ^xmlDict;
{$ENDIF}

{$IFDEF TYPE}
  xmlDict = record end;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Constructor and destructor.
 *)
function xmlDictCreate: xmlDictPtr; EXTDECL; external xml2lib;
function xmlDictCreateSub(sub: xmlDictPtr): xmlDictPtr; EXTDECL; external xml2lib;
function xmlDictReference(dict: xmlDictPtr): cint; EXTDECL; external xml2lib;
procedure xmlDictFree(dict: xmlDictPtr); EXTDECL; external xml2lib;

(*
 * Lookup of entry in the dictionnary.
 *)
function xmlDictLookup(dict: xmlDictPtr; name: xmlCharPtr; len: cint): xmlCharPtr; EXTDECL; external xml2lib;
function xmlDictExists(dict: xmlDictPtr; name: xmlCharPtr; len: cint): xmlCharPtr; EXTDECL; external xml2lib;
function xmlDictQLookup(dict: xmlDictPtr; prefix, name: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlDictOwns(dict: xmlDictPtr; str: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlDictOwns(dict: xmlDictPtr): cint; EXTDECL; external xml2lib;

(*
 * Cleanup function
 *)
procedure xmlDictCleanup; EXTDECL; external xml2lib;
{$ENDIF}
