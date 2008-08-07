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
{$IFDEF TYPE}
  xmlDict = record
  end;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Constructor and destructor.
 *)
function xmlDictCreate: xmlDictPtr; cdecl; external;
function xmlDictCreateSub(sub: xmlDictPtr): xmlDictPtr; cdecl; external;
function xmlDictReference(dict: xmlDictPtr): cint; cdecl; external;
procedure xmlDictFree(dict: xmlDictPtr); cdecl; external;

(*
 * Lookup of entry in the dictionnary.
 *)
function xmlDictLookup(dict: xmlDictPtr; name: xmlCharPtr; len: cint): xmlCharPtr; cdecl; external;
function xmlDictExists(dict: xmlDictPtr; name: xmlCharPtr; len: cint): xmlCharPtr; cdecl; external;
function xmlDictQLookup(dict: xmlDictPtr; prefix, name: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlDictOwns(dict: xmlDictPtr; str: xmlCharPtr): cint; cdecl; external;
function xmlDictOwns(dict: xmlDictPtr): cint; cdecl; external;

(*
 * Cleanup function
 *)
procedure xmlDictCleanup; cdecl; external;
{$ENDIF}
