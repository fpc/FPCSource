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
 * Initializer
 *)
function xmlInitializeDict: cint; EXTDECL; external xml2lib;

(*
 * Constructor and destructor.
 *)
function xmlDictCreate: xmlDictPtr; EXTDECL; external xml2lib;
function xmlDictSetLimit(dict: xmlDictPtr; limit: csize_t): csize_t; EXTDECL; external xml2lib;
function xmlDictGetUsage(dict: xmlDictPtr): csize_t; EXTDECL; external xml2lib;
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
function xmlDictSize(dict: xmlDictPtr): cint; EXTDECL; external xml2lib;

(*
 * Cleanup function
 *)
procedure xmlDictCleanup; EXTDECL; external xml2lib;
{$ENDIF}

{$IFDEF FUNCTIONVAR}
(*
 * Initializer
 *)
  xmlInitializeDict: function: cint; EXTDECL;

(*
 * Constructor and destructor.
 *)
  xmlDictCreate: function: xmlDictPtr; EXTDECL;
  xmlDictSetLimit: function(dict: xmlDictPtr; limit: csize_t): csize_t; EXTDECL;
  xmlDictGetUsage: function(dict: xmlDictPtr): csize_t; EXTDECL;
  xmlDictCreateSub: function(sub: xmlDictPtr): xmlDictPtr; EXTDECL;
  xmlDictReference: function(dict: xmlDictPtr): cint; EXTDECL;
  xmlDictFree: procedure(dict: xmlDictPtr); EXTDECL;

(*
 * Lookup of entry in the dictionnary.
 *)
  xmlDictLookup: function(dict: xmlDictPtr; name: xmlCharPtr; len: cint): xmlCharPtr; EXTDECL;
  xmlDictExists: function(dict: xmlDictPtr; name: xmlCharPtr; len: cint): xmlCharPtr; EXTDECL;
  xmlDictQLookup: function(dict: xmlDictPtr; prefix, name: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlDictOwns: function(dict: xmlDictPtr; str: xmlCharPtr): cint; EXTDECL;
  xmlDictSize: function(dict: xmlDictPtr): cint; EXTDECL;

(*
 * Cleanup function
 *)
  xmlDictCleanup: procedure; EXTDECL;
{$ENDIF}
