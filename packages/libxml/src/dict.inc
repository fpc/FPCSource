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
function xmlDictCreate: xmlDictPtr; XMLCALL; XMLPUBFUN;
function xmlDictCreateSub(sub: xmlDictPtr): xmlDictPtr; XMLCALL; XMLPUBFUN;
function xmlDictReference(dict: xmlDictPtr): cint; XMLCALL; XMLPUBFUN;
procedure xmlDictFree(dict: xmlDictPtr); XMLCALL; XMLPUBFUN;

(*
 * Lookup of entry in the dictionnary.
 *)
function xmlDictLookup(dict: xmlDictPtr; name: xmlCharPtr; len: cint): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlDictExists(dict: xmlDictPtr; name: xmlCharPtr; len: cint): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlDictQLookup(dict: xmlDictPtr; prefix, name: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlDictOwns(dict: xmlDictPtr; str: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlDictOwns(dict: xmlDictPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * Cleanup function
 *)
procedure xmlDictCleanup; XMLCALL; XMLPUBFUN;
{$ENDIF}
