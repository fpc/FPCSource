(*
 * Summary: set of routines to process strings
 * Description: type and interfaces needed for the internal string handling
 *              of the library, especially UTF8 processing.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF POINTER}
  xmlCharPtr = PChar;
  xmlCharPtrPtr = PPChar;
{$ENDIF}

{$IFDEF TYPE}
(**
 * xmlChar:
 *
 * This is a basic byte in an UTF-8 encoded string.
 * It's unsigned allowing to pinpoint case where char * are assigned
 * to xmlChar * (possibly making serialization back impossible).
 *)
  xmlChar = Char;
{$ENDIF}

{$IFDEF FUNCTION}
(**
 * BAD_CAST:
 *
 * Macro to cast a string to an xmlChar * when one know its safe.
 *)
//function BAD_CAST(str: string): xmlCharPtr; inline;

(*
 * xmlChar handling
 *)
function xmlStrdup(cur: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlStrndup(cur: xmlCharPtr; len: cint): xmlCharPtr; EXTDECL; external xml2lib;
function xmlCharStrndup(cur: pchar; len: cint): xmlCharPtr; EXTDECL; external xml2lib;
function xmlCharStrdup(cur: pchar): xmlCharPtr; EXTDECL; external xml2lib;
function xmlStrsub(str: xmlCharPtr; start: cint; len: cint): xmlCharPtr; EXTDECL; external xml2lib;
function xmlStrchr(str: xmlCharPtr; val: xmlChar): xmlCharPtr; EXTDECL; external xml2lib;
function xmlStrstr(str: xmlCharPtr; val: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlStrcasestr(str: xmlCharPtr; val: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlStrcmp(str1: xmlCharPtr; str2: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlStrncmp(str1: xmlCharPtr; str2: xmlCharPtr; len: cint): cint; EXTDECL; external xml2lib;
function xmlStrcasecmp(str1: xmlCharPtr; str2: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlStrncasecmp(str1: xmlCharPtr; str2: xmlCharPtr; len: cint): cint; EXTDECL; external xml2lib;
function xmlStrEqual(str1: xmlCharPtr; str2: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlStrQEqual(pref: xmlCharPtr; name: xmlCharPtr; str: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlStrlen(str: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlStrcat(cur: xmlCharPtr; add: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlStrncat(cur: xmlCharPtr; add: xmlCharPtr; len: cint): xmlCharPtr; EXTDECL; external xml2lib;
function xmlStrncatNew(str1: xmlCharPtr; str2: xmlCharPtr; len: cint): xmlCharPtr; EXTDECL; external xml2lib;
function xmlStrPrintf(buf: xmlCharPtr; len: cint; msg: xmlCharPtr; args: array of const): cint; cdecl; external xml2lib;
function xmlStrVPrintf(buf: xmlCharPtr; len: cint; msg: xmlCharPtr; ap: va_list): cint; EXTDECL; external xml2lib;
function xmlGetUTF8Char(utf: pchar; len: pcint): cint; EXTDECL; external xml2lib;
function xmlCheckUTF8(utf: pchar): cint; EXTDECL; external xml2lib;
function xmlUTF8Strsize(utf: xmlCharPtr; len: cint): cint; EXTDECL; external xml2lib;
function xmlUTF8Strndup(utf: xmlCharPtr; len: cint): xmlCharPtr; EXTDECL; external xml2lib;
function xmlUTF8Strpos(utf: xmlCharPtr; pos: cint): xmlCharPtr; EXTDECL; external xml2lib;
function xmlUTF8Strloc(utf: xmlCharPtr; utfchar: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlUTF8Strsub(str: xmlCharPtr; start: cint; len: cint): xmlCharPtr; EXTDECL; external xml2lib;
function xmlUTF8Strlen(utf: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlUTF8Size(utf: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlUTF8Charcmp(utf1: xmlCharPtr; utf2: xmlCharPtr): cint; EXTDECL; external xml2lib;
{$ENDIF}
