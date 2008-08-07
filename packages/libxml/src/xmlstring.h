(*
 * Summary: set of routines to process strings
 * Description: type and interfaces needed for the internal string handling
 *              of the library, especially UTF8 processing.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)


{$IFDEF TYPE}
(**
 * xmlChar:
 *
 * This is a basic byte in an UTF-8 encoded string.
 * It's unsigned allowing to pinpoint case where char * are assigned
 * to xmlChar * (possibly making serialization back impossible).
 *)
  xmlChar = cchar;
{$ENDIF}

(**
 * BAD_CAST:
 *
 * Macro to cast a string to an xmlChar * when one know its safe.
 *)
//#define BAD_CAST (xmlChar *)

{$IFDEF FUNCTION}
(*
 * xmlChar handling
 *)
function xmlStrdup(cur: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlStrndup(cur: xmlCharPtr; len: cint): xmlCharPtr; cdecl; external;
function xmlCharStrndup(cur: pchar; len: cint): xmlCharPtr; cdecl; external;
function xmlCharStrdup(cur: pchar): xmlCharPtr; cdecl; external;
function xmlStrsub(str: xmlCharPtr; start: cint; len: cint): xmlCharPtr; cdecl; external;
function xmlStrchr(str: xmlCharPtr; val: xmlChar): xmlCharPtr; cdecl; external;
function xmlStrstr(str: xmlCharPtr; val: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlStrcasestr(str: xmlCharPtr; val: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlStrcmp(str1: xmlCharPtr; str2: xmlCharPtr): cint; cdecl; external;
function xmlStrncmp(str1: xmlCharPtr; str2: xmlCharPtr; len: cint): cint; cdecl; external;
function xmlStrcasecmp(str1: xmlCharPtr; str2: xmlCharPtr): cint; cdecl; external;
function xmlStrncasecmp(str1: xmlCharPtr; str2: xmlCharPtr; len: cint): cint; cdecl; external;
function xmlStrEqual(str1: xmlCharPtr; str2: xmlCharPtr): cint; cdecl; external;
function xmlStrQEqual(pref: xmlCharPtr; name: xmlCharPtr; str: xmlCharPtr): cint; cdecl; external;
function xmlStrlen(str: xmlCharPtr): cint; cdecl; external;
function xmlStrcat(cur: xmlCharPtr; add: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlStrncat(cur: xmlCharPtr; add: xmlCharPtr; len: cint): xmlCharPtr; cdecl; external;
function xmlStrncatNew(str1: xmlCharPtr; str2: xmlCharPtr; len: cint): xmlCharPtr; cdecl; external;
function xmlStrPrintf(buf: xmlCharPtr; len: cint; msg: xmlCharPtr; args: array of const): cint; cdecl; external;
function xmlStrVPrintf(buf: xmlCharPtr; len: cint; msg: xmlCharPtr; ap: va_list): cint; cdecl; external;
function xmlGetUTF8Char(utf: pchar; len: pcint): cint; cdecl; external;
function xmlCheckUTF8(utf: pchar): cint; cdecl; external;
function xmlUTF8Strsize(utf: xmlCharPtr; len: cint): cint; cdecl; external;
function xmlUTF8Strndup(utf: xmlCharPtr; len: cint): xmlCharPtr; cdecl; external;
function xmlUTF8Strpos(utf: xmlCharPtr; pos: cint): xmlCharPtr; cdecl; external;
function xmlUTF8Strloc(utf: xmlCharPtr; utfchar: xmlCharPtr): cint; cdecl; external;
function xmlUTF8Strsub(str: xmlCharPtr; start: cint; len: cint): xmlCharPtr; cdecl; external;
function xmlUTF8Strlen(utf: xmlCharPtr): cint; cdecl; external;
function xmlUTF8Size(utf: xmlCharPtr): cint; cdecl; external;
function xmlUTF8Charcmp(utf1: xmlCharPtr; utf2: xmlCharPtr): cint; cdecl; external;
{$ENDIF}