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
  xmlCharPtr = pchar;
  xmlCharPtrPtr = ^xmlCharPtr;
{$ENDIF}

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

{$IFDEF FUNCTION}
(**
 * BAD_CAST:
 *
 * Macro to cast a string to an xmlChar * when one know its safe.
 *)
function BAD_CAST(str: pchar): xmlCharPtr; inline;
function BAD_CAST(str: string): xmlCharPtr; inline;

(*
 * xmlChar handling
 *)
function xmlStrdup(cur: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlStrndup(cur: xmlCharPtr; len: cint): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlCharStrndup(cur: pchar; len: cint): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlCharStrdup(cur: pchar): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlStrsub(str: xmlCharPtr; start: cint; len: cint): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlStrchr(str: xmlCharPtr; val: xmlChar): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlStrstr(str: xmlCharPtr; val: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlStrcasestr(str: xmlCharPtr; val: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlStrcmp(str1: xmlCharPtr; str2: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlStrncmp(str1: xmlCharPtr; str2: xmlCharPtr; len: cint): cint; XMLCALL; XMLPUBFUN;
function xmlStrcasecmp(str1: xmlCharPtr; str2: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlStrncasecmp(str1: xmlCharPtr; str2: xmlCharPtr; len: cint): cint; XMLCALL; XMLPUBFUN;
function xmlStrEqual(str1: xmlCharPtr; str2: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlStrQEqual(pref: xmlCharPtr; name: xmlCharPtr; str: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlStrlen(str: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlStrcat(cur: xmlCharPtr; add: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlStrncat(cur: xmlCharPtr; add: xmlCharPtr; len: cint): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlStrncatNew(str1: xmlCharPtr; str2: xmlCharPtr; len: cint): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlStrPrintf(buf: xmlCharPtr; len: cint; msg: xmlCharPtr; args: array of const): cint; XMLCDECL; XMLPUBFUN;
function xmlStrVPrintf(buf: xmlCharPtr; len: cint; msg: xmlCharPtr; ap: va_list): cint; XMLCALL; XMLPUBFUN;
function xmlGetUTF8Char(utf: pchar; len: pcint): cint; XMLCALL; XMLPUBFUN;
function xmlCheckUTF8(utf: pchar): cint; XMLCALL; XMLPUBFUN;
function xmlUTF8Strsize(utf: xmlCharPtr; len: cint): cint; XMLCALL; XMLPUBFUN;
function xmlUTF8Strndup(utf: xmlCharPtr; len: cint): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlUTF8Strpos(utf: xmlCharPtr; pos: cint): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlUTF8Strloc(utf: xmlCharPtr; utfchar: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlUTF8Strsub(str: xmlCharPtr; start: cint; len: cint): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlUTF8Strlen(utf: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlUTF8Size(utf: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlUTF8Charcmp(utf1: xmlCharPtr; utf2: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
{$ENDIF}
