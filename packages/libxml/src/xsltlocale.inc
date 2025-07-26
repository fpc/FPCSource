(*
 * Summary: Locale handling
 * Description: Interfaces for locale handling. Needed for language dependent
 *              sorting.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Nick Wellnhofer
 *)

{$IFDEF HAVE_STRXFRM_L}

{$IFDEF TYPE}
  {$IF DEFINED(POSIX)}
  (*
   * XSLT_LOCALE_POSIX:
   * Macro indicating to use POSIX locale extensions
   *)
  {$DEFINE XSLT_LOCALE_POSIX}
  xsltLocale = locale_t;
  xsltLocaleChar = xmlChar;
  {$ELSEIF DEFINED(WINDOWS)}
  (*
   * XSLT_LOCALE_WINAPI:
   * Macro indicating to use WinAPI for extended locale support
   *)
  {$DEFINE XSLT_LOCALE_WINAPI}
  xsltLocale = LCID;
  xsltLocaleChar = WideChar;
  {$ELSE}
  (*
   * XSLT_LOCALE_NONE:
   * Macro indicating that there's no extended locale support
   *)
  {$DEFINE XSLT_LOCALE_NONE}
  xsltLocale = Pointer;
  xsltLocaleChar = xmlChar;
  {$ENDIF}
  xsltLocaleCharPtr = ^xsltLocaleChar;
{$ENDIF} {TYPE}

{$IFDEF FUNCTION}
function xsltNewLocale(const langName: xmlCharPtr): xsltLocale; EXTDECL; external xsltlib;
procedure xsltFreeLocale(locale: xsltLocale); EXTDECL; external xsltlib;
function xsltStrxfrm(locale: xsltLocale; const astring: xmlCharPtr): xsltLocaleCharPtr; EXTDECL; external xsltlib;
function xsltLocaleStrcmp(locale: xsltLocale; const str1, str2: xsltLocaleCharPtr): LongInt; EXTDECL; external xsltlib;
procedure xsltFreeLocales; EXTDECL; external xsltlib;
{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
  xsltNewLocale: function(const langName: xmlCharPtr): xsltLocale; EXTDECL;
  xsltFreeLocale: procedure(locale: xsltLocale); EXTDECL;
  xsltStrxfrm: function(locale: xsltLocale; const astring: xmlCharPtr): xsltLocaleCharPtr; EXTDECL;
  xsltLocaleStrcmp: function(locale: xsltLocale; const str1, str2: xsltLocaleCharPtr): LongInt; EXTDECL;
  xsltFreeLocales: procedure; EXTDECL;
{$ENDIF} {FUNCTION}

{$ENDIF} {HAVE_STRXFRM_L}
