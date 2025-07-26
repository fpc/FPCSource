(*
 * Summary: interface for the libxslt security framework
 * Description: the libxslt security framework allow to restrict
 *              the access to new resources (file or URL) from
 *              the stylesheet at runtime.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF TYPE}
  (**
   * xsltSecurityPref:
   *
   * structure to indicate the preferences for security in the XSLT
   * transformation.
   *)
  xsltSecurityPrefs = record end;
  xsltSecurityPrefsPtr = ^xsltSecurityPrefs;

  (**
   * xsltSecurityOption:
   *
   * the set of option that can be configured
   *)
  xsltSecurityOption = (
    XSLT_SECPREF_READ_FILE = 1,
    XSLT_SECPREF_WRITE_FILE,
    XSLT_SECPREF_CREATE_DIRECTORY,
    XSLT_SECPREF_READ_NETWORK,
    XSLT_SECPREF_WRITE_NETWORK
  );

  (**
   * xsltSecurityCheck:
   *
   * User provided function to check the value of a string like a file
   * path or an URL ...
   *)
  xsltSecurityCheck = function(sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; const value: PAnsiChar): cint; EXTDECL;

{$ENDIF} {TYPE}

{$IFDEF FUNCTION}
(*
 * Module interfaces
 *)
function xsltNewSecurityPrefs: xsltSecurityPrefsPtr; EXTDECL; external xsltlib;
procedure xsltFreeSecurityPrefs(sec: xsltSecurityPrefsPtr); EXTDECL; external xsltlib;
function xsltSetSecurityPrefs(sec: xsltSecurityPrefsPtr; option: xsltSecurityOption; func: xsltSecurityCheck): cint; EXTDECL; external xsltlib;
function xsltGetSecurityPrefs(sec: xsltSecurityPrefsPtr; option: xsltSecurityOption): xsltSecurityCheck; EXTDECL; external xsltlib;
procedure xsltSetDefaultSecurityPrefs(sec: xsltSecurityPrefsPtr); EXTDECL; external xsltlib;
function xsltGetDefaultSecurityPrefs: xsltSecurityPrefsPtr; EXTDECL; external xsltlib;
function xsltSetCtxtSecurityPrefs(sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr): cint; EXTDECL; external xsltlib;
function xsltSecurityAllow(sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; const value: PAnsiChar): cint; EXTDECL; external xsltlib;
function xsltSecurityForbid(sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; const value: PAnsiChar): cint; EXTDECL; external xsltlib;

(*
 * internal interfaces
 *)
function xsltCheckWrite(sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; const URL: xmlCharPtr): cint; EXTDECL; external xsltlib;
function xsltCheckRead(sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; const URL: xmlCharPtr): cint; EXTDECL; external xsltlib;
{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
(*
 * Module interfaces
 *)
  xsltNewSecurityPrefs: function: xsltSecurityPrefsPtr; EXTDECL;
  xsltFreeSecurityPrefs: procedure(sec: xsltSecurityPrefsPtr); EXTDECL;
  xsltSetSecurityPrefs: function(sec: xsltSecurityPrefsPtr; option: xsltSecurityOption; func: xsltSecurityCheck): cint; EXTDECL;
  xsltGetSecurityPrefs: function(sec: xsltSecurityPrefsPtr; option: xsltSecurityOption): xsltSecurityCheck; EXTDECL;
  xsltSetDefaultSecurityPrefs: procedure(sec: xsltSecurityPrefsPtr); EXTDECL;
  xsltGetDefaultSecurityPrefs: function: xsltSecurityPrefsPtr; EXTDECL;
  xsltSetCtxtSecurityPrefs: function(sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr): cint; EXTDECL;
  xsltSecurityAllow: function(sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; const value: PAnsiChar): cint; EXTDECL;
  xsltSecurityForbid: function(sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; const value: PAnsiChar): cint; EXTDECL;

(*
 * internal interfaces
 *)
  xsltCheckWrite: function(sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; const URL: xmlCharPtr): cint; EXTDECL;
  xsltCheckRead: function(sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; const URL: xmlCharPtr): cint; EXTDECL;
{$ENDIF} {FUNCTIONVAR}
