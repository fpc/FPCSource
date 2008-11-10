(*
 * Summary: dynamic module loading
 * Description: basic API for dynamic module loading, used by
 *              libexslt added in 2.6.17
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Joel W. Reed
 *)

{$IFDEF LIBXML_MODULES_ENABLED}

{$IFDEF POINTER}
  xmlModulePtr = ^xmlModule;
{$ENDIF}

{$IFDEF TYPE}
(**
 * xmlModulePtr:
 *
 * A handle to a dynamically loaded module
 *)
  xmlModule = record end;

(**
 * xmlModuleOption:
 *
 * enumeration of options that can be passed down to xmlModuleOpen()
 *)
  xmlModuleOption = (
    XML_MODULE_LAZY = 1,	(* lazy binding *)
    XML_MODULE_LOCAL= 2		(* local binding *)
  );
{$ENDIF}

{$IFDEF FUNCTION}
function xmlModuleOpen(filename: char; options: cint): xmlModulePtr; XMLCALL; XMLPUBFUN;
function xmlModuleSymbol(module: xmlModulePtr; name: pchar; var result: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlModuleClose(module: xmlModulePtr): cint; XMLCALL; XMLPUBFUN;
function xmlModuleFree(module: xmlModulePtr): cint; XMLCALL; XMLPUBFUN;
{$ENDIF}

{$ENDIF} (* LIBXML_MODULES_ENABLED *)

