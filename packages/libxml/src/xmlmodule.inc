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
function xmlModuleOpen(filename: AnsiChar; options: cint): xmlModulePtr; EXTDECL; external xml2lib;
function xmlModuleSymbol(module: xmlModulePtr; name: PAnsiChar; var result: pointer): cint; EXTDECL; external xml2lib;
function xmlModuleClose(module: xmlModulePtr): cint; EXTDECL; external xml2lib;
function xmlModuleFree(module: xmlModulePtr): cint; EXTDECL; external xml2lib;
{$ENDIF}

{$IFDEF FUNCTIONVAR}
  xmlModuleOpen: function(filename: AnsiChar; options: cint): xmlModulePtr; EXTDECL;
  xmlModuleSymbol: function(module: xmlModulePtr; name: PAnsiChar; var result: pointer): cint; EXTDECL;
  xmlModuleClose: function(module: xmlModulePtr): cint; EXTDECL;
  xmlModuleFree: function(module: xmlModulePtr): cint; EXTDECL;
{$ENDIF}

{$ENDIF} (* LIBXML_MODULES_ENABLED *)

