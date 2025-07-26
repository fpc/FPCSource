(*
 * Summary: precomputing stylesheets
 * Description: this is the compilation phase, where most of the
 *              stylesheet is "compiled" into faster to use data.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)


{$IFDEF FUNCTION}
(*
 * Interfaces
 *)
function xsltDocumentComp(style: xsltStylesheetPtr; inst: xmlNodePtr; &function: xsltTransformFunction): xsltElemPreCompPtr; EXTDECL; external xsltlib;
procedure xsltStylePreCompute(style: xsltStylesheetPtr; inst: xmlNodePtr); EXTDECL; external xsltlib;
procedure xsltFreeStylePreComps(style: xsltStylesheetPtr); EXTDECL; external xsltlib;
{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
  xsltDocumentComp: function(style: xsltStylesheetPtr; inst: xmlNodePtr; &function: xsltTransformFunction): xsltElemPreCompPtr; EXTDECL;
  xsltStylePreCompute: procedure(style: xsltStylesheetPtr; inst: xmlNodePtr); EXTDECL;
  xsltFreeStylePreComps: procedure(style: xsltStylesheetPtr); EXTDECL;
{$ENDIF} {FUNCTIONVAR}

{$IFDEF EXTVAR}
var
  xsltExtMarker: xmlCharPtr; external xsltlib;

{$ENDIF}
