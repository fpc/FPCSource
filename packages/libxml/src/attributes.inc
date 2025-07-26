(*
 * Summary: interface for the XSLT attribute handling
 * Description: this module handles the specificities of attribute
 *              and attribute groups processing.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF FUNCTION}
procedure xsltParseStylesheetAttributeSet(style: xsltStylesheetPtr; cur: xmlNodePtr); EXTDECL; external xsltlib;
procedure xsltFreeAttributeSetsHashes(style: xsltStylesheetPtr); EXTDECL; external xsltlib;
procedure xsltApplyAttributeSet(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; const attributes: xmlCharPtr); EXTDECL; external xsltlib;
procedure xsltResolveStylesheetAttributeSet(style: xsltStylesheetPtr); EXTDECL; external xsltlib;
{$ENDIF}

{$IFDEF FUNCTIONVAR}
  xsltParseStylesheetAttributeSet: procedure(style: xsltStylesheetPtr; cur: xmlNodePtr); EXTDECL;
  xsltFreeAttributeSetsHashes: procedure(style: xsltStylesheetPtr); EXTDECL;
  xsltApplyAttributeSet: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; const attributes: xmlCharPtr); EXTDECL;
  xsltResolveStylesheetAttributeSet: procedure(style: xsltStylesheetPtr); EXTDECL;
{$ENDIF}

