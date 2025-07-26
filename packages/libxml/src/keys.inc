(*
 * Summary:  interface for the key matching used in key() and template matches.
 * Description: implementation of the key mechanims.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF CONST}
(**
 * NODE_IS_KEYED:
 *
 * check for bit 15 set
 *)
  NODE_IS_KEYED = 1 shr 15;
{$ENDIF} {CONST}

{$IFDEF FUNCTION}
function xsltAddKey(style: xsltStylesheetPtr; const name: xmlCharPtr; const nameURI: xmlCharPtr; const match: xmlCharPtr; const use: xmlCharPtr; inst: xmlNodePtr): cint; EXTDECL; external xsltlib;
function xsltGetKey(ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const nameURI: xmlCharPtr; const value: xmlCharPtr): xmlNodeSetPtr; EXTDECL; external xsltlib;
procedure xsltInitCtxtKeys(ctxt: xsltTransformContextPtr; doc: xsltDocumentPtr); EXTDECL; external xsltlib;
procedure xsltFreeKeys(style: xsltStylesheetPtr); EXTDECL; external xsltlib;
procedure xsltFreeDocumentKeys(doc: xsltDocumentPtr); EXTDECL; external xsltlib;
{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
  xsltAddKey: function(style: xsltStylesheetPtr; const name: xmlCharPtr; const nameURI: xmlCharPtr; const match: xmlCharPtr; const use: xmlCharPtr; inst: xmlNodePtr): cint; EXTDECL;
  xsltGetKey: function(ctxt: xsltTransformContextPtr; const name: xmlCharPtr; const nameURI: xmlCharPtr; const value: xmlCharPtr): xmlNodeSetPtr; EXTDECL;
  xsltInitCtxtKeys: procedure(ctxt: xsltTransformContextPtr; doc: xsltDocumentPtr); EXTDECL;
  xsltFreeKeys: procedure(style: xsltStylesheetPtr); EXTDECL;
  xsltFreeDocumentKeys: procedure(doc: xsltDocumentPtr); EXTDECL;
{$ENDIF} {FUNCTIONVAR}


