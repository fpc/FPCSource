(*
 * Summary: interface for the XSLT namespace handling
 * Description: set of function easing the processing and generation
 *              of namespace nodes in XSLT.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)


(*
 * Used within nsAliases hashtable when the default namespace is required
 * but it's not been explicitly defined
 *)

{$IFDEF CONST}
  (**
   * UNDEFINED_DEFAULT_NS:
   *
   * Special value for undefined namespace, internal
   *)
  UNDEFINED_DEFAULT_NS = clong(-1);
{$ENDIF} {CONST}

{$IFDEF FUNCTION}
procedure xsltNamespaceAlias(style: xsltStylesheetPtr; node: xmlNodePtr); EXTDECL; external xsltlib;
function xsltGetNamespace(ctxt: xsltTransformContextPtr; cur: xmlNodePtr; ns: xmlNsPtr; &out: xmlNodePtr): xmlNsPtr; EXTDECL; external xsltlib;
function xsltGetPlainNamespace(ctxt: xsltTransformContextPtr; cur: xmlNodePtr; ns: xmlNsPtr; &out: xmlNodePtr): xmlNsPtr; EXTDECL; external xsltlib;
function xsltGetSpecialNamespace(ctxt: xsltTransformContextPtr; cur: xmlNodePtr; const URI: xmlCharPtr; const prefix: xmlCharPtr; &out: xmlNodePtr): xmlNsPtr; EXTDECL; external xsltlib;
function xsltCopyNamespace(ctxt: xsltTransformContextPtr; elem: xmlNodePtr; ns: xmlNsPtr): xmlNsPtr; EXTDECL; external xsltlib;
function xsltCopyNamespaceList(ctxt: xsltTransformContextPtr; node: xmlNodePtr; cur: xmlNsPtr): xmlNsPtr; EXTDECL; external xsltlib;
procedure xsltFreeNamespaceAliasHashes(style: xsltStylesheetPtr); EXTDECL; external xsltlib;
{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
  xsltNamespaceAlias: procedure(style: xsltStylesheetPtr; node: xmlNodePtr); EXTDECL;
  xsltGetNamespace: function(ctxt: xsltTransformContextPtr; cur: xmlNodePtr; ns: xmlNsPtr; &out: xmlNodePtr): xmlNsPtr; EXTDECL;
  xsltGetPlainNamespace: function(ctxt: xsltTransformContextPtr; cur: xmlNodePtr; ns: xmlNsPtr; &out: xmlNodePtr): xmlNsPtr; EXTDECL;
  xsltGetSpecialNamespace: function(ctxt: xsltTransformContextPtr; cur: xmlNodePtr; const URI: xmlCharPtr; const prefix: xmlCharPtr; &out: xmlNodePtr): xmlNsPtr; EXTDECL;
  xsltCopyNamespace: function(ctxt: xsltTransformContextPtr; elem: xmlNodePtr; ns: xmlNsPtr): xmlNsPtr; EXTDECL;
  xsltCopyNamespaceList: function(ctxt: xsltTransformContextPtr; node: xmlNodePtr; cur: xmlNsPtr): xmlNsPtr; EXTDECL;
  xsltFreeNamespaceAliasHashes: procedure(style: xsltStylesheetPtr); EXTDECL;
{$ENDIF} {FUNCTIONVAR}


