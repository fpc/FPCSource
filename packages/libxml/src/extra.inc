(*
 * Summary: interface for the non-standard features
 * Description: implement some extension outside the XSLT namespace
 *              but not EXSLT with is in a different library.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF CONST}
  (**
   * XSLT_LIBXSLT_NAMESPACE:
   *
   * This is the libxslt namespace for specific extensions.
   *)
  XSLT_LIBXSLT_NAMESPACE = 'http://xmlsoft.org/XSLT/namespace';

  (**
   * XSLT_SAXON_NAMESPACE:
   *
   * This is Michael Kay's Saxon processor namespace for extensions.
   *)
  XSLT_SAXON_NAMESPACE = 'http://icl.com/saxon';

  (**
   * XSLT_XT_NAMESPACE:
   *
   * This is James Clark's XT processor namespace for extensions.
   *)
  XSLT_XT_NAMESPACE = 'http://www.jclark.com/xt';

  (**
   * XSLT_XALAN_NAMESPACE:
   *
   * This is the Apache project XALAN processor namespace for extensions.
   *)
  XSLT_XALAN_NAMESPACE = 'org.apache.xalan.xslt.extensions.Redirect';
{$ENDIF} {CONST}

{$IFDEF FUNCTION}
procedure xsltFunctionNodeSet(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xsltlib;
procedure xsltDebug(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL; external xsltlib;
procedure xsltRegisterExtras(ctxt: xsltTransformContextPtr); EXTDECL; external xsltlib;
procedure xsltRegisterAllExtras; EXTDECL; external xsltlib;
{$ENDIF FUNCTION}

{$IFDEF FUNCTIONVAR}
  xsltFunctionNodeSet: procedure(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL;
  xsltDebug: procedure(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;
  xsltRegisterExtras: procedure(ctxt: xsltTransformContextPtr); EXTDECL;
  xsltRegisterAllExtras: procedure; EXTDECL;
{$ENDIF FUNCTIONVAR}


