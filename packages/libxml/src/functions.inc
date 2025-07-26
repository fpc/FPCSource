(*
 * Summary: interface for the XSLT functions not from XPath
 * Description: a set of extra functions coming from XSLT but not in XPath
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard and Bjorn Reese <breese@users.sourceforge.net>
 *)

(**
 * XSLT_REGISTER_FUNCTION_LOOKUP:
 *
 * Registering macro, not general purpose at all but used in different modules.
 *
#define XSLT_REGISTER_FUNCTION_LOOKUP(ctxt)			\
    xmlXPathRegisterFuncLookup((ctxt)->xpathCtxt,		\
	xsltXPathFunctionLookup,				\
	(void * )(ctxt->xpathCtxt));
*)

{$IFDEF FUNCTION}
function xsltXPathFunctionLookup(vctxt: Pointer; const name: xmlCharPtr; const ns_uri: xmlCharPtr): xmlXPathFunction; EXTDECL; external xsltlib;

(*
 * Interfaces for the functions implementations.
 *)

procedure xsltDocumentFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xsltlib;
procedure xsltKeyFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xsltlib;
procedure xsltUnparsedEntityURIFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xsltlib;
procedure xsltFormatNumberFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xsltlib;
procedure xsltGenerateIdFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xsltlib;
procedure xsltSystemPropertyFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xsltlib;
procedure xsltElementAvailableFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xsltlib;
procedure xsltFunctionAvailableFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xsltlib;

(*
 * And the registration
 *)

procedure xsltRegisterAllFunctions(ctxt: xmlXPathContextPtr); EXTDECL; external xsltlib;

{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
  xsltXPathFunctionLookup: function(vctxt: Pointer; const name: xmlCharPtr; const ns_uri: xmlCharPtr): xmlXPathFunction; EXTDECL;

(*
 * Interfaces for the functions implementations.
 *)

  xsltDocumentFunction: procedure(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL;
  xsltKeyFunction: procedure(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL;
  xsltUnparsedEntityURIFunction: procedure(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL;
  xsltFormatNumberFunction: procedure(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL;
  xsltGenerateIdFunction: procedure(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL;
  xsltSystemPropertyFunction: procedure(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL;
  xsltElementAvailableFunction: procedure(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL;
  xsltFunctionAvailableFunction: procedure(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL;

(*
 * And the registration
 *)

  xsltRegisterAllFunctions: procedure(ctxt: xmlXPathContextPtr); EXTDECL;

{$ENDIF} {FUNCTIONVAR}


