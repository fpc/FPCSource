(*
 * Summary: interface for the template processing
 * Description: This set of routine encapsulates XPath calls
 *              and Attribute Value Templates evaluation.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF FUNCTION}
function xsltEvalXPathPredicate(ctxt: xsltTransformContextPtr; comp: xmlXPathCompExprPtr; nsList: xmlNsPtrPtr; nsNr: cint): cint; EXTDECL; external xsltlib;
function xsltEvalTemplateString(ctxt: xsltTransformContextPtr; contextNode: xmlNodePtr; inst: xmlNodePtr): xmlCharPtr; EXTDECL; external xsltlib;
function xsltEvalAttrValueTemplate(ctxt: xsltTransformContextPtr; node: xmlNodePtr; const name: xmlCharPtr; const ns: xmlCharPtr): xmlCharPtr; EXTDECL; external xsltlib;
function xsltEvalStaticAttrValueTemplate(style: xsltStylesheetPtr; node: xmlNodePtr; const name: xmlCharPtr; const ns: xmlCharPtr; found: pcint): xmlCharPtr; EXTDECL; external xsltlib;

(* TODO: this is obviously broken ... the namespaces should be passed too ! *)
function xsltEvalXPathString(ctxt: xsltTransformContextPtr; comp: xmlXPathCompExprPtr): xmlCharPtr; EXTDECL; external xsltlib;
function xsltEvalXPathStringNs(ctxt: xsltTransformContextPtr; comp: xmlXPathCompExprPtr; nsNr: cint; nsList: xmlNsPtrPtr): xmlCharPtr; EXTDECL; external xsltlib;
function xsltTemplateProcess(ctxt: xsltTransformContextPtr; node: xmlNodePtr): xmlNodePtrPtr; EXTDECL; external xsltlib;
function xsltAttrListTemplateProcess(ctxt: xsltTransformContextPtr; target: xmlNodePtr; cur: xmlAttrPtr): xmlAttrPtr; EXTDECL; external xsltlib;
function xsltAttrTemplateProcess(ctxt: xsltTransformContextPtr; target: xmlNodePtr; attr: xmlAttrPtr): xmlAttrPtr; EXTDECL; external xsltlib;
function xsltAttrTemplateValueProcess(ctxt: xsltTransformContextPtr; const attr: xmlCharPtr): xmlCharPtr; EXTDECL; external xsltlib;
function xsltAttrTemplateValueProcessNode(ctxt: xsltTransformContextPtr; const str: xmlCharPtr; node: xmlNodePtr): xmlCharPtr; EXTDECL; external xsltlib;
{$ENDIF} {FUNCTION}


{$IFDEF FUNCTIONVAR}
  xsltEvalXPathPredicate: function(ctxt: xsltTransformContextPtr; comp: xmlXPathCompExprPtr; nsList: xmlNsPtrPtr; nsNr: cint): cint; EXTDECL;
  xsltEvalTemplateString: function(ctxt: xsltTransformContextPtr; contextNode: xmlNodePtr; inst: xmlNodePtr): xmlCharPtr; EXTDECL;
  xsltEvalAttrValueTemplate: function(ctxt: xsltTransformContextPtr; node: xmlNodePtr; const name: xmlCharPtr; const ns: xmlCharPtr): xmlCharPtr; EXTDECL;
  xsltEvalStaticAttrValueTemplate: function(style: xsltStylesheetPtr; node: xmlNodePtr; const name: xmlCharPtr; const ns: xmlCharPtr; found: pcint): xmlCharPtr; EXTDECL;

  (* TODO: this is obviously broken ... the namespaces should be passed too ! *)
  xsltEvalXPathString: function(ctxt: xsltTransformContextPtr; comp: xmlXPathCompExprPtr): xmlCharPtr; EXTDECL;
  xsltEvalXPathStringNs: function(ctxt: xsltTransformContextPtr; comp: xmlXPathCompExprPtr; nsNr: cint; nsList: xmlNsPtrPtr): xmlCharPtr; EXTDECL;
  xsltTemplateProcess: function(ctxt: xsltTransformContextPtr; node: xmlNodePtr): xmlNodePtrPtr; EXTDECL;
  xsltAttrListTemplateProcess: function(ctxt: xsltTransformContextPtr; target: xmlNodePtr; cur: xmlAttrPtr): xmlAttrPtr; EXTDECL;
  xsltAttrTemplateProcess: function(ctxt: xsltTransformContextPtr; target: xmlNodePtr; attr: xmlAttrPtr): xmlAttrPtr; EXTDECL;
  xsltAttrTemplateValueProcess: function(ctxt: xsltTransformContextPtr; const attr: xmlCharPtr): xmlCharPtr; EXTDECL;
  xsltAttrTemplateValueProcessNode: function(ctxt: xsltTransformContextPtr; const str: xmlCharPtr; node: xmlNodePtr): xmlCharPtr; EXTDECL;
{$ENDIF} {FUNCTIONVAR}

