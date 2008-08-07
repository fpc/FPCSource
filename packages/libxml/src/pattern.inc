(*
 * Summary: pattern expression handling
 * Description: allows to compile and test pattern expressions for nodes
 *              either in a tree or based on a parser state.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_PATTERN_ENABLED}

{$IFDEF TYPE}
(**
 * xmlPattern:
 *
 * A compiled (XPath based) pattern to select nodes
 *)
  xmlPattern = record end;

  xmlStreamCtxt = record end;

(**
 * xmlPatternFlags:
 *
 * This is the set of options affecting the behaviour of pattern
 * matching with this module
 *
 *)
  xmlPatternFlags = (
    XML_PATTERN_DEFAULT		= 0,	(* simple pattern match *)
    XML_PATTERN_XPATH		= 1 shl 0,	(* standard XPath pattern *)
    XML_PATTERN_XSSEL		= 1 shl 1,	(* XPath subset for schema selector *)
    XML_PATTERN_XSFIELD		= 1 shl 2	(* XPath subset for schema field *)
  );
{$ENDIF}

{$IFDEF FUNCTION}
procedure xmlFreePattern(comp: xmlPatternPtr); XMLCALL; XMLPUBFUN;
procedure xmlFreePatternList(comp: xmlPatternPtr); XMLCALL; XMLPUBFUN;
function xmlPatterncompile(pattern: xmlCharPtr; dict: xmlDictPtr; flags: cint; namespaces: xmlCharPtrPtr): xmlPatternPtr; XMLCALL; XMLPUBFUN;
function xmlPatternMatch(comp: xmlPatternPtr; node: xmlNodePtr): cint; XMLCALL; XMLPUBFUN;

(* streaming interfaces *)
function xmlPatternStreamable(comp: xmlPatternPtr): cint; XMLCALL; XMLPUBFUN;
function xmlPatternMaxDepth(comp: xmlPatternPtr): cint; XMLCALL; XMLPUBFUN;
function xmlPatternMinDepth(comp: xmlPatternPtr): cint; XMLCALL; XMLPUBFUN;
function xmlPatternFromRoot(comp: xmlPatternPtr): cint; XMLCALL; XMLPUBFUN;
function xmlPatternGetStreamCtxt(comp: xmlPatternPtr): xmlStreamCtxtPtr; XMLCALL; XMLPUBFUN;
procedure xmlFreeStreamCtxt(stream: xmlStreamCtxtPtr); XMLCALL; XMLPUBFUN;
function xmlStreamPushNode(stream: xmlStreamCtxtPtr; name, ns: xmlCharPtr; nodeType: cint): cint; XMLCALL; XMLPUBFUN;
function xmlStreamPush(stream: xmlStreamCtxtPtr; name, ns: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlStreamPushAttr(stream: xmlStreamCtxtPtr; name, ns: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlStreamPop(stream: xmlStreamCtxtPtr): cint; XMLCALL; XMLPUBFUN;
function xmlStreamWantsAnyNode(stream: xmlStreamCtxtPtr): cint; XMLCALL; XMLPUBFUN;
{$ENDIF}

{$ENDIF} (* LIBXML_PATTERN_ENABLED *)