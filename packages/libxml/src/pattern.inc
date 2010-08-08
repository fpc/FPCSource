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

{$IFDEF POINTER}
  xmlPatternPtr = ^xmlPattern;
  xmlStreamCtxtPtr = ^xmlStreamCtxt;
{$ENDIF}

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
  xmlPatternFlags = type cint;
{$ENDIF}

{$IFDEF CONST}
  XML_PATTERN_DEFAULT		= 0;	(* simple pattern match *)
  XML_PATTERN_XPATH		= 1 shl 0;	(* standard XPath pattern *)
  XML_PATTERN_XSSEL		= 1 shl 1;	(* XPath subset for schema selector *)
  XML_PATTERN_XSFIELD		= 1 shl 2;	(* XPath subset for schema field *)
{$ENDIF}

{$IFDEF FUNCTION}
procedure xmlFreePattern(comp: xmlPatternPtr); EXTDECL; external xml2lib;
procedure xmlFreePatternList(comp: xmlPatternPtr); EXTDECL; external xml2lib;
function xmlPatterncompile(pattern: xmlCharPtr; dict: xmlDictPtr; flags: cint; namespaces: xmlCharPtrPtr): xmlPatternPtr; EXTDECL; external xml2lib;
function xmlPatternMatch(comp: xmlPatternPtr; node: xmlNodePtr): cint; EXTDECL; external xml2lib;

(* streaming interfaces *)
function xmlPatternStreamable(comp: xmlPatternPtr): cint; EXTDECL; external xml2lib;
function xmlPatternMaxDepth(comp: xmlPatternPtr): cint; EXTDECL; external xml2lib;
function xmlPatternMinDepth(comp: xmlPatternPtr): cint; EXTDECL; external xml2lib;
function xmlPatternFromRoot(comp: xmlPatternPtr): cint; EXTDECL; external xml2lib;
function xmlPatternGetStreamCtxt(comp: xmlPatternPtr): xmlStreamCtxtPtr; EXTDECL; external xml2lib;
procedure xmlFreeStreamCtxt(stream: xmlStreamCtxtPtr); EXTDECL; external xml2lib;
function xmlStreamPushNode(stream: xmlStreamCtxtPtr; name, ns: xmlCharPtr; nodeType: cint): cint; EXTDECL; external xml2lib;
function xmlStreamPush(stream: xmlStreamCtxtPtr; name, ns: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlStreamPushAttr(stream: xmlStreamCtxtPtr; name, ns: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlStreamPop(stream: xmlStreamCtxtPtr): cint; EXTDECL; external xml2lib;
function xmlStreamWantsAnyNode(stream: xmlStreamCtxtPtr): cint; EXTDECL; external xml2lib;
{$ENDIF}

{$ENDIF} (* LIBXML_PATTERN_ENABLED *)