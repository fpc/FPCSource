(*
 * Summary: API to handle XML Pointers
 * Description: API to handle XML Pointers
 * Base implementation was made accordingly to
 * W3C Candidate Recommendation 7 June 2000
 * http://www.w3.org/TR/2000/CR-xptr-20000607
 *
 * Added support for the element() scheme described in:
 * W3C Proposed Recommendation 13 November 2002
 * http://www.w3.org/TR/2002/PR-xptr-element-20021113/  
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_XPTR_ENABLED}

{$IFDEF POINTER}
  xmlLocationSetPtr = ^xmlLocationSet;
{$ENDIF}

{$IFDEF TYPE}
(*
 * A Location Set
 *)
  xmlLocationSet = record
    locNr   : cint;		      (* number of locations in the set *)
    locMax  : cint;		      (* size of the array as allocated *)
    locTab  : xmlXPathObjectPtrPtr;(* array of locations *)
  end;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Handling of location sets.
 *)
function xmlXPtrLocationSetCreate(val: xmlXPathObjectPtr): xmlLocationSetPtr; EXTDECL; external xml2lib;
procedure xmlXPtrLocationSetCreate(obj: xmlLocationSetPtr); EXTDECL; external xml2lib;
function xmlXPtrLocationSetMerge(val1, val2: xmlLocationSetPtr): xmlLocationSetPtr; EXTDECL; external xml2lib;
function xmlXPtrNewRange(start: xmlNodePtr; startindex: cint; _end: xmlNodePtr; endindex: cint): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPtrNewRangePoints(start, _end: xmlXPathObjectPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPtrNewRangeNodePoint(start: xmlNodePtr; _end: xmlXPathObjectPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPtrNewRangePointNode(start: xmlXPathObjectPtr; _end: xmlNodePtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPtrNewRangeNodes(start: xmlNodePtr; _end: xmlNodePtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPtrNewLocationSetNodes(start: xmlNodePtr; _end: xmlNodePtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPtrNewLocationSetNodeSet(_set: xmlNodeSetPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPtrNewRangeNodeObject(start: xmlNodePtr; _end: xmlXPathObjectPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPtrNewCollapsedRange(start: xmlNodePtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
procedure xmlXPtrLocationSetAdd(cur: xmlLocationSetPtr; val: xmlXPathObjectPtr); EXTDECL; external xml2lib;
function xmlXPtrWrapLocationSet(val: xmlLocationSetPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
procedure xmlXPtrLocationSetDel(cur: xmlLocationSetPtr; val: xmlXPathObjectPtr); EXTDECL; external xml2lib;
procedure xmlXPtrLocationSetRemove(cur: xmlLocationSetPtr; val: cint); EXTDECL; external xml2lib;

(*
 * Functions.
 *)
function xmlXPtrNewContext(doc: xmlDocPtr; here, origin: xmlNodePtr): xmlXPathContextPtr; EXTDECL; external xml2lib;
function xmlXPtrEval(str: xmlCharPtr; ctx: xmlXPathContextPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
procedure xmlXPtrRangeToFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
function xmlXPtrBuildNodeList(obj: xmlXPathObjectPtr): xmlNodePtr; EXTDECL; external xml2lib;
procedure xmlXPtrEvalRangePredicate(ctxt: xmlXPathParserContextPtr); EXTDECL; external xml2lib;
{$ENDIF}

{$ENDIF} (* LIBXML_XPTR_ENABLED *)
