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
function xmlXPtrLocationSetCreate(val: xmlXPathObjectPtr): xmlLocationSetPtr; XMLCALL; XMLPUBFUN;
procedure xmlXPtrLocationSetCreate(obj: xmlLocationSetPtr); XMLCALL; XMLPUBFUN;
function xmlXPtrLocationSetMerge(val1, val2: xmlLocationSetPtr): xmlLocationSetPtr; XMLCALL; XMLPUBFUN;
function xmlXPtrNewRange(start: xmlNodePtr; startindex: cint; _end: xmlNodePtr; endindex: cint): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function xmlXPtrNewRangePoints(start, _end: xmlXPathObjectPtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function xmlXPtrNewRangeNodePoint(start: xmlNodePtr; _end: xmlXPathObjectPtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function xmlXPtrNewRangePointNode(start: xmlXPathObjectPtr; _end: xmlNodePtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function xmlXPtrNewRangeNodes(start: xmlNodePtr; _end: xmlNodePtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function xmlXPtrNewLocationSetNodes(start: xmlNodePtr; _end: xmlNodePtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function xmlXPtrNewLocationSetNodeSet(_set: xmlNodeSetPtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function xmlXPtrNewRangeNodeObject(start: xmlNodePtr; _end: xmlXPathObjectPtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function xmlXPtrNewCollapsedRange(start: xmlNodePtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
procedure xmlXPtrLocationSetAdd(cur: xmlLocationSetPtr; val: xmlXPathObjectPtr); XMLCALL; XMLPUBFUN;
function xmlXPtrWrapLocationSet(val: xmlLocationSetPtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
procedure xmlXPtrLocationSetDel(cur: xmlLocationSetPtr; val: xmlXPathObjectPtr); XMLCALL; XMLPUBFUN;
procedure xmlXPtrLocationSetRemove(cur: xmlLocationSetPtr; val: cint); XMLCALL; XMLPUBFUN;

(*
 * Functions.
 *)
function xmlXPtrNewContext(doc: xmlDocPtr; here, origin: xmlNodePtr): xmlXPathContextPtr; XMLCALL; XMLPUBFUN;
function xmlXPtrEval(str: xmlCharPtr; ctx: xmlXPathContextPtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
procedure xmlXPtrRangeToFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
function xmlXPtrBuildNodeList(obj: xmlXPathObjectPtr): xmlNodePtr; XMLCALL; XMLPUBFUN;
procedure xmlXPtrEvalRangePredicate(ctxt: xmlXPathParserContextPtr); XMLCALL; XMLPUBFUN;
{$ENDIF}

{$ENDIF} (* LIBXML_XPTR_ENABLED *)
