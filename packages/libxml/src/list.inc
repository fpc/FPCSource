(*
 * Summary: lists interfaces
 * Description: this module implement the list support used in 
 * various place in the library.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Gary Pennington <Gary.Pennington@uk.sun.com>
 *)

{$IFDEF TYPE}
  xmlLink = record end;
  xmlList = record end;

(**
 * xmlListDeallocator:
 * @lk:  the data to deallocate
 *
 * Callback function used to free data from a list.
 *)
  xmlListDeallocator = procedure(lk: xmlLinkPtr); XMLCALL;

(**
 * xmlListDataCompare:
 * @data0: the first data
 * @data1: the second data
 *
 * Callback function used to compare 2 data.
 *
 * Returns 0 is equality, -1 or 1 otherwise depending on the ordering.
 *)
  xmlListDataCompare = function(data0, data1: pointer): cint; XMLCALL;

(**
 * xmlListWalker:
 * @data: the data found in the list
 * @user: extra user provided data to the walker
 *
 * Callback function used when walking a list with xmlListWalk().
 *
 * Returns 0 to stop walking the list, 1 otherwise.
 *)
  xmlListWalker = function(data, user: pointer): cint; XMLCALL;
{$ENDIF}

{$IFDEF FUNCTION}
(* Creation/Deletion *)
function xmlListCreate(deallocator: xmlListDeallocator; compare: xmlListDataCompare): xmlListPtr; XMLCALL; XMLPUBFUN;
procedure xmlListDelete(l: xmlListPtr); XMLCALL; XMLPUBFUN;

(* Basic Operators *)
function xmlListSearch(l: xmlListPtr; data: pointer): pointer; XMLCALL; XMLPUBFUN;
function xmlListReverseSearch(l: xmlListPtr; data: pointer): pointer; XMLCALL; XMLPUBFUN;
function xmlListInsert(l: xmlListPtr; data: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlListAppend(l: xmlListPtr; data: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlListRemoveFirst(l: xmlListPtr; data: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlListRemoveLast(l: xmlListPtr; data: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlListRemoveAll(l: xmlListPtr; data: pointer): cint; XMLCALL; XMLPUBFUN;
procedure xmlListClear(l: xmlListPtr); XMLCALL; XMLPUBFUN;
function xmlListEmpty(l: xmlListPtr): cint; XMLCALL; XMLPUBFUN;
function xmlListFront(l: xmlListPtr): xmlLinkPtr; XMLCALL; XMLPUBFUN;
function xmlListEnd(l: xmlListPtr): xmlLinkPtr; XMLCALL; XMLPUBFUN;
function xmlListSize(l: xmlListPtr): cint; XMLCALL; XMLPUBFUN;
procedure xmlListPopFront(l: xmlListPtr); XMLCALL; XMLPUBFUN;
procedure xmlListPopBack(l: xmlListPtr); XMLCALL; XMLPUBFUN;
function xmlListPushFront(l: xmlListPtr; data: pointer): cint; XMLCALL; XMLPUBFUN;
function xmlListPushBack(l: xmlListPtr; data: pointer): cint; XMLCALL; XMLPUBFUN;

(* Advanced Operators *)
procedure xmlListReverse(l: xmlListPtr); XMLCALL; XMLPUBFUN;
procedure xmlListSort(l: xmlListPtr); XMLCALL; XMLPUBFUN;
procedure xmlListWalk(l: xmlListPtr; walker: xmlListWalker; user: pointer); XMLCALL; XMLPUBFUN;
procedure xmlListReverseWalk(l: xmlListPtr; walker: xmlListWalker; user: pointer); XMLCALL; XMLPUBFUN;
procedure xmlListMerge(l1, l2: xmlListPtr); XMLCALL; XMLPUBFUN;
function xmlListDup(old: xmlListPtr): xmlListPtr; XMLCALL; XMLPUBFUN;
function xmlListDup(cur, old: xmlListPtr): cint; XMLCALL; XMLPUBFUN;

(* Link operators *)
function xmlLinkGetData(lk: xmlLinkPtr): cint; XMLCALL; XMLPUBFUN;
{$ENDIF}