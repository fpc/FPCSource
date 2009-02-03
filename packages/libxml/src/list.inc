(*
 * Summary: lists interfaces
 * Description: this module implement the list support used in 
 * various place in the library.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Gary Pennington <Gary.Pennington@uk.sun.com>
 *)

{$IFDEF POINTER}
  xmlLinkPtr = ^xmlLink;
  xmlListPtr = ^xmlList;
{$ENDIF}

{$IFDEF TYPE}
  xmlLink = record end;
  xmlList = record end;

(**
 * xmlListDeallocator:
 * @lk:  the data to deallocate
 *
 * Callback function used to free data from a list.
 *)
  xmlListDeallocator = procedure(lk: xmlLinkPtr); EXTDECL;

(**
 * xmlListDataCompare:
 * @data0: the first data
 * @data1: the second data
 *
 * Callback function used to compare 2 data.
 *
 * Returns 0 is equality, -1 or 1 otherwise depending on the ordering.
 *)
  xmlListDataCompare = function(data0, data1: pointer): cint; EXTDECL;

(**
 * xmlListWalker:
 * @data: the data found in the list
 * @user: extra user provided data to the walker
 *
 * Callback function used when walking a list with xmlListWalk().
 *
 * Returns 0 to stop walking the list, 1 otherwise.
 *)
  xmlListWalker = function(data, user: pointer): cint; EXTDECL;
{$ENDIF}

{$IFDEF FUNCTION}
(* Creation/Deletion *)
function xmlListCreate(deallocator: xmlListDeallocator; compare: xmlListDataCompare): xmlListPtr; EXTDECL; external xml2lib;
procedure xmlListDelete(l: xmlListPtr); EXTDECL; external xml2lib;

(* Basic Operators *)
function xmlListSearch(l: xmlListPtr; data: pointer): pointer; EXTDECL; external xml2lib;
function xmlListReverseSearch(l: xmlListPtr; data: pointer): pointer; EXTDECL; external xml2lib;
function xmlListInsert(l: xmlListPtr; data: pointer): cint; EXTDECL; external xml2lib;
function xmlListAppend(l: xmlListPtr; data: pointer): cint; EXTDECL; external xml2lib;
function xmlListRemoveFirst(l: xmlListPtr; data: pointer): cint; EXTDECL; external xml2lib;
function xmlListRemoveLast(l: xmlListPtr; data: pointer): cint; EXTDECL; external xml2lib;
function xmlListRemoveAll(l: xmlListPtr; data: pointer): cint; EXTDECL; external xml2lib;
procedure xmlListClear(l: xmlListPtr); EXTDECL; external xml2lib;
function xmlListEmpty(l: xmlListPtr): cint; EXTDECL; external xml2lib;
function xmlListFront(l: xmlListPtr): xmlLinkPtr; EXTDECL; external xml2lib;
function xmlListEnd(l: xmlListPtr): xmlLinkPtr; EXTDECL; external xml2lib;
function xmlListSize(l: xmlListPtr): cint; EXTDECL; external xml2lib;
procedure xmlListPopFront(l: xmlListPtr); EXTDECL; external xml2lib;
procedure xmlListPopBack(l: xmlListPtr); EXTDECL; external xml2lib;
function xmlListPushFront(l: xmlListPtr; data: pointer): cint; EXTDECL; external xml2lib;
function xmlListPushBack(l: xmlListPtr; data: pointer): cint; EXTDECL; external xml2lib;

(* Advanced Operators *)
procedure xmlListReverse(l: xmlListPtr); EXTDECL; external xml2lib;
procedure xmlListSort(l: xmlListPtr); EXTDECL; external xml2lib;
procedure xmlListWalk(l: xmlListPtr; walker: xmlListWalker; user: pointer); EXTDECL; external xml2lib;
procedure xmlListReverseWalk(l: xmlListPtr; walker: xmlListWalker; user: pointer); EXTDECL; external xml2lib;
procedure xmlListMerge(l1, l2: xmlListPtr); EXTDECL; external xml2lib;
function xmlListDup(old: xmlListPtr): xmlListPtr; EXTDECL; external xml2lib;
function xmlListDup(cur, old: xmlListPtr): cint; EXTDECL; external xml2lib;

(* Link operators *)
function xmlLinkGetData(lk: xmlLinkPtr): cint; EXTDECL; external xml2lib;
{$ENDIF}