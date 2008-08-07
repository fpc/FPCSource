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
  xmlListDeallocator = procedure(lk: xmlLinkPtr); cdecl;

(**
 * xmlListDataCompare:
 * @data0: the first data
 * @data1: the second data
 *
 * Callback function used to compare 2 data.
 *
 * Returns 0 is equality, -1 or 1 otherwise depending on the ordering.
 *)
  xmlListDataCompare = function(data0, data1: pointer): cint; cdecl;

(**
 * xmlListWalker:
 * @data: the data found in the list
 * @user: extra user provided data to the walker
 *
 * Callback function used when walking a list with xmlListWalk().
 *
 * Returns 0 to stop walking the list, 1 otherwise.
 *)
  xmlListWalker = function(data, user: pointer): cint; cdecl;
{$ENDIF}

{$IFDEF FUNCTION}
(* Creation/Deletion *)
function xmlListCreate(deallocator: xmlListDeallocator; compare: xmlListDataCompare): xmlListPtr; cdecl; external;
procedure xmlListDelete(l: xmlListPtr); cdecl; external;

(* Basic Operators *)
function xmlListSearch(l: xmlListPtr; data: pointer): pointer; cdecl; external;
function xmlListReverseSearch(l: xmlListPtr; data: pointer): pointer; cdecl; external;
function xmlListInsert(l: xmlListPtr; data: pointer): cint; cdecl; external;
function xmlListAppend(l: xmlListPtr; data: pointer): cint; cdecl; external;
function xmlListRemoveFirst(l: xmlListPtr; data: pointer): cint; cdecl; external;
function xmlListRemoveLast(l: xmlListPtr; data: pointer): cint; cdecl; external;
function xmlListRemoveAll(l: xmlListPtr; data: pointer): cint; cdecl; external;
procedure xmlListClear(l: xmlListPtr); cdecl; external;
function xmlListEmpty(l: xmlListPtr): cint; cdecl; external;
function xmlListFront(l: xmlListPtr): xmlLinkPtr; cdecl; external;
function xmlListEnd(l: xmlListPtr): xmlLinkPtr; cdecl; external;
function xmlListSize(l: xmlListPtr): cint; cdecl; external;
procedure xmlListPopFront(l: xmlListPtr); cdecl; external;
procedure xmlListPopBack(l: xmlListPtr); cdecl; external;
function xmlListPushFront(l: xmlListPtr; data: pointer): cint; cdecl; external;
function xmlListPushBack(l: xmlListPtr; data: pointer): cint; cdecl; external;

(* Advanced Operators *)
procedure xmlListReverse(l: xmlListPtr); cdecl; external;
procedure xmlListSort(l: xmlListPtr); cdecl; external;
procedure xmlListWalk(l: xmlListPtr; walker: xmlListWalker; user: pointer); cdecl; external;
procedure xmlListReverseWalk(l: xmlListPtr; walker: xmlListWalker; user: pointer); cdecl; external;
procedure xmlListMerge(l1, l2: xmlListPtr); cdecl; external;
function xmlListDup(old: xmlListPtr): xmlListPtr; cdecl; external;
function xmlListDup(cur, old: xmlListPtr): cint; cdecl; external;

(* Link operators *)
function xmlLinkGetData(lk: xmlLinkPtr): cint; cdecl; external;
{$ENDIF}