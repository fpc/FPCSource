unit dllist;

interface

{$linklib pq}

{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PByte     = ^Byte;
  PWord     = ^Word;
  PINteger  = ^Integer;
  PCardinal = ^Cardinal;
  PReal     = ^Real;
  PDouble   = ^Double;

{ next element }
{ previous element }
{ value of the element }
{ what list this element is in }

type
   { Pointer types } 
   PDllist= ^TDllist;
   PDlelem= ^TDlelem;

   TDlelem = record
        dle_next : PDlelem;
        dle_prev : PDlElem;
        dle_val : pointer;
        dle_list : PDllist;
     end;

   TDllist = record
        dll_head : PDlelem;
        dll_tail : PDlelem;
     end;

function  DLNewList:PDllist; cdecl;
procedure DLFreeList(_para1:PDllist); cdecl;
function  DLNewElem(val : pointer) :PDlelem; cdecl;
procedure DLFreeElem(_para1:PDlelem); cdecl;
function  DLGetHead(_para1:PDllist):PDlelem; cdecl;
function  DLGetTail(_para1:PDllist):PDlelem; cdecl;
function  DLRemTail(l:PDllist):PDlelem; cdecl;
function  DLGetPred(_para1:PDlelem):PDlelem; cdecl;
function  DLGetSucc(_para1:PDlelem):PDlelem; cdecl;
procedure DLRemove(_para1:PDlelem); cdecl;
procedure DLAddHead(list:PDllist; node:PDlelem);cdecl;
procedure DLAddTail(list:PDllist; node:PDlelem);cdecl;
function  DLRemHead(list:PDllist):PDlelem;cdecl;

{ Macro translated }
Function  DLE_VAL(elem : PDlelem) : pointer; 

implementation


function  DLNewList:PDllist;cdecl; external;
procedure DLFreeList(_para1:PDllist);cdecl; external;
function  DLNewElem(val : pointer) :PDlelem;cdecl;external;
procedure DLFreeElem(_para1:PDlelem);cdecl; external;
function  DLGetHead(_para1:PDllist):PDlelem;cdecl; external;
function  DLGetTail(_para1:PDllist):PDlelem;cdecl; external;
function  DLRemTail(l:PDllist):PDlelem;cdecl; external;
function  DLGetPred(_para1:PDlelem):PDlelem;cdecl; external;
function  DLGetSucc(_para1:PDlelem):PDlelem;cdecl; external;
procedure DLRemove(_para1:PDlelem);cdecl; external;
procedure DLAddHead(list:PDllist; node:PDlelem);cdecl; external;
procedure DLAddTail(list:PDllist; node:PDlelem);cdecl; external;
function  DLRemHead(list:PDllist):PDlelem;cdecl; external;

Function DLE_VAL(elem : PDlelem) : pointer;
begin
  DLE_VAL:=elem^.dle_val
end;

end.
  $Log$
  Revision 1.1  2002-01-29 17:54:56  peter
    * splitted to base and extra

  Revision 1.2  2000/07/13 11:33:30  michael
  + removed logs
 
}
