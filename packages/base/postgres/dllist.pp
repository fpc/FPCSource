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

{ Macro translated }
Function  DLE_VAL(elem : PDlelem) : pointer;

implementation



Function DLE_VAL(elem : PDlelem) : pointer;
begin
  DLE_VAL:=elem^.dle_val
end;

end.
  $Log$
  Revision 1.3  2004-11-21 16:33:55  peter
    * external fixes

  Revision 1.2  2002/09/07 15:42:53  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:54:56  peter
    * splitted to base and extra

}
