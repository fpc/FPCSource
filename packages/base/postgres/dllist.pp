unit dllist;

interface

{$linklib pq}

{$i dllisttypes.inc}

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

// This function is also defined in DllistDyn!
Function DLE_VAL(elem : PDlelem) : pointer;
begin
  DLE_VAL:=elem^.dle_val
end;

end.
  $Log$
  Revision 1.4  2005-01-24 10:58:26  michael
  + Dynamic library implementation by Joost van der Sluis

  Revision 1.3  2004/11/21 16:33:55  peter
    * external fixes

  Revision 1.2  2002/09/07 15:42:53  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:54:56  peter
    * splitted to base and extra

}
