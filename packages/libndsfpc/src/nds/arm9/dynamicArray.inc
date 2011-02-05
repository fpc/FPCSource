{$ifdef NDS_INTERFACE}
type
  DynamicArray = record 
    data: ppointer;  // pointer to array of void pointers
    cur_size: cuint; // currently allocated size of the array 
  end;
  TDynamicArray = DynamicArray;
  PDynamicArray = ^TDynamicArray;


function DynamicArrayInit(v: PDynamicArray; initialSize: cuint): pointer; cdecl; external;
procedure DynamicArrayDelete(v: PDynamicArray); cdecl; external;
function DynamicArrayGet(v: PDynamicArray; index: cuint): pointer; cdecl; external;
function DynamicArraySet(v: PDynamicArray; index: cuint; item: pointer): cbool; cdecl; external;
{$endif NDS_INTERFACE}

