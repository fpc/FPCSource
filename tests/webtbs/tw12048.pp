{ %opt=-gh }

{$mode objfpc}

program DynArrBug;

uses Types;

function GetDynArray: TStringDynArray;
begin
  SetLength( GetDynArray, 16 );
end;
  
var
  darr: array[1..1] of TStringDynArray;
begin
  keepreleased:=true;
  darr[1] := GetDynArray();
  Finalize( darr[1] );
  if pointer(darr[1])<>nil then
    halt(1);
  darr[1] := GetDynArray();
end.

