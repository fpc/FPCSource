{ %norun }
{ %needlibrary }
{ %target=linux }

library tw36880a;

{$mode objfpc}

uses
  cthreads;

threadvar
  tvCounter: Integer;

// Increments a threadvar of the calling thread and returns its new value
function TouchThreadVar: Integer; cdecl;

begin
  Inc(tvCounter);
  Result:=tvCounter;
end;


exports
  TouchThreadVar;

end.
