{ %fail }

{$mode objfpc}
type
   TMainForm = class
   end;

function FreeFPgtkObjects (Data:pointer) : longbool; Cdecl;

begin
  With (Data as TMainForm) do // <<--- this is the one
    ;
end;

begin
end.
