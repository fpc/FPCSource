{ %FAIL }

type
  r=record
    a :longint;
  end;
var
  w : ^r;
begin
  if w^<>$1111 then
   writeln;
end.
