{ %VERSION=1.1 }
{$mode objfpc}
var
   a : longint absolute 0;

begin
   if @a<>nil then
     halt(1);
end.
