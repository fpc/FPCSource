{ %fail }

{$mode objfpc}

program Project1;
type
tmyobj=object
            a:integer;
            function getsize:integer;
        end;
function tmyobj.getsize:integer;
begin
     result:=sizeof(self);
end;
begin
  writeln(tmyobj.getsize);
end.
