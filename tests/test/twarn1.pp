program Project1;

{$mode objfpc}{$H+}

var
   i: integer platform;
begin
   {$warn symbol_platform off}
   i := 1;
   {$warn symbol_platform off}
   i := 2;
end.
