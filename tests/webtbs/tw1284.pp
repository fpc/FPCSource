{ %version=1.1 }
{ %OPT=-CO- -Seh -vh }

{$mode objfpc}
 function f(p: word): boolean;
 begin
  result := (p mod 10 = 0);
 end;
begin
 f(0)
end.
