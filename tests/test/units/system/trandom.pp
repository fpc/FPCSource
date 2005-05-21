{ %INTERACTIVE }
program test_random;

var
 i: integer;
begin
  randomize;
  WriteLn('Random Values II:');
  for i:=0 to 100 do
   begin
     WriteLn(Random(100));
   end;
  randomize;
  WriteLn('Random Values I:');
  for i:=0 to 100 do
   begin
     WriteLn(Random(100));
   end;
end.
