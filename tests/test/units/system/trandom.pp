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

{
 $Log$
 Revision 1.2  2001-07-14 04:24:40  carl
 - remove graph unit usage

}
