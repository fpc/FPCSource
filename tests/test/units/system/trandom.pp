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
 Revision 1.3  2002-09-07 15:40:56  peter
   * old logs removed and tabs fixed

}
