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
 $Log: trandom.pp,v $
 Revision 1.4  2005/02/14 17:13:37  peter
   * truncate log

}
