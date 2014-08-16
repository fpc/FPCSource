{ %OPT=-Oodfa -Sew -vw }
{ %norun }

{ this test test needs dynamic dfa to work properly,
  this is a reminder so it will not be forgotten }
var
  j,i : longint;

begin
  j:=paramcount;
  if j=1 then
    i:=1;
  writeln;
  if j=1 then
    writeln(i);
end.
