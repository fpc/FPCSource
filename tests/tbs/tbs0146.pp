
procedure myfunction(var t : array of char);
begin
  writeln(sizeof(t)); { should be 51 }
  if sizeof(t)<>51 then halt(1);
end;

var
  mycharstring : array[0..50] of char;

begin
  myfunction(mycharstring);
  if sizeof(mycharstring)<>51 then halt(1);
end.
