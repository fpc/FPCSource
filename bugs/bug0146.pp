procedure myfunction(var t : array of char);
begin
  writeln(sizeof(t)); { should be 51 }
end;

var
  mycharstring : array[0..50] of char;

begin
  myfunction(mycharstring);
end.
