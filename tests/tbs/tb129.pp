{ Old file: tbs0146.pp }
{ no sizeof() for var arrays and the size is pushed incorrect OK 0.99.7 (PFV) }


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
