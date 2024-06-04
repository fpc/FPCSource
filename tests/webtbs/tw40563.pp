program tw40563;
uses sysutils;
var s:string[20];
  r:shortstring;
  x: Char;
begin
  r:='';
  s:=inttostr(12345);
  for x in s do begin
    r:=r+x;
    write(ord(x):4);
  end;
  writeln;
  if r<>s then
    Halt(1);
end.

