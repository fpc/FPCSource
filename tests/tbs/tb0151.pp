
{$ifdef fpc}{$mode tp}{$endif}

{$ifdef ENDIAN_BIG}
var
  i : longint;
  j : word;
begin
  j:=5;
  i:=-1;
  byte(i):=j;
  writeln('i: ',i,' (should be -251)');
  if i<>-251 then
   halt(1);
end.
{$else}
begin
end.
{$endif}
