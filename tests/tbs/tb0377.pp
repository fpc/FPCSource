{$ifdef fpc}{$mode tp}{$endif}

{$ifdef ENDIAN_BIG}
begin
end.
{$else}
var
  i : longint;
  j : word;
begin
  j:=5;
  i:=-1;
  { this is allowed in tp7 }
  byte(i):=j;
  writeln('i: ',i,' (should be -251)');
  if i<>-251 then
   halt(1);
end.
{$endif}
