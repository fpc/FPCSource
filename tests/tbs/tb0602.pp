{$ifdef fpc}
{$mode delphi}
{$endif}

procedure test;
var
  w: word;
  arr: array[0..1] of word;
begin
  arr[1]:=2;
  w:=1;
  dec(w,arr[w]);
  if cardinal(w)<>$ffff then
    halt(1);
end;

begin
  test;
end.
