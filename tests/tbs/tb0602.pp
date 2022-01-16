{ This test explicity generates
  dec(w,2) with w=1 of unsigned type word,
  this generates range check error.
  Thus, we need an explicit $R- }
{$R-}
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
