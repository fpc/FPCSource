program crtkeys;
uses
  crt;
var
  Ch, Ex: Char;
  Done: Boolean;
begin
  Done := False;
  repeat
    Ch := ReadKey;
    if Ch = #0 then
      Ex := ReadKey
    else
      Ex := #0;
    Writeln(Ord(Ch), ' ', Ord(Ex));
    if Ch = 'q' then
      Done := True;
  until Done;
end.
