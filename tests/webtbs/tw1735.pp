{$mode delphi}
function UpperCase(S: string): string;
var
  i: integer;
begin
  for i := 1 to length(s) do
    case s[i] of
      'a' .. 'z': Dec(s[i], 32);
//      'a' .. 'z': s[i] := chr(ord(s[i])-32);
    end;
  result := s;
end;

var
  s: string;
begin
  s := 'abcdef';
  writeln(uppercase(s));
  writeln(s);
  if (s <> 'abcdef') then
    halt(1);
end.
