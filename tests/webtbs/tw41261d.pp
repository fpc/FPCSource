program Project1;
{$R+}
const
  INTCHARS = ['0' .. '9'];
  LETTERCHARS = ['a' .. 'f', 'A' .. 'F'];
  HEXCHARS = LETTERCHARS;

var
  c : char;
begin
  c:='g';
  if (c in INTCHARS + LETTERCHARS) then
    begin
      writeln('Error g is not a valid hex char');
      halt(1);
    end
  else
    writeln('OK: g is not a hex char');
end.

