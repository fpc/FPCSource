{ %fail }

{$mode fpc}

var c: char;
    p: pchar;
begin
  { Don't allow this in fpc modes, in delphi it should be allowed }
  p := pchar(c);
end.
