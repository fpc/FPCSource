{ %fail }

{$mode fpc}

{ This is not allowed in normal fpc mode }
resourcestring
  s = 'OK';
begin
  writeln(s);
end.
