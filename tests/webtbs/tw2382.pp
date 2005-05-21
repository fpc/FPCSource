var
  sa : array[0..2] of char;
  s: string;
begin
  sa := '';
  s := sa;
  if length(s) <> 0 then
    begin
      writeln('error 0');
      halt(1);
    end;

  sa := 'a';
  s := sa;
  if length(s) <> 1 then
    begin
      writeln('error 1');
      halt(1);
    end;

  sa := 'ab';
  s := sa;
  if length(s) <> 2 then
    begin
      writeln('error 2');
      halt(1);
    end;

  sa := 'abc';
  // check for possible overflow in assignment
  s[4] := '1';
  s := sa;
  if length(s) <> 3 then
    begin
      writeln('error 3');
      halt(1);
    end;

  if s[4] <> '1' then
    begin
      writeln('error 4');
      halt(1);
    end;
end.
