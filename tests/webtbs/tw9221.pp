{%target=win32,win64}

var
  s: widestring;
begin
  winwidestringalloc:=false;
  s:='1234';
  SetLength(s, 10);
  if Length(s) <> 10 then begin
    writeln('Test failed!');
    Halt(1);
  end;
end.
