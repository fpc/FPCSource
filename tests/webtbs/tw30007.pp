program project6;

{$r+}

function LinesToText(Lines: array of String): String;
begin
end;

var
  SomeLines: array of String;
begin
  SetLength(SomeLines,1);
  LinesToText(SomeLines); // <-- ok
  SetLength(SomeLines,0);
  LinesToText(SomeLines); // <-- range error
end.
