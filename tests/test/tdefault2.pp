{ %FAIL }

{ Text files are not allowed for Default }
program tdefault2;

var
  t: TextFile;
begin
  t := Default(TextFile);
end.
