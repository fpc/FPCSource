{ %FAIL }

{ untyped files are not allowed for Default }
program tdefault4;

type
  TUntypedFile = file;

var
  t: TUntypedFile;
begin
  t := Default(TUntypedFile);
end.
