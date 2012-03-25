{ %FAIL }

{ Typed files are not allowed for Default }
program tdefault3;

type
  TFileLongInt = file of LongInt;

var
  t: TFileLongInt;
begin
  t := Default(TFileLongInt);
end.
