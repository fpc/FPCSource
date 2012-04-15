{ %FAIL }

{ In non-Delphi modes unsupported types like TextFile are not allowed inside
  records and objects - case 2 }
program tdefault7;

type
  TTestObject = object
    f: TextFile;
  end;

var
  tobj: TTestObject;
begin
  tobj := Default(TTestObject);
end.
