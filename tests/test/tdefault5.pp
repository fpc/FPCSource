{ %NORUN }

{ In Delphi mode unsupported types like TextFile are ignored inside records
  and objects }
program tdefault5;

{$mode delphi}

type
  TTestRecord = record
    f: TextFile;
  end;

  TTestObject = object
    f: TextFile;
  end;

var
  trec: TTestRecord;
  tobj: TTestObject;
begin
  trec := Default(TTestRecord);
  tobj := Default(TTestObject);
end.
