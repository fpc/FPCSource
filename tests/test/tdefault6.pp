{ %FAIL }

{ In non-Delphi modes unsupported types like TextFile are not allowed inside
  records and objects - case 1 }
program tdefault6;

type
  TTestRecord = record
    f: TextFile;
  end;

var
  trec: TTestRecord;
begin
  trec := Default(TTestRecord);
end.
