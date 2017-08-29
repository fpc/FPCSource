{ %NORUN }

program tb0630;

uses
  typinfo;

type
  TTest = record
    test: procedure(aTest: array of LongInt);
  end;

  TTestArray = array[0..1] of TTest;

var
  ti: PTypeInfo;
begin
  ti := TypeInfo(TTest);
end.
