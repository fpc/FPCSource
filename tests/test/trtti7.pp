program trtti7;

{$mode delphi}

uses
  typinfo;

type
  // RTTI for this type will have 1251 codepage
  T1251String = type AnsiString(1251);

var
  S: T1251String = 'Test';
  Info: PTypeInfo;
  Data: PTypeData;
begin
  // change runtime string codepage to make it different from RTTI value
  SetCodePage(RawByteString(S), 866, False);
  // check if runtime codepage is 866
  if StringCodePage(S) <> 866 then
    halt(1);
  // check that it is an ansistring in RTTI
  Info := TypeInfo(S);
  WriteLn(Info^.Kind);
  if Info^.Kind <> tkAString then
    halt(2);
  // check that compiletime RTTI is 1251
  Data := GetTypeData(Info);
  if Data^.CodePage <> 1251 then
    halt(3);
end.
