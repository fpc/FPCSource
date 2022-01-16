program trtti10;

{$MODE DELPHI}

uses
  TypInfo;

type
  TFoo = record
  end;

begin
  if GetTypeData(TypeInfo(TFoo)).RecInitInfo = nil then
    Halt(1);
end.
