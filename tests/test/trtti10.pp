program trtti10;

{$MODE DELPHI}

uses
  TypInfo;

type
  TFoo = record
  end;

begin
  if GetTypeData(TypeInfo(TFoo)).RecInitTable = nil then
    Halt(1);
end.
