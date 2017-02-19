program tmoperator10;

{$MODE DELPHI}

uses
  TypInfo;

type
  TFoo = record
  private
    class operator Initialize(var aFoo: TFoo);
  end;
  TFooArray = array of TFoo;

class operator TFoo.Initialize(var aFoo: TFoo);
begin
end;

begin
  if GetTypeData(TypeInfo(TFooArray))^.ElType = nil then
    Halt(1);
  if GetTypeData(TypeInfo(TFooArray))^.ElType2 = nil then
    Halt(2);
end. 