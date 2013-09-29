program trtti8;

{$mode delphi}

uses
  typinfo;

type
  TColor = (red, green, blue);
  TFirstArr = array[0..3] of Integer;
  TArr = array[TColor] of TFirstArr;
var
  Info: PTypeInfo;
  Data: PTypeData;
begin
  Info := TypeInfo(TArr);
  if Info^.Kind <> tkArray then
    halt(1);
  Data := GetTypeData(Info);
  if Data^.ArrayData.Size <> 12 * SizeOf(Integer) then
    halt(2);
  if Data^.ArrayData.ElCount <> 12 then
    halt(3);
  if Data^.ArrayData.ElType <> TypeInfo(Integer) then
    halt(4);
  if Data^.ArrayData.DimCount <> 2 then
    halt(5);
  if Data^.ArrayData.Dims[0] <> TypeInfo(TColor) then
    halt(6)
end.