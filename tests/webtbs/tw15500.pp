{$mode objfpc}{$H+}

uses
  SysUtils, fgl;

type
  TMap = specialize TFPGMap<String, String>;

var
  map: TMap;

const
  strings: array[0..4] of string =
    ('BaseDir', 'OutputDir', 'ModelName', 'Unit', 'SimSoftware');
var
  i, j: integer;
begin
  map := TMap.Create;
  map.Sorted := True;

  for i := low(strings) to high(strings) do
    map.add(strings[i], inttostr(i));

  for i := low(strings) to high(strings) do
  begin
    if not map.Find(strings[i], j) then
    begin
      writeln('not found: "', strings[i], '"');
      halt(1);
    end;
    if map.data[j] <> inttostr(i) then
    begin
      writeln('not matched: ', map.data[j], ' <> ', i);
      halt(1);
    end;
  end;

  map.Free;
end.

