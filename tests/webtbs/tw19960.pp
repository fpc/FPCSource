program extracttest;

{$mode objfpc}{$H+}
{$apptype console}

uses
  fgl;

type
  TIntegerList = specialize TFPGList<Integer>;

procedure PrintList(aList: TIntegerList);
var
  i: Integer;
begin
  for i := 0 to aList.Count - 1 do
    Write(#9, aList[i]);
  Writeln;
end;

var
  list: TIntegerList;
  i, j: Integer;
begin
  list := TIntegerList.Create;
  try
    for i := 0 to 5 do
      list.Add(i);

    while list.Count > 0 do begin
      if 6-list.Count<>list.Extract(list.First) then
        halt(1);
      PrintList(list);
    end;

    list.Clear;
    Writeln;

    for i := 0 to 5 do
      list.Add(i);

    for i := 2 to 4 do begin
      if list.Extract(i)<>i then
        halt(1);
      PrintList(list);
    end;
  finally
    list.Free;
  end;
  writeln('ok');
end.

