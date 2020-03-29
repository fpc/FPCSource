program tb0657;

{$mode objfpc}

uses
  fgl;

type
  TIntList = specialize TFPGList<LongInt>;

const
  c = 3;

var
  l: TIntList;
  i: LongInt;
begin
  l := TIntList.Create;
  try
    for i := 0 to c do
      l.Add(i);

    for i := 0 to l.Count - 1 do
      if l.List^[i] <> i then
        Halt(i + 1);
  finally
    l.Free;
  end;
end.
