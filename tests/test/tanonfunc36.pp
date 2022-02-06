program tanonfunc36;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test loops / arrays }

type
  TIntFunc = reference to function: Integer;

function CreateFunc(i: Integer): TIntFunc;
begin
  Result := function: Integer
  begin
    Result := i;
  end;
end;

var
  F: TIntFunc;
  Funcs: array of TIntFunc;
  Acc, i: Integer;
begin
  SetLength(Funcs, 10);
  for i := Low(Funcs) to High(Funcs) do
    Funcs[i] := CreateFunc(i);
  Acc := 0;
  for F in Funcs do
    Inc(Acc, F());
  if Acc <> 45 then
    halt(acc);
end.

