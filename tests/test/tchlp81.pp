{ helpers may introduce new default properties (includes default properties
  introudced by the helper's parent) }
program tchlp81;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class

  end;

  TObjectHelper = class helper for TObject
    function GetTest(aIndex: Integer): Integer;
    property Test[Index: Integer]: Integer read GetTest; default;
  end;

  TFooHelper = class helper(TObjectHelper) for TFoo
  end;

function TObjectHelper.GetTest(aIndex: Integer): Integer;
begin
  Result := aIndex;
end;

var
  f: TFoo;
  res: Integer;
begin
  f := TFoo.Create;
  res := f[3];
  Writeln('value: ', res);
  if res <> 3 then
    Halt(1);
  Writeln('ok');
end.
