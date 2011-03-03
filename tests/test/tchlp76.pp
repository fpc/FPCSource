{ helpers may introduce new default properties }
program tchlp76;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class

  end;

  TFooHelper = class helper for TFoo
    function GetTest(aIndex: Integer): Integer;
    property Test[Index: Integer]: Integer read GetTest; default;
  end;

function TFooHelper.GetTest(aIndex: Integer): Integer;
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
