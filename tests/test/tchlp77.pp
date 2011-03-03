{ helpers may override existing default properties }
program tchlp77;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class
  private
    function GetTest(aIndex: Integer): Integer;
  public
    property Test[Index: Integer]: Integer read GetTest; default;
  end;

  TFooHelper = class helper for TFoo
    function GetTest(aIndex: Integer): Integer;
    property Test[Index: Integer]: Integer read GetTest; default;
  end;

function TFoo.GetTest(aIndex: Integer): Integer;
begin
  Result := - aIndex;
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
