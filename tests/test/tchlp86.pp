{ %FAIL } {???}

program tchlp86;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TFoo = class
    class var
      Test: Integer;
  end;

  TFooHelper = class helper for TFoo
    class constructor Create;
  end;

class constructor TFooHelper.Create;
begin
  TFoo.Test := 42;
end;

begin
  Writeln('TFoo.Test: ', TFoo.Test);
  if TFoo.Test <> 42 then
    Halt(1);
  Writeln('ok');
end.
