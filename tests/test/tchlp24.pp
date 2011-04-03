{ published methods of class helpers are not accessible through the extended
  class' RTTI }
program tchlp24;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
{$M+}
  TTest = class
  end;
{$M-}

{$M+}
  TTestHelper = class helper for TTest
  published
    function Test: Integer;
  end;
{$M-}

function TTestHelper.Test: Integer;
begin
  Result := 1;
end;

var
  f: TTest;
  res: Pointer;
begin
  f := TTest.Create;
  res := f.MethodAddress('Test');
{$ifdef fpc}
  Writeln('Address of TTest.Test: ', PtrInt(res));
{$else}
  Writeln('Address of TTest.Test: ', NativeInt(res));
{$endif}
  if res <> Nil then
    Halt(1);
  Writeln('ok');
end.
