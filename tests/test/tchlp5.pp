{ the size of a class helper is equivalent to that of a pointer }
program tchlp5;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest = class
    s: String;
    i32: Integer;
    b: Boolean;
    i64: Int64;
  end;

  TTestHelper = class helper for TTest
  end;

var
  res: Integer;
begin
  res := SizeOf(TTestHelper);
  Writeln('SizeOf(TTest): ', SizeOf(TTest));
  Writeln('SizeOf(TTestHelper): ', res);
  if res <> SizeOf(Pointer) then
    Halt(1);
  Writeln('ok');
end.
