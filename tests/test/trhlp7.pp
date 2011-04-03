{ the size of a record helper is equivalent to that of a pointer }
program trhlp7;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest = packed record
    s: String;
    i32: Integer;
    b: Boolean;
    i64: Int64;
  end;

  TTestHelper = record helper for TTest
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
