{ size of a record helper is the size of a pointer }
program tchlp79;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTestRecord = record
    i: Integer;
    j: Integer;
  end;

  TTestRecordHelper = record helper for TTestRecord
  end;

begin
  Writeln('Size of TTestRecordHelper: ', SizeOf(TTestRecordHelper));
  if SizeOf(TTestRecordHelper) <> SizeOf(Pointer) then
    Halt(1);
  Writeln('ok');
end.
