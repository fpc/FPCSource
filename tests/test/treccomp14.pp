{ Record composition: a private (non-strict) field of an unnamed composed
  record is visible through the composing record within the same unit }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
  private
    C: Integer;
  end;

  TComposed = record
    A: Integer;
    B: Integer;
    contains TChildRec;
    D: Integer;
  end;

var
  c: TComposed;
begin
  WriteLn('@c.B: ', IntPtr(@c.B));
  WriteLn('@c.C: ', IntPtr(@c.C));
  WriteLn('@c.D: ', IntPtr(@c.D));
  if (SizeUInt(@c.B)<SizeUInt(@c.C)) and
     (SizeUInt(@c.C)<SizeUInt(@c.D)) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
