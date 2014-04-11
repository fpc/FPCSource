{ %NORUN }

program tw25605; // Fatal: Compilation aborted

{$MODE DELPHI}
{$modeswitch typehelpers}

type
  TValueInt32Helper = record helper for Int32
  const
    C: Int32 = 0;
  end;

var
  I: Int32;
begin
  I := Int32.C;
end.
