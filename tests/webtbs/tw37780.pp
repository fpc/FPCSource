program testbug;

type
  PTestRec = ^TTestRec;
  TTestRec = record
    Val: Integer;
    Next: PTestRec;
  end;

var
  TR: TTestRec;

begin
  TR.Val := 6;
  TR.Next := nil;
  if (TR.Val = 10) or ((TR.Val = 5) and (TR.Next^.Val = 5)) then
    Writeln('OK');
end.
