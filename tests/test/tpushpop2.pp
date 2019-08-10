program tpushpop2;

type
{$PackRecords 1}
  TTest1 = record
    b: Byte;
    l: LongInt;
  end;
{$Push}
{$PackRecords 4}
  TTest2 = record
    b: Byte;
    l: LongInt;
  end;
{$Pop}
  TTest3 = record
    b: Byte;
    l: LongInt;
  end;

begin
  if SizeOf(TTest1) <> SizeOf(TTest3) then
    Halt(1);
  if SizeOf(TTest1) = SizeOf(TTest2) then
    Halt(2);
  Writeln('ok');
end.
