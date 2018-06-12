program tpushpop3;

type
  TTest = (tOne, tTwo, tThree);

{$PackSet 1}
  TTestSet1 = set of TTest;
{$Push}
{$PackSet 2}
  TTestSet2 = set of TTest;
{$Pop}
  TTestSet3 = set of TTest;

begin
  if SizeOf(TTestSet1) <> 1 then
    Halt(1);
  if SizeOf(TTestSet2) <> 2 then
    Halt(2);
  if SizeOf(TTestSet3) <> 1 then
    Halt(3);
  Writeln('ok');
end.
