program tpushpop1;

type
{$MinEnumSize 1}
  TTest1 = (t1One, t1Two, t1Three);
{$push}
{$MinEnumSize 2}
  TTest2 = (t2One, t2Two, t2Three);
{$pop}
  TTest3 = (t3One, t3Two, t3Three);

begin
  if SizeOf(TTest1) <> 1 then
    Halt(1);
  if SizeOf(TTest2) <> 2 then
    Halt(2);
  if SizeOf(TTest3) <> 1 then
    Halt(3);
  Writeln('ok');
end.
