program tb0642;
{$mode objfpc}
{$h+}

type
  TResult = (
    resFirst,
    resSecond,
    resElse
  );

function Test(const aStr: String): TResult;
begin
  case aStr of
    'Hello', 'Hello2': Exit(resFirst);
    'a'..'z': Exit(resSecond);
    else
      Exit(resElse);
  end;
end;

begin
  if Test('Hello') <> resFirst then
    Halt(1);
  if Test('Hello2') <> resFirst then
    Halt(2);
  if Test('a') <> resSecond then
    Halt(3);
  if Test('z') <> resSecond then
    Halt(4);
  if Test('g') <> resSecond then
    Halt(5);
  if Test('alpha') <> resSecond then
    Halt(6);
  if Test('zeta') <> resElse then
    Halt(7);
  if Test('A') <> resElse then
    Halt(8);
  if Test('1') <> resElse then
    Halt(9);
  Writeln('ok');
end.
