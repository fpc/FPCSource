program parallel_for;
// TParallel.For: кожна ітерація виконується рівно один раз,
// цикл справді паралельний навіть на «холодному» пулі.
{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, System.Threading;

const
  N = 1000000;
var
  Hits: array of Integer;
  I, Bad: Integer;
  Res: TParallel.TLoopResult;
begin
  SetLength(Hits, N);
  Res := TParallel.&For(0, N - 1,
    procedure(aIndex: Integer)
    begin
      AtomicIncrement(Hits[aIndex]);   // різні потоки пишуть у різні елементи
    end);
  Bad := 0;
  for I := 0 to N - 1 do
    if Hits[I] <> 1 then
      Inc(Bad);
  WriteLn('Completed: ', Res.Completed, ', iterations not run exactly once: ', Bad);
end.
