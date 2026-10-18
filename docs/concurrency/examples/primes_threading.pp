program primes_threading;
// Варіант 2: System.Threading (TParallel.For, TTask.Future).
// Кількість і сума простих чисел до N.
{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, DateUtils, System.Threading;

const
  N = 5000000;

function IsPrime(X: Integer): Boolean;
var
  D: Integer;
begin
  if X < 2 then Exit(False);
  if X mod 2 = 0 then Exit(X = 2);
  D := 3;
  while D * D <= X do
  begin
    if X mod D = 0 then Exit(False);
    Inc(D, 2);
  end;
  Result := True;
end;

var
  Count: Integer;
  Sum: Int64;
  Largest: specialize IFuture<Integer>;
  Start: TDateTime;
begin
  Start := Now;
  // Незалежна задача паралельно з циклом: найбільше просте до N.
  Largest := TTask.specialize Future<Integer>(
    function: Integer
    begin
      Result := N;
      while not IsPrime(Result) do
        Dec(Result);
    end);

  Count := 0;
  Sum := 0;
  // Пул сам ділить роботу між потоками й вирівнює навантаження.
  TParallel.&For(1, N,
    procedure(I: Integer)
    begin
      if IsPrime(I) then
      begin
        AtomicIncrement(Count);
        InterlockedExchangeAdd64(Sum, I);
      end;
    end);

  WriteLn('Primes: ', Count, ', sum: ', Sum);
  WriteLn('Largest prime <= N: ', Largest.Value);   // Value чекає результат
  WriteLn('Time: ', MilliSecondsBetween(Now, Start), ' ms');
end.
