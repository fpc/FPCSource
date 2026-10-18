program primes_classic;
// Варіант 1: лише TThread з RTL (без System.Threading).
// Кількість і сума простих чисел до N.
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, DateUtils;

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

type
  // Свій клас потоку на кожну задачу: межі діапазону й результати — поля.
  TPrimeWorker = class(TThread)
  private
    FLow, FHigh: Integer;
  public
    Count: Integer;
    Sum: Int64;
    constructor Create(ALow, AHigh: Integer);
  protected
    procedure Execute; override;
  end;

constructor TPrimeWorker.Create(ALow, AHigh: Integer);
begin
  FLow := ALow;
  FHigh := AHigh;
  inherited Create(False);           // одразу стартує
end;

procedure TPrimeWorker.Execute;
var
  I: Integer;
begin
  for I := FLow to FHigh do
    if IsPrime(I) then
    begin
      Inc(Count);
      Inc(Sum, I);
    end;
end;

var
  Workers: array of TPrimeWorker;
  ThreadCount, I, Chunk, Low, High, Count: Integer;
  Sum: Int64;
  Start: TDateTime;
begin
  Start := Now;
  ThreadCount := TThread.ProcessorCount;
  // Ручний поділ на рівні шматки. Великі числа перевіряти довше,
  // тож останні потоки працюють довше за перші.
  Chunk := N div ThreadCount;
  SetLength(Workers, ThreadCount);
  for I := 0 to ThreadCount - 1 do
  begin
    Low := I * Chunk + 1;
    if I = ThreadCount - 1 then High := N else High := Low + Chunk - 1;
    Workers[I] := TPrimeWorker.Create(Low, High);
  end;
  Count := 0;
  Sum := 0;
  for I := 0 to ThreadCount - 1 do
  begin
    Workers[I].WaitFor;                // чекаємо кожен потік вручну
    if Assigned(Workers[I].FatalException) then
      raise Exception.Create('Worker failed: ' +
        Exception(Workers[I].FatalException).Message);  // помилки — теж вручну
    Inc(Count, Workers[I].Count);      // збираємо результати вручну
    Inc(Sum, Workers[I].Sum);
    Workers[I].Free;                   // і звільняємо вручну
  end;
  WriteLn('Primes: ', Count, ', sum: ', Sum);
  WriteLn('Threads: ', ThreadCount, ', time: ', MilliSecondsBetween(Now, Start), ' ms');
end.
