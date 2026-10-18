program batch_processing;
// Обробка порцій задач: 20 раундів по 500 незалежних задач (як сервер, що
// обробляє запити пачками). П'ять підходів на однаковій роботі; програма
// перевіряє, що всі дають той самий результат, і друкує таблицю часу.
{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, SyncObjs, System.Diagnostics, System.Threading;

const
  Rounds = 20;
  Jobs = 500;

var
  Results: array[0..Jobs - 1] of Int64;

// Одна задача: ~0,1 мс обчислень різної тривалості.
function Work(I: Integer): Int64;
var
  K: Integer;
begin
  Result := 0;
  for K := 1 to 20000 + (I mod 7) * 5000 do
    Result := Result + (K xor I) mod 7;
end;

function Checksum: Int64;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Jobs - 1 do
    Result := Result + Results[I] * (I + 1);
end;

{ --- 1. Послідовно ------------------------------------------------------- }

procedure RunSequential;
var
  R, I: Integer;
begin
  for R := 1 to Rounds do
    for I := 0 to Jobs - 1 do
      Results[I] := Work(I);
end;

{ --- 2. Класика: потік на кожну задачу ----------------------------------- }

type
  TJobThread = class(TThread)
  private
    FIndex: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AIndex: Integer);
  end;

constructor TJobThread.Create(AIndex: Integer);
begin
  FIndex := AIndex;
  inherited Create(False);
end;

procedure TJobThread.Execute;
begin
  Results[FIndex] := Work(FIndex);
end;

procedure RunThreadPerJob;
var
  R, I: Integer;
  T: array[0..Jobs - 1] of TJobThread;
begin
  for R := 1 to Rounds do
  begin
    for I := 0 to Jobs - 1 do
      T[I] := TJobThread.Create(I);
    for I := 0 to Jobs - 1 do
    begin
      T[I].WaitFor;
      T[I].Free;
    end;
  end;
end;

{ --- 3. Класика: робочі потоки + TThread.WaitFor ------------------------- }
{ --- 4. Класика «експерт»: робочі потоки + власна подія ------------------ }

var
  NextJob, Running: Integer;
  AllDone: TSimpleEvent;

type
  TWorker = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TWorker.Execute;
var
  I: Integer;
begin
  I := InterlockedIncrement(NextJob) - 1;       // кожен бере наступну задачу
  while I < Jobs do
  begin
    Results[I] := Work(I);
    I := InterlockedIncrement(NextJob) - 1;
  end;
  if InterlockedDecrement(Running) = 0 then
    AllDone.SetEvent;
end;

procedure RunWorkers(UseEvent: Boolean);
var
  R, I: Integer;
  W: array of TWorker;
begin
  SetLength(W, TThread.ProcessorCount);
  for R := 1 to Rounds do
  begin
    NextJob := 0;
    Running := Length(W);
    AllDone.ResetEvent;
    for I := 0 to High(W) do
      W[I] := TWorker.Create(False);
    if UseEvent then
      AllDone.WaitFor(INFINITE);   // прокидаємося одразу після останньої задачі
    for I := 0 to High(W) do
    begin
      W[I].WaitFor;                // у головному потоці опитує раз на 100 мс
      W[I].Free;
    end;
  end;
end;

{ --- 5. System.Threading ------------------------------------------------- }

procedure RunParallelFor;
var
  R: Integer;
begin
  for R := 1 to Rounds do
    TParallel.&For(0, Jobs - 1,
      procedure(I: Integer)
      begin
        Results[I] := Work(I);
      end);
end;

{ --- Заміри -------------------------------------------------------------- }

type
  TRunner = procedure;

var
  Reference: Int64;
  BaseMs: Int64;

procedure Measure(const Name: string; Run: TRunner);
var
  W: TStopWatch;
  Ms: Int64;
begin
  FillChar(Results, SizeOf(Results), 0);
  W := TStopWatch.StartNew;
  Run();
  Ms := W.ElapsedMilliseconds;
  if BaseMs = 0 then
  begin
    BaseMs := Ms;
    Reference := Checksum;
  end;
  WriteLn(Format('%-40s %6d ms  x%5.2f  %s',
    [Name, Ms, BaseMs / Ms, BoolToStr(Checksum = Reference, 'OK', 'WRONG RESULT')]));
end;

procedure RunWorkersWaitFor; begin RunWorkers(False); end;
procedure RunWorkersEvent;   begin RunWorkers(True);  end;

begin
  AllDone := TSimpleEvent.Create;
  WriteLn(Format('%d rounds x %d jobs, %d logical CPUs', [Rounds, Jobs, TThread.ProcessorCount]));
  Measure('1. Sequential', @RunSequential);
  Measure('2. TThread per job', @RunThreadPerJob);
  Measure('3. Worker threads + WaitFor', @RunWorkersWaitFor);
  Measure('4. Worker threads + event (expert)', @RunWorkersEvent);
  Measure('5. TParallel.For', @RunParallelFor);
  AllDone.Free;
end.
