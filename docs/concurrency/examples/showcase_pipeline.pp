program showcase_pipeline;
// Порівняння на 4 прикладних задачах:
//   Стандартний підхід (класи TThread + ручна синхронізація)
//   проти System.Threading (TParallel.For + Break/Stop/Exceptions, WaitForAny, Join).
{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, SyncObjs, System.Diagnostics, System.Threading;

const
  N = 200000;

function ExpensiveVerify(Index: Integer): Int64;
var
  K: Integer;
  V: UInt64;
begin
  V := UInt64(Index) * 2654435761 + 17;
  for K := 1 to 600 do
    V := (V xor (V shr 13)) * 1274126177 + UInt64(K);
  Result := Int64(V and $7FFFFFFF);
end;

function IsCorrupted(Index: Integer): Boolean; inline;
begin
  Result := (Index = 24000) or (Index = 65000) or (Index = 150000);
end;

{ =========================================================================
  1. Пошук першого пошкодженого запису за індексом (Break)
  ========================================================================= }

var
  NextIdx1, LowestIdx1: Integer;

type
  TBreakWorker = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TBreakWorker.Execute;
var
  I, CurMin, Old: Integer;
begin
  while True do
  begin
    I := InterlockedIncrement(NextIdx1) - 1;
    CurMin := InterlockedCompareExchange(LowestIdx1, 0, 0);
    if (I >= N) or (I >= CurMin) then Exit;
    ExpensiveVerify(I);
    if IsCorrupted(I) then
    begin
      // Атомарно оновлюємо найменший знайдений індекс (CAS-цикл вручну)
      repeat
        CurMin := LowestIdx1;
        if I >= CurMin then Break;
        Old := InterlockedCompareExchange(LowestIdx1, I, CurMin);
      until Old = CurMin;
      Exit;
    end;
  end;
end;

procedure Stage1_Break;
var
  W: array of TBreakWorker;
  I: Integer;
  SW: TStopWatch;
  ClassicMs, ThreadingMs: Int64;
  Res: TParallel.TLoopResult;
begin
  // А) Стандартний TThread: клас потоку + ручний CAS-цикл мінімуму + WaitFor
  SW := TStopWatch.StartNew;
  NextIdx1 := 0;
  LowestIdx1 := MaxInt;
  SetLength(W, TThread.ProcessorCount);
  for I := 0 to High(W) do W[I] := TBreakWorker.Create(False);
  for I := 0 to High(W) do begin W[I].WaitFor; W[I].Free; end;
  ClassicMs := SW.ElapsedMilliseconds;

  // Б) System.Threading: State.Break сам відстежує LowestBreakIteration
  SW := TStopWatch.StartNew;
  Res := TParallel.&For(0, N - 1,
    procedure(Idx: Integer; State: TParallel.TLoopState)
    begin
      if State.ShouldExit then Exit;
      ExpensiveVerify(Idx);
      if IsCorrupted(Idx) then
        State.Break;
    end);
  ThreadingMs := SW.ElapsedMilliseconds;

  WriteLn(Format('1. Ordered Break (index=%d=%d):   TThread %4d ms (38 lines) | System.Threading %3d ms (10 lines)',
    [LowestIdx1, Integer(Res.LowestBreakIteration), ClassicMs, ThreadingMs]));
end;

{ =========================================================================
  2. Пошук будь-якого придатного елемента з миттєвою зупинкою (Stop)
  ========================================================================= }

var
  NextIdx2, StopFlag2, FoundIdx2: Integer;

type
  TStopWorker = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TStopWorker.Execute;
var
  I: Integer;
begin
  while InterlockedCompareExchange(StopFlag2, 0, 0) = 0 do
  begin
    I := InterlockedIncrement(NextIdx2) - 1;
    if I >= N then Exit;
    if (ExpensiveVerify(I) mod 9973 = 0) and (I > 1000) then
    begin
      InterlockedCompareExchange(FoundIdx2, I, -1);
      InterlockedExchange(StopFlag2, 1);
      Exit;
    end;
  end;
end;

procedure Stage2_Stop;
var
  W: array of TStopWorker;
  I, FoundPar: Integer;
  SW: TStopWatch;
  ClassicMs, ThreadingMs: Int64;
  Res: TParallel.TLoopResult;
begin
  // А) Стандартний TThread: клас потоку + спільний прапорець зупинки
  SW := TStopWatch.StartNew;
  NextIdx2 := 0;
  StopFlag2 := 0;
  FoundIdx2 := -1;
  SetLength(W, TThread.ProcessorCount);
  for I := 0 to High(W) do W[I] := TStopWorker.Create(False);
  for I := 0 to High(W) do begin W[I].WaitFor; W[I].Free; end;
  ClassicMs := SW.ElapsedMilliseconds;

  // Б) System.Threading: State.Stop
  FoundPar := -1;
  SW := TStopWatch.StartNew;
  Res := TParallel.&For(0, N - 1,
    procedure(Idx: Integer; State: TParallel.TLoopState)
    begin
      if State.ShouldExit then Exit;
      if (ExpensiveVerify(Idx) mod 9973 = 0) and (Idx > 1000) then
      begin
        InterlockedCompareExchange(FoundPar, Idx, -1);
        State.Stop;
      end;
    end);
  ThreadingMs := SW.ElapsedMilliseconds;

  WriteLn(Format('2. Immediate Stop (completed=%s): TThread %4d ms (30 lines) | System.Threading %3d ms (11 lines)',
    [BoolToStr(Res.Completed, 'T', 'F'), ClassicMs, ThreadingMs]));
end;

{ =========================================================================
  3. Пакетна валідація з перехопленням винятків
  ========================================================================= }

type
  TValidateWorker = class(TThread)
  private
    FLow, FHigh: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ALow, AHigh: Integer);
  end;

constructor TValidateWorker.Create(ALow, AHigh: Integer);
begin
  FLow := ALow;
  FHigh := AHigh;
  inherited Create(False);
end;

procedure TValidateWorker.Execute;
var
  I: Integer;
begin
  for I := FLow to FHigh do
  begin
    ExpensiveVerify(I);
    if I = 150 then
      raise Exception.Create('Invalid checksum at record #150');
  end;
end;

procedure Stage3_Exceptions;
var
  W: array[0..3] of TValidateWorker;
  I: Integer;
  ClassicMsg, ThreadingMsg: string;
  SW: TStopWatch;
  ClassicMs, ThreadingMs: Int64;
  E: Exception;
begin
  // А) Стандартний TThread: виняток осідає у FatalException кожного потоку;
  //    якщо не перевірити вручну перед Free — помилку буде тихо втрачено.
  SW := TStopWatch.StartNew;
  ClassicMsg := '';
  for I := 0 to High(W) do
    W[I] := TValidateWorker.Create(I * 250, (I + 1) * 250 - 1);
  for I := 0 to High(W) do
  begin
    W[I].WaitFor;
    if Assigned(W[I].FatalException) then
      ClassicMsg := Exception(W[I].FatalException).Message;
    W[I].Free;
  end;
  ClassicMs := SW.ElapsedMilliseconds;

  // Б) System.Threading: виняток автоматично прокидається як EAggregateException
  SW := TStopWatch.StartNew;
  ThreadingMsg := '';
  try
    TParallel.&For(0, 999,
      procedure(Idx: Integer)
      begin
        ExpensiveVerify(Idx);
        if Idx = 150 then
          raise Exception.Create('Invalid checksum at record #150');
      end);
  except
    on Agg: EAggregateException do
      for E in Agg do
        ThreadingMsg := E.Message;
  end;
  ThreadingMs := SW.ElapsedMilliseconds;

  WriteLn(Format('3. Exception handling (match=%s):  TThread %4d ms (36 lines) | System.Threading %3d ms (14 lines)',
    [BoolToStr((ClassicMsg <> '') and (ClassicMsg = ThreadingMsg), 'TRUE', 'FALSE'),
     ClassicMs, ThreadingMs]));
end;

{ =========================================================================
  4. Найшвидша репліка (WaitForAny) + паралельне зведення (Join)
  ========================================================================= }

var
  FastestEvent: TSimpleEvent;
  WinnerIdx: Integer;

type
  TReplicaThread = class(TThread)
  private
    FIdx, FDelayMs: Integer;
  public
    Response: string;
    constructor Create(AIdx, ADelayMs: Integer; const AName: string);
  protected
    procedure Execute; override;
  end;

  TSumThread = class(TThread)
  private
    FParity: Integer;
  public
    Total: Int64;
    constructor Create(AParity: Integer);
  protected
    procedure Execute; override;
  end;

constructor TReplicaThread.Create(AIdx, ADelayMs: Integer; const AName: string);
begin
  FIdx := AIdx;
  FDelayMs := ADelayMs;
  Response := AName;
  inherited Create(False);
end;

procedure TReplicaThread.Execute;
begin
  Sleep(FDelayMs);
  if InterlockedCompareExchange(WinnerIdx, FIdx, -1) = -1 then
    FastestEvent.SetEvent;
end;

constructor TSumThread.Create(AParity: Integer);
begin
  FParity := AParity;
  inherited Create(False);
end;

procedure TSumThread.Execute;
var
  I: Integer;
begin
  Total := 0;
  for I := 0 to 49999 do
    if I mod 2 = FParity then
      Inc(Total, I);
end;

procedure Stage4_WaitAnyAndJoin;
var
  Rep: array[0..2] of TReplicaThread;
  S0, S1: TSumThread;
  ClassicFastest: string;
  ClassicTotal, SumEven, SumOdd: Int64;
  R0, R1, R2: specialize IFuture<string>;
  Replicas: array[0..2] of specialize IFuture<string>;
  Fastest, I: Integer;
  SW: TStopWatch;
  ClassicMs, ThreadingMs: Int64;
begin
  // А) Стандартний TThread: TThread.WaitFor не вміє чекати "будь-який" потік,
  //    тому потрібні TSimpleEvent, атомарний WinnerIdx і 2 класи потоків.
  SW := TStopWatch.StartNew;
  FastestEvent := TSimpleEvent.Create;
  WinnerIdx := -1;
  Rep[0] := TReplicaThread.Create(0, 250, 'eu-west');
  Rep[1] := TReplicaThread.Create(1, 40,  'eu-central');
  Rep[2] := TReplicaThread.Create(2, 180, 'us-east');
  FastestEvent.WaitFor(1000);
  ClassicFastest := Rep[WinnerIdx].Response;

  S0 := TSumThread.Create(0);
  S1 := TSumThread.Create(1);
  S0.WaitFor; S1.WaitFor;
  ClassicTotal := S0.Total + S1.Total;
  S0.Free; S1.Free;
  ClassicMs := SW.ElapsedMilliseconds;
  for I := 0 to 2 do begin Rep[I].WaitFor; Rep[I].Free; end;
  FastestEvent.Free;

  // Б) System.Threading: IFuture<string> + WaitForAny + TParallel.Join
  SW := TStopWatch.StartNew;
  R0 := TTask.specialize Future<string>(function: string begin Sleep(250); Result := 'eu-west'; end);
  R1 := TTask.specialize Future<string>(function: string begin Sleep(40);  Result := 'eu-central'; end);
  R2 := TTask.specialize Future<string>(function: string begin Sleep(180); Result := 'us-east'; end);
  Replicas[0] := R0; Replicas[1] := R1; Replicas[2] := R2;
  Fastest := TTask.WaitForAny([R0, R1, R2], 1000);

  SumEven := 0; SumOdd := 0;
  TParallel.Join([
    procedure var K: Integer; begin for K := 0 to 49999 do if K mod 2 = 0 then Inc(SumEven, K); end,
    procedure var K: Integer; begin for K := 0 to 49999 do if K mod 2 = 1 then Inc(SumOdd, K); end
  ]).Wait;
  ThreadingMs := SW.ElapsedMilliseconds;
  TTask.WaitForAll([R0, R1, R2]);

  WriteLn(Format('4. WaitForAny + Join (%s, %d): TThread %4d ms (64 lines) | System.Threading %3d ms (16 lines)',
    [Replicas[Fastest].Value, SumEven + SumOdd, ClassicMs, ThreadingMs]));
  Assert((ClassicFastest = Replicas[Fastest].Value) and (ClassicTotal = SumEven + SumOdd));
end;

begin
  WriteLn('=== Standard TThread vs System.Threading (', TThread.ProcessorCount, ' logical CPUs) ===');
  Stage1_Break;
  Stage2_Stop;
  Stage3_Exceptions;
  Stage4_WaitAnyAndJoin;
  WriteLn('Total helper thread classes:        TThread = 5 classes (168 lines) | System.Threading = 0 classes (51 lines)');
end.
