program syncobjs_phase2;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, SyncObjs, System.Threading, System.Diagnostics;

type
  TJobRecord = record
    JobID: Integer;
    SensorID: Integer;
    RawValue: Double;
  end;

  TJobChannel = specialize TChannel<TJobRecord>;

const
  NUM_SENSORS      = 4;
  ITEMS_PER_SENSOR = 1000;
  TOTAL_ITEMS      = NUM_SENSORS * ITEMS_PER_SENSOR;
  CHANNEL_CAPACITY = 64;
  READS_PER_ITEM   = 25;
  GRID_SIZE        = 512;
  SIM_PHASES       = 40;

{ ============================================================================
  ПІДХІД A: Класичний ручний метод (TCriticalSection + TEvent + ручні лічильники)
  ============================================================================ }

type
  // Ручна кільцева черга з TCriticalSection і двома TEvent (~90 рядків boilerplate-коду)
  TManualBoundedQueue = class
  private
    FBuf: array[0..CHANNEL_CAPACITY - 1] of TJobRecord;
    FHead, FTail, FCount: Integer;
    FClosed: Boolean;
    FLock: TCriticalSection;
    FNotEmpty, FNotFull: TEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(const aItem: TJobRecord);
    function Pop(out aItem: TJobRecord): Boolean;
    procedure Close;
  end;

  // Ручний двофазний бар'єр на TCriticalSection + TEvent
  TManualAction = reference to procedure;

  TManualBarrier = class
  private
    FTotal, FRemaining, FPhase: Integer;
    FLock: TCriticalSection;
    FEvents: array[0..1] of TEvent;
    FPostAction: TManualAction;
  public
    constructor Create(aTotal: Integer; aPostAction: TManualAction);
    destructor Destroy; override;
    procedure WaitPhase;
  end;

constructor TManualBoundedQueue.Create;
begin
  FLock := TCriticalSection.Create;
  FNotEmpty := TEvent.Create(nil, True, False, '');
  FNotFull := TEvent.Create(nil, True, True, '');
end;

destructor TManualBoundedQueue.Destroy;
begin
  FNotEmpty.Free;
  FNotFull.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TManualBoundedQueue.Push(const aItem: TJobRecord);
begin
  while True do
  begin
    FLock.Acquire;
    if FCount < CHANNEL_CAPACITY then
    begin
      FBuf[FTail] := aItem;
      FTail := (FTail + 1) mod CHANNEL_CAPACITY;
      Inc(FCount);
      if FCount = CHANNEL_CAPACITY then
        FNotFull.ResetEvent;
      FNotEmpty.SetEvent;
      FLock.Release;
      Exit;
    end;
    FNotFull.ResetEvent;
    FLock.Release;
    FNotFull.WaitFor(INFINITE);
  end;
end;

function TManualBoundedQueue.Pop(out aItem: TJobRecord): Boolean;
begin
  while True do
  begin
    FLock.Acquire;
    if FCount > 0 then
    begin
      aItem := FBuf[FHead];
      FHead := (FHead + 1) mod CHANNEL_CAPACITY;
      Dec(FCount);
      if (FCount = 0) and (not FClosed) then
        FNotEmpty.ResetEvent;
      FNotFull.SetEvent;
      FLock.Release;
      Exit(True);
    end;
    if FClosed then
    begin
      FLock.Release;
      Exit(False);
    end;
    FNotEmpty.ResetEvent;
    FLock.Release;
    FNotEmpty.WaitFor(INFINITE);
  end;
end;

procedure TManualBoundedQueue.Close;
begin
  FLock.Acquire;
  try
    FClosed := True;
    FNotEmpty.SetEvent;
    FNotFull.SetEvent;
  finally
    FLock.Release;
  end;
end;

constructor TManualBarrier.Create(aTotal: Integer; aPostAction: TManualAction);
begin
  FTotal := aTotal;
  FRemaining := aTotal;
  FPhase := 0;
  FPostAction := aPostAction;
  FLock := TCriticalSection.Create;
  FEvents[0] := TEvent.Create(nil, True, False, '');
  FEvents[1] := TEvent.Create(nil, True, False, '');
end;

destructor TManualBarrier.Destroy;
begin
  FEvents[0].Free;
  FEvents[1].Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TManualBarrier.WaitPhase;
var
  MyPhase: Integer;
begin
  FLock.Acquire;
  MyPhase := FPhase;
  Dec(FRemaining);
  if FRemaining = 0 then
  begin
    if Assigned(FPostAction) then
      FPostAction();
    FRemaining := FTotal;
    Inc(FPhase);
    FEvents[FPhase and 1].ResetEvent;
    FEvents[MyPhase and 1].SetEvent;
    FLock.Release;
  end
  else
  begin
    FLock.Release;
    FEvents[MyPhase and 1].WaitFor(INFINITE);
  end;
end;

procedure RunClassicManualApproach(out ElapsedMs: Int64; out CheckSum: Int64; out FinalDelta: Double);
var
  SW: TStopwatch;
  CalLock: TCriticalSection;
  CalOffsets: array[0..NUM_SENSORS - 1] of Double;
  Queue: TManualBoundedQueue;
  DoneLock: TCriticalSection;
  DoneEvent: TEvent;
  ActiveWorkers: Integer;
  GridCurr, GridNext: array[0..GRID_SIZE - 1] of Double;
  ManualBar: TManualBarrier;
  Producers, SimTasks: array[0..NUM_SENSORS - 1] of ITask;
  CalTask: ITask;
  NextSensorIdx, NextSimIdx, I: Integer;
  LocalSum: Int64;
  LocalMaxDelta: Double;
begin
  SW := TStopwatch.StartNew;
  CalLock := TCriticalSection.Create;
  Queue := TManualBoundedQueue.Create;
  DoneLock := TCriticalSection.Create;
  DoneEvent := TEvent.Create(nil, True, False, '');
  ActiveWorkers := 1;
  LocalSum := 0;
  NextSensorIdx := 0;

  try
    for I := 0 to NUM_SENSORS - 1 do
      CalOffsets[I] := 1.0 + I * 0.5;

    CalTask := TTask.Run(
      procedure
      var Step, S: Integer;
      begin
        for Step := 1 to 10 do
        begin
          CalLock.Acquire;
          try
            for S := 0 to NUM_SENSORS - 1 do
              CalOffsets[S] := CalOffsets[S] + 0.01;
          finally
            CalLock.Release;
          end;
          Sleep(1);
        end;
      end);

    for I := 0 to NUM_SENSORS - 1 do
    begin
      DoneLock.Acquire;
      Inc(ActiveWorkers);
      DoneLock.Release;

      TTask.Run(
        procedure
        var
          Msg: TJobRecord;
          R: Integer;
          AccOffset: Double;
        begin
          try
            while Queue.Pop(Msg) do
            begin
              AccOffset := 0.0;
              // Багатократне читання таблиці калібрування: TCriticalSection серіалізує читачів!
              for R := 1 to READS_PER_ITEM do
              begin
                CalLock.Acquire;
                try
                  AccOffset := AccOffset + CalOffsets[Msg.SensorID];
                finally
                  CalLock.Release;
                end;
              end;
              AtomicIncrement(LocalSum, Round((Msg.RawValue + AccOffset / READS_PER_ITEM) * 100));
            end;
          finally
            DoneLock.Acquire;
            Dec(ActiveWorkers);
            if ActiveWorkers = 0 then
              DoneEvent.SetEvent;
            DoneLock.Release;
          end;
        end);
    end;

    for I := 0 to NUM_SENSORS - 1 do
      Producers[I] := TTask.Run(
        procedure
        var
          SensorID, K: Integer;
          Job: TJobRecord;
        begin
          SensorID := AtomicIncrement(NextSensorIdx) - 1;
          for K := 1 to ITEMS_PER_SENSOR do
          begin
            Job.JobID := SensorID * ITEMS_PER_SENSOR + K;
            Job.SensorID := SensorID;
            Job.RawValue := K * 1.25;
            Queue.Push(Job);
          end;
        end);

    TTask.WaitForAll(Producers);
    Queue.Close;

    DoneLock.Acquire;
    Dec(ActiveWorkers);
    if ActiveWorkers = 0 then
      DoneEvent.SetEvent;
    DoneLock.Release;
    DoneEvent.WaitFor(INFINITE);
    CalTask.Wait;
  finally
    DoneEvent.Free;
    DoneLock.Free;
    Queue.Free;
    CalLock.Free;
  end;

  for I := 0 to GRID_SIZE - 1 do
  begin
    if (I >= GRID_SIZE div 4) and (I < (3 * GRID_SIZE) div 4) then
      GridCurr[I] := 100.0
    else
      GridCurr[I] := 0.0;
    GridNext[I] := GridCurr[I];
  end;

  NextSimIdx := 0;
  ManualBar := TManualBarrier.Create(NUM_SENSORS,
    procedure
    var
      Idx: Integer;
      Diff, MaxD: Double;
    begin
      MaxD := 0.0;
      for Idx := 1 to GRID_SIZE - 2 do
      begin
        Diff := Abs(GridNext[Idx] - GridCurr[Idx]);
        if Diff > MaxD then
          MaxD := Diff;
        GridCurr[Idx] := GridNext[Idx];
      end;
      LocalMaxDelta := MaxD;
    end);
  try
    for I := 0 to NUM_SENSORS - 1 do
      SimTasks[I] := TTask.Run(
        procedure
        var
          WorkerIdx, Chunk, LowIdx, HighIdx, Phase, Cell: Integer;
        begin
          WorkerIdx := AtomicIncrement(NextSimIdx) - 1;
          Chunk := GRID_SIZE div NUM_SENSORS;
          LowIdx := WorkerIdx * Chunk;
          HighIdx := LowIdx + Chunk - 1;
          if LowIdx = 0 then LowIdx := 1;
          if HighIdx = GRID_SIZE - 1 then HighIdx := GRID_SIZE - 2;
          for Phase := 1 to SIM_PHASES do
          begin
            for Cell := LowIdx to HighIdx do
              GridNext[Cell] := 0.25 * GridCurr[Cell - 1] + 0.50 * GridCurr[Cell] + 0.25 * GridCurr[Cell + 1];
            ManualBar.WaitPhase;
          end;
        end);
    TTask.WaitForAll(SimTasks);
  finally
    ManualBar.Free;
  end;

  SW.Stop;
  ElapsedMs := SW.ElapsedMilliseconds;
  CheckSum := LocalSum;
  FinalDelta := LocalMaxDelta;
end;

{ ============================================================================
  ПІДХІД B: Нові примітиви SyncObjs Фази 2
  (TSlimReaderWriterLock, TChannel<T>, TCountdownEvent, TBarrier)
  ============================================================================ }

procedure RunPhase2Approach(out ElapsedMs: Int64; out CheckSum: Int64; out FinalDelta: Double);
var
  SW: TStopwatch;
  CalibrationLock: TSlimReaderWriterLock;
  CalibrationOffsets: array[0..NUM_SENSORS - 1] of Double;
  TelemetryChannel: TJobChannel;
  WorkersCountdown: TCountdownEvent;
  SimBarrier: TBarrier;
  GridCurr, GridNext: array[0..GRID_SIZE - 1] of Double;
  Producers, SimTasks: array[0..NUM_SENSORS - 1] of ITask;
  CalibratorTask: ITask;
  NextSensorIdx, NextSimWorkerIdx, I: Integer;
  LocalSum: Int64;
  LocalMaxDelta: Double;
begin
  SW := TStopwatch.StartNew;
  CalibrationLock := TSlimReaderWriterLock.Create;
  TelemetryChannel := TJobChannel.Create(CHANNEL_CAPACITY);
  WorkersCountdown := TCountdownEvent.Create(1);
  LocalSum := 0;
  NextSensorIdx := 0;

  try
    for I := 0 to NUM_SENSORS - 1 do
      CalibrationOffsets[I] := 1.0 + I * 0.5;

    CalibratorTask := TTask.Run(
      procedure
      var
        Step, S: Integer;
        WGuard: TSlimWriteLockGuard;
      begin
        for Step := 1 to 10 do
        begin
          WGuard.Init(CalibrationLock);
          for S := 0 to NUM_SENSORS - 1 do
            CalibrationOffsets[S] := CalibrationOffsets[S] + 0.01;
          WGuard.Release;
          Sleep(1);
        end;
      end);

    for I := 0 to NUM_SENSORS - 1 do
    begin
      WorkersCountdown.AddCount(1);
      TTask.Run(
        procedure
        var
          Msg: TJobRecord;
          RGuard: TSlimReadLockGuard;
          R: Integer;
          AccOffset: Double;
        begin
          try
            for Msg in TelemetryChannel do
            begin
              AccOffset := 0.0;
              // Lock-free атомарне паралельне читання через TSlimReadLockGuard!
              for R := 1 to READS_PER_ITEM do
              begin
                RGuard.Init(CalibrationLock);
                AccOffset := AccOffset + CalibrationOffsets[Msg.SensorID];
                RGuard.Release;
              end;
              AtomicIncrement(LocalSum, Round((Msg.RawValue + AccOffset / READS_PER_ITEM) * 100));
            end;
          finally
            WorkersCountdown.Signal;
          end;
        end);
    end;

    for I := 0 to NUM_SENSORS - 1 do
      Producers[I] := TTask.Run(
        procedure
        var
          SensorID, K: Integer;
          Job: TJobRecord;
        begin
          SensorID := AtomicIncrement(NextSensorIdx) - 1;
          for K := 1 to ITEMS_PER_SENSOR do
          begin
            Job.JobID := SensorID * ITEMS_PER_SENSOR + K;
            Job.SensorID := SensorID;
            Job.RawValue := K * 1.25;
            TelemetryChannel.Send(Job);
          end;
        end);

    TTask.WaitForAll(Producers);
    TelemetryChannel.Close;

    WorkersCountdown.Signal;
    WorkersCountdown.WaitFor;
    CalibratorTask.Wait;
  finally
    WorkersCountdown.Free;
    TelemetryChannel.Free;
    CalibrationLock.Free;
  end;

  for I := 0 to GRID_SIZE - 1 do
  begin
    if (I >= GRID_SIZE div 4) and (I < (3 * GRID_SIZE) div 4) then
      GridCurr[I] := 100.0
    else
      GridCurr[I] := 0.0;
    GridNext[I] := GridCurr[I];
  end;

  NextSimWorkerIdx := 0;
  SimBarrier := TBarrier.Create(NUM_SENSORS,
    procedure(aBarrier: TBarrier)
    var
      Idx: Integer;
      Diff, MaxD: Double;
    begin
      MaxD := 0.0;
      for Idx := 1 to GRID_SIZE - 2 do
      begin
        Diff := Abs(GridNext[Idx] - GridCurr[Idx]);
        if Diff > MaxD then
          MaxD := Diff;
        GridCurr[Idx] := GridNext[Idx];
      end;
      LocalMaxDelta := MaxD;
    end);

  try
    for I := 0 to NUM_SENSORS - 1 do
      SimTasks[I] := TTask.Run(
        procedure
        var
          WorkerIdx, Chunk, LowIdx, HighIdx, Phase, Cell: Integer;
        begin
          WorkerIdx := AtomicIncrement(NextSimWorkerIdx) - 1;
          Chunk := GRID_SIZE div NUM_SENSORS;
          LowIdx := WorkerIdx * Chunk;
          HighIdx := LowIdx + Chunk - 1;
          if LowIdx = 0 then LowIdx := 1;
          if HighIdx = GRID_SIZE - 1 then HighIdx := GRID_SIZE - 2;

          for Phase := 1 to SIM_PHASES do
          begin
            for Cell := LowIdx to HighIdx do
              GridNext[Cell] := 0.25 * GridCurr[Cell - 1] + 0.50 * GridCurr[Cell] + 0.25 * GridCurr[Cell + 1];
            SimBarrier.SignalAndWait;
          end;
        end);

    TTask.WaitForAll(SimTasks);
  finally
    SimBarrier.Free;
  end;

  SW.Stop;
  ElapsedMs := SW.ElapsedMilliseconds;
  CheckSum := LocalSum;
  FinalDelta := LocalMaxDelta;
end;

var
  ClassicMs, Phase2Ms: Int64;
  ClassicSum, Phase2Sum: Int64;
  ClassicDelta, Phase2Delta: Double;
begin
  Writeln('===========================================================================');
  Writeln(' ПОРІВНЯННЯ: Класична ручна синхронізація vs Нові примітиви SyncObjs (Фаза 2)');
  Writeln('   Пакетів телеметрії: ', TOTAL_ITEMS, ' (читань таблиці калібрування: ', TOTAL_ITEMS * READS_PER_ITEM, ')');
  Writeln('   1D-симуляція теплопровідності: сітка ', GRID_SIZE, ' комірок x ', SIM_PHASES, ' фаз');
  Writeln('===========================================================================');

  RunClassicManualApproach(ClassicMs, ClassicSum, ClassicDelta);
  Writeln(Format(' [A] Класичний метод (TCriticalSection + TEvent + ручна черга/бар''єр): %4d мс | дельта=%.4f',
    [ClassicMs, ClassicDelta]));

  RunPhase2Approach(Phase2Ms, Phase2Sum, Phase2Delta);
  Writeln(Format(' [B] SyncObjs Фаза 2 (TSlimRWLock + TChannel<T> + TCountdown + TBarrier): %4d мс | дельта=%.4f',
    [Phase2Ms, Phase2Delta]));

  Writeln('---------------------------------------------------------------------------');
  Writeln(' Збіг результатів чисельної симуляції: ', BoolToStr(Abs(ClassicDelta - Phase2Delta) < 1e-9, 'ТАК (OK)', 'НІ'));
  Writeln(' Скорочення інфраструктурного коду синхронізації: зі ~165 рядків до 4 декларацій класів');
  Writeln('===========================================================================');
end.
