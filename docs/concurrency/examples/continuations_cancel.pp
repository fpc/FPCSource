program continuations_cancel;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, SyncObjs, System.Threading, System.Diagnostics;

const
  NUM_PIPELINES   = 64;
  VECTOR_LEN      = 2000;
  TREE_BRANCHES   = 2;
  TREE_ITERATIONS = 500;

type
  TDoubleArray = array of Double;

  TPipelineStats = record
    Sum: Double;
    Mean: Double;
    MaxVal: Double;
    ValidCount: Integer;
  end;

{ Допоміжні обчислення для етапів конвеєра }

function GenerateRawBatch(aBatchID: Integer; const aToken: TCancellationToken): TDoubleArray;
var
  I: Integer;
  Seed: Cardinal;
begin
  aToken.ThrowIfCancellationRequested;
  Sleep(1); // Симуляція асинхронного читання пакета з мережі/диска
  Result := nil;
  SetLength(Result, VECTOR_LEN);
  Seed := Cardinal(aBatchID * 2654435761);
  for I := 0 to VECTOR_LEN - 1 do
  begin
    if (I and 255) = 0 then
      aToken.ThrowIfCancellationRequested;
    Seed := Seed * 1664525 + 1013904223;
    Result[I] := ((Seed shr 8) mod 10000) / 100.0;
  end;
end;

function NormalizeBatch(const aInput: TDoubleArray): TDoubleArray;
var
  I: Integer;
  X: Double;
begin
  Result := nil;
  SetLength(Result, Length(aInput));
  for I := 0 to High(aInput) do
  begin
    X := aInput[I];
    // Нелінійне згладжування
    Result[I] := Sqrt(X * X + 1.0) * 0.85;
  end;
end;

function ComputeStats(const aData: TDoubleArray): TPipelineStats;
var
  I: Integer;
  V: Double;
begin
  Result := Default(TPipelineStats);
  if Length(aData) = 0 then
    Exit;
  Result.MaxVal := aData[0];
  for I := 0 to High(aData) do
  begin
    V := aData[I];
    Result.Sum := Result.Sum + V;
    if V > Result.MaxVal then
      Result.MaxVal := V;
    Inc(Result.ValidCount);
  end;
  Result.Mean := Result.Sum / Result.ValidCount;
end;

function FormatReport(const aStats: TPipelineStats): string;
begin
  Result := Format('N=%d Mean=%.2f Max=%.2f', [aStats.ValidCount, aStats.Mean, aStats.MaxVal]);
end;

{ ============================================================================
  ЧАСТИНА 1: Порівняння конвеєрів задач
  Підхід A — Класичний блокувальний (.Value / .Wait між кожним етапом)
  Підхід B — Неблокувальні продовження (.ThenBy / .Map / .ContinueWith)
  ============================================================================ }

procedure RunBlockingPipelines(out aElapsedMs: Int64; out aCompletedReports: Integer);
var
  SW: TStopwatch;
  ReportsDone: Integer;
  B: Integer;
  F1: specialize IFuture<TDoubleArray>;
  F2: specialize IFuture<TDoubleArray>;
  F3: specialize IFuture<TPipelineStats>;
  F4: specialize IFuture<string>;
  RawData, NormData: TDoubleArray;
  Stats: TPipelineStats;
  Report: string;

  function StartGen(aBatchID: Integer): specialize IFuture<TDoubleArray>;
  begin
    Result := TTask.specialize Future<TDoubleArray>(
      function: TDoubleArray
      begin
        Result := GenerateRawBatch(aBatchID, TCancellationToken.None);
      end);
  end;

  function StartNorm(const aRaw: TDoubleArray): specialize IFuture<TDoubleArray>;
  begin
    Result := TTask.specialize Future<TDoubleArray>(
      function: TDoubleArray
      begin
        Result := NormalizeBatch(aRaw);
      end);
  end;

  function StartStats(const aNorm: TDoubleArray): specialize IFuture<TPipelineStats>;
  begin
    Result := TTask.specialize Future<TPipelineStats>(
      function: TPipelineStats
      begin
        Result := ComputeStats(aNorm);
      end);
  end;

  function StartFormat(const aSt: TPipelineStats): specialize IFuture<string>;
  begin
    Result := TTask.specialize Future<string>(
      function: string
      begin
        Result := FormatReport(aSt);
      end);
  end;

begin
  ReportsDone := 0;
  SW := TStopwatch.StartNew;

  // Без .ThenBy/.ContinueWith передавання результату з F1 у F2 вимагає
  // блокувального читання .Value на кожному кроці (або ризикує вичерпати пул потоків,
  // якщо запустити 64 задачі-координатори, які блокують потоки пулу у .Value).
  for B := 0 to NUM_PIPELINES - 1 do
  begin
    F1 := StartGen(B);
    RawData := F1.Value; // <-- Блокувальне очікування етапу 1

    F2 := StartNorm(RawData);
    NormData := F2.Value; // <-- Блокувальне очікування етапу 2

    F3 := StartStats(NormData);
    Stats := F3.Value; // <-- Блокувальне очікування етапу 3

    F4 := StartFormat(Stats);
    Report := F4.Value; // <-- Блокувальне очікування етапу 4

    if Length(Report) > 0 then
      Inc(ReportsDone);
  end;

  SW.Stop;
  aElapsedMs := SW.ElapsedMilliseconds;
  aCompletedReports := ReportsDone;
end;

procedure RunNonBlockingPipelines(out aElapsedMs: Int64; out aCompletedReports: Integer);
var
  SW: TStopwatch;
  EndTasks: array[0..NUM_PIPELINES - 1] of ITask;
  ReportsDone: Integer;
  B: Integer;

  function LaunchNonBlockingPipeline(aBatchID: Integer): ITask;
  var
    Stage1: specialize IFuture<TDoubleArray>;
    Stage2: specialize IFuture<TDoubleArray>;
    Stage3: specialize IFuture<TPipelineStats>;
    Stage4: specialize IFuture<string>;
  begin
    // Жоден потік не блокується на проміжних етапах:
    // усі 64 конвеєри запускаються паралельно, а кожен наступний етап
    // планується автоматично по завершенню попереднього.
    Stage1 := TTask.specialize Future<TDoubleArray>(
      function: TDoubleArray
      begin
        Result := GenerateRawBatch(aBatchID, TCancellationToken.None);
      end);

    Stage2 := Stage1.Map(@NormalizeBatch);
    Stage3 := Stage2.specialize ThenBy<TDoubleArray, TPipelineStats>(@ComputeStats);
    Stage4 := Stage3.specialize ThenBy<TPipelineStats, string>(@FormatReport);

    Result := Stage4.ContinueWith(
      procedure(aFut: specialize IFuture<string>)
      begin
        if (aFut.Status = TTaskStatus.Completed) and (Length(aFut.Value) > 0) then
          AtomicIncrement(ReportsDone);
      end, [TTaskContinuationOption.OnlyOnCompleted, TTaskContinuationOption.ExecuteSynchronously]);
  end;

begin
  ReportsDone := 0;
  SW := TStopwatch.StartNew;

  for B := 0 to NUM_PIPELINES - 1 do
    EndTasks[B] := LaunchNonBlockingPipeline(B);

  TTask.WaitForAll(EndTasks);
  SW.Stop;
  aElapsedMs := SW.ElapsedMilliseconds;
  aCompletedReports := ReportsDone;
end;

{ ============================================================================
  ЧАСТИНА 2: Кооперативне скасування дерева задач і TParallel.For
  Порівняння: ручний прапорець + окремі .Cancel проти TCancellationTokenSource
  ============================================================================ }

procedure DemoCooperativeTreeCancellation;
var
  UserCTS, TimeoutCTS, LinkedCTS: TCancellationTokenSource;
  RootTask, AuditOnCancel, AuditOnSuccess: ITask;
  BranchesStarted, IterationsExecuted, CancelAuditTriggered, SuccessAuditTriggered: Integer;
  ReadyEvent: TLightweightEvent;
  SW: TStopwatch;
begin
  Writeln('=== ЧАСТИНА 2: Кооперативне скасування дерева операцій (TCancellationToken) ===');
  BranchesStarted := 0;
  IterationsExecuted := 0;
  CancelAuditTriggered := 0;
  SuccessAuditTriggered := 0;

  ReadyEvent := TLightweightEvent.Create;
  UserCTS := TCancellationTokenSource.Create;
  TimeoutCTS := TCancellationTokenSource.Create(5000); // резервний таймаут 5 с
  // Пов''язаний токен спрацює, якщо або користувач натисне Cancel, або спливе таймаут
  LinkedCTS := TCancellationTokenSource.CreateLinkedTokenSource([UserCTS.Token, TimeoutCTS.Token]);
  try
    // Реєструємо зворотний виклик миттєвого сповіщення про скасування
    LinkedCTS.Token.Register(
      procedure
      begin
        Writeln('  [Token Callback] Отримано сигнал скасування! Починаємо згортання дерева задач...');
      end);

    SW := TStopwatch.StartNew;

    // Коренева задача отримує LinkedCTS.Token; усі дочірні задачі та вкладений
    // TParallel.For успадковують його автоматично через контекст виконання!
    RootTask := TTask.Run(
      procedure
      var
        SubTasks: array[0..TREE_BRANCHES - 1] of ITask;
        I: Integer;
      begin
        for I := 0 to TREE_BRANCHES - 1 do
          SubTasks[I] := TTask.Run(
            procedure
            var
              LoopRes: TParallel.TLoopResult;
            begin
              if AtomicIncrement(BranchesStarted) = 1 then
                ReadyEvent.SetEvent;

              // Вкладений паралельний цикл кооперативно реагує на той самий токен
              LoopRes := TParallel.&For(1, TREE_ITERATIONS,
                procedure(aIdx: Integer)
                begin
                  if aIdx = 0 then;
                  TTask.CurrentTask.CheckCanceled;
                  AtomicIncrement(IterationsExecuted);
                  Sleep(2);
                end, LinkedCTS.Token);

              if not LoopRes.Completed then
                TTask.CurrentTask.CheckCanceled;
            end);

        TTask.WaitForAll(SubTasks);
      end, LinkedCTS.Token);

    // Підключаємо дві гілки продовжень: одну для успіху, іншу для компенсації при скасуванні
    AuditOnSuccess := RootTask.ContinueWith(
      procedure(aAnt: ITask)
      begin
        if aAnt = nil then;
        AtomicIncrement(SuccessAuditTriggered);
      end, [TTaskContinuationOption.OnlyOnCompleted]);

    AuditOnCancel := RootTask.ContinueWith(
      procedure(aAnt: ITask)
      begin
        if aAnt.Status = TTaskStatus.Canceled then
          AtomicIncrement(CancelAuditTriggered);
      end, [TTaskContinuationOption.OnlyOnCanceled, TTaskContinuationOption.ExecuteSynchronously]);

    // Чекаємо старту гілок і подаємо єдиний сигнал скасування на весь граф
    ReadyEvent.WaitFor(2000);
    Sleep(15);
    UserCTS.Cancel;

    try
      RootTask.Wait(5000);
    except
      on E: Exception do ;
    end;
    AuditOnCancel.Wait(2000);
    SW.Stop;

    Writeln(Format('  Статус кореневої задачі       : %d (очікувано Canceled = %d)',
      [Ord(RootTask.Status), Ord(TTaskStatus.Canceled)]));
    Writeln(Format('  Виконано ітерацій до зупинки  : %d з %d можливих',
      [IterationsExecuted, TREE_BRANCHES * TREE_ITERATIONS]));
    Writeln(Format('  Гілка OnlyOnCompleted         : статус=%d, викликів=%d',
      [Ord(AuditOnSuccess.Status), SuccessAuditTriggered]));
    Writeln(Format('  Гілка OnlyOnCanceled          : статус=%d, викликів=%d',
      [Ord(AuditOnCancel.Status), CancelAuditTriggered]));
    Writeln(Format('  Час координованого згортання  : %d мс', [SW.ElapsedMilliseconds]));
    Writeln;
  finally
    LinkedCTS.Free;
    TimeoutCTS.Free;
    UserCTS.Free;
    ReadyEvent.Free;
  end;
end;

{ ============================================================================
  ЧАСТИНА 3: Обробка помилок у неблокувальному конвеєрі (.ThenBy + OnlyOnFaulted)
  ============================================================================ }

procedure DemoPipelineErrorHandling;
var
  Stage1: specialize IFuture<Integer>;
  Stage2: specialize IFuture<Integer>;
  Stage3: specialize IFuture<string>;
  RecoveryTask: ITask;
  RecoveredMsg: string;
begin
  Writeln('=== ЧАСТИНА 3: Автоматичне прокидання винятків крізь конвеєр .ThenBy ===');
  RecoveredMsg := '';

  Stage1 := TTask.specialize Future<Integer>(
    function: Integer
    begin
      raise EInvalidOp.Create('Помилка читання зовнішнього датчика #7');
      Result := 0;
    end);

  // Наступні етапи .ThenBy автоматично пропускають виконання тіла і переносять помилку
  Stage2 := Stage1.specialize ThenBy<Integer, Integer>(
    function(const aVal: Integer): Integer
    begin
      Result := aVal * 10;
    end);

  Stage3 := Stage2.specialize ThenBy<Integer, string>(
    function(const aVal: Integer): string
    begin
      Result := 'Результат: ' + IntToStr(aVal);
    end);

  RecoveryTask := Stage3.ContinueWith(
    procedure(aFaultedFuture: specialize IFuture<string>)
    begin
      if aFaultedFuture.Status = TTaskStatus.Exception then
        RecoveredMsg := 'Перехоплено у OnlyOnFaulted (Stage3.Status = Exception)';
    end, [TTaskContinuationOption.OnlyOnFaulted, TTaskContinuationOption.ExecuteSynchronously]);

  RecoveryTask.Wait(3000);
  Writeln('  Статус Stage1 / Stage2 / Stage3 : ',
    Ord(Stage1.Status), ' / ', Ord(Stage2.Status), ' / ', Ord(Stage3.Status),
    ' (Exception = ', Ord(TTaskStatus.Exception), ')');
  Writeln('  Результат компенсації           : ', RecoveredMsg);
  Writeln;
end;

var
  BlockingMs, NonBlockingMs: Int64;
  BlockingCount, NonBlockingCount: Integer;
begin
  Writeln('==============================================================================');
  Writeln(' Демонстрація Фази 1.1: Неблокувальні продовження (.ContinueWith / .ThenBy)');
  Writeln(' та кооперативне скасування (TCancellationToken / TCancellationTokenSource)');
  Writeln('==============================================================================');
  Writeln;

  Writeln('=== ЧАСТИНА 1: Конвеєрна обробка ', NUM_PIPELINES, ' пакетів по ', VECTOR_LEN, ' елементів ===');
  RunBlockingPipelines(BlockingMs, BlockingCount);
  RunNonBlockingPipelines(NonBlockingMs, NonBlockingCount);

  Writeln(Format('  [Підхід A] Класичний блокувальний (.Value/.Wait) : %4d мс (оброблено %d пакетів)',
    [BlockingMs, BlockingCount]));
  Writeln(Format('  [Підхід B] Неблокувальний (.ThenBy/.ContinueWith): %4d мс (оброблено %d пакетів)',
    [NonBlockingMs, NonBlockingCount]));
  Writeln;
  Writeln('  Порівняльна таблиця архітектурних переваг:');
  Writeln('  +------------------------------------+---------------------------+-----------------------------+');
  Writeln('  | Критерій                           | Стандартний (.Wait/.Value)| Новий (.ThenBy + Token)     |');
  Writeln('  +------------------------------------+---------------------------+-----------------------------+');
  Writeln('  | Блокування робочих потоків пулу    | Так (4 блокування/пакет)  | Ні (0 блокувань, граф DAG)  |');
  Writeln('  | Ризик thread-pool starvation       | Високий при вкладеності   | Відсутній                   |');
  Writeln('  | Прокидання винятків крізь етапи    | Ручне try..except         | Автоматичне                 |');
  Writeln('  | Скасування дерева задач + циклів   | Ручні прапорці у кожній   | Єдиний TCancellationToken   |');
  Writeln('  | Підтримка таймауту (CancelAfter)   | Відсутня                  | Вбудована                   |');
  Writeln('  +------------------------------------+---------------------------+-----------------------------+');
  Writeln;

  DemoCooperativeTreeCancellation;
  DemoPipelineErrorHandling;

  Writeln('Усі демонстраційні сценарії успішно виконано.');
end.
