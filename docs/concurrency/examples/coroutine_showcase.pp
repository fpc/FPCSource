program coroutine_showcase;
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch anonymousfunctions}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes,
  System.Threading,
  eventloop.types, eventloop.core, fcleventloop,
  async.types, async.fiber, async.generator, async.statemachine, fclasync;

{ ============================================================================ }
{ 1. Компіляторні async функції та await оператор                              }
{ ============================================================================ }

procedure BackgroundLogger; async;
begin
  WriteLn('[Compiler async] Фоновий логер запущено');
end;

function ComputeBaseAsync: specialize IFuture<Integer>; async;
begin
  Result := TTask.specialize Future<Integer>(
    function: Integer
    begin
      Sleep(50);
      Result := 42;
    end);
end;

function ProcessDataAsync(const AInput: specialize IFuture<Integer>): specialize IFuture<Integer>; async;
var
  Val: Integer;
begin
  { Оператор await автоматично розгортає значення майбутньої задачі }
  Val := await AInput;
  Result := TTask.specialize Future<Integer>(
    function: Integer
    begin
      Result := Val * 2;
    end);
end;

procedure DemoCompilerAsyncAwait;
var
  F1, F2: specialize IFuture<Integer>;
  FinalAnswer: Integer;
  TaskObj: ITask;
begin
  WriteLn('--- 1. Компіляторні процедури async та оператор await ---');
  BackgroundLogger;

  F1 := ComputeBaseAsync;
  F2 := ProcessDataAsync(F1);

  FinalAnswer := await F2;
  WriteLn(Format('[Compiler await] Отримано результат: %d (очікувалось 84)', [FinalAnswer]));

  TaskObj := TTask.Run(
    procedure
    begin
      Sleep(20);
      WriteLn('[Compiler await] Фонова задача ITask успішно виконана');
    end);
  await TaskObj;
  WriteLn;
end;

{ ============================================================================ }
{ 2. Легковагові стекові корутини (Файбери) поверх fcl-eventloop та IFuture    }
{ ============================================================================ }

type
  TFiberDemoWorker = class
  public
    Loop: IEventLoop;
    Done: Boolean;
    procedure Run;
  end;

procedure TFiberDemoWorker.Run;
var
  Fut: specialize IFuture<string>;
  Greeting: string;
  T: ITask;
begin
  WriteLn('[Fiber] Запуск файбера на стеку 128 КБ');

  { 1. Неблокуючий Await на IFuture }
  Fut := TTask.specialize Future<string>(
    function: string
    begin
      Sleep(30);
      Result := 'Привіт із фонової Future!';
    end);

  Greeting := TFiber.specialize Await<string>(Fut);
  WriteLn('[Fiber] Await<string> повернув: ', Greeting);

  { 2. Неблокуючий Await на ITask }
  T := TTask.Run(
    procedure
    begin
      Sleep(20);
      WriteLn('[Fiber Task] Робота завершена');
    end);
  TFiber.Await(T);

  { 3. Неблокуючий сон через таймер EventLoop }
  WriteLn('[Fiber] Засинаємо на 50 мс через таймер EventLoop...');
  TFiber.Sleep(50, Loop);
  WriteLn('[Fiber] Прокинулись після сну!');
  Done := True;
end;

procedure DemoFibers;
var
  Worker: TFiberDemoWorker;
  Fiber: TFiber;
  I: Integer;
begin
  WriteLn('--- 2. Легковагові корутини TFiber ---');
  Worker := TFiberDemoWorker.Create;
  try
    Worker.Loop := TEventLoop.Create(bkAuto);
    Worker.Done := False;

    Fiber := TFiber.Create(@Worker.Run, 128 * 1024, Worker.Loop);
    try
      Fiber.Resume;

      { Прокручуємо EventLoop, доки файбер не завершиться }
      for I := 1 to 50 do
      begin
        if Worker.Done then
          Break;
        Worker.Loop.RunOnce(10);
      end;
    finally
      Fiber.Free;
    end;
  finally
    Worker.Free;
  end;
  WriteLn;
end;

{ ============================================================================ }
{ 3. Генератори послідовностей TGenerator<T> з підтримкою for..in             }
{ ============================================================================ }

type
  TIntGenerator = specialize TGenerator<Integer>;

procedure FibonacciProducer(const Yield: TIntGenerator.TYieldProc);
var
  A, B, Temp, I: Integer;
begin
  A := 0;
  B := 1;
  for I := 1 to 10 do
  begin
    Yield(B);
    Temp := A + B;
    A := B;
    B := Temp;
  end;
end;

procedure DemoGenerators;
var
  Gen: TIntGenerator;
  Val, Sum: Integer;
begin
  WriteLn('--- 3. Генератори TGenerator<T> (Fibonacci) ---');

  Gen := TIntGenerator.Create(@FibonacciProducer);
  try
    Write('[Generator for..in] Перші 10 чисел Фібоначчі: ');
    Sum := 0;
    for Val in Gen do
    begin
      Write(Val, ' ');
      Inc(Sum, Val);
    end;
    WriteLn;
    WriteLn('[Generator] Сума: ', Sum);
  finally
    Gen.Free;
  end;
  WriteLn;
end;

{ ============================================================================ }
{ 4. TTaskCompletionSource (Roslyn async/await фундамент)                      }
{ ============================================================================ }

type
  TIntTCS = specialize TTaskCompletionSource<Integer>;

procedure DemoStateMachine;
var
  Tcs: TIntTCS;
  TaskResult: specialize IFuture<Integer>;
begin
  WriteLn('--- 4. TTaskCompletionSource<T> ---');
  Tcs := TIntTCS.Create;
  try
    TaskResult := Tcs.Task;

    { Фонове асинхронне встановлення результату }
    TTask.Run(
      procedure
      begin
        Sleep(30);
        Tcs.SetResult(777);
      end);

    WriteLn('[TCS] Отримане значення через майбутню задачу: ', TaskResult.Value);
  finally
    Tcs.Free;
  end;
  WriteLn;
end;

begin
  WriteLn('================================================================');
  WriteLn('  FPC Modern Concurrency: Фаза 1.4 (Корутини та async/await)   ');
  WriteLn('================================================================');
  WriteLn;

  DemoCompilerAsyncAwait;
  DemoFibers;
  DemoGenerators;
  DemoStateMachine;

  WriteLn('Усі демонстраційні приклади Фази 1.4 успішно виконані!');
end.

