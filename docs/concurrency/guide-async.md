# Посібник користувача: Корутини, Файбери та async / await (Фаза 1.4)

Цей посібник документує нововведення **Фази 1.4** у гілці `feature/concurrency`:
1. **Крок 1 (Бібліотечний)**: легковагові корутини/файбери (`TFiber`), генератори (`TGenerator<T>`) та будівельники скінченних автоматів (`TTaskCompletionSource<T>`, `TAsyncTaskMethodBuilder<T>`) у новому пакеті `packages/fcl-async/`.
2. **Крок 2 (Компіляторний)**: синтаксичний модифікатор/директива `async` для процедур та функцій, що повертають `ITask` або `specialize IFuture<T>`, та оператор `await` для трансформації очікування результату в неблокувальне виконання.

---

## 1. Архітектурний огляд

```
+-------------------------------------------------------------------------------+
|                        Користувацький код (User Code)                         |
|   procedure Worker; async;        |   Val := await Future;   |   await Task;  |
+-----------------------------------+--------------------------+----------------+
                                    |
+-----------------------------------+-------------------------------------------+
|               Компілятор FPC Trunk (compiler/ppcx64)                          |
|   - pio_async у timplprocoption   |   - Контекстний оператор await            |
|   - Директива async для методів   |   - Автоматичний розгорт .Value / .Wait   |
+-----------------------------------+-------------------------------------------+
                                    |
+-----------------------------------+-------------------------------------------+
|                   Пакет fcl-async (packages/fcl-async/)                       |
|   - TFiber (стекові корутини)     |   - TGenerator<T> (for..in генератори)    |
|   - TFiber.Await<T> / Sleep       |   - TTaskCompletionSource<T> (стейт-машини)|
+-----------------------------------+-------------------------------------------+
                  |                                           |
+-----------------+-------------------+   +-------------------+-----------------+
|      fcl-eventloop (Фаза 1.3)       |   |       System.Threading (Фаза 1.1)   |
|   - epoll / io_uring / kqueue / IOCP|   |   - ITask, IFuture<T>, TThreadPool  |
|   - Min-Heap високоточні таймери    |   |   - TCancellationToken, ThenBy      |
+-------------------------------------+   +-------------------------------------+
```

---

## 2. Бібліотечні корутини (Файбери: `TFiber`)

Модуль `async.fiber.pp` реалізує легковагові стекові корутини (User-Space Fibers), які дозволяють виконувати асинхронний код у звичному послідовному стилі:

### 2.1. Особливості реалізації:
- **Швидке перемикання контексту**: власна асемблерна реалізація `FiberSwitchContext` для System V AMD64 ABI, що зберігає та відновлює лише callee-saved регістри (`%rbx, %rbp, %r12-%r15`) та покажчик стеку `%rsp` за менш ніж 10 наносекунд.
- **Вирівнювання стеку по ABI**: стек файбера гарантовано ініціалізується з 16-байтним вирівнюванням (`%rsp % 16 == 8` на вході в функцію), що запобігає збоям векторних інструкцій SSE/AVX (`movaps`).
- **Ізоляція стеку винятків FPC**: Free Pascal використовує глобальні threadvar змінні `ExceptAddrStack` та `ExceptObjectStack` для обробки `try..except` та `try..finally`. `TFiber` динамічно отримує зміщення цих змінних через механізм `FPC_THREADVAR_RELOCATE` та зберігає/відновлює окремий ланцюг обробників винятків для кожного файбера.
- **Безпечна міграція між системними потоками ОС**: адреси `ExceptAddrStack` повторно обчислюються після повернення у контекст, що дозволяє файберу, який призупинився на одному системному потоці (наприклад, головному потоці), бути безпечно відновленим на іншому (наприклад, у пулі робочих потоків чи в потоці реактора EventLoop).

### 2.2. Приклад використання TFiber:
```pascal
program FiberExample;
{$mode objfpc}{$H+}
uses
  cthreads, SysUtils, System.Threading, fcleventloop, fclasync;

type
  TWorker = class
    Loop: IEventLoop;
    procedure Run;
  end;

procedure TWorker.Run;
var
  Fut: specialize IFuture<Integer>;
  Res: Integer;
begin
  WriteLn('Файбер розпочав роботу');

  { Запускаємо важке обчислення у пулі потоків }
  Fut := TTask.specialize Future<Integer>(
    function: Integer
    begin
      Sleep(100);
      Result := 42;
    end);

  { Неблокуюче призупинення файбера до готовності Future:
    потік ОС звільняється для інших задач }
  Res := TFiber.specialize Await<Integer>(Fut);
  WriteLn('Отримано результат з Future: ', Res);

  { Неблокуючий сон через таймер EventLoop }
  WriteLn('Засинаємо на 50 мс...');
  TFiber.Sleep(50, Loop);
  WriteLn('Прокинулись та завершуємо файбер');
end;

var
  Worker: TWorker;
  Fiber: TFiber;
begin
  Worker := TWorker.Create;
  try
    Worker.Loop := TEventLoop.Create(bkAuto);
    Fiber := TFiber.Create(@Worker.Run, 128 * 1024, Worker.Loop);
    try
      Fiber.Resume;

      { Прокручуємо цикл подій для таймерів та I/O }
      while Fiber.State in [fsSuspended, fsRunning] do
        Worker.Loop.RunOnce(10);
    finally
      Fiber.Free;
    end;
  finally
    Worker.Free;
  end;
end.
```

---

## 3. Генератори послідовностей (`TGenerator<T>`)

Модуль `async.generator.pp` надає generic-клас `TGenerator<T>`, що дозволяє створювати ледачі (lazy) послідовності на базі корутин, з прямою підтримкою циклів `for..in`:

```pascal
program GeneratorExample;
{$mode objfpc}{$H+}
uses
  SysUtils, async.generator;

type
  TIntGen = specialize TGenerator<Integer>;

procedure FibonacciProducer(const Yield: TIntGen.TYieldProc);
var
  A, B, Temp, I: Integer;
begin
  A := 0;
  B := 1;
  for I := 1 to 10 do
  begin
    Yield(B); // Призупиняє генератор та повертає значення
    Temp := A + B;
    A := B;
    B := Temp;
  end;
end;

var
  Gen: TIntGen;
  Val: Integer;
begin
  Gen := TIntGen.Create(@FibonacciProducer);
  try
    { Ледачий обхід у циклі for..in }
    for Val in Gen do
      Write(Val, ' ');
    WriteLn;
  finally
    Gen.Free;
  end;
end.
```

---

## 4. Стейт-машини: `TTaskCompletionSource<T>`

Модуль `async.statemachine.pp` надає фундаментальні примітиви C# Roslyn-патерну для створення обіцянок (promises) та керування асинхронним станом:

```pascal
var
  Tcs: specialize TTaskCompletionSource<string>;
  Fut: specialize IFuture<string>;
begin
  Tcs := specialize TTaskCompletionSource<string>.Create;
  try
    Fut := Tcs.Task; // Отримуємо IFuture<string>

    TTask.Run(
      procedure
      begin
        Sleep(50);
        Tcs.SetResult('Успіх!'); // Пробуджує всі продовження та очікування
      end);

    WriteLn(Fut.Value); // Виведе 'Успіх!'
  finally
    Tcs.Free;
  end;
end;
```

---

## 5. Компіляторна підтримка: директива `async` та оператор `await`

У компіляторі trunk (`compiler/ppcx64`) впроваджено пряму підтримку ключових слів `async` та `await` як контекстних конструкцій без порушення бінарної сумісності PPU:

### 5.1. Директива `async`
Може вказуватися для процедур та функцій, що виконують асинхронні операції або повертають асинхронні дескриптори (`ITask` або `specialize IFuture<T>`):
```pascal
procedure ProcessLog; async;
begin
  WriteLn('Асинхронна процедура');
end;

function FetchDataAsync: specialize IFuture<Integer>; async;
begin
  Result := TTask.specialize Future<Integer>(
    function: Integer
    begin
      Result := 42;
    end);
end;
```

### 5.2. Оператор `await`
Оператор `await` може застосовуватися:
1. **До `specialize IFuture<T>` у виразах**: автоматично розгортає значення майбутньої задачі, повертаючи розпакований тип `T`:
   ```pascal
   function AddTenAsync(const AFuture: specialize IFuture<Integer>): specialize IFuture<Integer>; async;
   var
     Val: Integer;
   begin
     Val := await AFuture; // Val отримує Integer напряму
     Result := TTask.specialize Future<Integer>(
       function: Integer
       begin
         Result := Val + 10;
       end);
   end;
   ```
2. **До `ITask` або об'єктів з методом `Wait` як оператор або вираз**: автоматично очікує завершення виконання фонової операції без необхідності ручного виклику `.Wait`:
   ```pascal
   var
     T: ITask;
   begin
     T := TTask.Run(procedure begin DoWork; end);
     await T; // Неблокуюче очікування завершення задачі
   end;
   ```

---

## 6. Збірка та запуск тестів

Для перевірки роботи функціоналу Фази 1.4:
```sh
# Збірка і прогін модульних тестів fcl-async (12/12 тестів)
./tests/concurrency/build-async.sh
./tests/concurrency/out-async/testasync -a --format=plain

# Збірка і прогін демонстраційного прикладу coroutine_showcase
. tests/concurrency/fpcenv.sh
$FPC_TRUNK @"$FPC_CFG" -FE./tests/concurrency/out-async -FU./tests/concurrency/out-async \
  -Fi./packages/fcl-async/src -Fu./packages/fcl-async/src -Fu./packages/fcl-async/tests \
  docs/concurrency/examples/coroutine_showcase.pp
./tests/concurrency/out-async/coroutine_showcase
```

