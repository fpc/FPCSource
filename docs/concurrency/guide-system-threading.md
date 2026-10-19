# Посібник користувача: `System.Threading` у Free Pascal (`feature/concurrency`)

## 1. Призначення та архітектура

Модуль `System.Threading` (пакет `vcl-compat`) надає високорівневу бібліотеку паралельного програмування (PPL), сумісну з Delphi RTL:

- **`TThreadPool`** — пул робочих потоків із локальними чергами крадіжки роботи (`TWorkStealingQueue`) та фоновим монітором.
- **`ITask` / `TTask`** — легковагові асинхронні задачі з підтримкою неблокувальних продовжень (`ContinueWith`, `ThenBy`), скасування (`Cancel`, `TCancellationToken`), очікування з таймаутом (`Wait`, `WaitForAll`, `WaitForAny`) та агрегації винятків.
- **`IFuture<T>` / `TTask.Future<T>`** — типізовані обчислення, що повертають значення типу `T` через властивість `.Value` та підтримують конвеєрні перетворення `.ThenBy<T, TResult>`, `.Map` і `.ContinueWith`.
- **`TCancellationToken` / `TCancellationTokenSource`** — легковаговий кооперативний механізм узгодженого скасування дерев задач, конвеєрів `IFuture<T>` та паралельних циклів `TParallel.For`.
- **`TParallel.For`** — паралельний цикл (`Integer` та `Int64`) із динамічним кроком, керуванням зупинкою (`TLoopState.Break` / `Stop`, `TCancellationToken`) та передаванням винятків.
- **`TParallel.Join`** — одночасне виконання кількох незалежних процедур або обробників подій.
- **`EAggregateException`** — контейнер винятків, що виникли в паралельних задачах чи ітераціях циклу.

Необхідні директиви компілятора для використання анонімних процедур і функцій:

```pascal
{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, System.Threading;
```

---

## 2. Паралельні цикли: `TParallel.For`

### 2.1. Базовий паралельний цикл

`TParallel.For` розбиває замкнений діапазон `[LowInclusive..HighInclusive]` на порції між потоками пулу. Кожна ітерація у завершеному циклі гарантовано виконується **рівно один раз**:

```pascal
var
  Res: TParallel.TLoopResult;
begin
  Res := TParallel.&For(0, N - 1,
    procedure(I: Integer)
    begin
      Results[I] := Compute(I);
    end);
  Assert(Res.Completed);
end;
```

> **Примітка.** Оскільки `For` є ключовим словом Паскаля, при виклику використовується префікс `&`: `TParallel.&For(...)`.

### 2.2. Ранній вихід із циклу: `TLoopState.Break` vs `TLoopState.Stop`

Перевантаження `TParallel.For` з параметром `TParallel.TLoopState` передає кожній робочій репліці власний екземпляр стану циклу. Передбачено два режими дострокового завершення:

| Метод | `TLoopResult.Completed` | `TLoopResult.LowestBreakIteration` | Гарантія щодо попередніх ітерацій | Коли використовувати |
|---|---|---|---|---|
| `State.Break` | `False` | Найменший індекс `I`, на якому викликано `Break` (`Variant`) | **Усі** ітерації з індексом `< LowestBreakIteration` гарантовано виконаються | Впорядкований пошук першого елемента за індексом |
| `State.Stop` | `False` | `Null` (`varNull`) | Немає — усі потоки припиняють роботу якнайшвидше | Неупорядкований пошук («будь-який збіг»), скасування за зовнішньою умовою |

Приклад впорядкованого пошуку через `Break`:

```pascal
Res := TParallel.&For(0, High(Data),
  procedure(I: Integer; State: TParallel.TLoopState)
  begin
    if State.ShouldExit then Exit;
    if IsCorrupted(Data[I]) then
      State.Break;
  end);

if not Res.Completed then
  WriteLn('Перший пошкоджений запис: ', Integer(Res.LowestBreakIteration));
```

### 2.3. Обробка винятків у `TParallel.For`

Якщо одна або кілька ітерацій кидають виняток:
1. Цикл переходить у стан помилки (`State.Faulted = True`, `State.ShouldExit = True`) і припиняє роздавати нові діапазони.
2. Усі винятки з кореневої задачі та дочірніх реплік збираються й розгортаються (flatten) в один об'єкт `EAggregateException`.
3. `TParallel.For` кидає цей `EAggregateException` у потоці-викликачі:

```pascal
try
  TParallel.&For(0, High(Items),
    procedure(I: Integer)
    begin
      ValidateItem(Items[I]); // може кинути виняток
    end);
except
  on Agg: EAggregateException do
    for E in Agg do
      WriteLn('Помилка валідації: ', E.Message);
end;
```

---

## 3. Паралельне об'єднання процедур: `TParallel.Join`

`TParallel.Join` створює й **одразу запускає** задачу `ITask`, яка паралельно виконує передані процедури або методи `TNotifyEvent`. Щоб дочекатися завершення всіх гілок, викличте `.Wait`:

```pascal
TParallel.Join([
  procedure begin BuildIndexA; end,
  procedure begin BuildIndexB; end,
  procedure begin WarmupCache; end
]).Wait;
```

---

## 4. Асинхронні задачі (`TTask`) та майбутні значення (`IFuture<T>`)

### 4.1. Обчислення з результатом (`IFuture<T>`)

```pascal
var
  Total: specialize IFuture<Int64>;
begin
  Total := TTask.specialize Future<Int64>(
    function: Int64
    begin
      Result := CalculateTotal;
    end);
  // ... інша робота паралельно ...
  WriteLn('Підсумок: ', Total.Value); // .Value чекає на завершення і повертає результат
end;
```

### 4.2. Очікування групи задач (`WaitForAll` і `WaitForAny`)

- **`TTask.WaitForAll([T1, T2, ...], TimeoutMs)`** — чекає завершення **всіх** задач (повертає `False`, якщо сплив таймаут; кидає `EAggregateException`, якщо хоча б одна задача впала з винятком).
- **`TTask.WaitForAny([T1, T2, ...], TimeoutMs)`** — чекає завершення **першої** задачі з масиву й повертає її індекс (`0..High`), або `-1` у разі таймауту. Зручно для хеджування затримок (запит до кількох реплік одночасно).

---

## 5. Неблокувальні продовження задач (`ContinueWith`, `ThenBy`, `Map`)

### 5.1. Проблема блокувальних викликів `.Wait` і `.Value`

При побудові багатоетапних конвеєрів виклик `Future1.Value` всередині іншої задачі пулу блокує робочий потік ОС. Якщо одночасно запустити більше таких координувальних задач, ніж `MaxWorkerThreads`, усі потоки пулу заблокуються в очікуванні дочірніх етапів, які нікому виконати (**thread-pool starvation**).

Методи `.ContinueWith`, `.ThenBy` і `.Map` будують неблокувальний граф залежностей (DAG): наступна задача ставиться в чергу пулу автоматично в момент завершення попередньої.

### 5.2. `ITask.ContinueWith` та `IFuture<T>.ContinueWith`

Метод `ContinueWith` реєструє дію, яка отримає завершену задачу-попередника (`ITask` або `IFuture<T>`) і повертає нову задачу `ITask`:

```pascal
var
  FetchTask, LogTask: ITask;
begin
  FetchTask := TTask.Run(@DownloadPayload);

  // Виконається лише у разі успішного завершення FetchTask
  LogTask := FetchTask.ContinueWith(
    procedure(aAntecedent: ITask)
    begin
      WriteLn('Завантаження завершено успішно: статус = ', Ord(aAntecedent.Status));
    end, [TTaskContinuationOption.OnlyOnCompleted]);
end;
```

Підтримувані прапорці `TTaskContinuationOption` (`TTaskContinuationOptions`):
- **`OnlyOnCompleted`** (`[NotOnFaulted, NotOnCanceled]`) — запускати лише якщо попередник успішно завершився (`TTaskStatus.Completed`).
- **`OnlyOnFaulted`** (`[NotOnCompleted, NotOnCanceled]`) — запускати лише якщо попередник завершився з винятком (`TTaskStatus.Exception`).
- **`OnlyOnCanceled`** (`[NotOnCompleted, NotOnFaulted]`) — запускати лише якщо попередник було скасовано (`TTaskStatus.Canceled`).
- **`ExecuteSynchronously`** — виконати продовження безпосередньо в потоці, який завершив задачу-попередника, без повторної постановки в чергу пулу.

### 5.3. Типізовані конвеєри `.ThenBy` та `.Map`

Type helper `TITaskHelper` (для `ITask` та `IFuture<T>`) і статичний метод `TTask.ThenBy<T, TResult>` дозволяють будувати типізовані конвеєри з автоматичним прокиданням винятків і скасування:

```pascal
var
  Stage1, Stage2: specialize IFuture<Integer>;
  FinalReport: specialize IFuture<string>;
begin
  Stage1 := TTask.specialize Future<Integer>(
    function: Integer
    begin
      Result := ReadSensorValue;
    end);

  // .Map перетворює T -> T без додаткової спеціалізації
  Stage2 := Stage1.Map(
    function(const aVal: Integer): Integer
    begin
      Result := aVal * 2 + 10;
    end);

  // .ThenBy<T, TResult> перетворює результат типу T у новий тип TResult
  FinalReport := Stage2.specialize ThenBy<Integer, string>(
    function(const aVal: Integer): string
    begin
      Result := Format('Нормалізоване значення: %d', [aVal]);
    end);

  WriteLn(FinalReport.Value);
end;
```

**Автоматична семантика помилок і скасування у `.ThenBy`:**
- Якщо етап `Stage1` кидає виняток, тіла `Stage2` і `FinalReport` **не виконуються**, а виняток автоматично передається у `FinalReport` (`FinalReport.Status = TTaskStatus.Exception`).
- Якщо етап `Stage1` скасовано, усі наступні етапи автоматично переходять у `TTaskStatus.Canceled`.

---

## 6. Кооперативне скасування: `TCancellationToken` і `TCancellationTokenSource`

### 6.1. Основні класи та записи

- **`TCancellationTokenSource`** — власник стану скасування:
  - `Create` / `Create(MillisecondsDelay)` / `Create(TTimeSpan)` — створення джерела (з опційним автоматичним таймаутом).
  - `Cancel` — негайний перехід у стан скасування та виклик усіх зареєстрованих обробників.
  - `CancelAfter(MillisecondsDelay)` — планування автоматичного скасування за таймаутом.
  - `CreateLinkedTokenSource([Token1, Token2, ...])` — комбіноване джерело, яке скасовується автоматично, щойно скасовано будь-який із вхідних токенів.
  - `Reset` — скидання джерела у початковий нескасований стан для повторного використання.
- **`TCancellationToken`** — легковаговий запис (`record`), який передається у задачі, майбутні значення та цикли:
  - `TCancellationToken.None` — порожній токен (`CanBeCanceled = False`).
  - `IsCancellationRequested` — швидке неблокувальне (`lock-free` atomic) читання прапорця скасування.
  - `ThrowIfCancellationRequested` (або `CheckCanceled`) — кидає `EOperationCancelled`, якщо запрошено скасування.
  - `Register(Callback)` — реєстрація зворотного виклику (повертає `TCancellationTokenRegistration` із методом `.Unregister`).
  - `WaitHandle` — подія `TEvent`, яка переходить у сигнальний стан під час `Cancel`.

### 6.2. Скасування дерева операцій і `TParallel.For`

Коли `TCancellationToken` передається у кореневу задачу `TTask.Run(..., CTS.Token)`, усі дочірні задачі та вкладені виклики `TParallel.&For`, запущені всередині неї, **автоматично успадковують** цей токен із контексту поточного завдання (`TTask.CurrentTask`):

```pascal
var
  CTS: TCancellationTokenSource;
  RootTask: ITask;
begin
  CTS := TCancellationTokenSource.Create(2500); // таймаут 2.5 с
  try
    RootTask := TTask.Run(
      procedure
      var
        LoopRes: TParallel.TLoopResult;
      begin
        LoopRes := TParallel.&For(1, 10000,
          procedure(I: Integer)
          begin
            TTask.CurrentTask.CheckCanceled; // кооперативна перевірка
            ProcessChunk(I);
          end, CTS.Token);
        if not LoopRes.Completed then
          TTask.CurrentTask.CheckCanceled;
      end, CTS.Token);

    // Можемо скасувати вручну в будь-який момент:
    // CTS.Cancel;
    RootTask.Wait;
  finally
    CTS.Free;
  end;
end;
```

---

## 7. Керування власним пулом потоків (`TThreadPool`)

За замовчуванням усі методи використовують `TThreadPool.Default`. Якщо потрібна ізоляція навантаження або обмеження кількості потоків, створіть власний екземпляр `TThreadPool`:

```pascal
var
  Pool: TThreadPool;
begin
  Pool := TThreadPool.Create;
  try
    Pool.SetMaxWorkerThreads(4);
    TParallel.&For(1, 1000, @ProcessItem, Pool);
  finally
    Pool.Free; // гарантовано дочікується фактичного завершення всіх потоків пулу
  end;
end;
```

### Правила безпеки для `TThreadPool`
1. **Гарантія `Pool.Free`**: усі робочі потоки та потік-монітор належать пулу (`FreeOnTerminate = False`) і приєднуються (`WaitFor`) у `TThreadPool.Destroy`. Після повернення з `Pool.Free` жоден потік пулу більше не виконується.
2. **Обробник `OnThreadTerminate`**: викликається в контексті робочого потоку, що завершується. Оскільки `Destroy` чекає на приєднання кожного потоку, **не викликайте `TThread.Synchronize`** з `OnThreadTerminate`, якщо `Pool.Free` може викликатися з головного потоку (інакше виникне взаємне блокування).

---

## 8. Верифікація та тестування багатопоточного коду

1. **Перевірка гонок даних під Valgrind Helgrind / DRD**:
   - Модуль `fpvalgrind` (пакет `fcl-base`) надає клієнтські запити `VgHappensBefore`, `VgHappensAfter`, `VgCleanMemory`, `VgBenignRace`, `VgForgetAll`, `VgRunningOnValgrind`.
   - При компіляції з `-dFPC_VALGRIND` (`tests/concurrency/build-testcompat-valgrind.sh`) примітиви `TSpinLock` і `System.Threading` автоматично передають Helgrind/DRD інформацію про синхронізацію.
2. **Перевірки у робочих потоках під час модульного тестування**:
   - Не викликайте `Assert*` безпосередньо в робочих потоках `fpcunit`: використовуйте `ThreadCheck` та `ThreadCheckSame` із `TThreadedTestCase` (`packages/vcl-compat/tests/utthreading.pp`), які потокобезпечно фіксують помилки й звітують про них у `TearDown` після приєднання потоків.


