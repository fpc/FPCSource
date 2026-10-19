# Довідник API та Посібник з Конкурентності у Free Pascal
### Сучасна модель багатопоточності, синхронізації, реактивного I/O та async/await (Фази 1.1, 1.2, 1.3, 1.4)

Цей документ побудовано за стандартами документації **Microsoft Learn (C# / .NET Runtime)** та **Qt Documentation (Qt Concurrent / Qt Core)**. Він містить концептуальний опис, детальний опис класів та інтерфейсів, сигнатури методів, оцінку алгоритмічної складності, гарантії потокобезпеки, типові помилки та готові приклади коду для всіх введених примітивів.

---

## Зміст

1. [Огляд архітектури та матриця вибору парадигми](#1-огляд-архітектури-та-матриця-вибору-парадигми)
2. [Пакет `vcl-compat` (`System.Threading`): Задачі, Продовження та Скасування](#2-пакет-vcl-compat-systemthreading)
   - [2.1. `TCancellationTokenSource` та `TCancellationToken`](#21-tcancellationtokensource-та-tcancellationtoken)
   - [2.2. `ITask` та `TTask`: Створення та Життєвий Цикл](#22-itask-та-ttask)
   - [2.3. Неблокувальні продовження: `ContinueWith`, `ThenBy<TResult>`, `Map`](#23-неблокувальні-продовження-continuewith-thenby-map)
   - [2.4. `IFuture<T>`: Асинхронні типізовані значення](#24-ifuturet)
   - [2.5. `TParallel.For` та `TParallel.Join`](#25-tparallelfor-та-tparalleljoin)
   - [2.6. `TThreadPool`: Архітектура пулу потоків](#26-tthreadpool)
3. [Пакет `fcl-base` (`SyncObjs`): Нові примітиви синхронізації (Фаза 2)](#3-пакет-fcl-base-syncobjs)
   - [3.1. `TCountdownEvent`](#31-tcountdownevent)
   - [3.2. `TBarrier`](#32-tbarrier)
   - [3.3. `TSlimReaderWriterLock` та RAII-гарди](#33-tslimreaderwriterlock-та-raii-гарди)
   - [3.4. `generic TChannel<T>` (CSP-канали у стилі Go)](#34-generic-tchannelt)
4. [Пакет `fcl-eventloop`: Масштабований асинхронний I/O (Фаза 1.3)](#4-пакет-fcl-eventloop)
   - [4.1. Архітектура реакторів: epoll, io_uring, kqueue, IOCP](#41-архітектура-реакторів)
   - [4.2. `IEventLoop` та `TEventLoop`](#42-ieventloop-та-teventloop)
   - [4.3. `TEventLoopGroup`: Multi-Reactor на 100 000+ з'єднань](#43-teventloopgroup)
   - [4.4. `IAsyncSocket` та `IAsyncServerSocket`](#44-iasyncsocket-та-iasyncserversocket)
   - [4.5. `IAsyncPipe`, `IAsyncFile` та `TAsyncBufferPool`](#45-iasyncpipe-iasyncfile-та-tasyncbufferpool)
5. [Пакет `fcl-async` та Компілятор: Корутини, Файбери та async/await (Фаза 1.4)](#5-пакет-fcl-async-та-компілятор-корутини-файбери-та-asyncawait-фаза-14)
   - [5.1. `TFiber`: Стекові корутини поверх EventLoop та IFuture](#51-tfiber-стекові-корутини-поверх-eventloop-та-ifuture)
   - [5.2. `generic TGenerator<T>`: Ледачі послідовності та for..in](#52-generic-tgeneratort-ледачі-послідовності-та-forin)
   - [5.3. `TTaskCompletionSource<T>` та скінченні автомати](#53-ttaskcompletionsourcet-та-скінченні-автомати)
   - [5.4. Компілятор: Директива `async` та оператор `await`](#54-компілятор-директива-async-та-оператор-await)
6. [Комплексний приклад: Наскрізний реактивний конвеєр](#6-комплексний-приклад-наскрізний-реактивний-конвеєр)
7. [Правила безпеки, діагностика та Valgrind](#7-правила-безпеки-діагностика-та-valgrind)

---

## 1. Огляд архітектури та матриця вибору парадигми

Сучасний Free Pascal (`feature/concurrency`) надає три рівні конкурентності, оптимізовані під різні класи задач:

```
┌────────────────────────────────────────────────────────────────────────┐
│                        Free Pascal Concurrency Stack                   │
└────────────────────────────────────────────────────────────────────────┘
       ▲                                 ▲                       ▲
       │                                 │                       │
┌───────────────┐              ┌──────────────────┐    ┌─────────────────┐
│ CPU-Intensive │              │ Thread-Safe Comm │    │   I/O-Bound     │
│  (Data/Tasks) │              │  & Coordination  │    │  (Network/Disk) │
├───────────────┤              ├──────────────────┤    ├─────────────────┤
│ System.       │              │ SyncObjs         │    │ fcl-eventloop   │
│ Threading     │              │ (Phase 2)        │    │ (Phase 1.3)     │
│               │              │                  │    │                 │
│ • TParallel   │              │ • TChannel<T>    │    │ • IEventLoop    │
│ • ITask       │◄────────────►│ • TCountdownEvent│◄──►│ • TEventLoop    │
│ • IFuture<T>  │              │ • TBarrier       │    │   Group         │
│ • ContinueWith│              │ • TSlimRWLock    │    │ • IAsyncSocket  │
│ • CancellationToken          │ • RAII Guards    │    │ • epoll/io_uring│
└───────────────┘              └──────────────────┘    └─────────────────┘
```

### Матриця вибору примітиву («When to use what»)

| Сценарій / Задача | Рекомендований примітив | Чому саме він | Чого слід уникати |
|---|---|---|---|
| **Паралельна обробка масиву/діапазону (CPU)** | `TParallel.For` | Динамічний крок (work-stealing), балансування ядер, збір `EAggregateException`. | Ручне створення масиву `TThread` (надмірні накладні витрати). |
| **Асинхронне фонове обчислення з результатом** | `IFuture<T>` + `.ThenBy<TResult>` | Не блокує робочий потік пулу, підтримка лінійних конвеєрів. | Виклик `.Value` одразу після запуску (блокує потік). |
| **Координація завершення $N$ динамічних задач** | `TCountdownEvent` | Дозволяє динамічно додавати задачі (`AddCount`), $O(1)$ сигналізація. | `TTask.WaitForAll` (не підтримує зміну кількості задач). |
| **Пофазна ітеративна синхронізація групи потоків** | `TBarrier` | Автоматичний перехід фаз, пост-фазний бар'єрний колбек, подвійний буфер. | Ручна пара з двох подій (ризик стану гонки між фазами). |
| **Часте читання з рідкісним оновленням даних** | `TSlimReaderWriterLock` + `TSlimReadLockGuard` | Lock-free атомарний швидкий шлях у просторі користувача, нуль системних викликів, захист від голодування записувачів. | `TCriticalSection` (серіалізує читачів) або `pthread_rwlock` (повільний syscall). |
| **Передавання повідомлень між потоками (CSP)** | `generic TChannel<T>` | Буферизований або небуферизований канал, backpressure, безпечне закриття, `for Msg in Channel do`. | Ручна черга на базі `TList` + `TCriticalSection` (ризик дедлоку, відсутність backpressure). |
| **Мережевий сервер із 10 000 – 100 000+ клієнтів** | `fcl-eventloop` (`TEventLoopGroup`) | Нативні `epoll` / `io_uring` / `kqueue` / `IOCP`, витрата пам'яті ~2 КБ на клієнта, не потребує окремого стеку потоку на з'єднання. | Модель thread-per-connection (`ssockets` / блокуючі сокети — падіння ядра через вичерпання пам'яті). |
| **Стекові корутини (user-space fibers) та послідовний асинхронний I/O** | `TFiber` (`fcl-async`) | Швидке перемикання контексту AMD64 (10 нс), неблокуючий `TFiber.Await<T>`, `Sleep` через EventLoop. | Жорсткий паралелізм (файбер виконується на одному потоці до `Yield`). |
| **Ледачі послідовності з підтримкою `for..in`** | `generic TGenerator<T>` (`fcl-async`) | Генератор зі зворотним викликом `YieldValue`, сумісний з `IEnumerator<T>`. | Важкі міжпотокові конвеєри (використовуйте `TChannel<T>`). |
| **Природний асинхронний код без callback hell** | `async` / `await` (компілятор trunk) | Процедурна директива `async`, оператор `await` розгортає `IFuture<T>.Value` або очікує `ITask.Wait`. | Блокуючі системні виклики. |

---

## 2. Пакет `vcl-compat` (`System.Threading`)

Модуль надає реалізацію Task Parallel Library (TPL), оптимізовану для Free Pascal із розширеннями для неблокувальних продовжень та токенів скасування.

```pascal
{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
{$modeswitch advancedrecords}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, System.Threading;
```

---

### 2.1. `TCancellationTokenSource` та `TCancellationToken`

> **Аналог у C#:** `System.Threading.CancellationTokenSource`, `System.Threading.CancellationToken`  
> **Призначення:** Легковаговий потокобезпечний кооперативний механізм узгодженого скасування ієрархій задач, конвеєрів та операцій I/O.

#### Опис класу `TCancellationTokenSource`

```pascal
type
  TCancellationTokenSource = class
  public
    constructor Create; overload;
    constructor Create(MillisecondsDelay: Cardinal); overload;
    constructor Create(const Delay: TTimeSpan); overload;
    destructor Destroy; override;

    procedure Cancel;
    procedure CancelAfter(MillisecondsDelay: Cardinal); overload;
    procedure CancelAfter(const Delay: TTimeSpan); overload;
    procedure Reset;

    class function CreateLinkedTokenSource(constref Token1, Token2: TCancellationToken): TCancellationTokenSource; overload; static;
    class function CreateLinkedTokenSource(const Tokens: array of TCancellationToken): TCancellationTokenSource; overload; static;

    property Token: TCancellationToken read GetToken;
    property IsCancellationRequested: Boolean read GetIsCancellationRequested;
  end;
```

#### Опис запису `TCancellationToken`

```pascal
type
  TCancellationToken = record
  public
    function IsCancellationRequested: Boolean; inline;
    function CanBeCanceled: Boolean; inline;
    procedure ThrowIfCancellationRequested;
    function Register(const AProc: TProcRef): TCancellationTokenRegistration; overload;
    function Register(const ACallback: TNotifyEvent; AData: Pointer = nil): TCancellationTokenRegistration; overload;
    function WaitHandle: PRTLEvent;

    class function None: TCancellationToken; static;
  end;
```

#### Потокобезпека та характеристики виконання
- `TCancellationToken` є структурою (`record`), що містить інтерфейсне посилання `ICancellationState` з автоматичним підрахунком посилань. Передавання токена за значенням не спричиняє витоків пам'яті, навіть якщо `TCancellationTokenSource` було знищено.
- Перевірка `Token.IsCancellationRequested` виконує атомарне читання з бар'єром пам'яті (`ReadBarrier`) — складність $O(1)$, нуль системних викликів, безпечно викликати в щільних циклах.
- Метод `ThrowIfCancellationRequested` генерує виняток `EOperationCancelled`.

#### Приклад використання

```pascal
procedure ProcessDataWithCancellation(const Data: array of Integer);
var
  CTS: TCancellationTokenSource;
  Token: TCancellationToken;
begin
  CTS := TCancellationTokenSource.Create;
  try
    { Автоматичне скасування за таймаутом через 3 секунди }
    CTS.CancelAfter(3000);
    Token := CTS.Token;

    TParallel.&For(Low(Data), High(Data),
      procedure(I: Integer; State: TParallel.TLoopState)
      begin
        { Кооперативна перевірка в кожній ітерації }
        Token.ThrowIfCancellationRequested;

        DoHeavyWork(Data[I]);
      end,
      Token); { Передаємо токен скасування в цикл }

    WriteLn('Обробку успішно завершено.');
  except
    on E: EOperationCancelled do
      WriteLn('Операцію скасовано користувачем або за таймаутом!');
  end;
  finally
    CTS.Free;
  end;
end;
```

---

### 2.2. `ITask` та `TTask`

> **Аналог у C#:** `System.Threading.Tasks.Task`  
> **Аналог у Qt:** `QFuture`, `QRunnable`  
> **Призначення:** Одиниця асинхронної роботи, що планується у пулі потоків `TThreadPool`.

#### Опис інтерфейсу `ITask`

```pascal
type
  TTaskStatus = (Created, WaitingToRun, Running, Completed, WaitingForChildren, Canceled, Exception);

  ITask = interface
    ['{84C1C6C5-C3F3-4FE9-8230-0BC6D88ECB4B}']
    function GetId: Integer;
    function GetStatus: TTaskStatus;
    function Start: ITask;
    procedure Cancel;
    function Wait(Timeout: Cardinal = INFINITE): Boolean;
    function ContinueWith(const AAction: TProc<ITask>; Options: TTaskContinuationOptions = []): ITask;
    property Id: Integer read GetId;
    property Status: TTaskStatus read GetStatus;
  end;
```

#### Фабричні методи `TTask`

| Метод | Опис |
|---|---|
| `TTask.Create(Proc: TProc): ITask` | Створює задачу у стані `Created`. Потребує явного виклику `.Start`. |
| `TTask.Run(Proc: TProc): ITask` | Створює та негайно ставить задачу в чергу пулу потоків. |
| `TTask.Run(Proc: TProc; constref Token: TCancellationToken): ITask` | Запускає задачу з прив'язкою до токена скасування. |
| `TTask.WaitForAll(Tasks: array of ITask; Timeout: Cardinal): Boolean` | Блокує до завершення всіх задач у списку. |
| `TTask.WaitForAny(Tasks: array of ITask; Timeout: Cardinal): Integer` | Повертає індекс першої завершеної задачі. |

---

### 2.3. Неблокувальні продовження: `ContinueWith`, `ThenBy`, `Map`

> **Аналог у C#:** `Task.ContinueWith`, `Task.ContinueWith<TResult>`, синтаксис ланцюжків Tasks  
> **Аналог у Qt:** `QFuture::then()`  
> **Призначення:** Побудова неблокувальних конвеєрів обробки без блокування робочих потоків пулу під час очікування результату попереднього етапу.

#### Прапорці продовжень (`TTaskContinuationOption`)

```pascal
type
  TTaskContinuationOption = (
    NotOnCompleted,       // Не виконувати, якщо попередник завершився успішно
    NotOnFaulted,         // Не виконувати, якщо виникла помилка
    NotOnCanceled,        // Не виконувати, якщо задачу було скасовано
    ExecuteSynchronously  // Виконати в тому ж потоці без черги пулу
  );
  TTaskContinuationOptions = set of TTaskContinuationOption;

const
  OnlyOnCompleted = [NotOnFaulted, NotOnCanceled];
  OnlyOnFaulted   = [NotOnCompleted, NotOnCanceled];
  OnlyOnCanceled  = [NotOnCompleted, NotOnFaulted];
```

#### Методи побудови конвеєра (`TITaskHelper`)

Завдяки хелперу `TITaskHelper` для інтерфейсу `ITask` конвеєрні виклики доступні як для звичайних задач `ITask`, так і для типізованих `IFuture<T>`:

```pascal
type
  TITaskHelper = type helper for ITask
  public
    function ThenBy<TResult>(const ATransform: TFunc<ITask, TResult>): specialize IFuture<TResult>; overload;
    function ThenBy<TResult>(const ATransform: TFunc<ITask, TResult>; Options: TTaskContinuationOptions): specialize IFuture<TResult>; overload;
  end;
```

#### Приклад побудови неблокувального конвеєра:

```pascal
var
  Pipeline: specialize IFuture<string>;
begin
  { Етап 1: Завантаження сирих даних (IFuture<TBytes>) }
  Pipeline := TTask.Future<TBytes>(
    function: TBytes
    begin
      Result := DownloadRawBytes('https://api.example.com/payload');
    end)

  { Етап 2: Неблокувальна декомпресія (TBytes -> string) }
  .specialize ThenBy<string>(
    function(Prev: ITask): string
    var
      Bytes: TBytes;
    begin
      Bytes := (Prev as specialize IFuture<TBytes>).Value;
      Result := DecompressGZipToString(Bytes);
    end, OnlyOnCompleted)

  { Етап 3: Обробка помилок або компенсація (виконується ТІЛЬКИ у разі збою) }
  .ContinueWith(
    procedure(Prev: ITask)
    begin
      WriteLn('Помилка конвеєра: ', (Prev as specialize IFuture<string>).Exception.Message);
    end, OnlyOnFaulted);

  { Робочі потоки пулу НЕ блокуються між Етапом 1 та Етапом 2! }
end;
```

---

### 2.4. `IFuture<T>`

> **Аналог у C#:** `Task<TResult>`  
> **Аналог у Qt:** `QFuture<T>`

```pascal
type
  IFuture<T> = interface(ITask)
    function GetValue: T;
    function Map(const ATransform: specialize TFunc<T, T>): specialize IFuture<T>;
    property Value: T read GetValue;
  end;
```

- Властивість `.Value`: повертає обчислене значення. Якщо обчислення ще не завершено, потік-викликач блокується до завершення. Якщо задача впала з помилкою, `.Value` повторно кидає оригінальний виняток або `EAggregateException`.
- Метод `.Map`: дозволяє трансформувати значення $T \to T$ без повторної спеціалізації конвеєра.

---

### 2.5. `TParallel.For` та `TParallel.Join`

#### `TParallel.For` з `TLoopState`

```pascal
var
  LoopRes: TParallel.TLoopResult;
begin
  LoopRes := TParallel.&For(0, 1000000,
    procedure(I: Int64; State: TParallel.TLoopState)
    begin
      if State.ShouldExit then Exit;

      if MatchesCriteria(I) then
      begin
        ReportMatch(I);
        State.Stop; { Негайне переривання циклу для всіх ядер }
      end;
    end);

  if not LoopRes.Completed then
    WriteLn('Цикл було перервано достроково.');
end;
```

---

## 3. Пакет `fcl-base` (`SyncObjs`): Нові примітиви синхронізації (Фаза 2)

```pascal
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, SyncObjs;
```

---

### 3.1. `TCountdownEvent`

> **Аналог у C#:** `System.Threading.CountdownEvent`  
> **Аналог у Qt:** `QCountDownTimer` / Semaphore barrier

#### Інтерфейс класу

```pascal
type
  TCountdownEvent = class(TSynchroObject)
  public
    constructor Create(aInitialCount: Integer = 1);
    destructor Destroy; override;

    function Signal(aSignalCount: Integer = 1): Boolean;
    procedure AddCount(aSignalCount: Integer = 1);
    function TryAddCount(aSignalCount: Integer = 1): Boolean;
    procedure Reset; overload;
    procedure Reset(aCount: Integer); overload;
    function WaitFor(aTimeout: Cardinal = INFINITE): TWaitResult;

    property CurrentCount: Integer read GetCurrentCount;
    property InitialCount: Integer read FInitialCount;
    property IsSet: Boolean read GetIsSet;
  end;
```

#### Принцип роботи
- Розблоковує всі очікуючі потоки (`WaitFor`), коли лічильник сягає `0`.
- Атомарний швидкий шлях: метод `Signal` зменшує лічильник через `InterlockedDecrement`. Системний виклик ядра для розбудження події здійснюється **лише в момент переходу через 0**.
- Підтримує динамічне додавання підзадач (`AddCount` / `TryAddCount`) під час виконання.

---

### 3.2. `TBarrier`

> **Аналог у C#:** `System.Threading.Barrier`  
> **Призначення:** Пофазна циклічна синхронізація групи з $N$ потоків в ітеративних паралельних алгоритмах (клітинні автомати, фізичні симуляції, обробка графів).

#### Інтерфейс класу

```pascal
type
  TBarrier = class;
  TBarrierPhaseAction = procedure(aBarrier: TBarrier) of object;

  TBarrier = class(TSynchroObject)
  public
    constructor Create(aParticipantCount: Integer; aPostPhaseAction: TBarrierPhaseAction = nil);
    destructor Destroy; override;

    function SignalAndWait(aTimeout: Cardinal = INFINITE): Boolean;
    procedure AddParticipant;
    procedure AddParticipants(aCount: Integer);
    procedure RemoveParticipant;
    procedure RemoveParticipants(aCount: Integer);

    property CurrentPhaseNumber: Int64 read GetCurrentPhaseNumber;
    property ParticipantCount: Integer read GetParticipantCount;
    property ParticipantsRemaining: Integer read GetParticipantsRemaining;
  end;
```

#### Гарантії та подвійний буфер подій
`TBarrier` використовує подвійний перемикач подій `FPhaseEvents[0..1]`. Це гарантує, що жоден швидкий потік нової фази не зможе помилково отримати сигнал від попередньої фази (виключає race condition між фазами).

---

### 3.3. `TSlimReaderWriterLock` та RAII-гарди

> **Аналог у C#:** `System.Threading.ReaderWriterLockSlim`  
> **Аналог у Qt:** `QReadWriteLock`, `QReadLocker`, `QWriteLocker`  
> **Призначення:** Високошвидкісне блокування читання/запису в просторі користувача без важких системних викликів `pthread_rwlock`.

#### Інтерфейс

```pascal
type
  TSlimReaderWriterLock = class
  public
    constructor Create;
    procedure AcquireRead;
    procedure ReleaseRead;
    procedure AcquireWrite;
    procedure ReleaseWrite;

    function TryAcquireRead(aTimeoutMs: Cardinal = 0): Boolean;
    function TryAcquireWrite(aTimeoutMs: Cardinal = 0): Boolean;

    property CurrentReadCount: Integer read GetCurrentReadCount;
    property IsWriteLockHeld: Boolean read GetIsWriteLockHeld;
    property WaitingWriterCount: Integer read GetWaitingWriterCount;
  end;
```

#### RAII-гарди (автоматичне звільнення на виході з блоку)

```pascal
type
  TSlimReadLockGuard = record
    class function Create(aLock: TSlimReaderWriterLock): TSlimReadLockGuard; static;
  end;

  TSlimWriteLockGuard = record
    class function Create(aLock: TSlimReaderWriterLock): TSlimWriteLockGuard; static;
  end;
```

#### Приклад безпечного використання через RAII:

```pascal
var
  CacheLock: TSlimReaderWriterLock;
  Cache: TDictionary;

function ReadCache(const Key: string): string;
var
  Guard: TSlimReadLockGuard;
begin
  Guard := TSlimReadLockGuard.Create(CacheLock);
  { Автоматичне захоплення читання.
    При виході з функції (навіть за винятком) замок миттєво звільниться! }
  Result := Cache[Key];
end;

procedure UpdateCache(const Key, Val: string);
var
  Guard: TSlimWriteLockGuard;
begin
  Guard := TSlimWriteLockGuard.Create(CacheLock);
  { Пріоритет записувача запобігає голодуванню при інтенсивному читанні }
  Cache[Key] := Val;
end;
```

---

### 3.4. `generic TChannel<T>` (CSP-канали у стилі Go)

> **Аналог у Go:** `chan T`  
> **Аналог у C#:** `System.Threading.Channels.Channel<T>`  
> **Призначення:** Потокобезпечне передавання повідомлень за моделлю акторів/CSP з підтримкою закриття та перебору `for Msg in Channel do`.

#### Інтерфейс класу

```pascal
type
  generic TChannel<T> = class(TCustomChannel)
  public
    constructor Create(aCapacity: Integer = 0); { 0 = Unbounded, >0 = Bounded }
    destructor Destroy; override;

    procedure Send(constref aItem: T);
    function TrySend(constref aItem: T): Boolean;
    function Receive: T;
    function TryReceive(out aItem: T): Boolean;
    procedure Close;

    function GetEnumerator: TChannelEnumerator;
    property Capacity: Integer read FCapacity;
    property Count: Integer read GetCount;
    property IsClosed: Boolean read FIsClosed;
  end;
```

#### Приклад: Патерн Producer-Consumer з `for..in`:

```pascal
type
  TIntChannel = specialize TChannel<Integer>;

var
  Chan: TIntChannel;
begin
  Chan := TIntChannel.Create(100); { Обмежений буфер: 100 елементів }

  { Фоновий генератор (Producer) }
  TTask.Run(
    procedure
    var
      I: Integer;
    begin
      try
        for I := 1 to 1000 do
          Chan.Send(I); { Автоматичний backpressure при заповненні буфера }
      finally
        Chan.Close; { Сигнал споживачам про завершення даних }
      end;
    end);

  { Споживач (Consumer) у головному потоці }
  for Msg in Chan do
  begin
    ProcessMessage(Msg);
  end;
  { Цикл природно завершується, коли Chan закрито і він спорожнів! }

  Chan.Free;
end;
```

---

## 4. Пакет `fcl-eventloop`: Масштабований асинхронний I/O (Фаза 1.3)

```pascal
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  eventloop.types, eventloop.core, eventloop.sockets, eventloop.pipes, eventloop.files, fcleventloop;
```

---

### 4.1. Архітектура реакторів

Пакет автоматично вибирає найшвидший нативний механізм ядра:
- **Linux**: `epoll` ($O(1)$) з пробудженням `eventfd2` та безпосередній `io_uring` із SQ/CQ кільцями через `mmap`.
- **macOS / BSD**: `kqueue` з фільтрами `EVFILT_READ` / `EVFILT_WRITE`.
- **Windows**: `IOCP` (I/O Completion Ports).

---

### 4.2. `IEventLoop` та `TEventLoop`

```pascal
type
  TLoopBackend = (bkAuto, bkEpoll, bkIOUring, bkKqueue, bkIOCP);

  IEventLoop = interface
    function GetBackend: TLoopBackend;
    procedure Run;
    procedure RunOnce(ATimeoutMs: Integer = -1);
    procedure Stop;
    procedure Wakeup;
    procedure Post(const AAction: TProc);
    function SetTimer(AIntervalMs: Cardinal; const ACallback: TTimerCallback; APeriodic: Boolean = False): Int64;
    function CancelTimer(ATimerId: Int64): Boolean;
    property Backend: TLoopBackend read GetBackend;
  end;
```

---

### 4.3. `TEventLoopGroup`: Multi-Reactor на 100 000+ з'єднань

Модель Multi-Reactor дозволяє утилізувати всі ядра процесора без міжпотокової синхронізації на сокетах:

```
[ Мережеві пакети ]
        │
        ▼
┌──────────────────┐
│ Acceptor Thread  │ ──► Приймає TCP Handshake (AsyncAccept)
└──────────────────┘
        │
   (Round-Robin)
        │
        ├──► [ Worker Reactor 1 ] ──► Обслуговує 25,000 сокетів (CPU 0)
        ├──► [ Worker Reactor 2 ] ──► Обслуговує 25,000 сокетів (CPU 1)
        ├──► [ Worker Reactor 3 ] ──► Обслуговує 25,000 сокетів (CPU 2)
        └──► [ Worker Reactor 4 ] ──► Обслуговує 25,000 сокетів (CPU 3)
```

```pascal
var
  Group: IEventLoopGroup;
  AcceptorLoop: IEventLoop;
  Server: IAsyncServerSocket;
begin
  { Пул із 4 робочих потоків реактора }
  Group := TEventLoop.CreateGroup(4, bkAuto);
  Group.Start;

  AcceptorLoop := TEventLoop.Create(bkAuto);
  Server := TEventLoop.CreateTcpServer(AcceptorLoop);
  Server.Listen('0.0.0.0', 8080);

  Server.AsyncAccept(
    procedure(AClientHandle: TLoopHandle; const AClientAddr: string; AClientPort: Word; AStatus: Integer)
    var
      WorkerLoop: IEventLoop;
      Client: IAsyncSocket;
    begin
      if AStatus <> 0 then Exit;

      { Передаємо дескриптор клієнта робочому реактору }
      WorkerLoop := Group.NextLoop;
      Client := TAsyncSocket.Create(WorkerLoop, AClientHandle, True);
      Client.AsyncRead(...);
    end);

  AcceptorLoop.Run;
end;
```

---

### 4.4. `IAsyncSocket` та `IAsyncServerSocket`

#### Підтримка зворотних викликів та Future:

```pascal
type
  IAsyncSocket = interface
    procedure AsyncRead(ABuffer: Pointer; ACount: NativeInt; const AProc: TAsyncReadProc);
    procedure AsyncWrite(ABuffer: Pointer; ACount: NativeInt; const AProc: TAsyncWriteProc);
    function ReadAsync(ABuffer: Pointer; ACount: NativeInt; constref ACancellationToken: TCancellationToken): specialize IFuture<NativeInt>;
    function WriteAsync(ABuffer: Pointer; ACount: NativeInt; constref ACancellationToken: TCancellationToken): specialize IFuture<NativeInt>;
    procedure Close;
  end;
```

- **Eager Write**: `AsyncWrite` здійснює спробу миттєвого неблокуючого виклику `send()`/`fpsend()`. Якщо буфер сокета має вільне місце, запис виконується без жодного звернення до `epoll_ctl` (zero overhead).
- **Інтеграція з `IFuture` та `TCancellationToken`**: метод `ReadAsync` повертає `IFuture<NativeInt>`, який можна об'єднувати у конвеєри `.ThenBy` та передавати токени з таймаутом.

---

## 5. Пакет `fcl-async` та Компілятор: Корутини, Файбери та async/await (Фаза 1.4)

Пакет `fcl-async` (`packages/fcl-async/`) разом із доопрацюваннями компілятора FPC trunk реалізує гібридну модель асинхронного програмування:
1. **Бібліотечні стекові корутини (`TFiber`)**: надшвидке перемикання контексту у System V AMD64 ABI (до 10 нс), вирівнювання стеку за 16-байтним стандартом, ізоляція системних стеків винятків FPC (`ExceptAddrStack`, `ExceptObjectStack`) через dynamic threadvar relocation та безпечна міграція файберів між системними потоками ОС.
2. **Генератори (`TGenerator<T>`)**: ледачі послідовності з методом `YieldValue`, сумісні з інтерфейсами `IEnumerator<T>` / `IEnumerable<T>` та синтаксисом `for..in`.
3. **Фундамент скінченних автоматів (`TTaskCompletionSource<T>`, `TAsyncTaskMethodBuilder<T>`)**: реалізація C# Roslyn-сумісного контракту обіцянок.
4. **Компіляторні директиви `async` та оператор `await`**: процедурний модифікатор `async` у `timplprocoption` та контекстний оператор `await`, який автоматично розгортає значення `IFuture<T>.Value` або очікує завершення `ITask.Wait`.

### 5.1. `TFiber`: Стекові корутини поверх EventLoop та IFuture

```pascal
type
  TFiberState = (fsCreated, fsRunning, fsSuspended, fsCompleted, fsFaulted);
  TFiberProc = procedure of object;

  TFiber = class
  public
    constructor Create(AProc: TFiberProc; AStackSize: Cardinal = 128 * 1024; ALoop: IEventLoop = nil);
    destructor Destroy; override;
    procedure Resume;
    class procedure Yield;
    class function Await<T>(const AFuture: specialize IFuture<T>): T;
    class procedure Await(const ATask: ITask);
    class procedure Sleep(AMilliseconds: Cardinal; ALoop: IEventLoop = nil);
    property State: TFiberState read FState;
  end;
```

#### Приклад: Неблокуюче очікування Future у файбері
```pascal
TFiber.Create(
  procedure
  var
    Fut: specialize IFuture<string>;
    ResultStr: string;
  begin
    Fut := TTask.specialize Future<string>(
      function: string
      begin
        Sleep(50);
        Result := 'Обчислення завершено';
      end);

    // Файбер призупиняється (yield) і звільняє потік ОС до завершення Future
    ResultStr := TFiber.specialize Await<string>(Fut);
    WriteLn('Отримано: ', ResultStr);
  end).Resume;
```

### 5.2. `generic TGenerator<T>`: Ледачі послідовності та for..in

```pascal
type
  TIntGen = specialize TGenerator<Integer>;

procedure Fibonacci(const Yield: TIntGen.TYieldProc);
var
  A, B, Temp, I: Integer;
begin
  A := 0; B := 1;
  for I := 1 to 10 do
  begin
    Yield(B);
    Temp := A + B; A := B; B := Temp;
  end;
end;

var
  Gen: TIntGen;
  Val: Integer;
begin
  Gen := TIntGen.Create(@Fibonacci);
  try
    for Val in Gen do
      Write(Val, ' ');
    WriteLn;
  finally
    Gen.Free;
  end;
end;
```

### 5.3. `TTaskCompletionSource<T>` та скінченні автомати

`TTaskCompletionSource<T>` створює об'єкт `specialize IFuture<T>`, чий результат може бути виставлений пізніше через `.SetResult(Value)`, `.SetException(Ex)` або `.SetCanceled`.

### 5.4. Компілятор: Директива `async` та оператор `await`

У компіляторі trunk `compiler/ppcx64`:
- Процедурна директива `async` маркує процедури та функції, що повертають `ITask` або `specialize IFuture<T>`:
  ```pascal
  procedure BackgroundTask; async;
  begin
    WriteLn('Асинхронний метод');
  end;

  function FetchNumberAsync: specialize IFuture<Integer>; async;
  begin
    Result := TTask.specialize Future<Integer>(
      function: Integer begin Result := 42; end);
  end;
  ```
- Оператор `await <expr>` розпаковує типізовані `IFuture<T>` або очікує виконання `ITask`:
  ```pascal
  var
    V: Integer;
    T: ITask;
  begin
    V := await FetchNumberAsync; // V отримує значення Integer безпосередньо
    T := TTask.Run(procedure begin DoWork; end);
    await T;                     // Очікує завершення задачі
  end;
  ```

---

## 6. Комплексний приклад: Наскрізний реактивний конвеєр

Поєднання **`fcl-eventloop`** (отримання даних із мережі), **`TChannel<T>`** (безпечний буфер передавання) та **`System.Threading`** (паралельна обробка в пулі потоків):

```pascal
program ReactivePipelineShowcase;
{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, SyncObjs, System.Threading,
  eventloop.types, eventloop.core, eventloop.sockets, fcleventloop;

type
  TPacketChannel = specialize TChannel<string>;

var
  Channel: TPacketChannel;
  Loop: IEventLoop;
  Server: IAsyncServerSocket;
  CTS: TCancellationTokenSource;

begin
  CTS := TCancellationTokenSource.Create;
  Channel := TPacketChannel.Create(1000);

  { 1. Пул робочих задач: забирає пакети з каналу та обчислює у пулі потоків }
  TTask.Run(
    procedure
    var
      Packet: string;
    begin
      for Packet in Channel do
      begin
        { Паралельна обробка пакета без блокування мережевого циклу }
        TTask.Run(
          procedure
          begin
            ProcessPacketPayload(Packet);
          end, CTS.Token);
      end;
    end);

  { 2. Мережевий реактор: приймає дані на високій швидкості }
  Loop := TEventLoop.Create(bkAuto);
  Server := TEventLoop.CreateTcpServer(Loop);
  Server.Listen('127.0.0.1', 9000);

  Server.AsyncAccept(
    procedure(AHandle: TLoopHandle; const AAddr: string; APort: Word; AStatus: Integer)
    var
      Client: IAsyncSocket;
      Buffer: array[0..2047] of AnsiChar;
    begin
      if AStatus <> 0 then Exit;
      Client := TAsyncSocket.Create(Loop, AHandle, True);
      Client.AsyncRead(@Buffer[0], SizeOf(Buffer),
        procedure(ABytesRead: NativeInt; AReadStatus: Integer)
        begin
          if (AReadStatus = 0) and (ABytesRead > 0) then
          begin
            { Миттєво передаємо дані у чергу CSP каналу }
            Channel.Send(Copy(Buffer, 1, ABytesRead));
          end
          else
            Client.Close;
        end);
    end);

  WriteLn('Сервер запущено. Натисніть Ctrl+C для зупинки.');
  Loop.Run;

  Channel.Close;
  CTS.Cancel;
  CTS.Free;
  Channel.Free;
end.
```

---

## 7. Правила безпеки, діагностика та Valgrind

1. **Не блокуйте потік Event Loop**: Усі тривалі обчислення, звернення до БД або блокуючий I/O передавайте у `TTask.Run` або `TChannel<T>`. Потік реактора повинен лише пересилати байти.
2. **Уникайте захоплення змінних циклу за посиланням**: В анонімних функціях усередині циклів копіюйте лічильник у локальну змінну (`var LocalI := I;`) або використовуйте параметри процедури.
3. **Чистота під Valgrind / Helgrind**: Усі нові примітиви (`TCancellationToken`, `TCountdownEvent`, `TBarrier`, `TSlimReaderWriterLock`, `TChannel`) протестовано з ключем `-dFPC_VALGRIND` і містять вбудовані анотації `VgHappensBefore` / `VgHappensAfter`. Жодних хибних спрацювань детектора гонок даних!
