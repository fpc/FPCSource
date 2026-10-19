# Посібник користувача: Нові примітиви синхронізації `SyncObjs` (Фаза 2)

Модуль `SyncObjs` (`packages/fcl-base/src/syncobjs.pp`) розширено чотирма сучасними примітивами синхронізації та координації потоків:

1. **`TCountdownEvent`** — легковагова синхронізація завершення динамічної кількості підзадач.
2. **`TBarrier`** — багатофазна циклічна синхронізація групи потоків із підтримкою пост-фазної дії (`TBarrierPhaseAction`).
3. **`TSlimReaderWriterLock`** (разом із RAII-гардами **`TSlimReadLockGuard`** та **`TSlimWriteLockGuard`**) — швидке блокування читання/запису в просторі користувача з пріоритетом записувачів.
4. **`generic TChannel<T>`** — потокобезпечні обмежені та необмежені канали передавання повідомлень за моделлю CSP (Communicating Sequential Processes) у стилі Go з підтримкою закриття (`Close`) та перебору `for Msg in Channel do`.

---

## 1. `TCountdownEvent` — динамічний лічильник завершення підзадач

### Призначення
`TCountdownEvent` розблоковує потоки, що викликали `WaitFor`, коли його внутрішній лічильник досягає нуля (`CurrentCount = 0`). На відміну від фіксованого `TTask.WaitForAll`, кількість активних робочих елементів можна динамічно збільшувати під час виконання за допомогою `AddCount` або `TryAddCount`.

### Основний інтерфейс
```pascal
type
  TCountdownEvent = class(TSynchroObject)
  public
    constructor Create(aInitialCount: Integer = 1);
    function Signal(aSignalCount: Integer = 1): Boolean;
    procedure AddCount(aSignalCount: Integer = 1);
    function TryAddCount(aSignalCount: Integer = 1): Boolean;
    procedure Reset; overload;
    procedure Reset(aCount: Integer); overload;
    function WaitFor(aTimeout: Cardinal = INFINITE): TWaitResult; overload;
    function WaitFor(const aTimeout: TTimeSpan): TWaitResult; overload;
    property CurrentCount: Integer read GetCurrentCount;
    property InitialCount: Integer read FInitialCount;
    property IsSet: Boolean read GetIsSet;
  end;
```

### Типовий шаблон використання (з початковим токеном реєстрації)
Щоб уникнути передчасного обнулення лічильника, коли перша підзадача завершується швидше, ніж головний потік встигає запустити наступну, створюйте `TCountdownEvent.Create(1)` і викликайте фінальний `Signal` у головному потоці після циклу запуску:

```pascal
var
  CDE: TCountdownEvent;
  I: Integer;
begin
  CDE := TCountdownEvent.Create(1); // 1 = токен реєстрації головного потоку
  try
    for I := 1 to 16 do
    begin
      CDE.AddCount(1);
      TTask.Run(
        procedure
        begin
          try
            DoDynamicSubtask;
          finally
            CDE.Signal;
          end;
        end);
    end;
    CDE.Signal;  // Знімаємо токен реєстрації
    CDE.WaitFor; // Чекаємо завершення всіх 16 підзадач
  finally
    CDE.Free;
  end;
end;
```

---

## 2. `TBarrier` — пофазна синхронізація ітеративних алгоритмів

### Призначення
`TBarrier` дозволяє групі з $N$ потоків виконувати багатокрокові (ітеративні) алгоритми синхронними фазами: чисельні методи на сітках (рівняння теплопровідності, клітинні автомати), алгоритми на графах (PageRank, BFS за рівнями), паралельне сортування та матричні обчислення.

### Основний інтерфейс
```pascal
type
  TBarrierPhaseAction = reference to procedure(aBarrier: TBarrier);
  EBarrierPostPhaseException = class(ESyncObjectException);

  TBarrier = class(TSynchroObject)
  public
    constructor Create(aParticipantCount: Integer;
      const aPostPhaseAction: TBarrierPhaseAction = nil);
    function AddParticipant: Int64;
    function AddParticipants(aParticipantCount: Integer): Int64;
    procedure RemoveParticipant;
    procedure RemoveParticipants(aParticipantCount: Integer);
    function SignalAndWait(aTimeout: Cardinal = INFINITE): Boolean; overload;
    function SignalAndWait(const aTimeout: TTimeSpan): Boolean; overload;
    property CurrentPhaseNumber: Int64 read GetCurrentPhaseNumber;
    property ParticipantCount: Integer read GetParticipantCount;
    property ParticipantsRemaining: Integer read GetParticipantsRemaining;
  end;
```

### Особливості реалізації
- **Post-Phase Action**: якщо в конструктор передано `aPostPhaseAction`, цей делегат виконується **рівно одним потоком** (останнім учасником, що прийшов до бар'єра у поточній фазі) після того, як усі потоки завершили крок фази, але **до того**, як усі потоки будуть розблоковані для наступної фази. Це ідеальне місце для обміну буферів (`Swap(GridCurr, GridNext)`), агрегації похибки або перевірки критерію збіжності.
- **Проброс винятків**: якщо `aPostPhaseAction` згенерує виняток, усі учасники поточної фази отримають `EBarrierPostPhaseException`.
- **Захист від гонитв між фазами**: всередині використано подвійний буфер подій `FPhaseEvents[0..1]` за парністю номера фази (`Phase and 1`), що виключає ситуацію, коли швидкий потік у фазі $P+1$ скидає подію до того, як повільний потік прокинувся з фази $P$.

---

## 3. `TSlimReaderWriterLock` та RAII-гарди

### Призначення
`TSlimReaderWriterLock` призначений для захисту структур даних, які часто читаються багатьма потоками одночасно і рідко модифікуються (кеші, таблиці маршрутизації, конфігурації, індекси).

### Основний інтерфейс
```pascal
type
  TSlimReaderWriterLock = class(TSynchroObject)
  public
    procedure BeginRead;
    function TryBeginRead(aTimeout: Cardinal = 0): Boolean; overload;
    function TryBeginRead(const aTimeout: TTimeSpan): Boolean; overload;
    procedure EndRead;
    procedure BeginWrite;
    function TryBeginWrite(aTimeout: Cardinal = 0): Boolean; overload;
    function TryBeginWrite(const aTimeout: TTimeSpan): Boolean; overload;
    procedure EndWrite;
    property ReaderCount: Integer read GetReaderCount;
    property IsWriteLocked: Boolean read GetIsWriteLocked;
  end;

  TSlimReadLockGuard = record
    procedure Init(aLock: TSlimReaderWriterLock);
    procedure Release;
  end;

  TSlimWriteLockGuard = record
    procedure Init(aLock: TSlimReaderWriterLock);
    procedure Release;
  end;
```

### Переваги над класичними блокуваннями
1. **Швидкий шлях у просторі користувача (User-Space Fast Path)**: за відсутності конкуренції `BeginRead`/`EndRead` та `BeginWrite`/`EndWrite` виконуються однією атомарною інструкцією `InterlockedCompareExchange` без жодного системного виклику ядра ОС.
2. **Анти-голодування записувачів (Writer Preference)**: коли потік викликає `BeginWrite` і чекає на завершення поточних читачів, лічильник `FWaitingWriters` блокує вхід нових читачів, гарантуючи швидке отримання ексклюзивного доступу.
3. **Автоматичні RAII-гарди**: керовані записи `TSlimReadLockGuard` і `TSlimWriteLockGuard` автоматично викликають `EndRead`/`EndWrite` у `class operator Finalize` при виході з процедури або виникненні винятку.

```pascal
procedure ReadSensorOffset(SensorID: Integer; out Offset: Double);
var
  Guard: TSlimReadLockGuard;
begin
  Guard.Init(CalibrationLock);
  Offset := CalibrationTable[SensorID];
end; // Автоматичний виклик CalibrationLock.EndRead у Finalize(Guard)
```

---

## 4. `TChannel<T>` — потокобезпечні CSP-канали в стилі Go

### Призначення
`generic TChannel<T>` реалізує філософію CSP (*«Не обмінюйтеся даними через спільну пам'ять; натомість діліться пам'яттю, передаючи повідомлення»*). Підтримує схему MPMC (Multiple Producers, Multiple Consumers — багато виробників і багато споживачів).

### Основний інтерфейс
```pascal
type
  EChannelClosedException = class(ESyncObjectException);

  generic TChannel<T> = class(TCustomChannel)
  public
    constructor Create(aCapacity: Integer = 0);
    procedure Send(const aItem: T);
    function TrySend(const aItem: T; aTimeout: Cardinal = 0): Boolean; overload;
    function TrySend(const aItem: T; const aTimeout: TTimeSpan): Boolean; overload;
    function Receive(out aItem: T): Boolean; overload;
    function Receive: T; overload;
    function TryReceive(out aItem: T; aTimeout: Cardinal = 0): Boolean; overload;
    function TryReceive(out aItem: T; const aTimeout: TTimeSpan): Boolean; overload;
    procedure Close;
    function GetEnumerator: TChannelEnumerator; inline;
    property Count: Integer read GetCount;
    property Capacity: Integer read FCapacity;
    property IsClosed: Boolean read GetIsClosed;
  end;
```

### Режими роботи та `for..in`
- **Необмежений канал (`TChannel<T>.Create(0)`)**: `Send` ніколи не блокує виробника — внутрішній кільцевий буфер динамічно розширюється.
- **Обмежений канал (`TChannel<T>.Create(Capacity)`)**: коли в буфері накопичується `Capacity` елементів, `Send` блокується до звільнення місця споживачем, забезпечуючи автоматичне регулювання потоку (**backpressure**).
- **Закриття та ітерація `for..in`**: виклик `Channel.Close` сигналізує про завершення потоку даних. Усі елементи, які були надіслані до `Close`, гарантовано дочитуються споживачами, після чого цикл `for Msg in Channel do` коректно завершується:

```pascal
type
  TIntChannel = specialize TChannel<Integer>;
var
  Ch: TIntChannel;
  Item: Integer;
begin
  Ch := TIntChannel.Create(32);
  try
    TTask.Run(
      procedure
      var I: Integer;
      begin
        for I := 1 to 100 do
          Ch.Send(I * I);
        Ch.Close; // Сигнал споживачам про кінець потоку
      end);

    for Item in Ch do
      ProcessValue(Item);
  finally
    Ch.Free;
  end;
end;
```

