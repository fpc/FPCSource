# ADR 0004: Примітиви синхронізації Фази 2 в модулі `SyncObjs`

* **Статус:** Прийнято (Реалізовано)
* **Дата:** 2026-10-19
* **Модуль:** `packages/fcl-base/src/syncobjs.pp`

## Контекст і проблема

Стандартний модуль `SyncObjs` у Free Pascal містив базові примітиви синхронізації (`TCriticalSection`, `TEvent`, `TSemaphore`, `TMutex`, `TSpinWait`, `TSpinLock`, `TLightweightEvent`, `TLightweightSemaphore`), але не мав високорівневих примітивів координації потоків, які є стандартом у сучасних мовах та платформах (.NET `System.Threading`, Go channels, C++20 `<barrier>` / `<latch>` / `<shared_mutex>`, Rust `std::sync`):

1. **Динамічний fork-join (`TCountdownEvent`)**: розробникам доводилося вручну поєднувати `InterlockedDecrement`, `TCriticalSection` та `TEvent`, що часто призводило до гонитв між обнуленням лічильника та викликом `Reset`/`AddCount`.
2. **Багатофазна синхронізація (`TBarrier`)**: ітеративні алгоритми (чисельне моделювання, релаксація графів, паралельні матричні обчислення) потребують циклічного бар'єра з атомарною пост-фазною дією (`TBarrierPhaseAction`). Використання одного `TEvent` для кількох фаз поспіль спричиняє класичну помилку «швидкий потік скидає подію до того, як повільний потік прокинувся з попередньої фази».
3. **Швидке блокування читання/запису (`TSlimReaderWriterLock`)**: існуючий `TMultiReadExclusiveWriteSynchronizer` у `SysUtils` є важким і не має швидкого атомарного шляху в просторі користувача та RAII-гардів.
4. **Передавання повідомлень за моделлю CSP (`TChannel<T>`)**: відсутність потокобезпечних типізованих каналів змушувала розробників будувати власні черги з `TCriticalSection` і двома `TEvent`, де легко припуститися помилок взаємного блокування (deadlock) під час закриття каналу (`Close`) або при обмеженій ємності (backpressure).

## Архітектурні рішення

### 1. `TCountdownEvent`
- Використовує атомарне читання для `CurrentCount` та `IsSet` у швидкому шляху.
- Зміни стану між `0` та `> 0` координуються через легковаговий `TSpinLock` і `TSimpleEvent`, що унеможливлює гонитву між `Signal` (перехід у `0`), `TryAddCount` (відмова при `0`) та `Reset`.
- Анотовано макросами `VgHappensBefore` / `VgHappensAfter` при компіляції з `-dFPC_VALGRIND`, завдяки чому Helgrind/DRD коректно бачать відношення happens-before між потоками, що викликали `Signal`, та потоком, що прокинувся з `WaitFor`.

### 2. `TBarrier` з подвійним буфером подій (`FPhaseEvents[0..1]`)
- Кожна фаза `P` використовує власний екземпляр `TSimpleEvent` за індексом `P and 1` (ping-pong схема).
- Останній учасник фази `P`:
  1. Виконує `FPostPhaseAction(Self)` у блоці `try..except`. Якщо виникає виняток, його текст зберігається у `FPhaseExceptions[P and 1]`.
  2. Скидає подію **наступної** фази `FPhaseEvents[(P + 1) and 1].ResetEvent` ще до пробудження потоків поточної фази.
  3. Просуває `FCurrentPhaseNumber := P + 1`, відновлює `FRemainingParticipants := FParticipantCount` і сигналізує `FPhaseEvents[P and 1].SetEvent`.
- Якщо `FPostPhaseAction` згенерував виняток, **усі** учасники фази `P` (і останній потік, і ті, що чекали) після виходу з очікування отримують `EBarrierPostPhaseException`.

### 3. `TSlimReaderWriterLock` та RAII-гарди (`TSlimReadLockGuard`, `TSlimWriteLockGuard`)
- Стан блокування кодується в одному 32-бітному слові `FState`:
  - Біт `WRITER_HELD = $40000000` — ексклюзивне володіння записувачем.
  - Маска `READER_MASK = $000FFFFF` — кількість активних читачів (до 1 048 575 одночасних читачів).
- **Lock-free fast path**:
  - `TryAcquireReadFast` виконує `InterlockedCompareExchange` лише якщо немає активного записувача і `FWaitingWriters = 0` (пріоритет записувачів — захист від writer starvation).
  - `TryAcquireWriteFast` атомарно переводить `FState` з `0` у `WRITER_HELD`.
- **Slow path**: коротке адаптивне очікування через `TSpinWait` (до 10 ітерацій), після чого потік переходить до очікування на `FReadEvent` або `FWriteEvent`.
- **RAII-гарди**: реалізовані як керовані записи (`record`) з операторами `Initialize` та `Finalize`, а також методами `Init(aLock)` і `Release`, що гарантує автоматичне зняття блокування при виході з області видимості або винятку.

### 4. `TCustomChannel` + `generic TChannel<T>`
- Щоб уникнути дублювання коду при спеціалізації дженериків та обійти обмеження FPC щодо доступу дженериків у секції `interface` до символів секції `implementation` (`Resourcestring`, `fpvalgrind`), не-узагальнена логіка синхронізації винесена в базовий клас `TCustomChannel`.
- `generic TChannel<T>` реалізує кільцевий буфер `FBuffer: array of T`:
  - При `aCapacity = 0` (необмежений канал) буфер динамічно подвоюється методом `GrowBuffer` із перевпорядкуванням елементів з `FHead..FTail` у `0..FCount-1`.
  - При `aCapacity > 0` (обмежений канал) розмір буфера фіксований, а `Send` блокується на `FNotFullEvent`, забезпечуючи природний backpressure.
  - При читанні звільнена комірка буфера очищується `FBuffer[FHead] := Default(T)`, щоб коректно звільняти посилання на керовані типи (`string`, `AnsiString`, інтерфейси, динамічні масиви) і не створювати витоків пам'яті.
- `TChannelEnumerator` реалізовано як стековий `record` (без алокації в купі), що підтримує ідіоматичний цикл `for Msg in Channel do`.

## Наслідки

- **Переваги**: повна сумісність з архітектурою `SyncObjs` та `System.Threading`, нульові накладні витрати на купу для `for..in` по каналу та RAII-гардів, відсутність хибних спрацьовувань у Valgrind Helgrind/DRD.
- **Обмеження**: `TSlimReaderWriterLock` є нерекурсивним (non-reentrant) для максимальної швидкодії в просторі користувача; повторне захоплення тим самим потоком без звільнення не підтримується (відповідає поведінці `SRWLOCK` у Windows та `std::sync::RwLock` у Rust).

