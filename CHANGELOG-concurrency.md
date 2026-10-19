# Журнал змін: гілка feature/concurrency

## [Unreleased]

### Виправлено
- `TThreadPool` (`System.Threading`): на «холодному» пулі `TParallel.For` випадково
  виконувався **послідовно** (255 ітерацій × 80 мс: 20,7 с замість 1,7 с).
  Репліки циклу потрапляли в локальну чергу потоку з кореневою задачею, а вільні
  потоки заглядали в чужі черги лише після сигналу `FQueueEvent`, якого не було,
  поки пул ще створював потоки; без сигналу вони спали до 40 с (`IdleTimeout`).
  Тепер потік перед сном скидає подію і перевіряє чужі черги, а забравши роботу
  при наявності ще незавершених запитів, будить наступний потік.
  Регресійний тест: `TTestParallel.TestForColdPoolParallel`.
- `TParallel.For` (`Integer` і `Int64`): ітерації могли **губитися або виконуватися
  двічі**, при цьому `TLoopResult.Completed = True`. Потік забирав діапазон
  ітерацій з одним значенням кроку (`GetCurrentStart`), а виконував діапазон,
  обчислений з іншим, прочитаним окремо, тоді як інші потоки збільшували крок
  (`GetNextStride`). Тепер `GetCurrentStart(out aStride)` повертає крок, з яким
  діапазон забрано. Помилка була в trunk і раніше; стала помітною після
  попереднього виправлення, коли цикл почав справді працювати паралельно.
  Регресійні тести: `TTestParallel.TestForEachIterationOnce`, `…Once64`
  (200 000 ітерацій × 200 раундів, кожна має виконатися рівно один раз;
  з поверненою гонкою тест падає в 10 з 10 запусків).

- `TTask.GetIsComplete` (швидкий шлях `TTask.Wait`): читання стану без
  блокування не мало бар'єра пам'яті; на ARM64 потік міг побачити задачу
  завершеною раніше за її результат. Додано `ReadBarrier`.
- `TThreadPool.Destroy`: use-after-free. `StopCPUStats` скидав
  `FMonitorStatus` до завершення монітора, `Destroy` звільняв пул, а потік
  монітора потім писав у звільнену пам'ять. Статус тепер скидається атомарно
  останньою дією монітора.
- `TTask.WaitForAny`: результат (`Res`) читався без блокування в умовах
  циклів очікування; тепер під тим самим `Lock`, що й запис.
- Тести `utthreading.pp`: глобальний `Random` викликався з потоків пулу;
  замінено на потокобезпечний `ThreadRandom`.
- `TThreadPool.Destroy` не чекав фактичного завершення потоків: робочі потоки
  й монітор були `FreeOnTerminate` і виходили зі списку пулу у власному
  деструкторі, тож `Destroy` повертав керування, поки вони ще виконували
  деструктор і завершальну частину RTL. Для пулу за замовчуванням це
  перетиналося з фіналізацією програми (вимкнення менеджера пам'яті).
  Тепер потоки належать пулу (`FreeOnTerminate := False`): завершуючись,
  потік переходить у `FFinishedThreads` (раніше, ніж залишає `FThreads`),
  а пул приєднує й звільняє їх (`ReapFinishedThreads`) у моніторі та в
  `Destroy`; монітор зберігається в `FMonitorThread`.
  Регресійний тест: `TTestThreading.TestFreeJoinsThreads` (до виправлення:
  після `Pool.Free` живих потоків 25 замість 1).
- RTL `rtl/unix/tthread.inc` (`ThreadFunc`): новий потік повторно записував
  `FSuspendedInternal := true` без синхронізації; запис міг відбутися після
  того, як `Resume` у потоці-творці вже скинув прапорець. Рядок прибрано
  (значення встановлює `SysCreate` до старту потоку).
- RTL `rtl/unix/cthreads.pp` (`RemoveTidEntries`, `DoneTidEntries`):
  `ThreadTidList` перевірявся без `ThreadTidLock`; тепер лише під блокуванням.
- Тести `utthreading.pp`: потоки тестів (`TNotifyThread`, `TTaskThread`) були
  `FreeOnTerminate` і вважалися завершеними за `OnTerminate`, тобто до
  фактичного завершення; тепер `TThreadedTestCase.TearDown` їх приєднує й звільняє.
- Тести `utthreading.pp`: `Assert*` викликався з робочих потоків
  (`DoSimpleExecute`, `DoBurnCyclesExecute`, `OnTask`, `CalcIntegerEvent`).
  Виняток виникав у потоці, а не в тесті, тож невдала перевірка могла
  залишитися непоміченою; лічильники `fpcunit` не потокобезпечні. Тепер
  ці місця використовують `ThreadCheck`/`ThreadCheckSame`: невдача
  записується під блокуванням, а `TThreadedTestCase.TearDown` після
  приєднання потоків валить тест з переліком невдач.
  Тест механізму: `TTestThreading.TestThreadCheckRecordsFailure`.
- `EAggregateException`: перебір `for E in Agg` видавав зайвий елемент за
  межами масиву (`MoveNext` перевіряв межу до збільшення індексу), а після
  `Add` — ще й порожні місця запасу місткості; програма падала з
  `Access violation`. Тепер перебираються рівно `Count` помилок.
  Тести: `TTestAggregateException.TestEnumerate`, `TestEnumerateAfterAdd`.
- `TWorkStealingQueue.TrySteal`: при `aTimeout = 0` на порожній черзі зайво
  викликалися `FEvent.ResetEvent` і `FEvent.WaitFor(0)` (два системні виклики)
  під спін-блокуванням списку черг пулу, через що вільні потоки переходили в
  `Sleep(1)` у `TSpinWait` і приєднувалися до коротких циклів по одному.
  Тест: `TTestWorkStealingQueue.TestStealNoWaitIsCheap`.
- `TParallel.For` з `TLoopState`, `Break`, `Stop`:
  - `TLoopState` ніколи не створювався (`nil`), тож кожен цикл із параметром
    стану падав з `Access violation`; тепер кожна репліка створює власний
    `TLoopState32`/`TLoopState64` в `Invoke`.
  - `TLoopParams.Break` завжди повертав `False`, тож `FBreakAt` не
    оновлювався; `ShouldExitLoop(CurrentIter)` у режимі `objfpc` читав змінну
    результату замість виклику `ShouldExitLoop()`, тому `Stop` і винятки не
    зупиняли цикл; у `Invoke` викликався `TLoopParams.Break` замість
    `system.Break` та перевірявся `ShouldExitLoop(Start)` замість
    `ShouldExitLoop(I)`.
  - Тести: `TTestParallel.TestForWithState`, `TestForBreak`, `TestForStop`.
- `TParallel.For` і винятки в ітераціях:
  - `TExceptionList.Create(Initial, aCapacity)` не збільшував `Count` і не
    розгортав `EAggregateException`;
  - `FTasksWithExceptions` зберігав необліковувані `TTask` замість `ITask`,
    через що дочірня репліка з винятком знищувалася потоком пулу при
    `Itm := Nil` разом із `FException` ще до того, як коренева задача збирала
    помилки, а `ReplicaCallUserCode` двічі викликав `Complete`.
  - Тест: `TTestParallel.TestForExceptionPreserved`.
- `TThreadPool.Destroy` / `GetWorkItemForThread`: потік міг скинути
  `FQueueEvent.ResetEvent` після `FQueueEvent.SetEvent` у `Destroy`, залишаючи
  інші потоки спати до `IdleTimeout` (40 с).
- `TParallel.Join` (`TProcJoinTask`, `TEventJoinTask`):
  - локальний запис `Params: TTaskParams` не обнулявся (`Default(TTaskParams)`),
    через що поле `Params.Event` містило сміття зі стеку;
  - `Params.Parent := Self` робив задачу дочірньою для самої себе
    (`FSubTasks = 2`), через що `Wait` зависав назавжди;
  - `TParallel.Join` не запускав створену задачу (`.Start`).
  - Тест: `TTestParallel.TestJoin`.

### Додано
- **Фаза 1.1 (`packages/vcl-compat/src/system.threading.pp`) — неблокувальні продовження задач (`ContinueWith` / `ThenBy`) та кооперативне скасування (`TCancellationToken`):**
  - `TCancellationTokenSource`, `TCancellationToken` та `TCancellationTokenRegistration` —
    легковаговий потокобезпечний механізм кооперативного скасування з підрахунком посилань
    (`ICancellationState`), неблокувальним читанням `IsCancellationRequested`,
    таймаутним скасуванням `CancelAfter` (та конструкторами з `Cardinal` / `TTimeSpan`),
    комбінованими джерелами `CreateLinkedTokenSource`, методом `Reset`, подією
    `WaitHandle` і миттєвими зворотними викликами `Register` / `Unregister`.
  - Підтримка передавання `TCancellationToken` у `TTask.Create`, `TTask.Run`,
    `TTask.Future<T>`, `TParallel.&For` (32-бітні та 64-бітні перевантаження) та
    `TParallel.Join`, а також автоматичне успадкування контекстного токена дочірніми
    задачами та вкладеними циклами від `TTask.CurrentTask`.
  - `ITask.ContinueWith` та `IFuture<T>.ContinueWith` з прапорцями
    `TTaskContinuationOption` / `TTaskContinuationOptions` (`NotOnCompleted`,
    `NotOnFaulted`, `NotOnCanceled`, `OnlyOnCompleted`, `OnlyOnFaulted`,
    `OnlyOnCanceled`, `ExecuteSynchronously`).
  - Типізовані неблокувальні конвеєри `.ThenBy<TResult>` і `.ThenBy<T, TResult>`
    (через `TITaskHelper = type helper for ITask` та `TTask.ThenBy<T, TResult>`) і
    метод `IFuture<T>.Map` із автоматичним прокиданням винятків (`PropagateExceptionFrom`)
    та стану скасування крізь усі етапи конвеєра без блокування потоків пулу.
  - Набір тестів `TTestContinuationsAndCancellation` у `packages/vcl-compat/tests/utthreading.pp`,
    демонстраційна програма `docs/concurrency/examples/continuations_cancel.pp` та ADR-0005.
- **Фаза 2 (`packages/fcl-base/src/syncobjs.pp`) — нові примітиви синхронізації:**
  - `TCountdownEvent` — легковагова синхронізація завершення динамічної кількості
    підзадач (`Signal`, `AddCount`, `TryAddCount`, `Reset`, `WaitFor`,
    `CurrentCount`, `InitialCount`, `IsSet`) з атомарним швидким шляхом та
    анотаціями Valgrind (`VgHappensBefore` / `VgHappensAfter`).
  - `TBarrier` — пофазна циклічна синхронізація групи потоків для ітеративних
    паралельних алгоритмів (`SignalAndWait`, `AddParticipant`, `AddParticipants`,
    `RemoveParticipant`, `RemoveParticipants`, `CurrentPhaseNumber`,
    `ParticipantCount`, `ParticipantsRemaining`) з подвійним буфером подій
    `FPhaseEvents[0..1]`, підтримкою пост-фазної дії `TBarrierPhaseAction` та
    пробросом `EBarrierPostPhaseException` усім учасникам фази.
  - `TSlimReaderWriterLock` та RAII-гарди `TSlimReadLockGuard`,
    `TSlimWriteLockGuard` — швидке блокування читання/запису в просторі
    користувача (lock-free атомарний швидкий шлях `TryAcquireReadFast` /
    `TryAcquireWriteFast`, короткий `TSpinWait`, пріоритет записувачів проти
    голодування `FWaitingWriters` та автоматичне звільнення через `Finalize`).
  - `TCustomChannel` і `generic TChannel<T>` — потокобезпечні обмежені
    (`aCapacity > 0`, із автоматичним backpressure) та необмежені
    (`aCapacity = 0`, із динамічним розширенням кільцевого буфера) канали за
    моделлю CSP у стилі Go (`Send`, `TrySend`, `Receive`, `TryReceive`, `Close`,
    `EChannelClosedException`) та стековий перелічувач `TChannelEnumerator` для
    циклу `for Msg in Channel do` без алокацій у купі.
  - Набір тестів `TTestSyncObjsPhase2` у `packages/vcl-compat/tests/utthreading.pp`,
    демонстраційна програма `docs/concurrency/examples/syncobjs_phase2.pp`,
    посібник `docs/concurrency/guide-syncobjs.md`, ADR-0004 та fpdoc-опис
    `packages/fcl-base/xml/syncobjs-phase2.xml`.
- `docs/concurrency/examples/`: приклади порівняння «класичний TThread vs
  System.Threading» (`primes_*`, `report_*`, `batch_processing.pp`,
  `showcase_pipeline.pp`, `syncobjs_phase2.pp`, `continuations_cancel.pp`) і розділ 10 у PDF.
- `fcl-base/src/fpvalgrind.pp`: клієнтські запити valgrind (happens-before/after,
  clean memory, benign race); анотації в `TSpinLock` і `System.Threading`
  під `-dFPC_VALGRIND`. Див. `docs/concurrency/valgrind.md`.
- `tests/concurrency/build-testcompat-valgrind.sh`, `fpc-helgrind.supp`,
  `hgsummary.py`: збірка й аналіз прогонів helgrind/drd.
- Документація: `docs/concurrency/` (огляд, шаблон ADR, базова лінія тестів).
- `tests/concurrency/fpcenv.sh`: середовище для компіляції тестів свіжозібраним компілятором.
- `tests/concurrency/build-testcompat.sh`: збірка тестів vcl-compat з поточних джерел.
- `tests/concurrency/stress.sh`: стрес-прогін наборів `System.Threading` (N повторів, таймаут).
- **Фаза 1.4 (`packages/fcl-async/` та компілятор `compiler/ppcx64`) — корутини, файбери та async / await:**
  - **Крок 1 (бібліотечний)**: новий пакет `fcl-async` (`packages/fcl-async/`):
    - `TFiber` (`async.fiber.pp`): легковагові стекові корутини (user-space fibers) для System V AMD64 ABI з надшвидким перемиканням контексту на вбудованому асемблері (`FiberSwitchContext`), збереженням і відновленням callee-saved регістрів (`%rbx, %rbp, %r12-%r15`) та покажчика стеку `%rsp` (<10 нс).
    - Гарантоване 16-байтне вирівнювання стеку корутини згідно з System V AMD64 ABI (`%rsp % 16 == 8` на вході в процедуру), що запобігає апаратним аваріям інструкцій SSE/AVX (`movaps`).
    - Динамічна релокація глобальних змінних threadvar винятків FPC (`ExceptAddrStack`, `ExceptObjectStack`) через `FPC_THREADVAR_RELOCATE`, повна симетрична ізоляція стеку винятків для кожного файбера та безпечна міграція файбера між різними потоками ОС.
    - Методи `TFiber.Await<T>(IFuture<T>)` та `TFiber.Await(ITask)` для неблокуючого призупинення корутини без блокування системного потоку ОС, `TFiber.Sleep(ms, loop)` через таймери `fcl-eventloop` та неблокуюче читання/запис сокетів `TFiber.AwaitRead` / `TFiber.AwaitWrite`.
    - `generic TGenerator<T>` (`async.generator.pp`): ледачі генератори послідовностей на базі корутин із зворотним викликом `YieldValue`, повною підтримкою `IEnumerator<T>` / `IEnumerable<T>` та прямим обходом у циклі `for..in`.
    - Фундамент скінченних автоматів (`async.statemachine.pp`): `generic TTaskCompletionSource<TResult>` (`Task`, `SetResult`, `SetException`, `SetCanceled`), `TAsyncTaskMethodBuilder<TResult>` та `TAsyncTaskBuilder` за контрактом C# Roslyn.
    - Модульні тести `packages/fcl-async/tests/utasync.pp` (12 тестів: `TTestFibers`, `TTestGenerators`, `TTestStateMachines`), скрипт `tests/concurrency/build-async.sh`.
  - **Крок 2 (у компіляторі `compiler/ppcx64`)**:
    - Додано процедурний модифікатор/директиву `async` (`pio_async` у `timplprocoption` в `compiler/symconst.pas`, парсер `compiler/pdecsub.pas`) для маркування процедур та функцій, що повертають `ITask` або `specialize IFuture<T>`.
    - Додано контекстний оператор `await` (`compiler/pexpr.pas`) для очікування майбутніх задач у виразах (`Val := await Future;` автоматично читає властивість `.Value`) та задачах (`await Task;` викликає `.Wait`).
    - Збережено 100% сумісність двійкових файлів PPU (`tppuset1` / `tppuset8`) та чинних модулів RTL завдяки контекстному аналізу без додавання зарезервованих слів у загальний лексер.
  - Демонстраційний приклад `docs/concurrency/examples/coroutine_showcase.pp`, посібник користувача `docs/concurrency/guide-async.md` та оновлений повний довідник `docs/concurrency/modern-concurrency-reference.md`.
- **Розділ 2.1 (`compiler/ppcx64`) — Виведення типів для узагальнених методів (Generic Type Inference) та необов'язковий `specialize`:**
  - Додано новий перемикач режимів `{$modeswitch implicitgenerics}` (у `globtype.pas`, `scanner.pas`, `ppudump.pp`).
  - У режимі `objfpc` під `{$modeswitch implicitgenerics}` дозволено опускати ключове слово `specialize`:
    - Для узагальнених типів (наприклад, `IFuture<string>`, `TFPGList<Integer>`, `TBox<T>`);
    - Для явних викликів методів і процедур (наприклад, `Identity<string>('hello')`, `TTask.Future<string>(...)`).
  - Реалізовано виведення типів для параметрів узагальнених методів і функцій (Generic Type Inference):
    - Для звичайних значень аргументів (`Identity('hello')`, `TBoxFactory.Wrap(12345)`);
    - Для анонімних функцій (`anonymous functions`) та посилань на функції (`reference to function/procedure`, `is_invokable` інтерфейси), зокрема виведення параметра `<TResult>` із типу повернення функції (`returndef`) та вирівнювання прихованих параметрів (`vo_is_hidden_para`).
    - Підтримка виведення у багатопотокових ланцюжках задач `TTask.Future(function: string begin Result := 'ok'; end)` та `task.ThenBy(...)`.
  - Модульний тестовий набір `tests/concurrency/test_implicit_generics.pp`.
- **Розділ 2.2 (`compiler/ppcx64`) — Короткий синтаксис лямбда-виразів (`=>`):**
  - Додано новий перемикач режимів `{$modeswitch arrowfunctions}` (у `globtype.pas`, `scanner.pas`, `ppudump.pp`), що автоматично активує `anonymousfunctions` та `functionreferences`.
  - Додано токен `_ARROW` (`=>`) та реалізовано підтримку стрілкових функцій у парсері компілятора (`compiler/pexpr.pas`, `compiler/psub.pas`):
    - Безпараметричні лямбди: `() => Expr`;
    - Один аргумент без дужок або з дужками: `I => Stmt`, `(I) => Stmt`;
    - Кілька аргументів із контекстним виведенням типів: `(A, B) => A + B`;
    - Явно типізовані аргументи зі специфікаторами (`const`, `var`, `out`): `(A: Integer, B: Integer) => A + B`;
    - Опціональна явна декларація типу повернення: `(X): Integer => X * 2`;
    - Тіло-вираз (Expression body) з автоприсвоєнням результату для функцій або виконанням дії для процедур, включаючи коректну роботу з керованими типами (`string`, інтерфейси, динамічні масиви);
    - Тіло-блок (Block body): `(I) => begin ... end;`.
  - Повна інтеграція зі збіркою `System.Threading` (`TParallel.&For`, `TTask.Future`), колекціями та подіями зі скороченням шаблонного коду у 2–3 рази.
  - Повний модульний тестовий набір: `tests/concurrency/test_arrow_functions.pp`.
  - Окремий посібник користувача: `docs/concurrency/guide-arrow-functions.md`.

