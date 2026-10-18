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
- `docs/concurrency/examples/`: приклади порівняння «класичний TThread vs
  System.Threading» (`primes_*`, `report_*`, `batch_processing.pp`,
  `showcase_pipeline.pp`) і розділ 10 у PDF.
- `fcl-base/src/fpvalgrind.pp`: клієнтські запити valgrind (happens-before/after,
  clean memory, benign race); анотації в `TSpinLock` і `System.Threading`
  під `-dFPC_VALGRIND`. Див. `docs/concurrency/valgrind.md`.
- `tests/concurrency/build-testcompat-valgrind.sh`, `fpc-helgrind.supp`,
  `hgsummary.py`: збірка й аналіз прогонів helgrind/drd.
- Документація: `docs/concurrency/` (огляд, шаблон ADR, базова лінія тестів).
- `tests/concurrency/fpcenv.sh`: середовище для компіляції тестів свіжозібраним компілятором.
- `tests/concurrency/build-testcompat.sh`: збірка тестів vcl-compat з поточних джерел.
- `tests/concurrency/stress.sh`: стрес-прогін наборів `System.Threading` (N повторів, таймаут).
