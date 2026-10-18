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

### Додано
- `fcl-base/src/fpvalgrind.pp`: клієнтські запити valgrind (happens-before/after,
  clean memory, benign race); анотації в `TSpinLock` і `System.Threading`
  під `-dFPC_VALGRIND`. Див. `docs/concurrency/valgrind.md`.
- `tests/concurrency/build-testcompat-valgrind.sh`, `fpc-helgrind.supp`,
  `hgsummary.py`: збірка й аналіз прогонів helgrind/drd.
- Документація: `docs/concurrency/` (огляд, шаблон ADR, базова лінія тестів).
- `tests/concurrency/fpcenv.sh`: середовище для компіляції тестів свіжозібраним компілятором.
- `tests/concurrency/build-testcompat.sh`: збірка тестів vcl-compat з поточних джерел.
- `tests/concurrency/stress.sh`: стрес-прогін наборів `System.Threading` (N повторів, таймаут).
