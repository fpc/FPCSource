# Пошук гонок даних: valgrind helgrind / drd

## Навіщо анотації

`System.Threading` синхронізує потоки здебільшого без `pthread_mutex`:
`TSpinLock` (цикл на `CompareExchange`), атомарні лічильники, кеш об'єктів.
Helgrind і DRD цього не бачать і видають сотні хибних «possible data race».

Модуль `fcl-base/src/fpvalgrind.pp` надсилає valgrind клієнтські запити:

| Процедура | Запит valgrind | Де використано |
|---|---|---|
| `VgHappensBefore(p)` | `ANNOTATE_HAPPENS_BEFORE` | `TSpinLock.Exit`, кінець монітора пулу |
| `VgHappensAfter(p)` | `ANNOTATE_HAPPENS_AFTER` | `TSpinLock.Enter/TryEnter`, `TTask.GetIsComplete`, `WaitForMonitorThread` |
| `VgCleanMemory(p, n)` | `VALGRIND_HG_CLEAN_MEMORY` | об'єкт узято з кешу `TThreadPool.Caches` |
| `VgBenignRace(p, n)` | `VALGRIND_HG_DISABLE_CHECKING` | прапорці, які свідомо читаються без блокування |

Код анотацій компілюється лише з `-dFPC_VALGRIND`; у звичайній збірці його
немає. Запити реалізовано для x86_64 (System V ABI); на інших платформах це
порожні процедури. Без valgrind запит — кілька інструкцій без ефекту.

## Як запустити

```sh
tests/concurrency/build-testcompat-valgrind.sh            # -> tests/concurrency/out-valgrind/
valgrind --tool=helgrind \
  --suppressions=tests/concurrency/fpc-helgrind.supp \
  --log-file=hg.log \
  tests/concurrency/out-valgrind/testcompat --suite=TTestTask --format=plain
tests/concurrency/hgsummary.py hg.log                     # групування конфліктів
```

Нюанси:

- **`Assertion 'in_rx' failed`.** FPC з `-gw` експортує мітки
  налагоджувальної інформації (`_$UNIT$_LdNNN`) як глобальні символи секцій
  `.debug_*`; їхні значення — зміщення в цих секціях. valgrind 3.22 не
  перевіряє секцію і, коли зміщення випадково потрапляє в діапазон адрес коду,
  аварійно завершується ще до запуску програми. Тому падіння з'являється
  «випадково» після будь-якої зміни коду. Скрипт збірки прибирає ці символи
  (`objcopy --strip-symbols`); імена функцій і рядки у звітах лишаються.
  Перевіряйте лог на `Assertion`: інакше «0 конфліктів» може означати, що
  тести взагалі не запускались. (Раніше це помилково приписувалось
  `threadvar` — насправді він лише змінював розміщення символів.)
- Анотації змінюють inline-методи `TSpinLock` (інтерфейс `syncobjs`), тому
  `fcl-base` і всі залежні пакети компілюються з джерел (див. скрипт).
- Під helgrind програма в 50–100 разів повільніша: тести з перевіркою часу
  (`TestForColdPoolParallel`) і 40 млн ітерацій (`TestForEachIterationOnce`)
  під ним не запускати.

## Винятки (`tests/concurrency/fpc-helgrind.supp`)

Лише свідомо прийнята поведінка RTL:
`cthreads.CDoneCriticalSection` розблоковує м'ютекс, доки вдається (сумісність
з Delphi), що дає `UnlockUnlocked` / `PthAPIerror` (EPERM).

## Результати (2026-10-18)

Програма: `TParallel.For`, 5 × 2000 ітерацій з `AtomicIncrement`.

| Етап | Конфліктів |
|---|---|
| Без анотацій | ~600–1500 у ~120 місцях |
| + `TSpinLock` | 17 у 13 групах |
| + виправлення й анотації `System.Threading` | **0 у `System.Threading`**; 7 у RTL |

Знайдено й виправлено в `System.Threading`:

1. **`TTask.GetIsComplete` без бар'єра пам'яті.** Швидкий шлях `Wait` читає
   стан без блокування. На x86 це працює, але на ARM64 потік міг побачити
   «завершено» раніше за результат задачі. Додано `ReadBarrier`.
2. **Use-after-free під час знищення пулу.** `StopCPUStats` скидав
   `FMonitorStatus` ще до кінця роботи монітора; `Destroy` бачив це й звільняв
   пул, після чого потік монітора писав `FThreadPool.FMonitorStatus` у
   звільнену пам'ять. Тепер статус скидається лише в останньому рядку
   монітора, атомарно (`AtomicExchange`), а `WaitForMonitorThread` має
   `ReadBarrier`.

3. **`TTask.WaitForAny` читав результат без блокування** в умовах циклів
   очікування (`Res=-1`), тоді як колбеки завершення пишуть його під `Lock`.
   Тепер читання теж під `Lock` (`NoResult`).

Також виправлено тести: допоміжні процедури викликали глобальний `Random`
(не потокобезпечний) з потоків пулу. Замінено на `ThreadRandom`.

### Набори тестів під helgrind (анотована збірка з `cmem`)

| Набір | До виправлень | Після |
|---|---|---|
| TTestThreading | 105 | 18 |
| TTestWorkStealingQueue | 56 | 7 |
| TTestSparseArray | 43 | 5 |
| TTestTask | 113 | 20 |

Усі тести під helgrind проходять. Решта конфліктів — поза `System.Threading`
(див. нижче).

## Виправлено після аналізу RTL-груп (2026-10-19)

| Група | Рішення | Стан |
|---|---|---|
| Потоки пулу живуть довше за `Destroy` / програму | Потоки належать пулу (`FreeOnTerminate := False`), `Destroy` приєднує їх (`WaitFor`) і звільняє; тест `TestFreeJoinsThreads` | ✅ виправлено |
| `TThread.Resume` ↔ `ThreadFunc` (`rtl/unix/tthread.inc`) | прибрано повторний запис `FSuspendedInternal` у новому потоці | ✅ виправлено |
| `cthreads.RemoveTidEntries` / `DoneTidEntries` | `ThreadTidList` читається лише під `ThreadTidLock` | ✅ виправлено |
| Тестові потоки (`TNotifyThread`, `TTaskThread`) | та сама вада в тестах: тепер приєднуються в `TearDown` | ✅ виправлено |
| `Assert*` з робочих потоків у тестах | `ThreadCheck`/`ThreadCheckSame` записують невдачу, `TearDown` валить тест | ✅ виправлено |
| `pthread_mutex_init` ↔ `TThread.Terminate` | пам'ять звільненого об'єкта перевикористано; helgrind не вважає free/malloc синхронізацією | ⚪ хибна тривога |
| `TThread.WaitFor` ↔ `ThreadFunc` (`FFinished`) | `WaitFor` у головному потоці опитує прапорець `FFinished`, далі `pthread_join` дає синхронізацію | ⚪ доброякісне опитування в RTL |

Під час виправлення helgrind упіймав і нову гонку в щойно написаному коді:
`ReapFinishedThreads` викликав `Free` для потоку, який ще виконував
`DoTerminate`; деструктор виконувався до приєднання. Тепер спершу `WaitFor`,
потім `Free` (так само для монітора, `FreeMonitorThread`).

## Відкриті задачі (поза `System.Threading`)

| Конфлікт | Опис |
|---|---|
| `TInterfacedObject._Release` ↔ `_Release`/`Destroy` (`System`) | лічильник посилань атомарний, але helgrind цього не бачить. Потрібні анотації в RTL (happens-before перед декрементом, happens-after перед `Destroy`), як у libstdc++ для `shared_ptr`. Звідси ж хибні конфлікти «звільнення задачі ↔ `FDoneEvent.SetEvent`». |

## Правило для нових тестів

У коді, що виконується в робочих потоках (обробники пулу, задачі,
`Execute` тестових потоків), **не викликати `Assert*`/`Fail`**. Використовувати
`ThreadCheck(умова, повідомлення)` або `ThreadCheckSame(...)` з
`TThreadedTestCase`; невдачі буде показано в `TearDown` тесту.
