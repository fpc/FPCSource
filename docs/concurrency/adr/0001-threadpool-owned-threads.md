# ADR-0001: Володіння потоками в `TThreadPool` та синхронне приєднання в `Destroy`

- Статус: прийнято
- Дата: 2026-10-19

## Контекст
У початковій реалізації `System.Threading` робочі потоки (`TBaseWorkerThread`) та потік-монітор (`TMonitorThread`) створювалися з `FreeOnTerminate := True` і видаляли себе зі списку `FThreads` у власному деструкторі або в `DoTerminate`. Через це:
1. `TThreadPool.Destroy` повертав керування, поки потоки пулу все ще виконували свій деструктор `TThread.Destroy` та епілог потоку в RTL (`pthread_detach` / звільнення TLS).
2. При завершенні програми знищення `TThreadPool.Default` у `finalization` перетиналося з вимкненням менеджера пам'яті та фіналізацією RTL.
3. Потік-монітор міг записати `FMonitorStatus := MonitorNone` у вже звільнену пам'ять об'єкта `TThreadPool` (use-after-free).

## Рішення
1. Усі потоки пулу створюються з `FreeOnTerminate := False` і належать екземпляру `TThreadPool`.
2. Завершуючи `Execute`, робочий потік додає себе до списку `FFinishedThreads` **до** виходу з `FThreads`.
3. Метод `ReapFinishedThreads` (викликається монітором періодично та обов'язково в `TThreadPool.Destroy`) спочатку виконує `WaitFor` (`pthread_join`), а лише потім `Free`.
4. Потік-монітор зберігається в `FMonitorThread`, скидає `FMonitorStatus` атомарно останньою інструкцією, після чого приєднується (`WaitFor`) і звільняється в `FreeMonitorThread`.
5. У `GetWorkItemForThread` та `WaitForThreads` усунено гонку між `FQueueEvent.ResetEvent` і `FQueueEvent.SetEvent` при `FShutdown = True`, щоб потоки не засинали на `IdleTimeout` (40 с) під час знищення пулу.

## Розглянуті альтернативи
- **Лічильник активних потоків на атомарній події в кінці `Destroy` при збереженні `FreeOnTerminate := True`**: відхилено, оскільки навіть після останнього рядка `TThread.Destroy` код RTL у `ThreadFunc` (`rtl/unix/tthread.inc`) ще продовжує виконуватися до виходу з функції потоку ОС; лише `WaitFor` (`pthread_join`) гарантує повне завершення потоку на рівні ОС.

## Наслідки
- Після повернення з `Pool.Free` гарантовано не існує жодного живого потоку пулу (перевіряється регресійним тестом `TTestThreading.TestFreeJoinsThreads`).
- Зникли гонки use-after-free під час завершення тестів і програм під `valgrind --tool=helgrind`.
- Обробник `OnThreadTerminate` виконується до `WaitFor`, тому не повинен викликати `TThread.Synchronize`, якщо `Pool.Free` викликається з головного потоку.

