# ADR-0005: Неблокувальні продовження задач (`ContinueWith` / `ThenBy`) та кооперативне скасування (`TCancellationToken`)

- **Статус**: Прийнято (Фаза 1.1)
- **Дата**: 2026-10-19
- **Компоненти**: `packages/vcl-compat/src/system.threading.pp`

## Контекст і проблема

У базовій реалізації `System.Threading`:
1. Отримання результату `IFuture<T>` або очікування `ITask` вимагало блокування поточного потоку у `.Wait` чи `.Value`. При побудові багатоетапних конвеєрів (`Stage1 -> Stage2 -> Stage3`) координувальні задачі утримували робочі потоки пулу в заблокованому стані, що призводило до виснаження пулу потоків (**thread-pool starvation**).
2. Метод `ITask.Cancel` діяв лише на одну конкретну задачу. Скасування складного дерева операцій (коренева задача + вкладений `TParallel.For` + дочірні `IFuture<T>` + ланцюжок продовжень) або скасування за таймаутом вимагало ручних прапорців і окремих викликів `.Cancel` для кожного вузла.

## Рішення

### 1. Спільний стан скасування з підрахунком посилань (`ICancellationState` / `TCancellationToken` / `TCancellationTokenSource`)

- `TCancellationToken` реалізовано як легковаговий запис (`record`), що містить інтерфейсне посилання `ICancellationState`. Завдяки підрахунку посилань копії токена безпечно живуть у фонових задачах навіть після звільнення `TCancellationTokenSource` або його перезапуску через `Reset`.
- Перевірка `Token.IsCancellationRequested` використовує неблокувальне атомарне читання (`ReadBarrier`), що дозволяє викликати `ThrowIfCancellationRequested` / `CheckCanceled` у щільних циклах без накладних витрат на м'ютекс.
- `TCancellationToken.Register` прив'язує зворотний виклик (`TProcRef` або `TNotifyEvent`) до токена і повертає `TCancellationTokenRegistration` для безпечного скасування реєстрації (`Unregister`). Щоб уникнути взаємного блокування (self-deadlock), коли зворотний виклик `TTask.OnCancelToken` синхронно завершує задачу в тому самому потоці, який викликав `CTS.Cancel`, `UnregisterCallback` порівнює `GetCurrentThreadId` з `FExecutingThreadID` і чекає завершення паралельного виконання лише тоді, коли `Unregister` викликано з іншого потоку.
- Таймерне скасування `CancelAfter(MillisecondsDelay)` реалізовано через керований фоновий потік, який спить на події `FTimerAbortEvent` і миттєво завершується під час `Cancel` або `Destroy` (без висячих потоків після звільнення `TCancellationTokenSource`).
- Конструктор `TTask.Create` та `TParallel.Parallelize32`/`Parallelize64` автоматично успадковують контекстний токен (`_CurrentTask.FParams.CancellationToken`), якщо дочірній задачі чи вкладеному циклу не передано окремий токен.

### 2. Неблокувальні продовження (`ContinueWith`, `ThenBy`, `Map`)

- Додано перелік `TTaskContinuationOption` (`NotOnCompleted`, `NotOnFaulted`, `NotOnCanceled`, `ExecuteSynchronously`) та константи-предикати `OnlyOnCompleted`, `OnlyOnFaulted`, `OnlyOnCanceled`.
- Метод `TTask.AttachContinuation` підписує задачу-продовження на завершення задачі-попередника через `AddCompleteEvent`. Жоден потік пулу не блокується під час очікування попередника: продовження ставиться в чергу пулу (`Start`) або виконується синхронно (`ExecuteWork`) лише в момент завершення попередника.
- Оскільки в Object Pascal (`{$mode objfpc}`) інтерфейси не можуть містити generic-методів, типізовані конвеєрні перетворення `.ThenBy<TResult>` та `.ThenBy<T, TResult>` реалізовано через `TITaskHelper = type helper for ITask` (який завдяки спадкуванню інтерфейсів автоматично діє і на `ITask`, і на `IFuture<T>`) та статичний метод `TTask.ThenBy<T, TResult>`, а для перетворень того самого типу `T -> T` додано метод `IFuture<T>.Map` без потреби повторної спеціалізації.
- Спеціалізовані класи `TFutureContinuationTask<T>` та `TTransformFuture<T, TResult>` інкапсулюють перетворення без створення зайвих анонімних замикань всередині generic-методів.
- У конвеєрах `.ThenBy` винятки задачі-попередника автоматично переносяться у похідний `IFuture<TResult>` через `PropagateExceptionFrom`, а скасування попередника автоматично переводить похідний етап у `TTaskStatus.Canceled`.

## Наслідки

- **Відсутність thread-pool starvation**: конвеєри довільної глибини та ширини не блокують робочі потоки пулу між етапами.
- **Узгоджене згортання графа задач**: єдиний виклик `CTS.Cancel` (або спрацювання `CancelAfter`) кооперативно зупиняє активні задачі, вкладені цикли `TParallel.For`, скасовує задачі в черзі пулу (`DoRemoveWorkItem`) і запускає компенсаційні гілки `OnlyOnCanceled`.
- **Чистота під Valgrind Helgrind і stress-тестами**: анотації `VgHappensBefore` / `VgHappensAfter` у `TCancellationState` забезпечують 0 хибних спрацювань детектора гонок даних.

