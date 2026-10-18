# ADR-0003: Детермінованість `TParallel.For` — атомарний крок, локальний `TLoopState` і збереження винятків

- Статус: прийнято
- Дата: 2026-10-19

## Контекст
Аналіз та тестування `TParallel.For` і `TParallel.Join` у `packages/vcl-compat/src/system.threading.pp` виявили чотири фундаментальні проблеми:
1. **Втрата і дублювання ітерацій**: `GetCurrentStart` просував спільний індекс `FLoopProc.Index` на поточний `Stride`, але робочий потік читав `FLoopProc.Stride` повторно вже після виходу з `GetCurrentStart`. Оскільки інші потоки паралельно збільшували `Stride` у `GetNextStride`, діапазон виконання `[Start .. Start + Stride)` не збігався з фактично зарезервованим діапазоном.
2. **Неіснуючий та спільний `TLoopState`**: поле `State` лежало в записі `TInt32LoopProc` / `TInt64LoopProc`, ніколи не ініціалізувалося (`nil`) і мало б спільно модифікуватися всіма потоками (`State.CurrentIteration := Iteration`), що призводило до `Access violation` та гонки даних. Крім того, `TLoopParams.Break` завжди повертав `False`, `ShouldExitLoop(CurrentIter)` у режимі `objfpc` читав локальну змінну `Result` замість виклику `ShouldExitLoop()`, а `Break` у тілі `Invoke` викликав метод `TLoopParams.Break` замість виходу з циклу `system.Break`.
3. **Втрата винятків з дочірніх реплік**: масив `TTask.FTasksWithExceptions` мав тип `array of TTask` (слабкі посилання без підрахунку посилань). Коли дочірня задача `TReplicatedTask` кидала виняток, єдине інтерфейсне посилання на неї (`Itm: IThreadPoolWorkItem`) обнулялося робочим потоком пулу одразу після завершення `WrapExecute`, що викликало `TTask.Destroy` і `FreeAndNil(FException)` ще до того, як коренева задача збирала винятки в `GetExceptionObject`. Додатково `TExceptionList.Create(Initial, aCapacity)` не збільшував `Count` і не розгортав вкладений `EAggregateException`.
4. **Непрацездатний `TParallel.Join`**: `TEventJoinTask.Create` і `TProcJoinTask.Create` не ініціалізували локальний запис `Params: TTaskParams`, встановлювали `Params.Parent := Self` (що збільшувало `FSubTasks` до 2 і вічно блокувало `Wait`), а `TParallel.Join` не викликав `.Start`.

## Рішення
1. Метод `GetCurrentStart(out aStride)` повертає саме те значення кроку `aStride`, з яким було зарезервовано діапазон `Start`.
2. Поле `State` прибрано з `TInt32LoopProc` / `TInt64LoopProc`. Кожна репліка в `TInt32LoopParams.Invoke` / `TInt64LoopParams.Invoke` створює власний локальний екземпляр `TLoopState32` / `TLoopState64` і передає його в `FLoopProc.Execute(I, State)`. Виправлено `Break`, `DoBreak`, `GetBreakAt`, `ShouldExitLoop()` та `system.Break`.
3. Тип `FTasksWithExceptions` змінено на `TITaskArray` (`array of ITask`), щоб дочірні репліки з винятками залишалися живими до моменту збору помилок кореневою задачею та очищення списку в `Complete`. Конструктор `TExceptionList.Create(Initial, aCapacity)` тепер викликає `Result.Flatten(Initial)`.
4. У `TEventJoinTask.Create` і `TProcJoinTask.Create` додано `Params := Default(TTaskParams)`, прибрано `Params.Parent := Self`, а в `TParallel.Join` додано виклик `.Start`.

## Розглянуті альтернативи
- **Зберігання `TLoopState` у `threadvar`**: відхилено, оскільки `TParallel.For` може викликатися вкладено в межах одного робочого потоку; локальна змінна в стеку `Invoke` простіша, безпечна щодо вкладеності й створюється лише один раз на репліку (а не на кожну ітерацію).
- **Копіювання `FException` у кореневу задачу в момент виникнення винятку**: відхилено на користь `TITaskArray`, оскільки існуюча архітектура `TTask.GetExceptionObject` вже обходить `FTasksWithExceptions` після завершення всіх підзадач, і утримання `ITask` мінімально змінює логіку класу.

## Наслідки
- Публічний інтерфейс `System.Threading` залишився на 100% сумісним із Delphi та попереднім кодом FPC.
- Усі режими `TParallel.For` (звичайний, з `TLoopState`, `Break`, `Stop`, з винятками) та `TParallel.Join` працюють детерміновано й покриті регресійними тестами в `TTestParallel`.

