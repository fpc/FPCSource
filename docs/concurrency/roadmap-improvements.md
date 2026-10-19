# Дорожня карта розвитку Free Pascal: конкурентоспроможність мови та бібліотек

Цей документ систематизує напрямки вдосконалення Free Pascal (компілятора, RTL, FCL та інструментарію), щоб мова залишалася конкурентною порівняно з сучасними системними та прикладними мовами (**Rust, Go, C#/.NET, C++20, сучасний Delphi**), зберігаючи свої головні переваги: миттєву компіляцію в нативний код, відсутність пауз GC, строгу типізацію та зворотну сумісність.

---

## 1. Сучасна багатопоточність та асинхронність (Фази 2–4 гілки `feature/concurrency`)

Після стабілізації `System.Threading` та `TThreadPool` (Фаза 1) для мережевих сервісів, GUI-застосунків та високонавантажених систем потрібні наступні рівні:

### 1.1. Неблокувальні продовження задач (`Continuations`) і кооперативне скасування
- **Проблема**: зараз отримання результату `IFuture<T>` або очікування `ITask` вимагає блокування потоку у `.Wait` чи `.Value`, а `ITask.Cancel` прив'язаний до окремої задачі.
- **Рішення**:
  - Додати методи `.ContinueWith(const AAction: TProc<ITask>): ITask` та типізований `.ThenBy<TResult>(...)` для побудови неблокувальних конвеєрів задач.
  - Ввести легковаговий токен скасування **`TCancellationToken` / `TCancellationTokenSource`**, який можна передавати в `TParallel.For`, `TTask.Run`, `IFuture<T>` та асинхронний I/O для узгодженого переривання дерева операцій.

### 1.2. Нові примітиви синхронізації в `SyncObjs` (Фаза 2)
- **`TCountdownEvent`** — легковагова синхронізація завершення динамічної кількості підзадач.
- **`TBarrier`** — пофазна синхронізація групи потоків для ітеративних паралельних алгоритмів (симуляції, обробка графів/матриць).
- **`TSlimReaderWriterLock`** — швидке блокування читання/запису в просторі користувача (на базі `futex` / атомарних операцій без важких системних викликів `pthread_rwlock`).
- **`TChannel<T>` (обмежені та необмежені потокобезпечні канали в стилі Go)** — передавання повідомлень між потоками за моделлю CSP (Communicating Sequential Processes) із підтримкою закриття (`Close`) та перебору `for Msg in Channel do`.

### 1.3. Масштабований асинхронний I/O — пакет `fcl-eventloop` (Фаза 3)
- Єдиний кросплатформний цикл подій на базі нативних механізмів ОС:
  - **`io_uring` / `epoll`** (Linux)
  - **`kqueue`** (macOS, FreeBSD, OpenBSD)
  - **`IOCP`** (Windows)
- Неблокувальні сокети, таймери, файлові операції та пайпи, здатні обслуговувати 100 000+ одночасних з'єднань на пулі з кількох робочих потоків.

### 1.4. Корутини та `async` / `await` у компіляторі (Фаза 4)
- **Крок 1 (бібліотечний)**: безстекові або легковагові стекові корутини (fibers) поверх `fcl-eventloop` і `IFuture<T>`.
- **Крок 2 (у компіляторі)**: директива/модифікатор `async` для функцій, що повертають `ITask` або `IFuture<T>`, та оператор `await`, який трансформує тіло функції в кінцевий автомат (state machine) без алокації окремого стеку потоку ОС.

---

## 2. Синтаксична ергономіка мови (рівень компілятора `compiler/`)

У реальному багатопоточному й узагальненому коді Free Pascal поступається лаконічністю через надмірні синтаксичні вимоги режиму `objfpc`.

### 2.1. Виведення типів для узагальнених методів (Generic Type Inference) та необов'язковий `specialize` [РЕАЛІЗОВАНО]
- **Було**:
  ```pascal
  R := TTask.specialize Future<string>(function: string begin Result := 'ok'; end);
  ```
- **Реалізовано**:
  1. За перемикачем `{$modeswitch implicitgenerics}` дозволено опускати ключове слово `specialize` у режимі `objfpc` для типів (`IFuture<string>`, `TFPGList<Integer>`) та явних викликів методів і функцій (`Identity<string>('hello')`, `TTask.Future<string>(...)`).
  2. Реалізовано автоматичне виведення типового параметра `<T>` (Generic Type Inference) з типів переданих аргументів, зокрема з типу повернення та параметрів анонімних функцій і посилань на функції (`reference to function/procedure`):
     ```pascal
     {$mode objfpc}{$H+}
     {$modeswitch implicitgenerics}
     {$modeswitch anonymousfunctions}
     {$modeswitch functionreferences}

     // Автоматичне виведення T = string -> IFuture<string>:
     R := TTask.Future(function: string begin Result := 'ok'; end);

     // Автоматичне виведення в неблокувальних ланцюжках ThenBy:
     Next := R.ThenBy(function(const Prev: ITask): Integer begin Result := Length(IFuture<string>(Prev).Value); end);
     ```
  3. Повний набір тестів додано до `tests/concurrency/test_implicit_generics.pp`.

### 2.2. Короткий синтаксис лямбда-виразів (`=>`) [РЕАЛІЗОВАНО]
- **Статус**: Повністю реалізовано в компіляторі (`{$modeswitch arrowfunctions}`) разом із контекстним виведенням типів параметрів і результату.
- **Синтаксис**:
  ```pascal
  {$modeswitch arrowfunctions}
  {$modeswitch implicitgenerics}

  // Паралельний цикл (з дужками або без дужок для 1 аргументу):
  TParallel.&For(0, N - 1, (I) => Results[I] := Work(I));
  TParallel.&For(0, N - 1, I => Results[I] := Work(I));

  // Безпараметричні лямбди та Generic Type Inference:
  R := TTask.Future(() => ComputeResult(42));

  // Лямбди з кількома аргументами та операціями над рядками / керованими типами:
  ConcatOp := (Count, Msg) => Msg + ': ' + IntToStr(Count);

  // Блоки з кількома інструкціями:
  LogAndWork := (I) => begin Log(I); Work(I); end;
  ```
- **Документація та тести**:
  - Детальний посібник: [`guide-arrow-functions.md`](guide-arrow-functions.md).
  - Тестовий набір: [`tests/concurrency/test_arrow_functions.pp`](../../tests/concurrency/test_arrow_functions.pp).

### 2.3. Оголошення локальних змінних за місцем використання (`inlinevars`)
- За опціональним `{$modeswitch inlinevars}` (сумісно з Delphi 10.3+) дозволити:
  ```pascal
  var Fastest := TTask.WaitForAny([R0, R1, R2], 1000);
  for var E in Agg do
    WriteLn(E.Message);
  ```
  Особливо корисно для складних спеціалізацій дженериків та змінних-лічильників циклів, де область видимості обмежується самим циклом.

### 2.4. Кортежі (Tuples) та структурна декомпозиція
- Повернення кількох значень із функції (зокрема з `IFuture<...>`) без оголошення одноразових `record`:
  ```pascal
  function CountAndSumPrimes(N: Integer): (Integer, Int64);
  var (Count, Sum) := CountAndSumPrimes(5000000);
  ```

---

## 3. Пам'ять, безпека та нульові накладні витрати (рівень Rust / C++20)

### 3.1. Розумні вказівники на базі керованих записів (`TScoped<T>`, `TArc<T>`) у RTL
Механізм Management Operators (`Initialize`, `Finalize`, `AddRef`, `Copy`) для `advancedrecords` у FPC вже дозволяє створити стандартний модуль `sysmanaged` / `smartptrs`:
- **`TScoped<T>` (аналог `std::unique_ptr`)** — автоматично викликає `.Free` при виході зі стекового кадру (навіть у разі винятку), усуваючи потребу в `try..finally` для локальних об'єктів (`TStringList`, `TMemoryStream`, `TThreadPool`).
- **`TArc<T>` (Atomic Reference Counted)** — потокобезпечне спільне володіння для будь-якого класу `TObject` без вимоги успадковувати `TInterfacedObject`.

### 3.2. Безпечні зрізи пам'яті (`TSpan<T>` / `TReadOnlySpan<T>`)
- Легковагова структура `(Ptr: ^T; Length: SizeInt)` з перевіркою меж (при `-Cr`), що представляє неперервний піддіапазон статичного масиву, динамічного масиву, рядка або `TBytes` без копіювання і без алокацій у купі.
- Дозволяє безпечно передавати неперетинні сегменти масиву в паралельні задачі `System.Threading`.

### 3.3. Підтримка ThreadSanitizer (`-fsanitize=thread`) та AddressSanitizer (`-fsanitize=address`) у генераторі коду
- Доповнення модуля `fpvalgrind` вбудованою інструментацією читань/записів пам'яті в компіляторі для TSan та ASan:
  - у 5–10 разів менше сповільнення порівняно з Valgrind Helgrind;
  - робота на Linux, macOS, Windows та ARM64.

---

## 4. Інструментарій та екосистема

1. **Єдиний CLI-інструмент збірки, залежностей і тестування (`fpc pkg`)**:
   - Декларативний маніфест проєкту (альтернатива складним `Makefile.fpc` і ручним `-Fu`/`-Fi`), запуск модульних і стрес-тестів однією командою (`fpc test`), профілювання та перевірка під Valgrind/Sanitizer.
2. **Офіційний Language Server (LSP) на базі фронтенду компілятора**:
   - Повна підтримка `generic`/`specialize`, анонімних функцій, макросів та умовних директив у сучасних IDE та редакторах.

---

## 5. Пріоритети впровадження у гілці `feature/concurrency`

| Пріоритет | Напрямок | Де реалізується | Складність | Очікуваний ефект |
|---|---|---|---|---|
| **1** | `TChannel<T>`, `TCountdownEvent`, `TBarrier`, `TCancellationToken`, `.ContinueWith` [РЕАЛІЗОВАНО] | `packages/fcl-base`, `packages/vcl-compat` | Середня | Повноцінна оркестрація потоків і передавання повідомлень без низькорівневих блокувань |
| **2** | Розумні вказівники `TScoped<T>` / `TArc<T>` та `TSpan<T>` | `rtl/objpas` або `packages/rtl-objpas` | Низька–середня | Зникнення шаблонних блоків `try..finally` і витоків пам'яті в багатопоточному коді |
| **3** | Необов'язковий `specialize` + виведення типів для дженериків [РЕАЛІЗОВАНО] | `compiler/` (`pexpr.pas`, `ptype.pas`, `pgenutil.pas`) | Середня | Суттєве очищення синтаксису `TTask.Future`, колекцій та алгоритмів |
| **4** | Асинхронний цикл подій `fcl-eventloop` (`epoll`/`kqueue`/`IOCP`) [РЕАЛІЗОВАНО] | `packages/fcl-eventloop` | Висока | Високопродуктивні мережеві сервіси на FPC |
| **5** | Короткі лямбди `=>` та `async`/`await` [РЕАЛІЗОВАНО] | `compiler/`, `packages/fcl-async` | Висока | Рівень ергономіки сучасних C# / Rust / TypeScript при збереженні нативної швидкості FPC |

