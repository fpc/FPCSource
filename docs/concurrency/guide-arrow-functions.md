# Посібник користувача: Стрілкові функції (Arrow Functions `=>`) у Free Pascal

## 1. Вступ та мотивація

Традиційний синтаксис анонімних методів у Pascal, успадкований від Delphi, вимагає значної кількості шаблонного коду (boilerplate):
```pascal
TParallel.&For(0, N - 1, procedure(I: Integer)
begin
  Results[I] := Work(I);
end);

R := TTask.Future<Integer>(function: Integer
begin
  Result := ComputeResult(42);
end);
```

Стрілкові лямбда-вирази (**Arrow Functions**, оператор `=>`), активовані за допомогою директиви:
```pascal
{$modeswitch arrowfunctions}
```
дозволяють скоротити багатослівний запис у 2–3 рази, роблячи код значно чистішим, декларативнішим і зручнішим при роботі з бібліотеками паралелізму (`System.Threading`), колекціями (`Generics.Collections`), асинхронним кодом (`fcl-async`) та функціональними обробниками подій:

```pascal
// Лаконічний запис:
TParallel.&For(0, N - 1, (I) => Results[I] := Work(I));

// Ще коротше (без дужок для одного аргументу):
TParallel.&For(0, N - 1, I => Results[I] := Work(I));

// Безпараметричні лямбди та виведення типів для Future:
R := TTask.Future(() => ComputeResult(42));
```

---

## 2. Активація та сумісність режимів

Для увімкнення синтаксису стрілкових функцій використовується перемикач:
```pascal
{$modeswitch arrowfunctions}
```

> [!NOTE]
> Перемикач `arrowfunctions` автоматично активує залежні розширення компілятора:
> - `{$modeswitch anonymousfunctions}` (підтримка анонімних функцій та замикань);
> - `{$modeswitch functionreferences}` (підтримка посилань на функції `reference to procedure/function`).
>
> Для максимальної стислості рекомендується також поєднувати його з `{$modeswitch implicitgenerics}` (Розділ 2.1), що дозволяє не писати `specialize` та автоматично виводити типи узагальнених параметрів.

---

## 3. Синтаксичні форми лямбда-виразів

Компілятор підтримує всі стандартні форми стрілкових функцій:

### 3.1. Безпараметричні лямбди
```pascal
F := () => 100;
Task := TTask.Run(() => DoBackgroundJob());
Fut := TTask.Future(() => CalculateAnswer(42));
```

### 3.2. Один аргумент (з дужками або без дужок)
```pascal
// Без дужок (тип виводиться із сигнатури делегата):
TParallel.&For(0, N - 1, I => ProcessItem(I));
Square := X => X * X;

// З дужками:
TParallel.&For(0, N - 1, (I) => ProcessItem(I));
```

### 3.3. Кілька аргументів із контекстним виведенням типів
Якщо тип делегата відомий (наприклад, `TFunc2<Integer, Integer, Integer>`), типи аргументів лямбди можна опустити:
```pascal
type
  TBinaryOp = reference to function(A, B: Integer): Integer;
var
  Add: TBinaryOp;
begin
  Add := (A, B) => A + B;
end;
```

### 3.4. Явне декларування типів аргументів
Якщо потрібна однозначність або типи аргументів мають специфікатори (`const`, `var`, `out`):
```pascal
Filter := (const S: string; MinLen: Integer) => Length(S) >= MinLen;
SwapOp := (var A: Integer; var B: Integer) => begin T := A; A := B; B := T; end;
```

### 3.5. Явна декларація типу повернення
За замовчуванням тип значення, що повертається, виводиться із типу виразу або цільового функціонального посилання. Якщо потрібно явно обмежити тип повернення:
```pascal
// Синтаксис: (параметри): Тип => Вираз
Divide := (A, B: Double): Single => A / B;
```

### 3.6. Тіло-вираз (Expression Body) проти блоку (Block Body)
- **Expression Body**: якщо після `=>` йде одиночний вираз або присвоєння, компілятор автоматично транслює його у тіло методу. Для функцій значення виразу автоматично присвоюється результату (`Result := ...`), для процедур — виконується як дія.
  ```pascal
  IncCount := () => Inc(Counter);
  FullName := (First, Last) => First + ' ' + Last;
  ```
- **Block Body**: якщо тіло лямбди містить кілька інструкцій, використовується блок `begin ... end`:
  ```pascal
  Proc := (V) =>
    begin
      DoLog('Processing value: ' + IntToStr(V));
      Inc(Total, V);
    end;
  ```

---

## 4. Контекстне виведення типів (Type Inference)

Стрілкові функції інтегровані з підсистемою визначення очікуваних типів у `pexpr.pas` та `psub.pas`:

1. **Визначення параметрів під час виклику методів**:
   Коли стрілкова функція передається аргументом у виклик (наприклад, у `TParallel.&For(FromIdx, ToIdx, Lambda)`), компілятор аналізує очікуваний тип формального параметра процедури (`TProc<Integer>`) та автоматично підставляє тип `Integer` для параметра `I` у `(I) => ...`.

2. **Generic Type Inference для викликів `TTask.Future`**:
   У поєднанні з `{$modeswitch implicitgenerics}`, стрілкова функція передає тип результату свого тіла:
   ```pascal
   // Не потрібно писати TTask.Future<Integer>(...)
   var Fut := TTask.Future(() => ComputeResult(42)); // Автоматично IFuture<Integer>
   ```

3. **Коректна робота з керованими типами**:
   Для виразів, що повертають рядки (`string`, `AnsiString`), інтерфейси (`IInterface`) або динамічні масиви, присвоєння у тілі лямбди генерує повноцінне керування життєвим циклом (ARC, copy-on-write, `fpc_ansistr_assign`).

---

## 5. Замикання (Closures) та захоплення змінних

Стрілкові функції повністю підтримують захоплення лексичного контексту:
```pascal
procedure TestCapture;
var
  Factor: Integer;
  Multiplier: TFunc1<Integer, Integer>;
begin
  Factor := 5;
  Multiplier := X => X * Factor; // Захоплює Factor по посиланню
  Factor := 10;
  WriteLn(Multiplier(3)); // Виведе 30
end;
```
Компілятор автоматично виносить захоплені змінні у спільний фрейм кучі (capture frame), керований лічильником посилань інтерфейсу делегата. Коли делегат знищується, пам'ять звільняється автоматично.

---

## 6. Порівняння: До та Після

| Патерн використання | Класичний синтаксис FPC / Delphi | Стрілковий синтаксис `=>` |
|:---|:---|:---|
| **Паралельний цикл** | `TParallel.&For(0, N-1, procedure(I: Integer) begin Work(I); end);` | `TParallel.&For(0, N-1, I => Work(I));` |
| **Асинхронний Task** | `TTask.Run(procedure begin ProcessData(); end);` | `TTask.Run(() => ProcessData());` |
| **Фоновий Future** | `TTask.Future<Double>(function: Double begin Result := Calc(); end);` | `TTask.Future(() => Calc());` |
| **Конвеєрне продовження** | `Fut.ThenBy(function(const T: ITask): Integer begin Result := Work(T); end);` | `Fut.ThenBy(T => Work(T));` |
| **Фільтрація списку** | `List.Find(function(const Item: TUser): Boolean begin Result := Item.Active; end);` | `List.Find(Item => Item.Active);` |
| **Подія з блоком** | `Btn.OnClick := procedure(Sender: TObject) begin Close; end;` | `Btn.OnClick := Sender => Close;` |

---

## 7. Приклад повного коду

```pascal
{$mode objfpc}{$H+}
{$modeswitch arrowfunctions}
{$modeswitch implicitgenerics}

program ArrowDemo;

uses
  SysUtils, System.Threading;

function HeavyCalculation(Idx: Integer): Integer;
begin
  Sleep(10);
  Result := Idx * Idx;
end;

var
  Results: array[0..9] of Integer;
  Fut: IFuture<Integer>;
begin
  // 1. Паралельне обчислення через TParallel.&For:
  TParallel.&For(0, 9, I => Results[I] := HeavyCalculation(I));

  // 2. Асинхронний розрахунок у Future:
  Fut := TTask.Future(() => HeavyCalculation(42));

  WriteLn('Results[5] = ', Results[5]);
  WriteLn('Future Value = ', Fut.Value);
end.
```

