# Посібник користувача: Масштабований асинхронний I/O `fcl-eventloop` (Фаза 1.3)

Пакет `fcl-eventloop` (`packages/fcl-eventloop/`) надає єдиний кросплатформний цикл подій (Event Loop) та неблокуючі примітиви введення-виведення на базі нативних мультиплексорів сучасних операційних систем:
- **Linux**: `epoll` та `io_uring` (з міжпотоковим пробудженням через `eventfd2`)
- **macOS / FreeBSD / OpenBSD**: `kqueue`
- **Windows**: `IOCP` (I/O Completion Ports)

Пакет розрахований на обслуговування **100 000+ одночасних з'єднань (C100K+)** пулом із кількох робочих ядер без витрачання гігабайтів пам'яті на окремі стеки потоків ОС.

---

## 1. Архітектурні компоненти

1. **`IEventLoop` (`eventloop.core.pp`)**: інтерфейс одинарного циклу реактора. Керує реєстрацією системних дескрипторів (`RegisterHandle`, `UnregisterHandle`), таймерами (`SetTimer`, `CancelTimer`), міжпотоковими завданнями (`Post`) та пробудженням (`Wakeup`).
2. **`IEventLoopGroup` (`eventloop.core.pp`)**: пул робочих реакторів (`TEventLoopGroup`). Дозволяє реалізувати архітектуру Multi-Reactor: виділений потік-акцептор слухає порт, а робочі потоки реактора паралельно опрацьовують мережевий I/O підключених сокетів.
3. **`IAsyncSocket` та `IAsyncServerSocket` (`eventloop.sockets.pp`)**: неблокуючі TCP клієнти і сервери. Підтримують як швидкі callback-методи та анонімні замикання, так і високорівневу інтеграцію з `IFuture<NativeInt>` та `TCancellationToken`.
4. **`IAsyncPipe` (`eventloop.pipes.pp`)**: асинхронні неблокуючі конвеєри для швидкого IPC або міжпотокового обміну байтами.
5. **`IAsyncFile` (`eventloop.files.pp`)**: неблокуюче читання та запис файлів без зависання основного циклу подій.
6. **`TAsyncBufferPool` (`eventloop.types.pp`)**: високоефективний пул байтових буферів для усунення фрагментації купи під час роботи з десятками тисяч сокетів.

---

## 2. Швидкий старт: створення TCP сервера (Echo Server)

```pascal
program EchoServer;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, eventloop.types, eventloop.core, eventloop.sockets, fcleventloop;

var
  Loop: IEventLoop;
  Server: IAsyncServerSocket;
begin
  { Створення циклу з автоматичним вибором найкращого нативного бекенду }
  Loop := TEventLoop.Create(bkAuto);

  Server := TEventLoop.CreateTcpServer(Loop);
  Server.Listen('0.0.0.0', 8080);
  WriteLn('Сервер слухає на 0.0.0.0:8080...');

  Server.AsyncAccept(
    procedure(AClientHandle: TLoopHandle; const AClientAddr: string; AClientPort: Word; AStatus: Integer)
    var
      Client: IAsyncSocket;
      Buf: array[0..1023] of Byte;
    begin
      if AStatus <> 0 then Exit;
      WriteLn(Format('Нове підключення з %s:%d', [AClientAddr, AClientPort]));

      Client := TAsyncSocket.Create(Loop, AClientHandle, True);
      Client.AsyncRead(@Buf[0], SizeOf(Buf),
        procedure(ABytesRead: NativeInt; AReadStatus: Integer)
        begin
          if (AReadStatus = 0) and (ABytesRead > 0) then
          begin
            { Відправляємо ехо назад клієнту }
            Client.AsyncWrite(@Buf[0], ABytesRead, nil);
          end
          else
            Client.Close;
        end);
    end);

  { Запуск головного циклу обробки подій }
  Loop.Run;
end.
```

---

## 3. Масштабування на C100K: Multi-Reactor Worker Pool

Для максимального утилізування багатоядерних процесорів використовується `TEventLoop.CreateGroup(WorkerCount)`:

```pascal
var
  AcceptorLoop: IEventLoop;
  WorkerGroup: IEventLoopGroup;
  Server: IAsyncServerSocket;
begin
  { Створюємо пул з 8 робочих реакторів }
  WorkerGroup := TEventLoop.CreateGroup(8, bkAuto);
  WorkerGroup.Start;

  { Головний цикл-акцептор приймає нові TCP з'єднання }
  AcceptorLoop := TEventLoop.Create(bkAuto);
  Server := TEventLoop.CreateTcpServer(AcceptorLoop);
  Server.Listen('0.0.0.0', 8080);

  Server.AsyncAccept(
    procedure(AClientHandle: TLoopHandle; const AClientAddr: string; AClientPort: Word; AStatus: Integer)
    var
      AssignedLoop: IEventLoop;
      Client: IAsyncSocket;
    begin
      if AStatus <> 0 then Exit;

      { Передаємо клієнтський сокет робочому потоку через Round-Robin }
      AssignedLoop := WorkerGroup.NextLoop;
      Client := TAsyncSocket.Create(AssignedLoop, AClientHandle, True);
      
      { Читання і запис відбуваються в потоці призначеного циклу }
      Client.AsyncRead(...);
    end);

  AcceptorLoop.Run;
end;
```

---

## 4. Використання асинхронних Future та CancellationToken

Усі асинхронні сокети підтримують методи `ReadAsync`, `WriteAsync` та `AcceptAsync`, що повертають `IFuture<T>` і приймають `TCancellationToken`:

```pascal
var
  CTS: TCancellationTokenSource;
  Client: IAsyncSocket;
  WriteFut: specialize IFuture<NativeInt>;
  Data: AnsiString;
begin
  CTS := TCancellationTokenSource.Create;
  CTS.CancelAfter(5000); // 5 секунд таймаут

  Data := 'GET / HTTP/1.1'#13#10#13#10;
  WriteFut := Client.WriteAsync(PAnsiChar(Data), Length(Data), CTS.Token);

  { Отримання результату або перехоплення EOperationCancelled }
  try
    WriteLn('Відправлено байтів: ', WriteFut.Value);
  except
    on E: EOperationCancelled do
      WriteLn('Операцію скасовано за таймаутом!');
  end;
end;
```

---

## 5. Порівняння архітектур: Thread-Per-Connection vs Reactor

| Характеристика | Thread-Per-Connection (`ssockets`) | `fcl-eventloop` (Reactor) |
|---|---|---|
| **Одночасних з'єднань** | ~1 000 – 3 000 (ліміт пам'яті стеку ОС) | **100 000+** на тих самих ресурсах |
| **Використання пам'яті (100k сокетів)** | ~800 ГБ (за умови 8 МБ на стек потоку) | **~150 МБ** (спільна купа, буферний пул) |
| **Кількість потоків ОС** | 100 000 потоків ядра | **4 – 16** робочих ядер |
| **Перемикання контексту ядра** | Мільйони/сек (CPU thrashing) | Мінімальне (пакетна обробка подій) |
| **Системний мультиплексор** | `select()` ($O(N)$, максимум 1024 fd) | `epoll` / `io_uring` / `kqueue` ($O(1)$) |

