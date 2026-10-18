program pool_lifetime;
// Власний пул: Free повертає керування лише тоді, коли всі потоки пулу
// справді завершились (раніше вони могли ще працювати).
{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, System.Threading;

// Кількість потоків процесу (лише Linux).
function LiveThreads: Integer;
var
  Info: TSearchRec;
begin
  Result := 0;
  if FindFirst('/proc/self/task/*', faDirectory, Info) = 0 then
  try
    repeat
      if (Info.Name <> '.') and (Info.Name <> '..') then
        Inc(Result);
    until FindNext(Info) <> 0;
  finally
    FindClose(Info);
  end;
end;

var
  Pool: TThreadPool;
  Done, I: Integer;
begin
  WriteLn('Threads before: ', LiveThreads);
  Pool := TThreadPool.Create;
  try
    Pool.SetMaxWorkerThreads(4);
    Done := 0;
    for I := 1 to 20 do
      Pool.QueueWorkItem(
        procedure
        begin
          Sleep(10);
          AtomicIncrement(Done);
        end);
    while Done < 20 do
      Sleep(1);
    WriteLn('Threads while pool is alive: ', LiveThreads);
  finally
    Pool.Free;            // чекає (WaitFor) і звільняє всі свої потоки
  end;
  WriteLn('Threads after Free: ', LiveThreads);
end.
