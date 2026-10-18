program valgrind_annotate;
// Анотації для helgrind/drd: власна передача даних через прапорець.
// Звичайна збірка:     fpc valgrind_annotate.pp
// Збірка з анотаціями: fpc -dFPC_VALGRIND valgrind_annotate.pp
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes{$IFDEF FPC_VALGRIND}, fpvalgrind{$ENDIF};

var
  Payload: Integer;     // дані, які передаємо
  Ready: LongInt;       // прапорець «дані готові»

type
  TProducer = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TProducer.Execute;
begin
  Payload := 42;
  {$IFDEF FPC_VALGRIND}
  VgHappensBefore(@Ready);          // усе записане вище «опубліковано»
  {$ENDIF}
  InterlockedExchange(Ready, 1);    // атомарний запис з бар'єром
end;

var
  P: TProducer;
begin
  {$IFDEF FPC_VALGRIND}
  WriteLn('Running on valgrind: ', VgRunningOnValgrind);
  {$ENDIF}
  P := TProducer.Create(False);
  while InterlockedCompareExchange(Ready, 1, 1) <> 1 do
    Sleep(1);
  ReadBarrier;
  {$IFDEF FPC_VALGRIND}
  VgHappensAfter(@Ready);           // бачимо все, що опублікував виробник
  {$ENDIF}
  WriteLn('Payload: ', Payload);
  P.WaitFor;
  P.Free;
end.
