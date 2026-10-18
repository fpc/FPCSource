program report_classic;
// Варіант 1: лише TThread з RTL (без System.Threading).
// Звіт збирається з 4 незалежних джерел різної тривалості; одне падає.
// Вимоги: виконати паралельно, загальний ліміт 2 с, показати помилки.
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, SyncObjs, DateUtils;

var
  Remaining: Integer;       // скільки задач ще не завершилось
  AllDone: TSimpleEvent;    // подія «усі завершились» — для таймауту

type
  // Базовий клас: лічильник і подія завершення — вручну.
  TJob = class(TThread)
  protected
    procedure Work; virtual; abstract;
    procedure Execute; override;
  end;

  TOrdersJob = class(TJob)
  public Orders: Int64;
  protected procedure Work; override;
  end;

  TTopCustomerJob = class(TJob)
  public Name: string;
  protected procedure Work; override;
  end;

  TAverageJob = class(TJob)
  public Average: Double;
  protected procedure Work; override;
  end;

  TRatesJob = class(TJob)
  public Rate: Double;
  protected procedure Work; override;
  end;

procedure TJob.Execute;
begin
  try
    Work;                   // виняток потрапить у FatalException
  finally
    if InterlockedDecrement(Remaining) = 0 then
      AllDone.SetEvent;
  end;
end;

procedure TOrdersJob.Work;
begin
  Sleep(400); Orders := 1234;
end;

procedure TTopCustomerJob.Work;
begin
  Sleep(250); Name := 'Acme Ltd';
end;

procedure TAverageJob.Work;
begin
  Sleep(300); Average := 87.5;
end;

procedure TRatesJob.Work;
begin
  Sleep(150);
  raise Exception.Create('Rates service: connection refused');
end;

var
  Jobs: array[0..3] of TJob;
  I: Integer;
  Start: TDateTime;
begin
  Start := Now;
  AllDone := TSimpleEvent.Create;
  Remaining := Length(Jobs);
  Jobs[0] := TOrdersJob.Create(False);
  Jobs[1] := TTopCustomerJob.Create(False);
  Jobs[2] := TAverageJob.Create(False);
  Jobs[3] := TRatesJob.Create(False);
  try
    // TThread.WaitFor не має таймауту, тому — власна подія.
    if AllDone.WaitFor(2000) <> wrSignaled then
      WriteLn('Timeout: report is incomplete');
    for I := 0 to High(Jobs) do
      Jobs[I].WaitFor;
    for I := 0 to High(Jobs) do       // помилки — перевіряти кожен потік
      if Assigned(Jobs[I].FatalException) then
        WriteLn('Error: ', Exception(Jobs[I].FatalException).Message);
    WriteLn('Orders: ', TOrdersJob(Jobs[0]).Orders);
    WriteLn('Top customer: ', TTopCustomerJob(Jobs[1]).Name);
    WriteLn('Average check: ', TAverageJob(Jobs[2]).Average:0:1);
    if not Assigned(Jobs[3].FatalException) then
      WriteLn('Rate: ', TRatesJob(Jobs[3]).Rate:0:2);
  finally
    for I := 0 to High(Jobs) do
      Jobs[I].Free;
    AllDone.Free;
  end;
  WriteLn('Time: ', MilliSecondsBetween(Now, Start), ' ms');
end.
