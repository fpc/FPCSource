program report_threading;
// Варіант 2: System.Threading (IFuture, WaitForAll, EAggregateException).
// Звіт збирається з 4 незалежних джерел різної тривалості; одне падає.
// Вимоги: виконати паралельно, загальний ліміт 2 с, показати помилки.
{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, DateUtils, System.Threading;

var
  Orders: specialize IFuture<Int64>;
  TopCustomer: specialize IFuture<string>;
  Average, Rate: specialize IFuture<Double>;
  E: Exception;
  Start: TDateTime;
begin
  Start := Now;
  Orders := TTask.specialize Future<Int64>(
    function: Int64 begin Sleep(400); Result := 1234; end);
  TopCustomer := TTask.specialize Future<string>(
    function: string begin Sleep(250); Result := 'Acme Ltd'; end);
  Average := TTask.specialize Future<Double>(
    function: Double begin Sleep(300); Result := 87.5; end);
  Rate := TTask.specialize Future<Double>(
    function: Double
    begin
      Sleep(150);
      raise Exception.Create('Rates service: connection refused');
    end);

  try
    if not TTask.WaitForAll([Orders, TopCustomer, Average, Rate], 2000) then
      WriteLn('Timeout: report is incomplete');
  except
    on Agg: EAggregateException do    // усі помилки зібрано в одному місці
      for E in Agg do
        WriteLn('Error: ', E.Message);
  end;
  WriteLn('Orders: ', Orders.Value);
  WriteLn('Top customer: ', TopCustomer.Value);
  WriteLn('Average check: ', Average.Value:0:1);
  if Rate.Status = TTaskStatus.Completed then
    WriteLn('Rate: ', Rate.Value:0:2);
  WriteLn('Time: ', MilliSecondsBetween(Now, Start), ' ms');
end.
