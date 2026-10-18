program tasks;
// Задачі: TTask.Run, IFuture<T>, WaitForAll / WaitForAny.
{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, System.Threading;

var
  Answer: specialize IFuture<Integer>;
  Jobs: array[0..2] of ITask;
  First: Integer;
begin
  // Результат обчислюється в потоці пулу; Value чекає на нього.
  Answer := TTask.specialize Future<Integer>(
    function: Integer
    begin
      Sleep(50);
      Result := 6 * 7;
    end);
  WriteLn('Future value: ', Answer.Value);

  Jobs[0] := TTask.Run(procedure begin Sleep(300); end);
  Jobs[1] := TTask.Run(procedure begin Sleep(20);  end);
  Jobs[2] := TTask.Run(procedure begin Sleep(150); end);

  First := TTask.WaitForAny(Jobs);          // індекс першої завершеної
  WriteLn('First finished: task #', First);
  WriteLn('All finished: ', TTask.WaitForAll(Jobs, 2000));
end.
