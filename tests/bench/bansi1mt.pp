program bansi1mt;
{$define THREAD}
{$i bansi1.inc}

var
  NumThreads: Integer = 4;

type
  TBenchThread = class(TThread)
  public
    procedure Execute; override;
  end;

procedure TBenchThread.Execute;
begin
  Benchmark;
end;

var
  threads: array of TBenchThread;
  I: integer;
begin
  if ParamCount > 0 then
  begin
    NumThreads := StrToIntDef(ParamStr(1), 0);
    if NumThreads < 1 then
    begin
      writeln('Pass a valid number of threads, >= 1');
      exit;
    end;
  end;
  { main thread is also a thread }
  setlength(threads, NumThreads-1);
  for I := low(threads) to high(threads) do
    threads[I] := TBenchThread.Create(false);
  Benchmark;
  for I := low(threads) to high(threads) do
  begin
    threads[I].waitfor;
    threads[I].free;
  end;
end.
