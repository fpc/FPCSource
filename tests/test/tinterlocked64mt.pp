{$ifdef FPC}
  {$mode objfpc}
{$else}
  {$apptype console}
{$endif}


{$ifdef CPU64}
uses
{$ifndef FPC}
  Windows,
{$endif FPC}
{$ifdef unix}
  cthreads,
{$endif unix}
  SysUtils, Classes;

type
  TOperation = (opAdd, opDec, opExchange, opExchangeAdd, opExchangeDec, opCompareExchange);

  TWorker = class(TThread)
  private
    FOp: TOperation;
    FCount: int64;
    FOption: int64;
  protected
    procedure Execute; override;
  public
    constructor Create(ACount: int64; AOp: TOperation; AOption: int64 = 0);
  end;

//{$define TEST_BROKEN_IncDec}
//{$define TEST_BROKEN_Exchange}
//{$define TEST_BROKEN_ExchangeAdd}
//{$define TEST_BROKEN_CompareExchange}

const
  TotalThreadCount = 50;
  TestCount = 1000000;
  WaitTime = 60;

var
  Counter, Counter2, Counter3: int64;
  WorkingCount, FinishedCount: int64;
  AbortThread: boolean;
  LastCompareVal: int64;

{$ifndef FPC}
{$ifndef TEST_BROKEN_CompareExchange}
function InterlockedCompareExchange64(var Target: int64; NewValue: int64; Comperand: int64): int64;
begin
  Result:=int64(Windows.InterlockedCompareExchange64(pointer(Target), pointer(NewValue), pointer(Comperand)));
end;
{$endif TEST_BROKEN_CompareExchange}

procedure  ThreadSwitch;
begin
  Sleep(0);
end;
{$endif FPC}

{$ifdef TEST_BROKEN_CompareExchange}
function InterlockedCompareExchange64(var Target: int64; NewValue: int64; Comperand: int64): int64;
begin
  Result:=Target;
  if Result = Comperand then
    Target:=NewValue;
end;
{$endif TEST_BROKEN_CompareExchange}

{$ifdef TEST_BROKEN_IncDec}
function InterlockedIncrement64(var Target: int64): int64;
begin
  Result:=Target;
  Inc(Target);
end;

function InterlockedDecrement64(var Target: int64): int64;
begin
  Result:=Target;
  Dec(Target);
end;
{$endif TEST_BROKEN_IncDec}

{$ifdef TEST_BROKEN_Exchange}
function InterLockedExchange64(var Target: int64; Source: int64): int64;
begin
  Result:=Target;
  Target:=Source;
end;
{$endif TEST_BROKEN_Exchange}

{$ifdef TEST_BROKEN_ExchangeAdd}
function InterLockedExchangeAdd64(var Target: int64; Source: int64): int64;
begin
  Result:=Target;
  Inc(Target, Source);
end;
{$endif TEST_BROKEN_ExchangeAdd}

procedure CheckResult(check, expected, code: int64; const Msg: string);
begin
  if check <> expected then begin
    writeln(Msg, ' Result: ', check, '; Expected: ', expected);
    Halt(code);
  end;
end;

constructor TWorker.Create(ACount: int64; AOp: TOperation; AOption: int64);
begin
  FCount:=ACount;
  FOp:=AOp;
  FOption:=AOption;
  inherited Create(True);
  FreeOnTerminate:=True;
  if FOp = opCompareExchange then
    Priority:=tpHighest;
end;

procedure TWorker.Execute;
var
  i : longint;
  j, k, opt: int64;
  t: TDateTime;
begin
  InterLockedIncrement64(WorkingCount);
  Sleep(10);

  case FOp of
    opAdd:
      begin
        for i:=1 to FCount do begin
          InterLockedIncrement64(Counter);
          if AbortThread then
            break;
        end;
      end;
    opDec:
      begin
        for i:=1 to FCount do begin
          InterLockedDecrement64(Counter);
          if AbortThread then
            break;
        end;
      end;
    opExchange:
      begin
        for i:=1 to FCount do begin
          j:=InterLockedExchange64(Counter, 10);
          InterLockedExchangeAdd64(Counter, j - 10);
          if AbortThread then
            break;
        end;
      end;
    opExchangeAdd:
      begin
        for i:=1 to FCount do begin
          InterLockedExchangeAdd64(Counter, 3);
          if AbortThread then
            break;
        end;
      end;
    opExchangeDec:
      begin
        for i:=1 to FCount do begin
          InterLockedExchangeAdd64(Counter, -3);
          if AbortThread then
            break;
        end;
      end;
    opCompareExchange:
      begin
        opt:=FOption and 1;
        for i:=1 to FCount do begin
          t:=Now;
          j:=0;
          while not AbortThread do begin
            k:=InterLockedCompareExchange64(Counter3, FOption, opt);
            if k = opt then
              break;
            if (k < 0) or (k >= LastCompareVal) then begin
              writeln('InterLockedCompareExchange. Invalid return value (', k, ').');
              Halt(10);
            end;
            Inc(j);
            if j and $F = 0 then
              ThreadSwitch;
            if j and $FFFF = 0 then begin
              if Now - t >= 30/SecsPerDay then begin
                writeln('InterLockedCompareExchange seems to be broken.');
                Halt(12);
              end;
              Sleep(1);
            end;
          end;
          if AbortThread then
            break;
          ThreadSwitch;
          k:=InterLockedExchange64(Counter3, opt xor 1);
          if k <> FOption then begin
            writeln('InterLockedCompareExchange seems to be broken (', k, ').');
            Halt(11);
          end;
          InterLockedIncrement64(Counter2);
        end;
      end;
  end;

  InterLockedIncrement64(FinishedCount);
end;

procedure Run;
var
  i : longint;
  j, k, CmpCount, ThreadCount: int64;
  t: TDateTime;
  workers: array[0..TotalThreadCount - 1] of TWorker;
begin
  Counter:=0;
  Counter2:=0;
  Counter3:=0;
  CmpCount:=TestCount div 1000;
  writeln('Creating threads...');
  j:=0;
  k:=2;
  repeat
    i:=j;
    workers[j]:=TWorker.Create(TestCount, opAdd);
    Inc(j);
    workers[j]:=TWorker.Create(TestCount, opDec);
    Inc(j);
    workers[j]:=TWorker.Create(TestCount div 3, opExchange);
    Inc(j);
    workers[j]:=TWorker.Create(TestCount, opExchangeAdd);
    Inc(j);
    workers[j]:=TWorker.Create(TestCount, opExchangeDec);
    Inc(j);
    workers[j]:=TWorker.Create(CmpCount, opCompareExchange, k);
    Inc(j);
    Inc(k);
    workers[j]:=TWorker.Create(CmpCount, opCompareExchange, k);
    Inc(j);
    Inc(k);
  until j + (j - i) > TotalThreadCount;
  ThreadCount:=j;
  LastCompareVal:=k;
  writeln('Created ', ThreadCount ,' threads.');

  writeln('Starting threads...');
  t:=Now;
  for i:=0 to ThreadCount - 1 do begin
    workers[i].Suspended:=False;
    if Now -  t > 30/SecsPerDay then begin
      writeln('Threads start takes too long to complete.');
      Halt(4);
    end;
  end;

  t:=Now;
  while WorkingCount <> ThreadCount do begin
    if Now -  t > 30/SecsPerDay then begin
      writeln('Not all threads have started: ', ThreadCount - WorkingCount);
      Halt(5);
    end;
    Sleep(10);
  end;

  writeln('Waiting for threads to complete...');
  t:=Now;
  while FinishedCount <> ThreadCount do begin
    if Now -  t > WaitTime/SecsPerDay then begin
      if AbortThread then begin
        writeln('Unable to abort threads.');
        Halt(3);
      end
      else begin
        AbortThread:=True;
        writeln('Timeout has expired. Active threads left: ', ThreadCount - FinishedCount);
        t:=Now;
      end;
    end;
    Sleep(10);
  end;

  if AbortThread then begin
    writeln('The execution is too slow (', Counter2, ').');
    Halt(2);
  end;

  t:=Now - t;
  if t = 0 then
    t:=1/MSecsPerDay;

  CheckResult(Counter, 0, 20, 'Counter error:');

  CheckResult(Counter2, (LastCompareVal - 2)*CmpCount, 21, 'Counter2 error:');

  writeln('Test OK.');
  writeln('InterLockedCompareExchange: ', Round(Counter2/(t*SecsPerDay)), ' ops/sec.');
end;

begin
  Run;
end.

{$else CPU64}
begin
end.
{$endif CPU64}
