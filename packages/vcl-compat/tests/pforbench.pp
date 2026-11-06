{ Benchmark of TParallel.For against a serial loop, 3 runs of each.
  Optional argument: number of iterations, default 10 million. }
program pforbench;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,
  Classes,
  System.Diagnostics,
  System.Threading;

const
  WORK_REPEAT = 8;
  RUNS = 3;

var
  ResultCount: Integer = 10 * 1000 * 1000;
  SerialChecksum: Int64 = 0;
  ParallelChecksum: Int64 = 0;
  SerialResults: TArray<Int64>;
  ParallelResults: TArray<Int64>;

// Synthetic CPU-bound work for iteration aValue.
function DoWork(aValue: Integer): Int64;

var
  lI: Integer;
  lX: Double;

begin
  lX:=aValue+1.0;
  for lI:=1 to WORK_REPEAT do
    lX:=Sqrt(lX*1.000001+3.14159);
  Result:=Round(lX);
end;


// Fill the array with a plain loop and sum it.
procedure RunSerial;

var
  lWatch: TStopwatch;
  lI: Integer;

begin
  SetLength(SerialResults,ResultCount);
  SerialChecksum:=0;
  lWatch:=TStopwatch.StartNew;
  for lI:=0 to ResultCount-1 do
    SerialResults[lI]:=DoWork(lI);
  for lI:=0 to ResultCount-1 do
    Inc(SerialChecksum,SerialResults[lI]);
  lWatch.Stop;
  Writeln(Format('Serial   : %8d ms, checksum=%d',[lWatch.ElapsedMilliseconds,SerialChecksum]));
end;


// Fill the array with TParallel.For and sum it.
procedure RunParallel;

var
  lWatch: TStopwatch;
  lI: Integer;

begin
  SetLength(ParallelResults,ResultCount);
  ParallelChecksum:=0;
  lWatch:=TStopwatch.StartNew;
  TParallel.&For(0,ResultCount-1,
    procedure(aIndex: Integer)
    begin
      ParallelResults[aIndex]:=DoWork(aIndex);
    end);
  for lI:=0 to ResultCount-1 do
    Inc(ParallelChecksum,ParallelResults[lI]);
  lWatch.Stop;
  Writeln(Format('Parallel : %8d ms, checksum=%d',[lWatch.ElapsedMilliseconds,ParallelChecksum]));
end;


var
  lRun: Integer;

begin
  if ParamCount>0 then
    ResultCount:=StrToInt(ParamStr(1));
  Writeln(Format('ProcessorCount = %d, iterations = %d',[TThread.ProcessorCount,ResultCount]));
  for lRun:=1 to RUNS do
    begin
    RunSerial;
    RunParallel;
    if SerialChecksum<>ParallelChecksum then
      Writeln('Checksum mismatch');
    end;
end.
