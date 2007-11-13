{ The Computer Language Shootout
  http://shootout.alioth.debian.org

  contributed by Marc Weustink
}
program thread_ring;
{$mode objfpc}{$h-}{$i-}
uses
  PThreads;

var
  SemList: array[1..503] of TSemaphore;

  ThreadAttr: TThreadAttr;
  ThreadFuncAddr: TStartRoutine;
  FinishedSem: TSemaphore;
  Count: Integer;
  
function ThreadFunc(AIndex: PtrInt): Pointer; cdecl;
var
  MySem, NextSem: PSemaphore;
  Id: TThreadID;
begin
  MySem := @SemList[AIndex];
  if AIndex < High(SemList)
  then begin
    NextSem := MySem+1;
    sem_init(NextSem, 0, 0);
    pthread_create(@Id, @ThreadAttr, ThreadFuncAddr, Pointer(AIndex+1));
  end
  else NextSem := @SemList[Low(SemList)];

  repeat
    sem_wait(MySem);
    if Count = 0 then begin
      WriteLn(Aindex);
      sem_post(FinishedSem);
    end
    else begin
      Dec(Count);
      sem_post(NextSem);
    end;
  until False;
end;


var
  n: Integer;
  Id: TThreadId;
begin
  Val(paramstr(1), count, n);
  if n <> 0 then exit;

  sem_init(SemList[Low(SemList)], 0, 1);
  sem_init(FinishedSem, 0, 0);

  pthread_attr_init(@ThreadAttr);
  pthread_attr_setdetachstate(@ThreadAttr, 1);
  pthread_attr_setstacksize(@ThreadAttr, 1024 * 16);

  ThreadFuncAddr := TStartRoutine(@ThreadFunc);
  pthread_create(@Id, @ThreadAttr, ThreadFuncAddr, Pointer(PtrUInt(Low(SemList))));

  sem_wait(FinishedSem);
end.
