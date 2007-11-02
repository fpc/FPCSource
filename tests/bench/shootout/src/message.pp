{ The Computer Language Shootout
  http://shootout.alioth.debian.org
  contributed by Marc Weustink
}
program message;
{$mode objfpc}{$h-}
uses
  PThreads;

var
  PostOffice: array[0..499] of record
    Queue: array[0..15] of Integer;  // queuelength must be power of 2
    ReadIdx, WriteIdx: Integer;
    ReadSem, WriteSem: TSemaphore;
  end;
  ThreadAttr: TThreadAttr;
  ThreadFuncAddr: TStartRoutine;
  Sum: Integer = 0;
  FinishedSem: TSemaphore;

procedure PostMessage(AIndex, AMessage: Integer);
begin
  with PostOffice[AIndex] do begin
    sem_wait(WriteSem);
    Queue[WriteIdx] := AMessage;
    sem_post(ReadSem);
    WriteIdx := (WriteIdx + 1) and (Length(Queue) - 1);
  end;
end;

function ReadMessage(AIndex: Integer): Integer;
begin
  with PostOffice[AIndex] do begin
    sem_wait(ReadSem);
    Result := Queue[ReadIdx];
    sem_post(WriteSem);
    ReadIdx := (ReadIdx + 1) and (Length(Queue) - 1);
  end;
end;

function ThreadFunc(ANum: PtrInt): Pointer; cdecl;
var
  Value: Integer;
  Id: TThreadID;
begin
  if ANum <> 0
  then pthread_create(@Id, @ThreadAttr, ThreadFuncAddr, Pointer(ANum-1));

  repeat
    Value := ReadMessage(ANum);
    if Value <> -1
    then begin
      Inc(Value);
      if ANum = 0
      then Inc(Sum, Value)
      else PostMessage(ANum-1, Value);
    end
    else begin
      if ANum = 0
      then sem_post(@FinishedSem)
      else PostMessage(ANum-1, Value);
      //Break;
    end;
  until False;
end;


var
  n, count: Integer;
  Id: TThreadId;
begin
  Val(paramstr(1), count, n);
  if n <> 0 then exit;

  for n := 0 to High(PostOffice) do with PostOffice[n] do begin
    ReadIdx := 0;
    WriteIdx := 0;
    sem_init(@ReadSem, 0, 0);
    sem_init(@WriteSem, 0, Length(Queue));
  end;
  
  sem_init(FinishedSem, 0, 0);

  pthread_attr_init(@ThreadAttr);
  pthread_attr_setdetachstate(@ThreadAttr, 1);
  pthread_attr_setstacksize(@ThreadAttr, 1024 * 16);
  
  ThreadFuncAddr := TStartRoutine(@ThreadFunc);
  pthread_create(@Id, @ThreadAttr, ThreadFuncAddr, Pointer(High(PostOffice)));

  for n := 1 to count do
    PostMessage(High(PostOffice), 0);

  PostMessage(High(PostOffice), -1);
  
  sem_wait(FinishedSem);
  WriteLn(Sum);
end.
