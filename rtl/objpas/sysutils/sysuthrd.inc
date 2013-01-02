{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005,2009 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef FPC_HAS_FEATURE_THREADING}
constructor TSimpleRWSync.Create;
begin
  System.InitCriticalSection(Crit);
end;

destructor TSimpleRWSync.Destroy;
begin
  System.DoneCriticalSection(Crit);
end;

function  TSimpleRWSync.Beginwrite : boolean;
begin
  System.EnterCriticalSection(Crit);
  result:=true;
end;

procedure  TSimpleRWSync.Endwrite;
begin
  System.LeaveCriticalSection(Crit);
end;

procedure  TSimpleRWSync.Beginread;
begin
  System.EnterCriticalSection(Crit);
end;

procedure  TSimpleRWSync.Endread;
begin
  System.LeaveCriticalSection(Crit);
end;



constructor TMultiReadExclusiveWriteSynchronizer.Create;
begin
  System.InitCriticalSection(fwritelock);
  fwaitingwriterlock:=RTLEventCreate;
  RTLEventResetEvent(fwaitingwriterlock);
  { make sure all threads see the initialisation of fwritelock and
    freadercount (we only use atomic operation further on as well) }
  System.InterlockedExchange(freadercount,0);
  System.InterlockedExchange(fwritelocked,0);
  freaderqueue:=BasicEventCreate(nil,true,false,'');
end;


destructor TMultiReadExclusiveWriteSynchronizer.Destroy;
begin
  System.InterlockedExchange(fwritelocked,0);
  System.DoneCriticalSection(fwritelock);
  RtlEventDestroy(fwaitingwriterlock);
  BasicEventDestroy(freaderqueue);
end;


function  TMultiReadExclusiveWriteSynchronizer.Beginwrite : boolean;
begin
  { wait for any other writers that may be in progress }
  System.EnterCriticalSection(fwritelock);
  { it is possible that earlier on we missed waiting on the
    fwaitingwriterlock and that it's still set (must be done
    after acquiring the fwritelock, because otherwise one
    writer could reset the fwaitingwriterlock of another one
    that's about to wait on it) }
  RTLeventResetEvent(fwaitingwriterlock);
  { new readers have to block from now on; writers get priority to avoid
    writer starvation (since they have to compete with potentially many
    concurrent readers and other writers) }
  BasicEventResetEvent(freaderqueue);
  { for quick checking by candidate-readers -- use interlockedincrement/
    decrement instead of setting 1/0, because a single thread can
    recursively acquire the write lock multiple times }
  System.InterlockedIncrement(fwritelocked);

  { wait until all readers are gone -- freadercount and fwritelocked are only
    accessed using atomic operations (that's why we use
    InterLockedExchangeAdd(x,0) below) -> always in-order. The writer always
    first sets fwritelocked and then checks freadercount, while the readers
    always first increase freadercount and then check fwritelocked }
  while (System.InterLockedExchangeAdd(freadercount,0)<>0) do
    RTLEventWaitFor(fwaitingwriterlock);

  { Make sure that out-of-order execution cannot already perform reads
    inside the critical section before the lock has been acquired }
  ReadBarrier;

  { we have the writer lock, and all readers are gone }
  result:=true;
end;


procedure  TMultiReadExclusiveWriteSynchronizer.Endwrite;
begin
  { Finish all writes inside the section so that everything executing
    afterwards will certainly see these results }
  WriteBarrier;

  { signal potential readers that the coast is clear if all recursive
    write locks have been freed }
  if System.InterlockedDecrement(fwritelocked)=0 then
    begin
      { wake up waiting readers (if any); do not check first whether freadercount
        is <> 0, because the InterlockedDecrement in the while loop of BeginRead
        can have already occurred, so a single reader may be about to wait on
        freaderqueue even though freadercount=0. Setting an event multiple times
        is no problem. }
      BasicEventSetEvent(freaderqueue);
    end;
  { free the writer lock so another writer can become active }
  System.LeaveCriticalSection(fwritelock);
end;


procedure  TMultiReadExclusiveWriteSynchronizer.Beginread;
Const
  wrSignaled = 0;
  wrTimeout  = 1;
  wrAbandoned= 2;
  wrError    = 3;
begin
  System.InterlockedIncrement(freadercount);
  { wait until there is no more writer }
  while System.InterLockedExchangeAdd(fwritelocked,0)<>0 do
    begin
      { there's a writer busy or wanting to start -> wait until it's
        finished; a writer may already be blocked in the mean time, so
        wake it up if we're the last to go to sleep }
      if System.InterlockedDecrement(freadercount)=0 then
        RTLEventSetEvent(fwaitingwriterlock);
      if (BasicEventWaitFor(high(cardinal),freaderqueue) in [wrAbandoned,wrError]) then
        raise Exception.create('BasicEventWaitFor failed in TMultiReadExclusiveWriteSynchronizer.Beginread');
      { and try again: first increase freadercount, only then check
        fwritelocked }
      System.InterlockedIncrement(freadercount);
    end;
  { Make sure that out-of-order execution cannot perform reads
    inside the critical section before the lock has been acquired }
  ReadBarrier;
end;


procedure  TMultiReadExclusiveWriteSynchronizer.Endread;
begin
  { If no more readers, wake writer in the ready-queue if any. Since a writer
    always first atomically sets fwritelocked and then atomically checks the
    freadercount, first modifying freadercount and then checking fwritelock
    ensures that we cannot miss one of the events regardless of execution
    order. }
  if (System.InterlockedDecrement(freadercount)=0) and
     (System.InterLockedExchangeAdd(fwritelocked,0)<>0) then
    RTLEventSetEvent(fwaitingwriterlock);
end;
{$endif FPC_HAS_FEATURE_THREADING}

