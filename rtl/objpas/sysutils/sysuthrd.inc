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


type
  PMREWThreadInfo = ^TMREWThreadInfo;
  TMREWThreadInfo = record
    Next: PMREWThreadInfo;
    Active: longint;
    RefCount: LongInt;
    ThreadID: TThreadID;
    end;

const
  cInUse: LongInt = MaxInt;
  cAvail: LongInt = 0;

const
  cNewReader : LongInt  = -       $1;
  cNewWriter : LongInt  =     $10000;
  cReadMask  : LongInt  =  $0000FFFF;
  cWriteMask : LongInt  =  $7FFF0000;


constructor TMultiReadExclusiveWriteSynchronizer.Create;
begin
  System.InitCriticalSection(fwritelock);
  fwaitingwriterlock:=RTLEventCreate;
  RTLEventResetEvent(fwaitingwriterlock);
  fwriterequests:=0;
  factivethreads:=0;
  freaderqueue:=BasicEventCreate(nil,true,false,'');
  { synchronize initialization with later reads/writes }
  ReadWriteBarrier;
end;


destructor TMultiReadExclusiveWriteSynchronizer.Destroy;
var
  p,q: PMREWThreadInfo;
  i: integer;
begin
  System.DoneCriticalSection(fwritelock);
  RtlEventDestroy(fwaitingwriterlock);
  BasicEventDestroy(freaderqueue);

  { Clean up thread info }
  for i:=Low(fThreadList) to High(fThreadList) do
    begin
      q:=fThreadList[i];
      fThreadList[i]:=nil;
      while q<>nil do
        begin
          p:=q;
          q:=q^.Next;
          FreeMem(p);
        end;
    end;
end;


function  TMultiReadExclusiveWriteSynchronizer.Beginwrite : boolean;
var
  p: PMREWThreadInfo;
begin
  { Result indicates whether the protected memory was not modified,
    that is, it is set to false if another writer could have chagned
    memory}
  Result:=True;

  { for quick checking by candidate-readers -- use interlockedincrement/
    decrement instead of setting 1/0, because a single thread can
    recursively acquire the write lock multiple times }
  { Count pending not granted requests so writes always take priority over
    read requests}
  System.InterlockedIncrement(fwriterequests);

  { Get per thread counter }
  p:=PMREWThreadInfo( GetThreadInfo(True) );

  if System.TryEnterCriticalSection(fwritelock)=0 then
     begin
       { TryEnterCriticalSection failed, so not first in the write lock queue }
       Result:=False;

       { If we hold a read lock, then a deadlock will result.. This is because
         the first thread in the write queue (holding fwritelock) does not have
         mutexes write lock, as it must be waiting on this thread to release
         its' read lock }
       if p^.RefCount > 0 then
          begin
            System.InterlockedDecrement(fwriterequests);
            raise TMREWException.Create('Deadlock detected');
          end;

       { wait for any other writers that may be in progress }
       System.EnterCriticalSection(fwritelock);
     end;

  { Need to synchronize with readers only when this is the first
    write request by this thread }
  if (p^.RefCount and cWriteMask)=0 then
    begin
      { Count active threads rather than readers.  A write request can
        be granted whenever the count has reduced to one }

      { no order vs increment of fwriterequests needed, because we acquired
        a critical section which orders for us }
      if p^.RefCount=0 then
        System.InterlockedIncrement(factivethreads);

      { new readers have to block from now on; writers get priority to avoid
        writer starvation (since they have to compete with potentially many
        concurrent readers and other writers) }
      BasicEventResetEvent(freaderqueue);

      { it is possible that earlier on we missed waiting on the
        fwaitingwriterlock and that it's still set (must be done
        after acquiring the fwritelock, because otherwise one
        writer could reset the fwaitingwriterlock of another one
        that's about to wait on it) }
      RTLeventResetEvent(fwaitingwriterlock);

      { wait until we are the only active thread (no need for memory order
        barriers, because RTLeventResetEvent and RTLEventWaitFor imply a
        full barrier) }
      while System.InterlockedExchangeAdd(factivethreads,0) > 1 do
        RTLEventWaitFor(fwaitingwriterlock);

      { Make sure that out-of-order execution cannot already perform reads
        inside the critical section before the lock has been acquired }
      ReadBarrier;
    end;

  { Count write lock acquisitions by thread  }
  Inc(p^.RefCount,cNewWriter);
end;


procedure  TMultiReadExclusiveWriteSynchronizer.Endwrite;
var
  p: PMREWThreadInfo;
begin
  p:=PMREWThreadInfo( GetThreadInfo(False) );

  { Protect against EndWrite called out of sequence blocking lock }
  if (p<>nil) and
     ((p^.RefCount and cWriteMask)<>0) then
    begin
      { Update per thread counter before releasing next write thread }
      Dec(p^.RefCount,cNewWriter);

      { Finish all writes inside the section, and the update of the RefCount,
        so that everything executing afterwards will certainly see these
        results }
      WriteBarrier;

      { Reduce active thread count assuming it was not previously a read lock }
      if p^.RefCount=0 then
        begin
          System.InterlockedDecrement(factivethreads);
          { order w.r.t. decrement of fwriterequests below }
          WriteBarrier;
        end;

      { signal potential readers that the coast is clear if all recursive
        write locks have been freed }
      { Also test for pending write requests }
      if System.InterlockedDecrement(fwriterequests)=0 then
        begin
          { No more writers pending, wake any pending readers }
          BasicEventSetEvent(freaderqueue);
        end;

      { free the writer lock so another writer can become active }
      System.LeaveCriticalSection(fwritelock);

      { Remove reference to thread if not in use any more }
      if p^.RefCount=0 then
        RemoveThread(p);
    end
  else
    raise TMREWException.Create('EndWrite called before BeginWrite');
end;


procedure  TMultiReadExclusiveWriteSynchronizer.Beginread;
Const
  wrSignaled = 0;
  wrTimeout  = 1;
  wrAbandoned= 2;
  wrError    = 3;
var
  p: PMREWThreadInfo;
begin
  { Check if we already have a lock, if so grant immediate access }
  p:=PMREWThreadInfo( GetThreadInfo(True) );

  if p^.RefCount=0 then
    begin
      { Wanted non-recursive read lock, so increase active threads }
      System.InterlockedIncrement(factivethreads);

      { wait until there are no more writer active or pending }
      ReadWriteBarrier;
      while System.InterlockedExchangeAdd(fwriterequests,0)<>0 do
        begin
          ReadWriteBarrier;
          { This thread is not active }
          if System.InterlockedDecrement(factivethreads)<>0 then
            RTLEventSetEvent(fwaitingwriterlock);

          if (BasicEventWaitFor(high(cardinal),freaderqueue) in [wrAbandoned,wrError]) then
            raise TMREWException.create('BasicEventWaitFor failed in TMultiReadExclusiveWriteSynchronizer.Beginread');

          { Try again to make this thread active }
          System.InterlockedIncrement(factivethreads);
          { order w.r.t. reading fwriterequests }
          ReadWriteBarrier;
        end;

      { Make sure that out-of-order execution cannot perform reads
        inside the critical section before the lock has been acquired }
      ReadBarrier;
    end;
  { Count read lock acquisitions by thread: Inc(p^.RefCount,cNewReader)  }
  Inc(p^.RefCount);
end;


procedure  TMultiReadExclusiveWriteSynchronizer.Endread;
var
  p: PMREWThreadInfo;
begin
  p:=PMREWThreadInfo( GetThreadInfo(False) );
  if (p<>nil) and
     ((p^.RefCount and cReadMask)<>0) then
    begin
      { Update per thread counter: Dec(p^.RefCount,cNewReader) }
      Dec(p^.RefCount);

      { if this is the last recursive call }
      if p^.RefCount=0 then
        begin
          { Thread no longer has an active lock }
          { If no more readers, wake writer in the ready-queue if any.
            Every queued thread requesting a write lock increments fwriterequests,
            and the first queued writer thread checks factivethreads is active
            (will be set already during lock promotion) }
          if System.InterlockedDecrement(factivethreads)=1 then
            begin
              { order w.r.t. access to factivethreads }
              ReadBarrier;
              if fwriterequests>0 then
                RTLEventSetEvent(fwaitingwriterlock);
            end;

          { remove reference to this thread }
          RemoveThread(p);
        end;
    end
  else
    raise TMREWException.Create('EndRead called before BeginRead');
end;

function TMultiReadExclusiveWriteSynchronizer.ThreadIDtoIndex(aThreadID: TThreadID): integer;
begin
  Result:=
    (
     ptruint(aThreadID) xor (ptruint(aThreadID) shr 12)
{$ifdef cpu64}
     xor (ptruint(aThreadID) shr 32) xor (ptruint(aThreadID) shr 36) xor (ptruint(aThreadID) shr 48)
{$endif}
    ) and $FFFF;
  Result:=(Result xor (Result shr 4)) and $0F;         // Return range 0..15
end;

function TMultiReadExclusiveWriteSynchronizer.GetThreadInfo(AutoCreate: Boolean): Pointer;
var
   p: PMREWThreadInfo;
   AThreadID: TThreadID;
   FreeSlot: Boolean;
   OldState: LongInt;
   Index: integer;
begin
  FreeSlot:=False;
  AThreadID:=ThreadID;
  Index:=ThreadIDtoIndex( AThreadID );
  p:=PMREWThreadInfo(fThreadList[Index]);
  while (p<>nil) and
        (p^.ThreadID<>AThreadID) do
    begin
      if p^.Active=cAvail then         // Is slot available for use
        FreeSlot:=True;                // Yes, remember in case we need it as this is a new thread
      p:=p^.Next;
      ReadBarrier;
    end;

  if p=nil then
     begin
       { count threads with locks }
       if FreeSlot then
         begin
           p:=fThreadList[Index];
           while (p<>nil) do
             begin
               if p^.Active=cAvail then
                 begin
                   OldState:=InterlockedExchange( p^.Active, cInUse );
                   if OldState=cAvail then
                     begin
                       p^.ThreadID:=AThreadID;  // Tag to thread
                       Break;
                     end;
                 end;
               p:=p^.Next;
               ReadBarrier;
             end;
         end;

       if p=nil then
          begin
            p:=PMREWThreadInfo(AllocMem(SizeOf(TMREWThreadInfo)));
            p^.ThreadID:=AThreadID;
            p^.RefCount:=0;
            p^.Active:=cInUse;
            { Now insert into the chain header }
            p^.Next:=p;
            WriteBarrier;
            { other threads will spin (loop) after the InterlockedExchange
              until the field "next" is written, then this node is first in
              the forward linked list }

            p^.Next:=System.InterlockedExchange(fThreadList[Index],p);
          end;
     end;

  Result:=p;
end;

procedure TMultiReadExclusiveWriteSynchronizer.RemoveThread(AThreadInfo: Pointer);
var
  p: PMREWThreadInfo;
begin
  p:=PMREWThreadInfo(AThreadInfo);
  if p<>nil then
    begin
      { Prevent matching during GetThreadInfo }
      p^.ThreadID:=tthreadid(-1);
      WriteBarrier;
      { Mark slot available }
      p^.Active:=cAvail;
    end;
end;
{$endif FPC_HAS_FEATURE_THREADING}

