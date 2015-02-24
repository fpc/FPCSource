
type
  TThreadEntryfunction = function(data: Pointer): Pointer; cdecl;


  TMutextKind = (mkExclusive, mkShared);

  TAROSMutex = record
    Semaphore: TSignalSemaphore;
  end;
  PAROSMutex = ^TAROSMutex;
  
  TCondition = record
    Lock: TSignalSemaphore;
    Waiters: array of Pointer;
  end;
  PCondition = ^TCondition;

  TAROSThread = record
    Entry: TThreadEntryfunction;
    Data: Pointer;
    ThreadID: LongWord;
    Priority: LongInt;
    StackSize: LongInt;
    Task: PProcess;
    Lock: TSignalSemaphore;
    StartupSemaphore: TSignalSemaphore;
    EndCondition: PCondition;
    EndMutex: PAROSMutex;
    EndCount: Integer;
  end;
  PAROSThread = ^TAROSThread;
  
  TAROSThreadStruct = record
    MutexListSem: TSignalSemaphore;
    MutexList: array of PAROSMutex;
    //
    ThreadListSem: TSignalSemaphore;
    ThreadList: array of PAROSThread;
    //
    ConditionListSem: TSignalSemaphore;
    ConditionList: array of PCondition;
    //
    ThreadMemSem: TSignalSemaphore;
    EmptySemaphore: TSignalSemaphore;
    //
    LastThreadNum: LongWord;
  end;
  PAROSThreadStruct = ^TAROSThreadStruct;

var
  AROSThreadStruct: PAROSThreadStruct external name 'AROS_THREADLIB';


function CreateNewProcTags(const Tags: array of PtrUInt): PProcess;
begin
  CreateNewProcTags := CreateNewProc(@Tags[0]);
end; 

// Mutexe

function CreateMutex: PAROSMutex;
var
  Mutex: PAROSMutex;
  Idx, i: Integer;
begin
  if not Assigned(AROSThreadStruct) then
    Exit;
  New(Mutex);
  InitSemaphore(@(Mutex^.Semaphore));
  ObtainSemaphore(@(AROSThreadStruct^.MutexListSem));
  Idx := -1;
  for i := 0 to High(AROSThreadStruct^.MutexList) do
  begin
    if not Assigned(AROSThreadStruct^.MutexList[i]) then
    begin
      Idx := i;
      Break;
    end;
  end;
  if Idx < 0 then
  begin
    Idx := Length(AROSThreadStruct^.MutexList);
    SetLength(AROSThreadStruct^.MutexList, Idx + 1);
  end;
  AROSThreadStruct^.MutexList[Idx] := Mutex;
  ReleaseSemaphore(@(AROSThreadStruct^.MutexListSem));
  Result := Mutex;
end;

procedure DestroyMutex(Mutex: PAROSMutex);
var
  i: Integer;
begin
  if not Assigned(AROSThreadStruct) then
    Exit;
  ObtainSemaphore(@(AROSThreadStruct^.MutexListSem));
  for i := 0 to High(AROSThreadStruct^.MutexList) do
  begin
    if AROSThreadStruct^.MutexList[i] = Mutex then
    begin
      FillChar(Mutex^.Semaphore, SizeOf(TSignalSemaphore), 0);
      Dispose(Mutex);
      AROSThreadStruct^.MutexList[i] := nil;
    end;
  end;
  ReleaseSemaphore(@(AROSThreadStruct^.MutexListSem));  
end;


function IsValidMutex(Mutex: PAROSMutex): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not Assigned(AROSThreadStruct) then
    Exit;
  ObtainSemaphore(@(AROSThreadStruct^.MutexListSem));
  for i := 0 to High(AROSThreadStruct^.MutexList) do
  begin
    if AROSThreadStruct^.MutexList[i] = Mutex then
    begin
      Result := True;
      Break;
    end;
  end;
  ReleaseSemaphore(@(AROSThreadStruct^.MutexListSem));
end;

procedure LockMutex(Mutex: PAROSMutex);
begin
  if IsValidMutex(Mutex) then
  begin
    ObtainSemaphore(@(Mutex^.Semaphore));  
  end;
end;

function TryLockMutex(Mutex: PAROSMutex): Boolean;
begin
  Result := False;
  if IsValidMutex(Mutex) then
  begin
    Result := AttemptSemaphore(@(Mutex^.Semaphore)) <> 0;  
  end;
end;

procedure UnLockMutex(Mutex: PAROSMutex);
begin
  if IsValidMutex(Mutex) then
  begin
    ReleaseSemaphore(@(Mutex^.Semaphore));  
  end;
end;

// Conditions

function CreateCondition: PCondition;
var
  Idx, i: Integer;
  NewCond: PCondition;
begin
  if not Assigned(AROSThreadStruct) then
    Exit;
  New(NewCond);
  SetLength(NewCond^.Waiters, 0);
  InitSemaphore(@(NewCond^.Lock));
  ObtainSemaphore(@(AROSThreadStruct^.ConditionListSem));
  Idx := -1;
  for i := 0 to High(AROSThreadStruct^.ConditionList) do
  begin
    if not Assigned(AROSThreadStruct^.ConditionList[i]) then
    begin
      Idx := i;
      Break;
    end;
  end;
  if Idx < 0 then
  begin
    Idx := Length(AROSThreadStruct^.ConditionList);
    SetLength(AROSThreadStruct^.ConditionList, Idx + 1);
  end;
  AROSThreadStruct^.ConditionList[Idx] := NewCond;
  ReleaseSemaphore(@(AROSThreadStruct^.ConditionListSem));
  Result := NewCond;
end;

function DestroyCondition(Cond: PCondition): boolean;
var
  Idx, i: Integer;
begin
  if not Assigned(AROSThreadStruct) then
    Exit;
  ObtainSemaphore(@(Cond^.Lock));
  if Length(Cond^.Waiters) > 0 then
  begin
    ReleaseSemaphore(@(Cond^.Lock));
    Result := False;
    Exit;
  end;
  ObtainSemaphore(@(AROSThreadStruct^.ConditionListSem));
  Idx := -1;
  for i := 0 to High(AROSThreadStruct^.ConditionList) do
  begin
    if AROSThreadStruct^.ConditionList[i] = Cond then
    begin
      AROSThreadStruct^.ConditionList[i] := nil;
      Dispose(Cond);
      Break;
    end;
  end;
  ReleaseSemaphore(@(AROSThreadStruct^.ConditionListSem));
  Result := True;
end;

function WaitCondition(Cond: PCondition; Mutex: PAROSMutex): boolean;
var
  Idx: Integer;
begin
  if (not Assigned(Cond)) or (not Assigned(Mutex)) then
  begin
    Result := False;
    Exit;
  end;
  ObtainSemaphore(@Cond^.Lock);
  Idx := Length(Cond^.Waiters);
  SetLength(Cond^.Waiters, Idx + 1);
  Cond^.Waiters[Idx] := FindTask(nil);
  ReleaseSemaphore(@Cond^.Lock);
  
  Forbid();
  UnLockMutex(Mutex);
  Wait(SIGF_SINGLE);
  Permit();
  LockMutex(Mutex);
  Result := True;
end;

procedure SignalCondition(Cond: PCondition);
var
  Waiter: PTask;
  Idx: Integer;
begin
  if not Assigned(Cond) then
    Exit;
  ObtainSemaphore(@Cond^.Lock);
  Waiter := nil;
  //debugln(' found ' + IntToStr(Cond^.Waiters.Count) + ' Waiter');
  if Length(Cond^.Waiters) > 0 then
  begin
    Idx := High(Cond^.Waiters);
    Waiter := Cond^.Waiters[Idx];
    SetLength(Cond^.Waiters, Idx);
  end;  
  ReleaseSemaphore(@Cond^.Lock);
  if not Assigned(Waiter) then
  begin
    //debugln('Waiter not assigned');
    Exit;
  end;
  //debugln('Signal Waiter');  
  Signal(Waiter, SIGF_SINGLE);
end;

procedure BroadcastCondition(Cond: PCondition);
var
  Waiter: PTask;
  I: Integer;
begin
  if not Assigned(Cond) then
    Exit;
  Waiter := nil;
  ObtainSemaphore(@Cond^.Lock);
  for i := 0 to High(Cond^.Waiters) do
  begin
    Waiter := Cond^.Waiters[i];
    Signal(Waiter, SIGF_SINGLE);
  end;
  SetLength(Cond^.Waiters, 0);
  ReleaseSemaphore(@Cond^.Lock);   
end;

// Threads

procedure StarterFunc; cdecl;
var
  NewThread: PAROSThread;
  StackMem: Pointer;
  sswap: TStackSwapStruct; 
  Proc: PTask;
begin
  Proc := FindTask(nil);
  NewThread := PAROSThread(Proc^.tc_UserData);
  // create New Stack
  StackMem := GetMem(NewThread^.StackSize);
  sswap.stk_Lower := StackMem;
  sswap.stk_Upper := Pointer(PtrUInt(sswap.stk_Lower) + NewThread^.StackSize);
  sswap.stk_Pointer := sswap.stk_Upper;
  ReleaseSemaphore(@AROSThreadStruct^.ThreadMemSem);
  // semaphore against too fast startup
  ReleaseSemaphore(@(NewThread^.StartupSemaphore));
  // swap stack, run program, swap stack back
  Stackswap(@sswap);
  NewThread^.Entry(NewThread^.Data);
  Stackswap(@sswap);
  //debugln('5');

  // Free stack memory
  ObtainSemaphore(@AROSThreadStruct^.ThreadMemSem);
  FreeMem(StackMem);
  ReleaseSemaphore(@AROSThreadStruct^.ThreadMemSem);
  // finished mark as finished
	ObtainSemaphore(@NewThread^.Lock);
	NewThread^.Task := nil;
	ReleaseSemaphore(@NewThread^.Lock);
	// tell the others we are finished!
	//Debugln('wait for end ' + IntToStr(NewThread^.ThreadId));
	LockMutex(NewThread^.EndMutex);
	BroadcastCondition(NewThread^.EndCondition);
	UnLockMutex(NewThread^.EndMutex);
	//Debugln('End ' + IntToStr(NewThread^.ThreadId));	
end;

procedure EmptyFunc;
begin
  Delay(1);
  ReleaseSemaphore(@AROSThreadStruct^.EmptySemaphore);
end;


function AROSCreateThread(Entry: TThreadEntryfunction; data: Pointer; StackSize: Integer = 262144; Priority: Integer = 0): LongWord;
var
  NewThread: PAROSThread;
  Idx, i: Integer;
begin
  if not Assigned(AROSThreadStruct) then
    Exit;
  New(NewThread);
  ObtainSemaphore(@AROSThreadStruct^.ThreadListSem);
  Idx := -1;
  for i := 0 to High(AROSThreadStruct^.ThreadList) do
  begin
    if not Assigned(AROSThreadStruct^.ThreadList[i]) then
    begin
      Idx := i;
      Break;
    end;
  end;
  if Idx < 0 then
  begin
    Idx := Length(AROSThreadStruct^.ThreadList);
    SetLength(AROSThreadStruct^.ThreadList, Idx + 1);
  end;
  Inc(AROSThreadStruct^.LastThreadNum);
  AROSThreadStruct^.ThreadList[Idx] := NewThread; 
  NewThread^.ThreadID := AROSThreadStruct^.LastThreadNum;
  NewThread^.Entry := Entry;
  NewThread^.Data := Data;
  NewThread^.Priority := Priority;
  NewThread^.StackSize := StackSize;
  InitSemaphore(@(NewThread^.Lock));
  InitSemaphore(@(NewThread^.StartupSemaphore));
  NewThread^.EndCondition := CreateCondition;
  NewThread^.EndMutex := CreateMutex;
  NewThread^.EndCount := 0;
  ReleaseSemaphore(@AROSThreadStruct^.ThreadListSem);
  
  ObtainSemaphore(@AROSThreadStruct^.ThreadMemSem);
  
  // Semaphore for too fast startup
  ObtainSemaphore(@(NewThread^.StartupSemaphore));
  
  // a very ugly Bugfix, for crashing AROS, on the very first Task after reboot
  // recheck later if can be removed
  if NewThread^.ThreadID  = 1 then
  begin
    //debugln('make empty thread');
    ObtainSemaphore(@AROSThreadStruct^.EmptySemaphore);
    NewThread^.Task := CreateNewProcTags([
      NP_Entry, PtrUInt(@EmptyFunc),
      TAG_DONE, TAG_END]);
    ObtainSemaphore(@AROSThreadStruct^.EmptySemaphore);
    Delay(1);
  end;
  //
  NewThread^.Task := CreateNewProcTags([
    NP_Entry, PtrUInt(@StarterFunc),
    //NP_Name, PtrUInt(PChar('Thread' + IntToStr(LastThreadNum))),
    //NP_StackSize, 10024 * 1024,
    NP_Priority, Priority, 
    NP_UserData, PtrUInt(NewThread),
    TAG_DONE, TAG_END]);
  Result := NewThread^.ThreadID;
end;

function AROSCurrentThread: LongInt;
var
  Task: PProcess;
  i: Integer;
begin
  Result := 0;
  Task := PProcess(FindTask(nil));
  ObtainSemaphore(@AROSThreadStruct^.ThreadListSem);
  for i := 0 to High(AROSThreadStruct^.ThreadList) do
  begin
    if Assigned(AROSThreadStruct^.ThreadList[i]) then
    begin
      if AROSThreadStruct^.ThreadList[i]^.Task = Task then
      begin
        Result := AROSThreadStruct^.ThreadList[i]^.ThreadID;
        Break;
      end;  
    end;
  end; 
  ReleaseSemaphore(@AROSThreadStruct^.ThreadListSem);
end;


function AROSWaitThread(ThreadID: LongWord): Boolean;
var
  Thread: PAROSThread;
  Idx, i: Integer;
begin
  if not Assigned(AROSThreadStruct) then
    Exit;
  ObtainSemaphore(@AROSThreadStruct^.ThreadListSem);
  Thread := nil;
  Idx := -1;
  for i := 0 to High(AROSThreadStruct^.ThreadList) do
  begin
    if Assigned(AROSThreadStruct^.ThreadList[i]) then
    begin
      if AROSThreadStruct^.ThreadList[i]^.ThreadID = ThreadID then
      begin
        Thread := AROSThreadStruct^.ThreadList[i];
        Idx := i;
        break;
      end;  
    end;
  end;  
  ReleaseSemaphore(@AROSThreadStruct^.ThreadListSem);
  if Thread = nil then
  begin
    //debugln('Thread not found');
    Result := False;
    Exit;
  end;
  
  // check some 
  ObtainSemaphore(@Thread^.Lock);
  // hmm thats me... I do not wait for myself
  if Thread^.Task = PProcess(FindTask(nil)) then
  begin
    //debugln(' hmm its me :O ' + IntToStr(ThreadID));
    ReleaseSemaphore(@Thread^.Lock);
    Result := False;
    Exit;
  end;
  // wait that the thread start is finished somehow ;)
  ObtainSemaphore(@(Thread^.StartupSemaphore));
  ReleaseSemaphore(@(Thread^.StartupSemaphore));
  // check if Task is still running
  if Thread^.Task <> nil then
  begin
    Inc(Thread^.EndCount);
    ReleaseSemaphore(@Thread^.Lock);
    LockMutex(Thread^.EndMutex);
    //debugln(' Wait condition ' + IntToStr(ThreadID));
    WaitCondition(Thread^.EndCondition, Thread^.EndMutex);
    //debugln(' got condition ' + IntToStr(ThreadID));
    UnlockMutex(Thread^.EndMutex);
    ObtainSemaphore(@Thread^.Lock);  
    Dec(Thread^.EndCount);
  end;
  if Thread^.EndCount > 0 then
  begin
    ReleaseSemaphore(@Thread^.Lock);
    Result := True;
    Exit;  
  end;
  if Assigned(AROSThreadStruct) then
  begin
    // destroy Thread
    ObtainSemaphore(@AROSThreadStruct^.ThreadListSem);
    AROSThreadStruct^.ThreadList[Idx] := nil;
    ReleaseSemaphore(@AROSThreadStruct^.ThreadListSem);
  end;
  DestroyCondition(Thread^.EndCondition);
  DestroyMutex(Thread^.EndMutex);
  Dispose(Thread);
  Result := true;
end;

function AROSCurrentThread: LongWord;
var
  i: Integer;
  CurTask: PProcess;
begin
  if not Assigned(AROSThreadStruct) then
    Exit;
  Result := 0;
  ObtainSemaphore(@AROSThreadStruct^.ThreadListSem);
  CurTask := PProcess(FindTask(nil));
  for i := 0 to High(AROSThreadStruct^.ThreadList) do
  begin
    if Assigned(AROSThreadStruct^.ThreadList[i]) then
    begin
      if AROSThreadStruct^.ThreadList[i]^.Task = CurTask then
      begin
        Result := AROSThreadStruct^.ThreadList[i]^.ThreadID;
        Break;
      end;
    end;
  end;
  ReleaseSemaphore(@AROSThreadStruct^.ThreadListSem);
end;


procedure WaitAllThreads;
var
  i: Integer;
  TID: LongWord;
begin
  if not Assigned(AROSThreadStruct) then
    Exit;
  ObtainSemaphore(@AROSThreadStruct^.ThreadListSem);
  i := 0; 
  while i <= High(AROSThreadStruct^.ThreadList) do
  begin
    if Assigned(AROSThreadStruct^.ThreadList[i]) then
    begin
      TID := AROSThreadStruct^.ThreadList[i]^.ThreadID;      
      //
      ObtainSemaphore(@(AROSThreadStruct^.ThreadList[i]^.StartupSemaphore));
      ReleaseSemaphore(@(AROSThreadStruct^.ThreadList[i]^.StartupSemaphore));
      //
      ReleaseSemaphore(@AROSThreadStruct^.ThreadListSem);
      AROSWaitThread(TID);
      ObtainSemaphore(@AROSThreadStruct^.ThreadListSem); 
    end;  
    Inc(i);
  end;
  ReleaseSemaphore(@AROSThreadStruct^.ThreadListSem);
end;

{$ifdef THREAD_SYSTEM}

procedure InitThreadLib;
begin
  New(AROSThreadStruct);
  AROSThreadStruct^.LastThreadNum := 0;
  InitSemaphore(@(AROSThreadStruct^.MutexListSem));
  InitSemaphore(@(AROSThreadStruct^.ConditionListSem));
  InitSemaphore(@(AROSThreadStruct^.ThreadListSem));
  InitSemaphore(@(AROSThreadStruct^.ThreadMemSem));
  InitSemaphore(@(AROSThreadStruct^.EmptySemaphore));
end;

procedure FinishThreadLib;
var
  i: Integer;
begin
  if not Assigned(AROSThreadStruct) then
    Exit;
  WaitAllThreads;
  ObtainSemaphore(@AROSThreadStruct^.MutexListSem);
  i := 0;
  for i := 0 to High(AROSThreadStruct^.MutexList) do
  begin
    if Assigned(AROSThreadStruct^.MutexList[i]) then
    begin
      Dispose(AROSThreadStruct^.MutexList[i]);
    end;  
  end;
  ReleaseSemaphore(@AROSThreadStruct^.MutexListSem);
  ObtainSemaphore(@AROSThreadStruct^.ConditionListSem);
  i := 0;
  for i := 0 to High(AROSThreadStruct^.ConditionList) do
  begin
    if Assigned(AROSThreadStruct^.ConditionList[i]) then
    begin
      Dispose(AROSThreadStruct^.ConditionList[i]);
    end;  
  end;
  ReleaseSemaphore(@AROSThreadStruct^.ConditionListSem);
  Dispose(AROSThreadStruct);
  AROSThreadStruct := nil;
end;

{$endif THREAD_SYSTEM}

