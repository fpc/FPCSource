{ %target=win64 }
program tw34772;

{$mode objfpc}{$H+}
{$WARN 5058 off : Variable "$1" does not seem to be initialized}

uses
  Classes, SysUtils;


procedure Test1(a: array of Integer);
begin
  WriteLn('Test1 - Start ', a[0]);
  if a[0] = 1 then exit;
  if a[0] = 2 then raise EAbort.Create('Test');
  WriteLn('Test1 - End ', a[0]);
end;


procedure Test2(a: array of Integer);
var
  Test: Pointer;
begin
  GetMem(Test, 4);
  try
    WriteLn('Test2 - Start ', a[0]);
    if a[0] = 1 then exit;
    if a[0] = 2 then raise EAbort.Create('Test');
    WriteLn('Test2 - End ', a[0]);
  finally
    FreeMem(Test);
    WriteLn('Test2 - Finally ', a[0]);
  end;
end;


procedure Test3(a: array of Integer);
begin
  try
    WriteLn('Test3 - Start ', a[0]);
    if a[0] = 1 then exit;
    if a[0] = 2 then raise EAbort.Create('Test');
    WriteLn('Test3 - End ', a[0]);
  except
    on E: Exception do
      begin
        if E.ClassType <> EAbort then raise; { Unexpected exception }
        WriteLn('Test3 - Except ', a[0]);
      end;
  end;
end;


procedure Test4(a: array of Integer);
var
  Test: Pointer;
begin
  GetMem(Test, 4);
  try
    try
      WriteLn('Test4 - Start ', a[0]);
      if a[0] = 1 then exit;
      if a[0] = 2 then raise EAbort.Create('Test');
      WriteLn('Test4 - End ', a[0]);
    except
      on E: Exception do
        begin
          if E.ClassType <> EAbort then raise; { Unexpected exception }
          WriteLn('Test4 - Except ', a[0]);
        end;
    end;
  finally
    FreeMem(Test);
    WriteLn('Test4 - Finally ', a[0]);
  end;
end;


procedure Test5(a: array of Integer); safecall;
begin
  WriteLn('Test5 - Start ', a[0]);
  if a[0] = 1 then exit;
  if a[0] = 2 then raise EAbort.Create('Test');
  WriteLn('Test5 - End ', a[0]);
end;


procedure Test6(a: array of Integer); safecall;
var
  Test: Pointer;
begin
  GetMem(Test, 4);
  try
    WriteLn('Test6 - Start ', a[0]);
    if a[0] = 1 then exit;
    if a[0] = 2 then raise EAbort.Create('Test');
    WriteLn('Test6 - End ', a[0]);
  finally
    FreeMem(Test);
    WriteLn('Test6 - Finally ', a[0]);
  end;
end;


procedure Test7(a: array of Integer); safecall;
begin
  try
    WriteLn('Test7 - Start ', a[0]);
    if a[0] = 1 then exit;
    if a[0] = 2 then raise EAbort.Create('Test');
    WriteLn('Test7 - End ', a[0]);
  except
    on E: Exception do
      begin
        if E.ClassType <> EAbort then raise; { Unexpected exception }
        WriteLn('Test7 - Except ', a[0]);
      end;
  end;
end;


procedure Test8(a: array of Integer); safecall;
var
  Test: Pointer;
begin
  GetMem(Test, 4);
  try
    try
      WriteLn('Test8 - Start ', a[0]);
      if a[0] = 1 then exit;
      if a[0] = 2 then raise EAbort.Create('Test');
      WriteLn('Test8 - End ', a[0]);
    except
      on E: Exception do
        begin
          if E.ClassType <> EAbort then raise; { Unexpected exception }
          WriteLn('Test8 - Except ', a[0]);
        end;
    end;
  finally
    FreeMem(Test);
    WriteLn('Test8 - Finally ', a[0]);
  end;
end;


function Test9(a: array of Integer): Boolean;
var
  Test: Pointer;
begin
  Result := True;
  GetMem(Test, 4);
  try
    WriteLn('Test9 - Start ', a[0]);
    if a[0] = 1 then exit;
    if a[0] = 2 then raise EAbort.Create('Test');
    WriteLn('Test9 - End ', a[0]);
  finally
    FreeMem(Test);
    WriteLn('Test9 - Finally ', a[0]);
    if a[0] = 0 then Result := False;
  end;

  Result := True;

end;


procedure Test10(a: array of Integer);
var
  Test, Test2: Pointer;
begin
  GetMem(Test, 4);
  try
    GetMem(Test2, 4);
    try
      WriteLn('Test10 - Start ', a[0]);
      if a[0] = 1 then exit;
      if a[0] = 2 then raise EAbort.Create('Test');
      WriteLn('Test10 - End ', a[0]);
    finally
      FreeMem(Test2);
      WriteLn('Test10 - Finally A ', a[0]);
    end;
  finally
    FreeMem(Test);
    WriteLn('Test10 - Finally B ', a[0]);
  end;
end;


procedure Test11(a: array of Integer); safecall;
var
  Test, Test2: Pointer;
begin
  GetMem(Test, 4);
  try
    GetMem(Test2, 4);
    try
      WriteLn('Test11 - Start ', a[0]);
      if a[0] = 1 then exit;
      if a[0] = 2 then raise EAbort.Create('Test');
      WriteLn('Test11 - End ', a[0]);
    finally
      FreeMem(Test2);
      WriteLn('Test11 - Finally A ', a[0]);
    end;
  finally
    FreeMem(Test);
    WriteLn('Test11 - Finally B ', a[0]);
  end;
end;


procedure Test12(a: Integer); safecall;
var
  Test, Test2: Pointer;
begin
  GetMem(Test, 4);
  try
    GetMem(Test2, 4);
    try
      WriteLn('Test12 - Start ', a);
      if a = 1 then exit;
      if a = 2 then raise EAbort.Create('Test');
      WriteLn('Test12 - End ', a);
    finally
      FreeMem(Test2);
      WriteLn('Test12 - Finally A ', a);
    end;
  finally
    FreeMem(Test);
    WriteLn('Test12 - Finally B ', a);
  end;
end;


var
  X, TestCount: Integer;
  ReferenceCount: LongInt;
  MemMgr, NewMemMgr: TMemoryManager;
  Fail: Boolean;

function HookGetMem(Size: PtrUInt): Pointer;
  begin
    Inc(ReferenceCount);
    Result := MemMgr.GetMem(Size);
  end;

function HookReAllocMem(var p: Pointer; Size: PtrUInt): Pointer;
  begin
    if p = nil then
      Inc(ReferenceCount);

    Result := MemMgr.ReAllocMem(p, Size);

    { If ReAllocMem(nil, 0) is called, ReferenceCount is incremented then
      decremented, reflecting the null operation }
    if Size = 0 then
      Dec(ReferenceCount);
  end;

function HookFreeMem(ptr: Pointer): PtrUInt;
  begin
    Dec(ReferenceCount);
    Result := MemMgr.FreeMem(ptr);
  end;

function HookFreeMemSize(ptr: Pointer; Size: PtrUInt): PtrUInt;
  begin
    Dec(ReferenceCount);
    Result := MemMgr.FreeMemSize(ptr, Size);
  end;

procedure PostTestAnalysis;
  begin
    Inc(TestCount);
    if ReferenceCount <> 0 then
      begin
        WriteLn('FAIL - Reference count = ', ReferenceCount);
        Fail := True;
      end;
  end;

procedure CheckTestCount;
  begin
    if TestCount <> 3 then
      begin
        Fail := True;
        WriteLn('FAIL - Only ', TestCount, ' sub-tests were run for this test');
      end;
  end;

begin
  { Set up hooks to track memory leaks }
  GetMemoryManager(MemMgr);
  NewMemMgr := MemMgr;
  NewMemMgr.GetMem := @HookGetMem;
  NewMemMgr.ReAllocMem := @HookReAllocMem;
  NewMemMgr.FreeMem := @HookFreeMem;
  NewMemMgr.FreeMemSize := @HookFreeMemSize;
  SetMemoryManager(NewMemMgr);

  { Test parameters
    [0] = Run to end of procedure
    [1] = Exit prematurely
    [2] = raise exception
  }

  { Test1 - implicit try..finally }
  TestCount := 0;
  for X := 0 to 2 do
    begin
      ReferenceCount := 0;
      try
        Test1([X]);
      except
        on E: Exception do
          if E.ClassType <> EAbort then
            begin
              { Unexpected exception }
              WriteLn('FAIL - Exception ', E.ClassName, ' raised: "', E.Message, '"');
              Fail := True;
              Continue;
            end;
      end;

      PostTestAnalysis;
    end;

  CheckTestCount;

  { Test2 - implicit + explicit try..finally }
  TestCount := 0;
  for X := 0 to 2 do
    begin
      ReferenceCount := 0;
      try
        Test2([X]);
      except
        on E: Exception do
          if E.ClassType <> EAbort then
            begin
              { Unexpected exception }
              WriteLn('FAIL - Exception ', E.ClassName, ' raised: "', E.Message, '"');
              Fail := True;
              Continue;
            end;
      end;

      PostTestAnalysis;
    end;

  CheckTestCount;

  { Test3 - implicit try..finally and explicit try..except }
  TestCount := 0;
  for X := 0 to 2 do
    begin
      ReferenceCount := 0;
      try
        Test3([X]);
      except
        { Exceptions should be caught }
        on E: Exception do
          begin
            WriteLn('FAIL - Exception ', E.ClassName, ' raised: "', E.Message, '"');
            Fail := True;
            Continue;
          end;
      end;

      PostTestAnalysis;
    end;

  CheckTestCount;

  { Test4 - implicit + explicit try..finally and explicit try..except }
  TestCount := 0;
  for X := 0 to 2 do
    begin
      ReferenceCount := 0;
      try
        Test4([X]);
      except
        { Exceptions should be caught }
        on E: Exception do
          begin
            WriteLn('FAIL - Exception ', E.ClassName, ' raised: "', E.Message, '"');
            Fail := True;
            Continue;
          end;
      end;

      PostTestAnalysis;
    end;

  CheckTestCount;

  { Test5 - implicit try..finally with safecall }
  TestCount := 0;
  for X := 0 to 2 do
    begin
      ReferenceCount := 0;
      try
        Test5([X]);
      except
        { Everything gets wrapped into a ESafecallException }
      end;

      PostTestAnalysis;
    end;

  CheckTestCount;

  { Test6 - implicit + explicit try..finally with safecall }
  TestCount := 0;
  for X := 0 to 2 do
    begin
      ReferenceCount := 0;
      try
        Test6([X]);
      except
        { Everything gets wrapped into a ESafecallException }
      end;

      PostTestAnalysis;
    end;

  CheckTestCount;

  { Test7 - implicit try..finally and explicit try..except with safecall }
  TestCount := 0;
  for X := 0 to 2 do
    begin
      ReferenceCount := 0;
      try
        Test7([X]);
      except
        { Everything gets wrapped into a ESafecallException }
      end;

      PostTestAnalysis;
    end;

  CheckTestCount;

  { Test8 - implicit + explicit try..finally and explicit try..except with safecall }
  TestCount := 0;
  for X := 0 to 2 do
    begin
      ReferenceCount := 0;
      try
        Test8([X]);
      except
        { Everything gets wrapped into a ESafecallException }
      end;

      PostTestAnalysis;
    end;

  CheckTestCount;

  { Test9 - implicit + explicit try..finally with code following }
  TestCount := 0;
  for X := 0 to 2 do
    begin
      ReferenceCount := 0;
      try
        if not Test9([X]) then
          begin
            WriteLn('FAIL -  Code following finally block wasn''t executed');
            Fail := True;
            Continue;
          end;
      except
        on E: Exception do
          if E.ClassType <> EAbort then
            begin
              { Unexpected exception }
              WriteLn('FAIL - Exception ', E.ClassName, ' raised: "', E.Message, '"');
              Fail := True;
              Continue;
            end;
      end;

      PostTestAnalysis;
    end;

  CheckTestCount;

  { Test10 - implicit + 2 * explicit try..finally }
  TestCount := 0;
  for X := 0 to 2 do
    begin
      ReferenceCount := 0;
      try
        Test10([X]);
      except
        on E: Exception do
          if E.ClassType <> EAbort then
            begin
              { Unexpected exception }
              WriteLn('FAIL - Exception ', E.ClassName, ' raised: "', E.Message, '"');
              Fail := True;
              Continue;
            end;
      end;

      PostTestAnalysis;
    end;

  CheckTestCount;

  { Test11 - implicit + 2 * explicit try..finally with safecall }
  TestCount := 0;
  for X := 0 to 2 do
    begin
      ReferenceCount := 0;
      try
        Test11([X]);
      except
        { Everything gets wrapped into a ESafecallException }
      end;

      PostTestAnalysis;
    end;

  CheckTestCount;

  { Test12 - 2 * explicit try..finally with safecall }
  TestCount := 0;
  for X := 0 to 2 do
    begin
      ReferenceCount := 0;
      try
        Test12(X);
      except
        { Everything gets wrapped into a ESafecallException }
      end;

      PostTestAnalysis;
    end;

  CheckTestCount;

  if Fail then
    Halt(1)
  else
    WriteLn('ok');
end.
