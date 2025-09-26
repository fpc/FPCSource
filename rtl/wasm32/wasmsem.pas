{$IFNDEF FPC_DOTTEDUNITS}
unit WasmSem;
{$ENDIF}

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  Wasm.Api;
{$ELSE}  
  WebAssembly;
{$ENDIF}

type
  PWasmSemaphore = ^TWasmSemaphore;
  TWasmSemaphore = packed record
    counter: longint;     // Current counter value
    max_count: longint;   // Maximum count allowed
  end;

// Initialize a semaphore with initial count and maximum count
procedure semaphore_init(var sem: TWasmSemaphore; initial_count, max_count: longint);

// Wait (acquire) operation with timeout - decrements counter, blocks if counter would go negative
// timeout_ms: timeout in milliseconds (-1 for infinite timeout)
// Returns true if acquired successfully, false if timeout occurred
function semaphore_wait(var sem: TWasmSemaphore; timeout_ms: int64): boolean;

// Wait (acquire) operation without timeout - blocks indefinitely until semaphore is available
// Returns true when acquired successfully
function semaphore_wait_infinite(var sem: TWasmSemaphore): boolean;

// Signal (release) operation - increments counter up to max and notifies waiters
// Returns true if signaled successfully, false if at max count
function semaphore_signal(var sem: TWasmSemaphore): boolean;

// Get current semaphore count (read-only)
function semaphore_count(var sem: TWasmSemaphore): longint;

// Get maximum semaphore count (read-only)
function semaphore_max_count(var sem: TWasmSemaphore): longint;

implementation

procedure semaphore_init(var sem: TWasmSemaphore; initial_count, max_count: longint);
begin
  AtomicStore(sem.counter, initial_count);
  AtomicStore(sem.max_count, max_count);
end;

function semaphore_wait(var sem: TWasmSemaphore; timeout_ms: int64): boolean;
var
  current_count: longint;
  new_count: longint;
  expected: longint;
  wait_result: longint;
  timeout_ns: int64;
begin
  if timeout_ms = -1 then
    timeout_ns := awtInfiniteTimeout
  else
    timeout_ns := timeout_ms * 1000000;

  repeat
    current_count := AtomicLoad(sem.counter);

    if current_count > 0 then
      begin
      new_count := current_count - 1;
      expected := current_count;

      if AtomicCompareExchange(sem.counter, expected, new_count) = expected then
        exit(true);
      // Failed CAS, retry immediately
      end
    else
      begin
      wait_result := AtomicWait(sem.counter, current_count, timeout_ns);
      if wait_result = awrTimedOut then
        exit(false);
      // Either woke up (awrOk) or not-equal (awrNotEqual), retry the acquisition
      end;
  until false;
  // Should never reach here
  result := false;
end;

function semaphore_wait_infinite(var sem: TWasmSemaphore): boolean;
begin
  result := semaphore_wait(sem, -1);
end;

function semaphore_signal(var sem: TWasmSemaphore): boolean;
var
  current_count: longint;
  max_count: longint;
  new_count: longint;
  expected: longint;
  woken_count: longword;
begin
  max_count := AtomicLoad(sem.max_count);

  repeat
    current_count := AtomicLoad(sem.counter);
    if current_count >= max_count then
      exit(false);

    new_count := current_count + 1;
    expected := current_count;

    // Try atomic compare-and-swap
    if AtomicCompareExchange(sem.counter, expected, new_count) = expected then
      begin
      woken_count := AtomicNotify(sem.counter, 1);
      exit(true);
      end;
    // Failed CAS, retry
  until false;
  // Should never reach here
  result := false;
end;

function semaphore_count(var sem: TWasmSemaphore): longint;
begin
  result := AtomicLoad(sem.counter);
end;

function semaphore_max_count(var sem: TWasmSemaphore): longint;
begin
  result := AtomicLoad(sem.max_count);
end;

end.