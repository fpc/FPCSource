{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Valgrind client requests used to annotate synchronization that
    Helgrind and DRD cannot see by themselves (spin locks, lock-free
    hand-offs, object caches).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpvalgrind;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}
{$h+}

// Client requests are only implemented for x86_64 with the System V ABI.
// On other targets all routines are no-ops.
{$IF defined(CPUX86_64) and not defined(WINDOWS)}
  {$DEFINE VG_SUPPORTED}
{$ENDIF}

interface

// The current thread publishes everything it wrote so far, tagged with aObj
// (ANNOTATE_HAPPENS_BEFORE). Call just before releasing a lock.
procedure VgHappensBefore(aObj: Pointer);

// The current thread sees everything published with tag aObj
// (ANNOTATE_HAPPENS_AFTER). Call just after acquiring a lock.
procedure VgHappensAfter(aObj: Pointer);

// Forget all hand-offs tagged with aObj, e.g. before freeing it.
procedure VgForgetAll(aObj: Pointer);

// Tell Helgrind that a memory range is "new": accesses from other threads
// before this point are not compared with later ones. Use for memory that is
// recycled outside the heap manager (object caches).
procedure VgCleanMemory(aStart: Pointer; aLen: PtrUInt);

// Stop checking a memory range for races. Only for intentionally racy
// fields, such as flags that are polled without a lock.
procedure VgBenignRace(aStart: Pointer; aLen: PtrUInt);

// True when running under Valgrind.
function VgRunningOnValgrind: Boolean;

implementation

const
  // From valgrind/helgrind.h and valgrind/drd.h: VG_USERREQ_TOOL_BASE('H','G') + n.
  // DRD accepts the same codes for happens-before/after.
  VG_USERREQ__HG_CLEAN_MEMORY        = $48470000;
  VG_USERREQ__HG_USERSO_SEND_PRE     = $48470121;
  VG_USERREQ__HG_USERSO_RECV_POST    = $48470122;
  VG_USERREQ__HG_USERSO_FORGET_ALL   = $48470123;
  VG_USERREQ__HG_ARANGE_MAKE_UNTRACKED = $48470127;
  // From valgrind/valgrind.h
  VG_USERREQ__RUNNING_ON_VALGRIND    = $1001;

type
  TVgArgs = array[0..5] of PtrUInt;

{$IFDEF VG_SUPPORTED}
{$asmmode att}
// rdi = default result, rsi = pointer to the request block.
// The rotate sequence leaves rdi unchanged; Valgrind recognises it and
// places the request result in rdx.
function DoClientRequest(aDefault: PtrUInt; aArgs: Pointer): PtrUInt; assembler; nostackframe;
asm
  movq %rdi, %rdx
  movq %rsi, %rax
  rolq $3,  %rdi
  rolq $13, %rdi
  rolq $61, %rdi
  rolq $51, %rdi
  xchgq %rbx, %rbx
  movq %rdx, %rax
end;
{$ENDIF VG_SUPPORTED}

function ClientRequest(aDefault, aRequest, aArg1, aArg2: PtrUInt): PtrUInt;
{$IFDEF VG_SUPPORTED}
var
  Args: TVgArgs;
begin
  Args[0]:=aRequest;
  Args[1]:=aArg1;
  Args[2]:=aArg2;
  Args[3]:=0;
  Args[4]:=0;
  Args[5]:=0;
  Result:=DoClientRequest(aDefault,@Args);
end;
{$ELSE}
begin
  Result:=aDefault;
end;
{$ENDIF VG_SUPPORTED}

procedure VgHappensBefore(aObj: Pointer);
begin
  ClientRequest(0,VG_USERREQ__HG_USERSO_SEND_PRE,PtrUInt(aObj),0);
end;

procedure VgHappensAfter(aObj: Pointer);
begin
  ClientRequest(0,VG_USERREQ__HG_USERSO_RECV_POST,PtrUInt(aObj),0);
end;

procedure VgForgetAll(aObj: Pointer);
begin
  ClientRequest(0,VG_USERREQ__HG_USERSO_FORGET_ALL,PtrUInt(aObj),0);
end;

procedure VgCleanMemory(aStart: Pointer; aLen: PtrUInt);
begin
  ClientRequest(0,VG_USERREQ__HG_CLEAN_MEMORY,PtrUInt(aStart),aLen);
end;

procedure VgBenignRace(aStart: Pointer; aLen: PtrUInt);
begin
  ClientRequest(0,VG_USERREQ__HG_ARANGE_MAKE_UNTRACKED,PtrUInt(aStart),aLen);
end;

function VgRunningOnValgrind: Boolean;
begin
  Result:=ClientRequest(0,VG_USERREQ__RUNNING_ON_VALGRIND,0,0)<>0;
end;

end.
