{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Free Pascal development team

    Support for 64-bit Windows exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ exception flags }
const
  EXCEPTION_NONCONTINUABLE  = $01;
  EXCEPTION_UNWINDING       = $02;
  EXCEPTION_EXIT_UNWIND     = $04;
  EXCEPTION_STACK_INVALID   = $08;
  EXCEPTION_NESTED_CALL     = $10;
  EXCEPTION_TARGET_UNWIND   = $20;
  EXCEPTION_COLLIDED_UNWIND = $40;
  EXCEPTION_UNWIND          = $66;

  UNWIND_HISTORY_TABLE_SIZE = 12;

  UNW_FLAG_NHANDLER         = 0;

type
  PM128A=^M128A;
  M128A = record
    Low : QWord;
    High : Int64;
  end;

  PContext = ^TContext;
  TContext = record
    P1Home : QWord;
    P2Home : QWord;
    P3Home : QWord;
    P4Home : QWord;
    P5Home : QWord;
    P6Home : QWord;
    ContextFlags : DWord;
    MxCsr : DWord;
    SegCs : word;
    SegDs : word;
    SegEs : word;
    SegFs : word;
    SegGs : word;
    SegSs : word;
    EFlags : DWord;
    Dr0 : QWord;
    Dr1 : QWord;
    Dr2 : QWord;
    Dr3 : QWord;
    Dr6 : QWord;
    Dr7 : QWord;
    Rax : QWord;
    Rcx : QWord;
    Rdx : QWord;
    Rbx : QWord;
    Rsp : QWord;
    Rbp : QWord;
    Rsi : QWord;
    Rdi : QWord;
    R8 : QWord;
    R9 : QWord;
    R10 : QWord;
    R11 : QWord;
    R12 : QWord;
    R13 : QWord;
    R14 : QWord;
    R15 : QWord;
    Rip : QWord;
    Header : array[0..1] of M128A;
    Legacy : array[0..7] of M128A;
    Xmm0 : M128A;
    Xmm1 : M128A;
    Xmm2 : M128A;
    Xmm3 : M128A;
    Xmm4 : M128A;
    Xmm5 : M128A;
    Xmm6 : M128A;
    Xmm7 : M128A;
    Xmm8 : M128A;
    Xmm9 : M128A;
    Xmm10 : M128A;
    Xmm11 : M128A;
    Xmm12 : M128A;
    Xmm13 : M128A;
    Xmm14 : M128A;
    Xmm15 : M128A;
    VectorRegister : array[0..25] of M128A;
    VectorControl : QWord;
    DebugControl : QWord;
    LastBranchToRip : QWord;
    LastBranchFromRip : QWord;
    LastExceptionToRip : QWord;
    LastExceptionFromRip : QWord;
  end;

  { This is a simplified definition, only array part of unions }
  PKNONVOLATILE_CONTEXT_POINTERS=^KNONVOLATILE_CONTEXT_POINTERS;
  KNONVOLATILE_CONTEXT_POINTERS=record
    FloatingContext: array[0..15] of PM128A;
    IntegerContext: array[0..15] of PQWord;
  end;

  EXCEPTION_DISPOSITION=(
    ExceptionContinueExecution,
    ExceptionContinueSearch,
    ExceptionNestedException,
    ExceptionCollidedUnwind
  );

  PExceptionPointers = ^TExceptionPointers;
  TExceptionPointers = record
    ExceptionRecord   : PExceptionRecord;
    ContextRecord     : PContext;
  end;


  EXCEPTION_ROUTINE = function(
    var ExceptionRecord: TExceptionRecord;
    EstablisherFrame: Pointer;
    var ContextRecord: TContext;
    DispatcherContext: Pointer ): EXCEPTION_DISPOSITION;

  PRUNTIME_FUNCTION=^RUNTIME_FUNCTION;
  RUNTIME_FUNCTION=record
    BeginAddress: DWORD;
    EndAddress: DWORD;
    UnwindData: DWORD;
  end;

  UNWIND_HISTORY_TABLE_ENTRY=record
    ImageBase: QWord;
    FunctionEntry: PRUNTIME_FUNCTION;
  end;

  PUNWIND_HISTORY_TABLE=^UNWIND_HISTORY_TABLE;
  UNWIND_HISTORY_TABLE=record
    Count: DWORD;
    Search: Byte;
    RaiseStatusIndex: Byte;
    Unwind: Byte;
    Exception: Byte;
    LowAddress: QWord;
    HighAddress: QWord;
    Entry: array[0..UNWIND_HISTORY_TABLE_SIZE-1] of UNWIND_HISTORY_TABLE_ENTRY;
  end;

  PDispatcherContext = ^TDispatcherContext;
  TDispatcherContext = record
    ControlPc: QWord;
    ImageBase: QWord;
    FunctionEntry: PRUNTIME_FUNCTION;
    EstablisherFrame: QWord;
    TargetIp: QWord;
    ContextRecord: PContext;
    LanguageHandler: EXCEPTION_ROUTINE;
    HandlerData: Pointer;
    HistoryTable: PUNWIND_HISTORY_TABLE;
    ScopeIndex: DWord;
    Fill0: DWord;
  end;

procedure RtlCaptureContext(var ctx: TContext); stdcall;
  external 'kernel32.dll' name 'RtlCaptureContext';

function RtlCaptureStackBackTrace(
  FramesToSkip: DWORD;
  FramesToCapture: DWORD;
  var BackTrace: Pointer;
  BackTraceHash: PDWORD): Word; stdcall;
  external 'kernel32.dll' name 'RtlCaptureStackBackTrace';

function RtlLookupFunctionEntry(
  ControlPC: QWord;
  out ImageBase: QWord;
  HistoryTable: PUNWIND_HISTORY_TABLE): PRUNTIME_FUNCTION;
  external 'kernel32.dll' name 'RtlLookupFunctionEntry';

function RtlVirtualUnwind(
  HandlerType: DWORD;
  ImageBase: QWord;
  ControlPc: QWord;
  FunctionEntry: PRUNTIME_FUNCTION;
  var ContextRecord: TContext;
  HandlerData: PPointer;
  EstablisherFrame: PQWord;
  ContextPointers: PKNONVOLATILE_CONTEXT_POINTERS): EXCEPTION_ROUTINE;
  external 'kernel32.dll' name 'RtlVirtualUnwind';

procedure RtlUnwindEx(
  TargetFrame: Pointer;
  TargetIp: Pointer;
  ExceptionRecord: PExceptionRecord;
  ReturnValue: Pointer;
  OriginalContext: PContext;  { scratch space, initial contents ignored }
  HistoryTable: PUNWIND_HISTORY_TABLE);
  external 'kernel32.dll' name 'RtlUnwindEx';

procedure RaiseException(
  dwExceptionCode: DWORD;
  dwExceptionFlags: DWORD;
  dwArgCount: DWORD;
  lpArguments: Pointer);  // msdn: *ULONG_PTR
  external 'kernel32.dll' name 'RaiseException';

