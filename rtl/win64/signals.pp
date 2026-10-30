{
    This file is part of the Free Pascal run time library.
    This unit implements unix like signal handling for win32
    Copyright (c) 1999-2006 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit signals;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$PACKRECORDS C}

  { Signals }
  const
    SIGABRT   = 288;
    SIGFPE    = 289;
    SIGILL    = 290;
    SIGSEGV   = 291;
    SIGTERM   = 292;
    SIGALRM   = 293;
    SIGHUP    = 294;
    SIGINT    = 295;
    SIGKILL   = 296;
    SIGPIPE   = 297;
    SIGQUIT   = 298;
    SIGUSR1   = 299;
    SIGUSR2   = 300;
    SIGNOFP   = 301;
    SIGTRAP   = 302;
    SIGTIMR   = 303;    { Internal for setitimer (SIGALRM, SIGPROF) }
    SIGPROF   = 304;
    SIGMAX    = 320;

    SIG_BLOCK   = 1;
    SIG_SETMASK = 2;
    SIG_UNBLOCK = 3;

  function SIG_DFL( x: longint) : longint; cdecl;

  function SIG_ERR( x: longint) : longint; cdecl;

  function SIG_IGN( x: longint) : longint; cdecl;

  type

    SignalHandler  = function (v : longint) : longint;cdecl;

    PSignalHandler = ^SignalHandler; { to be compatible with linux.pp }

  function signal(sig : longint;func : SignalHandler) : SignalHandler;

  const

     EXCEPTION_MAXIMUM_PARAMETERS = 15;


{$define __HASCONTEXT__}
//
// Define 128-bit 16-byte aligned xmm register type.
//

//typedef struct DECLSPEC_ALIGN(16) _M128A {
{$note todo, fix alignment }
  type
     DWORD64 = QWORD;
     ULONGLONG = QWORD;
     LONGLONG = int64;

     M128A = record
          Low: ULONGLONG;
          High: LONGLONG;
       end;
     _M128A = M128A;
     TM128A = M128A;
     PM128A = TM128A;

//
// Format of data for 32-bit fxsave/fxrstor instructions.
//

//typedef struct _XMM_SAVE_AREA32 {
  type
     XMM_SAVE_AREA32 = record
          ControlWord: WORD;
          StatusWord: WORD;
          TagWord: BYTE;
          Reserved1: BYTE;
          ErrorOpcode: WORD;
          ErrorOffset: DWORD;
          ErrorSelector: WORD;
          Reserved2: WORD;
          DataOffset: DWORD;
          DataSelector: WORD;
          Reserved3: WORD;
          MxCsr: DWORD;
          MxCsr_Mask: DWORD;
          FloatRegisters: array[0..7] of M128A;
          XmmRegisters: array[0..15] of M128A;
          Reserved4: array[0..95] of BYTE;
       end;
     _XMM_SAVE_AREA32 = XMM_SAVE_AREA32;
     TXmmSaveArea = XMM_SAVE_AREA32;
     PXmmSaveArea = ^TXmmSaveArea;

  const
     LEGACY_SAVE_AREA_LENGTH = sizeof(XMM_SAVE_AREA32);

//
// Context Frame
//
//  This frame has a several purposes: 1) it is used as an argument to
//  NtContinue, 2) is is used to construct a call frame for APC delivery,
//  and 3) it is used in the user level thread creation routines.
//
//
// The flags field within this record controls the contents of a CONTEXT
// record.
//
// If the context record is used as an input parameter, then for each
// portion of the context record controlled by a flag whose value is
// set, it is assumed that that portion of the context record contains
// valid context. If the context record is being used to modify a threads
// context, then only that portion of the threads context is modified.
//
// If the context record is used as an output parameter to capture the
// context of a thread, then only those portions of the thread's context
// corresponding to set flags will be returned.
//
// CONTEXT_CONTROL specifies SegSs, Rsp, SegCs, Rip, and EFlags.
//
// CONTEXT_INTEGER specifies Rax, Rcx, Rdx, Rbx, Rbp, Rsi, Rdi, and R8-R15.
//
// CONTEXT_SEGMENTS specifies SegDs, SegEs, SegFs, and SegGs.
//
// CONTEXT_DEBUG_REGISTERS specifies Dr0-Dr3 and Dr6-Dr7.
//
// CONTEXT_MMX_REGISTERS specifies the floating point and extended registers
//     Mm0/St0-Mm7/St7 and Xmm0-Xmm15).
//

//typedef struct DECLSPEC_ALIGN(16) _CONTEXT {
{$packrecords C}
  type

     CONTEXT = record

          //
          // Register parameter home addresses.
          //
          // N.B. These fields are for convenience - they could be used to extend the
          //      context record in the future.
          //

          P1Home: DWORD64;
          P2Home: DWORD64;
          P3Home: DWORD64;
          P4Home: DWORD64;
          P5Home: DWORD64;
          P6Home: DWORD64;

          //
          // Control flags.
          //

          ContextFlags: DWORD;
          MxCsr: DWORD;

          //
          // Segment Registers and processor flags.
          //

          SegCs: WORD;
          SegDs: WORD;
          SegEs: WORD;
          SegFs: WORD;
          SegGs: WORD;
          SegSs: WORD;
          EFlags: DWORD;

          //
          // Debug registers
          //

          Dr0: DWORD64;
          Dr1: DWORD64;
          Dr2: DWORD64;
          Dr3: DWORD64;
          Dr6: DWORD64;
          Dr7: DWORD64;

          //
          // Integer registers.
          //

          Rax: DWORD64;
          Rcx: DWORD64;
          Rdx: DWORD64;
          Rbx: DWORD64;
          Rsp: DWORD64;
          Rbp: DWORD64;
          Rsi: DWORD64;
          Rdi: DWORD64;
          R8: DWORD64;
          R9: DWORD64;
          R10: DWORD64;
          R11: DWORD64;
          R12: DWORD64;
          R13: DWORD64;
          R14: DWORD64;
          R15: DWORD64;

          //
          // Program counter.
          //

          Rip: DWORD64;

          //
          // Floating point state.
          //

          FltSave: XMM_SAVE_AREA32; // MWE: only translated the FltSave part of the union
(*
          union  {
              XMM_SAVE_AREA32 FltSave;
              struct {
                  M128A Header[2];
                  M128A Legacy[8];
                  M128A Xmm0;
                  M128A Xmm1;
                  M128A Xmm2;
                  M128A Xmm3;
                  M128A Xmm4;
                  M128A Xmm5;
                  M128A Xmm6;
                  M128A Xmm7;
                  M128A Xmm8;
                  M128A Xmm9;
                  M128A Xmm10;
                  M128A Xmm11;
                  M128A Xmm12;
                  M128A Xmm13;
                  M128A Xmm14;
                  M128A Xmm15;
              };
          };
*)

          //
          // Vector registers.
          //

          VectorRegister: array[0..25] of M128A;
          VectorControl: DWORD64;

          //
          // Special debug control registers.
          //

          DebugControl: DWORD64;
          LastBranchToRip: DWORD64;
          LastBranchFromRip: DWORD64;
          LastExceptionToRip: DWORD64;
          LastExceptionFromRip: DWORD64;
      end;


(*
  type

     FLOATING_SAVE_AREA = record
          ControlWord : DWORD;
          StatusWord : DWORD;
          TagWord : DWORD;
          ErrorOffset : DWORD;
          ErrorSelector : DWORD;
          DataOffset : DWORD;
          DataSelector : DWORD;
          RegisterArea : array[0..79] of BYTE;
          Cr0NpxState : DWORD;
       end;
     _FLOATING_SAVE_AREA = FLOATING_SAVE_AREA;
     TFLOATINGSAVEAREA = FLOATING_SAVE_AREA;
     PFLOATINGSAVEAREA = ^FLOATING_SAVE_AREA;

     CONTEXT = record
          ContextFlags : DWORD;
          Dr0 : DWORD;
          Dr1 : DWORD;
          Dr2 : DWORD;
          Dr3 : DWORD;
          Dr6 : DWORD;
          Dr7 : DWORD;
          FloatSave : FLOATING_SAVE_AREA;
          SegGs : DWORD;
          SegFs : DWORD;
          SegEs : DWORD;
          SegDs : DWORD;
          Edi : DWORD;
          Esi : DWORD;
          Ebx : DWORD;
          Edx : DWORD;
          Ecx : DWORD;
          Eax : DWORD;
          Ebp : DWORD;
          Eip : DWORD;
          SegCs : DWORD;
          EFlags : DWORD;
          Esp : DWORD;
          SegSs : DWORD;
       end;

*)
     LPCONTEXT = ^CONTEXT;
     _CONTEXT = CONTEXT;
     TCONTEXT = CONTEXT;
     PCONTEXT = ^CONTEXT;


  type
     pexception_record = ^exception_record;
     EXCEPTION_RECORD  = record
       ExceptionCode   : cardinal;
       ExceptionFlags  : longint;
       ExceptionRecord : pexception_record;
       ExceptionAddress : pointer;
       NumberParameters : longint;
       ExceptionInformation : array[0..EXCEPTION_MAXIMUM_PARAMETERS-1] of pointer;
     end;

     PEXCEPTION_POINTERS = ^EXCEPTION_POINTERS;
     EXCEPTION_POINTERS = record
       ExceptionRecord   : PEXCEPTION_RECORD ;
       ContextRecord     : PCONTEXT ;
     end;


implementation

{$i signals.inc}

end.
