{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2017-2018 by Jonas Maebe,
    member of the Free Pascal development team

    This file contains support for Itanium psABI EH

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$packrecords c}

const
  FPC_URC_NO_REASON = FPC_Unwind_Reason_Code(0);
  FPC_URC_OK = FPC_URC_NO_REASON;
  FPC_URC_FOREIGN_EXCEPTION_CAUGHT = FPC_Unwind_Reason_Code(1);
{$ifndef __ARM_EABI_UNWINDER__}
  FPC_URC_FATAL_PHASE2_ERROR = FPC_Unwind_Reason_Code(2);
  FPC_URC_FATAL_PHASE1_ERROR = FPC_Unwind_Reason_Code(3);
  FPC_URC_NORMAL_STOP = FPC_Unwind_Reason_Code(4);
  FPC_URC_END_OF_STACK = FPC_Unwind_Reason_Code(5);
{$endif not __ARM_EABI_UNWINDER__}
  FPC_URC_HANDLER_FOUND = FPC_Unwind_Reason_Code(6);
  FPC_URC_INSTALL_CONTEXT = FPC_Unwind_Reason_Code(7);
  FPC_URC_CONTINUE_UNWIND = FPC_Unwind_Reason_Code(8);
{$ifdef __ARM_EABI_UNWINDER__}
  FPC_URC_FAILURE = FPC_Unwind_Reason_Code(9);
{$endif __ARM_EABI_UNWINDER__}

{$ifdef __ARM_EABI_UNWINDER__}
const
  FPC_US_VIRTUAL_UNWIND_FRAME  = FPC_Unwind_State(0);
  FPC_US_UNWIND_FRAME_STARTING = FPC_Unwind_State(1);
  FPC_US_UNWIND_FRAME_RESUME   = FPC_Unwind_State(2);
  FPC_US_FORCE_UNWIND          = FPC_Unwind_State(8);

  FPC_US_ACTION_MASK           = FPC_Unwind_State(3);
{$endif}

const
  FPC_UA_SEARCH_PHASE = FPC_Unwind_Action(1);
  FPC_UA_CLEANUP_PHASE = FPC_Unwind_Action(2);
  FPC_UA_HANDLER_FRAME = FPC_Unwind_Action(4);
  FPC_UA_FORCE_UNWIND = FPC_Unwind_Action(8);
  FPC_UA_END_OF_STACK = FPC_Unwind_Action(16);

type
  PFPC_Unwind_Context = ^FPC_Unwind_Context;
  FPC_Unwind_Context = record
  end;

  function _Unwind_RaiseException(exception_object: PFPC_Unwind_Exception): FPC_Unwind_Reason_Code; cdecl; external;
  procedure _Unwind_Resume(exception_object: PFPC_Unwind_Exception); compilerproc; cdecl; external;

type
  TFPC_psabieh_exceptionClass = record
    case byte of
      0: (u: qword); {cuint64}
      1: (a: array[0..7] of AnsiChar);
  end;

{$push}
{$j-}
const
  FPC_psabieh_exceptionClass_ID: TFPC_psabieh_exceptionClass =
    (a: 'FPC1PAS'#0);
{$pop}

{$ifdef __ARM_EABI_UNWINDER__}
function _FPC_psabieh_personality_v0(state: FPC_Unwind_State; libunwind_exception: PFPC_Unwind_Exception; context: PFPC_Unwind_Context): FPC_Unwind_Reason_Code; cdecl; public;
{$else}
function _FPC_psabieh_personality_v0(version: longint; actions: FPC_Unwind_Action; exceptionClass: qword; libunwind_exception: PFPC_Unwind_Exception; context: PFPC_Unwind_Context): FPC_Unwind_Reason_Code; cdecl; public;
{$endif}
function FPC_psabi_begin_catch(exc:PFPC_Unwind_Exception): pointer; compilerproc;
procedure FPC_psabi_end_catch; cdecl; compilerproc;

{ llvm cannot create a catch/cleanup block if there is no call inside the
  try block to a routine that can raise an exception. Hence, we will call
  a dummy routine that llvm cannot analyse for such try blocks }
procedure FPC_DummyPotentialRaise;

{$packrecords default}
