{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{Platform specific information}
type
{$ifdef CPU64}
  THandle = Int64;
{$else CPU64}
  THandle = LongInt;
{$endif CPU64}
  TThreadID = THandle;

  PRTLCriticalSection = ^TRTLCriticalSection;
{$IFDEF AROS}
  TRTLCriticalSection = Pointer;
{$ELSE}
  TRTLCriticalSection = record
    { This must actually be bigger or equal to sizeof(TSignalSemaphore)
      which seems to be 46 bytes on MorphOS and Amiga/m68k. }
    semaphore: array[0..63] of byte;
  end;
{$ENDIF}

{ BeginThread flags we support in AThreads }
const
  CREATE_SUSPENDED = 1;
  STACK_SIZE_PARAM_IS_A_RESERVATION = 2;

{ Thread Init/Exit Procedure support }
Procedure AddThreadInitProc(Proc: TProcedure);
Procedure AddThreadExitProc(Proc: TProcedure);
Procedure DoThreadInitProcChain;
Procedure DoThreadExitProcChain;
