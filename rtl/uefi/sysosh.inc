{
    Basic UEFI definitions

    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Olivier Coursière

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
 
{ Platform specific information }
type
  THandle = PtrUInt;
  TThreadID=  THandle;
  
  { the fields of this record are os dependent  }
  { and they shouldn't be used in a program     }
  { only the type TCriticalSection is important }
  PRTLCriticalSection = ^TRTLCriticalSection;
  TRTLCriticalSection = packed record
{    DebugInfo : pointer;
    LockCount : longint;
    RecursionCount : longint;
    OwningThread : THandle;
    LockSemaphore : THandle;
    SpinCount : LongWord; // PtrUInt?
}
  end;
