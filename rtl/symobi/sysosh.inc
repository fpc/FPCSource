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
  { fd are int in C also for 64bit targets (x86_64) }
  THandle = Longint;
  TThreadRec = record end;
  TThreadID  = ^TThreadRec;
  
  { pthread_mutex_t }
  PRTLCriticalSection = ^TRTLCriticalSection;
  TRTLCriticalSection = pointer;
  
const clib = 'c';

{$i ostypes.inc}

// because of "external" these functions need to be imported in the inteface part
{$i oscdeclh.inc}

// used in sysheap.inc
function FpMalloc(size: size_t): pointer; cdecl; external clib name 'malloc';
procedure FpFree(p: pointer); cdecl; external clib name 'free';


