{
    Basic Windows stuff

    This file is part of the Free Pascal run time library.
    Copyright (c) 2002-2005 by Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Platform specific information }
type
{$ifdef CPU64}
  THandle = QWord;
  ULONG_PTR = QWord;
{$else CPU64}
  THandle = DWord;
  ULONG_PTR = DWord;
{$endif CPU64}
  TThreadID = THandle;
  SIZE_T = ULONG_PTR;

  { the fields of this record are os dependent  }
  { and they shouldn't be used in a program     }
  { only the type TCriticalSection is important }
  PRTLCriticalSection = ^TRTLCriticalSection;
{$ifndef WINCE}
  TRTLCriticalSection = packed record
    DebugInfo : pointer;
    LockCount : longint;
    RecursionCount : longint;
    OwningThread : THandle;
    LockSemaphore : THandle;
    SpinCount : ULONG_PTR;
  end;
{$else WINCE}
  //CE only
  TRTLCriticalSection = packed record
    LockCount     : DWord ;   // Nesting count on critical section
    OwnerThread   : THandle; 	// Handle of owner thread
    hCrit         : THandle;	// Handle to this critical section
    needtrap      : DWORD;	// Trap in when freeing critical section
    dwContentions : DWORD ;	// Count of contentions
  end;
{$endif WINCE}

const
{$ifdef WINCE}
  KernelDLL = 'coredll';
  ApiSuffix = 'W';
{$else WINCE}
  KernelDLL = 'kernel32';
  ApiSuffix = 'A';
{$endif WINCE}

function OleStrToString(source: PWideChar) : UnicodeString;inline;
procedure OleStrToStrVar(source : PWideChar;var dest : UnicodeString);inline;
procedure OleStrToStrVar(source : PWideChar;var dest : AnsiString);inline;
function StringToOleStr(const source : ansistring) : PWideChar;inline;
function StringToOleStr(const source : UnicodeString) : PWideChar;inline;

{ package stuff }
type
  PLibModule = ^TLibModule;
  TLibModule = record
    Next: PLibModule;
    Instance: THandle;
    CodeInstance: THandle;
    DataInstance: THandle;
    ResInstance: THandle;
    Reserved: PtrInt;
  end;

var
  LibModuleList: PLibModule = nil;
