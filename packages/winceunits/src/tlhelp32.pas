{******************************************************************************}
{                                                                              }
{ ToolHelp API interface Unit for Object Pascal                                }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: tlhelp32.h, released June 2000. The original Pascal    }
{ code is: TlHelp32.pas, released December 2000. The initial developer of the  }
{ Pascal code is Marcel van Brakel (brakelm att chello dott nl).               }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
{                                                                              }
{ Modified for usage with WinCE by Sven Barth                                  }
{ Based on JwaTlHelp32.pas,v 1.11 2007/09/05 11:58:52                          }
{ FPC revision 15911                                                           }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI         }
{ APILIB home page, located at http://jedi-apilib.sourceforge.net              }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

unit TlHelp32;

{$mode objfpc}

interface

uses
  windows;

const
  libtoolhelp = 'toolhelp.dll';

// Snapshot function

function CreateToolhelp32Snapshot(dwFlags, th32ProcessID: DWORD): HANDLE; cdecl;
  external libtoolhelp name 'CreateToolhelp32Snapshot';
function CloseToolhelp32Snapshot(hSnapshot: HANDLE): BOOL; cdecl;
  external libtoolhelp name 'CloseToolhelp32Snapshot';

//
// The th32ProcessID argument is only used if TH32CS_SNAPHEAPLIST or
// TH32CS_SNAPMODULE is specified. th32ProcessID == 0 means the current
// process.
//
// NOTE that all of the snapshots are global except for the heap and module
//      lists which are process specific. To enumerate the heap or module
//      state for all WIN32 processes call with TH32CS_SNAPALL and the
//      current process. Then for each process in the TH32CS_SNAPPROCESS
//      list that isn't the current process, do a call with just
//      TH32CS_SNAPHEAPLIST and/or TH32CS_SNAPMODULE.
//
// dwFlags
//

const
  TH32CS_SNAPHEAPLIST = $00000001;
  TH32CS_SNAPPROCESS  = $00000002;
  TH32CS_SNAPTHREAD   = $00000004;
  TH32CS_SNAPMODULE   = $00000008;
  TH32CS_SNAPALL      = TH32CS_SNAPHEAPLIST or TH32CS_SNAPPROCESS or
                        TH32CS_SNAPTHREAD or TH32CS_SNAPMODULE;
  TH32CS_GETALLMODS   = $80000000;

//
// Use CloseToolhelp32Snapshot to destroy the snapshot
//

// Heap walking

type
  PHEAPLIST32 = ^HEAPLIST32;
  tagHEAPLIST32 = record
    dwSize: DWORD;
    th32ProcessID: DWORD;   // owning process
    th32HeapID: DWORD;  // heap (in owning process's context!)
    dwFlags: DWORD;
  end;
  HEAPLIST32 = tagHEAPLIST32;
  LPHEAPLIST32 = ^HEAPLIST32;
  THeapList32 = HEAPLIST32;

//
// dwFlags
//

const
  HF32_DEFAULT    = 1;  // process's default heap
  HF32_SHARED     = 2;  // is shared heap

function Heap32ListFirst(hSnapshot: HANDLE; var lphl: HEAPLIST32): BOOL; cdecl;
  external libtoolhelp name 'Heap32ListFirst';
function Heap32ListNext(hSnapshot: HANDLE; var lphl: HEAPLIST32): BOOL; cdecl;
  external libtoolhelp name 'Heap32ListNext';

type
  PHEAPENTRY32 = ^HEAPENTRY32;
  tagHEAPENTRY32 = record
    dwSize: DWORD;
    hHandle: HANDLE;       // Handle of this heap block
    dwAddress: DWORD;  // Linear address of start of block
    dwBlockSize: DWORD;   // Size of block in bytes
    dwFlags: DWORD;
    dwLockCount: DWORD;
    dwResvd: DWORD;
    th32ProcessID: DWORD;  // owning process
    th32HeapID: DWORD; // heap block is in
  end;
  HEAPENTRY32 = tagHEAPENTRY32;
  LPHEAPENTRY32 = ^HEAPENTRY32;
  THeapEntry32 = HEAPENTRY32;

//
// dwFlags
//

const
  LF32_FIXED    = $00000001;
  LF32_FREE     = $00000002;
  LF32_MOVEABLE = $00000004;

function Heap32First(var lphe: HEAPENTRY32; th32ProcessID: DWORD;
  th32HeapID: ULONG_PTR): BOOL; cdecl; external libtoolhelp name 'Heap32First';
function Heap32Next(var lphe: HEAPENTRY32): BOOL; cdecl; external libtoolhelp
  name 'Heap32Next';

function Toolhelp32ReadProcessMemory(th32ProcessID: DWORD; lpBaseAddress: LPCVOID;
  lpBuffer: LPVOID; cbRead: DWORD; lpNumberOfBytesRead: LPDWORD): BOOL; cdecl;
  external libtoolhelp name 'Toolhelp32ReadProcessMemory';

// Process walking

type
  PPROCESSENTRY32 = ^PROCESSENTRY32;
  tagPROCESSENTRY32 = record
    dwSize: DWORD;
    cntUsage: DWORD;
    th32ProcessID: DWORD;          // this process
    th32DefaultHeapID: DWORD;
    th32ModuleID:DWORD;            // associated exe
    cntThreads: DWORD;
    th32ParentProcessID: DWORD;    // this process's parent process
    pcPriClassBase: LONG;          // Base priority of process's threads
    dwFlags: DWORD;
    szExeFile: array [0..MAX_PATH - 1] of WCHAR;   // Path
    th32MemoryBase: DWORD;
    th32AccessKey: DWORD;
  end;
  PROCESSENTRY32 = tagPROCESSENTRY32;
  LPPROCESSENTRY32 = ^PROCESSENTRY32;
  TProcessEntry32 = PROCESSENTRY32;

function Process32First(hSnapshot: HANDLE; var lppe: PROCESSENTRY32): BOOL; cdecl;
  external libtoolhelp name 'Process32First';
function Process32Next(hSnapshot: HANDLE; var lppe: PROCESSENTRY32): BOOL; cdecl;
  external libtoolhelp name 'Process32Next';

// Thread walking

type
  PTHREADENTRY32 = ^THREADENTRY32;
  tagTHREADENTRY32 = record
    dwSize: DWORD;
    cntUsage: DWORD;
    th32ThreadID: DWORD;       // this thread
    th32OwnerProcessID: DWORD; // Process this thread is associated with
    tpBasePri: LONG;
    tpDeltaPri: LONG;
    dwFlags: DWORD;
    th32AccessKey: DWORD;
    th32CurrentProcessID: DWORD;
  end;
  THREADENTRY32 = tagTHREADENTRY32;
  LPTHREADENTRY32 = ^THREADENTRY32;
  TThreadEntry32 = THREADENTRY32;

function Thread32First(hSnapshot: HANDLE; var lpte: THREADENTRY32): BOOL; cdecl;
  external libtoolhelp name 'Thread32First';
function Thread32Next(hSnapshot: HANDLE; var lpte: THREADENTRY32): BOOL; cdecl;
  external libtoolhelp name 'Thread32Next';

// Module walking

type
  PMODULEENTRY32 = ^MODULEENTRY32;
  tagMODULEENTRY32 = record
    dwSize: DWORD;
    th32ModuleID: DWORD;       // This module
    th32ProcessID: DWORD;      // owning process
    GlblcntUsage: DWORD;       // Global usage count on the module
    ProccntUsage: DWORD;       // Module usage count in th32ProcessID's context
    modBaseAddr: LPBYTE;       // Base address of module in th32ProcessID's context
    modBaseSize: DWORD;        // Size in bytes of module starting at modBaseAddr
    hModule: HMODULE;          // The hModule of this module in th32ProcessID's context
    szModule: array [0..MAX_PATH - 1] of WCHAR;
    szExePath: array [0..MAX_PATH - 1] of WCHAR;
    dwFlags: DWORD;            // Reserved
  end;
  MODULEENTRY32 = tagMODULEENTRY32;
  LPMODULEENTRY32 = ^MODULEENTRY32;
  TModuleEntry32 = MODULEENTRY32;

//
// NOTE CAREFULLY that the modBaseAddr and hModule fields are valid ONLY
// in th32ProcessID's process context.
//
function Module32First(hSnapshot: HANDLE; var lpme: MODULEENTRY32): BOOL; cdecl;
  external libtoolhelp name 'Module32First';
function Module32Next(hSnapshot: HANDLE; var lpme: MODULEENTRY32): BOOL; cdecl;
  external libtoolhelp name 'Module32Next';

implementation

end.
