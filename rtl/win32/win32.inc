{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Florian Klaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

    const
       STD_INPUT_HANDLE = $fffffff6;
       STD_OUTPUT_HANDLE = $fffffff5;
       STD_ERROR_HANDLE = $fffffff4;
       INVALID_HANDLE_VALUE = $ffffffff;
       LMEM_FIXED = 0;

    type
       UINT = longint;
       DWORD = longint;
       LPDWORD = ^DWORD;
       BOOL = longint;
{$ifdef UNICODE}
       LPTCH = ^word;
       LPTSTR = ^word;
       LPCTSTR = ^word;
{$else UNICODE}
       LPTCH = ^char;
       LPTSTR = ^char;
       LPCTSTR = ^char;
{$endif UNICODE}
       PVOID = pointer;
       LPVOID = pointer;
       LPCVOID = pointer;
       HANDLE = pointer;
       HLOCAL = HANDLE;
       PSTR = pchar;

       OVERLAPPED = record
            Internal : DWORD;
            InternalHigh : DWORD;
            Offset : DWORD;
            OffsetHigh : DWORD;
            hEvent : HANDLE;
         end;
       LPOVERLAPPED = ^OVERLAPPED;

   { command line/enviroment functions }
   function GetCommandLine : LPTSTR;external 'kernel32' name 'GetCommandLineA';
   function GetEnvironmentStrings : LPVOID;[C];

   { string functions }
   function lstrlen(lpString:LPCTSTR):longint;[C];
   function lstrcat(lpString1:LPTSTR; lpString2:LPCTSTR):LPTSTR;[C];
   function lstrcpy(lpString1:LPTSTR; lpString2:LPCTSTR):LPTSTR;[C];

   { process functions }
   procedure ExitProcess(uExitCode : UINT);external 'kernel32' name 'ExitProcess';

   { file functions }
   function GetStdHandle(nStdHandle:DWORD):HANDLE;external 'kernel32' name 'GetStdHandle';
   function WriteFile(fh:longint;buf:pointer;len:longint;var loaded:longint;
                   overlap:pointer):longint;external 'kernel32' name 'WriteFile';
   function ReadFile(fh:longint;buf:pointer;len:longint;var loaded:longint;
                  overlap:pointer):longint;external 'kernel32' name 'ReadFile';
   function CloseHandle(h : longint) : longint;external 'kernel32' name 'CloseHandle';
   function DeleteFile(p : pchar) : longint;external 'kernel32' name 'DeleteFileA';
   function MoveFile(old,_new : pchar) : longint;external 'kernel32' name 'MoveFileA';
   function SetFilePointer(l1,l2 : longint;l3 : pointer;l4 : longint) : longint;
     external 'kernel32' name 'SetFilePointer';

   { module functions }
   function GetModuleFileName(l1:longint;p:pointer;l2:longint):longint;external 'kernel32' name 'GetModuleFileNameA';
   procedure GetStartupInfo(p : pointer);external 'kernel32' name 'GetStartupInfoA';
   function GetModuleHandle(p : pointer) : longint;external 'kernel32' name 'GetModuleHandleA';

   { memory functions }
   function GlobalAlloc(mode,size:longint):longint;external 'kernel32' name 'GlobalAlloc';
   function GlobalHandle(p:pointer):longint;external 'kernel32' name 'GlobalHandle';
   function GlobalLock(handle:longint):pointer;external 'kernel32' name 'GlobalLock';
   function GlobalUnlock(h:longint):longint;external 'kernel32' name 'GlobalUnlock';
   function GlobalFree(h:longint):longint;external 'kernel32' name 'GlobalUnlock';
   procedure GlobalMemoryStatus(p:pointer);external 'kernel32' name 'GlobalMemoryStatus';
   function LocalAlloc(uFlags : UINT;uBytes :UINT) : HLOCAL;external 'kernel32' name 'LocalAlloc';
   function LocalFree(hMem:HLOCAL):HLOCAL;external 'kernel32' name 'LocalFree';

   { misc. functions }
   function GetLastError : DWORD;external 'kernel32' name 'GetLastError';
   function messagebox(w1:longint;l1,l2:pointer;w2:longint):longint;external 'user32' name 'MessageBoxA';

{
  $Log$
  Revision 1.1  1998-03-25 11:18:47  root
  Initial revision

  Revision 1.5  1998/03/05 22:37:37  florian
    * some stuff added

  Revision 1.4  1998/01/26 12:02:33  michael
  + Added log at the end


  
  Working file: rtl/win32/win32.inc
  description:
  ----------------------------
  revision 1.3
  date: 1997/12/19 11:47:09;  author: florian;  state: Exp;  lines: +2 -2
  *** empty log message ***
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:42:45;  author: michael;  state: Exp;  lines: +15 -0
  + added copyright reference in header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 10:15:33;  author: florian;  state: Exp;
  Win32 files added (they are untested)
  =============================================================================
}
