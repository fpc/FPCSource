{
    $Id$
    This file contains the Win32-API import declarations
    for the system unit and the DOS unit

    This file is part of the Free Pascal run time library.
    Copyright (c) 1997,98 by Florian Klaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

    const
       { constants for GetStdHandle }
       STD_INPUT_HANDLE = $fffffff6;
       STD_OUTPUT_HANDLE = $fffffff5;
       STD_ERROR_HANDLE = $fffffff4;
       INVALID_HANDLE_VALUE = $ffffffff;

       { flags for CreateFile }
       GENERIC_READ=$80000000;
       GENERIC_WRITE=$40000000;
       CREATE_NEW = 1;
       CREATE_ALWAYS = 2;
       OPEN_EXISTING = 3;
       OPEN_ALWAYS = 4;
       TRUNCATE_EXISTING = 5;

       FILE_ATTRIBUTE_ARCHIVE = 32;
       FILE_ATTRIBUTE_COMPRESSED = 2048;
       FILE_ATTRIBUTE_NORMAL = 128;
       FILE_ATTRIBUTE_DIRECTORY = 16;
       FILE_ATTRIBUTE_HIDDEN = 2;
       FILE_ATTRIBUTE_READONLY = 1;
       FILE_ATTRIBUTE_SYSTEM = 4;
       FILE_ATTRIBUTE_TEMPORARY = 256;

       { flags for SetFilePos }
       FILE_BEGIN = 0;
       FILE_CURRENT = 1;
       FILE_END = 2;

    type
       UINT = longint;
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

       SYSTEMTIME = record
         wYear,wMonth,wDayOfWeek,wDay,
	 wHour,wMinute,wSecond,WMilliseconds : word;
       end;

       FILETIME = record
         dwLowDateTime : longint;
         dwHighDateTime : longint;
       end;

       PFILETIME = ^FILETIME;

   { command line/enviroment functions }
   function GetCommandLine : LPTSTR;
     external 'kernel32' name 'GetCommandLineA';
   function GetEnvironmentStrings : pchar;
     external 'kernel32' name 'GetEnvironmentStringsA';
   function FreeEnvironmentStrings(p : pchar) : boolean;
     external 'kernel32' name 'FreeEnvironmentStringsA';

   { string functions
   function lstrlen(lpString:LPCTSTR):longint;external;
   function lstrcat(lpString1:LPTSTR; lpString2:LPCTSTR):LPTSTR;external;
   function lstrcpy(lpString1:LPTSTR; lpString2:LPCTSTR):LPTSTR;external;
   }

   { process functions }
   procedure ExitProcess(uExitCode : UINT);
     external 'kernel32' name 'ExitProcess';

   { file functions }
   function GetStdHandle(nStdHandle:DWORD):HANDLE;
     external 'kernel32' name 'GetStdHandle';
   function WriteFile(fh:longint;buf:pointer;len:longint;var loaded:longint;
     overlap:pointer):longint;
     external 'kernel32' name 'WriteFile';
   function ReadFile(fh:longint;buf:pointer;len:longint;var loaded:longint;
     overlap:pointer):longint;
     external 'kernel32' name 'ReadFile';
   function CloseHandle(h : longint) : longint;
     external 'kernel32' name 'CloseHandle';
   function DeleteFile(p : pchar) : longint;
     external 'kernel32' name 'DeleteFileA';
   function MoveFile(old,_new : pchar) : longint;
     external 'kernel32' name 'MoveFileA';
   function SetFilePointer(l1,l2 : longint;l3 : pointer;l4 : longint) : longint;
     external 'kernel32' name 'SetFilePointer';
   function GetFileSize(h:longint;p:pointer) : longint;
     external 'kernel32' name 'GetFileSize';
   function CreateFile(name : pointer;access,sharing : longint;
     security : pointer;how,attr,template : longint) : longint;
     external 'kernel32' name 'CreateFileA';
   function CreateDirectory(name : pointer;sec : pointer) : longint;
     external 'kernel32' name 'CreateDirectoryA';
   function RemoveDirectory(name:pointer):longint;
     external 'kernel32' name 'RemoveDirectoryA';
   function SetCurrentDirectory(name : pointer) : longint;
     external 'kernel32' name 'SetCurrentDirectoryA';
   function GetCurrentDirectory(bufsize : longint;name : pchar) : longint;
     external 'kernel32' name 'GetCurrentDirectoryA';
   function SetFileAttributes(lpFileName : pchar;dwFileAttributes : longint) : boolean;
     external 'kernel32' name 'SetFileAttributesA';
   function GetFileAttributes(lpFileName : pchar) : longint;
     external 'kernel32' name 'GetFileAttributesA';
   function GetFileTime(h : longint;creation,lastaccess,lastwrite : PFILETIME) : boolean;
     external 'kernel32' name 'GetFileTime';
   function SetFileTime(h : longint;creation,lastaccess,lastwrite : PFILETIME) : boolean;
     external 'kernel32' name 'SetFileTime';
   function SetEndOfFile(h : longint) : boolean;
     external 'kernel32' name 'SetEndOfFile';

   { module functions }
   function GetModuleFileName(l1:longint;p:pointer;l2:longint):longint;
     external 'kernel32' name 'GetModuleFileNameA';
   procedure GetStartupInfo(p : pointer);
     external 'kernel32' name 'GetStartupInfoA';
   function GetModuleHandle(p : pointer) : longint;
     external 'kernel32' name 'GetModuleHandleA';

   { memory functions }
   function GlobalAlloc(mode,size:longint):longint;
     external 'kernel32' name 'GlobalAlloc';
   function GlobalHandle(p:pointer):longint;
     external 'kernel32' name 'GlobalHandle';
   function GlobalLock(handle:longint):pointer;
     external 'kernel32' name 'GlobalLock';
   function GlobalUnlock(h:longint):longint;
     external 'kernel32' name 'GlobalUnlock';
   function GlobalFree(h:longint):longint;
     external 'kernel32' name 'GlobalUnlock';
   procedure GlobalMemoryStatus(p:pointer);
     external 'kernel32' name 'GlobalMemoryStatus';
   function LocalAlloc(uFlags : UINT;uBytes :UINT) : HLOCAL;
     external 'kernel32' name 'LocalAlloc';
   function LocalFree(hMem:HLOCAL):HLOCAL;
     external 'kernel32' name 'LocalFree';

   { time and date functions }
   procedure GetLocalTime(var t : SYSTEMTIME);
     external 'kernel32' name 'GetLocalTime';
   function SetLocalTime(const t : SYSTEMTIME) : boolean;
     external 'kernel32' name 'SetLocalTime';
   function FileTimeToDosDateTime(const ft : FILETIME;var data,time : word) : boolean;
     external 'kernel32' name 'FileTimeToDosDateTime';
   function DosDateTimeToFileTime(date,time : word;var ft : FILETIME) : boolean;
     external 'kernel32' name 'DosDateTimeToFileTime';
   function GetTickCount : longint;
     external 'kernel32' name 'GetTickCount';
   function FileTimeToLocalFileTime(const ft : FILETIME;var lft : FILETIME) : boolean;
     external 'kernel32' name 'FileTimeToLocalFileTime';
   function LocalFileTimeToFileTime(const lft : FILETIME;var ft : FILETIME) : boolean;
     external 'kernel32' name 'LocalFileTimeToFileTime';

   { misc. functions }
   function GetLastError : DWORD;
     external 'kernel32' name 'GetLastError';
   function MessageBox(w1:longint;l1,l2:pointer;w2:longint):longint;
     external 'user32' name 'MessageBoxA';
   function GetVersion : longint;
     external 'kernel32' name 'GetVersion';

{
  $Log$
  Revision 1.6  1998-05-06 12:37:22  michael
  + Removed log from before restored version.

  Revision 1.5  1998/04/27 18:25:36  florian
    + constants for CreateFile added

  Revision 1.4  1998/04/26 22:37:22  florian
    * some small extensions

  Revision 1.3  1998/04/26 21:49:58  florian
    + more stuff added (??dir procedures etc.)

  Revision 1.2  1998/03/27 00:50:22  peter
    * small fixes so it compiles

  Revision 1.1.1.1  1998/03/25 11:18:47  root
  * Restored version
}
