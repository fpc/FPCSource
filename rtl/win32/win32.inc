{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Win32 Types and Constants

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
   { constants for GetStdHandle }
   STD_INPUT_HANDLE = longint($fffffff6);
   STD_OUTPUT_HANDLE = longint($fffffff5);
   STD_ERROR_HANDLE = longint($fffffff4);
   INVALID_HANDLE_VALUE = longint($ffffffff);

   IGNORE = 0;               { Ignore signal }
   INFINITE = longint($FFFFFFFF);     { Infinite timeout }

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

   { Share mode open }
   fmShareCompat    = $00000000;
   fmShareExclusive = $10;
   fmShareDenyWrite = $20;
   fmShareDenyRead  = $30;
   fmShareDenyNone  = $40;

   { flags for SetFilePos }
   FILE_BEGIN = 0;
   FILE_CURRENT = 1;
   FILE_END = 2;

   { GetFileType  }
   FILE_TYPE_UNKNOWN = 0;
   FILE_TYPE_DISK = 1;
   FILE_TYPE_CHAR = 2;
   FILE_TYPE_PIPE = 3;

   VER_PLATFORM_WIN32s = 0;
   VER_PLATFORM_WIN32_WINDOWS = 1;
   VER_PLATFORM_WIN32_NT = 2;
type
   {UINT  = longint;
   BOOL  = longint; obsolete }
   UINT  = cardinal;
   BOOL  = longbool;
//   WCHAR = word;
{$ifdef UNICODE}
   LPTCH   = ^word;
   LPTSTR  = ^word;
   LPCTSTR = ^word;
{$else UNICODE}
   LPTCH   = ^char;
   LPTSTR  = ^char;
   LPCTSTR = ^char;
{$endif UNICODE}
   LPWSTR  = ^wchar;
   PVOID   = pointer;
   LPVOID  = pointer;
   LPCVOID = pointer;
   LPDWORD = ^DWORD;
   HLocal  = THandle;
   PStr    = pchar;
   LPStr   = pchar;
   PLPSTR  = ^LPSTR;
   PLPWSTR = ^LPWSTR;

  PSecurityAttributes = ^TSecurityAttributes;
  TSecurityAttributes = packed record
    nLength : DWORD;
    lpSecurityDescriptor : Pointer;
    bInheritHandle : BOOL;
  end;

  PProcessInformation = ^TProcessInformation;
  TProcessInformation = record
    hProcess: THandle;
    hThread: THandle;
    dwProcessId: DWORD;
    dwThreadId: DWORD;
  end;

  PFileTime = ^TFileTime;
  TFileTime = record
    dwLowDateTime,
    dwHighDateTime : DWORD;
  end;

  LPSystemTime= ^PSystemTime;
  PSystemTime = ^TSystemTime;
  TSystemTime = record
    wYear,
    wMonth,
    wDayOfWeek,
    wDay,
    wHour,
    wMinute,
    wSecond,
    wMilliseconds: Word;
  end;

{
  $Log$
  Revision 1.10  2003-10-06 23:52:53  florian
    * some data types cleaned up

  Revision 1.9  2002/09/07 16:01:29  peter
    * old logs removed and tabs fixed

  Revision 1.8  2002/05/09 08:28:23  carl
  * Merges from Fixes branch

}
