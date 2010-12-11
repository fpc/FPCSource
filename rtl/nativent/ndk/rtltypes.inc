{%MainUnit ndk.pas}
{
    Native Development Kit for Native NT

    This file is part of the Free Pascal run time library.
    This unit contains types defined for RTL functions.
    Copyright (c) 2010 by Sven Barth

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
  //
  // Time Structure for RTL Time calls
  //
  _TIME_FIELDS = packed record
    Year: CSHORT;
    Month: CSHORT;
    Day: CSHORT;
    Hour: CSHORT;
    Minute: CSHORT;
    Second: CSHORT;
    Milliseconds: CSHORT;
    Weekday: CSHORT;
  end;
  TIME_FIELDS = _TIME_FIELDS;
  PTIME_FIELDS = ^TIME_FIELDS;

  //
  // Current Directory Structures
  //
  _CURDIR = packed record
    DosPath: UNICODE_STRING;
    Handle: HANDLE;
  end;
  CURDIR = _CURDIR;
  PCURDIR = ^CURDIR;

  _RTL_DRIVE_LETTER_CURDIR = packed record
    Flags: USHORT;
    Length: USHORT;
    TimeStamp: ULONG;
    DosPath: UNICODE_STRING;
  end;
  RTL_DRIVE_LETTER_CURDIR = _RTL_DRIVE_LETTER_CURDIR;
  PRTL_DRIVE_LETTER_CURDIR = ^RTL_DRIVE_LETTER_CURDIR;

  //
  // Structures for RtlCreateUserProcess
  //
  _RTL_USER_PROCESS_PARAMETERS = packed record
    MaximumLength: ULONG;
    Length: ULONG;
    Flags: ULONG;
    DebugFlags: ULONG;
    ConsoleHandle: HANDLE;
    ConsoleFlags: ULONG;
    StandardInput: HANDLE;
    StandardOutput: HANDLE;
    StandardError: HANDLE;
    CurrentDirectory: CURDIR;
    DllPath: UNICODE_STRING;
    ImagePathName: UNICODE_STRING;
    CommandLine: UNICODE_STRING;
    Environment: PWSTR;
    StartingX: ULONG;
    StartingY: ULONG;
    CountX: ULONG;
    CountY: ULONG;
    CountCharsX: ULONG;
    CountCharsY: ULONG;
    FillAttribute: ULONG;
    WindowFlags: ULONG;
    ShowWindowFlags: ULONG;
    WindowTitle: UNICODE_STRING;
    DesktopInfo: UNICODE_STRING;
    ShellInfo: UNICODE_STRING;
    RuntimeData: UNICODE_STRING;
    CurrentDirectories: array[0..31] of RTL_DRIVE_LETTER_CURDIR;
  end;
  RTL_USER_PROCESS_PARAMETERS = _RTL_USER_PROCESS_PARAMETERS;
  PRTL_USER_PROCESS_PARAMETERS = ^RTL_USER_PROCESS_PARAMETERS;

