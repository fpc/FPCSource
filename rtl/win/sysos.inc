{
    Basic stuff for windows rtls

    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
   { constants for GetStdHandle }
   STD_INPUT_HANDLE = dword(-10);
   STD_OUTPUT_HANDLE = dword(-11);
   STD_ERROR_HANDLE = dword(-12);
   INVALID_HANDLE_VALUE = THandle(-1);

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

  { These constants are used for conversion of error codes }
  { from win32 i/o errors to tp i/o errors                 }
  { errors 1 to 18 are the same as in Turbo Pascal         }
  { DO NOT MODIFY UNLESS YOU KNOW EXACTLY WHAT YOU ARE DOING! }

{  The media is write protected.                   }
    ERROR_WRITE_PROTECT       =      19;
{  The system cannot find the device specified.    }
    ERROR_BAD_UNIT            =      20;
{  The device is not ready.                        }
    ERROR_NOT_READY           =      21;
{  The device does not recognize the command.      }
    ERROR_BAD_COMMAND         =      22;
{  Data error (cyclic redundancy check)            }
    ERROR_CRC                 =      23;
{  The program issued a command but the            }
{  command length is incorrect.                    }
    ERROR_BAD_LENGTH           =     24;
{  The drive cannot locate a specific              }
{  area or track on the disk.                      }
    ERROR_SEEK                 =     25;
{  The specified disk or diskette cannot be accessed. }
    ERROR_NOT_DOS_DISK         =     26;
{  The drive cannot find the sector requested.     }
    ERROR_SECTOR_NOT_FOUND      =    27;
{  The printer is out of paper.                    }
    ERROR_OUT_OF_PAPER          =    28;
{  The system cannot write to the specified device. }
    ERROR_WRITE_FAULT           =    29;
{  The system cannot read from the specified device. }
    ERROR_READ_FAULT            =    30;
{  A device attached to the system is not functioning.}
    ERROR_GEN_FAILURE           =    31;
{  The process cannot access the file because         }
{  it is being used by another process.               }
    ERROR_SHARING_VIOLATION      =   32;
{   A pipe has been closed on the other end }
{   Removing that error allows eof to works as on other OSes }
    ERROR_BROKEN_PIPE = 109;
    ERROR_DIR_NOT_EMPTY = 145;
    ERROR_ALREADY_EXISTS = 183;

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
  TSecurityAttributes = record
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

threadvar
  errno : longint;


   { misc. functions }
   function GetLastError : DWORD;
     {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'GetLastError';

   { time and date functions }
   function GetTickCount : longint;
     {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'GetTickCount';

{$ifndef WINCE}
   { process functions }
   procedure ExitProcess(uExitCode : UINT);
     stdcall;external KernelDLL name 'ExitProcess';

   { Startup }
   procedure GetStartupInfo(p : pointer);
     stdcall;external KernelDLL name 'GetStartupInfoA';
   function GetStdHandle(nStdHandle:DWORD):THANDLE;
     stdcall;external KernelDLL name 'GetStdHandle';

   { command line/enviroment functions }
   function GetCommandLine : pchar;
     stdcall;external KernelDLL name 'GetCommandLineA';

   function GetCommandLineW : pwidechar;
     stdcall;external KernelDLL name 'GetCommandLineW';

  function GetCurrentProcessId:DWORD;
    stdcall; external KernelDLL name 'GetCurrentProcessId';

  function Win32GetCurrentThreadId:DWORD;
    stdcall; external KernelDLL name 'GetCurrentThreadId';
{$endif WINCE}

   { module functions }
   function GetModuleFileName(l1:longint;p:pointer;l2:longint):longint;
     {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'GetModuleFileName' + ApiSuffix;
   function GetModuleHandle(p : pointer) : PtrUInt;
     {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'GetModuleHandle' + ApiSuffix;

   { file functions }
   function WriteFile(fh:thandle;buf:pointer;len:longint;var loaded:longint;
     overlap:pointer):longint;
     {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'WriteFile';
   function ReadFile(fh:thandle;buf:pointer;len:longint;var loaded:longint;
     overlap:pointer):longint;
     {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'ReadFile';
   function CloseHandle(h : thandle) : longint;
     {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'CloseHandle';
   function SetFilePointer(l1,l2 : thandle;l3 : pointer;l4 : longint) : longint;
     {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'SetFilePointer';
   function GetFileSize(h:thandle;p:pointer) : longint;
     {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'GetFileSize';
   function SetEndOfFile(h : thandle) : longbool;
     {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'SetEndOfFile';
   function FreeLibrary(hLibModule:THandle):ByteBool; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'FreeLibrary';

{$ifndef WINCE}
   function LoadLibrary(lpLibFileName:pchar):THandle; stdcall; external KernelDLL name 'LoadLibraryA';
   function GetFileType(Handle:thandle):DWord;
     stdcall;external KernelDLL name 'GetFileType';
   function GetFileAttributes(p : pchar) : dword;
     stdcall;external KernelDLL name 'GetFileAttributesA';
   function DeleteFile(p : pchar) : longint;
     stdcall;external KernelDLL name 'DeleteFileA';
   function MoveFile(old,_new : pchar) : longint;
     stdcall;external KernelDLL name 'MoveFileA';
   function CreateFile(lpFileName:pchar; dwDesiredAccess:DWORD; dwShareMode:DWORD;
                       lpSecurityAttributes:PSECURITYATTRIBUTES; dwCreationDisposition:DWORD;
                       dwFlagsAndAttributes:DWORD; hTemplateFile:DWORD):THandle;
     stdcall;external KernelDLL name 'CreateFileA';
   function GetProcAddress(hModule:THandle; lpProcName:pchar):pointer; stdcall; external KernelDLL name 'GetProcAddress';

   { Directory }
   function CreateDirectory(name : pointer;sec : pointer) : longbool;
     stdcall;external KernelDLL name 'CreateDirectoryA';
   function RemoveDirectory(name:pointer):longbool;
     stdcall;external KernelDLL name 'RemoveDirectoryA';
   function SetCurrentDirectory(name : pointer) : longbool;
     stdcall;external KernelDLL name 'SetCurrentDirectoryA';
   function GetCurrentDirectory(bufsize : longint;name : pchar) : longbool;
     stdcall;external KernelDLL name 'GetCurrentDirectoryA';

   var
     SetFilePointerEx : function(hFile : THandle;
       liDistanceToMove : int64;lpNewFilePointer : pint64;
       dwMoveMethod : DWord) : ByteBool;stdcall;

  procedure SetupProcVars;
    var
      hinstLib : THandle;
    begin
      SetFilePointerEx:=nil;
      hinstLib:=LoadLibrary(KernelDLL);
      if hinstLib<>0 then
        begin
          pointer(SetFilePointerEx):=GetProcAddress(hinstLib,'SetFilePointerEx');
          FreeLibrary(hinstLib);
        end;
    end;
{$endif WINCE}

   Procedure Errno2InOutRes;
   var
     res : Word;
     pErrno : ^longint;
   Begin
     { DO NOT MODIFY UNLESS YOU KNOW EXACTLY WHAT YOU ARE DOING }
     pErrno:=@Errno;
     case pErrno^ of
       ERROR_WRITE_PROTECT..ERROR_GEN_FAILURE :
         begin
           { This is the offset to the Win32 to add to directly map  }
           { to the DOS/TP compatible error codes when in this range }
           res := word(pErrno^)+131;
         end;
       ERROR_DIR_NOT_EMPTY,
       ERROR_ALREADY_EXISTS,
       ERROR_SHARING_VIOLATION :
         begin
           res :=5;
         end;
       else
         begin
           { other error codes can directly be mapped }
           res := Word(pErrno^);
         end;
     end;
     pErrno^:=0;
     InOutRes:=res;
   end;


  function OleStrToString(source: PWideChar) : ansistring;inline;
    begin
      OleStrToStrVar(source,result);
    end;


  procedure OleStrToStrVar(source : PWideChar;var dest : ansistring);inline;
    begin
      WideCharLenToStrVar(source,length(WideString(pointer(source))),dest);
    end;


  function StringToOleStr(const source : ansistring) : PWideChar;inline;
    begin
      result:=nil;
      widestringmanager.Ansi2WideMoveProc(pchar(pointer(source)),widestring(pointer(result)),length(source));
    end;
