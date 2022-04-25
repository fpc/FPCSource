{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Type TExecuteFlags = Set of ( ExecInheritsHandles);

{ OS handling utilities }

{$ifdef HAS_OSERROR}
Function GetLastOSError : Integer;
{$endif}
Procedure RaiseLastOSError;overload;
Procedure RaiseLastOSError(LastError: Integer);overload;
procedure CheckOSError(LastError: Integer);platform;{$ifdef SYSTEMINLINE}inline;{$endif}

Function GetEnvironmentVariable(Const EnvVar : AnsiString) : AnsiString; overload;
Function GetEnvironmentVariable(Const EnvVar : UnicodeString) : UnicodeString; overload;
Function GetEnvironmentString(Index : Integer) : {$ifdef FPC_RTL_UNICODE}UnicodeString{$else}AnsiString{$endif};
Function GetEnvironmentVariableCount : Integer;
{$IFDEF HAS_SLEEP}
procedure Sleep(milliseconds: Cardinal);
{$ENDIF}
function ExecuteProcess(Const Path: RawByteString; Const ComLine: RawByteString;Flags:TExecuteFlags=[]):integer;
function ExecuteProcess(Const Path: RawByteString; Const ComLine: Array of RawByteString;Flags:TExecuteFlags=[]):integer;
function ExecuteProcess(Const Path: UnicodeString; Const ComLine: UnicodeString;Flags:TExecuteFlags=[]):integer;
function ExecuteProcess(Const Path: UnicodeString; Const ComLine: Array of UnicodeString;Flags:TExecuteFlags=[]):integer;

Function GetTempDir(Global : Boolean) : String;
Function GetTempDir : String;
Function GetTempFileName(Const Dir,Prefix : String) : String;
Function GetTempFileName : String;
{ For windows compatibility }
function GetTempFileName(Dir,Prefix: PChar; uUnique: DWORD; TempFileName: PChar):DWORD;
Function GetAppConfigDir(Global : Boolean) : String;
Function GetAppConfigFile(Global : Boolean) : String;
Function GetAppConfigFile(Global : Boolean; SubDir : Boolean) : String;
Function GetUserDir : String;
Function VendorName : String;
Function ApplicationName : String;

Const
  ConfigExtension : String = '.cfg';
  SysConfigDir    : String = '';

Type
  TGetVendorNameEvent  = Function : String;
  TGetAppNameEvent  = Function : String;
  TGetTempDirEvent  = Function (Global : Boolean) : String;
  TGetTempFileEvent = Function (Const Dir,Prefix : String) : String;

Type
  TEventType = (etCustom,etInfo,etWarning,etError,etDebug);
  TEventTypes = Set of TEventType;

Var
  OnGetVendorName      : TGetVendorNameEvent;
  OnGetApplicationName : TGetAppNameEvent;
  OnGetTempDir         : TGetTempDirEvent;
  OnGetTempFile        : TGetTempFileEvent;


