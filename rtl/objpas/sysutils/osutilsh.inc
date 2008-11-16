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

{ OS handling utilities }

{$ifdef HAS_OSERROR}
Function GetLastOSError : Integer;
{$endif}
Procedure RaiseLastOSError;
Function GetEnvironmentVariable(Const EnvVar : String) : String;
Function GetEnvironmentVariableCount : Integer;
Function GetEnvironmentString(Index : Integer) : String;
{$IFDEF HAS_SLEEP}
procedure Sleep(milliseconds: Cardinal);
{$ENDIF}
function ExecuteProcess(Const Path: AnsiString; Const ComLine: AnsiString):integer;
function ExecuteProcess(Const Path: AnsiString; Const ComLine: Array of AnsiString):integer;


Function GetTempDir(Global : Boolean) : String;
Function GetTempDir : String;
Function GetTempFileName(Const Dir,Prefix : String) : String;
Function GetTempFileName : String;
{ For windows compatibility }
function GetTempFileName(Dir,Prefix: PRtlChar; uUnique: DWORD; TempFileName: PRtlChar):DWORD;
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

Var
  OnGetVendorName      : TGetVendorNameEvent;
  OnGetApplicationName : TGetAppNameEvent;
  OnGetTempDir         : TGetTempDirEvent;
  OnGetTempFile        : TGetTempFileEvent;


