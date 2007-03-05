{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by Karoly Balogh

    Sysutils unit for Gameboy Advance.
    This unit is based on the MorphOS one and is adapted for Gameboy Advance
    simply by stripping out all stuff inside funcs and procs. 
    Copyright (c) 2006 by Francesco Lombardi

    Based on Amiga version by Carl Eric Codere, and other
    parts of the RTL

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit sysutils;

interface

{$MODE objfpc}
{ force ansistrings }
{$H+}

{ Include platform independent interface part }
{$i sysutilh.inc}


implementation

uses 
  dos, sysconst;

{ Include platform independent implementation part }
{$i sysutils.inc}


{****************************************************************************
                              File Functions
****************************************************************************}
function FileOpen(const FileName: string; Mode: Integer): LongInt;
begin
  result := -1;
end;


function FileGetDate(Handle: LongInt) : LongInt;
begin
  result := -1;
end;


function FileSetDate(Handle, Age: LongInt) : LongInt;
begin
  result := -1;
end;


function FileCreate(const FileName: string) : LongInt;
begin
  result := -1;
end;


function FileCreate(const FileName: string; Mode: integer): LongInt;
begin
  result := -1;
end;


function FileRead(Handle: LongInt; var Buffer; Count: LongInt): LongInt;
begin
  result := -1;
end;


function FileWrite(Handle: LongInt; const Buffer; Count: LongInt): LongInt;
begin
  result := -1;
end;


function FileSeek(Handle, FOffset, Origin: LongInt) : LongInt;
begin
  result := -1;
end;

function FileSeek(Handle: LongInt; FOffset: Int64; Origin: Longint): Int64;
begin
  result := -1;
end;


procedure FileClose(Handle: LongInt);
begin
end;


function FileTruncate(Handle: THandle; Size: Int64): Boolean;
begin
  result := false;
end;


function DeleteFile(const FileName: string) : Boolean;
begin
  result := false;
end;


function RenameFile(const OldName, NewName: string): Boolean;
begin
  result := false;
end;


(****** end of non portable routines ******)


Function FileAge (Const FileName : String): Longint;
begin
  result := -1;
end;


Function FileExists (Const FileName : String) : Boolean;
Begin
  result := false;
end;



Function FindFirst (Const Path : String; Attr : Longint; Out Rslt : TSearchRec) : Longint;
begin
  result := -1;
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;
begin
  result := -1;
end;

Procedure FindClose (Var F : TSearchrec);
begin
end;

Function FileGetAttr (Const FileName : String) : Longint;
begin
  result := -1;
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;
begin
  result := -1;
end;



{****************************************************************************
                              Disk Functions
****************************************************************************}

Procedure AddDisk(const path:string);
begin

end;



Function DiskFree(Drive: Byte): int64;
Begin
  result := -1;
End;


Function DiskSize(Drive: Byte): int64;
Begin
  result := -1;
End;


Function GetCurrentDir : String;
begin
  result := '';
end;


Function SetCurrentDir (Const NewDir : String) : Boolean;
begin
  result := false;
end;


Function CreateDir (Const NewDir : String) : Boolean;
begin
  result := false;
end;


Function RemoveDir (Const Dir : String) : Boolean;
begin
  result := false;
end;


function DirectoryExists(const Directory: string): Boolean;
begin
  result := false;
end;



{****************************************************************************
                              Misc Functions
****************************************************************************}

procedure Beep;
begin
end;


{****************************************************************************
                              Locale Functions
****************************************************************************}

Procedure GetLocalTime(var SystemTime: TSystemTime);
begin
end ;


function SysErrorMessage(ErrorCode: Integer): String;
begin
{  Result:=StrError(ErrorCode);}
  result := '';
end;

{****************************************************************************
                              OS utility functions
****************************************************************************}

Function GetEnvironmentVariable(Const EnvVar : String) : String;
begin
  result := '';
end;

Function GetEnvironmentVariableCount : Integer;
begin
  result := -1;
end;

Function GetEnvironmentString(Index : Integer) : String;
begin
  result := '';
end;

function ExecuteProcess (const Path: AnsiString; const ComLine: AnsiString): integer;
begin
  result := -1;
end;

function ExecuteProcess (const Path: AnsiString;
                                  const ComLine: array of AnsiString): integer;
begin
  result := -1;
end;


{****************************************************************************
                              Initialization code
****************************************************************************}

Initialization
  InitExceptions;
Finalization
  DoneExceptions;
end.
