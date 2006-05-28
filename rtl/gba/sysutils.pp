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

uses dos, sysconst;

{ Include platform independent implementation part }
{$i sysutils.inc}


{****************************************************************************
                              File Functions
****************************************************************************}
function FileOpen(const FileName: string; Mode: Integer): LongInt;
begin
end;


function FileGetDate(Handle: LongInt) : LongInt;
begin
end;


function FileSetDate(Handle, Age: LongInt) : LongInt;
begin
end;


function FileCreate(const FileName: string) : LongInt;
begin
end;


function FileCreate(const FileName: string; Mode: integer): LongInt;
begin
end;


function FileRead(Handle: LongInt; var Buffer; Count: LongInt): LongInt;
begin
end;


function FileWrite(Handle: LongInt; const Buffer; Count: LongInt): LongInt;
begin
end;


function FileSeek(Handle, FOffset, Origin: LongInt) : LongInt;
begin
end;

function FileSeek(Handle: LongInt; FOffset, Origin: Int64): Int64;
begin
end;


procedure FileClose(Handle: LongInt);
begin
end;


function FileTruncate(Handle, Size: LongInt): Boolean;
begin
end;


function DeleteFile(const FileName: string) : Boolean;
begin
end;


function RenameFile(const OldName, NewName: string): Boolean;
begin
end;


(****** end of non portable routines ******)


Function FileAge (Const FileName : String): Longint;
begin
end;


Function FileExists (Const FileName : String) : Boolean;
Begin
end;



Function FindFirst (Const Path : String; Attr : Longint; Out Rslt : TSearchRec) : Longint;
begin
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;
begin
end;

Procedure FindClose (Var F : TSearchrec);
begin
end;

Function FileGetAttr (Const FileName : String) : Longint;
begin

end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;
begin

end;



{****************************************************************************
                              Disk Functions
****************************************************************************}

Procedure AddDisk(const path:string);
begin

end;



Function DiskFree(Drive: Byte): int64;
Begin

End;


Function DiskSize(Drive: Byte): int64;
Begin

End;


Function GetCurrentDir : String;
begin

end;


Function SetCurrentDir (Const NewDir : String) : Boolean;
begin
end;


Function CreateDir (Const NewDir : String) : Boolean;
begin
end;


Function RemoveDir (Const Dir : String) : Boolean;
begin
end;


function DirectoryExists(const Directory: string): Boolean;
begin
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
end;

{****************************************************************************
                              OS utility functions
****************************************************************************}

Function GetEnvironmentVariable(Const EnvVar : String) : String;
begin
end;

Function GetEnvironmentVariableCount : Integer;
begin
end;

Function GetEnvironmentString(Index : Integer) : String;
begin
end;

function ExecuteProcess (const Path: AnsiString; const ComLine: AnsiString): integer;
begin
end;

function ExecuteProcess (const Path: AnsiString;
                                  const ComLine: array of AnsiString): integer;
begin
end;


{****************************************************************************
                              Initialization code
****************************************************************************}

Initialization
  InitExceptions;
Finalization
  DoneExceptions;
end.
