{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Sysutils unit for linux

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

uses
  beos,
  dos;

{ Include platform independent interface part }
{$i sysutilh.inc}


implementation

{ Include platform independent implementation part }
{$i sysutils.inc}


{****************************************************************************
                              File Functions
****************************************************************************}

Function FileOpen (Const FileName : string; Mode : Integer) : Longint;
BEGIN
end;


Function FileCreate (Const FileName : String) : longint;
begin
end;

Function FileCreate (Const FileName : String;Mode:longint) : longint;
begin
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;
begin
end;


Function FileWrite (Handle : Longint; const Buffer; Count : Longint) : Longint;
begin
end;


Function FileSeek (Handle,FOffset,Origin : longint) : longint;
begin
end;

Function FileSeek (Handle:longint;FOffset,Origin : int64) : int64;
begin
end;


Procedure FileClose (Handle : Longint);
begin
end;


Function FileTruncate (Handle,Size: Longint) : boolean;
begin
end;


Function FileAge (Const FileName : String): Longint;
begin
end;


Function FileExists (Const FileName : String) : Boolean;
begin
end;


Function FindFirst (Const Path : String; Attr : Longint; Var Rslt : TSearchRec) : Longint;
begin
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;
begin
end;


Procedure FindClose (Var F : TSearchrec);
begin
end;


Function FileGetDate (Handle : Longint) : Longint;
begin
end;


Function FileSetDate (Handle,Age : Longint) : Longint;
begin
end;


Function FileGetAttr (Const FileName : String) : Longint;
begin
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;
begin
end;


Function DeleteFile (Const FileName : String) : Boolean;
begin
end;


Function RenameFile (Const OldName, NewName : String) : Boolean;
begin
end;

{****************************************************************************
                              Disk Functions
****************************************************************************}

Function DiskFree(Drive: Byte): int64;
Begin
End;



Function DiskSize(Drive: Byte): int64;
Begin
End;


Function GetCurrentDir : String;
begin
  GetDir(0,Result);
end;


Function SetCurrentDir (Const NewDir : String) : Boolean;
begin
  {$I-}
   ChDir(NewDir);
  {$I+}
  result := (IOResult = 0);
end;


Function CreateDir (Const NewDir : String) : Boolean;
begin
  {$I-}
   MkDir(NewDir);
  {$I+}
  result := (IOResult = 0);
end;


Function RemoveDir (Const Dir : String) : Boolean;
begin
  {$I-}
   RmDir(Dir);
  {$I+}
  result := (IOResult = 0);
end;


function DirectoryExists (const Directory: string): boolean;
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


Procedure InitAnsi;
Var
  i : longint;
begin
  {  Fill table entries 0 to 127  }
  for i := 0 to 96 do
    UpperCaseTable[i] := chr(i);
  for i := 97 to 122 do
    UpperCaseTable[i] := chr(i - 32);
  for i := 123 to 191 do
    UpperCaseTable[i] := chr(i);
  Move (CPISO88591UCT,UpperCaseTable[192],SizeOf(CPISO88591UCT));

  for i := 0 to 64 do
    LowerCaseTable[i] := chr(i);
  for i := 65 to 90 do
    LowerCaseTable[i] := chr(i + 32);
  for i := 91 to 191 do
    LowerCaseTable[i] := chr(i);
  Move (CPISO88591LCT,UpperCaseTable[192],SizeOf(CPISO88591UCT));
end;


Procedure InitInternational;
begin
  InitAnsi;
end;

function SysErrorMessage(ErrorCode: Integer): String;

begin
  Str(Errorcode,Result);
  Result:='Error '+Result;
end;

{****************************************************************************
                              OS utility functions
****************************************************************************}

Function GetEnvironmentVariable(Const EnvVar : String) : String;

begin
  Result:=StrPas(beos.Getenv(PChar(EnvVar)));
end;


{****************************************************************************
                              Initialization code
****************************************************************************}

Initialization
  InitExceptions;       { Initialize exceptions. OS independent }
  InitInternational;    { Initialize internationalization settings }
Finalization
  DoneExceptions;
end.
{
  $Log$
  Revision 1.5  2003-03-29 15:16:26  hajny
    * dummy DirectoryExists added

  Revision 1.4  2003/01/08 21:56:54  marco
   * small fixes to prototypes to compile it

  Revision 1.3  2002/09/07 16:01:17  peter
    * old logs removed and tabs fixed

}
