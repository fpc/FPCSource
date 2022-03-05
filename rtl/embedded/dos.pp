{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by the Free Pascal Compiler development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Dos;

interface

{$MODE objfpc}

type
  SearchRec = Packed Record
    AnchorPtr : Pointer;    { Pointer to the Anchorpath structure }
    Fill: Array[1..15] of Byte; {future use}
    {End of replacement for fill}
    Attr : BYTE;        {attribute of found file}
    Time : LongInt;     {last modify date of found file}
    Size : LongInt;     {file size of found file}
    Name : String[255]; {name of found file}
  End;

{$I dosh.inc}

implementation

{$define HAS_GETMSCOUNT}

{$I dos.inc}


{******************************************************************************
                        --- Info / Date / Time ---
******************************************************************************}

function DosVersion: Word;
begin
  result := 0;
end;

procedure GetDate(Var Year, Month, MDay, WDay: Word);
begin
  Year := 0;
  Month := 0;
  MDay := 0;
  WDay := 0;
end;

procedure SetDate(Year, Month, Day: Word);
begin
end;

procedure GetTime(Var Hour, Minute, Second, Sec100: Word);
begin
  Hour := 0;
  Minute := 0;
  Second := 0;
  Sec100 := 0;
end;

Procedure SetTime(Hour, Minute, Second, Sec100: Word);
begin
end;

function GetMsCount: int64;
begin
  result := 0;
end;

{******************************************************************************
                               --- Exec ---
******************************************************************************}
procedure Exec(const Path: PathStr; const ComLine: ComStr);
begin
end;


{******************************************************************************
                               --- Disk ---
******************************************************************************}

Function DiskFree(Drive: Byte): int64;
Begin
  result := -1;
end;


Function DiskSize(Drive: Byte): int64;
Begin
  result := -1;
end;


procedure FindFirst(const Path: PathStr; Attr: Word; Var f: SearchRec);
begin
  DosError:=18;
end;


procedure FindNext(Var f: SearchRec);
begin
  DosError:=18;
end;


procedure FindClose(Var f: SearchRec);
begin
  DosError:=18;
end;


{******************************************************************************
                               --- File ---
******************************************************************************}


function FSearch(path: PathStr; dirlist: String) : PathStr;
begin
  result := '';
end;


Procedure getftime (var f; var time : longint);
begin
end;


Procedure setftime(var f; time : longint);
Begin
End;

procedure getfattr(var f; var attr : word);
begin
End;


procedure setfattr(var f; attr : word);
begin
end;


{******************************************************************************
                             --- Environment ---
******************************************************************************}


function EnvCount: Longint;
begin
  result := -1;
end;


function EnvStr(Index: LongInt): String;
begin
  result := '';
end;

function GetEnv(envvar : String): String;
begin
  result := '';
end;


end.
