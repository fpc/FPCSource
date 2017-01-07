{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2017 by the Free Pascal development team.

    DOS unit for BP7 compatible RTL, Atari implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit dos;

interface


type
  SearchRec = record
    { Replacement for Fill }
    Fill: Array[1..21] of Byte; {future use}
    {End of replacement for fill}
    Attr : BYTE;        {attribute of found file}
    Time : LongInt;     {last modify date of found file}
    Size : LongInt;     {file size of found file}
    Name : String[255]; {name of found file}
  end;

{$i dosh.inc}

implementation

{$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)
{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)

{$i dos.inc}


function DosVersion: Word;
begin
  DosVersion:=0;
end;

procedure GetDate(Var Year, Month, MDay, WDay: Word);
begin
end;

procedure SetDate(Year, Month, Day: Word);
begin
end;

procedure GetTime(Var Hour, Minute, Second, Sec100: Word);
begin
end;

procedure SetTime(Hour, Minute, Second, Sec100: Word);
begin
end;

procedure Exec(const Path: PathStr; const ComLine: ComStr);
begin
end;

function DiskFree(Drive: Byte): Int64;
begin
  DiskFree:=-1;
end;

function DiskSize(Drive: Byte): Int64;
begin
  DiskSize:=-1;
end;

procedure FindFirst(const Path: PathStr; Attr: Word; Var f: SearchRec);
begin
end;

procedure FindNext(Var f: SearchRec);
begin
end;

procedure FindClose(Var f: SearchRec);
begin
end;

function FSearch(path: PathStr; dirlist: String) : PathStr;
begin
  FSearch:='';
end;

procedure GetFAttr(var f; var Attr : word);
begin
end;

procedure GetFTime(var f; var Time : longint);
begin
end;

procedure SetFAttr(var f; attr : word);
begin
end;

procedure SetFTime(var f; time : longint);
begin
end;

function EnvCount: Longint;
begin
  EnvCount:=0;
end;

function EnvStr(Index: LongInt): String;
begin
  EnvStr:='';
end;

function GetEnv(envvar : String): String;
begin
  GetEnv:='';
end;


end.
