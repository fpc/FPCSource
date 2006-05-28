{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by Karoly Balogh for Genesi S.a.r.l.

    Heavily based on the Commodore Amiga/m68k RTL by Nils Sjoholm and
    Carl Eric Codere

    MorphOS port was done on a free Pegasos II/G4 machine
    provided by Genesi S.a.r.l. <www.genesi.lu>
    
    This unit is based on the MorphOS one and is adapted for Gameboy Advance
    simply by stripping out all stuff inside funcs and procs. 
    Copyright (c) 2006 by Francesco Lombardi
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Dos;

interface

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

{$I dos.inc}

{******************************************************************************
                           --- Internal routines ---
******************************************************************************}

function dosLock(const name: String; accessmode: Longint) : LongInt;
begin
end;

function IsLeapYear(Source : Word) : Boolean;
begin
end;

function dosSetProtection(const name: string; mask:longint): Boolean;
begin
end;

function dosSetFileDate(name: string): Boolean;
begin
end;


{******************************************************************************
                        --- Info / Date / Time ---
******************************************************************************}

function DosVersion: Word;
begin
end;

procedure NewList ();
begin
end;

function CreateExtIO (size: Longint): integer;
begin
end;

procedure DeleteExtIO ();
begin
end;

function Createport(name : PChar; pri : longint): integer;
begin
end;

procedure DeletePort ();
begin
end;


function Create_Timer(theUnit : longint) : integer;
begin
end;

Procedure Delete_Timer();
begin
end;

function set_new_time(secs, micro : longint): longint;
begin
end;

function get_sys_time(): longint;
begin
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


Procedure SetTime(Hour, Minute, Second, Sec100: Word);
begin
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
end;



Function DiskSize(Drive: Byte): int64;
Begin
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


{******************************************************************************
                               --- File ---
******************************************************************************}


function FSearch(path: PathStr; dirlist: String) : PathStr;
begin
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

function getpathstring: string;
begin
end;


function EnvCount: Longint;
begin
end;


function EnvStr(Index: LongInt): String;
begin
end;



function GetEnv(envvar : String): String;
begin
end;


procedure AddDevice(str : String);
begin
end;

function MakeDeviceName(str : pchar): string;
begin
end;

function IsInDeviceList(str : string): boolean;
begin
end;

procedure ReadInDevices;
begin
end;

begin
//  DosError:=0;
//  numberofdevices := 0;
//  StrOfPaths := '';
//  ReadInDevices;
end.
