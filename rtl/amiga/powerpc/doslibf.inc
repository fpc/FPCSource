{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Karoly Balogh

    dos.library functions for AmigaOS 4.x/PowerPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function Open(name: PChar; accessMode: longint): longint; syscall IDOS 76;
function dosClose(_file: longint): boolean; syscall IDOS 80;
function dosRead(_file: longint; buffer: Pointer; length: longint): longint; syscall IDOS 84;
function dosWrite(_file: longint; buffer: Pointer; length: longint): longint; syscall IDOS 88;
function dosInput: longint; syscall IDOS 92;
function dosOutput: longint; syscall IDOS 96;
function dosSeek(_file: longint;position: longint;offset: longint): longint; syscall IDOS 100;
function dosDeleteFile(name: PChar): boolean; syscall IDOS 104;
function dosRename(oldName: PChar;newName: PChar): boolean; syscall IDOS 108;
function Lock(name: PChar;_type: longint): longint; syscall IDOS 112;
procedure UnLock(lock: longint); syscall IDOS 116;

function Examine(lock: longint; fileInfoBlock: PFileInfoBlock): boolean; syscall IDOS 124;

function Info(lock: longint; parameterBlock: PInfoData): boolean; syscall IDOS 132;
function dosCreateDir(name: PChar): longint; syscall IDOS 136;
function CurrentDir(lock: longint): longint; syscall IDOS 140;
function IoErr: longint; syscall IDOS 144;

function SetProtection(name: PChar; protect: longword): boolean; syscall IDOS 180;
function DateStamp(date: PDateStamp): PDateStamp; syscall IDOS 184;

function SetFileDate(name: PChar; date: PDateStamp): boolean; syscall IDOS 356;
function NameFromLock(lock: longint; buffer: PChar; len: longint): boolean; syscall IDOS 360;
function NameFromFH(fh: longint; buffer: PChar; len: longint): longint; syscall IDOS 364;

function SetFileSize(fh: longint; pos: longint; mode: longint): longint; syscall IDOS 396;

function GetArgStr: PChar; syscall IDOS 456;
function SetArgStr(_string: PChar): PChar; syscall IDOS 460;

function GetCurrentDirName(buf: PChar; len: longint): boolean; syscall IDOS 476;

function GetProgramName(buf: PChar; len: longint): boolean; syscall IDOS 484;

function GetProgramDir: longint; syscall IDOS 500;
function SystemTagList(command: PChar; tags: PTagItem): longint; syscall IDOS 504;

function LockDosList(flags: longword): PDosList; syscall IDOS 544;
procedure UnLockDosList(flags: longword); syscall IDOS 548;

function NextDosEntry(dlist: PDosList; flags: longword): PDosList; syscall IDOS 568;

function MatchFirst(pat: PChar; anchor: PAnchorPath): longint; syscall IDOS 664;
function MatchNext(anchor: PAnchorPath): longint; syscall IDOS 668;
procedure MatchEnd(anchor: PAnchorPath); syscall IDOS 672;

function GetVar(name: PChar; buffer: PChar; size: longint; flags: longword): longint; syscall IDOS 720;
