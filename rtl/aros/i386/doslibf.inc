{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Marcus Sackrow

    dos.library functions for AROS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function Open(name: PChar; accessMode: longint): Cardinal; syscall AOS_DOSBase 5;
function dosClose(_file: Cardinal): LongBool;  syscall AOS_DOSBase 6;
function dosRead(_file: Cardinal; buffer: Pointer; length: longint): longint; syscall AOS_DOSBase 7;
function dosWrite(_file: Cardinal; buffer: Pointer; length: longint): longint;  syscall AOS_DOSBase 8;
function dosInput: Cardinal; syscall AOS_DOSBase 9;
function dosOutput: Cardinal; syscall AOS_DOSBase 10;
function DosError1: Cardinal;  syscall AOS_DOSBase 142;
function dosSeek(_file: longint;position: longint;offset: longint): longint; syscall AOS_DOSBase 11;
function dosDeleteFile(const name: PChar): LongBool; syscall AOS_DOSBase 12;
function dosRename(const oldName: PChar;newName: PChar): LongInt; syscall AOS_DOSBase 13;
function Lock(const name: PChar;_type: longint): longint; syscall AOS_DOSBase 14;
function UnLock(lock: longint): longint; syscall AOS_DOSBase 15;
function DupLock(Lock: longint): longint; syscall AOS_DOSBase 16;
function Examine(lock: longint; fileInfoBlock: PFileInfoBlock): LongInt; syscall AOS_DOSBase 17;
function Info(lock: longint; parameterBlock: PInfoData): LongInt; syscall AOS_DOSBase 19;
function dosCreateDir(const name: PChar): longint; syscall AOS_DOSBase 20;
function CurrentDir(lock: longint): longint; syscall AOS_DOSBase 21;
function IoErr: longint; syscall AOS_DOSBase 22;
procedure dosExit(ErrCode: longint); syscall AOS_DOSBase 24;
function SetProtection(const name: PChar; protect: longword): LongInt; syscall AOS_DOSBase 31;
function DateStamp(date: PDateStamp): PDateStamp; syscall AOS_DOSBase 32;
procedure Delay(ticks: LongWord); syscall AOS_DOSBase 33;
function AllocDosObject(Type_: LongWord; const Tags: PTagItem): Pointer; syscall AOS_DOSBase 38;
procedure FreeDosObject(Type_: LongWord; Ptr: Pointer); syscall AOS_DOSBase 39;
function SetFileDate(name: PChar; date: PDateStamp): LongBool; syscall AOS_DOSBase 66;
function NameFromLock(lock: longint; buffer: PChar; len: longint): LongBool; syscall AOS_DOSBase 67;
function SetFileSize(fh: longint; pos: longint; mode: longint): longint; syscall AOS_DOSBase 76;
function Cli : pCommandLineInterface; syscall AOS_DOSBase 82;
function CreateNewProc(const Tags: PTagItem): PProcess; syscall AOS_DOSBase 83;
function GetArgStr: PChar; syscall AOS_DOSBase 89;
function GetCurrentDirName(buf: PChar; len: longint): LongBool; syscall AOS_DOSBase 94;
function GetProgramName(buf: PChar; len: longint): LongBool; syscall AOS_DOSBase 96;
function GetProgramDir: longint; syscall AOS_DOSBase 100;
function SystemTagList(command: PChar; tags: PTagItem): longint; syscall AOS_DOSBase 101;
function LockDosList(flags: longword): PDosList; syscall AOS_DOSBase 109;
procedure UnLockDosList(flags: longword); syscall AOS_DOSBase 110;
function NextDosEntry(dlist: PDosList; flags: longword): PDosList; syscall AOS_DOSBase 115;
function MatchFirst(pat: PChar; anchor: PAnchorPath): longint; syscall AOS_DOSBase 137;
function MatchNext(anchor: PAnchorPath): longint; syscall AOS_DOSBase 138;
procedure MatchEnd(anchor: PAnchorPath); syscall AOS_DOSBase 139;
function GetVar(name: PChar; buffer: PChar; size: longint; flags: longword): longint; syscall AOS_DOSBase 151;
function ExamineFH(Fh: BPTR; Fib: PFileInfoBlock): LongBool; syscall AOS_DOSBase 65;
function NameFromFH(Fh: BPTR; Buffer: PChar; Length: LongInt): LongBool; syscall AOS_DOSBase 68;

