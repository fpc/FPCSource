{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Dos unit for BP7 compatible RTL - Interface declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF HAS_FILENAMELEN}
Const
  FileNameLen = 255;
{$ENDIF HAS_FILENAMELEN}

{$IFDEF CPUI386}
 {$IFNDEF HAS_REGISTERS}
type
  Registers = packed record
    case i : integer of
     0 : (ax,f1,bx,f2,cx,f3,dx,f4,bp,f5,si,f51,di,f6,ds,f7,es,f8,flags,fs,gs : word);
     1 : (al,ah,f9,f10,bl,bh,f11,f12,cl,ch,f13,f14,dl,dh : byte);
     2 : (eax, ebx, ecx, edx, ebp, esi, edi : longint);
    End;
 {$ENDIF HAS_REGISTERS}
{$ENDIF CPUI386}

Const
  {Bitmasks for CPU Flags}
  fcarry     = $0001;
  fparity    = $0004;
  fauxiliary = $0010;
  fzero      = $0040;
  fsign      = $0080;
  foverflow  = $0800;

  {Bitmasks for file attribute}
  readonly  = $01;
  hidden    = $02;
  sysfile   = $04;
  volumeid  = $08;
  directory = $10;
  archive   = $20;
  anyfile   = $3F;

  {File Status}
  fmclosed = $D7B0;
  fminput  = $D7B1;
  fmoutput = $D7B2;
  fminout  = $D7B3;


Type
{ Needed for LFN Support }
  ComStr  = String[FileNameLen];
  PathStr = String[FileNameLen];
  DirStr  = String[FileNameLen];
  NameStr = String[FileNameLen];
  ExtStr  = String[FileNameLen];

  DateTime = packed record
    Year,
    Month,
    Day,
    Hour,
    Min,
    Sec   : word;
  End;

{$ifdef FPC_HAS_FEATURE_THREADING}
threadvar
{$else FPC_HAS_FEATURE_THREADING}
var
{$endif FPC_HAS_FEATURE_THREADING}
  DosError : integer;

{$ifdef cpui386}
{Interrupt}
Procedure Intr(intno: byte; var regs: registers);
Procedure MSDos(var regs: registers);
{$endif cpui386}

{Info/Date/Time}
Function  DosVersion: Word;
Procedure GetDate(var year, month, mday, wday: word);
Procedure GetTime(var hour, minute, second, sec100: word);
procedure SetDate(year,month,day: word);
Procedure SetTime(hour,minute,second,sec100: word);
Procedure UnpackTime(p: longint; var t: datetime);
Procedure PackTime(var t: datetime; var p: longint);

{Exec}
Procedure Exec(const path: pathstr; const comline: comstr);
Function  DosExitCode: word;

{Disk}
Function  DiskFree(drive: byte) : int64;
Function  DiskSize(drive: byte) : int64;
Procedure FindFirst(const path: pathstr; attr: word; var f: searchRec);
Procedure FindNext(var f: searchRec);
Procedure FindClose(Var f: SearchRec);

{File}
Procedure GetFAttr(var f; var attr: word);
Procedure GetFTime(var f; var time: longint);
Function  FSearch(path: pathstr; dirlist: string): pathstr;
Function  FExpand(const path: pathstr): pathstr;
Procedure FSplit(path: pathstr; var dir: dirstr; var name: namestr; var ext: extstr);
function  GetShortName(var p : String) : boolean;
function  GetLongName(var p : String) : boolean;

{Environment}
Function  EnvCount: longint;
Function  EnvStr (Index: longint): string;
Function  GetEnv(envvar: string): string;

{Misc}
Procedure SetFAttr(var f; attr: word);
Procedure SetFTime(var f; time: longint);
Procedure GetCBreak(var breakvalue: boolean);
Procedure SetCBreak(breakvalue: boolean);
Procedure GetVerify(var verify: boolean);
Procedure SetVerify(verify: boolean);

{Do Nothing Functions}
Procedure SwapVectors;
Procedure GetIntVec(intno: byte; var vector: {$ifdef cpui8086}farpointer{$else}pointer{$endif});
Procedure SetIntVec(intno: byte; vector: {$ifdef cpui8086}farpointer{$else}pointer{$endif});
Procedure Keep(exitcode: word);

{Additional (non-TP) function for quick access to system timer - for FV etc.}
Function  GetMsCount: int64;
{Offset between two return values corresponds to amount of milliseconds
 between the two calls, however the exact value is not guaranteed to have
 particular meaning - it can be e.g. amount of milliseconds since computer
 startup on DOS-like x86 platforms, derived from Unix time on Unix etc.}


