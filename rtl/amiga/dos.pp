{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by Nils Sjoholm
    members of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


    {
       History:
       10.02.1998  First version for Amiga.
                   Just GetDate and GetTime.

       11.02.1998  Added AmigaToDt and DtToAmiga
                   Changed GetDate and GetTime to
                   use AmigaToDt and DtToAmiga.

                   Added DiskSize and DiskFree.
                   They are using a string as arg
                   have to try to fix that.

       12.02.1998  Added Fsplit and FExpand.
                   Cleaned up the unit and removed
                   stuff that was not used yet.

       13.02.1998  Added CToPas and PasToC and removed
                   the uses of strings.

       14.02.1998  Removed AmigaToDt and DtToAmiga
                   from public area.
                   Added deviceids and devicenames
                   arrays so now diskfree and disksize
                   is compatible with dos.
    }


Unit Dos;


Interface


Type
  ComStr  = String[255];  { size increased to be more compatible with Unix}
  PathStr = String[255];  { size increased to be more compatible with Unix}
  DirStr  = String[255];  { size increased to be more compatible with Unix}
  NameStr = String[255];  { size increased to be more compatible with Unix}
  ExtStr  = String[255];  { size increased to be more compatible with Unix}

  { If you need more devicenames just expand this two arrays }

  deviceids = (DF0ID, DF1ID, DF2ID, DF3ID, DH0ID, DH1ID,
               CD0ID, MDOS1ID, MDOS2ID);

       registers = record
         case i : integer of
            0 : (ax,f1,bx,f2,cx,f3,dx,f4,bp,f5,si,f51,di,f6,ds,f7,es,f8,flags,fs,gs : word);
            1 : (al,ah,f9,f10,bl,bh,f11,f12,cl,ch,f13,f14,dl,dh : byte);
            2 : (eax,  ebx,  ecx,  edx,  ebp,  esi,  edi : longint);
       end;


Const
  devicenames : array [DF0ID..MDOS2ID] of PChar = (
                'df0:','df1:','df2:','df3:','dh0:',
                'dh1:','cd0','A:','B:');

Type
  SearchRec = Record
    {Fill : array[1..21] of byte;  Fill replaced with below}

    SearchNum: LongInt; {to track which search this is}
    SearchPos: LongInt; {directory position}
    DirPtr: LongInt; {directory pointer for reading directory}
    SearchType: Byte;  {0=normal, 1=open will close}
    SearchAttr: Byte; {attribute we are searching for}
    Fill: Array[1..07] of Byte; {future use}
    {End of replacement for fill}

    Attr : Byte; {attribute of found file}
    Time : LongInt; {last modify date of found file}
    Size : LongInt; {file size of found file}
    Reserved : Word; {future use}
    Name : String[255]; {name of found file}
    SearchSpec: String[255]; {search pattern}
    NamePos: Word; {end of path, start of name position}
    End;


  FileRec = Record
    Handle : word;
    Mode : word;
    RecSize : word;
    _private : array[1..26] of byte;
    UserData: array[1..16] of byte;
    Name: array[0..255] of char;
    End;


  TextBuf = array[0..127] of char;


  TextRec = record
    handle : word;
    mode : word;
    bufSize : word;
    _private : word;
    bufpos : word;
    bufend : word;
    bufptr : ^textbuf;
    openfunc : pointer;
    inoutfunc : pointer;
    flushfunc : pointer;
    closefunc : pointer;
    userdata : array[1..16] of byte;
    name : array[0..255] of char;
    buffer : textbuf;
    End;


  DateTime = record
    Year: Word;
    Month: Word;
    Day: Word;
    Hour: Word;
    Min: Word;
    Sec: word;
    End;

    pClockData = ^tClockData;
    tClockData = Record
      sec   : Word;
      min   : Word;
      hour  : Word;
      mday  : Word;
      month : Word;
      year  : Word;
      wday  : Word;
    END;


Procedure GetDate(var year, month, mday, wday: word);
Procedure GetTime(var hour, minute, second, sec100: word);
Function  DosVersion: Word;
procedure SetDate(year,month,day: word);
Procedure SetTime(hour,minute,second,sec100: word);
Procedure GetCBreak(var breakvalue: boolean);
Procedure SetCBreak(breakvalue: boolean);
Procedure GetVerify(var verify: boolean);
Procedure SetVerify(verify: boolean);
Function  DiskFree(drive: byte) : longint;
Function  DiskSize(drive: byte) : longint;
Procedure FindFirst(const path: pathstr; attr: word; var f: searchRec);
Procedure FindNext(var f: searchRec);
Procedure FindClose(Var f: SearchRec);
Procedure SwapVectors;
Procedure MSDos(var regs: registers);
Procedure GetIntVec(intno: byte; var vector: pointer);
Procedure SetIntVec(intno: byte; vector: pointer);
Procedure Keep(exitcode: word);
Procedure Intr(intno: byte; var regs: registers);
Procedure GetFAttr(var f; var attr: word);
Procedure SetFAttr(var f; attr: word);
Procedure GetFTime(var f; var time: longint);
Procedure SetFTime(var f; time: longint);
Procedure UnpackTime(p: longint; var t: datetime);
Procedure PackTime(var t: datetime; var p: longint);
Function  FSearch(path: pathstr; dirlist: string): pathstr;
Function  FExpand(const path: pathstr): pathstr;
Procedure FSplit(path: pathstr; var dir: dirstr; var name: namestr;
  var ext: extstr);
Procedure Exec(const path: pathstr; const comline: comstr);
Function  DosExitCode: word;
Function  EnvCount: longint;
Function  EnvStr(index: integer): string;
Function  GetEnv (envvar: string): string;

Implementation


Type

    BPTR = Longint;

{$PACKRECORDS 4}

{ Returned by Examine() and ExInfo(), must be on a 4 byte boundary }

    pFileInfoBlock = ^tFileInfoBlock;
    tFileInfoBlock = record
        fib_DiskKey      : Longint;
        fib_DirEntryType : Longint;
                        { Type of Directory. If < 0, then a plain file.
                          If > 0 a directory }
        fib_FileName     : Array [0..107] of Char;
                        { Null terminated. Max 30 chars used for now }
        fib_Protection   : Longint;
                        { bit mask of protection, rwxd are 3-0. }
        fib_EntryType    : Longint;
        fib_Size         : Longint;      { Number of bytes in file }
        fib_NumBlocks    : Longint;      { Number of blocks in file }
        fib_Date         : tDateStamp;   { Date file last changed }
        fib_Comment      : Array [0..79] of Char;
                        { Null terminated comment associated with file }
        fib_OwnerUID     : Word;
        fib_OwnerGID     : Word;
        fib_Reserved     : Array [0..31] of Char;
    end;

{ returned by Info(), must be on a 4 byte boundary }

    pInfoData = ^tInfoData;
    tInfoData = record
        id_NumSoftErrors        : Longint;      { number of soft errors on disk
}
        id_UnitNumber           : Longint;      { Which unit disk is (was)
mounted on }
        id_DiskState            : Longint;      { See defines below }
        id_NumBlocks            : Longint;      { Number of blocks on disk }
        id_NumBlocksUsed        : Longint;      { Number of block in use }
        id_BytesPerBlock        : Longint;
        id_DiskType             : Longint;      { Disk Type code }
        id_VolumeNode           : BPTR;         { BCPL pointer to volume node }
        id_InUse                : Longint;      { Flag, zero if not in use }
    end;



{$PACKRECORDS NORMAL}



procedure CurrentTime(var Seconds, Micros : Longint); Assembler;
asm
    MOVE.L  A6,-(A7)
    MOVE.L  _IntuitionBase,A6
    MOVE.L  Seconds,a0
    MOVE.L  Micros,a1
    JSR -084(A6)
    MOVE.L  (A7)+,A6
end;


function Date2Amiga(date : pClockData) : Longint; Assembler;
asm
    MOVE.L  A6,-(A7)
    MOVE.L  _UtilityBase,A6
    MOVE.L  date,a0
    JSR -126(A6)
    MOVE.L  (A7)+,A6
end;


procedure Amiga2Date(amigatime : Longint;
                     resultat : pClockData); Assembler;
asm
    MOVE.L  A6,-(A7)
    MOVE.L  _UtilityBase,A6
    MOVE.L  amigatime,d0
    MOVE.L  resultat,a0
    JSR -120(A6)
    MOVE.L  (A7)+,A6
end;


function Examine(lock : BPTR;
                 info : pFileInfoBlock) : Boolean; Assembler;
asm
    MOVEM.L d2/a6,-(A7)
    MOVE.L  _DOSBase,A6
    MOVE.L  lock,d1
    MOVE.L  info,d2
    JSR -102(A6)
    MOVEM.L (A7)+,d2/a6
    TST.L   d0
    SNE     d0
    NEG.B   d0
end;


function Lock(name : Pchar;
              accessmode : Longint) : BPTR; Assembler;
asm
    MOVEM.L d2/a6,-(A7)
    MOVE.L  _DOSBase,A6
    MOVE.L  name,d1
    MOVE.L  accessmode,d2
    JSR -084(A6)
    MOVEM.L (A7)+,d2/a6
end;


procedure UnLock(lock : BPTR); Assembler;
asm
    MOVE.L  A6,-(A7)
    MOVE.L  _DOSBase,A6
    MOVE.L  lock,d1
    JSR -090(A6)
    MOVE.L  (A7)+,A6
end;


function Info(lock : BPTR;
              params : pInfoData) : Boolean; Assembler;
asm
    MOVEM.L d2/a6,-(A7)
    MOVE.L  _DOSBase,A6
    MOVE.L  lock,d1
    MOVE.L  params,d2
    JSR -114(A6)
    MOVEM.L (A7)+,d2/a6
    TST.L   d0
    SNE     d0
    NEG.B   d0
end;

function NameFromLock(Datei : BPTR;
                      Buffer : Pchar;
                      BufferSize : Longint) : Boolean; Assembler;
asm
    MOVEM.L d2/d3/a6,-(A7)
    MOVE.L  _DOSBase,A6
    MOVE.L  Datei,d1
    MOVE.L  Buffer,d2
    MOVE.L  BufferSize,d3
    JSR -402(A6)
    MOVEM.L (A7)+,d2/d3/a6
    TST.L   d0
    SNE     d0
    NEG.B   d0
end;


function PasToC(var s: string): Pchar;
var i: integer;
begin
    i := Length(s) + 1;
    if i > 255 then
    begin
        Delete(s, 255, 1);      { ensure there is a spare byte }
        Dec(i)
    end;
    s[i]     := #0;
    PasToC := @s[1]
end;

procedure CToPas(var s: string);
begin
    s[0] := #255;
    s[0] := Chr(Pos(#0, s) - 1)     { gives -1 (255) if not found }
end;


Function do_exec ( Commandline : pchar; tmp : integer) : integer;
begin
end;

Procedure Intr (intno: byte; var regs: registers);
  Begin
  { Does not apply to Linux - not implemented }
  End;


Var
  LastDosExitCode: word;

Procedure Exec (Const Path: PathStr; Const ComLine: ComStr);
  Begin
  End;


Function DosExitCode: Word;
  Begin
  End;


Function DosVersion: Word;
  Begin
  End;



Procedure GetDate(Var Year, Month, MDay, WDay: Word);
Var
  cd    : pClockData;
  mysec,
  tick  : Longint;
begin
  New(cd);
  CurrentTime(mysec,tick);
  Amiga2Date(mysec,cd);
  Year  := cd^.year;
  Month := cd^.month;
  MDay  := cd^.mday;
  WDay  := cd^.wday;
  Dispose(cd);
end;

Procedure SetDate(Year, Month, Day: Word);
  Begin
  { !! }
  End;


Procedure GetTime(Var Hour, Minute, Second, Sec100: Word);
Var
  mysec,
  tick    : Longint;
  cd      : pClockData;
begin
  New(cd);
  CurrentTime(mysec,tick);
  Amiga2Date(mysec,cd);
  Hour   := cd^.hour;
  Minute := cd^.min;
  Second := cd^.sec;
  Sec100 := 0;
  Dispose(cd);
END;

Procedure SetTime(Hour, Minute, Second, Sec100: Word);
  Begin
  { !! }
  End;


Procedure GetCBreak(Var BreakValue: Boolean);
  Begin
  { Not implemented for Linux, but set to true as a precaution. }
  breakvalue:=true
  End;


Procedure SetCBreak(BreakValue: Boolean);
  Begin
  { ! No Linux equivalent ! }
  End;


Procedure GetVerify(Var Verify: Boolean);
   Begin
   { Not implemented for Linux, but set to true as a precaution. }
   verify:=true;
   End;


Procedure SetVerify(Verify: Boolean);
  Begin
  { ! No Linux equivalent ! }
  End;


Function DiskFree(Drive: Byte): Longint;
Var
  MyLock      : BPTR;
  Inf         : pInfoData;
  Free        : Longint;
Begin
  Free := -1;
  New(Inf);
  MyLock := Lock(devicenames[Drive],-2);
  If MyLock <> NIL then begin
     if Info(MyLock,Inf) then begin
        Free := (Inf^.id_NumBlocks * Inf^.id_BytesPerBlock) -
                (Inf^.id_NumBlocksUsed * Inf^.id_BytesPerBlock);
     end;
     Unlock(MyLock);
  end;
  Dispose(Inf);
  diskfree := Free;
end;



Function DiskSize(Drive: Byte): Longint;
Var
  MyLock      : BPTR;
  Inf         : pInfoData;
  Size        : Longint;
Begin
  Size := -1;
  New(Inf);
  MyLock := Lock(devicenames[Drive],-2);
  If MyLock <> NIL then begin
     if Info(MyLock,Inf) then begin
        Size := (Inf^.id_NumBlocks * Inf^.id_BytesPerBlock);
     end;
     Unlock(MyLock);
  end;
  Dispose(Inf);
  disksize := Size;
end;



Procedure FindClose(Var f: SearchRec);
  Begin
  End;


Function FNMatch(Var Pattern: PathStr; Var Name: PathStr): Boolean;
  Begin {start FNMatch}
  End;



Procedure FindWorkProc(Var f: SearchRec);
  Begin
  End;


Function  FindLastUsed: Word;
  Begin
  End;


Procedure FindFirst(Const Path: PathStr; Attr: Word; Var f: SearchRec);
  Begin
  End;


Procedure FindNext(Var f: SearchRec);
  Begin
  End;



Procedure SwapVectors;
  Begin
  { Does not apply to Linux - Do Nothing }
  End;


Function EnvCount: Longint;

  Begin
  End;


Function EnvStr(Index: Integer): String;
  Begin
  End;


Function GetEnv(EnvVar: String): String;
  Begin
  End;

Procedure FSplit(Path: PathStr; Var Dir: DirStr; Var Name: NameStr;
var
  I: Word;
begin
  I := Length(Path);
  while (I > 0) and not ((Path[I] = '/') or (Path[I] = ':')) do Dec(I);
  if Path[I] = '/' then
     dir := Copy(Path, 0, I-1)
  else dir := Copy(Path,0,I);

  if Length(Path) > Length(dir) then
     name := Copy(Path, I + 1, Length(Path)-I)
     else name := '';

  I := Pos('.',Path);
  if I > 0 then
     ext := Copy(Path,I,Length(Path)-(I-1))
     else ext := '';
end;

Function FExpand(Const Path: PathStr): PathStr;
var
    FLock  : BPTR;
    buffer : PathStr;
begin
    FLock := Lock(PasToC(Path),-2);
    if FLock <> NIL then begin
       if NameFromLock(FLock,PasToC(buffer),255) then begin
          CToPas(buffer);
          Unlock(FLock);
          FExpend := buffer;
       end else begin
          Unlock(FLock);
          FExpand := '';
       end;
    end else FExpand := '';
end;







Procedure msdos(var regs : registers);
  Begin
  { ! Not implemented in Linux ! }
  End;


Procedure getintvec(intno : byte;var vector : pointer);
  Begin
  { ! Not implemented in Linux ! }
  End;


Procedure setintvec(intno : byte;vector : pointer);
  Begin
  { ! Not implemented in Linux ! }
  End;


Procedure keep(exitcode : word);
  Begin
  { ! Not implemented in Linux ! }
  End;


Procedure getfattr(var f; var attr : word);
  Begin
  End;


Procedure setfattr (var f;attr : word);
  Begin
  { ! Not implemented in Linux ! }
  End;


Procedure getftime (var f; var time : longint);
{
    This function returns a file's date and time as the number of
    seconds after January 1, 1978 that the file was created.
}
var
    FInfo : pFileInfoBlock;
    FTime : Longint;
    FLock : Longint;
begin
    FTime := 0;
    FLock := Lock(PasToC(filerec(f).name), -2);
    IF FLock <> NIL then begin
        New(FInfo);
        if Examine(FLock, FInfo) then begin
             with FInfo^.fib_Date do
             FTime := ds_Days * (24 * 60 * 60) +
             ds_Minute * 60 +
             ds_Tick div 50;
        end else begin
             FTime := 0;
        end;
        Unlock(FLock);
        Dispose(FInfo);
    end;
    time := FTime;
end;


Procedure setftime(var f; time : longint);
  Begin
  { ! Not implemented in Linux ! }
  End;


Procedure unpacktime(p : longint;var t : datetime);
Begin
  AmigaToDt(p,t);
End;


Procedure packtime(var t : datetime;var p : longint);
Begin
  p := DtToAmiga(t);
end;

Function  fsearch(path : pathstr;dirlist : string) : pathstr;
  Begin
  End;


Procedure AmigaToDt(SecsPast: LongInt; Var Dt: DateTime);
var
  cd : pClockData;
Begin
  New(cd);
  Amiga2Date(SecsPast,cd);
  Dt.sec   := cd^.sec;
  Dt.min   := cd^.min;
  Dt.hour  := cd^.hour;
  Dt.day   := cd^.mday;
  Dt.month := cd^.month;
  Dt.year  := cd^.year;
  Dispose(cd);
End;

Function DtToAmiga(DT: DateTime): LongInt;
var
  cd : pClockData;
  temp : Longint;
Begin
  New(cd);
  cd^.sec   := Dt.sec;
  cd^.min   := Dt.min;
  cd^.hour  := Dt.hour;
  cd^.mday  := Dt.day;
  cd^.month := Dt.month;
  cd^.year  := Dt.year;
  temp := Date2Amiga(cd);
  Dispose(cd);
  DtToAmiga := temp;
end;


End.









