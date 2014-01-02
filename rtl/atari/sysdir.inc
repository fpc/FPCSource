{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    FPC Pascal system unit for Amiga.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
                           Directory Handling
*****************************************************************************}
procedure DosDir(func:byte;const s:string);
var
  buffer : array[0..255] of char;
  c : word;
begin
  move(s[1],buffer,length(s));
  buffer[length(s)]:=#0;
  DoDirSeparators(pchar(@buffer));
  c:=word(func);
  asm
        move.l  d2,d6      { save d2 }
        movem.l d3/a2/a3,-(sp)
        pea     buffer
        move.w  c,-(sp)
        trap    #1
        add.l   #6,sp
        move.l  d6,d2       { restore d2 }
        movem.l (sp)+,d3/a2/a3
        tst.w   d0
        beq     @dosdirend
        move.w  d0,errno
     @dosdirend:
  end;
  if errno <> 0 then
     Error2InOut;
end;


procedure mkdir(const s : string);[IOCheck];
begin
  If InOutRes <> 0 then exit;
  DosDir($39,s);
end;


procedure rmdir(const s : string);[IOCheck];
begin
  If InOutRes <> 0 then exit;
  DosDir($3a,s);
end;


procedure chdir(const s : string);[IOCheck];
begin
  If InOutRes <> 0 then exit;
  DosDir($3b,s);
end;


function GetDirIO (DriveNr: byte; var Dir: ShortString): word;
                                               [public, alias: 'FPC_GETDIRIO'];
var
  temp : array[0..255] of char;
  i    : longint;
  j: byte;
  drv: word;
begin
  GetDirIO := 0;
  drv:=word(drivenr);
  asm
            move.l  d2,d6      { save d2 }
            movem.l d3/a2/a3,-(sp)

            { Get dir from drivenr : 0=default, 1=A etc... }
            move.w drv,-(sp)

            { put (previously saved) offset in si }
{            move.l temp,-(sp)}
             pea   temp

            { call attos function 47H : Get dir }
            move.w #$47,-(sp)

            { make the call }
            trap   #1
            add.l  #8,sp

            move.l d6,d2         { restore d2 }
            movem.l (sp)+,d3/a2/a3
  end;
  { conversion to pascal string }
  i:=0;
  while (temp[i]<>#0) do
   begin
     if temp[i] in AllowDirectorySeparators then
       temp[i]:=DirectorySeparator;
     dir[i+3]:=temp[i];
     inc(i);
   end;
  dir[2]:=':';
  dir[3]:='\';
  dir[0]:=char(i+2);
{ upcase the string (FPC Pascal function) }
  dir:=upcase(dir);
  if drivenr<>0 then   { Drive was supplied. We know it }
   dir[1]:=chr(65+drivenr-1)
  else
   begin
      asm
        move.l  d2,d6      { save d2 }
        movem.l d3/a2/a3,-(sp)
        move.w #$19,-(sp)
        trap   #1
        add.l  #2,sp
        move.w d0,drv
        move.l d6,d2        { restore d2 }
        movem.l (sp)+,d3/a2/a3
     end;
     dir[1]:=chr(byte(drv)+ord('A'));
   end;
end;

procedure GetDir (DriveNr: byte; var Dir: ShortString);

begin
end;

procedure do_mkdir(const s : rawbytestring);
var
  tmpStr : rawbytestring;
  tmpLock: LongInt;
begin
  checkCTRLC;
  tmpStr:=PathConv(s);
  tmpLock:=dosCreateDir(pchar(tmpStr));
  if tmpLock=0 then begin
    dosError2InOut(IoErr);
    exit;
  end;
  UnLock(tmpLock);
end;

procedure do_rmdir(const s : rawbytestring);
var
  tmpStr : rawbytestring;
begin
  checkCTRLC;
  if (s='.') then
    begin
      InOutRes:=16;
      exit;
    end;
  tmpStr:=PathConv(s);
  if not dosDeleteFile(pchar(tmpStr)) then
    dosError2InOut(IoErr);
end;

procedure do_ChDir(const s: rawbytestring);
var
  tmpStr : rawbytestring;
  tmpLock: LongInt;
  FIB    : PFileInfoBlock;
begin
  checkCTRLC;
  tmpStr:=PathConv(s);
  tmpLock:=0;

  { Changing the directory is a pretty complicated affair }
  {   1) Obtain a lock on the directory                   }
  {   2) CurrentDir the lock                              }
  tmpLock:=Lock(pchar(tmpStr),SHARED_LOCK);
  if tmpLock=0 then begin
    dosError2InOut(IoErr);
    exit;
  end;

  FIB:=nil;
  new(FIB);

  if (Examine(tmpLock,FIB)=True) and (FIB^.fib_DirEntryType>0) then begin
    tmpLock:=CurrentDir(tmpLock);
    if AOS_OrigDir=0 then begin
      AOS_OrigDir:=tmpLock;
      tmpLock:=0;
    end;
  end;

  if tmpLock<>0 then Unlock(tmpLock);
  if assigned(FIB) then dispose(FIB);
end;

procedure do_GetDir (DriveNr: byte; var Dir: RawByteString);
var tmpbuf: array[0..255] of char;
begin
  checkCTRLC;
  Dir:='';

  if not GetCurrentDirName(tmpbuf,256) then
    dosError2InOut(IoErr)
  else
    begin
      Dir:=tmpbuf;
      SetCodePage(Dir,DefaultFileSystemCodePage,false);
    end;
end;
