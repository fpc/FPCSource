{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Free Pascal development team

    Low level directory functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
                           Directory Handling
*****************************************************************************}
Procedure do_MkDir(const s: rawbytestring);
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

Procedure do_RmDir(const s: rawbytestring);
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

Procedure do_ChDir(const s: rawbytestring);
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
    if MOS_OrigDir=0 then begin
      MOS_OrigDir:=tmpLock;
      tmpLock:=0;
    end;
  end else begin
    dosError2InOut(ERROR_DIR_NOT_FOUND);
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
