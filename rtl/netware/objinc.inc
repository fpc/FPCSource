{ 2001/04/16 armin: first version for netware }

{$i errno.inc}
{$i nwsys.inc}

FUNCTION errno : LONGINT;
BEGIN
  errno := __get_errno_ptr^;
END;

FUNCTION FileOpen (Var FileName: AsciiZ; Mode: Word): THandle;
VAR NWMode : longint;
BEGIN
  NWMode:=0;
  if Mode=stCreate then
  Begin
     NWMode:=O_Creat;
     NWMode:=NWMode or O_RdWr;
  end
  else
   Begin
     Case (Mode and 3) of
      0 : NWMode:=NWMode or O_RdOnly;
      1 : NWMode:=NWMode or O_WrOnly;
      2 : NWMode:=NWMode or O_RdWr;
     end;
   end;
  FileOpen:=_open (pchar(@FileName[0]),NWMode,0);
  If FileOpen=-1 then FileOpen:=0;
  DosStreamError:=Errno;
END;

FUNCTION FileRead (Handle: THandle; Var BufferArea; BufferLength: Sw_Word;
Var BytesMoved: Sw_Word): Word;
BEGIN
  BytesMoved:=_read (Handle,@BufferArea,BufferLength);
  IF BytesMoved = -1 THEN
  BEGIN
    DosStreamError:=Errno;
    FileRead:=Errno;
  END ELSE
  BEGIN
    DosStreamError:=0;
    FileRead:=0;
  END;
END;

FUNCTION FileWrite (Handle:  THandle; Var BufferArea; BufferLength: Sw_Word;
Var BytesMoved: Sw_Word): Word;
BEGIN
  BytesMoved:=_write (Handle,@BufferArea,BufferLength);
  IF BytesMoved = -1 THEN
  BEGIN
    DosStreamError:=Errno;
    FileWrite:=Errno;
  END ELSE
  BEGIN
    DosStreamError:=0;
    FileWrite:=0;
  END;
END;

FUNCTION SetFilePos (Handle: THandle; Pos: LongInt; MoveType: Word;
VAR NewPos: LongInt): Word;

BEGIN
  NewPos:=_lseek (Handle,Pos,MoveType);
  IF NewPos = -1 THEN
    SetFilePos:=Errno
  ELSE
    SetFilePos := 0;
END;

FUNCTION FileClose (Handle: THandle): Word;
BEGIN
  _Close (Handle);
  DosStreamError:=Errno;
  FileClose := Errno;
END;

FUNCTION SetFileSize (Handle: THandle; FileSize: LongInt): Word;
BEGIN
  if _chsize (Handle, FileSize) = -1 then
    SetFileSize := Errno
  else
    SetFileSize := 0;
END;
