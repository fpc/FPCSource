{ For linux we 'steal' the following from system unit, this way
  we don't need to change the system unit interface. }

Var errno : Longint;

{$i sysnr.inc}
{$i errno.inc}
{$i sysconst.inc}
{$i systypes.inc}
{$i syscalls.inc}

FUNCTION FileOpen (Var FileName: AsciiZ; Mode: Word): THandle;

Var LinuxMode : longint;

BEGIN
  LinuxMode:=0;
  if Mode=stCreate then
  Begin
     LinuxMode:=Open_Creat;
     LinuxMode:=LinuxMode or Open_RdWr;
  end
  else
   Begin
     Case (Mode and 3) of
      0 : LinuxMode:=LinuxMode or Open_RdOnly;
      1 : LinuxMode:=LinuxMode or Open_WrOnly;
      2 : LinuxMode:=LinuxMode or Open_RdWr;
     end;
   end;
  FileOpen:=SYS_Open (pchar(@FileName[0]),LinuxMode,438 {666 octal});
  If FileOpen=-1 then FileOpen:=0;
  DosStreamError:=Errno;
END;

FUNCTION FileRead (Handle: THandle; Var BufferArea; BufferLength: Sw_Word;
Var BytesMoved: Sw_Word): Word;
BEGIN
  BytesMoved:=Sys_read (Handle,Pchar(@BufferArea),BufferLength);
  DosStreamError:=Errno;
  FileRead:=Errno;
END;

FUNCTION FileWrite (Handle:  THandle; Var BufferArea; BufferLength: Sw_Word;
Var BytesMoved: Sw_Word): Word;
BEGIN
  BytesMoved:=Sys_Write (Handle,Pchar(@BufferArea),BufferLength);
  FileWrite:=Errno;
  DosStreamError:=Errno;
END;

FUNCTION SetFilePos (Handle: THandle; Pos: LongInt; MoveType: Word;
VAR NewPos: LongInt): Word;

BEGIN
  NewPos:=Sys_LSeek (Handle,Pos,MoveType);
  SetFilePos:=Errno;
END;

FUNCTION FileClose (Handle: THandle): Word;
BEGIN
  Sys_Close (Handle);
  DosStreamError:=Errno;
  FileClose := Errno;
END;

FUNCTION SetFileSize (Handle: THandle; FileSize: LongInt): Word;

{$IFNDEF BSD}
Var sr : syscallregs;
{$ENDIF}
{$IFDEF DOSSETFILE1}
    Actual, Buf: LongInt;
{$ENDIF}

BEGIN
 {$IFDEF BSD}
  Do_Syscall(Syscall_Nr_ftruncate,handle,filesize,0); {0 -> offset =64 bit}
 {$ELSE}
  sr.reg2:=Handle;
  sr.reg3:=FileSize;
  Syscall(syscall_nr_fTruncate,sr);
 {$ENDIF}
  If Errno=0 then
    SetFileSize:=0
  else
    SetFileSize:=103;
{$IFDEF DOSSETFILE1}
   If (Actual = FileSize) Then Begin                  { No position error }
     Actual := FileWrite(Handle, Pointer(@Buf), 0,Actual);   { Truncate the file }
     If (Actual <> -1) Then SetFileSize := 0 Else     { No truncate error }
       SetFileSize := 103;                            { File truncate error }
   End Else SetFileSize := 103;                       { File truncate error }
{$ENDIF}
END;

