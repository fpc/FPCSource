{ For linux we 'steal' the following from system unit, this way
  we don't need to change the system unit interface. }

Var errno : Longint;

{$i sysnr.inc}
{$i errno.inc}
{$i sysconst.inc}
{$i systypes.inc}
{$i syscalls.inc}

FUNCTION FileOpen (Var FileName: AsciiZ; Mode: Word): THandle;

Var LinuxMode : Word;

BEGIN
  LinuxMode:=0;
  if (Mode and stCreate)=stCreate then LinuxMode:=Open_Creat;
  if (Mode and stOpenRead)=stOpenRead then LinuxMode:=LinuxMode or Open_RdOnly;
  If (Mode and stOpenWrite)=stOpenWrite then LinuxMode:=LinuxMode or Open_WrOnly;
  if (Mode and stOpen)=stOpen then LinuxMode:=LinuxMode or Open_RdWr;
  FileOpen:=SYS_Open (pchar(@FileName[0]),438 {666 octal},LinuxMode);
  DosStreamError:=Errno;
  FileOpen:=Errno;
END;

FUNCTION FileRead (Handle: THandle; Var BufferArea; BufferLength: Sw_Word;
Var BytesMoved: Sw_Word): Word;
BEGIN
  BytesMoved:=Sys_read (Handle,Pchar(@BufferArea),BufferLength);
  DosStreamError:=Errno;
  FileRead := Errno;
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
{$IFDEF DOSSETFILE1}  
VAR Actual, Buf: LongInt;
{$ENDIF}   
BEGIN
  if Sys_Truncate(Handle,FileSize)=0 then
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

