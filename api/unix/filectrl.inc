{
  System independent filecontrol interface for linux

  $Id$
}
uses
  Linux;

function OpenFileStr(FName: PChar; Flags: Longint): TFileHandle; [ alias: 'OpenFile' ];
var
  RC : longint;
  Todo: TErrorHandlerReturnValue;
begin
  repeat
    OpenFileStr:=fdOpen(FName, Flags, FilePerms);
    RC:=LinuxError;
    if (RC > 0) then
      Todo := ErrorHandler(RC, FName);
  until (RC <= 0) or (Todo <> errRetry);
end;

function CreateFileStr(FName: PChar): TFileHandle; [ alias: 'CreateFile' ];
const
  O_RDONLY = 0;
  O_WRONLY = 1;
  O_RDWR   = 2;
  O_CREATE = 64;
  O_EXCL   = 128;
  O_NOCTTY = 256;
  O_TRUNC  = 512;
  O_APPEND = 1024;
begin
  CreateFileStr := OpenFileStr(FName, O_RDWR+O_CREATE+O_TRUNC);
end;

procedure CloseFile(Handle: TFileHandle);
var
  RC: Longint;
  Todo: TErrorHandlerReturnValue;
begin
  repeat
    fdClose(Handle);
    RC := LinuxError;
    if (RC > 0) then
      Todo := ErrorHandler(RC, nil);
  until (RC <= 0) or (Todo <> errRetry);
end;

function SeekFile(Handle: TFileHandle; Pos: TFileInt; SeekType: Word): TFileInt;
var
  RC: Longint;
  Todo: TErrorHandlerReturnValue;
begin
  repeat
    RC := -fdSeek(Handle, Pos, SeekType);
    if (RC > 0) then
      Todo := ErrorHandler(RC, nil);
  until (RC <= 0) or (Todo <> errRetry);
  SeekFile := -RC;
end;

procedure DeleteFileStr(FName: PChar); [ alias: 'DeleteFile' ];
var
  RC: Longint;
  Todo: TErrorHandlerReturnValue;
begin
  repeat
    UnLink(FName);
    RC:=LinuxError;
    if (RC > 0) then
      Todo := ErrorHandler(RC, nil);
  until (RC <= 0) or (Todo <> errRetry);
end;

function ReadFile(Handle: TFileHandle; var Buff; Count: CPUWord): CPUWord;
var
  RC: Longint;
  BytesRead: LongInt;
  Todo: TErrorHandlerReturnValue;
begin
  repeat
    BytesRead := fdRead(Handle, Buff, Count);
    RC:=LinuxError;
    if (RC > 0) then
      Todo := ErrorHandler(RC, nil);
  until (RC <= 0) or (Todo <> errRetry);
  if (RC > 0) then
    ReadFile := 0
   else
    ReadFile := BytesRead;
end;


function WriteFile(Handle: TFileHandle; var Buff; Count: CPUWord): CPUWord;
var
  RC: Longint;
  BytesWritten: LongInt;
  Todo: TErrorHandlerReturnValue;
begin
  repeat
    BytesWritten := fdWrite(Handle, Buff, Count);
    RC:=LinuxError;
    if (RC > 0) then
      Todo := ErrorHandler(RC, nil);
  until (RC <= 0) or (Todo <> errRetry);
  if (RC > 0) then
    WriteFile := 0
   else
    WriteFile := BytesWritten;
end;

{ The following two routines should go to syscalls... }

procedure FlushFile(Handle: TFileHandle);
var
  RC: Longint;
  Todo: TErrorHandlerReturnValue;
begin
  repeat
    fdFlush(Handle);
    RC:=LinuxError;
    if (RC > 0) then
      Todo := ErrorHandler(RC, nil);
  until (RC <= 0) or (Todo <> errRetry);
end;

procedure TruncateFile(Handle: TFileHandle);
var
  RC: Longint;
  Todo: TErrorHandlerReturnValue;
begin
  repeat
    fdTruncate(Handle,0);
    RC:=LinuxError;
    if (RC > 0) then
      Todo := ErrorHandler(RC, nil);
  until (RC <= 0) or (Todo <> errRetry);
end;

function EndOfFile(Handle: TFileHandle): Boolean;
begin
  EndOfFile := FilePos(Handle) >= FileSize(Handle);
end;

function FilePos(Handle: TFileHandle): TFileInt;
begin
  FilePos := SeekFile(Handle, 0, skCur);
end;

function FileSize(Handle: TFileHandle): TFileInt;
var
  L: Longint;
begin
  L := FilePos(Handle);
  FileSize := SeekFile(Handle, 0, skEnd);
  SeekFile(Handle, L, skBeg);
end;

{
  $Log$
  Revision 1.2  2000-10-26 23:08:48  peter
    * merged freebsd from fixes

  Revision 1.1.2.1  2000/10/25 12:23:20  marco
   * Linux dir split up

  Revision 1.1  2000/07/13 06:29:39  michael
  + Initial import

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/11/24 23:36:38  peter
    * moved to packages dir

  Revision 1.1  1998/12/04 12:48:30  peter
    * moved some dirs

  Revision 1.1  1998/10/26 11:31:47  peter
    + inital include files

}
