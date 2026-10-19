{ %target=solaris,linux }
{ Opening, seeking, sizing and locking a file larger than 4 GB, issue #35650 }
program tbigfile;

{$mode objfpc}{$h+}

uses
  SysUtils, BaseUnix, Unix;

const
  FileName = 'tbigfile.dat';
  BigOffset = Int64(5) shl 30;
  Marker: array[0..3] of AnsiChar = 'ABCD';
{$ifdef solaris}
  LockWrite = 2;
  LockNone = 3;
{$else}
  LockWrite = 1;
  LockNone = 2;
{$endif}

var
  lErrors: Integer = 0;


procedure Check(aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
    Writeln('Failed: ',aMsg);
    Inc(lErrors);
    end;
end;


procedure CreateBigFile;

var
  lHandle: THandle;
begin
  lHandle:=FileCreate(FileName);
  Check(lHandle<>THandle(-1),'FileCreate');
  Check(FileSeek(lHandle,BigOffset,fsFromBeginning)=BigOffset,'FileSeek past 4 GB on a new file');
  Check(FileWrite(lHandle,Marker,SizeOf(Marker))=SizeOf(Marker),'FileWrite past 4 GB');
  FileClose(lHandle);
end;


procedure TestOpenAndSeek;

var
  lHandle: THandle;
  lBuf: array[0..3] of AnsiChar;
begin
  lHandle:=FileOpen(FileName,fmOpenRead);
  Check(lHandle<>THandle(-1),'FileOpen of a file larger than 4 GB');
  if lHandle=THandle(-1) then
    exit;
  Check(FileSeek(lHandle,Int64(0),fsFromEnd)=BigOffset+SizeOf(Marker),'FileSeek to the end returns the 64-bit size');
  Check(FileSeek(lHandle,BigOffset,fsFromBeginning)=BigOffset,'FileSeek to an offset past 4 GB');
  FillChar(lBuf,SizeOf(lBuf),0);
  Check(FileRead(lHandle,lBuf,SizeOf(lBuf))=SizeOf(lBuf),'FileRead past 4 GB');
  Check(lBuf=Marker,'data read past 4 GB');
  FileClose(lHandle);
end;


procedure TestSize;

var
  lStat: TStat;
  lInfo: TSearchRec;
begin
  Check(FpStat(FileName,lStat)=0,'FpStat of a file larger than 4 GB');
  Check(lStat.st_size=BigOffset+SizeOf(Marker),'st_size is the 64-bit size');
  Check(FindFirst(FileName,faAnyFile,lInfo)=0,'FindFirst of a file larger than 4 GB');
  Check(lInfo.Size=BigOffset+SizeOf(Marker),'TSearchRec.Size is the 64-bit size');
  FindClose(lInfo);
end;


procedure TestLock;

var
  lHandle: cint;
  lLock: FLock;
begin
  lHandle:=FpOpen(FileName,O_RDWR);
  Check(lHandle>=0,'FpOpen read/write of a file larger than 4 GB');
  if lHandle<0 then
    exit;
  FillChar(lLock,SizeOf(lLock),0);
  lLock.l_type:=LockWrite;
  lLock.l_whence:=SEEK_SET;
  lLock.l_start:=BigOffset;
  lLock.l_len:=SizeOf(Marker);
  Check(FpFcntl(lHandle,F_SetLk,lLock)=0,'record lock past 4 GB');
  FillChar(lLock,SizeOf(lLock),0);
  lLock.l_type:=LockWrite;
  lLock.l_whence:=SEEK_SET;
  lLock.l_start:=BigOffset;
  lLock.l_len:=SizeOf(Marker);
  Check(FpFcntl(lHandle,F_GetLk,lLock)=0,'F_GetLk past 4 GB');
  Check(lLock.l_type=LockNone,'own lock is not reported as a conflict');
  Check(FpFlock(lHandle,LOCK_EX)=0,'FpFlock of a file larger than 4 GB');
  FpClose(lHandle);
end;


procedure TestTruncate;

var
  lHandle: THandle;
begin
  lHandle:=FileOpen(FileName,fmOpenReadWrite);
  Check(lHandle<>THandle(-1),'FileOpen read/write of a file larger than 4 GB');
  if lHandle=THandle(-1) then
    exit;
  Check(FileTruncate(lHandle,BigOffset+1),'FileTruncate to a size past 4 GB');
  Check(FileSeek(lHandle,Int64(0),fsFromEnd)=BigOffset+1,'size after FileTruncate');
  FileClose(lHandle);
end;


begin
  CreateBigFile;
  TestOpenAndSeek;
  TestSize;
  TestLock;
  TestTruncate;
  DeleteFile(FileName);
  if lErrors>0 then
    Halt(1);
  Writeln('ok');
end.
