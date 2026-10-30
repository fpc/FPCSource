{ %TARGET=win32,win64 }
{ FileOpen, FileCreate and TFileStream use System.fmShareDenyNoneFlags for fmShareDenyNone, issue #8898 }
program tsharedenynone;

{$mode objfpc}{$h+}

uses
  Windows, SysUtils, Classes;

var
  gDir, gFile, gRenamed: string;


// Halts with aCode when aCondition does not hold
procedure Check(aCode: Integer; aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
    writeln('Failed: ',aMsg);
    halt(aCode);
    end;
end;


// Creates gFile with some content
procedure MakeFile;

var
  lHandle: THandle;

begin
  DeleteFile(gRenamed);
  lHandle:=FileCreate(gFile);
  FileWrite(lHandle,'abc',3);
  FileClose(lHandle);
end;


// True if gFile can be renamed while open; renames it back
function CanRename: Boolean;

begin
  Result:=RenameFile(gFile,gRenamed);
  if Result then
    RenameFile(gRenamed,gFile);
end;


var
  lHandle, lSecond: THandle;
  lStream: TFileStream;

begin
  gDir:=IncludeTrailingPathDelimiter(GetTempDir)+'tsharedenynone'+IntToStr(GetCurrentProcessId);
  ForceDirectories(gDir);
  gFile:=gDir+PathDelim+'test.txt';
  gRenamed:=gDir+PathDelim+'renamed.txt';
  MakeFile;

  Check(1,fmShareDenyNoneFlags=FILE_SHARE_READ or FILE_SHARE_WRITE,'default fmShareDenyNoneFlags');
  lHandle:=FileOpen(gFile,fmOpenRead or fmShareDenyNone);
  Check(2,lHandle<>THandle(-1),'FileOpen with fmShareDenyNone');
  lSecond:=FileOpen(gFile,fmOpenReadWrite or fmShareDenyNone);
  Check(3,lSecond<>THandle(-1),'a second FileOpen for writing is shared');
  FileClose(lSecond);
  Check(4,not CanRename,'renaming is not shared by default');
  FileClose(lHandle);

  fmShareDenyNoneFlags:=FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE;
  lHandle:=FileOpen(gFile,fmOpenRead or fmShareDenyNone);
  Check(10,lHandle<>THandle(-1),'FileOpen with fmShareDenyNone and delete sharing');
  Check(11,CanRename,'FileOpen shares renaming with FILE_SHARE_DELETE');
  Check(12,DeleteFile(gFile),'FileOpen shares deleting with FILE_SHARE_DELETE');
  FileClose(lHandle);
  Check(13,not FileExists(gFile),'file is gone after closing');

  MakeFile;
  lStream:=TFileStream.Create(gFile,fmOpenRead or fmShareDenyNone);
  Check(20,CanRename,'TFileStream shares renaming with FILE_SHARE_DELETE');
  lStream.Free;

  lHandle:=FileCreate(gFile,fmShareDenyNone,0);
  Check(30,lHandle<>THandle(-1),'FileCreate with fmShareDenyNone');
  Check(31,CanRename,'FileCreate shares renaming with FILE_SHARE_DELETE');
  FileClose(lHandle);

  fmShareDenyNoneFlags:=FILE_SHARE_READ or FILE_SHARE_WRITE;
  lHandle:=FileCreate(gFile,fmShareDenyNone,0);
  Check(32,not CanRename,'FileCreate does not share renaming by default');
  FileClose(lHandle);

  lHandle:=FileOpen(gFile,fmOpenRead or $50);
  Check(40,lHandle<>THandle(-1),'FileOpen with an unknown share mode');
  lSecond:=FileOpen(gFile,fmOpenRead or fmShareDenyNone);
  Check(41,lSecond=THandle(-1),'an unknown share mode shares nothing');
  FileClose(lHandle);

  DeleteFile(gFile);
  RemoveDir(gDir);
  writeln('ok');
end.
