{ FileCreate and TFileStream with fmExclusive create a file only when it does not exist yet }
program tfmexclusive;

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  BaseUnix,
{$endif}
  SysUtils, Classes;

var
  gDir: string;
  gFailed: Boolean;


// Writes aMsg and marks the test as failed when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    writeln('check ',aCode,' failed: ',aMsg);
    gFailed:=True;
    end;
end;


// The size of aFileName, -1 if it does not exist
function FileSizeOf(const aFileName: string): Int64;

var
  lInfo: TSearchRec;

begin
  Result:=-1;
  if FindFirst(aFileName,faAnyFile,lInfo)=0 then
    Result:=lInfo.Size;
  FindClose(lInfo);
end;


// Creates aFileName with fmExclusive and aShare, writes aData; False if the create failed
function CreateExclusive(const aFileName: string; aShare: Integer; const aData: AnsiString): Boolean;

var
  lHandle: THandle;

begin
  lHandle:=FileCreate(aFileName,fmExclusive or aShare,&600);
  Result:=lHandle<>feInvalidHandle;
  if not Result then
    Exit;
  if aData<>'' then
    FileWrite(lHandle,aData[1],Length(aData));
  FileClose(lHandle);
end;


// FileCreate with and without fmExclusive
procedure TestFileCreate;

var
  lName: string;
  lHandle: THandle;

begin
  lName:=gDir+'a.txt';
  Check(1,CreateExclusive(lName,0,'abc'),'fmExclusive creates a new file');
  Check(2,not CreateExclusive(lName,0,''),'fmExclusive fails on an existing file');
  Check(3,FileSizeOf(lName)=3,'a failed fmExclusive create leaves the file untouched');
  Check(4,not CreateExclusive(lName,fmShareDenyNone,''),'fmExclusive or fmShareDenyNone fails on an existing file');
  lName:=gDir+'b.txt';
  Check(5,CreateExclusive(lName,fmShareDenyNone,'xy'),'fmExclusive or fmShareDenyNone creates a new file');
  lHandle:=FileCreate(lName,fmShareExclusive,&600);
  Check(6,lHandle<>feInvalidHandle,'FileCreate without fmExclusive opens an existing file');
  FileClose(lHandle);
  Check(7,FileSizeOf(lName)=0,'FileCreate without fmExclusive truncates an existing file');
end;


// TFileStream with fmCreate or fmExclusive
procedure TestFileStream;

var
  lName: string;
  lStream: TFileStream;
  lClass: string;
{$ifdef unix}
  lStat: TStat;
{$endif}

begin
  lName:=gDir+'c.txt';
  lStream:=TFileStream.Create(lName,fmCreate or fmExclusive,&600);
  lStream.Free;
  Check(10,FileExists(lName),'TFileStream with fmCreate or fmExclusive creates a new file');
{$ifdef unix}
  Check(11,(fpStat(lName,lStat)=0) and ((lStat.st_mode and &777)=&600),'fmExclusive uses the rights argument');
{$endif}
  lClass:='';
  try
    TFileStream.Create(lName,fmCreate or fmExclusive).Free;
  except
    on E: Exception do
      lClass:=E.ClassName;
  end;
  Check(12,lClass='EFCreateError','TFileStream with fmCreate or fmExclusive raises EFCreateError on an existing file, got "'+lClass+'"');
end;


{$ifdef unix}
// fmExclusive on a symbolic link to a file that does not exist
procedure TestSymlink;

var
  lLink, lTarget: string;

begin
  lLink:=gDir+'link';
  lTarget:=gDir+'target';
  if fpSymlink(PAnsiChar(lTarget),PAnsiChar(lLink))<>0 then
    begin
    Check(20,False,'could not create a symbolic link');
    Exit;
    end;
  Check(21,not CreateExclusive(lLink,0,'z'),'fmExclusive fails on a dangling symbolic link');
  Check(22,not FileExists(lTarget),'fmExclusive does not create the target of a symbolic link');
  DeleteFile(lLink);
end;
{$endif}


// Deletes the files of the test and the test directory
procedure Cleanup;

begin
  DeleteFile(gDir+'a.txt');
  DeleteFile(gDir+'b.txt');
  DeleteFile(gDir+'c.txt');
  DeleteFile(gDir+'target');
  DeleteFile(gDir+'link');
  RemoveDir(gDir);
end;


begin
  gFailed:=False;
  gDir:=IncludeTrailingPathDelimiter(GetTempDir(False))+'tfmexclusive'+IntToStr(GetProcessID);
  if not ForceDirectories(gDir) then
    begin
    writeln('cannot create ',gDir);
    halt(1);
    end;
  gDir:=IncludeTrailingPathDelimiter(gDir);
  try
    TestFileCreate;
    TestFileStream;
{$ifdef unix}
    TestSymlink;
{$endif}
  finally
    Cleanup;
  end;
  if gFailed then
    halt(1);
  writeln('ok');
end.
