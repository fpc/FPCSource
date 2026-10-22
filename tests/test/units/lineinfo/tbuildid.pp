{ %TARGET=linux }
{ %OPT=-gw -Xg -k--build-id }
{ ReadBuildId, and ReadDebugLink finding separate debug info as <dir>/.build-id/xx/rest.debug }
program tbuildid;

{$mode objfpc}{$H+}

uses
  SysUtils, StrUtils, exeinfo;

var
  gErrors: Integer = 0;
  gRoot: string;


procedure Check(const aName, aActual, aExpected: string);

begin
  if aActual=aExpected then
    Writeln(aName,': ',aActual,' OK')
  else
    begin
    Writeln(aName,': ',aActual,' expected ',aExpected,' ERROR');
    Inc(gErrors);
    end;
end;


procedure CheckTrue(const aName: string; aValue: Boolean);

begin
  if aValue then
    Writeln(aName,' OK')
  else
    begin
    Writeln(aName,' ERROR');
    Inc(gErrors);
    end;
end;


// Copies aSource to aDest, creating the directories of aDest.
procedure CopyFileTo(const aSource, aDest: string);

var
  lIn, lOut: File;
  lBuf: array[0..4095] of Byte;
  lCount: Longint;

begin
  ForceDirectories(ExtractFileDir(aDest));
  AssignFile(lIn,aSource);
  FileMode:=fmOpenRead;
  Reset(lIn,1);
  AssignFile(lOut,aDest);
  Rewrite(lOut,1);
  repeat
    BlockRead(lIn,lBuf,SizeOf(lBuf),lCount);
    BlockWrite(lOut,lBuf,lCount);
  until lCount<SizeOf(lBuf);
  CloseFile(lOut);
  CloseFile(lIn);
end;


// Writes a file with the given text, creating its directories.
procedure WriteTextFile(const aFileName, aText: string);

var
  lFile: Text;

begin
  ForceDirectories(ExtractFileDir(aFileName));
  AssignFile(lFile,aFileName);
  Rewrite(lFile);
  Write(lFile,aText);
  CloseFile(lFile);
end;


// Overwrites the bytes of aFileName at aOffset with aData.
procedure PatchFile(const aFileName: string; aOffset: Int64; const aData: AnsiString);

var
  lFile: File;

begin
  AssignFile(lFile,aFileName);
  FileMode:=fmOpenReadWrite;
  Reset(lFile,1);
  Seek(lFile,aOffset);
  BlockWrite(lFile,aData[1],Length(aData));
  CloseFile(lFile);
end;


// The offset of the last occurrence of aText in aFileName, or -1.
function FindLastInFile(const aFileName: string; const aText: AnsiString): Int64;

var
  lFile: File;
  lData: AnsiString;

begin
  AssignFile(lFile,aFileName);
  FileMode:=fmOpenRead;
  Reset(lFile,1);
  SetLength(lData,FileSize(lFile));
  BlockRead(lFile,lData[1],Length(lData));
  CloseFile(lFile);
  Result:=RPos(aText,lData)-1;
end;


// Deletes aFileName and its empty directories up to, not including, gRoot.
procedure RemoveFile(const aFileName: string);

var
  lDir: string;

begin
  DeleteFile(aFileName);
  lDir:=ExtractFileDir(aFileName);
  while (Length(lDir)>Length(gRoot)) and RemoveDir(lDir) do
    lDir:=ExtractFileDir(lDir);
end;


// The build-id of aFileName, or '' if it has none.
function GetBuildId(const aFileName: string): string;

var
  lExe: TExeFile;
  lId: ansistring;

begin
  Result:='';
  if OpenExeFile(lExe,aFileName) and ReadBuildId(lExe,lId) then
    Result:=lId;
  CloseExeFile(lExe);
end;


// The offset of the build-id bytes in aFileName, or -1.
function GetBuildIdOffset(const aFileName: string): Int64;

var
  lExe: TExeFile;
  lOfs, lLen: Longint;

begin
  Result:=-1;
  if OpenExeFile(lExe,aFileName) and FindExeSection(lExe,'.note.gnu.build-id',lOfs,lLen) then
    Result:=lOfs+16;
  CloseExeFile(lExe);
end;


// The debug file found by ReadDebugLink for aExe, or '' if none.
function FindDebugFile(const aExe: string): string;

var
  lExe: TExeFile;
  lFn: ansistring;

begin
  Result:='';
  if not OpenExeFile(lExe,aExe) then
    Exit('cannot open '+aExe);
  if ReadDebugLink(lExe,lFn) then
    Result:=lFn;
  CloseExeFile(lExe);
end;


var
  lDbgFile, lLinkName, lBuildId, lExe, lBinDir, lGlobal, lIdFile, lFound: string;
  lOfs: Int64;
  lHex: Boolean;
  i: Integer;

begin
  DebugInfoSearchPath:='';
  lFound:=FindDebugFile(ParamStr(0));
  if (lFound='') or not FileExists(lFound) then
    begin
    Writeln('No debug link in ',ParamStr(0),': ',lFound);
    Halt(1);
    end;
  lDbgFile:=ExpandFileName(lFound);
  lLinkName:=ExtractFileName(lFound);

  lBuildId:=GetBuildId(ParamStr(0));
  lHex:=(Length(lBuildId)>=4) and not Odd(Length(lBuildId));
  for i:=1 to Length(lBuildId) do
    lHex:=lHex and (lBuildId[i] in ['0'..'9','a'..'f']);
  CheckTrue('Build-id of the executable is lowercase hex: '+lBuildId, lHex);
  Check('Build-id of the debug file', GetBuildId(lDbgFile), lBuildId);
  if not lHex then
    Halt(1);

  gRoot:=IncludeTrailingPathDelimiter(GetTempDir(False))+'tbuildid'+IntToStr(GetProcessID);
  lBinDir:=gRoot+'/bin/';
  lExe:=lBinDir+'prog';
  lGlobal:=gRoot+'/global';
  lIdFile:=lGlobal+'/.build-id/'+Copy(lBuildId,1,2)+'/'+Copy(lBuildId,3,Length(lBuildId))+'.debug';
  CopyFileTo(ParamStr(0),lExe);
  ChDir(gRoot);
  try
    Check('No debug file', FindDebugFile(lExe), '');

    CopyFileTo(lDbgFile,lIdFile);
    Check('Search path not set', FindDebugFile(lExe), '');
    DebugInfoSearchPath:=gRoot+'/missing:'+lGlobal+'/';
    Check('Build-id path', FindDebugFile(lExe), lIdFile);

    CopyFileTo(lDbgFile,lBinDir+lLinkName);
    Check('Build-id path before debug link', FindDebugFile(lExe), lIdFile);

    lOfs:=GetBuildIdOffset(lIdFile);
    CheckTrue('Build-id note found in the copied debug file', lOfs>0);
    PatchFile(lIdFile,lOfs,#$FF#$FF#$FF#$FF);
    Check('Other build-id skipped', FindDebugFile(lExe), lBinDir+lLinkName);
    RemoveFile(lBinDir+lLinkName);
    Check('Other build-id only', FindDebugFile(lExe), '');

    RemoveFile(lIdFile);
    WriteTextFile(lIdFile,'not the debug info');
    Check('Not an executable file', FindDebugFile(lExe), '');
    RemoveFile(lIdFile);

    lOfs:=FindLastInFile(lExe,'.gnu_debuglink'#0);
    CheckTrue('Debug link section name found', lOfs>0);
    PatchFile(lExe,lOfs,'.gnu_debuglinX');
    CopyFileTo(lDbgFile,lBinDir+lLinkName);
    Check('Without debug link, no build-id file', FindDebugFile(lExe), '');
    CopyFileTo(lDbgFile,lIdFile);
    Check('Without debug link', FindDebugFile(lExe), lIdFile);
    RemoveFile(lIdFile);
    RemoveFile(lBinDir+lLinkName);
  finally
    ChDir(ExtractFileDir(lDbgFile));
    RemoveFile(lExe);
    RemoveDir(gRoot);
  end;
  Writeln(gErrors,' errors');
  Halt(gErrors);
end.
