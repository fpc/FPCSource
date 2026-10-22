{ %TARGET=linux }
{ %OPT=-gw -Xg }
{ ReadDebugLink: separate debug info in the current dir, the exe dir, <exedir>.debug and DebugInfoSearchPath }
program tdebuglink;

{$mode objfpc}{$H+}

uses
  SysUtils, exeinfo;

var
  gErrors: Integer = 0;
  gRoot: string;
  gLinkName: string;
  gDbgFile: string;


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


// The debug file found by the shortstring ReadDebugLink for aExe, or '' if none.
function FindDebugFileShort(const aExe: string): string;

var
  lExe: TExeFile;
  lFn: shortstring;

begin
  Result:='';
  if not OpenExeFile(lExe,aExe) then
    Exit('cannot open '+aExe);
  if ReadDebugLink(lExe,lFn) then
    Result:=lFn;
  CloseExeFile(lExe);
end;


var
  lExe, lBinDir, lGlobal, lFound: string;

begin
  lFound:=FindDebugFile(ParamStr(0));
  if (lFound='') or not FileExists(lFound) then
    begin
    Writeln('No debug link in ',ParamStr(0),': ',lFound);
    Halt(1);
    end;
  gLinkName:=ExtractFileName(lFound);
  gDbgFile:=ExpandFileName(lFound);

  gRoot:=IncludeTrailingPathDelimiter(GetTempDir(False))+'tdebuglink'+IntToStr(GetProcessID);
  lBinDir:=gRoot+'/bin/';
  lExe:=lBinDir+'prog';
  lGlobal:=gRoot+'/global';
  CopyFileTo(ParamStr(0),lExe);
  ChDir(gRoot);
  try
    DebugInfoSearchPath:='';
    Check('No debug file', FindDebugFile(lExe), '');

    CopyFileTo(gDbgFile,lBinDir+'.debug/'+gLinkName);
    Check('Exe dir .debug', FindDebugFile(lExe), lBinDir+'.debug/'+gLinkName);
    Check('Exe dir .debug, shortstring', FindDebugFileShort(lExe), lBinDir+'.debug/'+gLinkName);

    CopyFileTo(gDbgFile,lBinDir+gLinkName);
    Check('Exe dir before .debug', FindDebugFile(lExe), lBinDir+gLinkName);
    RemoveFile(lBinDir+gLinkName);

    CopyFileTo(gDbgFile,gRoot+'/'+gLinkName);
    Check('Current dir before exe dir', FindDebugFile(lExe), gLinkName);
    RemoveFile(gRoot+'/'+gLinkName);
    RemoveFile(lBinDir+'.debug/'+gLinkName);

    CopyFileTo(gDbgFile,lGlobal+lBinDir+gLinkName);
    Check('Search path not set', FindDebugFile(lExe), '');
    DebugInfoSearchPath:=gRoot+'/missing:'+lGlobal+'/';
    Check('Search path', FindDebugFile(lExe), lGlobal+lBinDir+gLinkName);
    DebugInfoSearchPath:=lGlobal;
    Check('Search path without trailing separator', FindDebugFile(lExe), lGlobal+lBinDir+gLinkName);
    DebugInfoSearchPath:=':'+lGlobal+':';
    Check('Search path with empty entries', FindDebugFile(lExe), lGlobal+lBinDir+gLinkName);
    RemoveFile(lGlobal+lBinDir+gLinkName);

    WriteTextFile(lBinDir+'.debug/'+gLinkName,'not the debug info');
    CopyFileTo(gDbgFile,lGlobal+lBinDir+gLinkName);
    Check('CRC mismatch skipped', FindDebugFile(lExe), lGlobal+lBinDir+gLinkName);
    RemoveFile(lGlobal+lBinDir+gLinkName);
    Check('CRC mismatch only', FindDebugFile(lExe), '');
    RemoveFile(lBinDir+'.debug/'+gLinkName);
  finally
    ChDir(ExtractFileDir(gDbgFile));
    RemoveFile(lExe);
    RemoveDir(gRoot);
  end;
  Writeln(gErrors,' errors');
  Halt(gErrors);
end.
