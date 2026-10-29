{ %TARGET=win32,win64 }
{ GetTempDir and GetTempFileName with a non-ASCII temporary directory, issue #15233 }
program ttempdirwin;

{$mode objfpc}{$h+}

uses
  Windows, SysUtils;

var
  gBase: UnicodeString;


// Halts with aCode when aCondition does not hold
procedure Check(aCode: Integer; aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
    writeln('Failed: ',aMsg);
    halt(aCode);
    end;
end;


// Returns the temporary directory of the system before the test changes TMP and TEMP
function SystemTempDir: UnicodeString;

begin
  SetLength(Result,MAX_PATH+1);
  SetLength(Result,GetTempPathW(Length(Result),PWideChar(Result)));
end;


// Creates aName below gBase, makes it the temporary directory and checks GetTempDir and GetTempFileName
procedure CheckTempDir(aCode: Integer; const aName: UnicodeString);

var
  lDir, lFile: UnicodeString;
  lTemp, lTempFile: String;
  lHandle: THandle;

begin
  lDir:=gBase+aName;
  CreateDirectoryW(PWideChar(lDir),nil);
  Check(aCode,DirectoryExists(lDir),'test directory created');
  SetEnvironmentVariableW('TMP',PWideChar(lDir));
  SetEnvironmentVariableW('TEMP',PWideChar(lDir));
  try
    lTemp:=GetTempDir;
    Check(aCode+1,UnicodeString(lTemp)=lDir+'\','GetTempDir is the directory: '+UTF8Encode(UnicodeString(lTemp)));
    Check(aCode+2,DirectoryExists(lTemp),'GetTempDir exists');
    lTempFile:=GetTempFileName;
    lFile:=UnicodeString(lTempFile);
    Check(aCode+3,Copy(lFile,1,Length(lDir)+1)=lDir+'\','GetTempFileName is in the directory: '+UTF8Encode(lFile));
    lHandle:=FileCreate(lTempFile);
    Check(aCode+4,lHandle<>THandle(-1),'file of GetTempFileName can be created');
    FileClose(lHandle);
    Check(aCode+5,FileExists(lFile),'created file is found by its UnicodeString name');
    DeleteFileW(PWideChar(lFile));
  finally
    RemoveDirectoryW(PWideChar(lDir));
  end;
end;


// Checks that GetTempDir follows TMP before TEMP, and is not empty without either
procedure CheckOrder(aCode: Integer);

var
  lTmpDir, lTempDir: UnicodeString;

begin
  lTmpDir:=gBase+'ttempdirwin_tmp';
  lTempDir:=gBase+'ttempdirwin_temp';
  CreateDirectoryW(PWideChar(lTmpDir),nil);
  CreateDirectoryW(PWideChar(lTempDir),nil);
  try
    SetEnvironmentVariableW('TMP',PWideChar(lTmpDir));
    SetEnvironmentVariableW('TEMP',PWideChar(lTempDir));
    Check(aCode,UnicodeString(GetTempDir)=lTmpDir+'\','TMP is used before TEMP: '+GetTempDir);
    SetEnvironmentVariableW('TMP',nil);
    Check(aCode+1,UnicodeString(GetTempDir)=lTempDir+'\','TEMP is used without TMP: '+GetTempDir);
    SetEnvironmentVariableW('TEMP',nil);
    Check(aCode+2,GetTempDir<>'','GetTempDir is not empty without TMP and TEMP');
    Check(aCode+3,DirectoryExists(GetTempDir),'GetTempDir without TMP and TEMP exists: '+GetTempDir);
  finally
    RemoveDirectoryW(PWideChar(lTmpDir));
    RemoveDirectoryW(PWideChar(lTempDir));
  end;
end;


// Checks that GetTempDir uses DefaultRTLFileSystemCodePage when it differs from DefaultSystemCodePage
procedure CheckFileSystemCodePage(aCode: Integer; const aName: UnicodeString);

var
  lDir: UnicodeString;
  lTemp: String;

begin
  lDir:=gBase+aName;
  CreateDirectoryW(PWideChar(lDir),nil);
  SetEnvironmentVariableW('TMP',PWideChar(lDir));
  try
    lTemp:=GetTempDir;
{$if SizeOf(Char)=1}
    Check(aCode,StringCodePage(lTemp)=DefaultRTLFileSystemCodePage,'GetTempDir has the RTL file system code page');
{$endif}
    Check(aCode+1,UnicodeString(lTemp)=lDir+'\','GetTempDir keeps the characters of the directory');
    Check(aCode+2,DirectoryExists(lTemp),'GetTempDir exists');
  finally
    RemoveDirectoryW(PWideChar(lDir));
  end;
end;


var
  lOldTmp, lOldTemp: UnicodeString;
  lOldSystemCP, lOldFileSystemCP: TSystemCodePage;

begin
  gBase:=SystemTempDir;
  Check(1,gBase<>'','system temporary directory');
  lOldTmp:=GetEnvironmentVariable(UnicodeString('TMP'));
  lOldTemp:=GetEnvironmentVariable(UnicodeString('TEMP'));
  lOldSystemCP:=DefaultSystemCodePage;
  lOldFileSystemCP:=DefaultRTLFileSystemCodePage;
  try
    CheckTempDir(10,'ttempdirwin_'#$00E9#$00FC#$20AC);
    CheckOrder(20);

    DefaultRTLFileSystemCodePage:=CP_UTF8;
    CheckFileSystemCodePage(30,'ttempdirwin_'#$4E2D#$6587#$03A9);

    DefaultSystemCodePage:=CP_UTF8;
    CheckTempDir(40,'ttempdirwin_'#$4E2D#$6587#$03A9);

    OnGetTempDir:=nil;
    Check(50,GetTempDir(False)=GetTempDir(True),'GetTempDir(False) matches GetTempDir(True)');
  finally
    DefaultSystemCodePage:=lOldSystemCP;
    DefaultRTLFileSystemCodePage:=lOldFileSystemCP;
    SetEnvironmentVariableW('TMP',PWideChar(lOldTmp));
    SetEnvironmentVariableW('TEMP',PWideChar(lOldTemp));
  end;
  writeln('ok');
end.
