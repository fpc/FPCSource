unit utcdirwatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, punit, testutils, testregistry, dirwatch;

procedure RegisterTests;

implementation

uses typinfo, inifiles;

type
  TChangedEntry = record
    Dir : TWatchDirectoryEntry;
    Events : TWatchFileEvents;
    FN : String;
  end;
  TChangedEntryArray = Array of TChangedEntry;

  { TEventObject }

  TEventObject = class
    procedure DoChange(Sender: TObject; const aEvent: TFileChangeEvent);
  private
    procedure DoCheck(sender: TObject; var aContinue: Boolean);
    procedure HandleCreateFile(Sender: TObject);
  end;

var
  FDirWatch: TDirwatch;
  FTestDir: string;
  FChanged: TChangedEntryArray;
  FCheckCount : Integer;
  FMaxLoopCount : Integer;
  FDoCheckOne : TNotifyEvent;
  BaseDir : String;
  Events : TEventObject;

// Helper functions from the original test case
procedure CleanDirs(aDir: String);
Var
  Info : TSearchRec;
  lDir,lFull : String;
begin
  lDir:=IncludeTrailingPathDelimiter(aDir);
  If FindFirst(lDir+AllFilesMask,sysutils.faDirectory,Info)=0 then
    try
      Repeat
        lFull:=lDir+Info.Name;
        if (Info.Attr and faDirectory)<>0 then
          begin
          if not ((Info.Name='.') or (Info.Name='..')) then
            begin
            CleanDirs(lFull);
            if not RemoveDir(lFull) then
              Fail('Failed to remove directory '+lFull)
            end;
          end
        else if not DeleteFile(lFull) then
          Fail('Failed to remove file '+lFull)
      until FindNext(Info)<>0;
    finally
      FindClose(Info);
    end;
end;


procedure TEventObject.DoChange(Sender: TObject; const aEvent: TFileChangeEvent);
var
  Len : Integer;
begin
  len:=Length(FChanged);
  SetLength(FChanged,Len+1);
  FChanged[Len].Dir:=aEvent.Entry;
  FChanged[Len].Events:=aEvent.Events;
  FChanged[Len].FN:=aEvent.FileName;
end;

procedure TEventObject.DoCheck(sender: TObject; var aContinue: Boolean);
begin
  aContinue:=FCheckCount<FMaxLoopCount;
  if (FCheckCount=0) then
    if Assigned(FDoCheckOne) then
       FDoCheckOne(nil);
  inc(FCheckCount);
end;

procedure DoAppendFile(const aName : string);
var
  FD : THandle;
begin
  FD:=FileOpen(FTestDir+aName,fmOpenWrite);
  try
    FileSeek(FD,0,fsFromEnd);
    if FileWrite(FD,aName[1],Length(aName))=-1 then
      Writeln(GetLastOSError);
  finally
    FileClose(FD);
  end;
end;

procedure DoCreateFile(const aName : string);
var
  L: TStrings;
begin
  L:=TStringList.Create;
  try
    L.Add(aName);
    L.SaveToFile(FTestDir+aName);
  finally
    L.Free;
  end;
end;

procedure DoDeleteFile(const aName: string);
begin
  If not DeleteFile(FTestDir+aName) then
    Fail('Failed to delete file '+FTestDir+aName);
end;

procedure TEventObject.HandleCreateFile(Sender: TObject);
begin
  DoCreateFile('name.txt');
end;

procedure AssertEventsEquals(const Msg: String; aExpected, aActual: TWatchFileEvents);
begin
  AssertEquals(Msg,SetToString(PTypeInfo(TypeInfo(TWatchFileEvents)),Longint(aExpected),False),
                   SetToString(PTypeInfo(TypeInfo(TWatchFileEvents)),Longint(aActual),False));
end;

procedure AssertChange(const Msg: String; aIndex: Integer; aEntry: TWatchDirectoryEntry; aEvents: TWatchFileEvents; const aFileName: string = '');
var
  M : String;
begin
  M:=Msg+Format(' [%d]: ',[aIndex]);
  AssertTrue(M+'correct index',aIndex<Length(FChanged));
  AssertSame(M+'correct dir entry',aEntry,FChanged[aIndex].Dir);
  AssertEventsEquals(M+'correct changes',aEvents,FChanged[aIndex].Events);
  if aFileName<>'' then
    AssertEquals(M+'correct fileName',aFileName,FChanged[aIndex].FN);
end;

// Per-test setup and teardown
procedure TestSetup;
begin
  FDirWatch:=TDirwatch.Create(Nil);
  FTestDir:=IncludeTrailingPathDelimiter(BaseDir);
  ForceDirectories(FTestDir);
  FDirWatch.OnChange:=@Events.DoChange;
  FMaxLoopCount:=0;
  FCheckCount:=0;
  FDoCheckOne:=Nil;
  SetLength(FChanged, 0);
end;

procedure TestTearDown;
begin
  FDirWatch.Free;
  CleanDirs(FTestDir);
end;

// Flattened Tests
function TestDirWatch_TestHookUp: TTestString;
begin
  Result := '';
  TestSetup;
  try
    AssertNotNull('Have watch',FDirWatch);
    AssertEquals('No watches',0,FDirWatch.Watches.Count);
    AssertTrue('Have test dir',FTestDir<>'');
    AssertTrue('test dir exists',DirectoryExists(FTestDir));
    AssertEquals('No max check count',0,FMaxLoopCount);
    AssertEquals('No check count',0,FCheckCount);
    AssertTrue('No docheckone',FDoCheckOne=nil);
  finally
    TestTearDown;
  end;
end;

function TestDirWatch_TestAddFile: TTestString;
begin
  Result := '';
  TestSetup;
  try
    FDirwatch.AddWatch(FTestDir,[feCreate]);
    FDirWatch.InitWatch;
    DoCreateFile('name.txt');
    AssertEquals('Change detected', 1, FDirWatch.Check);
    AssertChange('Create',0,FDirWatch.Watches[0],[feCreate],'name.txt');
  finally
    TestTearDown;
  end;
end;

function TestDirWatch_TestAppendFile: TTestString;
begin
  Result := '';
  TestSetup;
  try
    FDirwatch.AddWatch(FTestDir,[feModify]);
    DoCreateFile('name.txt');
    FDirWatch.InitWatch;
    DoAppendFile('name.txt');
    AssertEquals('Change detected',1,FDirWatch.Check);
    AssertChange('Change detected',0,FDirWatch.Watches[0],[feModify],'name.txt');
  finally
    TestTearDown;
  end;
end;

function TestDirWatch_TestDeleteFile: TTestString;
begin
  Result := '';
  TestSetup;
  try
    FDirwatch.AddWatch(FTestDir,[feDelete]);
    DoCreateFile('name.txt');
    FDirWatch.InitWatch;
    DoDeleteFile('name.txt');
    AssertEquals('Change detected',1,FDirWatch.Check);
    AssertChange('Change detected',0,FDirWatch.Watches[0],[feDelete],'name.txt');
  finally
    TestTearDown;
  end;
end;

function TestDirWatch_TestLoopNoThread: TTestString;
begin
  Result := '';
  TestSetup;
  try
    FDirwatch.AddWatch(FTestDir,[feCreate]);
    FDirwatch.OnCheck:=@Events.DoCheck;
    FDoCheckOne:=@Events.HandleCreateFile;
    FMaxLoopCount:=2;
    FDirWatch.StartLoop;
    AssertChange('Change detected',0,FDirWatch.Watches[0],[feCreate],'name.txt');
  finally
    TestTearDown;
  end;
end;

function TestDirWatch_TestLoopThread: TTestString;
var
  I : Integer;
begin
  Result := '';
  TestSetup;
  try
    FDirwatch.AddWatch(FTestDir,[feCreate]);
    FDirwatch.Threaded:=True;
    FDirWatch.StartLoop;
    Sleep(50);
    DoCreateFile('name.txt');
    I:=0;
    Repeat
      Sleep(10);
      CheckSynchronize;
      inc(i);
    until (I>=50) or (length(FChanged)>0);
    AssertChange('Change detected',0,FDirWatch.Watches[0],[feCreate],'name.txt');
  finally
    TestTearDown;
  end;
end;

function TestDirWatch_TestAddFileBaseDir: TTestString;
begin
  Result := '';
  TestSetup;
  try
    FDirwatch.BaseDir:=FTestDir;
    AssertTrue('Create Subdir ',ForceDirectories(FTestDir+'sub'));
    FDirwatch.AddWatch('',[feCreate]);
    FDirWatch.InitWatch;
    DoCreateFile('sub/name.txt');
    AssertEquals('Subdirs not watched',0,FDirWatch.Check);
  finally
    TestTearDown;
  end;
end;

// Suite setup and registration
procedure GetBaseDir;
var
  FN : string;
begin
  BaseDir:=IncludeTrailingPathDelimiter(GetTempDir)+'Dirwatch'+PathDelim;
  FN:=ExtractFilePath(ParamStr(0))+'config.ini';
  If FileExists(FN) then
    With TMemIniFile.Create(FN) do
      try
        BaseDir:=ReadString('dirwatch','basedir',BaseDir);
      finally
        Free;
      end;
end;

function SuiteSetup: string;
begin
  Result := '';
  GetBaseDir;
end;

procedure RegisterTests;
var
  lSuite : PSuite;
begin
  lSuite:=AddSuite('TDirWatchTests', @SuiteSetup, nil, nil, true);
  AddTest('TestHookUp', @TestDirWatch_TestHookUp, lSuite);
  AddTest('TestAddFile', @TestDirWatch_TestAddFile, lSuite);
  AddTest('TestAppendFile', @TestDirWatch_TestAppendFile, lSuite);
  AddTest('TestDeleteFile', @TestDirWatch_TestDeleteFile, lSuite);
  AddTest('TestLoopNoThread', @TestDirWatch_TestLoopNoThread, lSuite);
  AddTest('TestLoopThread', @TestDirWatch_TestLoopThread, lSuite);
  AddTest('TestAddFileBaseDir', @TestDirWatch_TestAddFileBaseDir, lSuite);
end;

end.
