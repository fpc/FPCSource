unit utdirwatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, dirwatch;

type
  TChangedEntry = record
    Dir : TWatchDirectoryEntry;
    Events : TWatchFileEvents;
    FN : String;
  end;
  TChangedEntryArray = Array of TChangedEntry;

  { TTestDirWatch }

  TTestDirWatch= class(TTestCase)
  private
    FDirWatch: TDirwatch;
    FTestDir: string;
    FChanged: TChangedEntryArray;
    FCheckCount : Integer;
    FMaxLoopCount : Integer;
    FDoCheckOne : TNotifyEvent;
    procedure AssertChange(const Msg: String; aIndex: Integer; aEntry: TWatchDirectoryEntry; aEvents: TWatchFileEvents; const aFileName : string = '');
    procedure CleanDirs(aDir: String);
    procedure DoAppendFile(const aName: string);
    procedure DoChange(Sender: TObject; const aEvent: TFileChangeEvent);
    procedure DoCheck(sender: TObject; var aContinue: Boolean);
    procedure DoCreateFile(const aName: string);
    procedure DoDeleteFile(const aName: string);
    procedure HandleCreateFile(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property dirwatch : TDirwatch read FDirWatch;
    Property TestDir : string Read FTestDir;
    property CheckCount : Integer Read FCheckCount;
    property MaxLoopCount : Integer Read FMaxLoopCount Write FMaxLoopCount;
  Public
    class procedure AssertEquals(const Msg: String; aExpected, aActual: TWatchFileEvents); overload;
  published
    procedure TestHookUp;
    procedure TestAddFile;
    procedure TestAppendFile;
    procedure TestDeleteFile;
    procedure TestLoopNoThread;
    procedure TestLoopThread;
    procedure TestAddFileBaseDir;
  end;

implementation

uses typinfo, inifiles;

var
  BaseDir : String;

procedure TTestDirWatch.CleanDirs(aDir: String);

Var
  Info : TSearchRec;
  lDir,lFull : String;

begin
  lDir:=IncludeTrailingPathDelimiter(aDir);
  If FIndFirst(lDir+AllFilesMask,sysutils.faDirectory,Info)=0 then
    try
      Repeat
        lFull:=lDir+Info.Name;
        if (Info.Attr and faDirectory)<>0 then
          begin
          if not ((Info.Name='.') or (Info.Name='..')) then
            begin
            CleanDirs(lFull);
            if not RemoveDir(lFull) then
              Fail('Failed to remove directory %s',[lFull])
            end;
          end
        else if not DeleteFIle(lFull) then
          Fail('Failed to remove file %s',[lFull])
      until FIndNext(Info)<>0;
    finally
      FindClose(Info);
    end;
end;

procedure TTestDirWatch.DoChange(Sender: TObject; const aEvent: TFileChangeEvent);
var
  Len : Integer;
begin
  len:=Length(FChanged);
  SetLength(FChanged,Len+1);
  FChanged[Len].Dir:=aEvent.Entry;
  FChanged[Len].Events:=aEvent.Events;
  FChanged[Len].FN:=aEvent.FileName;
end;

procedure TTestDirWatch.DoCheck(sender: TObject; var aContinue: Boolean);
begin
  aContinue:=CheckCount<MaxLoopCount;
  if (FCheckCount=0) then
    if Assigned(FDoCheckOne) then
       FDoCheckOne(Self);
  inc(FCheckCount);
end;

procedure TTestDirWatch.TestHookUp;
begin
  AssertNotNull('Have watch',Dirwatch);
  AssertEquals('No watches',0,Dirwatch.Watches.Count);
  AssertTrue('Have test dir',TestDir<>'');
  AssertTrue('test dir exists',DirectoryExists(TestDir));
  AssertEquals('No max check count',0,FMaxLoopCount);
  AssertEquals('No check count',0,FCheckCount);
  AssertTrue('No docheckone',FDoCheckOne=nil);
end;

procedure TTestDirWatch.DoAppendFile(const aName : string);

var
  FD : THandle;
begin
  FD:=FileOpen(TestDir+aName,fmOpenWrite);
  try
    FileSeek(FD,0,fsFromEnd);
    if FileWrite(FD,aName[1],Length(aName))=-1 then
      Writeln(GetLastOSError);
  finally
    FileClose(FD);
  end;
end;

procedure TTestDirWatch.DoCreateFile(const aName : string);

var
  L: TStrings;
begin
  L:=TStringList.Create;
  try
    L.Add(aName);
    L.SaveToFile(TestDir+aName);
  finally
    L.Free;
  end;
end;

procedure TTestDirWatch.DoDeleteFile(const aName: string);

begin
  If not DeleteFile(TestDir+aName) then
    Fail('Failed to delete file '+TestDir+aName);
end;

procedure TTestDirWatch.HandleCreateFile(Sender: TObject);
begin
  DoCreateFile('name.txt');
end;

class procedure TTestDirWatch.AssertEquals(const Msg: String; aExpected,aActual : TWatchFileEvents);

begin
  AssertEquals(Msg,SetToString(PTypeInfo(TypeInfo(TWatchFileEvents)),Longint(aExpected),False),
                   SetToString(PTypeInfo(TypeInfo(TWatchFileEvents)),Longint(aActual),False));
end;

procedure TTestDirWatch.AssertChange(const Msg: String; aIndex: Integer; aEntry: TWatchDirectoryEntry; aEvents: TWatchFileEvents;
  const aFileName: string);
var
  M : String;
begin
  M:=Msg+Format(' [%d]: ',[aIndex]);
  AssertTrue(M+'correct index',aIndex<Length(FChanged));
  AssertSame(M+'correct dir entry',aEntry,FChanged[aIndex].Dir);
  AssertEquals(M+'correct changes',aEvents,FChanged[aIndex].Events);
  if aFileName<>'' then
    AssertEquals(M+'correct fileName',aFileName,FChanged[aIndex].FN);
end;

procedure TTestDirWatch.TestAddFile;
begin
  FDirwatch.AddWatch(TestDir,[feCreate]);
  FDirWatch.InitWatch;
  DoCreateFile('name.txt');
  AssertEquals(1,FDirWatch.Check);
  AssertChange('Create',0,FDirWatch.Watches[0],[feCreate],'name.txt');
end;

procedure TTestDirWatch.TestAppendFile;
begin
  FDirwatch.AddWatch(TestDir,[feModify]);
  DoCreateFile('name.txt');
  FDirWatch.InitWatch;
  DoAppendFile('name.txt');
  AssertEquals('Change detected',1,FDirWatch.Check);
  AssertChange('Change detected',0,FDirWatch.Watches[0],[feModify],'name.txt');
end;


procedure TTestDirWatch.TestDeleteFile;
begin
  FDirwatch.AddWatch(TestDir,[feDelete]);
  DoCreateFile('name.txt');
  FDirWatch.InitWatch;
  DoDeleteFile('name.txt');
  AssertEquals('Change detected',1,FDirWatch.Check);
  AssertChange('Change detected',0,FDirWatch.Watches[0],[feDelete],'name.txt');
end;

procedure TTestDirWatch.TestLoopNoThread;
begin
  FDirwatch.AddWatch(TestDir,[feCreate]);
  FDirwatch.OnCheck:=@DoCheck;
  FDoCheckOne:=@HandleCreateFile;
  MaxLoopCount:=2;
  FDirWatch.StartLoop;
  AssertChange('Change detected',0,FDirWatch.Watches[0],[feCreate],'name.txt');
end;

procedure TTestDirWatch.TestLoopThread;
var
  I : Integer;
begin
  FDirwatch.AddWatch(TestDir,[feCreate]);
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
end;

procedure TTestDirWatch.TestAddFileBaseDir;
begin
  FDirwatch.BaseDir:=TestDir;
  AssertTrue('Create Subdir ',ForceDirectories(TestDir+'sub'));
  FDirwatch.AddWatch('',[feCreate]);
  FDirWatch.InitWatch;
  DoCreateFile('sub/name.txt');
  AssertEquals('Subdirs not watched',0,FDirWatch.Check);
end;

procedure TTestDirWatch.SetUp;
begin
  FDirWatch:=TDirwatch.Create(Nil);
  FTestDir:=IncludeTrailingPathDelimiter(BaseDir);
  ForceDirectories(TestDir);
  FDirWatch.OnChange:=@DoChange;
  FMaxLoopCount:=0;
  FCheckCount:=0;
  FDoCheckOne:=Nil;
end;

procedure TTestDirWatch.TearDown;
begin
  CleanDirs(TestDir);
  FDirWatch.Free;
end;

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

initialization
  GetBaseDir;
  RegisterTest(TTestDirWatch);
end.

