{ **********************************************************************
    This file is part of the Free Pascal run time library.
    Copyright (c) 2024 by the Free Pascal development team

    File/Directory watch component.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit dirwatch;

{ $define forcegeneric}

{$IFDEF FORCEGENERIC}
{$DEFINE USEGENERIC}
{$DEFINE USEDIRLIST}
{$ELSE FORCEGENERIC}
{$IFDEF LINUX}
{$DEFINE USEINOTIFY}
{$ELSE LINUX}
{$IFDEF MSWINDOWS}
{$DEFINE USEWINAPI}
{$DEFINE USEDIRLIST}
{$ELSE MSWINDOWS}
{$IFDEF BSD}
{$DEFINE USEKQUEUE}
{$DEFINE USEDIRLIST}
{$ELSE}
{$DEFINE USEGENERIC}
{$ENDIF BSD}
{$ENDIF MSWINDOWS}
{$ENDIF LINUX}
{$ENDIF FORCEGENERIC}

// Safety
{$ifdef USEGENERIC}
{$DEFINE USEDIRLIST}
{$ENDIF}

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}  
  {$IFDEF USEDIRLIST}
  System.Types, System.StrUtils,System.Contnrs,
  {$ENDIF}
  System.Classes, System.SysUtils;
{$ELSE}
  {$IFDEF USEDIRLIST}
  types, strutils,contnrs,
  {$ENDIF}
  Classes, SysUtils;
{$ENDIF}

const
  // Default check timeout
  DefaultCheckTimeout = 10;
  // Default loop interval
  DefaultLoopInterval = 90;

Type
  TWatchFileEvent = (feModify,feAttrib,feCreate,feDelete);
  TWatchFileEvents = set of TWatchFileEvent;
  EDirWatch = Class(Exception);
  TCustomDirwatch = Class;

  { TWatchDirectoryEntry }
  TWatchDirectoryEntry = Class(TCollectionItem)
  private
    FDriverData: TObject;
    FEvents: TWatchFileEvents;
    FPath: String;
  Protected
    function GetDisplayName: string; override;
   // when set, it will be destroyed when this object is destroyed
   Property DriverData : TObject Read FDriverData Write FDriverData;
  Public
    destructor destroy; override;
  Published
    // FileName/Dirname to watch. Relative to BaseDir
    Property Path : String Read FPath Write FPath;
    // Events to be notified of for this directory.
    Property Events : TWatchFileEvents Read FEvents Write FEvents;
  end;

  { TWatchDirectoryEntries }

  TWatchDirectoryEntries = Class(TCollection)
  private
    function GetE(AIndex : Integer): TWatchDirectoryEntry;
    procedure SetE(AIndex : Integer; AValue: TWatchDirectoryEntry);
  Public
    Function IndexOfEntry(Const APath : String) : Integer;
    function EntryByPath(const APath: String): TWatchDirectoryEntry;
    Function AddEntry(Const APath : String) : TWatchDirectoryEntry;
    Property Entries[AIndex : Integer] : TWatchDirectoryEntry Read GetE Write SetE; default;
  end;

  TFileChangeEvent = record
    Entry : TWatchDirectoryEntry;
    FileName : string;
    Events : TWatchFileEvents;
  end;

  TWatchFileEventHandler = procedure (Sender : TObject; const aEvent : TFileChangeEvent) of Object;

  { TDirWatchDriver }

  TDirWatchDriver = Class(TObject)
  private
    FTerminate: Boolean;
    FWatch: TCustomDirWatch;
    FTerminated : Boolean;
  protected
    function DoCheck : cardinal; virtual; abstract;
  public
    Constructor Create(aWatch : TCustomDirwatch); virtual;
    procedure Init; virtual; abstract;
    procedure Done; virtual; abstract;
    procedure Terminate;
    function Check : cardinal;
    property Watch : TCustomDirWatch read FWatch;
    property Terminated : Boolean Read FTerminate;
  end;
  TDirWatchDriverClass = class of TDirwatchDriver;
  { TDirwatch }

  TNotifyCheckEvent = Procedure (sender: TObject; var aContinue : Boolean) of object;

  { TCustomDirwatch }

  TCustomDirwatch = Class(TComponent)
  private
    FCheckTimeout: Cardinal;
    FLoopInterval: Cardinal;
    FOnCheck: TNotifyCheckEvent;
    FOnIdle: TNotifyEvent;
    FOnIdleNotify: TNotifyEvent;
    FTerminated: Boolean;
    FThreaded: Boolean;
    FWatches: TWatchDirectoryEntries;
    FBaseDir: String;
    FOnChange: TWatchFileEventHandler;
    FDriver : TDirwatchDriver;
    FInitOK : Boolean;
    FDesignEnabled : Boolean;
    FThread: TThread;
    function GetEnabled: Boolean;
    procedure SetBaseDir(AValue: String);
    procedure SetEnabled(AValue: Boolean);
    procedure SetThreaded(AValue: Boolean);
  Protected
    Class var
       DefaultDriverClass : TDirwatchDriverClass;
    procedure DoChangeEvent(const aEvent: TFileChangeEvent);
    procedure DoIdle; virtual;
    function DoCheck : cardinal; virtual;
    procedure DoStartWatch; virtual;
    procedure Loaded; override;
    function DirectoryEntryForFileName(S: String): TWatchDirectoryEntry;
  Public
    Constructor Create(AOWner : TComponent); override;
    Destructor Destroy; override;
    procedure InitWatch; virtual;
    procedure DoneWatch; virtual;
    function Check : cardinal;
    Procedure StartLoop;
    Procedure AddWatch(const aFileName : string; aEvents : TWatchFileEvents);
    Procedure Terminate;
  Protected
    // Was Terminate called ? If yes, the loop is stopped
    Property Terminated : Boolean Read FTerminated;
    // Set this to True to start the WatchLoop
    property Enabled : Boolean Read GetEnabled write SetEnabled;
    // Is the watch loop run in a thread ?
    Property Threaded : Boolean Read FThreaded Write SetThreaded;
    // Base directory. All filenames are relative to this directory. Setting it will clear watches.
    Property BaseDir : String read FBaseDir Write SetBaseDir;
    // Timeout when checking for changes.
    Property CheckTimeout : Cardinal Read FCheckTimeout Write FCheckTimeout default DefaultCheckTimeout;
    // Loop interval: interval between checks.
    Property LoopInterval : Cardinal Read FLoopInterval Write FLoopInterval default DefaultLoopInterval;
    // A list of directories or files to watch.
    Property Watches : TWatchDirectoryEntries Read FWatches Write FWatches;
    // Triggered when a change is detected.
    Property OnChange : TWatchFileEventHandler Read FOnChange Write FOnChange;
    // Called when loop is idle
    Property OnIdle : TNotifyEvent Read FOnIdle Write FOnIdleNotify;
    // Called before the check call is executed. If continue is set to false, the loop is terminated
    Property OnCheck : TNotifyCheckEvent Read FOnCheck Write FOnCheck;
  end;

  TDirWatch = class(TCustomDirwatch)
  Public
    Property Terminated;
  Published
    property Enabled;
    Property BaseDir;
    Property OnChange;
    Property Threaded;
    Property Watches;
    Property OnIdle;
    Property OnCheck;
    Property CheckTimeOut;
  end;

{$IFDEF USEDIRLIST}

  TDirListDriver = Class(TDirWatchDriver)
  Protected
  Type
    TDirData = class
    Private
      FEntry : TWatchDirectoryEntry;
      FReference : TFPStringHashTable;
      FInitDir : String;
      FCount : Integer;
      FWatch : TCustomDirwatch;
    Protected
      procedure ConstructList(const aBaseDir, aDir: String; aList: TFPStringHashTable);
      procedure DoCheckItem(Item: String; const Key: string; var Continue: Boolean);
      procedure DoDeletedItem(Item: String; const Key: string; var Continue: Boolean);
      procedure InitWatch;
      function Check : cardinal;
    Public
      constructor Create(aWatch : TCustomDirwatch; aEntry : TWatchDirectoryEntry);
      Destructor Destroy; override;
      Property Entry : TWatchDirectoryEntry Read FEntry;
      Property Watch : TCustomDirwatch Read FWatch;
    end;
  end;

Function SearchRecToString(Info : TSearchRec; AEvents : TWatchFileEvents) : String;

{$ENDIF}

Const
  EventNames : Array[TWatchFileEvent] of string = ('Modify','Attrib','Create','Delete');
  AllEvents =   [feModify,feAttrib,feCreate,feDelete];

Function FileEventsToStr(Events : TWatchFileEvents) : String;

implementation

{$IFDEF USEINOTIFY}
{$INCLUDE dwinotify.inc}
{$ENDIF}
{$IFDEF USEWINAPI}
{$INCLUDE dwwinapi.inc}
{$ENDIF}
{$IFDEF USEGENERIC}
{$INCLUDE dwgeneric.inc}
{$ENDIF}
{$IFDEF USEKQUEUE}
{$INCLUDE dwkqueue.inc}
{$ENDIF}


Function FileEventsToStr(Events : TWatchFileEvents) : String;

Var
  E : TWatchFileEvent;

begin
  Result:='';
  for E in Events do
    begin
    if Result<>'' then
      Result:=Result+',';
    Result:=Result+EventNames[E];
    end;

end;

{ TCustomDirwatch }
Type

  { TCustomDirwatchThread }

  TCustomDirwatchThread = class(TThread)
  Private
    FDir:TCustomDirwatch;
  Public
    Constructor Create(ADirwatch : TCustomDirwatch);
    Procedure Execute; override;
  end;




{ TCustomDirwatchThread }

constructor TCustomDirwatchThread.Create(ADirwatch: TCustomDirwatch);

begin
  FDir:=ADirWatch;
  FreeOnTerminate:=True;
  inherited create(False);
end;

procedure TCustomDirwatchThread.Execute;
begin
  FDir.DoStartWatch;
end;


procedure TCustomDirwatch.SetBaseDir(AValue: String);
begin
  if FBaseDir=AValue then Exit;
  FBaseDir:=AValue;
  FWatches.Clear;
end;

procedure TCustomDirwatch.SetEnabled(AValue: Boolean);
begin
  if (csDesigning in ComponentState) then
    FDesignEnabled:=aValue
  else
    begin
    if aValue then
      begin
      if not FInitOK then
        StartLoop;
      end
    else
      Terminate;
    end;
end;

procedure TCustomDirwatch.SetThreaded(AValue: Boolean);
begin
  if FThreaded=AValue then Exit;
  if FInitOK then
    Raise EDirWatch.Create('Cannot change threaded after calling InitWatch');
  FThreaded:=AValue;
end;

constructor TCustomDirwatch.Create(AOWner: TComponent);
begin
  inherited Create(AOWner);
  FWatches:=TWatchDirectoryEntries.Create(TWatchDirectoryEntry);
  FDriver:=DefaultDriverClass.Create(Self);
  FCheckTimeOut:=DefaultCheckTimeout;
  FLoopInterval:=DefaultLoopInterval;
end;

destructor TCustomDirwatch.Destroy;
begin
  if FInitOK then
    begin
    if Threaded then
      Terminate;
    DoneWatch;
    end;
  FreeAndNil(FDriver);
  FreeAndNil(FWatches);
  inherited Destroy;
end;

Type
  { TCustomDirwatchChange }
  TCustomDirwatchChange = Class
    FDirWatch : TCustomDirwatch;
    FEvent : TFileChangeEvent;
    constructor Create(aDirWatch: TCustomDirwatch; aEvent: TFileChangeEvent);
    Procedure DoEvent;
 end;

{ TCustomDirwatchChange }

constructor TCustomDirwatchChange.Create(aDirWatch : TCustomDirwatch; aEvent: TFileChangeEvent);

begin
  FEvent:=aEvent;
  FDirWatch:=ADirWatch;
end;

procedure TCustomDirwatchChange.DoEvent;

begin
  if not FDirWatch.Terminated then
    FDirwatch.FonChange(FDirwatch,FEvent);
end;

procedure TCustomDirwatch.DoChangeEvent(const aEvent :TFileChangeEvent);

Var
  W : TCustomDirwatchChange;

begin
  if Assigned(FOnChange) then
    if Not Threaded then
      FonChange(Self,aEvent)
    else
      begin
      W:=TCustomDirwatchChange.Create(Self,aEvent);
      try
        TThread.Synchronize(TThread.CurrentThread,@W.DoEvent)
      finally
        W.Free;
      end;
      end
end;

function TCustomDirwatch.GetEnabled: Boolean;
begin
  if csDesigning in ComponentState then
    Result:=FDesignEnabled
  else
    Result:=FInitOK;
end;


procedure TCustomDirwatch.DoIdle;

begin
  if Assigned(FOnIdle) then
    FOnIdle(Self);
end;

function TCustomDirwatch.Check : cardinal;
var
  Continue : Boolean;
begin
  Result:=0;
  if not FInitOK then
    InitWatch;
  Continue:=True;
  If Assigned(FOnCheck) then
    FOnCheck(Self,Continue);
  if Continue then
    Result:=DoCheck
  else
    Terminate;
end;

function TCustomDirwatch.DoCheck: cardinal;
begin
  Result:=FDriver.Check;
end;

procedure TCustomDirwatch.DoneWatch;
begin
  if assigned(FDriver) then
    FDriver.Done;
  FInitOK:=False;
end;


function TCustomDirwatch.DirectoryEntryForFileName(S: String): TWatchDirectoryEntry;

begin
  Result:=FWatches.EntryByPath(S);
  if (Result=Nil) then
    Result:=FWatches.EntryByPath(ExtractFilePath(S));
end;

procedure TCustomDirwatch.DoStartWatch;

begin
  InitWatch;
  try
    While not Terminated do
      begin
      Check;
      if not Terminated then
        begin
        if Threaded then
          TThread.Synchronize(TThread.CurrentThread,@DoIdle)
        else
          DoIdle;
        end;
      if (LoopInterval>0) and not Terminated then
        Sleep(LoopInterval);
      end;
  Finally
    DoneWatch;
  end;
end;

procedure TCustomDirwatch.Loaded;
begin
  inherited Loaded;
  if FDesignEnabled then
    StartLoop;
end;

procedure TCustomDirwatch.InitWatch;
begin
  FDriver.Init;
  FInitOK:=True;
  FTerminated:=False;
end;

procedure TCustomDirwatch.StartLoop;

begin
  If Threaded then
    FThread:=TCustomDirwatchThread.Create(Self)
  else
    DoStartWatch;
end;

procedure TCustomDirwatch.AddWatch(const aFileName: string; aEvents: TWatchFileEvents);
begin
  FWatches.AddEntry(AFileName).Events:=AEvents;
end;

procedure TCustomDirwatch.Terminate;
Var
  lThread : TThread;
begin
  FDriver.Terminate;
  FTerminated:=True;
  if assigned(FThread) then
    begin
    lThread:=FThread;
    FThread:=Nil;
    TCustomDirwatchThread(lThread).FDir:=nil;
    lThread.Terminate;
    lThread.WaitFor;
    end;
end;

function TWatchDirectoryEntry.GetDisplayName: string;
begin
  Result:=Path;
end;

destructor TWatchDirectoryEntry.destroy;
begin
  FreeAndNil(FDriverData);
  inherited destroy;
end;

{ TWatchDirectoryEntries }

function TWatchDirectoryEntries.GetE(AIndex : Integer): TWatchDirectoryEntry;
begin
  Result:=TWatchDirectoryEntry(Items[AIndex]);
end;

procedure TWatchDirectoryEntries.SetE(AIndex : Integer; AValue: TWatchDirectoryEntry);
begin
  Items[AIndex]:=AValue;
end;

function TWatchDirectoryEntries.IndexOfEntry(const APath: String): Integer;

begin
  Result:=Count-1;
  While (Result>=0) and (GetE(Result).Path<>APath) do
    Dec(Result);
end;

function TWatchDirectoryEntries.EntryByPath(const APath: String): TWatchDirectoryEntry;

Var
  I : Integer;

begin
  I:=IndexOfEntry(APath);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetE(I);
end;

function TWatchDirectoryEntries.AddEntry(Const APath: String): TWatchDirectoryEntry;
begin
  Result:=Add as TWatchDirectoryEntry;
  Result.Path:=aPath;
end;

{ TCustomDirwatchDriver }

constructor TDirWatchDriver.Create(aWatch: TCustomDirwatch);
begin
  FWatch:=aWatch;
end;

procedure TDirWatchDriver.Terminate;
begin
  FTerminated:=True;
end;

function TDirWatchDriver.Check: cardinal;
begin
  FTerminated:=False;
  Result:=DoCheck;
end;

{$IFDEF USEDIRLIST}
const
  // Parts of string
  pSize = 0;
  pTime = 1;
  pAttr = 2;

Function SearchRecToString(Info : TSearchRec; AEvents : TWatchFileEvents) : String;

begin
  Result:=IntToStr(Info.Size)+';'+IntToStr(Info.Time)+';'+IntToStr(Info.Attr);
end;

constructor TDirListDriver.TDirData.Create(aWatch : TCustomDirwatch;aEntry: TWatchDirectoryEntry);
begin
  FWatch:=aWatch;
  FEntry:=aEntry;
end;

destructor TDirListDriver.TDirData.Destroy;
begin
  FreeAndNil(FReference);
  inherited Destroy;
end;

procedure TDirListDriver.TDirData.ConstructList(const aBaseDir,aDir: String; aList : TFPStringHashTable);

var
  Info : TSearchRec;
  FN,FFN,RFN,ldata : String;
begin
  FN:=aDir+AllFilesMask;
  if FindFirst(FN,faAnyFile,Info)=0 then
    try
      Repeat
        FFN:=aDir+Info.Name;
        RFN:=ExtractRelativePath(aBaseDir,FFN);
        if (faDirectory and Info.Attr) = 0 then
          begin
          lData:=SearchRecToString(Info,FEntry.Events);
          AList.Add(RFN,lData);
          end
      until FindNext(Info)<>0;
    finally
      Sysutils.FindClose(Info);
    end;
end;

procedure TDirListDriver.TDirData.InitWatch;

Var
  FN : String;

begin
  if (Watch.BaseDir<>'') then
    FN:=IncludeTrailingPathDelimiter(Watch.BaseDir)+Entry.Path
  else
    FN:=Entry.Path;
  FReference:=TFPStringHashTable.Create;
  ConstructList(FN,FN,FReference);
  FInitDir:=FN;
end;

function TDirListDriver.TDirData.Check: Cardinal;

var
  FNew : TFPStringHashTable;

begin
  FCount:=0;
  Result:=0;
  FNew:=TFPStringHashTable.Create;
  try
    ConstructList(FInitDir,FInitDir,FNew);
    if Watch.Terminated then
      begin
      FreeAndNil(FNew);
      exit;
      end;
    // doCheckItem removes seen files from FReference
    FNew.Iterate(@doCheckItem);
    // Whatever is left in FReference was deleted
    if FReference.Count>0 then
      FReference.Iterate(@doDeletedItem);
    FreeAndNil(FReference);
    FReference:=FNew;
    FNew:=Nil;
  except
    FNew.Free;
    Raise;
  end;
  Result:=FCount;
  FCount:=0;
end;

procedure TDirListDriver.TDirData.DoDeletedItem(Item: String; const Key: string; var Continue: Boolean);

Var
  lEvent : TFileChangeEvent;

begin
  lEvent.Entry:=Self.Entry;
  lEvent.FileName:=Key;
  lEvent.Events:=[feDelete];
  Watch.DoChangeEvent(lEvent);
  Inc(FCount);
  Continue:=Not Watch.Terminated;
end;

procedure TDirListDriver.TDirData.DoCheckItem(Item: String; const Key: string; var Continue: Boolean);

Var
  S : String;
  lNewParts,lOldParts : TStringDynArray;
  E : TWatchFileEvents;
  lInfo : TFileChangeEvent;

begin
  E:=[];
  S:=FReference[Key];
  if (S='') then
    begin
    E:=[feCreate];
    end
  else
    begin
    FReference.Delete(Key);
    if (S<>Item) then
      begin
      lNewParts:=SplitString(Item,';');
      lOldParts:=SplitString(S,';');
      if (feAttrib in Entry.Events) then
        if lNewParts[pAttr]<>lOldParts[pAttr] then
          Include(E,feAttrib);
      if (feModify in Entry.Events) then
         begin
         if (lNewParts[pTime]<>lOldParts[pTime]) or
            (lNewParts[pSize]<>lOldParts[pSize]) then
            Include(E,feModify);
         end;
      end;
    end;
  if E<>[] then
    begin
    Inc(FCount);
    lInfo.Entry:=Entry;
    lInfo.FileName:=Key;
    lInfo.Events:=E;
    Watch.DoChangeEvent(lInfo);
    end;
  Continue:=Not Watch.Terminated;
end;

{$ENDIF}

initialization
{$IFDEF USEGENERIC}
  TCustomDirwatch.DefaultDriverClass:=TGenericDriver;
{$ENDIF}
{$IFDEF USEINOTIFY}
  TCustomDirwatch.DefaultDriverClass:=TINotifyDriver;
{$ENDIF}
{$IFDEF USEWINAPI}
  TCustomDirwatch.DefaultDriverClass:=TWinAPIDriver;
{$ENDIF}
{$IFDEF USEKQUEUE}
  TCustomDirwatch.DefaultDriverClass:=TKQueueDriver;
{$ENDIF}
end.

