unit dirwatch;
{$IFDEF LINUX}
{$DEFINE USEINOTIFY}
{$ELSE}
{$DEFINE USEGENERIC}
{$ENDIF}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
{$IFDEF UNIX}
  baseunix,
{$IFDEF USEINOTIFY}
  ctypes,
  linux,
{$ENDIF}
{$ENDIF}
  contnrs;



Type
  TFileEvent = (feModify,feAttrib,feCreate,feDelete);
  TFileEvents = set of TFileEvent;

  { TDirectoryEntry }
  TDirectoryEntry = Class(TCollectionItem)
  private
    FEvents: TFileEvents;
    FName: String;
    FAttributes: Integer;
{$IFDEF UNIX}
    FGroup: gid_t;
    FMode: mode_t;
    FOwner: uid_t;
{$ENDIF}
    FSize: Int64;
    FTimeStamp: TDateTime;
  Protected
{$IFDEF USEGENERIC}
    procedure InitWatch(ABaseDir: String; AList: TFPStringHashTable);
{$ENDIF}
  Public
    Property TimeStamp : TDateTime Read FTimeStamp Write FTimeStamp;
    Property Size : Int64 Read FSize Write FSize;
    Property Attributes : Integer Read FAttributes Write FAttributes;
{$IFDEF UNIX}
    Property Mode : mode_t Read FMode Write FMode;
    Property Owner : uid_t Read FOwner Write FOwner;
    Property Group : gid_t Read FGroup Write FGroup;
{$ENDIF}
  Published
    Property Name : String Read FName Write FName;
    Property Events : TFileEvents Read FEvents Write FEvents;
  end;

  { TDirectoryEntries }

  TDirectoryEntries = Class(TCollection)
  private
    function GetE(AIndex : Integer): TDirectoryEntry;
    procedure SetE(AIndex : Integer; AValue: TDirectoryEntry);
  Public
    Function IndexOfEntry(Const AName : String) : Integer;
    Function EntryByName(Const AName : String) : TDirectoryEntry;
    Function AddEntry(Const AName : String) : TDirectoryEntry;
    Property Entries[AIndex : Integer] : TDirectoryEntry Read GetE Write SetE; default;
  end;

  TFileEventHandler = procedure (Sender : TObject; aEntry : TDirectoryEntry; AEvents : TFileEvents) of Object;

  { TDirwatch }

  TDirwatch = Class(TComponent)
  private
    FIdleInterval: Cardinal;
    FOnIdle: TNotifyEvent;
    FOnIdleNotify: TNotifyEvent;
    FTerminated: Boolean;
    FThreaded: Boolean;
    FWatches: TDirectoryEntries;
    FBaseDir: String;
    FOnChange: TFileEventHandler;
{$IFDEF USEGENERIC}
    FReference : TFPStringHashTable;
    FOldReference : TFPStringHashTable;
    procedure DoCheckItem(Item: String; const Key: string; var Continue: Boolean);
    procedure DoDeletedItem(Item: String; const Key: string; var Continue: Boolean);
{$ENDIF}
{$IFDEF USEINOTIFY}
    FINotifyFD : Cint;
{$ENDIF}
    function DirectoryEntryForFileName(S: String): TDirectoryEntry;
    procedure DoChangeEvent(Entry: TDirectoryEntry; Events: TFileEvents);
    procedure SetBaseDir(AValue: String);
  Protected
    procedure DoIdle; virtual;
    procedure Check; virtual;
    procedure DoneWatch; virtual;
    procedure DoStartWatch; virtual;
    procedure InitWatch;virtual;
  Public
    Constructor Create(AOWner : TComponent); override;
    Destructor Destroy; override;
    Procedure StartWatch;
    Procedure AddWatch(const aFileName : string; aEvents : TFileEvents);
    Procedure Terminate;
    Property Terminated : Boolean Read FTerminated;
  Published
    Property BaseDir : String read FBaseDir Write SetBaseDir;
    Property OnChange : TFileEventHandler Read FOnChange Write FOnChange;
    Property Threaded : Boolean Read FThreaded Write FThreaded;
    Property Watches : TDirectoryEntries Read FWatches Write FWatches;
    Property OnIdle : TNotifyEvent Read FOnIdle Write FOnIdleNotify;
    Property IdleInterval : Cardinal Read FIdleInterval Write FIdleInterval;
  end;

Const
  EventNames : Array[TFileEvent] of string = ('Modify','Attrib','Create','Delete');
  AllEvents =   [feModify,feAttrib,feCreate,feDelete];

Function FileEventsToStr(Events : TFileEvents) : String;

implementation


Function FileEventsToStr(Events : TFileEvents) : String;

Var
  E : TFileEvent;

begin
  Result:='';
  for E in Events do
    begin
    if Result<>'' then
      Result:=Result+',';
    Result:=Result+EventNames[E];
    end;

end;

{ TDirwatch }
Type

  { TDirwatchThread }

  TDirwatchThread = class(TThread)
  Private
    FDir:TDirWatch;
  Public
    Constructor Create(ADirwatch : TDirWatch);
    Procedure Execute; override;
  end;

{ TDirectoryEntry }

Function SearchRecToString(Info : TSearchRec; AEvents : TFileEvents) : String;

begin
  if feAttrib in AEvents then
    Result:=IntToStr(Info.Attr)
  else
    Result:='';
  Result:=Result+';'+IntToStr(Info.Size)+';'+IntToStr(Info.Time);
end;

{$IFDEF USEGENERIC}
procedure TDirectoryEntry.InitWatch(ABaseDir: String; AList: TFPStringHashTable);

Var
  Info : TSearchRec;
  FN : String;

begin
  if (ABaseDir<>'') then
    FN:=IncludeTrailingPathDelimiter(ABaseDir)+Name
  else
    FN:=Name;
  if FindFirst(FN,faAnyFile,Info)=0 then
    begin
    if (faDirectory and Info.Attr) = 0 then
      begin
      AList.Add(FN,SearchRecToString(Info,Self.Events))
      end
    else
      begin
      FindClose(Info);
      FN:=IncludeTrailingPathDelimiter(FN);
      if FindFirst(FN+AllFilesMask,0,Info)=0  then
        Repeat
          if (info.Name<>'.') and (Info.Name<>'..') then
            AList.Add(FN+Info.Name,SearchRecToString(Info,Self.Events));
        until (FindNext(Info)<>0)
      end;
    FindClose(Info);
    end
end;

{$ENDIF}
{$IFDEF USEINOTIFY}

{$ENDIF}
{ TDirwatchThread }

constructor TDirwatchThread.Create(ADirwatch: TDirWatch);

begin
  FDir:=ADirWatch;
  FreeOnTerminate:=True;
  inherited create(False);
end;

procedure TDirwatchThread.Execute;
begin
  FDir.DoStartWatch;
end;


procedure TDirwatch.SetBaseDir(AValue: String);
begin
  if FBaseDir=AValue then Exit;
  FBaseDir:=AValue;
  FWatches.Clear;
end;

constructor TDirwatch.Create(AOWner: TComponent);
begin
  inherited Create(AOWner);
  FWatches:=TDirectoryEntries.Create(TDirectoryEntry);
  FidleInterval:=100;
end;

destructor TDirwatch.Destroy;
begin
  FreeAndNil(FWatches);
  inherited Destroy;
end;

Type
  { TDirwatchChange }
  TDirwatchChange = Class
    FEntry : TDirectoryEntry;
    FEvents : TFileEvents;
    FDirWatch : TDirWatch;
    Constructor Create(AEntry : TDirectoryEntry;aEvents : TFileEvents;ADirWatch : TDirWatch);
    Procedure DoEvent;
 end;

{ TDirwatchChange }

constructor TDirwatchChange.Create(AEntry: TDirectoryEntry; aEvents: TFileEvents; ADirWatch: TDirWatch);

begin
  FEntry:=AEntry;
  FEvents:=AEvents;
  FDirWatch:=ADirWatch;
end;

procedure TDirwatchChange.DoEvent;
begin
  FDirwatch.FonChange(FDirwatch,FEntry,FEvents);
end;

Procedure TDirwatch.DoChangeEvent(Entry : TDirectoryEntry; Events : TFileEvents);

Var
  W : TDirWatchChange;

begin
  try
    if Assigned(FOnChange) then
      if Not Threaded then
        FonChange(Self,Entry,Events)
      else
        begin
        W:=TDirWatchChange.Create(Entry,Events,Self);
        try
          TThread.Synchronize(TThread.CurrentThread,@W.DoEvent)
        finally
          W.Free;
        end;
        end
  Finally
    // Specially created
    if Entry.Collection=Nil then
      FreeAndNil(Entry);
  end;
end;


procedure TDirwatch.DoIdle;

begin
  if Assigned(FOnIdle) then
    FOnIdle(Self);
end;

Function TDirwatch.DirectoryEntryForFileName(S : String) : TDirectoryEntry;

begin
  Result:=FWatches.EntryByName(S);
  if (Result=Nil) then
    Result:=FWatches.EntryByName(ExtractFilePath(S));
  if (Result=Nil) then
    begin
    Result:=TDirectoryEntry.Create(Nil);
    Result.Name:=S;
    end;
end;

{$IFDEF USEGENERIC}
procedure TDirwatch.DoneWatch;

begin
  FreeAndNil(FReference);
end;

procedure TDirwatch.InitWatch;

Var
  I : Integer;

begin
  FReference:=TFPStringHashTable.Create;
  For I:=0 to FWatches.Count-1 do
    FWatches[i].InitWatch(BaseDir,FReference);
end;

procedure TDirwatch.DoDeletedItem(Item: String; const Key: string; var Continue: Boolean);

Var
  DE : TDirectoryEntry;

begin
  DE:=FWatches.EntryByName(Key);
  if (DE=Nil) then
    DE:=FWatches.EntryByName(ExtractFilePath(Key));
  if (DE=Nil) then
    begin
    DE:=TDirectoryEntry.Create(Nil);
    DE.Name:=Key;
    end;
  DoChangeEvent(DE,[feDelete]);
  Continue:=False;
end;

procedure TDirwatch.DoCheckItem(Item: String; const Key: string; var Continue: Boolean);

Var
  S : String;
  E : TFileEvents;
  DE : TDirectoryEntry;

begin
//  Writeln('check file: ',key,' attrs : ',Item);
  E:=[];
  S:=FOldReference[Key];
  if (S='') then
    E:=[feCreate]
  else
    begin
    FOldReference.Delete(Key);
    if (S<>Item) then
      E:=[feAttrib];
    end;
  if E<>[] then
    begin
    DE:=DirectoryEntryForFileName(Key);
    DoChangeEvent(DE,E);
    Continue:=False;
    end;
end;

procedure TDirwatch.Check;

begin
  FOldReference:=FReference;
  try
    FReference:=TFPStringHashTable.Create;
    InitWatch;
    FReference.Iterate(@doCheckItem);
    if FoldReference.Count>0 then
      FReference.Iterate(@doDeletedItem);
      // Deleted files
    Sleep(IdleInterval);
  finally
    FreeAndNil(FoldReference);
  end;
end;
{$ENDIF}

{$IFDEF USEINOTIFY}
Procedure WatchDirectory(d : string);

Const
  Events = IN_MODIFY or IN_ATTRIB or IN_CREATE or IN_DELETE;

Var
  fd, wd,fnl,len : cint;
  fds : tfdset;
  e : ^inotify_event;
  buf : Array[0..1023*4] of Byte; // 4K Buffer
  fn : string;
  p : pchar;

begin
  fd:=inotify_init;
  try
    wd:=inotify_add_watch(fd,pchar(d),Events);
    fpFD_Zero(fds);
    fpFD_SET(fd,fds);
    While (fpSelect(fd+1,@fds,nil,nil,nil)>=0) do
      begin
      len:=fpRead(fd,buf,sizeof(buf));
      e:=@buf;
      While ((pchar(e)-@buf)<len) do
        begin
        fnl:=e^.len;
        if (fnl>0) then
          begin
          p:=@e^.name+fnl-1;
          While (p^=#0) do
            begin
            dec(p);
            dec(fnl);
            end;
          end;
        setlength(fn,fnl);
        if (fnl>0) then
          move(e^.name,fn[1],fnl);
        {$ifdef VerboseDirWatch}
        Writeln('Change ',e^.mask,' (',
//                InotifyEventsToString(e^.mask),
                ') detected for file "',fn,'"');
        {$endif}
        ptrint(e):=ptrint(e)+sizeof(inotify_event)+e^.len-1;
        end;
      end;
  finally
    fpClose(fd);
  end;
end;

procedure TDirwatch.DoneWatch;

begin
  fpClose(FInotifyFD);
end;

procedure TDirwatch.InitWatch;

Const
  NativeEvents : Array[TFileEvent] of cint = (IN_Modify,IN_Attrib,IN_Create,IN_Delete);

Var
  WD,I,NEvents : Integer;
  E : TFileEvent;
  BD,FN : String;

begin
  BD:=BaseDir;
  if BD<>'' then
    BD:=IncludeTrailingPathDelimiter(BD);
  FINotifyFD:=inotify_init;
  For I:=0 to FWatches.Count-1 do
    begin
    NEvents:=0;
    for E in FWatches[i].Events do
      NEvents:=NEvents OR NativeEvents[E];
    FN:=BD+FWatches[i].Name;
    wd:=inotify_add_watch(FINotifyFD,PChar(FN),NEvents);
    end;
end;

Function NativeEventsToEvents(Native : cint) : TFileEvents;

  Procedure MA(C : cint; AEvent : TFileEvent);

  begin
    if (Native and C)<>0 then
      Include(Result,AEvent);
  end;

begin
  Result:=[];
  MA(IN_ACCESS,feAttrib);
  MA(IN_MODIFY,feModify);
  MA(IN_ATTRIB,feAttrib);
  MA(IN_CLOSE_WRITE,feAttrib);
  MA(IN_CLOSE_NOWRITE,feAttrib);
  MA(IN_OPEN,feAttrib);
  MA(IN_MOVED_FROM,feCreate);
  MA(IN_MOVED_TO,feDelete);
  MA(IN_CREATE,feCreate);
  Ma(IN_DELETE,feDelete);
  Ma(IN_DELETE_SELF,feDelete);
  Ma(IN_MOVE_SELF,feDelete);
  Ma(IN_UNMOUNT,feDelete);
  // IN_Q_OVERFLOW
  // IN_IGNORED

end;

procedure TDirwatch.Check;

Var
  fnl,len : cint;
  e : ^inotify_event;
  buf : Array[0..1023*4] of Byte; // 4K Buffer
  fn : string;
  p : pchar;
  fds : tfdset;
  Timeout : ttimeval;

begin
  fpFD_Zero(fds);
  fpFD_SET(FINotifyFD,fds);
  timeout.tv_sec:=FIdleInterval div 1000;
  timeout.tv_usec:=(FIdleInterval mod 1000)*1000;
  if (fpSelect(FINotifyFD+1,@fds,nil,nil,@Timeout)<=0) then
    exit;
  len:=fpRead(FINotifyFD,buf,sizeof(buf));
  e:=@buf;
  While ((pchar(e)-@buf)<len) do
    begin
    fnl:=e^.len;
    if (fnl>0) then
      begin
      p:=@e^.name+fnl-1;
      While (p^=#0) do
        begin
        dec(p);
        dec(fnl);
        end;
      end;
    setlength(fn,fnl);
    if (fnl>0) then
      move(e^.name,fn[1],fnl);
    DoChangeEvent(DirectoryEntryForFileName(FN),NativeEventsToEvents(E^ .mask));
    ptrint(e):=ptrint(e)+sizeof(inotify_event)+e^.len-1;
    end;
end;
{$ENDIF}

procedure TDirwatch.DoStartWatch;

begin
  InitWatch;
  try
    While not Terminated do
      begin
      Check;
      if Threaded then
        TThread.Synchronize(TThread.CurrentThread,@DoIdle)
      else
        DoIdle;
      end;
  Finally
    DoneWatch;
  end;
end;

procedure TDirwatch.StartWatch;

begin
  If Threaded then
    TDirwatchThread.Create(Self).WaitFor
  else
    DoStartWatch;
end;

procedure TDirwatch.AddWatch(const aFileName: string; aEvents: TFileEvents);
begin
  FWatches.AddEntry(AFileName).Events:=AEvents;
end;

procedure TDirwatch.Terminate;
begin
  FTerminated:=True;
end;

{ TDirectoryEntries }

function TDirectoryEntries.GetE(AIndex : Integer): TDirectoryEntry;
begin
  Result:=TDirectoryEntry(Items[AIndex]);
end;

procedure TDirectoryEntries.SetE(AIndex : Integer; AValue: TDirectoryEntry);
begin
  Items[AIndex]:=AValue;
end;

function TDirectoryEntries.IndexOfEntry(const AName: String): Integer;

begin
  Result:=Count-1;
  While (Result>=0) and (GetE(Result).Name<>AName) do
    Dec(Result);
end;

function TDirectoryEntries.EntryByName(const AName: String): TDirectoryEntry;

Var
  I : Integer;

begin
  I:=IndexOfEntry(AName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetE(I);
end;

function TDirectoryEntries.AddEntry(Const AName: String): TDirectoryEntry;
begin
  Result:=Add as TDirectoryEntry;
  Result.Name:=AName;
end;

end.

