{ **********************************************************************
    This file is part of the Free Pascal run time library.
    Copyright (c) 2024 by the Free Pascal development team

    File/Directory watch component: KQueue based implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


uses
{$IFDEF FPC_DOTTEDUNITS}
  UnixApi.Types, UnixApi.Base, BsdApi;
{$ELSE}
  unixtype,baseunix,bsd;
{$ENDIF}


{ TKQueueDriver }
Type


  TKQueueDriver = Class(TDirListDriver)
  Protected
  Type

    { TKQueueData }

    TKQueueData = class(TDirData)
    private
      FFd: cint;
    public
      Constructor create(aFd : cint; aWatch : TCustomDirwatch; aEntry : TWatchDirectoryEntry);
      Destructor destroy;override;
      Property FD : cint Read FFd;
    end;
  Private
    FKQueue : cint;
    FKEvents : Array of TKEvent;
  private
    class function EventsToFLags(const aEvents: TWatchFileEvents): cuint;
  Public
    Destructor destroy; Override;
    Procedure Init; override;
    function DoCheck : cardinal; override;
    Procedure Done; override;
  end;

{ TKQueueData }

constructor TKQueueDriver.TKQueueData.create(aFd: cint; aWatch : TCustomDirwatch; aEntry : TWatchDirectoryEntry);
begin
  Inherited create(aWatch,aEntry);
  FFd:=aFd;
end;

destructor TKQueueDriver.TKQueueData.destroy;
begin
  fpClose(FFd);
  inherited destroy;
end;

destructor TKQueueDriver.destroy;
begin
  Done;
  inherited destroy;
end;

class function TKQueueDriver.EventsToFLags(const aEvents : TWatchFileEvents) : cuint;
begin
  Result:=0;
  Result:=Result or NOTE_WRITE or NOTE_EXTEND or NOTE_DELETE OR NOTE_RENAME OR NOTE_ATTRIB;
end;

procedure TKQueueDriver.Init;
var
  i,lCount : Integer;
  fd : cInt;
  lData : TKQueueData;
  BD,FN : String;
  lFlags : cuint;
  lEntry : TWatchDirectoryEntry;

begin
  lCount:=0;
  FKQueue:=kQueue;
  if FKQueue=-1 then
    Raise EDirWatch.Create('Failed to create kernel queue');
  BD:= Watch.BaseDir;
  if BD<>'' then
    BD:=IncludeTrailingPathDelimiter(BD);
  For I:=0 to Watch.Watches.count-1 do
    begin
    lEntry:=Watch.Watches[i];
    FN:=BD+lEntry.Path;
    fd:=fpopen(FN,O_RDONLY {or O_DIRECTORY});
    if fd<>-1 then
      begin
      lData:=TKQueueData.create(fd,Watch,lEntry);
      Watch.Watches[i].DriverData:=lData;
      lData.InitWatch;
      inc(lCount);
      end;
    end;
  SetLength(FKevents,lCount);
  lCount:=0;
  For I:=0 to Watch.Watches.Count-1 do
    begin
    lEntry:=Watch.Watches[i];
    lData:=TKQueueData(lEntry.DriverData);
    if assigned(lData) then
      begin
      lFlags:=EventsToFlags(lEntry.Events);
      EV_SET(@FKevents[lCount],lData.FD,EVFILT_VNODE, EV_ADD or EV_CLEAR, lFlags,0,lData);
      inc(lCount);
      end;
    end;
  DoCheck;
end;

function TKQueueDriver.DoCheck: cardinal;
var
  lEvents: Array of TKEvent;
  timeout : Ttimespec;
  i,lCount : cInt;
  lData : TKQueueData;

begin
  Result:=0;
  lEvents:=[];
  SetLength(lEvents,Length(FKevents)*2);
  timeout.tv_sec:=FWatch.CheckTimeOut div 1000;
  timeout.tv_nsec:=(FWatch.CheckTimeOut mod 1000)*1000*1000;
  lCount:=kevent(FKQueue,PKEvent(FKEvents),Length(FKevents),PKevent(lEvents),Length(lEvents),@Timeout);
  For I:=0 to lCount-1 do
    begin
    lData:=TKQueueData(lEvents[i].uData);
    if Assigned(lData) then
      Result:=Result+LData.Check;
    end;
end;

procedure TKQueueDriver.Done;
var
  I : Integer;
  D : TObject;

begin
  if Assigned(Watch) then
    For I:=0 to Watch.Watches.Count-1 do
      begin
      D:=Watch.Watches[i].DriverData;
      Watch.Watches[i].DriverData:=Nil;
      FreeAndNil(D);
      end;
  fpclose(FKQueue);
  FKEvents:=[];
  FKQueue:=0;
end;
