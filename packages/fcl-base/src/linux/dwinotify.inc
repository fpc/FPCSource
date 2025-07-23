{ **********************************************************************
    This file is part of the Free Pascal run time library.
    Copyright (c) 2024 by the Free Pascal development team

    File/Directory watch component: inotify implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

uses
{$IFDEF FPC_DOTTEDUNITS}
  UnixApi.Base, System.CTypes, LinuxApi;
{$ELSE}
  baseunix,ctypes,linux;
{$ENDIF}
Type

  { TINotifyData }

  TINotifyData = Class(TObject)
  private
    FDescriptor: cInt;
  public
    Constructor create(aDescriptor : cint);
    property Descriptor : cInt Read FDescriptor;
  end;

  { TINotifyDriver }

  TINotifyDriver = Class(TDirwatchDriver)
  Private
    FINotifyFD : Cint;
    function FindEntryFromDescriptor(Wd: Cint): TWatchDirectoryEntry;
  protected
    property INotifyFD : Cint Read FINotifyFD;
  Public
    Procedure Init; override;
    function DoCheck : cardinal; override;
    Procedure Done; override;
  end;

{ TINotifyData }

constructor TINotifyData.create(aDescriptor: cint);
begin
  FDescriptor:=aDescriptor;
end;

procedure TINotifyDriver.Done;

begin
  fpClose(FInotifyFD);
end;

procedure TINotifyDriver.Init;

Const
  NativeEvents : Array[TWatchFileEvent] of cint = (IN_Modify,IN_Attrib,IN_Create,IN_Delete);

Var
  WD,I,NEvents : Integer;
  E : TWatchFileEvent;
  BD,FN : AnsiString;

begin
  BD:=Watch.BaseDir;
  if BD<>'' then
    BD:=IncludeTrailingPathDelimiter(BD);
  FINotifyFD:=inotify_init;
  For I:=0 to Watch.Watches.Count-1 do
    begin
    NEvents:=0;
    for E in FWatch.Watches[i].Events do
      NEvents:=NEvents OR NativeEvents[E];
    FN:=BD+FWatch.Watches[i].Path;
    wd:=inotify_add_watch(FINotifyFD,PAnsiChar(FN),NEvents);
    if Assigned(FWatch.Watches[i].DriverData) then
      TINotifyDAta(FWatch.Watches[i].DriverData).FDescriptor:=Wd
    else
      FWatch.Watches[i].DriverData:=TINotifyData.create(wd);
    end;
end;

Function NativeEventsToEvents(Native : cint) : TWatchFileEvents;

  Procedure MA(C : cint; AEvent : TWatchFileEvent);

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

function TINotifyDriver.FindEntryFromDescriptor(Wd : Cint) : TWatchDirectoryEntry;

 Function IsMatch(W : TWatchDirectoryEntry): boolean;
 begin
   Result:=Assigned(W) and (W.DriverData is TINotifyData) and (TINotifyData(W.DriverData).Descriptor=wd);
 end;

var
  I : Integer;

begin
  Result:=Nil;
  I:=Watch.Watches.Count-1;
  While (I>=0) and not IsMatch(Watch.Watches[i]) do
    Dec(I);
  if I>=0 then
    Result:=Watch.Watches[i];
end;

function TINotifyDriver.DoCheck: cardinal;

Type
  TINotifyBuffer = Array[0..1023*4] of Byte;  // 4K Buffer

Var
  fnl,len : cint;
  e : ^inotify_event;
  buf : TINotifyBuffer;
  fn : ansistring;
  p : pansichar;
  fds : tfdset;
  Timeout : ttimeval;
  lEvent : TFileChangeEvent;

begin
  Result:=0;
  Buf:=Default(TINotifyBuffer);
  fn:='';
  fpFD_Zero(fds);
  fpFD_SET(FINotifyFD,fds);
  timeout.tv_sec:=FWatch.CheckTimeOut div 1000;
  timeout.tv_usec:=(FWatch.CheckTimeOut mod 1000)*1000;
  if (fpSelect(FINotifyFD+1,@fds,nil,nil,@Timeout)<=0) then
    exit;
  len:=fpRead(FINotifyFD,buf,sizeof(buf));
  e:=@buf;
  While ((pansichar(e)-pansichar(@buf))<len) do
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
    lEvent.FileName:=FN;
    lEvent.Entry:=FindEntryFromDescriptor(E^.wd);
    if (lEvent.Entry=Nil) then
      lEvent.Entry:=Watch.DirectoryEntryForFileName(FN);
    lEvent.Events:=NativeEventsToEvents(E^.mask);
    Watch.DoChangeEvent(lEvent);
    PByte(e):=PByte(e)+sizeof(inotify_event)+e^.len;
    Inc(Result);
    end;
end;


