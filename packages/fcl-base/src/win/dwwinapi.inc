{%MainUnit dirwatch.pp}
{ **********************************************************************
    This file is part of the Free Pascal run time library.
    Copyright (c) 2024 by the Free Pascal development team

    File/Directory watch component: winapi implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFDEF FPC_DOTTEDUNITS}
uses WinApi.Windows;
{$ELSE}
uses windows;
{$ENDIF}

const
  DefaultGracePeriod = 10; // Number of milliseconds to wait after findfirstnotificationchange returns

{ TWinAPIDriver }

Type


  TWinAPIDriver = Class(TDirListDriver)
  Type
    { TWinAPIWatchData }
    TWinAPIData = class(TDirData)
    private
      FHandle: THandle;
    Public
      constructor Create(aHandle : THandle; aWatch : TCustomDirwatch; aEntry : TWatchDirectoryEntry);
      Property Handle : THandle Read FHandle;
    end;
  Private
    FGracePeriod: Integer;
    FHandles : Array of THandle;
  protected
    Function FindDataForHandle(aHandle : THandle) : TWinAPIData;
  Public
    Procedure Init; override;
    function DoCheck : cardinal; override;
    Procedure Done; override;
    Property GracePeriod : Integer Read FGracePeriod;
  end;

constructor TWinAPIDriver.TWinAPIData.Create(aHandle: THandle; aWatch : TCustomDirwatch; aEntry : TWatchDirectoryEntry);
begin
  Inherited Create(aWatch,aEntry);
  FHandle:=aHandle;
end;

Function EventsToNotifyFilters(Events : TWatchFileEvents) : DWord;
begin
  Result:=0;
  if ([feCreate,feDelete] * Events)<>[] then
    Result:=Result or FILE_NOTIFY_CHANGE_FILE_NAME;
  if (feAttrib in Events) then
    Result:=Result or FILE_NOTIFY_CHANGE_ATTRIBUTES;
  if (feModify in Events) then
    Result:=Result or FILE_NOTIFY_CHANGE_LAST_WRITE;
end;

function TWinAPIDriver.FindDataForHandle(aHandle: THandle): TWinAPIData;
var
  I : integer;
begin
  Result:=Nil;
  I:=0;
  While (Result=Nil) and (I<Watch.Watches.Count) do
    begin
    Result:=TWinAPIData(Watch.Watches[i].DriverData);
    if Assigned(Result) and (Result.Handle<>aHandle) then
      Result:=Nil;
    Inc(I);
    end;
end;

procedure TWinAPIDriver.Init;
var
  lCount,I : Integer;
  BD,FN : String;
  lFilters : DWord;
  lEntry : TWatchDirectoryEntry;
  lHandle : THandle;
  lData : TWinAPIData;

begin
  FGracePeriod:=DefaultGracePeriod;
  lCount:=0;
  BD:=Watch.BaseDir;
  if BD<>'' then
    BD:=IncludeTrailingPathDelimiter(BD);
  For I:=0 to Watch.Watches.Count-1 do
    begin
    LEntry:=Watch.Watches[i];
    FN:=BD+lEntry.Path;
    lFilters:=EventsToNotifyFilters(lEntry.Events);
    lHandle := FindFirstChangeNotification(PAnsiChar(FN), False, lFilters);
    if (lHandle<>INVALID_HANDLE_VALUE) then
      begin
      lData:=TWinAPIData.Create(lHandle,Watch,lEntry);
      lEntry.DriverData:=lData;
      lData.InitWatch;
      inc(lCount);
      end;
    end;
  SetLength(FHandles,lCount);
  if lCount=0 then
    exit;
  lCount:=0;
  For I:=0 to Watch.Watches.Count-1 do
    if assigned(Watch.Watches[i].DriverData) then
      begin
      FHandles[lCount]:=TWinAPIData(Watch.Watches[i].DriverData).Handle;
      Inc(lCount);
      end;
end;

function TWinAPIDriver.DoCheck: cardinal;

var
  Idx : Integer;
  H : THandle;
  Res : DWord;
  lData : TWinAPIData;

begin
  Result:=0;
  Res:=WaitForMultipleObjects(Length(FHandles),PWOHandleArray(FHandles),False,Watch.CheckTimeOut);
  Idx:=(Res-WAIT_OBJECT_0);
  if ((IDx>=0) and (Idx<Length(FHandles))) then
    begin
    // Empty queue
    H:=FHandles[Idx];
    repeat
      FindNextChangeNotification(H);
    until Terminated or (WaitForSingleObject(H,GracePeriod) <> WAIT_OBJECT_0);
    lData:=FindDataForHandle(H);
    Result:=Result+lData.Check;
    end;
end;

procedure TWinAPIDriver.Done;

var
  I : integer;
  Obj : TObject;
  lHandle : TWinAPIData absolute Obj;

begin
  For I:=0 to Watch.Watches.Count-1 do
    if assigned(Watch.Watches[i].DriverData) then
      begin
      Obj:=Watch.Watches[i].DriverData;
      Watch.Watches[i].DriverData:=Nil;
      if Assigned(lHandle) then
        begin
        FindCloseChangeNotification(lHandle.Handle);
        lHandle.Free;
        end;
      end;
end;

