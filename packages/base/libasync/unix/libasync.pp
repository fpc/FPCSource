{
    $Id$

    libasync: Asynchronous event management
    Copyright (C) 2001-2002 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Unix implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit libasync;

{$MODE objfpc}

interface

type

  TAsyncData = record
    IsRunning, DoBreak: Boolean;
    HasCallbacks: Boolean;	// True as long as callbacks are set
    FirstTimer: Pointer;
    FirstIOCallback: Pointer;
    CurIOCallback: Pointer;	// current callback being processed within 'run'
    NextIOCallback: Pointer;	// next callback to get processed within 'run'
    FDData: Pointer;
    HighestHandle: LongInt;
  end;

{$INCLUDE libasynch.inc}



implementation

{$ifdef VER1_0}
uses Linux;
{$else}
uses Unix;
{$endif}

const
  MaxHandle = SizeOf(TFDSet) * 8 - 1;

type
  PIOCallbackData = ^TIOCallbackData;
  TIOCallbackData = record
    Next: PIOCallbackData;
    IOHandle: LongInt;
    ReadCallback, WriteCallback: TAsyncCallback;
    ReadUserData, WriteUserData: Pointer;
    SavedHandleFlags: LongInt;
  end;

{$INCLUDE libasync.inc}



procedure InternalInit(Handle: TAsyncHandle);
begin
  Handle^.Data.HighestHandle := -1;
end;

procedure InternalFree(Handle: TAsyncHandle);
var
  IOCallback: PIOCallbackData;
begin
  IOCallback := PIOCallbackData(Handle^.Data.FirstIOCallback);
  while Assigned(IOCallback) do
  begin
    if (IOCallback^.SavedHandleFlags and Open_NonBlock) = 0 then
      fcntl(IOCallback^.IOHandle, F_SetFl, IOCallback^.SavedHandleFlags);
    IOCallback := IOCallback^.Next;
  end;

  if Assigned(Handle^.Data.FDData) then
    FreeMem(Handle^.Data.FDData);
end;

procedure InternalRun(Handle: TAsyncHandle; TimeOut: Int64);
var
  AsyncResult: Integer;
  CurReadFDSet, CurWriteFDSet: TFDSet;
  CurIOCallback: PIOCallbackData;
begin
  if Handle^.Data.HighestHandle < 0 then
    // No I/O checks to do, so just wait...
    AsyncResult := Select(0, nil, nil, nil, TimeOut)
  else
  begin
    CurReadFDSet := PFDSet(Handle^.Data.FDData)[0];
    CurWriteFDSet := PFDSet(Handle^.Data.FDData)[1];
    AsyncResult := Select(Handle^.Data.HighestHandle + 1,
      @CurReadFDSet, @CurWriteFDSet, nil, TimeOut);

    if AsyncResult > 0 then
    begin
      // Check for I/O events
      Handle^.Data.CurIOCallback := Handle^.Data.FirstIOCallback;
      while Assigned(Handle^.Data.CurIOCallback) do
      begin
        CurIOCallback := PIOCallbackData(Handle^.Data.CurIOCallback);
        Handle^.Data.NextIOCallback := CurIOCallback^.Next;
        if FD_IsSet(CurIOCallback^.IOHandle, CurReadFDSet) and
	  FD_IsSet(CurIOCallback^.IOHandle, PFDSet(Handle^.Data.FDData)[0]) and
	  Assigned(CurIOCallback^.ReadCallback) then
        begin
	  CurIOCallback^.ReadCallback(CurIOCallback^.ReadUserData);
	  if Handle^.Data.DoBreak then
	    break;
        end;

        CurIOCallback := PIOCallbackData(Handle^.Data.CurIOCallback);
        if Assigned(CurIOCallback) and
	  FD_IsSet(CurIOCallback^.IOHandle, CurWriteFDSet) and
	  FD_IsSet(CurIOCallback^.IOHandle, PFDSet(Handle^.Data.FDData)[1]) and
	  Assigned(CurIOCallback^.WriteCallback) then
        begin
	  CurIOCallback^.WriteCallback(CurIOCallback^.WriteUserData);
	  if Handle^.Data.DoBreak then
	    break;
        end;

        Handle^.Data.CurIOCallback := Handle^.Data.NextIOCallback;
      end;
    end;
  end;
end;

procedure InternalInitIOCallback(Handle: TAsyncHandle; Data: PIOCallbackData;
  InitData: Boolean; CallbackTypes: TCallbackTypes);
var
  i: LongInt;
begin
  if InitData then
  begin
    if not Assigned(Handle^.Data.FDData) then
    begin
      GetMem(Handle^.Data.FDData, SizeOf(TFDSet) * 2);
      FD_Zero(PFDSet(Handle^.Data.FDData)[0]);
      FD_Zero(PFDSet(Handle^.Data.FDData)[1]);
    end;
    if Data^.IOHandle > Handle^.Data.HighestHandle then
      Handle^.Data.HighestHandle := Data^.IOHandle;
  end;

  Data^.SavedHandleFlags := fcntl(Data^.IOHandle, F_GetFl);
  fcntl(Data^.IOHandle, F_SetFl, Data^.SavedHandleFlags or Open_NonBlock);

  case Data^.IOHandle of
    StdInputHandle:
      i := Open_RdOnly;
    StdOutputHandle, StdErrorHandle:
      i := Open_WrOnly;
    else
      i := Data^.SavedHandleFlags and Open_Accmode;
  end;

  case i of
    Open_RdOnly:
      if cbRead in CallbackTypes then
        FD_Set(Data^.IOHandle, PFDSet(Handle^.Data.FDData)[0]);
    Open_WrOnly:
      if cbWrite in CallbackTypes then
        FD_Set(Data^.IOHandle, PFDSet(Handle^.Data.FDData)[1]);
    Open_RdWr:
      begin
        if cbRead in CallbackTypes then
	  FD_Set(Data^.IOHandle, PFDSet(Handle^.Data.FDData)[0]);
	if cbWrite in CallbackTypes then
	  FD_Set(Data^.IOHandle, PFDSet(Handle^.Data.FDData)[1]);
      end;
  end;
end;

procedure InternalClearIOCallback(Handle: TAsyncHandle; IOHandle: LongInt;
  CallbackTypes: TCallbackTypes);
begin
  if cbRead in CallbackTypes then
    FD_Clr(IOHandle, PFDSet(Handle^.Data.FDData)[0]);
  if cbWrite in CallbackTypes then
    FD_Clr(IOHandle, PFDSet(Handle^.Data.FDData)[1]);
end;

function asyncGetTicks: Int64; cdecl;
var
  Time: TimeVal;
begin
  GetTimeOfDay(Time);
  Result := Int64(Time.Sec) * 1000 + Int64(Time.USec div 1000);
end;


end.


{
  $Log$
  Revision 1.5  2002-09-25 21:53:39  sg
  * Split in common implementation an platform dependent implementation

  Revision 1.4  2002/09/15 15:51:09  sg
  * Removed debugging output code
}
