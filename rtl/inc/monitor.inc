{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2024 by the Free Pascal development team

    TMonitor support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ *********************************************************************
  TMonitor
  *********************************************************************}

var
  _monitormanager : TMonitorManager;

class procedure TMonitor.FreeMonitorData(aData : Pointer);
begin
  if Not assigned(aData) then
    system.exit;
  _monitormanager.DoFreeMonitorData(aData);
end;

class procedure TMonitor.SetDefaultSpinCount(const aSpinCount: LongInt);

begin
  _monitormanager.DoSetDefaultSpinCount(aSpinCount);
end;

class function TMonitor.GetDefaultSpinCount : LongInt;

begin
  Result:=_monitormanager.DoGetDefaultSpinCount();
end;


class procedure TMonitor.Enter(Const aObject: TObject);
begin
  _monitormanager.DoEnter(aObject);
end;

class function TMonitor.Enter(Const aObject: TObject; aTimeout: Cardinal): Boolean;
begin
  Result:=_monitormanager.DoEnterTimeout(aObject,aTimeout);
end;

class procedure TMonitor.Exit(Const aObject: TObject);
begin
  _monitormanager.DoExit(aObject);
end;

class function TMonitor.TryEnter(Const aObject: TObject): Boolean;
begin
  Result:=_monitormanager.DoTryEnter(aObject);
end;

class function TMonitor.Wait(Const aObject: TObject; aTimeout: Cardinal): Boolean;
begin
  Result:=_monitormanager.DoWait(aObject,aTimeout);
end;

class function TMonitor.Wait(Const aObject, aLock: TObject; aTimeout: Cardinal): Boolean;
begin
  Result:=_monitormanager.DoWaitLock(aObject,aLock,aTimeout);
end;

class procedure TMonitor.Pulse(Const aObject: TObject);
begin
  _monitormanager.DoPulse(aObject);
end;

class procedure TMonitor.PulseAll(Const aObject: TObject);
begin
  _monitormanager.DoPulseAll(aObject);
end;


{ *********************************************************************
  Monitor manager
  *********************************************************************}

function SysMonitorSetObjectDataProc(const aObject : TObject; aData,aComparand : Pointer) : Pointer;
begin
  Result:=aObject.SetMonitorData(aData,aComparand);
end;

function SysMonitorGetObjectDataFunc (const aObject : TObject): Pointer;
begin
  Result:=aObject.GetMonitorData;
end;

var
  MMsys : TMonitorManager;


function SetMonitorManager (var aNew : TMonitorManager) : TMonitorManager;

begin
  Result:=_monitormanager;
  aNew.DoSetMonitorObjectData:=@SysMonitorSetObjectDataProc;
  aNew.DoGetMonitorObjectData:=@SysMonitorGetObjectDataFunc;
  _monitormanager:=aNew;
end;

function GetMonitorManager : TMonitorManager;

begin
  Result:=_monitormanager;
end;

procedure DoNoMonitor;
begin
  RunError(235);
end;

procedure SysFreeMonitorData(aData : Pointer);

begin
  // Do nothing
end;

procedure SysNoMonitor(const aObject : TObject);

begin
  DoNoMonitor;
end;

function SysNoMonitorFunc(const aObject : TObject) : Boolean;

begin
  Result:=False;
  DoNoMonitor;
end;

function SysNoMonitorTimeout(const aObject : TObject; aTimeout : Cardinal) : Boolean;

begin
  Result:=False;
  DoNoMonitor;
end;

function SysNoMonitorGetDefaultSpinCount : Longint;
begin
  Result:=0;
  DoNoMonitor;
end;

procedure SysNoMonitorSetDefaultSpinCount(const aValue : Longint);
begin
  DoNoMonitor;
end;

function SysNoMonitorDowaitLock(const aObject,aLock : TObject; aTimeout : Cardinal) : Boolean;

begin
  Result:=False;
  DoNoMonitor;
end;

Procedure InitMonitor;

begin
  MMsys.DoEnter:=@SysNoMonitor;
  MMsys.DoExit:=@SysNoMonitor;
  MMsys.DoPulse:=@SysNoMonitor;
  MMsys.DoPulseAll:=@SysNoMonitor;
  MMsys.DoEnterTimeout:=@SysNoMonitorTimeout;
  MMsys.DoTryEnter:=@SysNoMonitorFunc;
  MMsys.DoWait:=@SysNoMonitorTimeout;
  MMsys.DoSetDefaultSpinCount:=@SysNoMonitorSetDefaultSpinCount;
  MMsys.DoGetDefaultSpinCount:=@SysNoMonitorGetDefaultSpinCount;
  MMsys.DoWaitLock:=@SysNoMonitorDowaitLock;
  MMsys.DoFreeMonitorData:=@SysFreeMonitorData;
  SetMonitorManager(MMsys);
end;

{ *********************************************************************
  Shortcuts
  *********************************************************************}

function MonitorEnter(Const aObject: TObject; aTimeout: Cardinal = INFINITE): Boolean;

begin
  if atimeout=Infinite then
    TMonitor.Enter(aObject)
  else
    TMonitor.Enter(aObject,aTimeOut);
end;


function MonitorTryEnter(Const aObject: TObject): Boolean;

begin
  TMonitor.TryEnter(aObject);
end;


procedure MonitorExit(Const aObject: TObject);

begin
  TMonitor.Exit(aObject);
end;


function MonitorWait(Const aObject: TObject; aTimeout: Cardinal): Boolean;

begin
  TMonitor.Wait(aObject,aTimeOut);
end;


function MonitorWait(Const aObject, aLock: TObject; aTimeout: Cardinal): Boolean;

begin
  TMonitor.Wait(aObject,aLock,aTimeOut);
end;


procedure MonitorPulse(Const aObject: TObject);

begin
  TMonitor.Pulse(aObject);
end;


procedure MonitorPulseAll(Const aObject: TObject);

begin
  TMonitor.PulseAll(aObject);
end;


