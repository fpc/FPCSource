{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (Fcl)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$H+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpapache;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, Fcl.CustApp, FpWeb.HostApp.Custom.Apache;
{$ELSE FPC_DOTTEDUNITS}
uses
  sysutils, custapp, custapache;
{$ENDIF FPC_DOTTEDUNITS}

Type
  // Backwards compatibility defines.
  TApacheHandler = {$IFDEF FPC_DOTTEDUNITS}FpWeb.HostApp.Custom.Apache{$ELSE}custapache{$ENDIF}.TApacheHandler;
  TApacheRequest = {$IFDEF FPC_DOTTEDUNITS}FpWeb.HostApp.Custom.Apache{$ELSE}custapache{$ENDIF}.TApacheRequest;
  TApacheResponse = {$IFDEF FPC_DOTTEDUNITS}FpWeb.HostApp.Custom.Apache{$ELSE}custapache{$ENDIF}.TApacheResponse;
  THandlerPriority = {$IFDEF FPC_DOTTEDUNITS}FpWeb.HostApp.Custom.Apache{$ELSE}custapache{$ENDIF}.THandlerPriority;
  TBeforeRequestEvent = {$IFDEF FPC_DOTTEDUNITS}FpWeb.HostApp.Custom.Apache{$ELSE}custapache{$ENDIF}.TBeforeRequestEvent;
  TCustomApacheApplication = {$IFDEF FPC_DOTTEDUNITS}FpWeb.HostApp.Custom.Apache{$ELSE}custapache{$ENDIF}.TCustomApacheApplication;

  TApacheApplication = Class(TCustomApacheApplication)
  Public
    Property HandlerPriority;
    Property BeforeModules;
    Property AfterModules;
    Property AllowDefaultModule;
    Property OnGetModule;
    Property BaseLocation;
    Property ModuleName;
    Property MaxRequests;
    Property IdleWebModuleCount;
    Property WorkingWebModuleCount;
  end;

Function Application : TCustomApacheApplication;

Implementation

Function Application : TCustomApacheApplication;

begin
  Result:={$IFDEF FPC_DOTTEDUNITS}FpWeb.HostApp.Custom.Apache{$ELSE}custapache{$ENDIF}.Application;
end;

Procedure InitApache;

begin
  {$IFDEF FPC_DOTTEDUNITS}FpWeb.HostApp.Custom.Apache{$ELSE}custapache{$ENDIF}.Application:=TApacheApplication.Create(Nil);
  if not assigned(CustomApplication) then
    CustomApplication := Application;
end;

Procedure DoneApache;

begin
  Try
    if CustomApplication={$IFDEF FPC_DOTTEDUNITS}FpWeb.HostApp.Custom.Apache{$ELSE}custapache{$ENDIF}.Application then
      CustomApplication := nil;
    FreeAndNil({$IFDEF FPC_DOTTEDUNITS}FpWeb.HostApp.Custom.Apache{$ELSE}custapache{$ENDIF}.Application);
  except
    if ShowCleanUpErrors then
      Raise;
  end;
end;

Initialization
  InitApache;
  
Finalization
  DoneApache;
  
end.
