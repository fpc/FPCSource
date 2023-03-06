{
    This file is part of the Free Component Library (Fcl)
    Copyright (c) 1999-2009 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$H+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpfcgi;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils,System.Classes,FpWeb.HostApp.Custom.Fcgi;
{$ELSE FPC_DOTTEDUNITS}
uses SysUtils,Classes,custfcgi;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TFCGIApplication }

  TFCGIApplication = Class(TCustomFCGIApplication)
  end;

Var
  Application : TFCGIApplication;
  ShowCleanUpErrors : Boolean = False;
  
Implementation

{$IFDEF FPC_DOTTEDUNITS}
uses Fcl.CustApp;
{$ELSE FPC_DOTTEDUNITS}
uses CustApp;
{$ENDIF FPC_DOTTEDUNITS}

Procedure InitFCGI;

begin
  Application:=TFCGIApplication.Create(Nil);
  if not assigned(CustomApplication) then
    CustomApplication := Application;
end;

Procedure DoneFCGI;

begin
  Try
    if CustomApplication=Application then
      CustomApplication := nil;
    FreeAndNil(Application);
  except
    if ShowCleanUpErrors then
      Raise;
  end;
end;

Initialization
  InitFCGI;
  
Finalization
  DoneFCGI;
  
end.
