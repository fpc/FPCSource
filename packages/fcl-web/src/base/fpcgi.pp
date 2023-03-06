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
unit fpcgi;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, FpWeb.HostApp.Custom.Cgi;
{$ELSE FPC_DOTTEDUNITS}
uses SysUtils,Classes,CustCgi;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TCGIApplication }

  TCGIApplication = Class(TCustomCGIApplication)
  end;

Var
  Application : TCGIApplication;
  ShowCleanUpErrors : Boolean = False;
  
Implementation

{$IFDEF FPC_DOTTEDUNITS}
uses Fcl.CustApp;
{$ELSE FPC_DOTTEDUNITS}
uses CustApp;
{$ENDIF FPC_DOTTEDUNITS}

Procedure InitCGI;

begin
  Application:=TCGIApplication.Create(Nil);
  if not assigned(CustomApplication) then
    CustomApplication := Application;
end;

Procedure DoneCGI;

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
  InitCGI;
  
Finalization
  DoneCGI;
  
end.
