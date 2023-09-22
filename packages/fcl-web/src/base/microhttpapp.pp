{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    TMicroHTTPApplication class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$H+}
{$IFNDEF FPC_DOTTEDUNITS}
unit microhttpapp;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils,System.Classes, FpWeb.HostApp.Custom.MicroHttpApp;
{$ELSE FPC_DOTTEDUNITS}
uses SysUtils,Classes, custmicrohttpapp;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TCGIApplication }

  TMicroHTTPApplication = Class(TCustomMicroHTTPApplication)
  end;

Var
  Application : TMicroHTTPApplication;
  ShowCleanUpErrors : Boolean = False;
  
Implementation

{$IFDEF FPC_DOTTEDUNITS}
uses Fcl.CustApp;
{$ELSE FPC_DOTTEDUNITS}
uses CustApp;
{$ENDIF FPC_DOTTEDUNITS}

Procedure InitHTTP;

begin
  Application:=TMicroHTTPApplication.Create(Nil);
  if not assigned(CustomApplication) then
    CustomApplication := Application;
end;

Procedure DoneHTTP;

begin
  if CustomApplication=Application then
    CustomApplication := nil;
  try  
    FreeAndNil(Application);
  except
    if ShowCleanUpErrors then
      Raise;
  end;
end;

Initialization
  InitHTTP;
  
Finalization
  DoneHTTP;
  
end.
