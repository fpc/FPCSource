{
    This file is part of the Free Component Library (Fcl)
    Copyright (c) 2017-2018 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpHTTPSys;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  FpWeb.HostApp.Custom.HttpSys;
{$ELSE FPC_DOTTEDUNITS}
uses
  custHTTPSys;
{$ENDIF FPC_DOTTEDUNITS}

type
  THTTPSysApplication = class(TCustomHTTPSysApplication)
  end;

var
  Application: THTTPSysApplication;
  ShowCleanUpErrors: Boolean = False;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, Fcl.CustApp;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, custApp;
{$ENDIF FPC_DOTTEDUNITS}

procedure InitHTTPSys;
begin
  Application := THTTPSysApplication.Create(Nil);
  if not Assigned(CustomApplication) then
    CustomApplication := Application;
end;

procedure DoneHTTPSys;
begin
  try
    if CustomApplication = Application then
      CustomApplication := Nil;
    FreeAndNil(Application);
  except
    if ShowCleanUpErrors then
      raise;
  end;
end;

initialization
  InitHTTPSys;

finalization
  DoneHTTPSys;

end.

