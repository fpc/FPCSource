{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2017-2018 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpHTTPSys;

{$mode objfpc}{$H+}

interface

uses
  custHTTPSys;

type
  THTTPSysApplication = class(TCustomHTTPSysApplication)
  end;

var
  Application: THTTPSysApplication;
  ShowCleanUpErrors: Boolean = False;

implementation

uses
  SysUtils, custApp;

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

