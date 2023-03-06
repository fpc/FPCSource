{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by the Free Pascal development team

    SQLDB REST bridge : REST module

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
 unit sqldbrestmodule;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpWeb.Http.Defs, FpWeb.Http.Base, FpWeb.RestBridge.Bridge;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, httpdefs, fphttp, sqldbrestbridge;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TSQLDBRestModule }

  TSQLDBRestModule = Class (TSessionHTTPModule)
  private
    FDispatcher: TSQLDBRestDispatcher;
    procedure SetDispatcher(AValue: TSQLDBRestDispatcher);
  Protected
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ConfigureDispatcherFromRequest(Disp: TSQLDBRestDispatcher; aRequest: TRequest); virtual;
    Function FindDispatcher : TSQLDBRestDispatcher; virtual;
  Public
    constructor Create(AOwner: TComponent); override;
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse); override;
  Published
    Property Dispatcher : TSQLDBRestDispatcher Read FDispatcher Write SetDispatcher;
    Property CORS;
    Property BaseURL;
    Property AfterInitModule;
    Property Kind;
    Property Session;
    Property OnNewSession;
    Property OnSessionExpired;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses FpWeb.RestBridge.Schema, FpWeb.RestBridge.Consts;
{$ELSE FPC_DOTTEDUNITS}
uses sqldbrestschema, sqldbrestconst;
{$ENDIF FPC_DOTTEDUNITS}

{ TSQLDBRestModule }

procedure TSQLDBRestModule.SetDispatcher(AValue: TSQLDBRestDispatcher);
begin
  if FDispatcher=AValue then Exit;
  if Assigned(Dispatcher) then
    FDispatcher.RemoveFreeNotification(Self);
  FDispatcher:=AValue;
  if Assigned(Dispatcher) then
    begin
    FDispatcher.Active:=False;
    FDispatcher.FreeNotification(Self);
    end;
end;

procedure TSQLDBRestModule.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FDispatcher then
      FDispatcher:=Nil;
end;

function TSQLDBRestModule.FindDispatcher: TSQLDBRestDispatcher;
begin
  Result:=Dispatcher;
end;

constructor TSQLDBRestModule.Create(AOwner: TComponent);
begin
  Kind:=wkOneShot;
  inherited Create(AOwner);
end;

procedure TSQLDBRestModule.ConfigureDispatcherFromRequest(Disp : TSQLDBRestDispatcher; aRequest : TRequest);

begin
  Disp.VerifyPathInfo(aRequest);
end;

procedure TSQLDBRestModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);

Var
  Disp : TSQLDBRestDispatcher;

begin
  Disp:=FindDispatcher;
  If assigned(Disp) then
    begin
    Disp.Active:=False;
    ConfigureDispatcherFromRequest(Disp,aRequest);
    Disp.HandleRequest(aRequest,aResponse)
    end
  else
    Raise EHTTP.Create(SErrNoRESTDispatcher);
end;

end.

