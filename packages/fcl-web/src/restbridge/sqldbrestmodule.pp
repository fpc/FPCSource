unit sqldbrestmodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, fphttp, sqldbrestbridge;

Type

  { TSQLDBRestModule }

  TSQLDBRestModule = Class (TSessionHTTPModule)
  private
    FDispatcher: TSQLDBRestDispatcher;
    procedure SetDispatcher(AValue: TSQLDBRestDispatcher);
  Protected
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Function FindDispatcher : TSQLDBRestDispatcher; virtual;
  Public
    constructor Create(AOwner: TComponent); override;
    Procedure HandleRequest(ARequest : TRequest; AResponse : TResponse); override;
  Published
    Property Dispatcher : TSQLDBRestDispatcher Read FDispatcher Write SetDispatcher;
    Property Kind;
  end;

implementation

uses sqldbrestconst;

{ TSQLDBRestModule }

procedure TSQLDBRestModule.SetDispatcher(AValue: TSQLDBRestDispatcher);
begin
  if FDispatcher=AValue then Exit;
  if Assigned(Dispatcher) then
    FDispatcher.RemoveFreeNotification(Self);
  FDispatcher:=AValue;
  if Assigned(Dispatcher) then
    FDispatcher.FreeNotification(Self);
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

procedure TSQLDBRestModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);

Var
  Disp : TSQLDBRestDispatcher;

begin
  Disp:=FindDispatcher;
  If assigned(Disp) then
    Disp.HandleRequest(aRequest,aResponse)
  else
    Raise EHTTP.Create(SErrNoRESTDispatcher);
end;

end.

