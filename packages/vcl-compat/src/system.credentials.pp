unit system.credentials;

interface
{$mode objfpc}
{$h+}
{$modeswitch functionreferences}

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Contnrs;
{$ELSE}
uses SysUtils, Classes, Contnrs;
{$ENDIF}

Type
  ELoginCredentialError = class(Exception);

  { TLoginCredentialService }

  TLoginCredentialService = class sealed
  public const
    Default        = '';
    DefaultUsrPw   = 'DefaultUsrPw';
    DefaultUsrPwDm = 'DefaultUsrPwDm'; 
  public type
    TLoginFunc = reference to function (const Username, Password, Domain: string): Boolean;
    TLoginEvent = procedure (Sender: TObject; const Username, Password, Domain: string; var Handled: Boolean) of object;
    TLoginCredentialEvent = procedure (Sender: TObject; Callback: TLoginEvent; var Success: Boolean) of object;
  private 
    type

      { TRegisteredHandler }

      TRegisteredHandler = class (TObject)
        FEvent: TLoginCredentialEvent;
        constructor Create(aEvent: TLoginCredentialEvent);
      end;

    class var _Handlers: TStringList;
    class constructor Create;
    class destructor Destroy;
    class function FindLoginCredentialEvent(const aContext: string): TLoginCredentialEvent;
    class function DoGetLoginCredentialEvent(const aContext: string): TLoginCredentialEvent;
    class function IndexOfHandler(const aContext: String; aEvent: TLoginCredentialEvent): Integer;
  public
    Class procedure Clear;
    class procedure RegisterLoginHandler(const aContext: string; const aEvent: TLoginCredentialEvent); static;
    class procedure UnregisterLoginHandler(const aContext: string; const aEvent: TLoginCredentialEvent); static;
    class function HandlerCount : Integer;

    class function GetLoginCredentialEvent(const aContext: string): TLoginCredentialEvent; static;
    class function GetLoginCredentials(const aContext: string; Sender: TObject; const aCallback: TLoginEvent): Boolean; overload; static;
    class function GetLoginCredentials(const aContext: string; const aCallback: TLoginFunc): Boolean; overload; static;
    class function GetLoginCredentials(const aContext: string; var aUsername, aPassword: string): Boolean; overload; static;
    class function GetLoginCredentials(const aContext: string; var aUsername, aPassword, aDomain: string): Boolean; overload; static;
  end;

Implementation

Resourcestring
  SServiceNotFound = 'Service %s not found';

{ TLoginCredentialService }

class constructor TLoginCredentialService.Create;
begin
  _handlers:=TStringList.Create(True);
end;

class destructor TLoginCredentialService.Destroy;
begin
  FreeAndNil(_Handlers)
end;

class function TLoginCredentialService.IndexOfHandler(const aContext : String; aEvent: TLoginCredentialEvent): Integer;

Var
  MC, M : TMethod;
  I : Integer;

begin
  Result:=-1;
  MC:=TMethod(aEvent);
  For I:=_Handlers.Count-1 downto 0 do
    if (_Handlers[I]=aContext) then
      begin
      M:=TMethod(TRegisteredHandler(_Handlers.Objects[i]).FEvent);
      If ((M.Data=MC.Data) and (M.Code=MC.Code))  then
        Exit(I);
      end;
end;

class procedure TLoginCredentialService.Clear;
begin
  _Handlers.Clear;
end;

class procedure TLoginCredentialService.RegisterLoginHandler(const aContext: string; const aEvent: TLoginCredentialEvent);
begin
  if (IndexOfHandler(aContext,aEvent)=-1) then
    _Handlers.AddObject(aContext,TRegisteredHandler.Create(aEvent));
end;

class procedure TLoginCredentialService.UnregisterLoginHandler(const aContext: string; const aEvent: TLoginCredentialEvent);

var
  Idx : Integer;

begin
  Idx:=IndexOfHandler(aContext,aEvent);
  if (Idx<>-1) then
    _Handlers.Delete(idx);
end;

class function TLoginCredentialService.HandlerCount: Integer;
begin
  Result:=_Handlers.Count;
end;

class function TLoginCredentialService.DoGetLoginCredentialEvent(const aContext: string): TLoginCredentialEvent;

begin
  Result:=FindLoginCredentialEvent(aContext);
  if Not Assigned(Result) then
    raise ELoginCredentialError.CreateFmt(SServiceNotFound,[aContext]);
end;

class function TLoginCredentialService.GetLoginCredentialEvent(const aContext: string): TLoginCredentialEvent;
begin
  Result:=FindLoginCredentialEvent(aContext);
end;

class function TLoginCredentialService.FindLoginCredentialEvent(const aContext: string): TLoginCredentialEvent;

var
  Idx : Integer;

begin
  Result:=Nil;
  Idx:=_Handlers.IndexOf(aContext);
  if Idx=-1 then
    Idx:=_Handlers.IndexOf('');
  if Idx<>-1 then
    Result:=TRegisteredHandler(_Handlers.Objects[Idx]).FEvent;
end;

class function TLoginCredentialService.GetLoginCredentials(const aContext: string; Sender: TObject; const aCallback: TLoginEvent): Boolean;

var
  Event: TLoginCredentialEvent;

begin
  Result:=True;
  Event:=DoGetLoginCredentialEvent(aContext);
  Event(Sender,aCallback,Result)
end;

Type

  { TEventObj }

  TEventObj = Class(TObject)
    FCallBack : TLoginCredentialService.TLoginFunc;
    procedure DoCallBack(Sender: TObject; const aUsername, aPassword, aDomain: string; var aHandled: Boolean) ;
  end;

{ TEventObj }

procedure TEventObj.DoCallBack(Sender: TObject; const aUsername, aPassword, aDomain: string; var aHandled: Boolean);
begin
  aHandled:=FCallBack(aUsername,aPassword,aDomain);
end;

class function TLoginCredentialService.GetLoginCredentials(const aContext: string; const aCallback: TLoginFunc): Boolean;

var
  Event: TLoginCredentialEvent;
  Obj : TEventObj;


begin
  Result:=False;
  Event:=DoGetLoginCredentialEvent(aContext);
  Obj:=TEventObj.Create;
  try
    Obj.FCallBack:=aCallBack;
    Event(Obj,@Obj.DoCallback,Result);
  finally
    Obj.Free;
  end;
end;

class function TLoginCredentialService.GetLoginCredentials(const aContext: string; var aUsername, aPassword: string): Boolean;

var
  Dummy : String;

begin
  Dummy:='';
  Result:=GetLoginCredentials(aContext,aUserName,aPassword,Dummy);
end;

class function TLoginCredentialService.GetLoginCredentials(const aContext: string; var aUsername, aPassword, aDomain: string): Boolean;

Var
  U,P,D : String;

  function Callback (const UN,PWD,Dom : string): Boolean;
  begin
    U:=UN;
    P:=PWD;
    D:=Dom;
    Result:=True;
  end;

begin
  Result:=GetLoginCredentials(aContext,@CallBack);
  if Result then
    begin
    aUserName:=U;
    aPassword:=P;
    aDomain:=D;
    end;
end;

{ TLoginCredentialService.TRegisteredHandler }

constructor TLoginCredentialService.TRegisteredHandler.Create(aEvent: TLoginCredentialEvent);
begin
  FEvent:=aEvent;
end;

end.
