unit utccredentials;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, system.credentials, fpcunit, testregistry;

Type

  { TTestCredentials }

  TTestCredentials = class(TTestcase)
  private
    FSuccess : Boolean;
    FUser,FPass,FDom : String;
    FEventMethod,
    FEventMethod2 : TMethod;
    procedure DoLoginEvent(Sender: TObject; Callback: TLoginCredentialService.TLoginEvent; var Success: Boolean);
    procedure DoLoginEvent2(Sender: TObject; Callback: TLoginCredentialService.TLoginEvent; var Success: Boolean);
    procedure ReceiveEvent(Sender: TObject; const Username, Password, Domain: string; var Handled: Boolean);
  Public
    procedure AssertCredentials(const Msg, aUser, aPassword, aDomain: String);
    Procedure Setup; override;
    Procedure TearDown; override;
  Published
    Procedure TestHookup;
    Procedure TestAddHandler;
    Procedure TestRemoveHandler;
    Procedure TestGetLoginCredentialEvent;
    Procedure TestGetloginCredentials;
    procedure TestGetloginCredentials2;
    procedure TestGetloginCredentials3;
  end;

implementation

{ TTestCredentials }

procedure TTestCredentials.DoLoginEvent(Sender: TObject; Callback: TLoginCredentialService.TLoginEvent; var Success: Boolean);
begin
  CallBack(Sender,FUser,FPass,FDom,Success);
end;

procedure TTestCredentials.DoLoginEvent2(Sender: TObject; Callback: TLoginCredentialService.TLoginEvent; var Success: Boolean);
begin
  CallBack(Sender,FUser+'2',FPass+'2',FDom+'2',Success);
end;

procedure TTestCredentials.AssertCredentials(const Msg, aUser, aPassword, aDomain: String);

begin
  AssertEquals(Msg+': User',FUser,aUser);
  AssertEquals(Msg+': Password',FPass,aPassword);
  AssertEquals(Msg+': Domain',FDom,aDomain);
end;

procedure TTestCredentials.Setup;
begin
  inherited Setup;
  TLoginCredentialService.Clear;
  FEventMethod2:=TMethod(@Self.DoLoginEvent2);
  FEventMethod:=TMethod(@Self.DoLoginEvent);
  FUser:='U';
  FPass:='P';
  FDom:='D';
  FSuccess:=True;
end;

procedure TTestCredentials.TearDown;
begin
  inherited TearDown;
end;

procedure TTestCredentials.TestHookup;
begin
  AssertEquals('handler count',0,TLoginCredentialService.HandlerCount);
  AssertEquals('Default user','U',FUser);
  AssertEquals('Default pwd','P',FPass);
  AssertEquals('Default Dom','D',FDom);
end;

procedure TTestCredentials.TestAddHandler;
begin
  TLoginCredentialService.RegisterLoginHandler('x',@DoLoginEvent);
  AssertEquals('handler count a',1,TLoginCredentialService.HandlerCount);
  TLoginCredentialService.RegisterLoginHandler('y',@DoLoginEvent);
  AssertEquals('handler count b',2,TLoginCredentialService.HandlerCount);
  TLoginCredentialService.RegisterLoginHandler('x',@DoLoginEvent);
  AssertEquals('handler count c',2,TLoginCredentialService.HandlerCount);
  TLoginCredentialService.RegisterLoginHandler('x',@DoLoginEvent2);
  AssertEquals('handler count d',3,TLoginCredentialService.HandlerCount);
end;

procedure TTestCredentials.TestRemoveHandler;

begin
  TLoginCredentialService.RegisterLoginHandler('x',@DoLoginEvent);
  AssertEquals('handler count a',1,TLoginCredentialService.HandlerCount);

  TLoginCredentialService.RegisterLoginHandler('y',@DoLoginEvent);
  AssertEquals('handler count b',2,TLoginCredentialService.HandlerCount);

  TLoginCredentialService.RegisterLoginHandler('x',@DoLoginEvent);
  AssertEquals('handler count c',2,TLoginCredentialService.HandlerCount);

  TLoginCredentialService.RegisterLoginHandler('x',@DoLoginEvent2);
  AssertEquals('handler count d',3,TLoginCredentialService.HandlerCount);

  TLoginCredentialService.UnRegisterLoginHandler('x',@DoLoginEvent2);
  AssertEquals('handler count e',2,TLoginCredentialService.HandlerCount);

  TLoginCredentialService.UnRegisterLoginHandler('z',@DoLoginEvent2);
  AssertEquals('handler count f',2,TLoginCredentialService.HandlerCount);

  TLoginCredentialService.UnRegisterLoginHandler('y',@DoLoginEvent);
  AssertEquals('handler count g',1,TLoginCredentialService.HandlerCount);

  TLoginCredentialService.UnRegisterLoginHandler('x',@DoLoginEvent);
  AssertEquals('handler count h',0,TLoginCredentialService.HandlerCount);
end;

procedure TTestCredentials.TestGetLoginCredentialEvent;

var
  E : TLoginCredentialService.TLoginCredentialEvent;
  ME : TMethod;
begin
  TLoginCredentialService.RegisterLoginHandler('x',@DoLoginEvent);
  AssertEquals('handler count a',1,TLoginCredentialService.HandlerCount);

  TLoginCredentialService.RegisterLoginHandler('y',@DoLoginEvent2);
  AssertEquals('handler count b',2,TLoginCredentialService.HandlerCount);

  E:=TLoginCredentialService.GetLoginCredentialEvent('y');
  ME:=TMethod(E);
  AssertTrue('Same method',(ME.Code=FEventMethod2.Code) and (ME.Data=FEventMethod2.Data));

  E:=TLoginCredentialService.GetLoginCredentialEvent('x');
  ME:=TMethod(E);
  AssertTrue('Same method',(ME.Code=FEventMethod.Code) and (ME.Data=FEventMethod.Data));
  E:=TLoginCredentialService.GetLoginCredentialEvent('z');
  AssertTrue('Same method',E=Nil);
end;

procedure TTestCredentials.TestGetloginCredentials;

var
  U,P,D : String;
begin

  TLoginCredentialService.RegisterLoginHandler('x',@DoLoginEvent);
  AssertTrue('Getcreds',TLoginCredentialService.GetLoginCredentials('x',U,P,D));
  AssertCredentials('Login',U,P,D);
end;

procedure TTestCredentials.ReceiveEvent(Sender: TObject; const Username, Password, Domain: string; var Handled: Boolean) ;

begin
  Handled:=FSuccess;
  AssertSame('Correct sender ',Sender,Self);
  if FSuccess then
    AssertCredentials('Login',UserName,Password,Domain);
end;
procedure TTestCredentials.TestGetloginCredentials2;

var
  U,P,D : String;
  Res : Boolean;

  Function DoReceive(const Username, Password, Domain: string): Boolean;
  begin
    Result:=Res;
    if Res then
      begin
      U:=userName;
      P:=Password;
      D:=Domain;
      end;
  end;

begin
  Res:=True;
  TLoginCredentialService.RegisterLoginHandler('x',@DoLoginEvent);
  AssertTrue('Getcreds success',TLoginCredentialService.GetLoginCredentials('x',@DoReceive));
  AssertCredentials('Login',U,P,D);
  Res:=False;
  AssertFalse('Getcreds fail',TLoginCredentialService.GetLoginCredentials('x',@DoReceive));
end;

procedure TTestCredentials.TestGetloginCredentials3;
begin
  TLoginCredentialService.RegisterLoginHandler('x',@DoLoginEvent);
  AssertTrue('Getcreds success',TLoginCredentialService.GetLoginCredentials('x',Self,@ReceiveEvent));
  FSuccess:=False;
  AssertFalse('Getcreds fail',TLoginCredentialService.GetLoginCredentials('x',Self,@ReceiveEvent));
end;


initialization
  RegisterTest(TTestCredentials);
end.

