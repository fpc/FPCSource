{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    FCM Messaging demo - web client

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program webclient;

{$mode objfpc}
{$externalclasses}

uses
  BrowserConsole, JS, Classes, SysUtils, weborworker, Web, browserapp, firebaseapp,
  module.messagingservice, service.messagingserver, fprpcclient;

Type

  { TDemoApp }

  TDemoApp = class(TBrowserApplication)
  private
    procedure HandleReceivedMessage(aMessage: TJSObject);
    procedure HaveToken(aToken: string);
    procedure requestPermission;
    procedure SendToken(aToken: string);
    procedure ShowToken(aToken: string);
  Public
    pnlToken : TJSHTMLElement;
    lblToken : TJSHTMLElement;
    edtMessage : TJSHTMLInputElement;
    btnSend: TJSHTMLButtonElement;
    btnRegister:TJSHTMLButtonElement;
    App : TFirebaseApp;
    Reg: weborworker.TJSServiceWorkerRegistration;
    Procedure DoRun; override;
    procedure handleregister(event : TJSEvent); async;
    procedure handlesend(event : TJSEvent); async;
  end;

var
  Application : TDemoApp;
  config : TJSObject; external name 'firebaseConfig';

Const
  TheVAPIDKey = 'The VAPID key for your FCM application';

{ TDemoApp }


procedure TDemoApp.HandleReceivedMessage(aMessage: TJSObject);
begin
  if assigned(aMessage) then
    console.debug('Message received: ',aMessage);
end;

procedure TDemoApp.DoRun;


begin
  RPCModule:=TRPCModule.Create(Self);
  pnlToken:=GetHTMLElement('pnlToken');
  lblToken:=GetHTMLElement('lblToken');
  edtMessage:=TJSHTMLInputElement(GetHTMLElement('edtMessage'));
  btnSend:=TJSHTMLButtonElement(GetHTMLElement('btnSend'));
  btnSend.addEventListener('click',@HandleSend);
  btnRegister:=TJSHTMLButtonElement(GetHTMLElement('btnRegister'));
  btnRegister.addEventListener('click',@HandleRegister);
  Writeln('Initializing application...');
  App:=Firebase.initializeApp(config);
  App.messaging.onMessage(@HandleReceivedMessage);
  Window.Navigator.serviceWorker.register('firebase-messaging-sw.js')._then(function (js : JSValue) :JSValue
    begin
    reg:=weborworker.TJSServiceWorkerRegistration(js);
    if assigned(Reg) then
      Writeln('Registered service worker...')
    end,function (js : JSValue) :JSValue
    begin
      Writeln('Unable to register service worker')
    end);
end;

procedure TDemoApp.ShowToken(aToken : string);
begin
  pnlToken.classlist.remove('is-hidden');
  lblToken.innerText:=aToken;
  Writeln(aToken);
end;

procedure TDemoApp.SendToken(aToken : string);

  procedure DoOK(aResult: JSValue);
  begin
    Writeln('Registered token on server');
  end;

  procedure DoFail(Sender: TObject; const aError: TRPCError);
  begin
    Writeln('Failed to register token on server: '+aError.Message);
  end;

begin
  Writeln('Sending token to server: ',aToken);
  RPCModule.Service.RegisterSubscription(aToken,@DoOK,@DoFail);

end;

procedure TDemoApp.HaveToken(aToken : string);

begin
  Showtoken(aToken);
  Sendtoken(aToken);
  btnSend.disabled:=False;
  btnRegister.disabled:=False;
end;

procedure TDemoApp.requestPermission;

 function onpermission (permission : jsvalue) : jsvalue;

 var
   token : string;

 begin
   if (permission='granted') then
     begin
     writeln('Notification permission granted.');
     handleregister(nil);
     end;
 end;

begin
  Writeln('Requesting permission...');
  TJSNotification.requestPermission()._then(@OnPermission)
end;

procedure TDemoApp.handleregister(event: TJSEvent);

var
  Token : string;
  opt : TMessagingGetTokenOptions;

begin
  opt:=TMessagingGetTokenOptions.New;
  opt.serviceworkerRegistration:=self.Reg;
  opt.vapidKey:=TheVAPIDKey;
  Token:=Await(App.messaging.getToken(opt));
  if (token='') then
    RequestPermission
  else
    HaveToken(token);
end;

procedure TDemoApp.handlesend(event: TJSEvent);

  procedure DoOK(aResult: JSValue);
  begin
    Writeln('Message transferred to server for sending');
  end;

  procedure DoFail(Sender: TObject; const aError: TRPCError);
  begin
    Writeln('Failed to transfer message to server for sending: '+aError.Message);
  end;

var
  Msg : TJSObject;

begin
  Msg:=New([
    'title','Free Pascal FCM demo',
    'body',edtMessage.Value,
    'image','https://www.freepascal.org/favicon.png'
  ]);
  RPCModule.Service.SendNotification(Msg,@DoOK,@DoFail);
end;

begin
  Application:=TDemoApp.Create(Nil);
  Application.Initialize;
  Application.Run;
end.
