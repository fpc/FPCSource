unit utMessageManager;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, system.messaging;

type

  { TMyMessage }

  TMyBaseMessage = Class(Specialize TMessage<String>)
    OnDestroy: TNotifyEvent;
    destructor Destroy; override;
  end;

  TMyMessage1 = Class(TMyBaseMessage);
  TMyMessage2 = Class(TMyBaseMessage);

  { TTestMessageManager }

  generic TTestMessageManager<T: TBaseMessageManager> = class(TTestCase)
  private
    FM1: TMyMessage1;
    FM2: TMyMessage2;
    FMID1 : Integer;
    FMID2 : Integer;
    FManager: T;
    FSenderHandled: Array[1..2] of TObject;
    FMessageHandled: Array[1..2] of TMessagebase;
    procedure DoDestroy1(Sender: TObject);
    procedure DoDestroy2(Sender: TObject);
  protected
    procedure DoMyMessage(const aSender : TObject; const aMessage : TMessagebase);
    procedure DoMyMessage2(const aSender : TObject; const aMessage : TMessagebase);
    procedure DoMyMessage1Unsubscribe2(const aSender : TObject; const aMessage : TMessagebase);
    procedure SetUp; override;
    procedure TearDown; override;
    Property Manager : T Read FManager Write FManager;
    Property Msg1 : TMyMessage1 Read FM1 Write FM1;
    Property Msg2 : TMyMessage2 Read FM2 Write FM2;
    Property MsgID1 : Integer Read FMID1 Write FMID1;
    Property MsgID2 : Integer Read FMID2 Write FMID2;
  published
    procedure TestHookUp;
    procedure TestRegisterMessage;
    procedure TestSendMessage;
    procedure TestSendMessage2;
    procedure TestSendMessageDelete;
    procedure TestUnsubscribe;
    procedure TestUnsubscribeMethod;
    procedure TestUnsubscribeRef;
    procedure TestUnsubscribeInMessage;
  end;

  TSimpleMessageManagerTest = Class(specialize TTestMessageManager<TSimpleMessageManager>);
  TDefaultMessageManagerTest = Class(specialize TTestMessageManager<TMessageManager>);

implementation

{ TMyMessage }

destructor TMyBaseMessage.Destroy;
begin
  if Assigned(OnDestroy) then
    OnDestroy(Self);
  inherited Destroy;
end;

procedure TTestMessageManager.TestHookUp;

var
  I : Integer;

begin
  AssertNotnull(Manager);
  For I:=1 to 2 do
    begin
    AssertNull(Format('Msg %d',[I]),FMessageHandled[I]);
    AssertNull(Format('Sender %d',[I]),FSenderHandled[I]);
    end;
end;

procedure TTestMessageManager.TestRegisterMessage;
begin
  Manager.SubscribeToMessage(TMyMessage1,@DoMyMessage);
  // nothing to test. Just  needs to compile (and no memleaks)
end;

procedure TTestMessageManager.TestSendMessage;
begin
  MsgID1:=Manager.SubscribeToMessage(TMyMessage1,@DoMyMessage);
  AssertTrue('Have Message registration ID',MsgID1>0);
  MsgID2:=Manager.SubscribeToMessage(TMyMessage2,@DoMyMessage2);
  AssertTrue('New Message registration ID',MsgID2>MsgID1);
  Manager.SendMessage(Self,Msg1,False);
  AssertNotNull('Message not destroyed',Msg1);
  AssertSame('Message handled',Msg1,FMessageHandled[1]);
  AssertSame('Sender passed',Self,FSenderHandled[1]);
  AssertNull('Message 2 not handled',FMessageHandled[2]);
end;

procedure TTestMessageManager.TestSendMessage2;
begin
  MsgID1:=Manager.SubscribeToMessage(TMyMessage1,@DoMyMessage);
  AssertTrue('Have Message registration ID',MsgID1>0);
  MsgID2:=Manager.SubscribeToMessage(TMyMessage1,@DoMyMessage2);
  AssertTrue('New Message registration ID',MsgID2>MsgID1);
  Manager.SendMessage(Self,Msg1,False);
  AssertNotNull('Message not destroyed',Msg1);
  AssertSame('Message handled',Msg1,FMessageHandled[1]);
  AssertSame('Sender passed',Self,FSenderHandled[1]);
  AssertSame('Message handled second handler',Msg1,FMessageHandled[2]);
  AssertSame('Sender passed second handler',Self,FSenderHandled[2]);
end;

procedure TTestMessageManager.TestSendMessageDelete;

var
  Was : TMyMessage1;

begin
  Was:=Msg1;
  MsgID1:=Manager.SubscribeToMessage(TMyMessage1,@DoMyMessage);
  AssertTrue('Have Message registration ID',MsgID1>0);
  MsgID2:=Manager.SubscribeToMessage(TMyMessage2,@DoMyMessage2);
  AssertTrue('New Message registration ID',MsgID2>MsgID1);
  Manager.SendMessage(Self,Msg1,True);
  AssertNull('Message destroyed',Msg1);
  AssertSame('Message handled',Was,FMessageHandled[1]);
  AssertSame('Sender passed',Self,FSenderHandled[1]);
  AssertNull('Message 2 not handled',FMessageHandled[2]);
end;

procedure TTestMessageManager.TestUnsubscribe;

begin
  MsgID1:=Manager.SubscribeToMessage(TMyMessage1,@DoMyMessage);
  Manager.Unsubscribe(TMyMessage1,MsgID1);
  Manager.SendMessage(Self,Msg1,False);
  AssertNotNull('Message not destroyed',Msg1);
  AssertNull('Message not handled',FMessageHandled[1]);
end;

procedure TTestMessageManager.TestUnsubscribeMethod;
begin
  MsgID1:=Manager.SubscribeToMessage(TMyMessage1,@DoMyMessage);
  Manager.Unsubscribe(TMyMessage1,@DoMyMessage);
  Manager.SendMessage(Self,Msg1,False);
  AssertNotNull('Message not destroyed',Msg1);
  AssertNull('Message not handled',FMessageHandled[1]);
end;

procedure TTestMessageManager.TestUnsubscribeRef;

  Procedure DoAMessage(const Sender : TObject; Const Msg : TMessageBase);

  begin
    FMessageHandled[1]:=Msg;
    FSenderHandled[1]:=Msg;
  end;

var
  M : TMessageListener;

begin
  M:=@DoAMessage;
  MsgID1:=Manager.SubscribeToMessage(TMyMessage1,M);
  Manager.Unsubscribe(TMyMessage1,M);
  Manager.SendMessage(Self,Msg1,False);
  AssertNotNull('Message not destroyed',Msg1);
  AssertNull('Message not handled',FMessageHandled[1]);
end;

procedure TTestMessageManager.TestUnsubscribeInMessage;
begin
  MsgID1:=Manager.SubscribeToMessage(TMyMessage1,@DoMyMessage1Unsubscribe2);
  MsgID2:=Manager.SubscribeToMessage(TMyMessage1,@DoMyMessage2);
  AssertTrue('Have Message registration ID',MsgID1>0);
  AssertTrue('New Message registration ID',MsgID2>MsgID1);
  Manager.SendMessage(Self,Msg1,False);
  AssertNotNull('Message not destroyed',Msg1);
  AssertSame('Message handled',Msg1,FMessageHandled[1]);
  AssertSame('Message sender handled',Self,FSenderHandled[1]);
  AssertNull('Message 2 not handled',FMessageHandled[2]);
  AssertNull('Message 2 sender not handled',FSenderHandled[2]);
end;

procedure TTestMessageManager.DoMyMessage(const aSender: TObject;
  const aMessage: TMessagebase);
begin
  FMessageHandled[1]:=aMessage;
  FSenderHandled[1]:=aSender;
end;

procedure TTestMessageManager.DoMyMessage2(const aSender: TObject;
  const aMessage: TMessagebase);
begin
  FMessageHandled[2]:=aMessage;
  FSenderHandled[2]:=aSender;
end;

procedure TTestMessageManager.DoMyMessage1Unsubscribe2(const aSender: TObject;
  const aMessage: TMessagebase);
begin
  FMessageHandled[1]:=aMessage;
  FSenderHandled[1]:=aSender;
  Manager.Unsubscribe(TMyMessage1,MsgID2);
end;

procedure TTestMessageManager.DoDestroy1(Sender : TObject);

begin
  FM1:=Nil;
end;

procedure TTestMessageManager.DoDestroy2(Sender : TObject);

begin
  FM2:=Nil;
end;

procedure TTestMessageManager.SetUp;

var
  I : Integer;

begin
  FManager:=T.Create;
  FM1:=TMyMessage1.Create('ABC');
  FM1.OnDestroy:=@DoDestroy1;
  FM2:=TMyMessage2.Create('DEF');
  FM2.OnDestroy:=@DoDestroy2;
  FMID1:=0;
  FMID2:=0;
  For I:=1 to 2 do
    begin
    FMessageHandled[I]:=Nil;
    FSenderHandled[I]:=Nil;
    end;
end;

procedure TTestMessageManager.TearDown;
begin
  FreeAndNil(FManager);
  FreeAndNil(FM1);
  FreeAndNil(FM2);
end;

initialization
  RegisterTests([TSimpleMessageManagerTest,TDefaultMessageManagerTest]);
end.

