unit utcanalytics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, system.analytics;

type

  { TListener }

  TListener = Class(TInterfacedObject,IApplicationActivityListener)
  private
    FLastActivity: TAppActivity;
    FLastContext: TObject;
    FLastSender: TObject;
    FLastTimeStamp: TDateTime;
  Protected
    procedure TrackAppStart(const TimeStamp: TDateTime);
    procedure TrackAppExit(const TimeStamp: TDateTime);
    procedure TrackControlFocused(const TimeStamp: TDateTime; const Sender: TObject);
    procedure TrackWindowActivated(const TimeStamp: TDateTime; const Sender: TObject);
    procedure TrackEvent(const TimeStamp: TDateTime; const Sender, Context: TObject);
    procedure TrackException(const TimeStamp: TDateTime; const E: Exception);
  Public
    Procedure Reset;
    Property LastTimestamp : TDateTime Read FLastTimeStamp;
    Property LastActivity : TAppActivity Read FLastActivity;
    Property LastSender : TObject Read FLastSender;
    Property LastContext : TObject Read FLastContext;
  end;

  { TTestAnalytics }

  TTestAnalytics= class(TTestCase)
  private
    FListener: TListener;
    FListener2: TListener;
    FListenerIntf : IApplicationActivityListener;
    FListener2Intf : IApplicationActivityListener;
    FManager: TAnalyticsManager;
    FTime: TDateTime;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure Register;
    Procedure AssertEquals(const Msg : String; aExpected, aActual : TAppActivity); overload;
    procedure AssertEvent(const Msg: String; aListener: TListener;
      aActivity: TAppActivity; aSender: TObject = Nil; aContext: TObject = Nil);
    property Manager : TAnalyticsManager Read FManager;
    Property Listener : TListener Read FListener;
    Property Listener2 : TListener Read FListener2;
  published
    procedure TestHookUp;
    procedure TestRegister;
    Procedure TestAppStart;
    procedure TestAppExit;
    procedure TestFocused;
    procedure TestWindowActivated;
    procedure TestEvent;
    procedure TestException;
    procedure TestUnRegister;
  end;

implementation

uses typinfo;

{ TListener }

procedure TListener.TrackAppStart(const TimeStamp: TDateTime);
begin
  FLastTimeStamp:=TimeStamp;
  FLastActivity:=TAppActivity.AppStart;
end;

procedure TListener.TrackAppExit(const TimeStamp: TDateTime);
begin
  FLastTimeStamp:=TimeStamp;
  FLastActivity:=TAppActivity.AppExit;
end;

procedure TListener.TrackControlFocused(const TimeStamp: TDateTime;
  const Sender: TObject);
begin
  FLastTimeStamp:=TimeStamp;
  FLastActivity:=TAppActivity.ControlFocused;
  FLastSender:=Sender;
  FLastContext:=Nil;
end;

procedure TListener.TrackWindowActivated(const TimeStamp: TDateTime;
  const Sender: TObject);
begin
  FLastTimeStamp:=TimeStamp;
  FLastActivity:=TAppActivity.WindowActivated;
  FLastSender:=Sender;
  FLastContext:=Nil;
end;

procedure TListener.TrackEvent(const TimeStamp: TDateTime; const Sender,
  Context: TObject);
begin
  FLastTimeStamp:=TimeStamp;
  FLastActivity:=TAppActivity.Custom;
  FLastSender:=Sender;
  FLastContext:=Context;
end;

procedure TListener.TrackException(const TimeStamp: TDateTime;
  const E: Exception);
begin
  FLastTimeStamp:=TimeStamp;
  FLastActivity:=TAppActivity.Exception;
  FLastSender:=E;
  FLastContext:=Nil;
end;

procedure TListener.Reset;
begin
  FLastActivity:=Default(TAppActivity);
  FLastContext:=Default(TObject);
  FLastSender:=Default(TObject);
  FLastTimeStamp:=Default(TDateTime);
end;

procedure TTestAnalytics.TestHookUp;
begin
  AssertNotNull('Have manager',Manager);
  AssertNotNull('Have listener',Listener);
end;

procedure TTestAnalytics.TestRegister;
begin
  Register;
  AssertTrue('Tracking enabled',Manager.TrackingEnabled);
end;

procedure TTestAnalytics.TestAppStart;
begin
  Register;
  Manager.RecordActivity(TAppActivity.AppStart);
  AssertEvent('Listener 1',Listener,TAppActivity.AppStart);
  AssertEvent('Listener 2',Listener2,TAppActivity.AppStart);
end;

procedure TTestAnalytics.TestAppExit;
begin
  Register;
  Manager.RecordActivity(TAppActivity.AppExit);
  AssertEvent('Listener 1',Listener,TAppActivity.AppExit);
  AssertEvent('Listener 2',Listener2,TAppActivity.AppExit);
end;

procedure TTestAnalytics.TestFocused;
begin
  Register;
  Manager.RecordActivity(TAppActivity.ControlFocused,Self);
  AssertEvent('Listener 1',Listener,TAppActivity.ControlFocused,Self,Nil);
  AssertEvent('Listener 2',Listener2,TAppActivity.ControlFocused,Self,Nil);
end;

procedure TTestAnalytics.TestWindowActivated;
begin
  Register;
  Manager.RecordActivity(TAppActivity.WindowActivated,Self,Nil);
  AssertEvent('Listener 1',Listener,TAppActivity.WindowActivated,Self,Nil);
  AssertEvent('Listener 2',Listener2,TAppActivity.WindowActivated,Self,Nil);
end;

procedure TTestAnalytics.TestEvent;
begin
  Register;
  Manager.RecordActivity(TAppActivity.Custom,Self,Listener);
  AssertEvent('Listener 1',Listener,TAppActivity.Custom,Self,Listener);
  AssertEvent('Listener 2',Listener2,TAppActivity.Custom,Self,Listener);
end;

procedure TTestAnalytics.TestException;

var
  E : Exception;

begin
  Register;
  E:=Exception.Create('Soso');
  try
    Manager.RecordActivity(TAppActivity.Exception,E);
    AssertEvent('Listener 1',Listener,TAppActivity.Exception,E);
    AssertEvent('Listener 2',Listener2,TAppActivity.Exception,E);
  finally
    E.Free;
  end;
end;

procedure TTestAnalytics.TestUnRegister;
begin
  Register;
  Manager.RecordActivity(TAppActivity.AppExit);
  AssertEvent('Listener 1',Listener,TAppActivity.AppExit);
  AssertEvent('Listener 2',Listener2,TAppActivity.AppExit);
  Manager.UnregisterActivityListener(FListener2Intf);
  Listener2.Reset;
  Manager.RecordActivity(TAppActivity.AppExit);
  AssertEvent('Listener 1',Listener,TAppActivity.AppExit);
  AssertEquals('Listener2',0,Listener2.LastTimestamp);
end;

procedure TTestAnalytics.SetUp;
begin
  FManager:=TAnalyticsManager.Create;
  FListener:=TListener.Create;
  FListenerIntf:=FListener as IApplicationActivityListener;
  FListener2:=TListener.Create;
  FListener2Intf:=FListener2 as IApplicationActivityListener;
end;

procedure TTestAnalytics.TearDown;
begin
  FreeAndNil(FManager);
  // FreeAndNil(FListener);
  FListenerIntf:=Nil; // Will free
  FListener:=nil;
  FListener2Intf:=Nil; // Will free
  FListener2:=nil;
end;

procedure TTestAnalytics.Register;
begin
  Manager.RegisterActivityListener(Listener as IApplicationActivityListener);
  Manager.RegisterActivityListener(Listener2 as IApplicationActivityListener);
  FTime:=Now;
end;

procedure TTestAnalytics.AssertEquals(const Msg: String; aExpected, aActual: TAppActivity);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TAppActivity),Ord(aExpected)),
                   GetEnumName(TypeInfo(TAppActivity),Ord(aActual)));
end;

procedure TTestAnalytics.AssertEvent(const Msg: String; aListener: TListener;
  aActivity: TAppActivity; aSender: TObject; aContext: TObject);
begin
  AssertEquals(Msg+' activity',aActivity,aListener.LastActivity);
  AssertTrue(Msg+' timestamp',aListener.LastTimestamp>=FTime);
  AssertSame(Msg+' sender',aSender,aListener.LastSender);
  AssertSame(Msg+' context',aContext,aListener.LastContext);
end;

initialization

  RegisterTest(TTestAnalytics);
end.

