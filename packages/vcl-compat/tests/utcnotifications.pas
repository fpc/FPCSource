unit utcnotifications;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, system.notification;

type

  { TTestNotification }

  TTestNotification= class(TTestCase)
  private
    FCenter: TNotificationCenter;
    FChannel: TChannel;
    FChannels: TChannels;
    FNotification: TNotification;
    FSender : TObject;
    FNotifiedNotification : TNotification;
    FNotificationName : String;
    FNotifyChannels: TChannels;
    FNotifyChannel: TChannel;
    FNumber : Integer;
    procedure DoCancelByname(Sender: TObject; aName: String);
    procedure DoChannelEvent(Sender: TObject; aChannel: TChannel);
    procedure DoGetChannels(Sender: TObject; aChannels: TChannels);
    procedure DoNamedChannelEvent(Sender: TObject; aName: String);
    procedure DoNotifiy(Sender: TObject);
    procedure DoGetIconNumber(Sender: TObject; var aNumber: Integer);
    procedure DoNotifyNotification(Sender: TObject; aNotification: TNotification);
    procedure DoSetBadgeNumber(Sender: TObject; const aNumber: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    property Notification : TNotification Read FNotification;
    Property Center : TNotificationCenter Read FCenter;
    Property Channels : TChannels Read FChannels;
    Property Channel : TChannel Read FChannel;
  published
    procedure TestHookUp;
    procedure TestAssign;
    procedure TestAssignChannel;
    procedure TestGetIconBadgeCount;
    procedure TestSetIconBadgeCount;
    procedure TestRequestPermission;
    procedure TestResetIconBadgeNumber;
    Procedure TestAuthorizationStatus;
    procedure TestCancelAll;
    procedure TestCancelByName;
    procedure TestGetAllChannels;
    procedure TestCreateChannel;
    procedure TestDeleteChannel;
    Procedure TestPresentNotification;
    Procedure TestScheduleNotification;
  end;

implementation

procedure TTestNotification.TestHookUp;
begin
  AssertNotNull('Default',TEventedNotificationCenter.Instance);
  AssertNotNull('Center',Center);
  AssertNull('Sender',FSender);
  AssertNotNull('Channels',FChannels);
  AssertNotNull('Channel',FChannel);
  AssertTrue('TeventedNotificationCenter.Instance.OnGetIconBadgeNumber',TeventedNotificationCenter.Instance.OnGetIconBadgeNumber=Nil);
  AssertTrue('TeventedNotificationCenter.Instance.OnCancelAllNotifications',TeventedNotificationCenter.Instance.OnCancelAllNotifications=Nil);
  AssertTrue('TeventedNotificationCenter.Instance.OnCancelNotificationByName',TeventedNotificationCenter.Instance.OnCancelNotificationByName=Nil);
  AssertTrue('TeventedNotificationCenter.Instance.OnGetChannels',TeventedNotificationCenter.Instance.OnGetChannels=Nil);
  AssertTrue('TeventedNotificationCenter.Instance.OnCreateChannel',TeventedNotificationCenter.Instance.OnCreateChannel=Nil);
  AssertTrue('TeventedNotificationCenter.Instance.OnDeleteChannel',TeventedNotificationCenter.Instance.OnDeleteChannel=Nil);
  AssertTrue('TeventedNotificationCenter.Instance.OnPresentNotification',TeventedNotificationCenter.Instance.OnPresentNotification=Nil);
  AssertTrue('TeventedNotificationCenter.Instance.OnScheduleNotification',TeventedNotificationCenter.Instance.OnScheduleNotification=Nil);
  AssertTrue('TeventedNotificationCenter.Instance.OnRequestPermission',TeventedNotificationCenter.Instance.OnRequestPermission=Nil);
  AssertTrue('TeventedNotificationCenter.Instance.OnResetIconBadgeNumber',TeventedNotificationCenter.Instance.OnResetIconBadgeNumber=Nil);
  AssertTrue('TeventedNotificationCenter.Instance.OnCancelNotification',TeventedNotificationCenter.Instance.OnCancelNotification=Nil);
  AssertTrue('TeventedNotificationCenter.Instance.OnSetIconBadgeNumber',TeventedNotificationCenter.Instance.OnSetIconBadgeNumber=Nil);
  AssertTrue('TeventedNotificationCenter.Instance.AuthorizationStatus',TAuthorizationStatus.NotDetermined=TeventedNotificationCenter.Instance.AuthorizationStatus);
end;

procedure TTestNotification.TestAssign;

var
  N : TNotification;

begin
  With Notification do
    begin
    Name:='x';
    Title:='y';
    AlertBody:='z';
    AlertAction:='q';
    Number:=134;
    FireDate:=Date+1;
    EnableSound:=True;
    SoundName:='bigben';
    HasAction:=True;
    RepeatInterval:=TRepeatInterval.Year;
    ChannelId:='c4';
    end;
  N:=TEventedNotificationCenter.Instance.CreateNotification;
  try
    N.Assign(Notification);
    AssertEquals('Name',Notification.Name,N.Name);
    AssertEquals('Title',Notification.Title,N.Title);
    AssertEquals('AlertBody',Notification.AlertBody,N.AlertBody);
    AssertEquals('AlertAction',Notification.AlertAction,N.AlertAction);
    AssertEquals('Number',Notification.Number,N.Number);
    AssertEquals('Firedate',Notification.FireDate,N.FireDate);
    AssertEquals('EnableSound',Notification.EnableSound,N.EnableSOund);
    AssertEquals('SoundName',Notification.SoundName,N.SoundName);
    AssertEquals('HasAction',Notification.HasAction,N.HasAction);
    AssertTrue('RepeatInterval',Notification.RepeatInterval=N.RepeatInterval);
    AssertEquals('ChannelId',Notification.ChannelId,N.ChannelId);
  finally
    N.Free;
  end;
end;

procedure TTestNotification.TestAssignChannel;
begin

end;

procedure TTestNotification.TestGetIconBadgeCount;
begin
  TeventedNotificationCenter.Instance.OnGetIconBadgeNumber:=@DoGetIconNumber;
  AssertEquals('Correct number',12,FCenter.ApplicationIconBadgeNumber);
end;

procedure TTestNotification.TestSetIconBadgeCount;
begin
  TeventedNotificationCenter.Instance.OnSetIconBadgeNumber:=@DoSetBadgeNumber;
  Center.ApplicationIconBadgeNumber:=16;
  AssertNotNull('Called',FSender);
  AssertEquals('Number',16,FNumber);
end;

procedure TTestNotification.TestRequestPermission;
begin
  TEventedNotificationCenter.Instance.OnRequestPermission:=@DoNotifiy;
  Center.RequestPermission;
  AssertNotNull('Called',FSender);
end;

procedure TTestNotification.TestResetIconBadgeNumber;
begin
  TeventedNotificationCenter.Instance.OnResetIconBadgeNumber:=@DoNotifiy;
  Center.ResetIconBadgeNumber;
  AssertNotNull('Called',FSender);
end;

procedure TTestNotification.TestAuthorizationStatus;
begin
  TeventedNotificationCenter.Instance.AuthorizationStatus:=TAuthorizationStatus.NotDetermined;
  AssertTrue('Correct',TAuthorizationStatus.NotDetermined=Center.AuthorizationStatus);

end;

procedure TTestNotification.TestCancelAll;
begin
  TeventedNotificationCenter.Instance.OnCancelAllNotifications:=@DoNotifiy;
  Center.CancelAll;
  AssertNotNull('Called',FSender);
end;

procedure TTestNotification.TestCancelByName;
begin
  TeventedNotificationCenter.Instance.OnCancelNotificationByName:=@DoCancelByname;
  Center.CancelNotification('Name');
  AssertNotNull('Called',FSender);
  AssertEquals('Name','Name',FNotificationName);
end;

procedure TTestNotification.TestGetAllChannels;
begin
  TeventedNotificationCenter.Instance.OnGetChannels:=@DoGetChannels;
  Center.GetAllChannels(Channels);
  AssertNotNull('Called',FSender);
  AssertSame('Channels',Channels,FNotifyChannels);
end;

procedure TTestNotification.TestCreateChannel;
begin
  TeventedNotificationCenter.Instance.OnCreateChannel:=@DoChannelEvent;
  Center.CreateOrUpdateChannel(Channel);
  AssertNotNull('Called',FSender);
  AssertSame('Channel',Channel,FNotifyChannel);
end;

procedure TTestNotification.TestDeleteChannel;
begin
  TeventedNotificationCenter.Instance.OnDeleteChannel:=@DoNamedChannelEvent;
  Center.DeleteChannel('Name');
  AssertNotNull('Called',FSender);
  AssertEquals('Name','Name',FNotificationName);
end;

procedure TTestNotification.TestPresentNotification;
begin
  TeventedNotificationCenter.Instance.OnPresentNotification:=@DoNotifyNotification;
  Center.PresentNotification(Notification);
  AssertNotNull('Called',FSender);
  AssertSame('Notification',Notification,FNotifiedNotification);


end;

procedure TTestNotification.TestScheduleNotification;
begin
  TeventedNotificationCenter.Instance.OnScheduleNotification:=@DoNotifyNotification;
  Center.ScheduleNotification(Notification);
  AssertNotNull('Called',FSender);
  AssertSame('Notification',Notification,FNotifiedNotification);
end;

procedure TTestNotification.DoNotifiy(Sender: TObject);
begin
  FSender:=Sender;
end;

procedure TTestNotification.DoCancelByname(Sender: TObject; aName: String);
begin
  FNotificationName:=aName;
  FSender:=Sender;
end;

procedure TTestNotification.DoChannelEvent(Sender: TObject; aChannel: TChannel);
begin
  FSender:=Sender;
  FNotifyChannel:=aChannel
end;

procedure TTestNotification.DoGetChannels(Sender: TObject; aChannels: TChannels);
begin
  FSender:=Sender;
  FNotifyChannels:=aChannels;
end;

procedure TTestNotification.DoNamedChannelEvent(Sender: TObject; aName: String);
begin
  FSender:=Sender;
  FNotificationName:=aName;
end;

procedure TTestNotification.DoNotifyNotification(Sender: TObject; aNotification: TNotification);
begin
  FSender:=Sender;
  FNotifiedNotification:=aNotification;
end;

procedure TTestNotification.DoSetBadgeNumber(Sender: TObject; const aNumber: Integer);
begin
  FSender:=Sender;
  FNumber:=aNumber;
end;

procedure TTestNotification.DoGetIconNumber(Sender: TObject; var aNumber: Integer);
begin
  aNumber:=12;
end;


procedure TTestNotification.SetUp;
begin
  FNotification:=TEventedNotificationCenter.Instance.CreateNotification;
  FCenter:=TNotificationCenter.Create(Nil);
  FSender:=nil;
  FChannels:=TChannels.Create();
  FChannel:=TChannel.Create;
  FNotifiedNotification:=Nil;
  FNotificationName:='';
  FNotifyChannels:=Nil;
  FNumber:=0;
  TeventedNotificationCenter.Instance.OnGetIconBadgeNumber:=Nil;
  TeventedNotificationCenter.Instance.OnCancelAllNotifications:=Nil;
  TeventedNotificationCenter.Instance.OnCancelNotificationByName:=Nil;
  TeventedNotificationCenter.Instance.OnGetChannels:=Nil;
  TeventedNotificationCenter.Instance.OnCreateChannel:=Nil;
  TeventedNotificationCenter.Instance.OnDeleteChannel:=Nil;
  TeventedNotificationCenter.Instance.OnPresentNotification:=Nil;
  TeventedNotificationCenter.Instance.OnScheduleNotification:=Nil;
  TeventedNotificationCenter.Instance.OnRequestPermission:=Nil;
  TeventedNotificationCenter.Instance.OnResetIconBadgeNumber:=Nil;
  TeventedNotificationCenter.Instance.OnCancelNotification:=Nil;
  TeventedNotificationCenter.Instance.OnSetIconBadgeNumber:=Nil;
  TeventedNotificationCenter.Instance.AuthorizationStatus:=TAuthorizationStatus.NotDetermined;
end;

procedure TTestNotification.TearDown;
begin
  FreeAndNil(FChannels);
  FreeAndNil(FChannel);
  FreeAndNil(FNotification);
  FreeAndNil(FCenter);
  FSender:=nil;
end;

initialization

  RegisterTest(TTestNotification);
end.

