unit utcpush;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, system.pushnotifications;

type
  
  { TMyPushService }

  TMyPushService = class(TPushService)
  Private
    FMyDeviceID : TPropArray;
    FMyDeviceToken : TPropArray;
    FMyStartError: String;
    FMyStatus: TStatus;
  protected
    function GetDeviceID: TPropArray; override;
    function GetDeviceToken: TPropArray; override;
    function GetStartupError: string; override;
    function GetStartupNotifications: TPushServiceNotificationArray; override;
    function GetStatus: TStatus; override;
    procedure StartService; override;
    procedure StopService; override;
  Public
    constructor Create(const aOwner: TPushServiceManager; const aServiceName: string); override;
    procedure Change(AChanges: TChanges);
    procedure ReceiveNotification(const aNotification: TPushServiceNotification);
    Property MyStatus : TStatus Read FMyStatus Write FMyStatus;
    Property MyDeviceID : TPropArray Read FMyDeviceID Write FMyDeviceID;
    Property MyDeviceToken : TPropArray Read FMyDeviceToken Write FMyDeviceToken;
    Property MyStartError : String Read FMyStartError write FMyStartError;
  end;

  { TTestPushNotifications }

  TTestPushNotifications= class(TTestCase)
  private
    FConn: TPushServiceConnection;
    FConn2: TPushServiceConnection;
    FManager: TPushServiceManager;
    FMyService: TMyPushService;
    FChanges: TPushService.TChanges;
    FChangeCount : Integer;
    FNotif: TPushServiceNotification;
    FReceiveCount : Integer;
    FReceived : TPushServiceNotification;
    procedure DoChange(Sender: TObject; AChange: TPushService.TChanges);
    procedure DoReceive(Sender: TObject; const aNotification: TPushServiceNotification);
    function GetService: TPushService;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Property Manager : TPushServiceManager Read FManager;
    Property Service : TPushService Read GetService;
    Property MyService : TMyPushService Read FMyService Write FMyService;
    Property Conn : TPushServiceConnection Read FConn Write FConn;
    Property Conn2 : TPushServiceConnection Read FConn2 Write FConn2;
    Property Notif : TPushServiceNotification Read FNotif Write FNotif;
  published
    procedure TestHookUp;
    Procedure TestCreateService;
    Procedure TestAddService;
    Procedure TestRemoveService;
    Procedure TestIndexOfService;
    Procedure TestIndexOfServiceName;
    Procedure TestGetServiceByName;
    Procedure TestCreateServiceConnection;
    Procedure TestDestroyServiceConnection;
    Procedure TestIndexOfConnection;
    Procedure TestIndexOfConnection2;
    Procedure TestConnectionActive;
    Procedure TestServiceAppProp;
    Procedure TestDeviceID;
    Procedure TestDeviceToken;
    Procedure TestStartupError;
    Procedure TestOnChange;
    Procedure TestOnChange2;
    Procedure TestOnReceive;
    Procedure TestOnReceive2;
  end;

implementation

{ TMyPushService }

function TMyPushService.GetDeviceID: TPropArray;
begin
  Result:=FMyDeviceID;
end;

function TMyPushService.GetDeviceToken: TPropArray;
begin
  Result:=FMyDeviceToken;
end;

function TMyPushService.GetStartupError: string;

begin
  Result:=FMyStartError;
end;

function TMyPushService.GetStartupNotifications: TPushServiceNotificationArray;

begin
  Result:=[];
end;

function TMyPushService.GetStatus: TStatus;

begin
  Result:=FMyStatus;
end;

procedure TMyPushService.StartService;

begin
  FMyStatus:=TStatus.Started;
end;

procedure TMyPushService.StopService;

begin
  FMyStatus:=TStatus.Stopped;
end;

constructor TMyPushService.Create(const aOwner: TPushServiceManager; const aServiceName: string);

begin
  inherited Create(aOwner, aServiceName);
  FMyDeviceID:=[];
  FMyDeviceToken:=[];
  FMyStartError:='';
  FMyStatus:=TStatus.Stopped;
end;

procedure TMyPushService.Change(AChanges: TChanges);
begin
  DoChange(aChanges);
end;

procedure TMyPushService.ReceiveNotification(const aNotification: TPushServiceNotification);
begin
  DoReceiveNotification(aNotification);

end;

procedure TTestPushNotifications.TestHookUp;

begin
  AssertNotNull('Have Global Instance',TPushServiceManager.Instance);
  AssertNotNull('Have local Instance',Manager);
  AssertEquals('No services',0, Manager.Count);
  AssertEquals('No change count',0, FChangeCount);
  AssertTrue('FChanges empty',[]=FChanges);

end;

procedure TTestPushNotifications.TestCreateService;
begin
  MyService:=TMyPushService.Create(Manager,'my');
  AssertEquals('ServiceName','my',MyService.ServiceName);
  AssertSame('Manager',Manager,MyService.Manager);
  AssertEquals('Registered in Manager 1',1,Manager.Count);
  AssertSame('Registered in Manager 2',Service,Manager.Services[0]);
end;

procedure TTestPushNotifications.TestAddService;
begin
  MyService:=TMyPushService.Create(Nil,'my');
  AssertNull('Manager',MyService.Manager);
  Manager.AddService(MyService,False);
  AssertSame('Manager',Manager,MyService.Manager);
  AssertEquals('Registered in Manager 1',1,Manager.Count);
  AssertSame('Registered in Manager 2',Service,Manager.Services[0]);
end;

procedure TTestPushNotifications.TestRemoveService;
begin
  TestAddService;
  Manager.RemoveService(MyService);
  AssertEquals('No longer Registered in Manager ',0,Manager.Count);
end;

procedure TTestPushNotifications.TestIndexOfService;
begin
  MyService:=TMyPushService.Create(Manager,'my');
  TMyPushService.Create(Manager,'my2');
  AssertEquals('IndexOf',0,Manager.IndexOfService(MyService));
end;

procedure TTestPushNotifications.TestIndexOfServiceName;
begin
  MyService:=TMyPushService.Create(Manager,'my');
  TMyPushService.Create(Manager,'my2');
  AssertEquals('IndexOf',0,Manager.IndexOfServiceByName('my'));
end;

procedure TTestPushNotifications.TestGetServiceByName;
begin
  TestAddService;
  AssertSame('Result', MyService, Manager.GetServiceByName('my'));

end;

procedure TTestPushNotifications.TestCreateServiceConnection;
begin
  MyService:=TMyPushService.Create(Manager,'my');
  Conn:=MyService.CreateConnection;
  AssertFalse('Not active',Conn.Active);
  AssertEquals('Conn count',1,MyService.ConnectionCount);
  AssertSame('Conn[0]',Conn,MyService.Connections[0]);
end;

procedure TTestPushNotifications.TestDestroyServiceConnection;
begin
  TestCreateServiceConnection;
  FreeAndNil(FConn);
  AssertEquals('Conn count',0,MyService.ConnectionCount);
end;

procedure TTestPushNotifications.TestIndexOfConnection;


begin
  TestCreateServiceConnection;
  Conn2:=MyService.CreateConnection;
  AssertEquals('IndexOf',0,Service.IndexOfConnection(Conn));

end;

procedure TTestPushNotifications.TestIndexOfConnection2;

begin
  TestCreateServiceConnection;
  Conn2:=TPushServiceConnection.Create(Nil);
  AssertEquals('IndexOf',-1,Service.IndexOfConnection(Conn2));
end;

procedure TTestPushNotifications.TestConnectionActive;
begin
  TestCreateServiceConnection;
  Conn.Active:=True;
  AssertTrue('Service running',TPushService.TStatus.Started=MyService.Status);
end;

procedure TTestPushNotifications.TestServiceAppProp;
begin
  MyService:=TMyPushService.Create(Nil,'my');
  MyService.AppProps['a']:='b';
  MyService.AppProps['b']:='c';
  AssertEquals('Approps','b',MyService.AppProps['a']);
end;

procedure TTestPushNotifications.TestDeviceID;
begin
  MyService:=TMyPushService.Create(Nil,'my');
  MyService.MyDeviceID:=[TPushService.TPropPair.Create('a','b'),TPushService.TPropPair.Create('b','c')];
  AssertEquals('Device ID','c',MyService.DeviceIDValue['b']);
end;

procedure TTestPushNotifications.TestDeviceToken;
begin
  MyService:=TMyPushService.Create(Nil,'my');
  MyService.MyDeviceToken:=[TPushService.TPropPair.Create('a','b'),TPushService.TPropPair.Create('b','c')];
  AssertEquals('Device token','c',MyService.DeviceTokenValue['b']);
end;

procedure TTestPushNotifications.TestStartupError;
begin
  MyService:=TMyPushService.Create(Nil,'my');
  MyService.MyStartError:='xyz';
  AssertEquals('Service ','xyz',Service.StartupError);
end;

procedure TTestPushNotifications.TestOnChange;
begin
  TestCreateServiceConnection;
  Conn.OnChange:=@DoChange;
  Conn.Active:=True;
  Conn2:=MyService.CreateConnection;
  Conn2.OnChange:=@DoChange;
  Conn2.Active:=True;
  MyService.Change([TPushService.TChange.DeviceToken, TPushService.TChange.StartupNotifications]);
  AssertEquals('Count',2,FChangeCount);
  AssertTrue('Correct changes',[TPushService.TChange.DeviceToken, TPushService.TChange.StartupNotifications]=FChanges);

end;

procedure TTestPushNotifications.TestOnChange2;
begin
  // only active connections need to get the event
  TestCreateServiceConnection;
  Conn.OnChange:=@DoChange;
  Conn2:=MyService.CreateConnection;
  Conn2.OnChange:=@DoChange;
  MyService.Change([TPushService.TChange.DeviceToken, TPushService.TChange.StartupNotifications]);
  AssertEquals('Count',0,FChangeCount);
  AssertTrue('Correct changes',[]=FChanges);
end;

procedure TTestPushNotifications.TestOnReceive;
begin
  // only active connections need to get the event
  TestCreateServiceConnection;
  Conn.OnReceiveNotification:=@DoReceive;
  Conn.Active:=True;
  Conn2:=MyService.CreateConnection;
  Conn2.OnReceiveNotification:=@DoReceive;
  Conn2.Active:=True;
  Notif:=TPushServiceNotification.Create;
  MyService.ReceiveNotification(Notif);
  AssertEquals('Count',2,FReceiveCount);
  AssertSame('Correct changes',Notif,FReceived);
end;

procedure TTestPushNotifications.TestOnReceive2;
begin
  TestCreateServiceConnection;
  Conn.OnReceiveNotification:=@DoReceive;
  Conn2:=MyService.CreateConnection;
  Conn2.OnReceiveNotification:=@DoReceive;
  Notif:=TPushServiceNotification.Create;
  MyService.ReceiveNotification(Notif);
  AssertEquals('Count',0,FReceiveCount);
end;

function TTestPushNotifications.GetService: TPushService;

begin
  Result:=FMyService;
end;

procedure TTestPushNotifications.DoChange(Sender: TObject; AChange: TPushService.TChanges);
begin
  FChanges:=aChange;
  Inc(FChangeCount);
end;

procedure TTestPushNotifications.DoReceive(Sender: TObject; const aNotification: TPushServiceNotification);
begin
  FReceived:=aNotification;
  inc(FReceiveCount);
end;

procedure TTestPushNotifications.SetUp;

begin
  FManager:=TPushServiceManager.Create;
  FChanges:=[];
  FChangeCount:=0;
  FReceiveCount:=0;
  FReceived:=Nil;
end;

procedure TTestPushNotifications.TearDown;

begin
  FreeAndNil(FNotif);
  FreeAndNil(FConn);
  FreeAndNil(FConn2);
  FreeAndNil(FMyService);
  FreeAndNil(FManager);
end;

initialization

  RegisterTest(TTestPushNotifications);
end.

