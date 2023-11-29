{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2023 the Free Pascal development team

   Generic push notification service classes.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit System.PushNotifications;

interface

{$MODE OBJFPC}
{$H+}
{$SCOPEDENUMS ON}
{$modeswitch advancedrecords}

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.Sysutils, System.Contnrs, System.JSON, System.Messaging, System.Generics.Collections;
{$ELSE}
  classes, sysutils, contnrs, system.json, system.messaging, generics.collections;
{$ENDIF}

type
  TPushServiceNotification = class;
  TPushServiceManager = class;
  TPushService = class;
  TPushServiceConnection = class;

  { TPushService }

  TPushService = class abstract
  private
    FManager: TPushServiceManager;
    FServiceName: string;
    FConnections : TFPList;
    FProps : TFPStringHashTable;
    function GetAppProp(const aName: string): string;
    function GetConnection(aIndex: Integer): TPushServiceConnection;
    function GetConnectionCount: Integer;
    function GetDeviceIDValue(const aName: string): string;
    function GetDeviceTokenValue(const aName: string): string;
    procedure SetAppProp(const aName: string; aValue: string);
  public
    type
      TPropPair = Specialize TPair<string, string>;
      TPropArray = Specialize TArray<TPropPair>;
      TPushServiceNotificationArray = specialize TArray<TPushServiceNotification>;
      TServiceNames = record
      const
        GCM = 'gcm';
        FCM = 'fcm';
        APS = 'aps';
      end;
      TAppPropNames = record
      const
        GCMAppID = 'gcmappid';
      end;
      TDeviceTokenNames = record
      const
        DeviceToken = 'devicetoken';
      end;
      TDeviceIDNames = record
      const
        DeviceID = 'deviceid';
      end;
  public
    type
      TChange = (Status, DeviceToken, StartupNotifications);
      TChanges = set of TChange;
      TStatus = (Stopped, Starting, Started, StartupError);
  const
    NeedsStartStatus = [TStatus.Stopped, TStatus.StartupError];
  protected
    function FindPropInArray(const aProp : String; anArray : TPropArray) : string;
    procedure RemoveConnection(const aConnection: TPushServiceConnection);
    procedure AddConnection(const aConnection: TPushServiceConnection);
    procedure DoChange(aChanges: TChanges);
    procedure DoReceiveNotification(const aNotification: TPushServiceNotification);
    function GetStatus: TStatus; virtual; abstract;
    function NeedsStart : Boolean;
    procedure StartService; virtual; abstract;
    procedure StopService; virtual; abstract;
    function GetDeviceToken: TPropArray; virtual; abstract;
    function GetDeviceID: TPropArray; virtual; abstract;
    function GetStartupNotifications: TPushServiceNotificationArray; virtual; abstract;
    function GetStartupError: string; virtual; abstract;
  public
    constructor Create(const aOwner: TPushServiceManager; const aServiceName: string); virtual;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function CreateConnection: TPushServiceConnection;
    function IndexOfConnection(const aConnection: TPushServiceConnection): Integer;
    property ServiceName: string read FServiceName;
    property Manager: TPushServiceManager read FManager;
    property ConnectionCount: Integer read GetConnectionCount;
    property Connections[aIndex: Integer]: TPushServiceConnection read GetConnection;
    property Status: TStatus read GetStatus;
    property AppProps[const aName: string]: string read GetAppProp write SetAppProp;
    property DeviceToken: TPropArray read GetDeviceToken;
    property DeviceTokenValue[const aName: string]: string read GetDeviceTokenValue;
    property DeviceID: TPropArray read GetDeviceID;
    property DeviceIDValue[const aName: string]: string read GetDeviceIDValue;
    property StartupNotifications: TPushServiceNotificationArray read GetStartupNotifications;
    property StartupError: string read GetStartupError;
  end;

  { TPushServiceConnection }

  TPushServiceConnection = class sealed
  public
    type
      TReceiveNotificationEvent = procedure(Sender: TObject; const aNotification: TPushServiceNotification) of object;
      TChangeEvent = procedure(Sender: TObject; AChange: TPushService.TChanges) of object;
  private
    FActive: Boolean;
    FOnChange: TChangeEvent;
    FOnReceiveNotification: TReceiveNotificationEvent;
    FService: TPushService;
    procedure SetActive(AValue: Boolean);
  protected
    procedure DoChange(AChanges: TPushService.TChanges);
    procedure DoReceiveNotification(const aNotification: TPushServiceNotification);
  public
    constructor Create(const aService: TPushService); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    property Active: Boolean read FActive write SetActive;
    property Service: TPushService read FService;
    property OnReceiveNotification: TReceiveNotificationEvent read FOnReceiveNotification write FOnReceiveNotification;
    property OnChange: TChangeEvent read FOnChange write FOnChange;
  end;

  { TPushServiceManager }

  TPushServiceManager = class sealed
  private
    Type

      { TServiceRegistration }

      TServiceRegistration = Class
      private
        FOwned: Boolean;
        FService: TPushService;
      public
        Constructor create(aService : TPushService; aOwned : Boolean);
        Destructor Destroy; override;
        Property Service : TPushService Read FService;
        Property Owned : Boolean Read FOwned;
      end;
  class var
    _Instance : TPushServiceManager;
  private
    FList : TFPObjectList;
    function GetCount: Integer;
    function GetService(aIndex: Integer): TPushService;
  public
    class constructor Init;
    class destructor done;
    constructor create;
    destructor Destroy; override;
    procedure AddService(aService: TPushService; aOwnsService: Boolean = True);
    procedure RemoveService(aService: TPushService);
    function GetServiceByName(const aServiceName: string): TPushService;
    function IndexOfServiceByName(const aServiceName: string): Integer;
    function IndexOfService(const aService: TPushService): Integer;
    property Count: Integer read GetCount;
    property Services[aIndex: Integer]: TPushService read GetService; default;
    class property Instance: TPushServiceManager read _Instance;
  end;

  TPushServiceNotification = class abstract
  protected
    function GetDataKey: string; virtual; abstract;
    function GetJson: TJSONObject; virtual; abstract;
    function GetDataObject: TJSONObject; virtual; abstract;
  public
    property DataKey: string read GetDataKey;
    property Json: TJSONObject read GetJson;
    property DataObject: TJSONObject read GetDataObject;
  end;

  EPushServiceError = class(Exception)
  end;


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Generics.Defaults;
{$ELSE}
  Generics.Defaults;
{$ENDIF}

Resourcestring
  SErrServiceRegistered = 'Service %s already registered';
  SErrConnectionNotConnectedToThisService = 'Service Connection not connected to this service';
  SErrConnectionAlreadyConnectedToThisService = 'Service Connection already connected to this service';

{ TPushService }

function TPushService.GetAppProp(const aName: string): string;
begin
  Result:=FProps[aName];
end;

function TPushService.GetConnection(aIndex: Integer): TPushServiceConnection;
begin
  Result:=TPushServiceConnection(FConnections.Items[aIndex]);
end;

function TPushService.GetConnectionCount: Integer;
begin
  Result:=FConnections.Count;
end;

function TPushService.GetDeviceIDValue(const aName: string): string;
begin
  Result:=FindPropInArray(aName,DeviceID);
end;

function TPushService.GetDeviceTokenValue(const aName: string): string;
begin
  Result:=FindPropInArray(aName,DeviceToken);
end;

procedure TPushService.SetAppProp(const aName: string; aValue: string);
begin
  FProps.Items[aName]:=aValue;
end;

function TPushService.FindPropInArray(const aProp: String; anArray: TPropArray): string;

var
  P : TPropPair;

begin
  Result:='';
  For P in anArray do
    if SameText(aProp,P.Key) then
      Exit(P.Value);
end;

procedure TPushService.DoChange(aChanges: TChanges);

var
  P : Pointer;
  C : TPushServiceConnection absolute P;

begin
  For P in FConnections do
    C.DoChange(aChanges);
end;

procedure TPushService.DoReceiveNotification(const aNotification: TPushServiceNotification);

var
  P : Pointer;
  C : TPushServiceConnection absolute P;

begin
  For P in FConnections do
    C.DoReceiveNotification(aNotification);
end;

function TPushService.NeedsStart: Boolean;
begin
  Result:=Status in NeedsStartStatus;
end;

constructor TPushService.Create(const aOwner: TPushServiceManager; const aServiceName: string);
begin
  FManager:=aOwner;
  FServiceName:=aServiceName;
  FConnections:=TFPList.Create;
  FProps:=TFPStringHashTable.Create;
end;

procedure TPushService.AfterConstruction;
begin
  inherited AfterConstruction;
  if Assigned(FManager) then
    FManager.AddService(Self,True);
end;

destructor TPushService.Destroy;
begin
  if Assigned(FManager) then
    FManager.RemoveService(Self); // Will clear FManager
  FreeAndNil(FConnections);
  FreeAndNil(FProps);
  inherited Destroy;
end;

function TPushService.CreateConnection: TPushServiceConnection;
begin
  Result:=TPushServiceConnection.Create(Self);
end;

function TPushService.IndexOfConnection(const aConnection: TPushServiceConnection): Integer;
begin
  Result:=FConnections.IndexOf(aConnection);
end;

procedure TPushService.RemoveConnection(const aConnection: TPushServiceConnection);
begin
  if not (aConnection.FService=Self) then
    raise EPushServiceError.Create(SErrConnectionNotConnectedToThisService);
  FConnections.Remove(aConnection);
end;

procedure TPushService.AddConnection(const aConnection: TPushServiceConnection);
begin
  if FConnections.IndexOf(aConnection)<>-1 then
    raise EPushServiceError.Create(SErrConnectionAlreadyConnectedToThisService);
  aConnection.FService:=Self;
  FConnections.Add(aConnection);
end;

{ TPushServiceConnection }

procedure TPushServiceConnection.SetActive(AValue: Boolean);

begin
  if FActive=AValue then
    Exit;
  if Service.NeedsStart then
    Service.StartService;
  FActive:=AValue;
end;

procedure TPushServiceConnection.DoChange(aChanges: TPushService.TChanges);
begin
  if Active and Assigned(OnChange) then
    OnChange(Self,aChanges);
end;

procedure TPushServiceConnection.DoReceiveNotification(const aNotification: TPushServiceNotification);
begin
  if Active and Assigned(OnReceiveNotification) then
    OnReceiveNotification(Self,aNotification);
end;

constructor TPushServiceConnection.Create(const aService: TPushService);
begin
  FService:=aService;
end;

destructor TPushServiceConnection.Destroy;
begin
  if Assigned(FService) then
    FService.RemoveConnection(Self);
  inherited Destroy;
end;

procedure TPushServiceConnection.AfterConstruction;
begin
  inherited AfterConstruction;
  if Assigned(FService) then
    FService.AddConnection(Self);
end;

{ TPushServiceManager }

function TPushServiceManager.GetCount: Integer;
begin
  Result:=FList.Count;
end;

function TPushServiceManager.GetService(aIndex: Integer): TPushService;
begin
  Result:=TServiceRegistration(FList.Items[aIndex]).Service;
end;

class constructor TPushServiceManager.Init;
begin
  _instance:=TPushServiceManager.Create;
end;

class destructor TPushServiceManager.done;
begin
  FreeAndNil(_instance);
end;

constructor TPushServiceManager.create;
begin
  FList:=TFPObjectList.Create(True);
end;

destructor TPushServiceManager.Destroy;

var
  I : integer;

begin
  For I:=0 to FList.Count-1 do
    GetService(I).FManager:=nil;
  FreeAndNil(FList);
  inherited Destroy;
end;


procedure TPushServiceManager.AddService(aService: TPushService; aOwnsService: Boolean);
begin
  if IndexOfService(aService)<>-1 then
    Raise EPushServiceError.CreateFmt(SErrServiceRegistered,[aService.ServiceName]);
  FList.Add(TServiceRegistration.Create(aService,aOwnsService));
  aService.FManager:=Self;
end;

procedure TPushServiceManager.RemoveService(aService: TPushService);

var
  Idx : Integer;

begin
  Idx:=IndexOfService(aService);
  if Idx<0 then
    Exit;
  aService.FManager:=Nil;
  TServiceRegistration(FList[Idx]).FService:=nil;
  FList.Delete(Idx);
end;

function TPushServiceManager.GetServiceByName(const aServiceName: string): TPushService;

var
  Idx : integer;

begin
  Result:=nil;
  Idx:=IndexOfServiceByName(aServiceName);
  if Idx<>-1 then
    Result:=GetService(Idx);
end;

function TPushServiceManager.IndexOfServiceByName(const aServiceName: string): Integer;
begin
  Result:=FList.Count-1;
  While (Result>=0) and Not SameText(GetService(Result).ServiceName,aServiceName) do
    Dec(Result);
end;

function TPushServiceManager.IndexOfService(const aService: TPushService): Integer;

begin
  Result:=FList.Count-1;
  While (Result>=0) and (GetService(Result)<>aService) do
    Dec(Result);
end;

{ TPushServiceManager.TServiceRegistration }

constructor TPushServiceManager.TServiceRegistration.create(aService: TPushService; aOwned: Boolean);
begin
  FService:=aService;
  FOwned:=aOwned;
end;

destructor TPushServiceManager.TServiceRegistration.Destroy;
begin
  if FOwned then
    FreeAndNil(FService);
  inherited Destroy;
end;

end.


