{
    This file is part of the Free Pascal Run Time Library (rtl)
    Copyright (c) 2023 by the Free Pascal development team

    This file provides a basic Delphi-compatible notifications engine.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System.Notification;

{$mode objfpc}
{$H+}
{$SCOPEDENUMS ON}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, System.Generics.Collections, System.Messaging;
  {$ELSE}
  Classes, SysUtils, Generics.Collections, System.Messaging;
  {$ENDIF}

type
  { TNotification }

  TRepeatInterval = (None, Second, Minute, Hour, Day, Week, Weekday, Month, Quarter, Year, Era);

  TNotification = class(TPersistent)
  public
    Name: string;
    Title: string;
    AlertBody: string;
    AlertAction: string;
    Number: Integer;
    FireDate: TDateTime;
    EnableSound: Boolean;
    SoundName: string;
    HasAction: Boolean;
    RepeatInterval: TRepeatInterval;
    ChannelId: string;
  Public
    constructor Create;
    procedure Assign(Source : TPersistent); override;
  end;

  TLockscreenVisibility = (Public, Private, Secret);
  TImportance = (None, Default, Min, Low, High);

  { TChannel }

  TChannel = class(TPersistent)
  public
    Id: string;
    Title: string;
    Description: string;
    LockscreenVisibility: TLockscreenVisibility;
    Importance: TImportance;
    ShouldShowLights: Boolean;
    ShouldVibrate: Boolean;
    ShouldShowBadge: Boolean;
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  end;

{ TBaseNotificationCenter }

  ELocalNotification = class(Exception);
  ELocalNotificationAccess = class(ELocalNotification);
  
  TNotifications = array of TNotification;
  TAuthorizationStatus = (NotDetermined, Restricted, Denied, Authorized);
  TOnReceiveLocalNotification = procedure (Sender: TObject; aNotification: TNotification) of object;
  TOnPermissionRequestResult = procedure (Sender: TObject; const aIsGranted: Boolean) of object;
  TPermissionRequestResultMessage = class(specialize TMessage<Boolean>);
  TChannels = specialize TList<TChannel>;

  TMessage = System.Messaging.TMessageBase;
  TMessageNotification = Specialize TMessage<TNotification>;

  TBaseNotificationCenter = Class;
  TBaseNotificationCenterClass = class of TBaseNotificationCenter;

  TBaseNotificationCenter = class
  private
    FOnReceiveLocalNotification: TOnReceiveLocalNotification;
    FOnPermissionRequestResult: TOnPermissionRequestResult;
    class function _PlatformInstance: TBaseNotificationCenter; static;
  protected
    class var _Class : TBaseNotificationCenterClass;
  protected
    class function GetInstance: TBaseNotificationCenter; virtual; abstract;
    function CreatePermissionResultMessage(aIsGranted: Boolean): TPermissionRequestResultMessage; virtual;
    procedure DoPlatformInitialize; virtual;
    procedure DoRequestPermission; virtual; abstract;
    function DoAuthorizationStatus: TAuthorizationStatus; virtual; abstract;
    procedure DoScheduleNotification(const aNotification: TNotification); virtual; abstract;
    procedure DoPresentNotification(const aNotification: TNotification); virtual; abstract;
    procedure DoCancelNotification(const aName: string); overload; virtual; abstract;
    procedure DoCancelNotification(const aNotification: TNotification); overload; virtual; abstract;
    procedure DoCancelAllNotifications; virtual; abstract;
    procedure DoCreateOrUpdateChannel(const aChannel: TChannel); virtual;
    procedure DoDeleteChannel(const aChannelId: string); virtual;
    procedure DoGetAllChannels(const aChannels: TChannels); virtual;
    procedure DoSetIconBadgeNumber(const aCount: Integer); virtual; abstract;
    function DoGetIconBadgeNumber: Integer; virtual; abstract;
    procedure DoResetIconBadgeNumber; virtual; abstract;
    procedure DoReceiveLocalNotification(const Sender: TObject; const Msg: TMessage); virtual;
    procedure DoReceivePermissionRequestResult(const Sender: TObject; const Msg: TMessage); virtual;
    procedure DoLoaded; virtual;
    procedure NotifyPermissionRequestResult(const aIsGranted: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RequestPermission;
    function AuthorizationStatus: TAuthorizationStatus;
    function CreateNotification: TNotification; overload;
    function CreateNotification(const aName, aAlertBody: string; const aFireDate: TDateTime): TNotification; overload;
    procedure PresentNotification(const aNotification: TNotification);
    procedure ScheduleNotification(const aNotification: TNotification);
    procedure CancelAll;
    procedure CancelNotification(const aName: string);
    procedure CreateOrUpdateChannel(const aChannel: TChannel);
    procedure DeleteChannel(const aChannelId: string);
    procedure GetAllChannels(const aChannels: TChannels);
    property ApplicationIconBadgeNumber: Integer read DoGetIconBadgeNumber write DoSetIconBadgeNumber;
    property OnReceiveLocalNotification: TOnReceiveLocalNotification read FOnReceiveLocalNotification write FOnReceiveLocalNotification;
    property OnPermissionRequestResult: TOnPermissionRequestResult read FOnPermissionRequestResult write FOnPermissionRequestResult;
  end;

Type

  { TEventedNotificationCenter }
  THandleNotificationEvent = Procedure(Sender : TObject; aNotification : TNotification) of object;
  THandleChannelEvent = Procedure(Sender : TObject; aChannel : TChannel) of object;
  ThandleChannelsEvent = Procedure(Sender : TObject; aChannels : TChannels) of object;
  THandleNotificationByNameEvent = Procedure(Sender : TObject; aName : String) of object;
  TGetNotificationIconBadgeNumberEvent = Procedure(Sender : TObject; var aNumber : Integer) of object;
  TSetNotificationIconBadgeNumberEvent = Procedure(Sender : TObject; const aNumber : Integer) of object;

  TEventedNotificationCenter = Class(TBaseNotificationCenter)
  Private
    class var _instance : TEventedNotificationCenter;
  private
    FAuthorizationStatus: TAuthorizationStatus;
    FOnCancelAll: TNotifyEvent;
    FOnCancelAllNotifications: TNotifyEvent;
    FOnCancelNotification: THandleNotificationEvent;
    FOnCancelNotificationByName: THandleNotificationByNameEvent;
    FOnCreateChannel: THandleChannelEvent;
    FOnDeleteChannel: THandleNotificationByNameEvent;
    FOnGetChannels: THandleChannelsEvent;
    FOnGetIconBadgeNumber: TGetNotificationIconBadgeNumberEvent;
    FOnPresentNotification: THandleNotificationEvent;
    FOnRequestPermission: TNotifyEvent;
    FOnResetIconBadgeNumber: TNotifyEvent;
    FOnScheduleNotification: THandleNotificationEvent;
    FOnSetIconBadgeNumber: TSetNotificationIconBadgeNumberEvent;
  protected
    function DoAuthorizationStatus: TAuthorizationStatus; override;
    procedure DoCancelAllNotifications; override;
    procedure DoCancelNotification(const aName: string); overload; override;
    procedure DoCancelNotification(const aNotification: TNotification); overload; override;
    function DoGetIconBadgeNumber: Integer; override;
    procedure DoPresentNotification(const aNotification: TNotification); override;
    procedure DoRequestPermission; override;
    procedure DoResetIconBadgeNumber; override;
    procedure DoCreateOrUpdateChannel(const aChannel: TChannel); override;
    procedure DoDeleteChannel(const aChannelId: string); override;
    procedure DoGetAllChannels(const aChannels: TChannels); override;
    procedure DoScheduleNotification(const aNotification: TNotification); override;
    procedure DoSetIconBadgeNumber(const aCount: Integer); override;
    class function GetInstance: TBaseNotificationCenter; override;
  Public
    constructor Create;
    class constructor Init;
    class destructor Done;
    Class property Instance : TEventedNotificationCenter Read _Instance;
    Property OnRequestPermission : TNotifyEvent Read FOnRequestPermission Write FOnRequestPermission;
    property OnResetIconBadgeNumber : TNotifyEvent Read FOnResetIconBadgeNumber Write FOnResetIconBadgeNumber;
    property OnCancelAllNotifications: TNotifyEvent Read FOnCancelAllNotifications Write FOnCancelAllNotifications;
    Property OnCancelNotification : THandleNotificationEvent Read FOnCancelNotification Write FOnCancelNotification;
    Property OnCancelNotificationByName : THandleNotificationByNameEvent Read FOnCancelNotificationByName Write FOnCancelNotificationByName;
    Property OnPresentNotification : THandleNotificationEvent Read FOnPresentNotification Write FOnPresentNotification;
    Property OnScheduleNotification : THandleNotificationEvent Read FOnScheduleNotification Write FOnScheduleNotification;
    Property OnGetIconBadgeNumber : TGetNotificationIconBadgeNumberEvent Read FOnGetIconBadgeNumber Write FOnGetIconBadgeNumber;
    Property OnSetIconBadgeNumber : TSetNotificationIconBadgeNumberEvent Read FOnSetIconBadgeNumber Write FOnSetIconBadgeNumber;
    Property AuthorizationStatus : TAuthorizationStatus Read FAuthorizationStatus Write FAuthorizationStatus;
    Property OnDeleteChannel : THandleNotificationByNameEvent Read FOnDeleteChannel Write FOnDeleteChannel;
    Property OnCreateChannel : THandleChannelEvent Read FOnCreateChannel Write FOnCreateChannel;
    Property OnGetChannels : THandleChannelsEvent Read FOnGetChannels Write FOnGetChannels;
  end;

  { TNotificationCenter }
  
  { TCustomNotificationCenter }

  TCustomNotificationCenter = class(TComponent)
  private
    FCenter: TBaseNotificationCenter;
    FOnReceiveLocalNotification: TOnReceiveLocalNotification;
    FOnPermissionRequestResult: TOnPermissionRequestResult;
    function HaveCenter : Boolean; inline;
  protected
    procedure DoPlatformInitialize; virtual;
    procedure DoRequestPermission; virtual;
    function DoAuthorizationStatus: TAuthorizationStatus; virtual;
    procedure DoScheduleNotification(const aNotification: TNotification); virtual;
    procedure DoPresentNotification(const aNotification: TNotification); virtual;
    procedure DoCancelNotification(const aName: string); overload; virtual;
    procedure DoCancelNotification(const aNotification: TNotification); overload; virtual;
    procedure DoCancelAllNotifications; virtual;
    procedure DoCreateOrUpdateChannel(const aChannel: TChannel); virtual;
    procedure DoDeleteChannel(const aChannelId: string); virtual;
    procedure DoGetAllChannels(const aChannels: TChannels); virtual;
    procedure DoSetIconBadgeNumber(const aCount: Integer); virtual;
    function DoGetIconBadgeNumber: Integer; virtual;
    procedure DoResetIconBadgeNumber; virtual;
    procedure DoReceiveLocalNotification(const Sender: TObject; const aMsg: TMessage); virtual;
    procedure DoReceivePermissionRequestResult(const Sender: TObject; const aMsg: TMessage); virtual;
    procedure DoLoaded; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function Supported: Boolean; inline;
    procedure PlatformInitialize;
    procedure RequestPermission;
    function AuthorizationStatus: TAuthorizationStatus;
    function CreateNotification: TNotification; overload;
    function CreateNotification(const aName, aAlertBody: string; const aFireDate: TDateTime): TNotification; overload;
    function CreateChannel: TChannel; overload;
    function CreateChannel(const aId: string; const aTitle: string; const aDescription: string = ''): TChannel; overload;
    procedure PresentNotification(const aNotification: TNotification);
    procedure ScheduleNotification(const aNotification: TNotification);
    procedure CancelAll;
    procedure CancelNotification(const aName: string);
    procedure ResetIconBadgeNumber;
    procedure CreateOrUpdateChannel(const aChannel: TChannel);
    procedure DeleteChannel(const aChannelId: string);
    procedure GetAllChannels(const aChannels: TChannels);
    property ApplicationIconBadgeNumber: Integer read DoGetIconBadgeNumber write DoSetIconBadgeNumber default 0;
    property OnReceiveLocalNotification: TOnReceiveLocalNotification read FOnReceiveLocalNotification
      write FOnReceiveLocalNotification;
    property OnPermissionRequestResult: TOnPermissionRequestResult read FOnPermissionRequestResult
      write FOnPermissionRequestResult;
  end;

  TNotificationCenter = class(TCustomNotificationCenter)
  published
    property ApplicationIconBadgeNumber;
    property OnReceiveLocalNotification;
    property OnPermissionRequestResult;
  end;

implementation


{ TNotification }

procedure TNotification.Assign(Source: TPersistent);
var
  Src : TNotification absolute source;

begin
  if Source is TNotification then
    begin
    Name:=Src.Name;
    Title:=Src.Title;
    AlertBody:=Src.AlertBody;
    AlertAction:=Src.AlertAction;
    Number:=Src.Number;
    FireDate:=Src.FireDate;
    EnableSound:=Src.EnableSound;
    SoundName:=Src.SoundName;
    HasAction:=Src.HasAction;
    RepeatInterval:=Src.RepeatInterval;
    ChannelId:=Src.ChannelId;
    end
  else
    inherited Assign(Source);
end;


constructor TNotification.Create;

begin
  EnableSound:=True;
  HasAction:=False;
  FireDate:=Now;
  RepeatInterval:=TRepeatInterval.None;
  Number:=0;
end;


{ TChannel }

procedure TChannel.Assign(Source: TPersistent);

var
  Src : TChannel absolute Source;

begin
  if Source is TChannel then
    begin
    Id:=Src.Id;
    Title:=Src.Title;
    Description:=Src.Description;
    LockscreenVisibility:=Src.LockscreenVisibility;
    Importance:=Src.Importance;
    ShouldShowLights:=Src.ShouldShowLights;
    ShouldVibrate:=Src.ShouldVibrate;
    ShouldShowBadge:=Src.ShouldShowBadge;
    end
  else
    inherited Assign(Src);
end;


constructor TChannel.Create;

begin
  Importance:=TImportance.Default;
  LockscreenVisibility:=TLockscreenVisibility.&Public;
end;


{ TEventedNotificationCenter }

function TEventedNotificationCenter.DoAuthorizationStatus: TAuthorizationStatus;
begin
  Result:=FAuthorizationStatus;
end;

procedure TEventedNotificationCenter.DoCancelAllNotifications;
begin
  If Assigned(FOnCancelAllNotifications) then
    FOnCancelAllNotifications(Self);
end;

procedure TEventedNotificationCenter.DoCancelNotification(const aName: string);
begin
  if Assigned(OnCancelNotificationByName) then
    OnCancelNotificationByName(Self,aName);
end;

procedure TEventedNotificationCenter.DoCancelNotification(const aNotification: TNotification);
begin
  if Assigned(OnCancelNotification) then
    FOnCancelNotification(Self,aNotification);
end;

function TEventedNotificationCenter.DoGetIconBadgeNumber: Integer;
begin
  if assigned(OnGetIconBadgeNumber) then
    OnGetIconBadgeNumber(Self,Result)
  else
    Result:=0;
end;

procedure TEventedNotificationCenter.DoPresentNotification(const aNotification: TNotification);
begin
  if Assigned(OnPresentNotification) then
    OnPresentNotification(Self,aNotification);
end;

procedure TEventedNotificationCenter.DoRequestPermission;
begin
  if Assigned(OnRequestPermission) then
    OnRequestPermission(Self);
end;

procedure TEventedNotificationCenter.DoResetIconBadgeNumber;
begin
  if Assigned(OnResetIconBadgeNumber) then
    OnResetIconBadgeNumber(Self);
end;

procedure TEventedNotificationCenter.DoCreateOrUpdateChannel(const aChannel: TChannel);
begin
  If Assigned(OnCreateChannel) then
    FOnCreateChannel(Self,aChannel);
end;

procedure TEventedNotificationCenter.DoDeleteChannel(const aChannelId: string);
begin
  If Assigned(OnDeleteChannel) then
    OnDeleteChannel(Self,aChannelId);

end;

procedure TEventedNotificationCenter.DoGetAllChannels(const aChannels: TChannels);
begin
  If Assigned(OnGetChannels) then
    OnGetChannels(Self,aChannels);
end;

procedure TEventedNotificationCenter.DoScheduleNotification(const aNotification: TNotification);
begin
  if Assigned(OnScheduleNotification) then
    OnScheduleNotification(Self,aNotification);
end;

procedure TEventedNotificationCenter.DoSetIconBadgeNumber(const aCount: Integer);
begin
  if Assigned(OnSetIconBadgeNumber) then
    OnSetIconBadgeNumber(Self,aCount);
end;

class function TEventedNotificationCenter.GetInstance: TBaseNotificationCenter;
begin
  Result:=_instance;
end;

constructor TEventedNotificationCenter.Create;
begin
  FAuthorizationStatus:=TAuthorizationStatus.Denied;
end;

class constructor TEventedNotificationCenter.Init;
begin
  _Instance:=TEventedNotificationCenter.Create;
end;

class destructor TEventedNotificationCenter.Done;
begin
  FreeAndNil(_Instance);
end;

{ TBaseNotificationCenter }

class function TBaseNotificationCenter._PlatformInstance: TBaseNotificationCenter;
begin
  if not assigned(_Class) then
    _Class:=TEventedNotificationCenter;
  Result:=_Class.GetInstance;
end;

procedure TBaseNotificationCenter.DoPlatformInitialize;
begin
  // For implementation in descendants
end;

procedure TBaseNotificationCenter.DoCreateOrUpdateChannel(const aChannel: TChannel);
begin
  // For implementation in descendants
end;

procedure TBaseNotificationCenter.DoDeleteChannel(const aChannelId: string);
begin
  // For implementation in descendants
end;

procedure TBaseNotificationCenter.DoGetAllChannels(const aChannels: TChannels);
begin
  // For implementation in descendants
end;

procedure TBaseNotificationCenter.DoReceiveLocalNotification(const Sender: TObject; const Msg: TMessage);
begin
  if Assigned(FOnReceiveLocalNotification) then
    exit;
  if (Msg is TMessageNotification) then
    FOnReceiveLocalNotification(Self,TMessageNotification(Msg).Value);
end;

procedure TBaseNotificationCenter.DoReceivePermissionRequestResult(const Sender: TObject; const Msg: TMessage);
begin
  if not Assigned(FOnPermissionRequestResult) then
    Exit;
  if (Msg is TPermissionRequestResultMessage) then
    FOnPermissionRequestResult(Self,TPermissionRequestResultMessage(Msg).Value);
end;

procedure TBaseNotificationCenter.DoLoaded;
begin

end;

function TBaseNotificationCenter.CreatePermissionResultMessage(aIsGranted : Boolean) : TPermissionRequestResultMessage;

begin
  Result:=TPermissionRequestResultMessage.Create(aIsGranted);
end;

procedure TBaseNotificationCenter.NotifyPermissionRequestResult(const aIsGranted: Boolean);

var
  Msg : TPermissionRequestResultMessage;

begin
  Msg:=CreatePermissionResultMessage(aIsGranted);
  TMessageManager.DefaultManager.SendMessage(Self,Msg);
end;

constructor TBaseNotificationCenter.Create;
begin
  With TMessageManager.DefaultManager do
    begin
    SubscribeToMessage(TPermissionRequestResultMessage,@DoReceivePermissionRequestResult);
    SubscribeToMessage(TMessageNotification,@DoReceiveLocalNotification);
    end;

end;

destructor TBaseNotificationCenter.Destroy;
begin
  With TMessageManager.DefaultManager do
    begin
    UnSubscribe(TMessageNotification,@DoReceiveLocalNotification);
    UnSubscribe(TPermissionRequestResultMessage,@DoReceivePermissionRequestResult);
    end;
  inherited Destroy;
end;

procedure TBaseNotificationCenter.RequestPermission;
begin
  DoRequestPermission;
end;

function TBaseNotificationCenter.AuthorizationStatus: TAuthorizationStatus;
begin
  Result:=DoAuthorizationStatus;
end;

function TBaseNotificationCenter.CreateNotification: TNotification;
begin
  Result:=TNotification.Create;
end;

function TBaseNotificationCenter.CreateNotification(const aName, aAlertBody: string; const aFireDate: TDateTime): TNotification;
begin
  Result:=CreateNotification();
  With Result do
    begin
    Name:=aName;
    AlertBody:=aAlertBody;
    FireDate:=aFireDate;
    end;
end;

procedure TBaseNotificationCenter.PresentNotification(const aNotification: TNotification);
begin
   DoPresentNotification(aNotification);
end;

procedure TBaseNotificationCenter.ScheduleNotification(const aNotification: TNotification);
begin
  DoScheduleNotification(aNotification);
end;

procedure TBaseNotificationCenter.CancelAll;
begin

end;

procedure TBaseNotificationCenter.CancelNotification(const aName: string);
begin
  DoCancelNotification(aName);
end;

procedure TBaseNotificationCenter.CreateOrUpdateChannel(const aChannel: TChannel);
begin
  DoCreateOrUpdateChannel(aChannel);
end;

procedure TBaseNotificationCenter.DeleteChannel(const aChannelId: string);
begin
  DoDeleteChannel(aChannelId);
end;

procedure TBaseNotificationCenter.GetAllChannels(const aChannels: TChannels);
begin
  DoGetAllChannels(aChannels);
end;

{ TCustomNotificationCenter }

function TCustomNotificationCenter.HaveCenter: Boolean;
begin
  Result:=Assigned(FCenter);
end;

procedure TCustomNotificationCenter.DoPlatformInitialize;

begin
  if not HaveCenter then
    exit;
  FCenter.DoPlatformInitialize;
end;


procedure TCustomNotificationCenter.DoRequestPermission;

begin
  if not HaveCenter then
    exit;
  FCenter.DoRequestPermission;
end;


function TCustomNotificationCenter.DoAuthorizationStatus: TAuthorizationStatus;

begin
  Result:=TAuthorizationStatus.NotDetermined;
  if not HaveCenter then
    exit;
  Result:=FCenter.DoAuthorizationStatus;
end;


procedure TCustomNotificationCenter.DoScheduleNotification(const aNotification: TNotification);

begin
  if not HaveCenter then
    exit;
  FCenter.DoScheduleNotification(aNotification);
end;


procedure TCustomNotificationCenter.DoPresentNotification(const aNotification: TNotification);

begin
  if not HaveCenter then
    exit;
  FCenter.DoPresentNotification(aNotification);
end;


procedure TCustomNotificationCenter.DoCancelNotification(const aName: string);

begin
  if not HaveCenter then
    exit;
  FCenter.DoCancelNotification(aName);
end;


procedure TCustomNotificationCenter.DoCancelNotification(const aNotification: TNotification);

begin
  if not HaveCenter then
    exit;
  FCenter.DoCancelNotification(aNotification);
end;


procedure TCustomNotificationCenter.DoCancelAllNotifications;

begin
  if not HaveCenter then
    exit;
  FCenter.DoCancelAllNotifications;
end;


procedure TCustomNotificationCenter.DoCreateOrUpdateChannel(const aChannel: TChannel);

begin
  if not HaveCenter then
    exit;
  FCenter.DoCreateOrUpdateChannel(aChannel);
end;


procedure TCustomNotificationCenter.DoDeleteChannel(const aChannelId: string);

begin
  if not HaveCenter then
    exit;
  FCenter.DoDeleteChannel(aChannelId);
end;


procedure TCustomNotificationCenter.DoGetAllChannels(const aChannels: TChannels);

begin
  if not HaveCenter then
    exit;
  FCenter.DoGetAllChannels(aChannels);
end;


procedure TCustomNotificationCenter.DoSetIconBadgeNumber(const aCount: Integer);

begin
  if not HaveCenter then
    exit;
  FCenter.DoSetIconBadgeNumber(aCount);
end;


function TCustomNotificationCenter.DoGetIconBadgeNumber: Integer;

begin
  Result:=0;
  if not HaveCenter then
    exit;
  Result:=FCenter.DoGetIconBadgeNumber;
end;


procedure TCustomNotificationCenter.DoResetIconBadgeNumber;

begin
  if not HaveCenter then
    exit;
  FCenter.DoResetIconBadgeNumber;
end;


procedure TCustomNotificationCenter.DoReceiveLocalNotification(const Sender: TObject; const aMsg: TMessage);

begin
  if not Assigned(FOnReceiveLocalNotification) then
    exit;
  if (aMsg is TMessageNotification) then
    FOnReceiveLocalNotification(Self, TMessageNotification(aMsg).Value);
end;


procedure TCustomNotificationCenter.DoReceivePermissionRequestResult(const Sender: TObject; const aMsg: TMessage);

begin
  if not Assigned(FOnPermissionRequestResult) then
    exit;
  if (aMsg is TPermissionRequestResultMessage) then
    FOnPermissionRequestResult(Self, TPermissionRequestResultMessage(aMsg).Value);
end;


procedure TCustomNotificationCenter.DoLoaded;

begin
  if not HaveCenter then
    exit;
  FCenter.DoLoaded;
end;


constructor TCustomNotificationCenter.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  FCenter:=TBaseNotificationCenter._PlatformInstance;
  With TMessageManager.DefaultManager do
    begin
    SubscribeToMessage(TMessageNotification,@DoReceiveLocalNotification);
    SubscribeToMessage(TPermissionRequestResultMessage,@DoReceivePermissionRequestResult);
    end;
end;


destructor TCustomNotificationCenter.Destroy;

begin
  FCenter:=Nil; // We don't own it
  With TMessageManager.DefaultManager do
    begin
    Unsubscribe(TPermissionRequestResultMessage,@DoReceivePermissionRequestResult);
    Unsubscribe(TMessageNotification,@DoReceiveLocalNotification);
    end;
  inherited Destroy;
end;


procedure TCustomNotificationCenter.Loaded;

begin
  inherited Loaded;
  DoLoaded;
end;


function TCustomNotificationCenter.Supported: Boolean;

begin
  Result:=HaveCenter;
end;


procedure TCustomNotificationCenter.PlatformInitialize;

begin
  DoPlatformInitialize;
end;


procedure TCustomNotificationCenter.RequestPermission;

begin
  DoRequestPermission;
end;


function TCustomNotificationCenter.AuthorizationStatus: TAuthorizationStatus;

begin
  Result:=DoAuthorizationStatus;
end;


function TCustomNotificationCenter.CreateNotification: TNotification;

begin
  Result:=Nil;
  If not HaveCenter then
    exit;
  Result:=FCenter.CreateNotification;
end;


function TCustomNotificationCenter.CreateNotification(const aName, aAlertBody: string; const aFireDate: TDateTime): TNotification;

begin
  Result:=Nil;
  If not HaveCenter then
    exit;
  Result:=FCenter.CreateNotification(aName,aAlertBody,aFireDate);
end;


function TCustomNotificationCenter.CreateChannel: TChannel;

begin
  Result:=Nil;
  if Not HaveCenter then
    exit;
  Result:=TChannel.Create;
end;


function TCustomNotificationCenter.CreateChannel(const aId: string; const aTitle: string; const aDescription: string): TChannel;

begin
  Result:=CreateChannel();
  If not Assigned(Result) then
    exit;
  With Result do
    begin
    Id:=aId;
    Title:=aTitle;
    Description:=aDescription;
    end;
end;


procedure TCustomNotificationCenter.PresentNotification(const aNotification: TNotification);

begin
  DoPresentNotification(aNotification);
end;


procedure TCustomNotificationCenter.ScheduleNotification(const aNotification: TNotification);

begin
  DoScheduleNotification(aNotification);
end;


procedure TCustomNotificationCenter.CancelAll;

begin
  DoCancelAllNotifications;
end;


procedure TCustomNotificationCenter.CancelNotification(const aName: string);

begin
  DoCancelNotification(aName);
end;

procedure TCustomNotificationCenter.ResetIconBadgeNumber;
begin
  DoResetIconBadgeNumber;
end;


procedure TCustomNotificationCenter.CreateOrUpdateChannel(const aChannel: TChannel);

begin
  DoCreateOrUpdateChannel(aChannel)
end;


procedure TCustomNotificationCenter.DeleteChannel(const aChannelId: string);

begin
  DoDeleteChannel(aChannelId);
end;


procedure TCustomNotificationCenter.GetAllChannels(const aChannels: TChannels);

begin
   DoGetAllChannels(aChannels);
end;

end.
