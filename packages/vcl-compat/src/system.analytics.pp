{
    This file is part of the Free Pascal Run Time Library (rtl)
    Copyright (c) 1999-2019 by the Free Pascal development team

    This file provides the base of an application analytics system.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit System.Analytics;

{$MODE OBJFPC}
{$SCOPEDENUMS ON}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Contnrs, System.Classes;
{$ELSE}
uses
  sysutils, contnrs, classes;
{$ENDIF}


type
  IApplicationActivityCacheManager = interface ['{6145E812-8ECA-4B69-994C-26A81B2A84DC}']
    function GetCacheCount: Integer;
    procedure PersistData(const Wait: Boolean);
    procedure ClearData;
    procedure Log(const AMessage: string);
    procedure RemoveEventAtIndex(const Index: Integer);
    function GetEventAtIndex(const Index: Integer): string;
    procedure SetOnDataCacheFull(const AValue: TNotifyEvent);
    function GetOnDataCacheFull: TNotifyEvent;
    procedure SetMaxCacheSize(const AValue: Integer);
    function GetMaxCacheSize: Integer;
    property CacheCount: Integer read GetCacheCount;
    property MaxCacheSize: Integer read GetMaxCacheSize write SetMaxCacheSize;
    property Event[const Index: Integer]: string read GetEventAtIndex;
    property OnDataCacheFull: TNotifyEvent read GetOnDataCacheFull write SetOnDataCacheFull;
  end;

  IAppAnalyticsStartupDataRecorder = interface ['{783ED8DB-86BC-41C7-BBD3-443C19468FF1}']
    procedure AddEnvironmentField(const AKey, AValue: string);
  end;


  IApplicationActivityListener = interface ['{A67DE237-F274-4028-AAC8-DA0BDA0D5D78}']
    procedure TrackAppStart(const aTimeStamp: TDateTime);
    procedure TrackAppExit(const aTimeStamp: TDateTime);
    procedure TrackControlFocused(const aTimeStamp: TDateTime; const aSender: TObject);
    procedure TrackWindowActivated(const aTimeStamp: TDateTime; const aSender: TObject);
    procedure TrackEvent(const aTimeStamp: TDateTime; const aSender, aContext: TObject);
    procedure TrackException(const aTimeStamp: TDateTime; const E: Exception);
  end;


  TAppActivity = (AppStart, AppExit, ControlFocused, WindowActivated, Exception, Custom);
  TAppActivityOptions = set of TAppActivity;

  TAnalyticsManager = class
  private
    FClients: TInterfaceList;
    function GetTrackingEnabled: Boolean;
    function GetClientCount : Integer;
    function GetClient(aIndex : Integer) : IApplicationActivityListener;
  Protected  
    Property Clients[aIndex : Integer] : IApplicationActivityListener Read GetClient;
    Property ClientCount : Integer Read GetClientCount;
  public
    destructor Destroy; override;
    procedure RegisterActivityListener(const aListener: IApplicationActivityListener);
    procedure UnregisterActivityListener(const aListener: IApplicationActivityListener);
    procedure RecordActivity(const aActivity: TAppActivity); overload;
    procedure RecordActivity(const aActivity: TAppActivity; const aSender: TObject); overload;
    procedure RecordActivity(const aActivity: TAppActivity; const aSender: TObject; const aContext: TObject); overload;
    property TrackingEnabled: Boolean read GetTrackingEnabled;
  end;

  EAnalyticsInitializationFailed = class(Exception);

implementation

{ TAnalyticsManager }

destructor TAnalyticsManager.Destroy;
begin
  FreeAndNil(FClients);
  inherited;
end;

function TAnalyticsManager.GetClientCount : Integer;

begin
  if not assigned(FClients) then
    Result:=0
  else
    Result:=FClients.Count;    
end;

function TAnalyticsManager.GetClient(aIndex : Integer) : IApplicationActivityListener;

begin
  if not Assigned(FClients) then
    Raise EListError.Create('Index (%d) out of bounds');
  Result:=(FClients[aIndex]) as IApplicationActivityListener;
end;

procedure TAnalyticsManager.RecordActivity(const aActivity: TAppActivity);
begin
  RecordActivity(aActivity,nil,nil);
end;

procedure TAnalyticsManager.RecordActivity(const aActivity: TAppActivity; const aSender: TObject);
begin
  RecordActivity(aActivity,aSender,nil);
end;

function TAnalyticsManager.GetTrackingEnabled: Boolean;
begin
  Result:=(ClientCount>0)
end;

procedure TAnalyticsManager.RecordActivity(const aActivity: TAppActivity; const aSender,aContext: TObject);

var
  I  : Integer;
  TS : TDateTime;
  A  : IApplicationActivityListener;
  
begin
  if ClientCount=0 then 
    exit;
  TS:=Now;
  for I:=0 to ClientCount-1 do
    begin
    A:=Clients[I];
    case aActivity of
      TAppActivity.AppStart:
        A.TrackAppStart(Ts);
      TAppActivity.AppExit:
        A.TrackAppExit(Ts);
      TAppActivity.ControlFocused:
        A.TrackControlFocused(Ts,aSender);
      TAppActivity.WindowActivated:
        A.TrackWindowActivated(Ts,aSender);
      TAppActivity.Exception:
        if aSender is Exception then
          A.TrackException(Ts,Exception(aSender));
      TAppActivity.Custom:
        A.TrackEvent(Ts,aSender,aContext);
    end;
    A:=Nil;
    end;
end;

procedure TAnalyticsManager.RegisterActivityListener(const aListener: IApplicationActivityListener);

begin
  if Not Assigned(FClients) then
    FClients:=TInterfaceList.Create
  else if FClients.IndexOf(aListener)<>-1 then
    Exit;
  FClients.Add(aListener);
end;


procedure TAnalyticsManager.UnregisterActivityListener(const aListener: IApplicationActivityListener);
begin
  if Assigned(FClients) then
    FClients.Remove(aListener);
end;

end.
