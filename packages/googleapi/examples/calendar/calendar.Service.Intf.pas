{ -----------------------------------------------------------------------
  Do not edit !
  
  This file was automatically generated on 2026-10-03 10:20.
  Used command-line parameters:
     -s calendar -L -o calendar -q
  Source OpenAPI document data:
    Title: Calendar API
    Version: v3
  -----------------------------------------------------------------------}
unit calendar.Service.Intf;

{$mode objfpc}
{$h+}
{$modeswitch advancedrecords}

interface

uses
   SysUtils, classes, fpopenapiclient, calendar.Dto;

Type
  // Complex response result types
  
  TAclDeleteResponseKind = (
    AclDeleterkSuccess,
    AclDeleterkUnexpected
  );
  
  TAclDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TAclDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TAclDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TCalendarListDeleteResponseKind = (
    CalendarListDeleterkSuccess,
    CalendarListDeleterkUnexpected
  );
  
  TCalendarListDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TCalendarListDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TCalendarListDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TCalendarsDeleteResponseKind = (
    CalendarsDeleterkSuccess,
    CalendarsDeleterkUnexpected
  );
  
  TCalendarsDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TCalendarsDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TCalendarsDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  TEventsDeleteResponseKind = (
    EventsDeleterkSuccess,
    EventsDeleterkUnexpected
  );
  
  TEventsDeleteResult = record
    public
    FStatusCode: Integer;
    FStatusText: String;
    FContentType: String;
    FResponseKind: TEventsDeleteResponseKind;
    FRawContent: String;
    FRawStream: TStream;
    property StatusCode: Integer read FStatusCode;
    property StatusText: String read FStatusText;
    property ContentType: String read FContentType;
    property ResponseKind: TEventsDeleteResponseKind read FResponseKind;
    property RawContent: String read FRawContent;
    property RawStream: TStream read FRawStream;
    function Success: Boolean; inline;
    function IsClientError: Boolean; inline;
    function IsServerError: Boolean; inline;
    procedure Clear;
    function ExtractRawStream: TStream;
  end;
  
  // Service result types
  TAclRuleServiceResult = specialize TServiceResult<TAclRule>;
  TAclServiceResult = specialize TServiceResult<TAcl>;
  TCalendarListEntryServiceResult = specialize TServiceResult<TCalendarListEntry>;
  TCalendarListServiceResult = specialize TServiceResult<TCalendarList>;
  TCalendarServiceResult = specialize TServiceResult<TCalendar>;
  TChannelServiceResult = specialize TServiceResult<TChannel>;
  TColorsServiceResult = specialize TServiceResult<TColors>;
  TEventServiceResult = specialize TServiceResult<TEvent>;
  TEventsServiceResult = specialize TServiceResult<TEvents>;
  TFreeBusyResponseServiceResult = specialize TServiceResult<TFreeBusyResponse>;
  TSettingServiceResult = specialize TServiceResult<TSetting>;
  TSettingsServiceResult = specialize TServiceResult<TSettings>;
  
  // Service IAcl
  
  IAcl = interface  ['{1B4B432C-234E-4D1E-843E-C7BC512FC252}']
    Function Delete(aCalendarId : string; aRuleId : string) : TAclDeleteResult;
    Function Get(aCalendarId : string; aRuleId : string) : TAclRuleServiceResult;
    Function Insert(aCalendarId : string; aSendNotifications : boolean; aRequest : TAclRule) : TAclRuleServiceResult;
    Function List(aCalendarId : string; aMaxResults : integer; aPageToken : string; aShowDeleted : boolean; aSyncToken : string) : TAclServiceResult;
    Function Patch(aCalendarId : string; aRuleId : string; aSendNotifications : boolean; aRequest : TAclRule) : TAclRuleServiceResult;
    Function Update(aCalendarId : string; aRuleId : string; aSendNotifications : boolean; aRequest : TAclRule) : TAclRuleServiceResult;
    Function Watch(aCalendarId : string; aMaxResults : integer; aPageToken : string; aShowDeleted : boolean; aSyncToken : string; aRequest : TChannel) : TChannelServiceResult;
  end;
  
  // Service ICalendarList
  
  ICalendarList = interface  ['{F3343D9B-A227-498B-BC81-278E66D967D1}']
    Function Delete(aCalendarId : string) : TCalendarListDeleteResult;
    Function Get(aCalendarId : string) : TCalendarListEntryServiceResult;
    Function Insert(aColorRgbFormat : boolean; aRequest : TCalendarListEntry) : TCalendarListEntryServiceResult;
    Function List(aMaxResults : integer; aMinAccessRole : string; aPageToken : string; aShowDeleted : boolean; aShowHidden : boolean; aShowOwnOrganizationOnly : boolean; aSyncToken : string) : TCalendarListServiceResult;
    Function Patch(aCalendarId : string; aColorRgbFormat : boolean; aRequest : TCalendarListEntry) : TCalendarListEntryServiceResult;
    Function Update(aCalendarId : string; aColorRgbFormat : boolean; aRequest : TCalendarListEntry) : TCalendarListEntryServiceResult;
    Function Watch(aMaxResults : integer; aMinAccessRole : string; aPageToken : string; aShowDeleted : boolean; aShowHidden : boolean; aShowOwnOrganizationOnly : boolean; aSyncToken : string; aRequest : TChannel) : TChannelServiceResult;
  end;
  
  // Service ICalendars
  
  ICalendars = interface  ['{019E9E5E-2E93-48FB-93D5-4741C3BB20B3}']
    Function Delete(aCalendarId : string) : TCalendarsDeleteResult;
    Function Get(aCalendarId : string) : TCalendarServiceResult;
    Function Insert(aRequest : TCalendar) : TCalendarServiceResult;
    Function Patch(aCalendarId : string; aRequest : TCalendar) : TCalendarServiceResult;
    Function Update(aCalendarId : string; aRequest : TCalendar) : TCalendarServiceResult;
  end;
  
  // Service IColors
  
  IColors = interface  ['{880E2BCD-1909-4CBD-9C5E-761F29DA4C45}']
    Function Get() : TColorsServiceResult;
  end;
  
  // Service IEvents
  
  IEvents = interface  ['{5A433BEE-3ED7-4D83-9D8A-BA24F0DFE804}']
    Function Delete(aCalendarId : string; aEventId : string; aSendNotifications : boolean; aSendUpdates : string) : TEventsDeleteResult;
    Function Get(aAlwaysIncludeEmail : boolean; aCalendarId : string; aEventId : string; aMaxAttendees : integer; aTimeZone : string) : TEventServiceResult;
    Function Import(aCalendarId : string; aConferenceDataVersion : integer; aEventLabelVersion : integer; aSupportsAttachments : boolean; aRequest : TEvent) : TEventServiceResult;
    Function Insert(aCalendarId : string; aConferenceDataVersion : integer; aEventLabelVersion : integer; aMaxAttendees : integer; aSendNotifications : boolean; aSendUpdates : string; aSupportsAttachments : boolean; aRequest : TEvent) : TEventServiceResult;
    Function Instances(aAlwaysIncludeEmail : boolean; aCalendarId : string; aEventId : string; aMaxAttendees : integer; aMaxResults : integer; aOriginalStart : string; aPageToken : string; aShowDeleted : boolean; aTimeMax : string; aTimeMin : string; aTimeZone : string) : TEventsServiceResult;
    Function List(aAlwaysIncludeEmail : boolean; aCalendarId : string; aEventTypes : string; aICalUID : string; aMaxAttendees : integer; aOrderBy : string; aPageToken : string; aPrivateExtendedProperty : string; aQ : string; aSharedExtendedProperty : string; aShowDeleted : boolean; aShowHiddenInvitations : boolean; aSingleEvents : boolean; aSyncToken : string; aTimeMax : string; aTimeMin : string; aTimeZone : string; aUpdatedMin : string; aMaxResults : integer = 250) : TEventsServiceResult;
    Function Move(aCalendarId : string; aDestination : string; aEventId : string; aSendNotifications : boolean; aSendUpdates : string) : TEventServiceResult;
    Function Patch(aAlwaysIncludeEmail : boolean; aCalendarId : string; aConferenceDataVersion : integer; aEventId : string; aEventLabelVersion : integer; aMaxAttendees : integer; aSendNotifications : boolean; aSendUpdates : string; aSupportsAttachments : boolean; aRequest : TEvent) : TEventServiceResult;
    Function QuickAdd(aCalendarId : string; aSendNotifications : boolean; aSendUpdates : string; aText : string) : TEventServiceResult;
    Function Update(aAlwaysIncludeEmail : boolean; aCalendarId : string; aConferenceDataVersion : integer; aEventId : string; aEventLabelVersion : integer; aMaxAttendees : integer; aSendNotifications : boolean; aSendUpdates : string; aSupportsAttachments : boolean; aRequest : TEvent) : TEventServiceResult;
    Function Watch(aAlwaysIncludeEmail : boolean; aCalendarId : string; aEventTypes : string; aICalUID : string; aMaxAttendees : integer; aOrderBy : string; aPageToken : string; aPrivateExtendedProperty : string; aQ : string; aSharedExtendedProperty : string; aShowDeleted : boolean; aShowHiddenInvitations : boolean; aSingleEvents : boolean; aSyncToken : string; aTimeMax : string; aTimeMin : string; aTimeZone : string; aUpdatedMin : string; aRequest : TChannel; aMaxResults : integer = 250) : TChannelServiceResult;
  end;
  
  // Service IFreebusy
  
  IFreebusy = interface  ['{2AF56FE6-CB50-4A27-A9FE-A3D293D091C7}']
    Function Query(aRequest : TFreeBusyRequest) : TFreeBusyResponseServiceResult;
  end;
  
  // Service ISettings
  
  ISettings = interface  ['{477BF62E-ED3C-4E54-B383-7C1FC56EFEA0}']
    Function Get(aSetting : string) : TSettingServiceResult;
    Function List(aMaxResults : integer; aPageToken : string; aSyncToken : string) : TSettingsServiceResult;
    Function Watch(aMaxResults : integer; aPageToken : string; aSyncToken : string; aRequest : TChannel) : TChannelServiceResult;
  end;
  

implementation

function TAclDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TAclDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TAclDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TAclDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TAclDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TAclDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TCalendarListDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TCalendarListDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TCalendarListDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TCalendarListDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TCalendarListDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TCalendarListDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TCalendarsDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TCalendarsDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TCalendarsDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TCalendarsDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TCalendarsDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TCalendarsDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

function TEventsDeleteResult.Success: Boolean;
begin
  Result:=(FStatusCode >= 200) and (FStatusCode < 300);
end;

function TEventsDeleteResult.IsClientError: Boolean;
begin
  Result:=(FStatusCode >= 400) and (FStatusCode < 500);
end;

function TEventsDeleteResult.IsServerError: Boolean;
begin
  Result:=(FStatusCode >= 500) and (FStatusCode < 600);
end;

procedure TEventsDeleteResult.Clear;
begin
  FStatusCode:=0;
  FStatusText:='';
  FContentType:='';
  FResponseKind:=Default(TEventsDeleteResponseKind);
  FRawContent:='';
  FreeAndNil(FRawStream);
end;

function TEventsDeleteResult.ExtractRawStream: TStream;
begin
  Result:=FRawStream;
  FRawStream:=nil;
end;

end.
