{ -----------------------------------------------------------------------
  Do not edit !
  
  This file was automatically generated on 2026-10-03 10:20.
  Used command-line parameters:
     -s calendar -L -o calendar -q
  Source OpenAPI document data:
    Title: Calendar API
    Version: v3
  -----------------------------------------------------------------------}
unit calendar.Service.Impl;

{$mode objfpc}
{$h+}

interface

uses
  classes, fpopenapiclient
  , calendar.Service.Intf                     // Service definition 
  , calendar.Dto;

Type
  // Service IAcl
  
  TAclProxy = Class (TFPOpenAPIServiceClient,IAcl)
    Function Delete(aCalendarId : string; aRuleId : string) : TAclDeleteResult;
    Function Get(aCalendarId : string; aRuleId : string) : TAclRuleServiceResult;
    Function Insert(aCalendarId : string; aSendNotifications : boolean; aRequest : TAclRule) : TAclRuleServiceResult;
    Function List(aCalendarId : string; aMaxResults : integer; aPageToken : string; aShowDeleted : boolean; aSyncToken : string) : TAclServiceResult;
    Function Patch(aCalendarId : string; aRuleId : string; aSendNotifications : boolean; aRequest : TAclRule) : TAclRuleServiceResult;
    Function Update(aCalendarId : string; aRuleId : string; aSendNotifications : boolean; aRequest : TAclRule) : TAclRuleServiceResult;
    Function Watch(aCalendarId : string; aMaxResults : integer; aPageToken : string; aShowDeleted : boolean; aSyncToken : string; aRequest : TChannel) : TChannelServiceResult;
  end;
  
  // Service ICalendarList
  
  TCalendarListProxy = Class (TFPOpenAPIServiceClient,ICalendarList)
    Function Delete(aCalendarId : string) : TCalendarListDeleteResult;
    Function Get(aCalendarId : string) : TCalendarListEntryServiceResult;
    Function Insert(aColorRgbFormat : boolean; aRequest : TCalendarListEntry) : TCalendarListEntryServiceResult;
    Function List(aMaxResults : integer; aMinAccessRole : string; aPageToken : string; aShowDeleted : boolean; aShowHidden : boolean; aShowOwnOrganizationOnly : boolean; aSyncToken : string) : TCalendarListServiceResult;
    Function Patch(aCalendarId : string; aColorRgbFormat : boolean; aRequest : TCalendarListEntry) : TCalendarListEntryServiceResult;
    Function Update(aCalendarId : string; aColorRgbFormat : boolean; aRequest : TCalendarListEntry) : TCalendarListEntryServiceResult;
    Function Watch(aMaxResults : integer; aMinAccessRole : string; aPageToken : string; aShowDeleted : boolean; aShowHidden : boolean; aShowOwnOrganizationOnly : boolean; aSyncToken : string; aRequest : TChannel) : TChannelServiceResult;
  end;
  
  // Service ICalendars
  
  TCalendarsProxy = Class (TFPOpenAPIServiceClient,ICalendars)
    Function Delete(aCalendarId : string) : TCalendarsDeleteResult;
    Function Get(aCalendarId : string) : TCalendarServiceResult;
    Function Insert(aRequest : TCalendar) : TCalendarServiceResult;
    Function Patch(aCalendarId : string; aRequest : TCalendar) : TCalendarServiceResult;
    Function Update(aCalendarId : string; aRequest : TCalendar) : TCalendarServiceResult;
  end;
  
  // Service IColors
  
  TColorsProxy = Class (TFPOpenAPIServiceClient,IColors)
    Function Get() : TColorsServiceResult;
  end;
  
  // Service IEvents
  
  TEventsProxy = Class (TFPOpenAPIServiceClient,IEvents)
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
  
  TFreebusyProxy = Class (TFPOpenAPIServiceClient,IFreebusy)
    Function Query(aRequest : TFreeBusyRequest) : TFreeBusyResponseServiceResult;
  end;
  
  // Service ISettings
  
  TSettingsProxy = Class (TFPOpenAPIServiceClient,ISettings)
    Function Get(aSetting : string) : TSettingServiceResult;
    Function List(aMaxResults : integer; aPageToken : string; aSyncToken : string) : TSettingsServiceResult;
    Function Watch(aMaxResults : integer; aPageToken : string; aSyncToken : string; aRequest : TChannel) : TChannelServiceResult;
  end;
  

implementation

uses
  SysUtils, DateUtils
  , calendar.Serializer;

Function TAclProxy.Delete(aCalendarId : string; aRuleId : string) : TAclDeleteResult;

const
  lMethodURL = '/calendars/{calendarId}/acl/{ruleId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TAclDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lUrl:=ReplacePathParam(lURL,'ruleId',aRuleId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TAclDeleteResponseKind.AclDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TAclDeleteResponseKind.AclDeleterkUnexpected;
    end;
  end;
end;

Function TAclProxy.Get(aCalendarId : string; aRuleId : string) : TAclRuleServiceResult;

const
  lMethodURL = '/calendars/{calendarId}/acl/{ruleId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TAclRuleServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lUrl:=ReplacePathParam(lURL,'ruleId',aRuleId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TAclRuleServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TAclRule.Deserialize(lResponse.Content);
end;

Function TAclProxy.Insert(aCalendarId : string; aSendNotifications : boolean; aRequest : TAclRule) : TAclRuleServiceResult;

const
  lMethodURL = '/calendars/{calendarId}/acl';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TAclRuleServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lQuery:=ConcatRestParam(lQuery,'sendNotifications',cRESTBooleans[aSendNotifications]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TAclRuleServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TAclRule.Deserialize(lResponse.Content);
end;

Function TAclProxy.List(aCalendarId : string; aMaxResults : integer; aPageToken : string; aShowDeleted : boolean; aSyncToken : string) : TAclServiceResult;

const
  lMethodURL = '/calendars/{calendarId}/acl';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TAclServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lQuery:=ConcatRestParam(lQuery,'maxResults',IntToStr(aMaxResults));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'showDeleted',cRESTBooleans[aShowDeleted]);
  lQuery:=ConcatRestParam(lQuery,'syncToken',aSyncToken);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TAclServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TAcl.Deserialize(lResponse.Content);
end;

Function TAclProxy.Patch(aCalendarId : string; aRuleId : string; aSendNotifications : boolean; aRequest : TAclRule) : TAclRuleServiceResult;

const
  lMethodURL = '/calendars/{calendarId}/acl/{ruleId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TAclRuleServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lUrl:=ReplacePathParam(lURL,'ruleId',aRuleId);
  lQuery:=ConcatRestParam(lQuery,'sendNotifications',cRESTBooleans[aSendNotifications]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('patch',lURL,aRequest.Serialize);
  Result:=TAclRuleServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TAclRule.Deserialize(lResponse.Content);
end;

Function TAclProxy.Update(aCalendarId : string; aRuleId : string; aSendNotifications : boolean; aRequest : TAclRule) : TAclRuleServiceResult;

const
  lMethodURL = '/calendars/{calendarId}/acl/{ruleId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TAclRuleServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lUrl:=ReplacePathParam(lURL,'ruleId',aRuleId);
  lQuery:=ConcatRestParam(lQuery,'sendNotifications',cRESTBooleans[aSendNotifications]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('put',lURL,aRequest.Serialize);
  Result:=TAclRuleServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TAclRule.Deserialize(lResponse.Content);
end;

Function TAclProxy.Watch(aCalendarId : string; aMaxResults : integer; aPageToken : string; aShowDeleted : boolean; aSyncToken : string; aRequest : TChannel) : TChannelServiceResult;

const
  lMethodURL = '/calendars/{calendarId}/acl/watch';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TChannelServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lQuery:=ConcatRestParam(lQuery,'maxResults',IntToStr(aMaxResults));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'showDeleted',cRESTBooleans[aShowDeleted]);
  lQuery:=ConcatRestParam(lQuery,'syncToken',aSyncToken);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TChannelServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TChannel.Deserialize(lResponse.Content);
end;

Function TCalendarListProxy.Delete(aCalendarId : string) : TCalendarListDeleteResult;

const
  lMethodURL = '/users/me/calendarList/{calendarId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCalendarListDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TCalendarListDeleteResponseKind.CalendarListDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TCalendarListDeleteResponseKind.CalendarListDeleterkUnexpected;
    end;
  end;
end;

Function TCalendarListProxy.Get(aCalendarId : string) : TCalendarListEntryServiceResult;

const
  lMethodURL = '/users/me/calendarList/{calendarId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCalendarListEntryServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TCalendarListEntryServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCalendarListEntry.Deserialize(lResponse.Content);
end;

Function TCalendarListProxy.Insert(aColorRgbFormat : boolean; aRequest : TCalendarListEntry) : TCalendarListEntryServiceResult;

const
  lMethodURL = '/users/me/calendarList';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TCalendarListEntryServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'colorRgbFormat',cRESTBooleans[aColorRgbFormat]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TCalendarListEntryServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCalendarListEntry.Deserialize(lResponse.Content);
end;

Function TCalendarListProxy.List(aMaxResults : integer; aMinAccessRole : string; aPageToken : string; aShowDeleted : boolean; aShowHidden : boolean; aShowOwnOrganizationOnly : boolean; aSyncToken : string) : TCalendarListServiceResult;

const
  lMethodURL = '/users/me/calendarList';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TCalendarListServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'maxResults',IntToStr(aMaxResults));
  lQuery:=ConcatRestParam(lQuery,'minAccessRole',aMinAccessRole);
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'showDeleted',cRESTBooleans[aShowDeleted]);
  lQuery:=ConcatRestParam(lQuery,'showHidden',cRESTBooleans[aShowHidden]);
  lQuery:=ConcatRestParam(lQuery,'showOwnOrganizationOnly',cRESTBooleans[aShowOwnOrganizationOnly]);
  lQuery:=ConcatRestParam(lQuery,'syncToken',aSyncToken);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TCalendarListServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCalendarList.Deserialize(lResponse.Content);
end;

Function TCalendarListProxy.Patch(aCalendarId : string; aColorRgbFormat : boolean; aRequest : TCalendarListEntry) : TCalendarListEntryServiceResult;

const
  lMethodURL = '/users/me/calendarList/{calendarId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TCalendarListEntryServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lQuery:=ConcatRestParam(lQuery,'colorRgbFormat',cRESTBooleans[aColorRgbFormat]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('patch',lURL,aRequest.Serialize);
  Result:=TCalendarListEntryServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCalendarListEntry.Deserialize(lResponse.Content);
end;

Function TCalendarListProxy.Update(aCalendarId : string; aColorRgbFormat : boolean; aRequest : TCalendarListEntry) : TCalendarListEntryServiceResult;

const
  lMethodURL = '/users/me/calendarList/{calendarId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TCalendarListEntryServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lQuery:=ConcatRestParam(lQuery,'colorRgbFormat',cRESTBooleans[aColorRgbFormat]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('put',lURL,aRequest.Serialize);
  Result:=TCalendarListEntryServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCalendarListEntry.Deserialize(lResponse.Content);
end;

Function TCalendarListProxy.Watch(aMaxResults : integer; aMinAccessRole : string; aPageToken : string; aShowDeleted : boolean; aShowHidden : boolean; aShowOwnOrganizationOnly : boolean; aSyncToken : string; aRequest : TChannel) : TChannelServiceResult;

const
  lMethodURL = '/users/me/calendarList/watch';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TChannelServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'maxResults',IntToStr(aMaxResults));
  lQuery:=ConcatRestParam(lQuery,'minAccessRole',aMinAccessRole);
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'showDeleted',cRESTBooleans[aShowDeleted]);
  lQuery:=ConcatRestParam(lQuery,'showHidden',cRESTBooleans[aShowHidden]);
  lQuery:=ConcatRestParam(lQuery,'showOwnOrganizationOnly',cRESTBooleans[aShowOwnOrganizationOnly]);
  lQuery:=ConcatRestParam(lQuery,'syncToken',aSyncToken);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TChannelServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TChannel.Deserialize(lResponse.Content);
end;

Function TCalendarsProxy.Delete(aCalendarId : string) : TCalendarsDeleteResult;

const
  lMethodURL = '/calendars/{calendarId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCalendarsDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TCalendarsDeleteResponseKind.CalendarsDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TCalendarsDeleteResponseKind.CalendarsDeleterkUnexpected;
    end;
  end;
end;

Function TCalendarsProxy.Get(aCalendarId : string) : TCalendarServiceResult;

const
  lMethodURL = '/calendars/{calendarId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCalendarServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TCalendarServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCalendar.Deserialize(lResponse.Content);
end;

Function TCalendarsProxy.Insert(aRequest : TCalendar) : TCalendarServiceResult;

const
  lMethodURL = '/calendars';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCalendarServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TCalendarServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCalendar.Deserialize(lResponse.Content);
end;

Function TCalendarsProxy.Patch(aCalendarId : string; aRequest : TCalendar) : TCalendarServiceResult;

const
  lMethodURL = '/calendars/{calendarId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCalendarServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lResponse:=ExecuteRequest('patch',lURL,aRequest.Serialize);
  Result:=TCalendarServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCalendar.Deserialize(lResponse.Content);
end;

Function TCalendarsProxy.Update(aCalendarId : string; aRequest : TCalendar) : TCalendarServiceResult;

const
  lMethodURL = '/calendars/{calendarId}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TCalendarServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lResponse:=ExecuteRequest('put',lURL,aRequest.Serialize);
  Result:=TCalendarServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TCalendar.Deserialize(lResponse.Content);
end;

Function TColorsProxy.Get() : TColorsServiceResult;

const
  lMethodURL = '/colors';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TColorsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TColorsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TColors.Deserialize(lResponse.Content);
end;

Function TEventsProxy.Delete(aCalendarId : string; aEventId : string; aSendNotifications : boolean; aSendUpdates : string) : TEventsDeleteResult;

const
  lMethodURL = '/calendars/{calendarId}/events/{eventId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TEventsDeleteResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lUrl:=ReplacePathParam(lURL,'eventId',aEventId);
  lQuery:=ConcatRestParam(lQuery,'sendNotifications',cRESTBooleans[aSendNotifications]);
  lQuery:=ConcatRestParam(lQuery,'sendUpdates',aSendUpdates);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('delete',lURL,'');
  Result.FStatusCode:=lResponse.StatusCode;
  Result.FStatusText:=lResponse.StatusText;
  Result.FContentType:=lResponse.ContentType;
  Result.FRawContent:=lResponse.Content;
  Result.FRawStream:=lResponse.ContentStream;
  
  case lResponse.StatusCode of
    200: begin
      Result.FResponseKind:=TEventsDeleteResponseKind.EventsDeleterkSuccess;
    end;
    else begin
      Result.FResponseKind:=TEventsDeleteResponseKind.EventsDeleterkUnexpected;
    end;
  end;
end;

Function TEventsProxy.Get(aAlwaysIncludeEmail : boolean; aCalendarId : string; aEventId : string; aMaxAttendees : integer; aTimeZone : string) : TEventServiceResult;

const
  lMethodURL = '/calendars/{calendarId}/events/{eventId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TEventServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lUrl:=ReplacePathParam(lURL,'eventId',aEventId);
  lQuery:=ConcatRestParam(lQuery,'alwaysIncludeEmail',cRESTBooleans[aAlwaysIncludeEmail]);
  lQuery:=ConcatRestParam(lQuery,'maxAttendees',IntToStr(aMaxAttendees));
  lQuery:=ConcatRestParam(lQuery,'timeZone',aTimeZone);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TEventServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TEvent.Deserialize(lResponse.Content);
end;

Function TEventsProxy.Import(aCalendarId : string; aConferenceDataVersion : integer; aEventLabelVersion : integer; aSupportsAttachments : boolean; aRequest : TEvent) : TEventServiceResult;

const
  lMethodURL = '/calendars/{calendarId}/events/import';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TEventServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lQuery:=ConcatRestParam(lQuery,'conferenceDataVersion',IntToStr(aConferenceDataVersion));
  lQuery:=ConcatRestParam(lQuery,'eventLabelVersion',IntToStr(aEventLabelVersion));
  lQuery:=ConcatRestParam(lQuery,'supportsAttachments',cRESTBooleans[aSupportsAttachments]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TEventServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TEvent.Deserialize(lResponse.Content);
end;

Function TEventsProxy.Insert(aCalendarId : string; aConferenceDataVersion : integer; aEventLabelVersion : integer; aMaxAttendees : integer; aSendNotifications : boolean; aSendUpdates : string; aSupportsAttachments : boolean; aRequest : TEvent) : TEventServiceResult;

const
  lMethodURL = '/calendars/{calendarId}/events';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TEventServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lQuery:=ConcatRestParam(lQuery,'conferenceDataVersion',IntToStr(aConferenceDataVersion));
  lQuery:=ConcatRestParam(lQuery,'eventLabelVersion',IntToStr(aEventLabelVersion));
  lQuery:=ConcatRestParam(lQuery,'maxAttendees',IntToStr(aMaxAttendees));
  lQuery:=ConcatRestParam(lQuery,'sendNotifications',cRESTBooleans[aSendNotifications]);
  lQuery:=ConcatRestParam(lQuery,'sendUpdates',aSendUpdates);
  lQuery:=ConcatRestParam(lQuery,'supportsAttachments',cRESTBooleans[aSupportsAttachments]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TEventServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TEvent.Deserialize(lResponse.Content);
end;

Function TEventsProxy.Instances(aAlwaysIncludeEmail : boolean; aCalendarId : string; aEventId : string; aMaxAttendees : integer; aMaxResults : integer; aOriginalStart : string; aPageToken : string; aShowDeleted : boolean; aTimeMax : string; aTimeMin : string; aTimeZone : string) : TEventsServiceResult;

const
  lMethodURL = '/calendars/{calendarId}/events/{eventId}/instances';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TEventsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lUrl:=ReplacePathParam(lURL,'eventId',aEventId);
  lQuery:=ConcatRestParam(lQuery,'alwaysIncludeEmail',cRESTBooleans[aAlwaysIncludeEmail]);
  lQuery:=ConcatRestParam(lQuery,'maxAttendees',IntToStr(aMaxAttendees));
  lQuery:=ConcatRestParam(lQuery,'maxResults',IntToStr(aMaxResults));
  lQuery:=ConcatRestParam(lQuery,'originalStart',aOriginalStart);
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'showDeleted',cRESTBooleans[aShowDeleted]);
  lQuery:=ConcatRestParam(lQuery,'timeMax',aTimeMax);
  lQuery:=ConcatRestParam(lQuery,'timeMin',aTimeMin);
  lQuery:=ConcatRestParam(lQuery,'timeZone',aTimeZone);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TEventsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TEvents.Deserialize(lResponse.Content);
end;

Function TEventsProxy.List(aAlwaysIncludeEmail : boolean; aCalendarId : string; aEventTypes : string; aICalUID : string; aMaxAttendees : integer; aOrderBy : string; aPageToken : string; aPrivateExtendedProperty : string; aQ : string; aSharedExtendedProperty : string; aShowDeleted : boolean; aShowHiddenInvitations : boolean; aSingleEvents : boolean; aSyncToken : string; aTimeMax : string; aTimeMin : string; aTimeZone : string; aUpdatedMin : string; aMaxResults : integer = 250) : TEventsServiceResult;

const
  lMethodURL = '/calendars/{calendarId}/events';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TEventsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lQuery:=ConcatRestParam(lQuery,'alwaysIncludeEmail',cRESTBooleans[aAlwaysIncludeEmail]);
  lQuery:=ConcatRestParam(lQuery,'eventTypes',aEventTypes);
  lQuery:=ConcatRestParam(lQuery,'iCalUID',aICalUID);
  lQuery:=ConcatRestParam(lQuery,'maxAttendees',IntToStr(aMaxAttendees));
  lQuery:=ConcatRestParam(lQuery,'maxResults',IntToStr(aMaxResults));
  lQuery:=ConcatRestParam(lQuery,'orderBy',aOrderBy);
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'privateExtendedProperty',aPrivateExtendedProperty);
  lQuery:=ConcatRestParam(lQuery,'q',aQ);
  lQuery:=ConcatRestParam(lQuery,'sharedExtendedProperty',aSharedExtendedProperty);
  lQuery:=ConcatRestParam(lQuery,'showDeleted',cRESTBooleans[aShowDeleted]);
  lQuery:=ConcatRestParam(lQuery,'showHiddenInvitations',cRESTBooleans[aShowHiddenInvitations]);
  lQuery:=ConcatRestParam(lQuery,'singleEvents',cRESTBooleans[aSingleEvents]);
  lQuery:=ConcatRestParam(lQuery,'syncToken',aSyncToken);
  lQuery:=ConcatRestParam(lQuery,'timeMax',aTimeMax);
  lQuery:=ConcatRestParam(lQuery,'timeMin',aTimeMin);
  lQuery:=ConcatRestParam(lQuery,'timeZone',aTimeZone);
  lQuery:=ConcatRestParam(lQuery,'updatedMin',aUpdatedMin);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TEventsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TEvents.Deserialize(lResponse.Content);
end;

Function TEventsProxy.Move(aCalendarId : string; aDestination : string; aEventId : string; aSendNotifications : boolean; aSendUpdates : string) : TEventServiceResult;

const
  lMethodURL = '/calendars/{calendarId}/events/{eventId}/move';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TEventServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lUrl:=ReplacePathParam(lURL,'eventId',aEventId);
  lQuery:=ConcatRestParam(lQuery,'destination',aDestination);
  lQuery:=ConcatRestParam(lQuery,'sendNotifications',cRESTBooleans[aSendNotifications]);
  lQuery:=ConcatRestParam(lQuery,'sendUpdates',aSendUpdates);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,'');
  Result:=TEventServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TEvent.Deserialize(lResponse.Content);
end;

Function TEventsProxy.Patch(aAlwaysIncludeEmail : boolean; aCalendarId : string; aConferenceDataVersion : integer; aEventId : string; aEventLabelVersion : integer; aMaxAttendees : integer; aSendNotifications : boolean; aSendUpdates : string; aSupportsAttachments : boolean; aRequest : TEvent) : TEventServiceResult;

const
  lMethodURL = '/calendars/{calendarId}/events/{eventId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TEventServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lUrl:=ReplacePathParam(lURL,'eventId',aEventId);
  lQuery:=ConcatRestParam(lQuery,'alwaysIncludeEmail',cRESTBooleans[aAlwaysIncludeEmail]);
  lQuery:=ConcatRestParam(lQuery,'conferenceDataVersion',IntToStr(aConferenceDataVersion));
  lQuery:=ConcatRestParam(lQuery,'eventLabelVersion',IntToStr(aEventLabelVersion));
  lQuery:=ConcatRestParam(lQuery,'maxAttendees',IntToStr(aMaxAttendees));
  lQuery:=ConcatRestParam(lQuery,'sendNotifications',cRESTBooleans[aSendNotifications]);
  lQuery:=ConcatRestParam(lQuery,'sendUpdates',aSendUpdates);
  lQuery:=ConcatRestParam(lQuery,'supportsAttachments',cRESTBooleans[aSupportsAttachments]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('patch',lURL,aRequest.Serialize);
  Result:=TEventServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TEvent.Deserialize(lResponse.Content);
end;

Function TEventsProxy.QuickAdd(aCalendarId : string; aSendNotifications : boolean; aSendUpdates : string; aText : string) : TEventServiceResult;

const
  lMethodURL = '/calendars/{calendarId}/events/quickAdd';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TEventServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lQuery:=ConcatRestParam(lQuery,'sendNotifications',cRESTBooleans[aSendNotifications]);
  lQuery:=ConcatRestParam(lQuery,'sendUpdates',aSendUpdates);
  lQuery:=ConcatRestParam(lQuery,'text',aText);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,'');
  Result:=TEventServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TEvent.Deserialize(lResponse.Content);
end;

Function TEventsProxy.Update(aAlwaysIncludeEmail : boolean; aCalendarId : string; aConferenceDataVersion : integer; aEventId : string; aEventLabelVersion : integer; aMaxAttendees : integer; aSendNotifications : boolean; aSendUpdates : string; aSupportsAttachments : boolean; aRequest : TEvent) : TEventServiceResult;

const
  lMethodURL = '/calendars/{calendarId}/events/{eventId}';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TEventServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lUrl:=ReplacePathParam(lURL,'eventId',aEventId);
  lQuery:=ConcatRestParam(lQuery,'alwaysIncludeEmail',cRESTBooleans[aAlwaysIncludeEmail]);
  lQuery:=ConcatRestParam(lQuery,'conferenceDataVersion',IntToStr(aConferenceDataVersion));
  lQuery:=ConcatRestParam(lQuery,'eventLabelVersion',IntToStr(aEventLabelVersion));
  lQuery:=ConcatRestParam(lQuery,'maxAttendees',IntToStr(aMaxAttendees));
  lQuery:=ConcatRestParam(lQuery,'sendNotifications',cRESTBooleans[aSendNotifications]);
  lQuery:=ConcatRestParam(lQuery,'sendUpdates',aSendUpdates);
  lQuery:=ConcatRestParam(lQuery,'supportsAttachments',cRESTBooleans[aSupportsAttachments]);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('put',lURL,aRequest.Serialize);
  Result:=TEventServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TEvent.Deserialize(lResponse.Content);
end;

Function TEventsProxy.Watch(aAlwaysIncludeEmail : boolean; aCalendarId : string; aEventTypes : string; aICalUID : string; aMaxAttendees : integer; aOrderBy : string; aPageToken : string; aPrivateExtendedProperty : string; aQ : string; aSharedExtendedProperty : string; aShowDeleted : boolean; aShowHiddenInvitations : boolean; aSingleEvents : boolean; aSyncToken : string; aTimeMax : string; aTimeMin : string; aTimeZone : string; aUpdatedMin : string; aRequest : TChannel; aMaxResults : integer = 250) : TChannelServiceResult;

const
  lMethodURL = '/calendars/{calendarId}/events/watch';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TChannelServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lUrl:=ReplacePathParam(lURL,'calendarId',aCalendarId);
  lQuery:=ConcatRestParam(lQuery,'alwaysIncludeEmail',cRESTBooleans[aAlwaysIncludeEmail]);
  lQuery:=ConcatRestParam(lQuery,'eventTypes',aEventTypes);
  lQuery:=ConcatRestParam(lQuery,'iCalUID',aICalUID);
  lQuery:=ConcatRestParam(lQuery,'maxAttendees',IntToStr(aMaxAttendees));
  lQuery:=ConcatRestParam(lQuery,'maxResults',IntToStr(aMaxResults));
  lQuery:=ConcatRestParam(lQuery,'orderBy',aOrderBy);
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'privateExtendedProperty',aPrivateExtendedProperty);
  lQuery:=ConcatRestParam(lQuery,'q',aQ);
  lQuery:=ConcatRestParam(lQuery,'sharedExtendedProperty',aSharedExtendedProperty);
  lQuery:=ConcatRestParam(lQuery,'showDeleted',cRESTBooleans[aShowDeleted]);
  lQuery:=ConcatRestParam(lQuery,'showHiddenInvitations',cRESTBooleans[aShowHiddenInvitations]);
  lQuery:=ConcatRestParam(lQuery,'singleEvents',cRESTBooleans[aSingleEvents]);
  lQuery:=ConcatRestParam(lQuery,'syncToken',aSyncToken);
  lQuery:=ConcatRestParam(lQuery,'timeMax',aTimeMax);
  lQuery:=ConcatRestParam(lQuery,'timeMin',aTimeMin);
  lQuery:=ConcatRestParam(lQuery,'timeZone',aTimeZone);
  lQuery:=ConcatRestParam(lQuery,'updatedMin',aUpdatedMin);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TChannelServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TChannel.Deserialize(lResponse.Content);
end;

Function TFreebusyProxy.Query(aRequest : TFreeBusyRequest) : TFreeBusyResponseServiceResult;

const
  lMethodURL = '/freeBusy';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TFreeBusyResponseServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TFreeBusyResponseServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TFreeBusyResponse.Deserialize(lResponse.Content);
end;

Function TSettingsProxy.Get(aSetting : string) : TSettingServiceResult;

const
  lMethodURL = '/users/me/settings/{setting}';

var
  lURL : String;
  lResponse : TServiceResponse;

begin
  Result:=Default(TSettingServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lUrl:=ReplacePathParam(lURL,'setting',aSetting);
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TSettingServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TSetting.Deserialize(lResponse.Content);
end;

Function TSettingsProxy.List(aMaxResults : integer; aPageToken : string; aSyncToken : string) : TSettingsServiceResult;

const
  lMethodURL = '/users/me/settings';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TSettingsServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'maxResults',IntToStr(aMaxResults));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'syncToken',aSyncToken);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('get',lURL,'');
  Result:=TSettingsServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TSettings.Deserialize(lResponse.Content);
end;

Function TSettingsProxy.Watch(aMaxResults : integer; aPageToken : string; aSyncToken : string; aRequest : TChannel) : TChannelServiceResult;

const
  lMethodURL = '/users/me/settings/watch';

var
  lURL : String;
  lResponse : TServiceResponse;
  lQuery : String;

begin
  Result:=Default(TChannelServiceResult);
  lURL:=BuildEndPointURL(lMethodURL);
  lQuery:='';
  lQuery:=ConcatRestParam(lQuery,'maxResults',IntToStr(aMaxResults));
  lQuery:=ConcatRestParam(lQuery,'pageToken',aPageToken);
  lQuery:=ConcatRestParam(lQuery,'syncToken',aSyncToken);
  lURL:=lURL+lQuery;
  lResponse:=ExecuteRequest('post',lURL,aRequest.Serialize);
  Result:=TChannelServiceResult.Create(lResponse);
  if Result.Success then
    Result.Value:=TChannel.Deserialize(lResponse.Content);
end;


end.
