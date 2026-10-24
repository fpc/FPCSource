{ -----------------------------------------------------------------------
  Do not edit !
  
  This file was automatically generated on 2026-10-03 18:11.
  Used command-line parameters:
     -s calendar -L -o calendar -q
  Source OpenAPI document data:
    Title: Calendar API
    Version: v3
  -----------------------------------------------------------------------}
unit calendar.Serializer;

interface

{$mode objfpc}
{$h+}
{$modeswitch typehelpers}


uses
  Types,
  fpJSON,
  calendar.Dto;

Type
  TAclRuleSerializer = class helper for TAclRule
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TAclRule; overload; static;
    class function Deserialize(aJSON : String) : TAclRule; overload; static;
  end;
  
  TAclSerializer = class helper for TAcl
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TAcl; overload; static;
    class function Deserialize(aJSON : String) : TAcl; overload; static;
  end;
  
  TConferencePropertiesSerializer = class helper for TConferenceProperties
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TConferenceProperties; overload; static;
    class function Deserialize(aJSON : String) : TConferenceProperties; overload; static;
  end;
  
  TEventLabelSerializer = class helper for TEventLabel
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TEventLabel; overload; static;
    class function Deserialize(aJSON : String) : TEventLabel; overload; static;
  end;
  
  TLabelPropertiesSerializer = class helper for TLabelProperties
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TLabelProperties; overload; static;
    class function Deserialize(aJSON : String) : TLabelProperties; overload; static;
  end;
  
  TCalendarSerializer = class helper for TCalendar
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TCalendar; overload; static;
    class function Deserialize(aJSON : String) : TCalendar; overload; static;
  end;
  
  TEventReminderSerializer = class helper for TEventReminder
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TEventReminder; overload; static;
    class function Deserialize(aJSON : String) : TEventReminder; overload; static;
  end;
  
  TCalendarListEntrySerializer = class helper for TCalendarListEntry
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TCalendarListEntry; overload; static;
    class function Deserialize(aJSON : String) : TCalendarListEntry; overload; static;
  end;
  
  TCalendarListSerializer = class helper for TCalendarList
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TCalendarList; overload; static;
    class function Deserialize(aJSON : String) : TCalendarList; overload; static;
  end;
  
  TCalendarNotificationSerializer = class helper for TCalendarNotification
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TCalendarNotification; overload; static;
    class function Deserialize(aJSON : String) : TCalendarNotification; overload; static;
  end;
  
  TChannelSerializer = class helper for TChannel
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TChannel; overload; static;
    class function Deserialize(aJSON : String) : TChannel; overload; static;
  end;
  
  TColorDefinitionSerializer = class helper for TColorDefinition
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TColorDefinition; overload; static;
    class function Deserialize(aJSON : String) : TColorDefinition; overload; static;
  end;
  
  TColorsSerializer = class helper for TColors
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TColors; overload; static;
    class function Deserialize(aJSON : String) : TColors; overload; static;
  end;
  
  TConferenceSolutionKeySerializer = class helper for TConferenceSolutionKey
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TConferenceSolutionKey; overload; static;
    class function Deserialize(aJSON : String) : TConferenceSolutionKey; overload; static;
  end;
  
  TConferenceSolutionSerializer = class helper for TConferenceSolution
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TConferenceSolution; overload; static;
    class function Deserialize(aJSON : String) : TConferenceSolution; overload; static;
  end;
  
  TConferenceRequestStatusSerializer = class helper for TConferenceRequestStatus
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TConferenceRequestStatus; overload; static;
    class function Deserialize(aJSON : String) : TConferenceRequestStatus; overload; static;
  end;
  
  TCreateConferenceRequestSerializer = class helper for TCreateConferenceRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TCreateConferenceRequest; overload; static;
    class function Deserialize(aJSON : String) : TCreateConferenceRequest; overload; static;
  end;
  
  TEntryPointSerializer = class helper for TEntryPoint
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TEntryPoint; overload; static;
    class function Deserialize(aJSON : String) : TEntryPoint; overload; static;
  end;
  
  TConferenceParametersAddOnParametersSerializer = class helper for TConferenceParametersAddOnParameters
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TConferenceParametersAddOnParameters; overload; static;
    class function Deserialize(aJSON : String) : TConferenceParametersAddOnParameters; overload; static;
  end;
  
  TConferenceParametersSerializer = class helper for TConferenceParameters
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TConferenceParameters; overload; static;
    class function Deserialize(aJSON : String) : TConferenceParameters; overload; static;
  end;
  
  TConferenceDataSerializer = class helper for TConferenceData
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TConferenceData; overload; static;
    class function Deserialize(aJSON : String) : TConferenceData; overload; static;
  end;
  
  TErrorSerializer = class helper for TError
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TError; overload; static;
    class function Deserialize(aJSON : String) : TError; overload; static;
  end;
  
  TEventAttachmentSerializer = class helper for TEventAttachment
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TEventAttachment; overload; static;
    class function Deserialize(aJSON : String) : TEventAttachment; overload; static;
  end;
  
  TEventAttendeeSerializer = class helper for TEventAttendee
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TEventAttendee; overload; static;
    class function Deserialize(aJSON : String) : TEventAttendee; overload; static;
  end;
  
  TEventBirthdayPropertiesSerializer = class helper for TEventBirthdayProperties
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TEventBirthdayProperties; overload; static;
    class function Deserialize(aJSON : String) : TEventBirthdayProperties; overload; static;
  end;
  
  TEventDateTimeSerializer = class helper for TEventDateTime
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TEventDateTime; overload; static;
    class function Deserialize(aJSON : String) : TEventDateTime; overload; static;
  end;
  
  TEventFocusTimePropertiesSerializer = class helper for TEventFocusTimeProperties
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TEventFocusTimeProperties; overload; static;
    class function Deserialize(aJSON : String) : TEventFocusTimeProperties; overload; static;
  end;
  
  TEventOutOfOfficePropertiesSerializer = class helper for TEventOutOfOfficeProperties
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TEventOutOfOfficeProperties; overload; static;
    class function Deserialize(aJSON : String) : TEventOutOfOfficeProperties; overload; static;
  end;
  
  TEventWorkingLocationPropertiesSerializer = class helper for TEventWorkingLocationProperties
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TEventWorkingLocationProperties; overload; static;
    class function Deserialize(aJSON : String) : TEventWorkingLocationProperties; overload; static;
  end;
  
  TEventSerializer = class helper for TEvent
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TEvent; overload; static;
    class function Deserialize(aJSON : String) : TEvent; overload; static;
  end;
  
  TEventsSerializer = class helper for TEvents
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TEvents; overload; static;
    class function Deserialize(aJSON : String) : TEvents; overload; static;
  end;
  
  TTimePeriodSerializer = class helper for TTimePeriod
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TTimePeriod; overload; static;
    class function Deserialize(aJSON : String) : TTimePeriod; overload; static;
  end;
  
  TFreeBusyCalendarSerializer = class helper for TFreeBusyCalendar
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TFreeBusyCalendar; overload; static;
    class function Deserialize(aJSON : String) : TFreeBusyCalendar; overload; static;
  end;
  
  TFreeBusyGroupSerializer = class helper for TFreeBusyGroup
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TFreeBusyGroup; overload; static;
    class function Deserialize(aJSON : String) : TFreeBusyGroup; overload; static;
  end;
  
  TFreeBusyRequestItemSerializer = class helper for TFreeBusyRequestItem
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TFreeBusyRequestItem; overload; static;
    class function Deserialize(aJSON : String) : TFreeBusyRequestItem; overload; static;
  end;
  
  TFreeBusyRequestSerializer = class helper for TFreeBusyRequest
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TFreeBusyRequest; overload; static;
    class function Deserialize(aJSON : String) : TFreeBusyRequest; overload; static;
  end;
  
  TFreeBusyResponseSerializer = class helper for TFreeBusyResponse
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TFreeBusyResponse; overload; static;
    class function Deserialize(aJSON : String) : TFreeBusyResponse; overload; static;
  end;
  
  TSettingSerializer = class helper for TSetting
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TSetting; overload; static;
    class function Deserialize(aJSON : String) : TSetting; overload; static;
  end;
  
  TSettingsSerializer = class helper for TSettings
    function SerializeObject : TJSONObject;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONObject) : TSettings; overload; static;
    class function Deserialize(aJSON : String) : TSettings; overload; static;
  end;
  
  TAclRuleArraySerializer = type helper for TAclRuleArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TAclRuleArray; overload; static;
    class function Deserialize(aJSON : String) : TAclRuleArray; overload; static;
  end;
  TCalendarListEntryArraySerializer = type helper for TCalendarListEntryArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TCalendarListEntryArray; overload; static;
    class function Deserialize(aJSON : String) : TCalendarListEntryArray; overload; static;
  end;
  TEntryPointArraySerializer = type helper for TEntryPointArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TEntryPointArray; overload; static;
    class function Deserialize(aJSON : String) : TEntryPointArray; overload; static;
  end;
  TErrorArraySerializer = type helper for TErrorArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TErrorArray; overload; static;
    class function Deserialize(aJSON : String) : TErrorArray; overload; static;
  end;
  TEventAttachmentArraySerializer = type helper for TEventAttachmentArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TEventAttachmentArray; overload; static;
    class function Deserialize(aJSON : String) : TEventAttachmentArray; overload; static;
  end;
  TEventAttendeeArraySerializer = type helper for TEventAttendeeArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TEventAttendeeArray; overload; static;
    class function Deserialize(aJSON : String) : TEventAttendeeArray; overload; static;
  end;
  TEventLabelArraySerializer = type helper for TEventLabelArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TEventLabelArray; overload; static;
    class function Deserialize(aJSON : String) : TEventLabelArray; overload; static;
  end;
  TEventReminderArraySerializer = type helper for TEventReminderArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TEventReminderArray; overload; static;
    class function Deserialize(aJSON : String) : TEventReminderArray; overload; static;
  end;
  TEventArraySerializer = type helper for TEventArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TEventArray; overload; static;
    class function Deserialize(aJSON : String) : TEventArray; overload; static;
  end;
  TFreeBusyRequestItemArraySerializer = type helper for TFreeBusyRequestItemArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TFreeBusyRequestItemArray; overload; static;
    class function Deserialize(aJSON : String) : TFreeBusyRequestItemArray; overload; static;
  end;
  TSettingArraySerializer = type helper for TSettingArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TSettingArray; overload; static;
    class function Deserialize(aJSON : String) : TSettingArray; overload; static;
  end;
  TTimePeriodArraySerializer = type helper for TTimePeriodArray
    function SerializeArray : TJSONArray;
    function Serialize : String;
    class function Deserialize(aJSON : TJSONArray) : TTimePeriodArray; overload; static;
    class function Deserialize(aJSON : String) : TTimePeriodArray; overload; static;
  end;
implementation

uses Generics.Collections, SysUtils, DateUtils, StrUtils;

function ISO8601ToDateDef(S: String; aDefault : TDateTime; aConvertUTC: Boolean = True) : TDateTime;

begin
  if (S='') then
    Exit(aDefault);
  try
    Result:=ISO8601ToDate(S,aConvertUTC);
  except
    Result:=aDefault;
  end;
end;

function ISO8601ToDateOnlyDef(S: String; aDefault : TDateTime) : TDateTime;

begin
  Result:=DateOf(ISO8601ToDateDef(S,aDefault,True));
end;

function DateOnlyToISO8601(aDate : TDateTime) : String;

begin
  Result:=FormatDateTime('yyyy"-"mm"-"dd',aDate);
end;

function JSONDataAsString(aData: TJSONData) : String;

begin
  if aData=Nil then
    Result:=''
  else
    Result:=aData.AsJSON;
end;

function TAclRuleSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('etag',etag);
    Result.Add('id',id);
    Result.Add('kind',kind);
    Result.Add('role',role);
    if (scope<>'') then
      Result.Add('scope',GetJSON(scope));
  except
    Result.Free;
    raise;
  end;
end;

function TAclRuleSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TAclRuleSerializer.Deserialize(aJSON : TJSONObject) : TAclRule;

begin
  Result := TAclRule.Create;
  If (aJSON=Nil) then
    exit;
  Result.etag:=aJSON.Get('etag','');
  Result.id:=aJSON.Get('id','');
  Result.kind:=aJSON.Get('kind','');
  Result.role:=aJSON.Get('role','');
  Result.scope:=JSONDataAsString(aJSON.Get('scope',TJSONObject(Nil)));
end;

class function TAclRuleSerializer.Deserialize(aJSON : String) : TAclRule;

var
  lObj : TJSONObject;
begin
  Result := Default(TAclRule);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TAclSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('etag',etag);
    Arr:=TJSONArray.Create;
    Result.Add('items',Arr);
    For I:=0 to Length(items)-1 do
      Arr.Add(items[i].SerializeObject);
    Result.Add('kind',kind);
    Result.Add('nextPageToken',nextPageToken);
    Result.Add('nextSyncToken',nextSyncToken);
  except
    Result.Free;
    raise;
  end;
end;

function TAclSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TAclSerializer.Deserialize(aJSON : TJSONObject) : TAcl;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TAcl.Create;
  If (aJSON=Nil) then
    exit;
  try
    Result.etag:=aJSON.Get('etag','');
    lArr:=aJSON.Get('items',TJSONArray(Nil));
    if Assigned(lArr) then
      begin
      SetLength(Result.items,lArr.Count);
      For I:=0 to Length(Result.items)-1 do
        Result.items[i]:=TAclRule.Deserialize(lArr[i] as TJSONObject);
      end;
    Result.kind:=aJSON.Get('kind','');
    Result.nextPageToken:=aJSON.Get('nextPageToken','');
    Result.nextSyncToken:=aJSON.Get('nextSyncToken','');
  except
    Result.Free;
    raise;
  end;
end;

class function TAclSerializer.Deserialize(aJSON : String) : TAcl;

var
  lObj : TJSONObject;
begin
  Result := Default(TAcl);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TConferencePropertiesSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('allowedConferenceSolutionTypes',Arr);
    For I:=0 to Length(allowedConferenceSolutionTypes)-1 do
      Arr.Add(allowedConferenceSolutionTypes[i]);
  except
    Result.Free;
    raise;
  end;
end;

function TConferencePropertiesSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TConferencePropertiesSerializer.Deserialize(aJSON : TJSONObject) : TConferenceProperties;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TConferenceProperties.Create;
  If (aJSON=Nil) then
    exit;
  lArr:=aJSON.Get('allowedConferenceSolutionTypes',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.allowedConferenceSolutionTypes,lArr.Count);
    For I:=0 to Length(Result.allowedConferenceSolutionTypes)-1 do
      Result.allowedConferenceSolutionTypes[i]:=lArr[i].Asstring;
    end;
end;

class function TConferencePropertiesSerializer.Deserialize(aJSON : String) : TConferenceProperties;

var
  lObj : TJSONObject;
begin
  Result := Default(TConferenceProperties);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TEventLabelSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('backgroundColor',backgroundColor);
    Result.Add('id',id);
    Result.Add('name',name);
  except
    Result.Free;
    raise;
  end;
end;

function TEventLabelSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEventLabelSerializer.Deserialize(aJSON : TJSONObject) : TEventLabel;

begin
  Result := TEventLabel.Create;
  If (aJSON=Nil) then
    exit;
  Result.backgroundColor:=aJSON.Get('backgroundColor','');
  Result.id:=aJSON.Get('id','');
  Result.name:=aJSON.Get('name','');
end;

class function TEventLabelSerializer.Deserialize(aJSON : String) : TEventLabel;

var
  lObj : TJSONObject;
begin
  Result := Default(TEventLabel);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TLabelPropertiesSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('eventLabels',Arr);
    For I:=0 to Length(eventLabels)-1 do
      Arr.Add(eventLabels[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TLabelPropertiesSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TLabelPropertiesSerializer.Deserialize(aJSON : TJSONObject) : TLabelProperties;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TLabelProperties.Create;
  If (aJSON=Nil) then
    exit;
  try
    lArr:=aJSON.Get('eventLabels',TJSONArray(Nil));
    if Assigned(lArr) then
      begin
      SetLength(Result.eventLabels,lArr.Count);
      For I:=0 to Length(Result.eventLabels)-1 do
        Result.eventLabels[i]:=TEventLabel.Deserialize(lArr[i] as TJSONObject);
      end;
  except
    Result.Free;
    raise;
  end;
end;

class function TLabelPropertiesSerializer.Deserialize(aJSON : String) : TLabelProperties;

var
  lObj : TJSONObject;
begin
  Result := Default(TLabelProperties);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TCalendarSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('autoAcceptInvitations',autoAcceptInvitations);
    if Assigned(conferenceProperties) then
      Result.Add('conferenceProperties',conferenceProperties.SerializeObject);
    Result.Add('dataOwner',dataOwner);
    Result.Add('description',description);
    Result.Add('etag',etag);
    Result.Add('id',id);
    Result.Add('kind',kind);
    if Assigned(labelProperties) then
      Result.Add('labelProperties',labelProperties.SerializeObject);
    Result.Add('location',location);
    Result.Add('summary',summary);
    Result.Add('timeZone',timeZone);
  except
    Result.Free;
    raise;
  end;
end;

function TCalendarSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TCalendarSerializer.Deserialize(aJSON : TJSONObject) : TCalendar;

begin
  Result := TCalendar.Create;
  If (aJSON=Nil) then
    exit;
  try
    Result.autoAcceptInvitations:=aJSON.Get('autoAcceptInvitations',False);
    Result.conferenceProperties:=TConferenceProperties.Deserialize(aJSON.Get('conferenceProperties',TJSONObject(Nil)));
    Result.dataOwner:=aJSON.Get('dataOwner','');
    Result.description:=aJSON.Get('description','');
    Result.etag:=aJSON.Get('etag','');
    Result.id:=aJSON.Get('id','');
    Result.kind:=aJSON.Get('kind','');
    Result.labelProperties:=TLabelProperties.Deserialize(aJSON.Get('labelProperties',TJSONObject(Nil)));
    Result.location:=aJSON.Get('location','');
    Result.summary:=aJSON.Get('summary','');
    Result.timeZone:=aJSON.Get('timeZone','');
  except
    Result.Free;
    raise;
  end;
end;

class function TCalendarSerializer.Deserialize(aJSON : String) : TCalendar;

var
  lObj : TJSONObject;
begin
  Result := Default(TCalendar);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TEventReminderSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('method',method);
    Result.Add('minutes',minutes);
  except
    Result.Free;
    raise;
  end;
end;

function TEventReminderSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEventReminderSerializer.Deserialize(aJSON : TJSONObject) : TEventReminder;

begin
  Result := TEventReminder.Create;
  If (aJSON=Nil) then
    exit;
  Result.method:=aJSON.Get('method','');
  Result.minutes:=aJSON.Get('minutes',0);
end;

class function TEventReminderSerializer.Deserialize(aJSON : String) : TEventReminder;

var
  lObj : TJSONObject;
begin
  Result := Default(TEventReminder);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TCalendarListEntrySerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('accessRole',accessRole);
    Result.Add('autoAcceptInvitations',autoAcceptInvitations);
    Result.Add('backgroundColor',backgroundColor);
    Result.Add('colorId',colorId);
    if Assigned(conferenceProperties) then
      Result.Add('conferenceProperties',conferenceProperties.SerializeObject);
    Result.Add('dataOwner',dataOwner);
    Arr:=TJSONArray.Create;
    Result.Add('defaultReminders',Arr);
    For I:=0 to Length(defaultReminders)-1 do
      Arr.Add(defaultReminders[i].SerializeObject);
    Result.Add('deleted',deleted);
    Result.Add('description',description);
    Result.Add('etag',etag);
    Result.Add('foregroundColor',foregroundColor);
    Result.Add('hidden',hidden);
    Result.Add('id',id);
    Result.Add('kind',kind);
    Result.Add('location',location);
    if (notificationSettings<>'') then
      Result.Add('notificationSettings',GetJSON(notificationSettings));
    Result.Add('primary',primary);
    Result.Add('selected',selected);
    Result.Add('summary',summary);
    Result.Add('summaryOverride',summaryOverride);
    Result.Add('timeZone',timeZone);
  except
    Result.Free;
    raise;
  end;
end;

function TCalendarListEntrySerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TCalendarListEntrySerializer.Deserialize(aJSON : TJSONObject) : TCalendarListEntry;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TCalendarListEntry.Create;
  If (aJSON=Nil) then
    exit;
  try
    Result.accessRole:=aJSON.Get('accessRole','');
    Result.autoAcceptInvitations:=aJSON.Get('autoAcceptInvitations',False);
    Result.backgroundColor:=aJSON.Get('backgroundColor','');
    Result.colorId:=aJSON.Get('colorId','');
    Result.conferenceProperties:=TConferenceProperties.Deserialize(aJSON.Get('conferenceProperties',TJSONObject(Nil)));
    Result.dataOwner:=aJSON.Get('dataOwner','');
    lArr:=aJSON.Get('defaultReminders',TJSONArray(Nil));
    if Assigned(lArr) then
      begin
      SetLength(Result.defaultReminders,lArr.Count);
      For I:=0 to Length(Result.defaultReminders)-1 do
        Result.defaultReminders[i]:=TEventReminder.Deserialize(lArr[i] as TJSONObject);
      end;
    Result.deleted:=aJSON.Get('deleted',False);
    Result.description:=aJSON.Get('description','');
    Result.etag:=aJSON.Get('etag','');
    Result.foregroundColor:=aJSON.Get('foregroundColor','');
    Result.hidden:=aJSON.Get('hidden',False);
    Result.id:=aJSON.Get('id','');
    Result.kind:=aJSON.Get('kind','');
    Result.location:=aJSON.Get('location','');
    Result.notificationSettings:=JSONDataAsString(aJSON.Get('notificationSettings',TJSONObject(Nil)));
    Result.primary:=aJSON.Get('primary',False);
    Result.selected:=aJSON.Get('selected',False);
    Result.summary:=aJSON.Get('summary','');
    Result.summaryOverride:=aJSON.Get('summaryOverride','');
    Result.timeZone:=aJSON.Get('timeZone','');
  except
    Result.Free;
    raise;
  end;
end;

class function TCalendarListEntrySerializer.Deserialize(aJSON : String) : TCalendarListEntry;

var
  lObj : TJSONObject;
begin
  Result := Default(TCalendarListEntry);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TCalendarListSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('etag',etag);
    Arr:=TJSONArray.Create;
    Result.Add('items',Arr);
    For I:=0 to Length(items)-1 do
      Arr.Add(items[i].SerializeObject);
    Result.Add('kind',kind);
    Result.Add('nextPageToken',nextPageToken);
    Result.Add('nextSyncToken',nextSyncToken);
  except
    Result.Free;
    raise;
  end;
end;

function TCalendarListSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TCalendarListSerializer.Deserialize(aJSON : TJSONObject) : TCalendarList;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TCalendarList.Create;
  If (aJSON=Nil) then
    exit;
  try
    Result.etag:=aJSON.Get('etag','');
    lArr:=aJSON.Get('items',TJSONArray(Nil));
    if Assigned(lArr) then
      begin
      SetLength(Result.items,lArr.Count);
      For I:=0 to Length(Result.items)-1 do
        Result.items[i]:=TCalendarListEntry.Deserialize(lArr[i] as TJSONObject);
      end;
    Result.kind:=aJSON.Get('kind','');
    Result.nextPageToken:=aJSON.Get('nextPageToken','');
    Result.nextSyncToken:=aJSON.Get('nextSyncToken','');
  except
    Result.Free;
    raise;
  end;
end;

class function TCalendarListSerializer.Deserialize(aJSON : String) : TCalendarList;

var
  lObj : TJSONObject;
begin
  Result := Default(TCalendarList);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TCalendarNotificationSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('method',method);
    Result.Add('type',type_);
  except
    Result.Free;
    raise;
  end;
end;

function TCalendarNotificationSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TCalendarNotificationSerializer.Deserialize(aJSON : TJSONObject) : TCalendarNotification;

begin
  Result := TCalendarNotification.Create;
  If (aJSON=Nil) then
    exit;
  Result.method:=aJSON.Get('method','');
  Result.type_:=aJSON.Get('type','');
end;

class function TCalendarNotificationSerializer.Deserialize(aJSON : String) : TCalendarNotification;

var
  lObj : TJSONObject;
begin
  Result := Default(TCalendarNotification);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TChannelSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('address',address);
    Result.Add('expiration',expiration);
    Result.Add('id',id);
    Result.Add('kind',kind);
    if (params<>'') then
      Result.Add('params',GetJSON(params));
    Result.Add('payload',payload);
    Result.Add('resourceId',resourceId);
    Result.Add('resourceUri',resourceUri);
    Result.Add('token',token);
    Result.Add('type',type_);
  except
    Result.Free;
    raise;
  end;
end;

function TChannelSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TChannelSerializer.Deserialize(aJSON : TJSONObject) : TChannel;

begin
  Result := TChannel.Create;
  If (aJSON=Nil) then
    exit;
  Result.address:=aJSON.Get('address','');
  Result.expiration:=aJSON.Get('expiration','');
  Result.id:=aJSON.Get('id','');
  Result.kind:=aJSON.Get('kind','');
  Result.params:=JSONDataAsString(aJSON.Get('params',TJSONObject(Nil)));
  Result.payload:=aJSON.Get('payload',False);
  Result.resourceId:=aJSON.Get('resourceId','');
  Result.resourceUri:=aJSON.Get('resourceUri','');
  Result.token:=aJSON.Get('token','');
  Result.type_:=aJSON.Get('type','');
end;

class function TChannelSerializer.Deserialize(aJSON : String) : TChannel;

var
  lObj : TJSONObject;
begin
  Result := Default(TChannel);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TColorDefinitionSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('background',background);
    Result.Add('foreground',foreground);
  except
    Result.Free;
    raise;
  end;
end;

function TColorDefinitionSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TColorDefinitionSerializer.Deserialize(aJSON : TJSONObject) : TColorDefinition;

begin
  Result := TColorDefinition.Create;
  If (aJSON=Nil) then
    exit;
  Result.background:=aJSON.Get('background','');
  Result.foreground:=aJSON.Get('foreground','');
end;

class function TColorDefinitionSerializer.Deserialize(aJSON : String) : TColorDefinition;

var
  lObj : TJSONObject;
begin
  Result := Default(TColorDefinition);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TColorsSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if (calendar<>'') then
      Result.Add('calendar',GetJSON(calendar));
    if (event<>'') then
      Result.Add('event',GetJSON(event));
    Result.Add('kind',kind);
    if (updated<>0) then
      Result.Add('updated',DateToISO8601(updated,False));
  except
    Result.Free;
    raise;
  end;
end;

function TColorsSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TColorsSerializer.Deserialize(aJSON : TJSONObject) : TColors;

begin
  Result := TColors.Create;
  If (aJSON=Nil) then
    exit;
  Result.calendar:=JSONDataAsString(aJSON.Get('calendar',TJSONObject(Nil)));
  Result.event:=JSONDataAsString(aJSON.Get('event',TJSONObject(Nil)));
  Result.kind:=aJSON.Get('kind','');
  Result.updated:=ISO8601ToDateDef(aJSON.Get('updated',''),0,False);
end;

class function TColorsSerializer.Deserialize(aJSON : String) : TColors;

var
  lObj : TJSONObject;
begin
  Result := Default(TColors);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TConferenceSolutionKeySerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('type',type_);
  except
    Result.Free;
    raise;
  end;
end;

function TConferenceSolutionKeySerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TConferenceSolutionKeySerializer.Deserialize(aJSON : TJSONObject) : TConferenceSolutionKey;

begin
  Result := TConferenceSolutionKey.Create;
  If (aJSON=Nil) then
    exit;
  Result.type_:=aJSON.Get('type','');
end;

class function TConferenceSolutionKeySerializer.Deserialize(aJSON : String) : TConferenceSolutionKey;

var
  lObj : TJSONObject;
begin
  Result := Default(TConferenceSolutionKey);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TConferenceSolutionSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('iconUri',iconUri);
    if Assigned(key) then
      Result.Add('key',key.SerializeObject);
    Result.Add('name',name);
  except
    Result.Free;
    raise;
  end;
end;

function TConferenceSolutionSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TConferenceSolutionSerializer.Deserialize(aJSON : TJSONObject) : TConferenceSolution;

begin
  Result := TConferenceSolution.Create;
  If (aJSON=Nil) then
    exit;
  try
    Result.iconUri:=aJSON.Get('iconUri','');
    Result.key:=TConferenceSolutionKey.Deserialize(aJSON.Get('key',TJSONObject(Nil)));
    Result.name:=aJSON.Get('name','');
  except
    Result.Free;
    raise;
  end;
end;

class function TConferenceSolutionSerializer.Deserialize(aJSON : String) : TConferenceSolution;

var
  lObj : TJSONObject;
begin
  Result := Default(TConferenceSolution);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TConferenceRequestStatusSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('statusCode',statusCode);
  except
    Result.Free;
    raise;
  end;
end;

function TConferenceRequestStatusSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TConferenceRequestStatusSerializer.Deserialize(aJSON : TJSONObject) : TConferenceRequestStatus;

begin
  Result := TConferenceRequestStatus.Create;
  If (aJSON=Nil) then
    exit;
  Result.statusCode:=aJSON.Get('statusCode','');
end;

class function TConferenceRequestStatusSerializer.Deserialize(aJSON : String) : TConferenceRequestStatus;

var
  lObj : TJSONObject;
begin
  Result := Default(TConferenceRequestStatus);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TCreateConferenceRequestSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if Assigned(conferenceSolutionKey) then
      Result.Add('conferenceSolutionKey',conferenceSolutionKey.SerializeObject);
    Result.Add('requestId',requestId);
    if Assigned(status) then
      Result.Add('status',status.SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TCreateConferenceRequestSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TCreateConferenceRequestSerializer.Deserialize(aJSON : TJSONObject) : TCreateConferenceRequest;

begin
  Result := TCreateConferenceRequest.Create;
  If (aJSON=Nil) then
    exit;
  try
    Result.conferenceSolutionKey:=TConferenceSolutionKey.Deserialize(aJSON.Get('conferenceSolutionKey',TJSONObject(Nil)));
    Result.requestId:=aJSON.Get('requestId','');
    Result.status:=TConferenceRequestStatus.Deserialize(aJSON.Get('status',TJSONObject(Nil)));
  except
    Result.Free;
    raise;
  end;
end;

class function TCreateConferenceRequestSerializer.Deserialize(aJSON : String) : TCreateConferenceRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TCreateConferenceRequest);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TEntryPointSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('accessCode',accessCode);
    Arr:=TJSONArray.Create;
    Result.Add('entryPointFeatures',Arr);
    For I:=0 to Length(entryPointFeatures)-1 do
      Arr.Add(entryPointFeatures[i]);
    Result.Add('entryPointType',entryPointType);
    Result.Add('label',label_);
    Result.Add('meetingCode',meetingCode);
    Result.Add('passcode',passcode);
    Result.Add('password',password);
    Result.Add('pin',pin);
    Result.Add('regionCode',regionCode);
    Result.Add('uri',uri);
  except
    Result.Free;
    raise;
  end;
end;

function TEntryPointSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEntryPointSerializer.Deserialize(aJSON : TJSONObject) : TEntryPoint;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TEntryPoint.Create;
  If (aJSON=Nil) then
    exit;
  Result.accessCode:=aJSON.Get('accessCode','');
  lArr:=aJSON.Get('entryPointFeatures',TJSONArray(Nil));
  if Assigned(lArr) then
    begin
    SetLength(Result.entryPointFeatures,lArr.Count);
    For I:=0 to Length(Result.entryPointFeatures)-1 do
      Result.entryPointFeatures[i]:=lArr[i].Asstring;
    end;
  Result.entryPointType:=aJSON.Get('entryPointType','');
  Result.label_:=aJSON.Get('label','');
  Result.meetingCode:=aJSON.Get('meetingCode','');
  Result.passcode:=aJSON.Get('passcode','');
  Result.password:=aJSON.Get('password','');
  Result.pin:=aJSON.Get('pin','');
  Result.regionCode:=aJSON.Get('regionCode','');
  Result.uri:=aJSON.Get('uri','');
end;

class function TEntryPointSerializer.Deserialize(aJSON : String) : TEntryPoint;

var
  lObj : TJSONObject;
begin
  Result := Default(TEntryPoint);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TConferenceParametersAddOnParametersSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if (parameters<>'') then
      Result.Add('parameters',GetJSON(parameters));
  except
    Result.Free;
    raise;
  end;
end;

function TConferenceParametersAddOnParametersSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TConferenceParametersAddOnParametersSerializer.Deserialize(aJSON : TJSONObject) : TConferenceParametersAddOnParameters;

begin
  Result := TConferenceParametersAddOnParameters.Create;
  If (aJSON=Nil) then
    exit;
  Result.parameters:=JSONDataAsString(aJSON.Get('parameters',TJSONObject(Nil)));
end;

class function TConferenceParametersAddOnParametersSerializer.Deserialize(aJSON : String) : TConferenceParametersAddOnParameters;

var
  lObj : TJSONObject;
begin
  Result := Default(TConferenceParametersAddOnParameters);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TConferenceParametersSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if Assigned(addOnParameters) then
      Result.Add('addOnParameters',addOnParameters.SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TConferenceParametersSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TConferenceParametersSerializer.Deserialize(aJSON : TJSONObject) : TConferenceParameters;

begin
  Result := TConferenceParameters.Create;
  If (aJSON=Nil) then
    exit;
  try
    Result.addOnParameters:=TConferenceParametersAddOnParameters.Deserialize(aJSON.Get('addOnParameters',TJSONObject(Nil)));
  except
    Result.Free;
    raise;
  end;
end;

class function TConferenceParametersSerializer.Deserialize(aJSON : String) : TConferenceParameters;

var
  lObj : TJSONObject;
begin
  Result := Default(TConferenceParameters);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TConferenceDataSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('conferenceId',conferenceId);
    if Assigned(conferenceSolution) then
      Result.Add('conferenceSolution',conferenceSolution.SerializeObject);
    if Assigned(createRequest) then
      Result.Add('createRequest',createRequest.SerializeObject);
    Arr:=TJSONArray.Create;
    Result.Add('entryPoints',Arr);
    For I:=0 to Length(entryPoints)-1 do
      Arr.Add(entryPoints[i].SerializeObject);
    Result.Add('notes',notes);
    if Assigned(parameters) then
      Result.Add('parameters',parameters.SerializeObject);
    Result.Add('signature',signature);
  except
    Result.Free;
    raise;
  end;
end;

function TConferenceDataSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TConferenceDataSerializer.Deserialize(aJSON : TJSONObject) : TConferenceData;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TConferenceData.Create;
  If (aJSON=Nil) then
    exit;
  try
    Result.conferenceId:=aJSON.Get('conferenceId','');
    Result.conferenceSolution:=TConferenceSolution.Deserialize(aJSON.Get('conferenceSolution',TJSONObject(Nil)));
    Result.createRequest:=TCreateConferenceRequest.Deserialize(aJSON.Get('createRequest',TJSONObject(Nil)));
    lArr:=aJSON.Get('entryPoints',TJSONArray(Nil));
    if Assigned(lArr) then
      begin
      SetLength(Result.entryPoints,lArr.Count);
      For I:=0 to Length(Result.entryPoints)-1 do
        Result.entryPoints[i]:=TEntryPoint.Deserialize(lArr[i] as TJSONObject);
      end;
    Result.notes:=aJSON.Get('notes','');
    Result.parameters:=TConferenceParameters.Deserialize(aJSON.Get('parameters',TJSONObject(Nil)));
    Result.signature:=aJSON.Get('signature','');
  except
    Result.Free;
    raise;
  end;
end;

class function TConferenceDataSerializer.Deserialize(aJSON : String) : TConferenceData;

var
  lObj : TJSONObject;
begin
  Result := Default(TConferenceData);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TErrorSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('domain',domain);
    Result.Add('reason',reason);
  except
    Result.Free;
    raise;
  end;
end;

function TErrorSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TErrorSerializer.Deserialize(aJSON : TJSONObject) : TError;

begin
  Result := TError.Create;
  If (aJSON=Nil) then
    exit;
  Result.domain:=aJSON.Get('domain','');
  Result.reason:=aJSON.Get('reason','');
end;

class function TErrorSerializer.Deserialize(aJSON : String) : TError;

var
  lObj : TJSONObject;
begin
  Result := Default(TError);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TEventAttachmentSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('fileId',fileId);
    Result.Add('fileUrl',fileUrl);
    Result.Add('iconLink',iconLink);
    Result.Add('mimeType',mimeType);
    Result.Add('title',title);
  except
    Result.Free;
    raise;
  end;
end;

function TEventAttachmentSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEventAttachmentSerializer.Deserialize(aJSON : TJSONObject) : TEventAttachment;

begin
  Result := TEventAttachment.Create;
  If (aJSON=Nil) then
    exit;
  Result.fileId:=aJSON.Get('fileId','');
  Result.fileUrl:=aJSON.Get('fileUrl','');
  Result.iconLink:=aJSON.Get('iconLink','');
  Result.mimeType:=aJSON.Get('mimeType','');
  Result.title:=aJSON.Get('title','');
end;

class function TEventAttachmentSerializer.Deserialize(aJSON : String) : TEventAttachment;

var
  lObj : TJSONObject;
begin
  Result := Default(TEventAttachment);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TEventAttendeeSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('additionalGuests',additionalGuests);
    Result.Add('asyncOperation',asyncOperation);
    Result.Add('comment',comment);
    Result.Add('displayName',displayName);
    Result.Add('email',email);
    Result.Add('id',id);
    Result.Add('optional',optional);
    Result.Add('organizer',organizer);
    Result.Add('resource',resource);
    Result.Add('responseStatus',responseStatus);
    Result.Add('self',self_);
  except
    Result.Free;
    raise;
  end;
end;

function TEventAttendeeSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEventAttendeeSerializer.Deserialize(aJSON : TJSONObject) : TEventAttendee;

begin
  Result := TEventAttendee.Create;
  If (aJSON=Nil) then
    exit;
  Result.additionalGuests:=aJSON.Get('additionalGuests',0);
  Result.asyncOperation:=aJSON.Get('asyncOperation','');
  Result.comment:=aJSON.Get('comment','');
  Result.displayName:=aJSON.Get('displayName','');
  Result.email:=aJSON.Get('email','');
  Result.id:=aJSON.Get('id','');
  Result.optional:=aJSON.Get('optional',False);
  Result.organizer:=aJSON.Get('organizer',False);
  Result.resource:=aJSON.Get('resource',False);
  Result.responseStatus:=aJSON.Get('responseStatus','');
  Result.self_:=aJSON.Get('self',False);
end;

class function TEventAttendeeSerializer.Deserialize(aJSON : String) : TEventAttendee;

var
  lObj : TJSONObject;
begin
  Result := Default(TEventAttendee);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TEventBirthdayPropertiesSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('contact',contact);
    Result.Add('customTypeName',customTypeName);
    Result.Add('type',type_);
  except
    Result.Free;
    raise;
  end;
end;

function TEventBirthdayPropertiesSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEventBirthdayPropertiesSerializer.Deserialize(aJSON : TJSONObject) : TEventBirthdayProperties;

begin
  Result := TEventBirthdayProperties.Create;
  If (aJSON=Nil) then
    exit;
  Result.contact:=aJSON.Get('contact','');
  Result.customTypeName:=aJSON.Get('customTypeName','');
  Result.type_:=aJSON.Get('type','');
end;

class function TEventBirthdayPropertiesSerializer.Deserialize(aJSON : String) : TEventBirthdayProperties;

var
  lObj : TJSONObject;
begin
  Result := Default(TEventBirthdayProperties);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TEventDateTimeSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if (date<>0) then
      Result.Add('date',DateOnlyToISO8601(date));
    if (dateTime<>0) then
      Result.Add('dateTime',DateToISO8601(dateTime,False));
    Result.Add('timeZone',timeZone);
  except
    Result.Free;
    raise;
  end;
end;

function TEventDateTimeSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEventDateTimeSerializer.Deserialize(aJSON : TJSONObject) : TEventDateTime;

begin
  Result := TEventDateTime.Create;
  If (aJSON=Nil) then
    exit;
  Result.date:=ISO8601ToDateOnlyDef(aJSON.Get('date',''),0);
  Result.dateTime:=ISO8601ToDateDef(aJSON.Get('dateTime',''),0,False);
  Result.timeZone:=aJSON.Get('timeZone','');
end;

class function TEventDateTimeSerializer.Deserialize(aJSON : String) : TEventDateTime;

var
  lObj : TJSONObject;
begin
  Result := Default(TEventDateTime);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TEventFocusTimePropertiesSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('autoDeclineMode',autoDeclineMode);
    Result.Add('chatStatus',chatStatus);
    Result.Add('declineMessage',declineMessage);
  except
    Result.Free;
    raise;
  end;
end;

function TEventFocusTimePropertiesSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEventFocusTimePropertiesSerializer.Deserialize(aJSON : TJSONObject) : TEventFocusTimeProperties;

begin
  Result := TEventFocusTimeProperties.Create;
  If (aJSON=Nil) then
    exit;
  Result.autoDeclineMode:=aJSON.Get('autoDeclineMode','');
  Result.chatStatus:=aJSON.Get('chatStatus','');
  Result.declineMessage:=aJSON.Get('declineMessage','');
end;

class function TEventFocusTimePropertiesSerializer.Deserialize(aJSON : String) : TEventFocusTimeProperties;

var
  lObj : TJSONObject;
begin
  Result := Default(TEventFocusTimeProperties);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TEventOutOfOfficePropertiesSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('autoDeclineMode',autoDeclineMode);
    Result.Add('declineMessage',declineMessage);
  except
    Result.Free;
    raise;
  end;
end;

function TEventOutOfOfficePropertiesSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEventOutOfOfficePropertiesSerializer.Deserialize(aJSON : TJSONObject) : TEventOutOfOfficeProperties;

begin
  Result := TEventOutOfOfficeProperties.Create;
  If (aJSON=Nil) then
    exit;
  Result.autoDeclineMode:=aJSON.Get('autoDeclineMode','');
  Result.declineMessage:=aJSON.Get('declineMessage','');
end;

class function TEventOutOfOfficePropertiesSerializer.Deserialize(aJSON : String) : TEventOutOfOfficeProperties;

var
  lObj : TJSONObject;
begin
  Result := Default(TEventOutOfOfficeProperties);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TEventWorkingLocationPropertiesSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if (customLocation<>'') then
      Result.Add('customLocation',GetJSON(customLocation));
    if (homeOffice<>'') then
      Result.Add('homeOffice',GetJSON(homeOffice));
    if (officeLocation<>'') then
      Result.Add('officeLocation',GetJSON(officeLocation));
    Result.Add('type',type_);
  except
    Result.Free;
    raise;
  end;
end;

function TEventWorkingLocationPropertiesSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEventWorkingLocationPropertiesSerializer.Deserialize(aJSON : TJSONObject) : TEventWorkingLocationProperties;

begin
  Result := TEventWorkingLocationProperties.Create;
  If (aJSON=Nil) then
    exit;
  Result.customLocation:=JSONDataAsString(aJSON.Get('customLocation',TJSONObject(Nil)));
  Result.homeOffice:=JSONDataAsString(aJSON.Get('homeOffice',TJSONObject(Nil)));
  Result.officeLocation:=JSONDataAsString(aJSON.Get('officeLocation',TJSONObject(Nil)));
  Result.type_:=aJSON.Get('type','');
end;

class function TEventWorkingLocationPropertiesSerializer.Deserialize(aJSON : String) : TEventWorkingLocationProperties;

var
  lObj : TJSONObject;
begin
  Result := Default(TEventWorkingLocationProperties);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TEventSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('anyoneCanAddSelf',anyoneCanAddSelf);
    Arr:=TJSONArray.Create;
    Result.Add('attachments',Arr);
    For I:=0 to Length(attachments)-1 do
      Arr.Add(attachments[i].SerializeObject);
    Arr:=TJSONArray.Create;
    Result.Add('attendees',Arr);
    For I:=0 to Length(attendees)-1 do
      Arr.Add(attendees[i].SerializeObject);
    Result.Add('attendeesOmitted',attendeesOmitted);
    if Assigned(birthdayProperties) then
      Result.Add('birthdayProperties',birthdayProperties.SerializeObject);
    Result.Add('colorId',colorId);
    if Assigned(conferenceData) then
      Result.Add('conferenceData',conferenceData.SerializeObject);
    if (created<>0) then
      Result.Add('created',DateToISO8601(created,False));
    if (creator<>'') then
      Result.Add('creator',GetJSON(creator));
    Result.Add('description',description);
    Result.Add('endTimeUnspecified',endTimeUnspecified);
    if Assigned(end_) then
      Result.Add('end',end_.SerializeObject);
    Result.Add('etag',etag);
    Result.Add('eventLabelId',eventLabelId);
    Result.Add('eventType',eventType);
    if (extendedProperties<>'') then
      Result.Add('extendedProperties',GetJSON(extendedProperties));
    if Assigned(focusTimeProperties) then
      Result.Add('focusTimeProperties',focusTimeProperties.SerializeObject);
    if (gadget<>'') then
      Result.Add('gadget',GetJSON(gadget));
    Result.Add('guestsCanInviteOthers',guestsCanInviteOthers);
    Result.Add('guestsCanModify',guestsCanModify);
    Result.Add('guestsCanSeeOtherGuests',guestsCanSeeOtherGuests);
    Result.Add('hangoutLink',hangoutLink);
    Result.Add('htmlLink',htmlLink);
    Result.Add('iCalUID',iCalUID);
    Result.Add('id',id);
    Result.Add('kind',kind);
    Result.Add('location',location);
    Result.Add('locked',locked);
    if (organizer<>'') then
      Result.Add('organizer',GetJSON(organizer));
    if Assigned(originalStartTime) then
      Result.Add('originalStartTime',originalStartTime.SerializeObject);
    if Assigned(outOfOfficeProperties) then
      Result.Add('outOfOfficeProperties',outOfOfficeProperties.SerializeObject);
    Result.Add('privateCopy',privateCopy);
    Arr:=TJSONArray.Create;
    Result.Add('recurrence',Arr);
    For I:=0 to Length(recurrence)-1 do
      Arr.Add(recurrence[i]);
    Result.Add('recurringEventId',recurringEventId);
    if (reminders<>'') then
      Result.Add('reminders',GetJSON(reminders));
    Result.Add('sequence',sequence);
    if (source<>'') then
      Result.Add('source',GetJSON(source));
    if Assigned(start) then
      Result.Add('start',start.SerializeObject);
    Result.Add('status',status);
    Result.Add('summary',summary);
    Result.Add('transparency',transparency);
    if (updated<>0) then
      Result.Add('updated',DateToISO8601(updated,False));
    Result.Add('visibility',visibility);
    if Assigned(workingLocationProperties) then
      Result.Add('workingLocationProperties',workingLocationProperties.SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TEventSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEventSerializer.Deserialize(aJSON : TJSONObject) : TEvent;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TEvent.Create;
  If (aJSON=Nil) then
    exit;
  try
    Result.anyoneCanAddSelf:=aJSON.Get('anyoneCanAddSelf',False);
    lArr:=aJSON.Get('attachments',TJSONArray(Nil));
    if Assigned(lArr) then
      begin
      SetLength(Result.attachments,lArr.Count);
      For I:=0 to Length(Result.attachments)-1 do
        Result.attachments[i]:=TEventAttachment.Deserialize(lArr[i] as TJSONObject);
      end;
    lArr:=aJSON.Get('attendees',TJSONArray(Nil));
    if Assigned(lArr) then
      begin
      SetLength(Result.attendees,lArr.Count);
      For I:=0 to Length(Result.attendees)-1 do
        Result.attendees[i]:=TEventAttendee.Deserialize(lArr[i] as TJSONObject);
      end;
    Result.attendeesOmitted:=aJSON.Get('attendeesOmitted',False);
    Result.birthdayProperties:=TEventBirthdayProperties.Deserialize(aJSON.Get('birthdayProperties',TJSONObject(Nil)));
    Result.colorId:=aJSON.Get('colorId','');
    Result.conferenceData:=TConferenceData.Deserialize(aJSON.Get('conferenceData',TJSONObject(Nil)));
    Result.created:=ISO8601ToDateDef(aJSON.Get('created',''),0,False);
    Result.creator:=JSONDataAsString(aJSON.Get('creator',TJSONObject(Nil)));
    Result.description:=aJSON.Get('description','');
    Result.endTimeUnspecified:=aJSON.Get('endTimeUnspecified',False);
    Result.end_:=TEventDateTime.Deserialize(aJSON.Get('end',TJSONObject(Nil)));
    Result.etag:=aJSON.Get('etag','');
    Result.eventLabelId:=aJSON.Get('eventLabelId','');
    Result.eventType:=aJSON.Get('eventType','');
    Result.extendedProperties:=JSONDataAsString(aJSON.Get('extendedProperties',TJSONObject(Nil)));
    Result.focusTimeProperties:=TEventFocusTimeProperties.Deserialize(aJSON.Get('focusTimeProperties',TJSONObject(Nil)));
    Result.gadget:=JSONDataAsString(aJSON.Get('gadget',TJSONObject(Nil)));
    Result.guestsCanInviteOthers:=aJSON.Get('guestsCanInviteOthers',False);
    Result.guestsCanModify:=aJSON.Get('guestsCanModify',False);
    Result.guestsCanSeeOtherGuests:=aJSON.Get('guestsCanSeeOtherGuests',False);
    Result.hangoutLink:=aJSON.Get('hangoutLink','');
    Result.htmlLink:=aJSON.Get('htmlLink','');
    Result.iCalUID:=aJSON.Get('iCalUID','');
    Result.id:=aJSON.Get('id','');
    Result.kind:=aJSON.Get('kind','');
    Result.location:=aJSON.Get('location','');
    Result.locked:=aJSON.Get('locked',False);
    Result.organizer:=JSONDataAsString(aJSON.Get('organizer',TJSONObject(Nil)));
    Result.originalStartTime:=TEventDateTime.Deserialize(aJSON.Get('originalStartTime',TJSONObject(Nil)));
    Result.outOfOfficeProperties:=TEventOutOfOfficeProperties.Deserialize(aJSON.Get('outOfOfficeProperties',TJSONObject(Nil)));
    Result.privateCopy:=aJSON.Get('privateCopy',False);
    lArr:=aJSON.Get('recurrence',TJSONArray(Nil));
    if Assigned(lArr) then
      begin
      SetLength(Result.recurrence,lArr.Count);
      For I:=0 to Length(Result.recurrence)-1 do
        Result.recurrence[i]:=lArr[i].Asstring;
      end;
    Result.recurringEventId:=aJSON.Get('recurringEventId','');
    Result.reminders:=JSONDataAsString(aJSON.Get('reminders',TJSONObject(Nil)));
    Result.sequence:=aJSON.Get('sequence',0);
    Result.source:=JSONDataAsString(aJSON.Get('source',TJSONObject(Nil)));
    Result.start:=TEventDateTime.Deserialize(aJSON.Get('start',TJSONObject(Nil)));
    Result.status:=aJSON.Get('status','');
    Result.summary:=aJSON.Get('summary','');
    Result.transparency:=aJSON.Get('transparency','');
    Result.updated:=ISO8601ToDateDef(aJSON.Get('updated',''),0,False);
    Result.visibility:=aJSON.Get('visibility','');
    Result.workingLocationProperties:=TEventWorkingLocationProperties.Deserialize(aJSON.Get('workingLocationProperties',TJSONObject(Nil)));
  except
    Result.Free;
    raise;
  end;
end;

class function TEventSerializer.Deserialize(aJSON : String) : TEvent;

var
  lObj : TJSONObject;
begin
  Result := Default(TEvent);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TEventsSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('accessRole',accessRole);
    Arr:=TJSONArray.Create;
    Result.Add('defaultReminders',Arr);
    For I:=0 to Length(defaultReminders)-1 do
      Arr.Add(defaultReminders[i].SerializeObject);
    Result.Add('description',description);
    Result.Add('etag',etag);
    Arr:=TJSONArray.Create;
    Result.Add('items',Arr);
    For I:=0 to Length(items)-1 do
      Arr.Add(items[i].SerializeObject);
    Result.Add('kind',kind);
    Result.Add('nextPageToken',nextPageToken);
    Result.Add('nextSyncToken',nextSyncToken);
    Result.Add('summary',summary);
    Result.Add('timeZone',timeZone);
    if (updated<>0) then
      Result.Add('updated',DateToISO8601(updated,False));
  except
    Result.Free;
    raise;
  end;
end;

function TEventsSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEventsSerializer.Deserialize(aJSON : TJSONObject) : TEvents;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TEvents.Create;
  If (aJSON=Nil) then
    exit;
  try
    Result.accessRole:=aJSON.Get('accessRole','');
    lArr:=aJSON.Get('defaultReminders',TJSONArray(Nil));
    if Assigned(lArr) then
      begin
      SetLength(Result.defaultReminders,lArr.Count);
      For I:=0 to Length(Result.defaultReminders)-1 do
        Result.defaultReminders[i]:=TEventReminder.Deserialize(lArr[i] as TJSONObject);
      end;
    Result.description:=aJSON.Get('description','');
    Result.etag:=aJSON.Get('etag','');
    lArr:=aJSON.Get('items',TJSONArray(Nil));
    if Assigned(lArr) then
      begin
      SetLength(Result.items,lArr.Count);
      For I:=0 to Length(Result.items)-1 do
        Result.items[i]:=TEvent.Deserialize(lArr[i] as TJSONObject);
      end;
    Result.kind:=aJSON.Get('kind','');
    Result.nextPageToken:=aJSON.Get('nextPageToken','');
    Result.nextSyncToken:=aJSON.Get('nextSyncToken','');
    Result.summary:=aJSON.Get('summary','');
    Result.timeZone:=aJSON.Get('timeZone','');
    Result.updated:=ISO8601ToDateDef(aJSON.Get('updated',''),0,False);
  except
    Result.Free;
    raise;
  end;
end;

class function TEventsSerializer.Deserialize(aJSON : String) : TEvents;

var
  lObj : TJSONObject;
begin
  Result := Default(TEvents);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TTimePeriodSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if (end_<>0) then
      Result.Add('end',DateToISO8601(end_,False));
    if (start<>0) then
      Result.Add('start',DateToISO8601(start,False));
  except
    Result.Free;
    raise;
  end;
end;

function TTimePeriodSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TTimePeriodSerializer.Deserialize(aJSON : TJSONObject) : TTimePeriod;

begin
  Result := TTimePeriod.Create;
  If (aJSON=Nil) then
    exit;
  Result.end_:=ISO8601ToDateDef(aJSON.Get('end',''),0,False);
  Result.start:=ISO8601ToDateDef(aJSON.Get('start',''),0,False);
end;

class function TTimePeriodSerializer.Deserialize(aJSON : String) : TTimePeriod;

var
  lObj : TJSONObject;
begin
  Result := Default(TTimePeriod);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TFreeBusyCalendarSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('busy',Arr);
    For I:=0 to Length(busy)-1 do
      Arr.Add(busy[i].SerializeObject);
    Arr:=TJSONArray.Create;
    Result.Add('errors',Arr);
    For I:=0 to Length(errors)-1 do
      Arr.Add(errors[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TFreeBusyCalendarSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TFreeBusyCalendarSerializer.Deserialize(aJSON : TJSONObject) : TFreeBusyCalendar;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TFreeBusyCalendar.Create;
  If (aJSON=Nil) then
    exit;
  try
    lArr:=aJSON.Get('busy',TJSONArray(Nil));
    if Assigned(lArr) then
      begin
      SetLength(Result.busy,lArr.Count);
      For I:=0 to Length(Result.busy)-1 do
        Result.busy[i]:=TTimePeriod.Deserialize(lArr[i] as TJSONObject);
      end;
    lArr:=aJSON.Get('errors',TJSONArray(Nil));
    if Assigned(lArr) then
      begin
      SetLength(Result.errors,lArr.Count);
      For I:=0 to Length(Result.errors)-1 do
        Result.errors[i]:=TError.Deserialize(lArr[i] as TJSONObject);
      end;
  except
    Result.Free;
    raise;
  end;
end;

class function TFreeBusyCalendarSerializer.Deserialize(aJSON : String) : TFreeBusyCalendar;

var
  lObj : TJSONObject;
begin
  Result := Default(TFreeBusyCalendar);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TFreeBusyGroupSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Arr:=TJSONArray.Create;
    Result.Add('calendars',Arr);
    For I:=0 to Length(calendars)-1 do
      Arr.Add(calendars[i]);
    Arr:=TJSONArray.Create;
    Result.Add('errors',Arr);
    For I:=0 to Length(errors)-1 do
      Arr.Add(errors[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;

function TFreeBusyGroupSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TFreeBusyGroupSerializer.Deserialize(aJSON : TJSONObject) : TFreeBusyGroup;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TFreeBusyGroup.Create;
  If (aJSON=Nil) then
    exit;
  try
    lArr:=aJSON.Get('calendars',TJSONArray(Nil));
    if Assigned(lArr) then
      begin
      SetLength(Result.calendars,lArr.Count);
      For I:=0 to Length(Result.calendars)-1 do
        Result.calendars[i]:=lArr[i].Asstring;
      end;
    lArr:=aJSON.Get('errors',TJSONArray(Nil));
    if Assigned(lArr) then
      begin
      SetLength(Result.errors,lArr.Count);
      For I:=0 to Length(Result.errors)-1 do
        Result.errors[i]:=TError.Deserialize(lArr[i] as TJSONObject);
      end;
  except
    Result.Free;
    raise;
  end;
end;

class function TFreeBusyGroupSerializer.Deserialize(aJSON : String) : TFreeBusyGroup;

var
  lObj : TJSONObject;
begin
  Result := Default(TFreeBusyGroup);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TFreeBusyRequestItemSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('id',id);
  except
    Result.Free;
    raise;
  end;
end;

function TFreeBusyRequestItemSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TFreeBusyRequestItemSerializer.Deserialize(aJSON : TJSONObject) : TFreeBusyRequestItem;

begin
  Result := TFreeBusyRequestItem.Create;
  If (aJSON=Nil) then
    exit;
  Result.id:=aJSON.Get('id','');
end;

class function TFreeBusyRequestItemSerializer.Deserialize(aJSON : String) : TFreeBusyRequestItem;

var
  lObj : TJSONObject;
begin
  Result := Default(TFreeBusyRequestItem);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TFreeBusyRequestSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('calendarExpansionMax',calendarExpansionMax);
    Result.Add('groupExpansionMax',groupExpansionMax);
    Arr:=TJSONArray.Create;
    Result.Add('items',Arr);
    For I:=0 to Length(items)-1 do
      Arr.Add(items[i].SerializeObject);
    if (timeMax<>0) then
      Result.Add('timeMax',DateToISO8601(timeMax,False));
    if (timeMin<>0) then
      Result.Add('timeMin',DateToISO8601(timeMin,False));
    Result.Add('timeZone',timeZone);
  except
    Result.Free;
    raise;
  end;
end;

function TFreeBusyRequestSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TFreeBusyRequestSerializer.Deserialize(aJSON : TJSONObject) : TFreeBusyRequest;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TFreeBusyRequest.Create;
  If (aJSON=Nil) then
    exit;
  try
    Result.calendarExpansionMax:=aJSON.Get('calendarExpansionMax',0);
    Result.groupExpansionMax:=aJSON.Get('groupExpansionMax',0);
    lArr:=aJSON.Get('items',TJSONArray(Nil));
    if Assigned(lArr) then
      begin
      SetLength(Result.items,lArr.Count);
      For I:=0 to Length(Result.items)-1 do
        Result.items[i]:=TFreeBusyRequestItem.Deserialize(lArr[i] as TJSONObject);
      end;
    Result.timeMax:=ISO8601ToDateDef(aJSON.Get('timeMax',''),0,False);
    Result.timeMin:=ISO8601ToDateDef(aJSON.Get('timeMin',''),0,False);
    Result.timeZone:=aJSON.Get('timeZone','');
  except
    Result.Free;
    raise;
  end;
end;

class function TFreeBusyRequestSerializer.Deserialize(aJSON : String) : TFreeBusyRequest;

var
  lObj : TJSONObject;
begin
  Result := Default(TFreeBusyRequest);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TFreeBusyResponseSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    if (calendars<>'') then
      Result.Add('calendars',GetJSON(calendars));
    if (groups<>'') then
      Result.Add('groups',GetJSON(groups));
    Result.Add('kind',kind);
    if (timeMax<>0) then
      Result.Add('timeMax',DateToISO8601(timeMax,False));
    if (timeMin<>0) then
      Result.Add('timeMin',DateToISO8601(timeMin,False));
  except
    Result.Free;
    raise;
  end;
end;

function TFreeBusyResponseSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TFreeBusyResponseSerializer.Deserialize(aJSON : TJSONObject) : TFreeBusyResponse;

begin
  Result := TFreeBusyResponse.Create;
  If (aJSON=Nil) then
    exit;
  Result.calendars:=JSONDataAsString(aJSON.Get('calendars',TJSONObject(Nil)));
  Result.groups:=JSONDataAsString(aJSON.Get('groups',TJSONObject(Nil)));
  Result.kind:=aJSON.Get('kind','');
  Result.timeMax:=ISO8601ToDateDef(aJSON.Get('timeMax',''),0,False);
  Result.timeMin:=ISO8601ToDateDef(aJSON.Get('timeMin',''),0,False);
end;

class function TFreeBusyResponseSerializer.Deserialize(aJSON : String) : TFreeBusyResponse;

var
  lObj : TJSONObject;
begin
  Result := Default(TFreeBusyResponse);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TSettingSerializer.SerializeObject : TJSONObject;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('etag',etag);
    Result.Add('id',id);
    Result.Add('kind',kind);
    Result.Add('value',value);
  except
    Result.Free;
    raise;
  end;
end;

function TSettingSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TSettingSerializer.Deserialize(aJSON : TJSONObject) : TSetting;

begin
  Result := TSetting.Create;
  If (aJSON=Nil) then
    exit;
  Result.etag:=aJSON.Get('etag','');
  Result.id:=aJSON.Get('id','');
  Result.kind:=aJSON.Get('kind','');
  Result.value:=aJSON.Get('value','');
end;

class function TSettingSerializer.Deserialize(aJSON : String) : TSetting;

var
  lObj : TJSONObject;
begin
  Result := Default(TSetting);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;

function TSettingsSerializer.SerializeObject : TJSONObject;

var
  i : integer;
  Arr : TJSONArray;

begin
  Result:=TJSONObject.Create;
  try
    Result.Add('etag',etag);
    Arr:=TJSONArray.Create;
    Result.Add('items',Arr);
    For I:=0 to Length(items)-1 do
      Arr.Add(items[i].SerializeObject);
    Result.Add('kind',kind);
    Result.Add('nextPageToken',nextPageToken);
    Result.Add('nextSyncToken',nextSyncToken);
  except
    Result.Free;
    raise;
  end;
end;

function TSettingsSerializer.Serialize : String;
var
  lObj : TJSONObject;
begin
  lObj:=SerializeObject;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TSettingsSerializer.Deserialize(aJSON : TJSONObject) : TSettings;

var
  lArr : TJSONArray;
  i : Integer;
begin
  Result := TSettings.Create;
  If (aJSON=Nil) then
    exit;
  try
    Result.etag:=aJSON.Get('etag','');
    lArr:=aJSON.Get('items',TJSONArray(Nil));
    if Assigned(lArr) then
      begin
      SetLength(Result.items,lArr.Count);
      For I:=0 to Length(Result.items)-1 do
        Result.items[i]:=TSetting.Deserialize(lArr[i] as TJSONObject);
      end;
    Result.kind:=aJSON.Get('kind','');
    Result.nextPageToken:=aJSON.Get('nextPageToken','');
    Result.nextSyncToken:=aJSON.Get('nextSyncToken','');
  except
    Result.Free;
    raise;
  end;
end;

class function TSettingsSerializer.Deserialize(aJSON : String) : TSettings;

var
  lObj : TJSONObject;
begin
  Result := Default(TSettings);
  if (aJSON='') then
    exit;
  lObj := GetJSON(aJSON) as TJSONObject;
  if (lObj = nil) then
    exit;
  try
    Result:=Deserialize(lObj);
  finally
    lObj.Free
  end;
end;


function TAclRuleArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TAclRuleArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TAclRuleArraySerializer.Deserialize(aJSON : TJSONArray) : TAclRuleArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TAclRule.Deserialize(aJSON[i] as TJSONObject);
end;

class function TAclRuleArraySerializer.Deserialize(aJSON : String) : TAclRuleArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TCalendarListEntryArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TCalendarListEntryArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TCalendarListEntryArraySerializer.Deserialize(aJSON : TJSONArray) : TCalendarListEntryArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TCalendarListEntry.Deserialize(aJSON[i] as TJSONObject);
end;

class function TCalendarListEntryArraySerializer.Deserialize(aJSON : String) : TCalendarListEntryArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TEntryPointArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TEntryPointArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEntryPointArraySerializer.Deserialize(aJSON : TJSONArray) : TEntryPointArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TEntryPoint.Deserialize(aJSON[i] as TJSONObject);
end;

class function TEntryPointArraySerializer.Deserialize(aJSON : String) : TEntryPointArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TErrorArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TErrorArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TErrorArraySerializer.Deserialize(aJSON : TJSONArray) : TErrorArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TError.Deserialize(aJSON[i] as TJSONObject);
end;

class function TErrorArraySerializer.Deserialize(aJSON : String) : TErrorArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TEventAttachmentArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TEventAttachmentArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEventAttachmentArraySerializer.Deserialize(aJSON : TJSONArray) : TEventAttachmentArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TEventAttachment.Deserialize(aJSON[i] as TJSONObject);
end;

class function TEventAttachmentArraySerializer.Deserialize(aJSON : String) : TEventAttachmentArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TEventAttendeeArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TEventAttendeeArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEventAttendeeArraySerializer.Deserialize(aJSON : TJSONArray) : TEventAttendeeArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TEventAttendee.Deserialize(aJSON[i] as TJSONObject);
end;

class function TEventAttendeeArraySerializer.Deserialize(aJSON : String) : TEventAttendeeArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TEventLabelArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TEventLabelArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEventLabelArraySerializer.Deserialize(aJSON : TJSONArray) : TEventLabelArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TEventLabel.Deserialize(aJSON[i] as TJSONObject);
end;

class function TEventLabelArraySerializer.Deserialize(aJSON : String) : TEventLabelArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TEventReminderArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TEventReminderArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEventReminderArraySerializer.Deserialize(aJSON : TJSONArray) : TEventReminderArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TEventReminder.Deserialize(aJSON[i] as TJSONObject);
end;

class function TEventReminderArraySerializer.Deserialize(aJSON : String) : TEventReminderArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TEventArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TEventArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TEventArraySerializer.Deserialize(aJSON : TJSONArray) : TEventArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TEvent.Deserialize(aJSON[i] as TJSONObject);
end;

class function TEventArraySerializer.Deserialize(aJSON : String) : TEventArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TFreeBusyRequestItemArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TFreeBusyRequestItemArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TFreeBusyRequestItemArraySerializer.Deserialize(aJSON : TJSONArray) : TFreeBusyRequestItemArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TFreeBusyRequestItem.Deserialize(aJSON[i] as TJSONObject);
end;

class function TFreeBusyRequestItemArraySerializer.Deserialize(aJSON : String) : TFreeBusyRequestItemArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TSettingArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TSettingArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TSettingArraySerializer.Deserialize(aJSON : TJSONArray) : TSettingArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TSetting.Deserialize(aJSON[i] as TJSONObject);
end;

class function TSettingArraySerializer.Deserialize(aJSON : String) : TSettingArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


function TTimePeriodArraySerializer.SerializeArray : TJSONArray;
var
  I : Integer;
begin
  Result:=TJSONArray.Create;
  try
    For I:=0 to length(Self)-1 do
      Result.Add(self[i].SerializeObject);
  except
    Result.Free;
    raise;
  end;
end;


function TTimePeriodArraySerializer.Serialize : String;
var
  lObj : TJSONArray;
begin
  lObj:=SerializeArray;
  try
    Result:=lObj.AsJSON;
  finally
    lObj.Free
  end;
end;

class function TTimePeriodArraySerializer.Deserialize(aJSON : TJSONArray) : TTimePeriodArray; 

var
  i : integer;
begin
  SetLength(Result,aJSON.Count);
  For i:=0 to aJSON.Count-1 do
    Result[i]:=TTimePeriod.Deserialize(aJSON[i] as TJSONObject);
end;

class function TTimePeriodArraySerializer.Deserialize(aJSON : String) : TTimePeriodArray; 

var
  lObj : TJSONData;
  lArr : TJSONArray absolute lobj;
begin
  lObj:=GetJSON(aJSON);
  try
    Result:=DeSerialize(lArr);
  finally
    lObj.Free;
  end;
end;


end.
