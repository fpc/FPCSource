{ -----------------------------------------------------------------------
  Do not edit !
  
  This file was automatically generated on 2026-10-03 18:11.
  Used command-line parameters:
     -s calendar -L -o calendar -q
  Source OpenAPI document data:
    Title: Calendar API
    Version: v3
  -----------------------------------------------------------------------}
unit calendar.Dto;

{$mode objfpc}
{$h+}


interface

uses types;

Type

  TAclRule = class;
  TAcl = class;
  TConferenceProperties = class;
  TEventLabel = class;
  TLabelProperties = class;
  TCalendar = class;
  TEventReminder = class;
  TCalendarListEntry = class;
  TCalendarList = class;
  TCalendarNotification = class;
  TChannel = class;
  TColorDefinition = class;
  TColors = class;
  TConferenceSolutionKey = class;
  TConferenceSolution = class;
  TConferenceRequestStatus = class;
  TCreateConferenceRequest = class;
  TEntryPoint = class;
  TConferenceParametersAddOnParameters = class;
  TConferenceParameters = class;
  TConferenceData = class;
  TError = class;
  TEventAttachment = class;
  TEventAttendee = class;
  TEventBirthdayProperties = class;
  TEventDateTime = class;
  TEventFocusTimeProperties = class;
  TEventOutOfOfficeProperties = class;
  TEventWorkingLocationProperties = class;
  TEvent = class;
  TEvents = class;
  TTimePeriod = class;
  TFreeBusyCalendar = class;
  TFreeBusyGroup = class;
  TFreeBusyRequestItem = class;
  TFreeBusyRequest = class;
  TFreeBusyResponse = class;
  TSetting = class;
  TSettings = class;
  TAclRuleArray = Array of TAclRule;
  TCalendarListEntryArray = Array of TCalendarListEntry;
  TEntryPointArray = Array of TEntryPoint;
  TErrorArray = Array of TError;
  TEventAttachmentArray = Array of TEventAttachment;
  TEventAttendeeArray = Array of TEventAttendee;
  TEventLabelArray = Array of TEventLabel;
  TEventReminderArray = Array of TEventReminder;
  TEventArray = Array of TEvent;
  TFreeBusyRequestItemArray = Array of TFreeBusyRequestItem;
  TSettingArray = Array of TSetting;
  TTimePeriodArray = Array of TTimePeriod;
  
  TAclRule = Class(TObject)
    etag : string;
    id : string;
    kind : string;
    role : string;
    scope : string;
  end;
  
  TAcl = Class(TObject)
    etag : string;
    items : TAclRuleArray;
    kind : string;
    nextPageToken : string;
    nextSyncToken : string;
    destructor Destroy; override;
  end;
  
  TConferenceProperties = Class(TObject)
    allowedConferenceSolutionTypes : TStringDynArray;
  end;
  
  TEventLabel = Class(TObject)
    backgroundColor : string;
    id : string;
    name : string;
  end;
  
  TLabelProperties = Class(TObject)
    eventLabels : TEventLabelArray;
    destructor Destroy; override;
  end;
  
  TCalendar = Class(TObject)
    autoAcceptInvitations : boolean;
    conferenceProperties : TConferenceProperties;
    dataOwner : string;
    description : string;
    etag : string;
    id : string;
    kind : string;
    labelProperties : TLabelProperties;
    location : string;
    summary : string;
    timeZone : string;
    constructor CreateWithMembers;
    destructor Destroy; override;
  end;
  
  TEventReminder = Class(TObject)
    method : string;
    minutes : integer;
  end;
  
  TCalendarListEntry = Class(TObject)
    accessRole : string;
    autoAcceptInvitations : boolean;
    backgroundColor : string;
    colorId : string;
    conferenceProperties : TConferenceProperties;
    dataOwner : string;
    defaultReminders : TEventReminderArray;
    deleted : boolean;
    description : string;
    etag : string;
    foregroundColor : string;
    hidden : boolean;
    id : string;
    kind : string;
    location : string;
    notificationSettings : string;
    primary : boolean;
    selected : boolean;
    summary : string;
    summaryOverride : string;
    timeZone : string;
    constructor CreateWithMembers;
    destructor Destroy; override;
  end;
  
  TCalendarList = Class(TObject)
    etag : string;
    items : TCalendarListEntryArray;
    kind : string;
    nextPageToken : string;
    nextSyncToken : string;
    destructor Destroy; override;
  end;
  
  TCalendarNotification = Class(TObject)
    method : string;
    type_ : string;
  end;
  
  TChannel = Class(TObject)
    address : string;
    expiration : string;
    id : string;
    kind : string;
    params : string;
    payload : boolean;
    resourceId : string;
    resourceUri : string;
    token : string;
    type_ : string;
  end;
  
  TColorDefinition = Class(TObject)
    background : string;
    foreground : string;
  end;
  
  TColors = Class(TObject)
    calendar : string;
    event : string;
    kind : string;
    updated : TDateTime;
  end;
  
  TConferenceSolutionKey = Class(TObject)
    type_ : string;
  end;
  
  TConferenceSolution = Class(TObject)
    iconUri : string;
    key : TConferenceSolutionKey;
    name : string;
    constructor CreateWithMembers;
    destructor Destroy; override;
  end;
  
  TConferenceRequestStatus = Class(TObject)
    statusCode : string;
  end;
  
  TCreateConferenceRequest = Class(TObject)
    conferenceSolutionKey : TConferenceSolutionKey;
    requestId : string;
    status : TConferenceRequestStatus;
    constructor CreateWithMembers;
    destructor Destroy; override;
  end;
  
  TEntryPoint = Class(TObject)
    accessCode : string;
    entryPointFeatures : TStringDynArray;
    entryPointType : string;
    label_ : string;
    meetingCode : string;
    passcode : string;
    password : string;
    pin : string;
    regionCode : string;
    uri : string;
  end;
  
  TConferenceParametersAddOnParameters = Class(TObject)
    parameters : string;
  end;
  
  TConferenceParameters = Class(TObject)
    addOnParameters : TConferenceParametersAddOnParameters;
    constructor CreateWithMembers;
    destructor Destroy; override;
  end;
  
  TConferenceData = Class(TObject)
    conferenceId : string;
    conferenceSolution : TConferenceSolution;
    createRequest : TCreateConferenceRequest;
    entryPoints : TEntryPointArray;
    notes : string;
    parameters : TConferenceParameters;
    signature : string;
    constructor CreateWithMembers;
    destructor Destroy; override;
  end;
  
  TError = Class(TObject)
    domain : string;
    reason : string;
  end;
  
  TEventAttachment = Class(TObject)
    fileId : string;
    fileUrl : string;
    iconLink : string;
    mimeType : string;
    title : string;
  end;
  
  TEventAttendee = Class(TObject)
    additionalGuests : integer;
    asyncOperation : string;
    comment : string;
    displayName : string;
    email : string;
    id : string;
    optional : boolean;
    organizer : boolean;
    resource : boolean;
    responseStatus : string;
    self_ : boolean;
  end;
  
  TEventBirthdayProperties = Class(TObject)
    contact : string;
    customTypeName : string;
    type_ : string;
  end;
  
  TEventDateTime = Class(TObject)
    date : TDateTime;
    dateTime : TDateTime;
    timeZone : string;
  end;
  
  TEventFocusTimeProperties = Class(TObject)
    autoDeclineMode : string;
    chatStatus : string;
    declineMessage : string;
  end;
  
  TEventOutOfOfficeProperties = Class(TObject)
    autoDeclineMode : string;
    declineMessage : string;
  end;
  
  TEventWorkingLocationProperties = Class(TObject)
    customLocation : string;
    homeOffice : string;
    officeLocation : string;
    type_ : string;
  end;
  
  TEvent = Class(TObject)
    anyoneCanAddSelf : boolean;
    attachments : TEventAttachmentArray;
    attendees : TEventAttendeeArray;
    attendeesOmitted : boolean;
    birthdayProperties : TEventBirthdayProperties;
    colorId : string;
    conferenceData : TConferenceData;
    created : TDateTime;
    creator : string;
    description : string;
    endTimeUnspecified : boolean;
    end_ : TEventDateTime;
    etag : string;
    eventLabelId : string;
    eventType : string;
    extendedProperties : string;
    focusTimeProperties : TEventFocusTimeProperties;
    gadget : string;
    guestsCanInviteOthers : boolean;
    guestsCanModify : boolean;
    guestsCanSeeOtherGuests : boolean;
    hangoutLink : string;
    htmlLink : string;
    iCalUID : string;
    id : string;
    kind : string;
    location : string;
    locked : boolean;
    organizer : string;
    originalStartTime : TEventDateTime;
    outOfOfficeProperties : TEventOutOfOfficeProperties;
    privateCopy : boolean;
    recurrence : TStringDynArray;
    recurringEventId : string;
    reminders : string;
    sequence : integer;
    source : string;
    start : TEventDateTime;
    status : string;
    summary : string;
    transparency : string;
    updated : TDateTime;
    visibility : string;
    workingLocationProperties : TEventWorkingLocationProperties;
    constructor CreateWithMembers;
    destructor Destroy; override;
  end;
  
  TEvents = Class(TObject)
    accessRole : string;
    defaultReminders : TEventReminderArray;
    description : string;
    etag : string;
    items : TEventArray;
    kind : string;
    nextPageToken : string;
    nextSyncToken : string;
    summary : string;
    timeZone : string;
    updated : TDateTime;
    destructor Destroy; override;
  end;
  
  TTimePeriod = Class(TObject)
    end_ : TDateTime;
    start : TDateTime;
  end;
  
  TFreeBusyCalendar = Class(TObject)
    busy : TTimePeriodArray;
    errors : TErrorArray;
    destructor Destroy; override;
  end;
  
  TFreeBusyGroup = Class(TObject)
    calendars : TStringDynArray;
    errors : TErrorArray;
    destructor Destroy; override;
  end;
  
  TFreeBusyRequestItem = Class(TObject)
    id : string;
  end;
  
  TFreeBusyRequest = Class(TObject)
    calendarExpansionMax : integer;
    groupExpansionMax : integer;
    items : TFreeBusyRequestItemArray;
    timeMax : TDateTime;
    timeMin : TDateTime;
    timeZone : string;
    destructor Destroy; override;
  end;
  
  TFreeBusyResponse = Class(TObject)
    calendars : string;
    groups : string;
    kind : string;
    timeMax : TDateTime;
    timeMin : TDateTime;
  end;
  
  TSetting = Class(TObject)
    etag : string;
    id : string;
    kind : string;
    value : string;
  end;
  
  TSettings = Class(TObject)
    etag : string;
    items : TSettingArray;
    kind : string;
    nextPageToken : string;
    nextSyncToken : string;
    destructor Destroy; override;
  end;
  
implementation

destructor TAcl.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(items)-1 do
    items[lI].Free;
  inherited Destroy;
end;

destructor TLabelProperties.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(eventLabels)-1 do
    eventLabels[lI].Free;
  inherited Destroy;
end;

constructor TCalendar.CreateWithMembers;

begin
  conferenceProperties := TConferenceProperties.Create;
  labelProperties := TLabelProperties.Create;
end;

destructor TCalendar.Destroy;

begin
  conferenceProperties.Free;
  labelProperties.Free;
  inherited Destroy;
end;

constructor TCalendarListEntry.CreateWithMembers;

begin
  conferenceProperties := TConferenceProperties.Create;
end;

destructor TCalendarListEntry.Destroy;

var
  lI : Integer;

begin
  conferenceProperties.Free;
  for lI:=0 to Length(defaultReminders)-1 do
    defaultReminders[lI].Free;
  inherited Destroy;
end;

destructor TCalendarList.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(items)-1 do
    items[lI].Free;
  inherited Destroy;
end;

constructor TConferenceSolution.CreateWithMembers;

begin
  key := TConferenceSolutionKey.Create;
end;

destructor TConferenceSolution.Destroy;

begin
  key.Free;
  inherited Destroy;
end;

constructor TCreateConferenceRequest.CreateWithMembers;

begin
  conferenceSolutionKey := TConferenceSolutionKey.Create;
  status := TConferenceRequestStatus.Create;
end;

destructor TCreateConferenceRequest.Destroy;

begin
  conferenceSolutionKey.Free;
  status.Free;
  inherited Destroy;
end;

constructor TConferenceParameters.CreateWithMembers;

begin
  addOnParameters := TConferenceParametersAddOnParameters.Create;
end;

destructor TConferenceParameters.Destroy;

begin
  addOnParameters.Free;
  inherited Destroy;
end;

constructor TConferenceData.CreateWithMembers;

begin
  conferenceSolution := TConferenceSolution.CreateWithMembers;
  createRequest := TCreateConferenceRequest.CreateWithMembers;
  parameters := TConferenceParameters.CreateWithMembers;
end;

destructor TConferenceData.Destroy;

var
  lI : Integer;

begin
  conferenceSolution.Free;
  createRequest.Free;
  for lI:=0 to Length(entryPoints)-1 do
    entryPoints[lI].Free;
  parameters.Free;
  inherited Destroy;
end;

constructor TEvent.CreateWithMembers;

begin
  birthdayProperties := TEventBirthdayProperties.Create;
  conferenceData := TConferenceData.CreateWithMembers;
  end_ := TEventDateTime.Create;
  focusTimeProperties := TEventFocusTimeProperties.Create;
  originalStartTime := TEventDateTime.Create;
  outOfOfficeProperties := TEventOutOfOfficeProperties.Create;
  start := TEventDateTime.Create;
  workingLocationProperties := TEventWorkingLocationProperties.Create;
end;

destructor TEvent.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(attachments)-1 do
    attachments[lI].Free;
  for lI:=0 to Length(attendees)-1 do
    attendees[lI].Free;
  birthdayProperties.Free;
  conferenceData.Free;
  end_.Free;
  focusTimeProperties.Free;
  originalStartTime.Free;
  outOfOfficeProperties.Free;
  start.Free;
  workingLocationProperties.Free;
  inherited Destroy;
end;

destructor TEvents.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(defaultReminders)-1 do
    defaultReminders[lI].Free;
  for lI:=0 to Length(items)-1 do
    items[lI].Free;
  inherited Destroy;
end;

destructor TFreeBusyCalendar.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(busy)-1 do
    busy[lI].Free;
  for lI:=0 to Length(errors)-1 do
    errors[lI].Free;
  inherited Destroy;
end;

destructor TFreeBusyGroup.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(errors)-1 do
    errors[lI].Free;
  inherited Destroy;
end;

destructor TFreeBusyRequest.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(items)-1 do
    items[lI].Free;
  inherited Destroy;
end;

destructor TSettings.Destroy;

var
  lI : Integer;

begin
  for lI:=0 to Length(items)-1 do
    items[lI].Free;
  inherited Destroy;
end;

end.
