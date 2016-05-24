unit googlecalendar;
{
  This is the file COPYING.FPC, it applies to the Free Pascal Run-Time Library 
  (RTL) and packages (packages) distributed by members of the Free Pascal 
  Development Team.
  
  The source code of the Free Pascal Runtime Libraries and packages are 
  distributed under the Library GNU General Public License 
  (see the file COPYING) with the following modification:
  
  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,
  and to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a module
  which is not derived from or based on this library. If you modify this
  library, you may extend this exception to your version of the library, but you are
  not obligated to do so. If you do not wish to do so, delete this exception
  statement from your version.
  
  If you didn't receive a copy of the file COPYING, contact:
        Free Software Foundation
        675 Mass Ave
        Cambridge, MA  02139
        USA
  
}
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  //
  TAcl = class;
  TAclArray = Array of TAcl;
  TAclitems = class;
  TAclitemsArray = Array of TAclitems;
  TAclRule = class;
  TAclRuleArray = Array of TAclRule;
  TAclRulescope = class;
  TAclRulescopeArray = Array of TAclRulescope;
  TCalendar = class;
  TCalendarArray = Array of TCalendar;
  TCalendarList = class;
  TCalendarListArray = Array of TCalendarList;
  TCalendarListitems = class;
  TCalendarListitemsArray = Array of TCalendarListitems;
  TCalendarListEntry = class;
  TCalendarListEntryArray = Array of TCalendarListEntry;
  TCalendarListEntrydefaultReminders = class;
  TCalendarListEntrydefaultRemindersArray = Array of TCalendarListEntrydefaultReminders;
  TCalendarListEntrynotificationSettings = class;
  TCalendarListEntrynotificationSettingsArray = Array of TCalendarListEntrynotificationSettings;
  TCalendarListEntrynotificationSettingsnotifications = class;
  TCalendarListEntrynotificationSettingsnotificationsArray = Array of TCalendarListEntrynotificationSettingsnotifications;
  TCalendarNotification = class;
  TCalendarNotificationArray = Array of TCalendarNotification;
  TChannel = class;
  TChannelArray = Array of TChannel;
  TChannelparams = class;
  TChannelparamsArray = Array of TChannelparams;
  TColorDefinition = class;
  TColorDefinitionArray = Array of TColorDefinition;
  TColors = class;
  TColorsArray = Array of TColors;
  TColorscalendar = class;
  TColorscalendarArray = Array of TColorscalendar;
  TColorsevent = class;
  TColorseventArray = Array of TColorsevent;
  TError = class;
  TErrorArray = Array of TError;
  TEvent = class;
  TEventArray = Array of TEvent;
  TEventattendees = class;
  TEventattendeesArray = Array of TEventattendees;
  TEventcreator = class;
  TEventcreatorArray = Array of TEventcreator;
  TEventextendedProperties = class;
  TEventextendedPropertiesArray = Array of TEventextendedProperties;
  TEventextendedPropertiesprivate = class;
  TEventextendedPropertiesprivateArray = Array of TEventextendedPropertiesprivate;
  TEventextendedPropertiesshared = class;
  TEventextendedPropertiessharedArray = Array of TEventextendedPropertiesshared;
  TEventgadget = class;
  TEventgadgetArray = Array of TEventgadget;
  TEventgadgetpreferences = class;
  TEventgadgetpreferencesArray = Array of TEventgadgetpreferences;
  TEventorganizer = class;
  TEventorganizerArray = Array of TEventorganizer;
  TEventrecurrence = class;
  TEventrecurrenceArray = Array of TEventrecurrence;
  TEventreminders = class;
  TEventremindersArray = Array of TEventreminders;
  TEventremindersoverrides = class;
  TEventremindersoverridesArray = Array of TEventremindersoverrides;
  TEventsource = class;
  TEventsourceArray = Array of TEventsource;
  TEventAttachment = class;
  TEventAttachmentArray = Array of TEventAttachment;
  TEventAttendee = class;
  TEventAttendeeArray = Array of TEventAttendee;
  TEventDateTime = class;
  TEventDateTimeArray = Array of TEventDateTime;
  TEventReminder = class;
  TEventReminderArray = Array of TEventReminder;
  TEvents = class;
  TEventsArray = Array of TEvents;
  TEventsdefaultReminders = class;
  TEventsdefaultRemindersArray = Array of TEventsdefaultReminders;
  TEventsitems = class;
  TEventsitemsArray = Array of TEventsitems;
  TFreeBusyCalendar = class;
  TFreeBusyCalendarArray = Array of TFreeBusyCalendar;
  TFreeBusyCalendarbusy = class;
  TFreeBusyCalendarbusyArray = Array of TFreeBusyCalendarbusy;
  TFreeBusyCalendarerrors = class;
  TFreeBusyCalendarerrorsArray = Array of TFreeBusyCalendarerrors;
  TFreeBusyGroup = class;
  TFreeBusyGroupArray = Array of TFreeBusyGroup;
  TFreeBusyGroupcalendars = class;
  TFreeBusyGroupcalendarsArray = Array of TFreeBusyGroupcalendars;
  TFreeBusyGrouperrors = class;
  TFreeBusyGrouperrorsArray = Array of TFreeBusyGrouperrors;
  TFreeBusyRequest = class;
  TFreeBusyRequestArray = Array of TFreeBusyRequest;
  TFreeBusyRequestitems = class;
  TFreeBusyRequestitemsArray = Array of TFreeBusyRequestitems;
  TFreeBusyRequestItem = class;
  TFreeBusyRequestItemArray = Array of TFreeBusyRequestItem;
  TFreeBusyResponse = class;
  TFreeBusyResponseArray = Array of TFreeBusyResponse;
  TFreeBusyResponsecalendars = class;
  TFreeBusyResponsecalendarsArray = Array of TFreeBusyResponsecalendars;
  TFreeBusyResponsegroups = class;
  TFreeBusyResponsegroupsArray = Array of TFreeBusyResponsegroups;
  TSetting = class;
  TSettingArray = Array of TSetting;
  TSettings = class;
  TSettingsArray = Array of TSettings;
  TSettingsitems = class;
  TSettingsitemsArray = Array of TSettingsitems;
  TTimePeriod = class;
  TTimePeriodArray = Array of TTimePeriod;
  
  { --------------------------------------------------------------------
    TAcl
    --------------------------------------------------------------------}
  
  TAcl = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TAclitems;
    Fkind : string;
    FnextPageToken : string;
    FnextSyncToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TAclitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextSyncToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TAclitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property nextSyncToken : string Index 32 Read FnextSyncToken Write SetnextSyncToken;
  end;
  TAclClass = Class of TAcl;
  
  { --------------------------------------------------------------------
    TAclitems
    --------------------------------------------------------------------}
  
  TAclitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAclitemsClass = Class of TAclitems;
  
  { --------------------------------------------------------------------
    TAclRule
    --------------------------------------------------------------------}
  
  TAclRule = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fid : string;
    Fkind : string;
    Frole : string;
    Fscope : TAclRulescope;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setrole(AIndex : Integer; AValue : string); virtual;
    Procedure Setscope(AIndex : Integer; AValue : TAclRulescope); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property role : string Index 24 Read Frole Write Setrole;
    Property scope : TAclRulescope Index 32 Read Fscope Write Setscope;
  end;
  TAclRuleClass = Class of TAclRule;
  
  { --------------------------------------------------------------------
    TAclRulescope
    --------------------------------------------------------------------}
  
  TAclRulescope = Class(TGoogleBaseObject)
  Private
    F_type : string;
    Fvalue : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _type : string Index 0 Read F_type Write Set_type;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TAclRulescopeClass = Class of TAclRulescope;
  
  { --------------------------------------------------------------------
    TCalendar
    --------------------------------------------------------------------}
  
  TCalendar = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    Fetag : string;
    Fid : string;
    Fkind : string;
    Flocation : string;
    Fsummary : string;
    FtimeZone : string;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : string); virtual;
    Procedure Setsummary(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeZone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property etag : string Index 8 Read Fetag Write Setetag;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property location : string Index 32 Read Flocation Write Setlocation;
    Property summary : string Index 40 Read Fsummary Write Setsummary;
    Property timeZone : string Index 48 Read FtimeZone Write SettimeZone;
  end;
  TCalendarClass = Class of TCalendar;
  
  { --------------------------------------------------------------------
    TCalendarList
    --------------------------------------------------------------------}
  
  TCalendarList = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TCalendarListitems;
    Fkind : string;
    FnextPageToken : string;
    FnextSyncToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TCalendarListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextSyncToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TCalendarListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property nextSyncToken : string Index 32 Read FnextSyncToken Write SetnextSyncToken;
  end;
  TCalendarListClass = Class of TCalendarList;
  
  { --------------------------------------------------------------------
    TCalendarListitems
    --------------------------------------------------------------------}
  
  TCalendarListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCalendarListitemsClass = Class of TCalendarListitems;
  
  { --------------------------------------------------------------------
    TCalendarListEntry
    --------------------------------------------------------------------}
  
  TCalendarListEntry = Class(TGoogleBaseObject)
  Private
    FaccessRole : string;
    FbackgroundColor : string;
    FcolorId : string;
    FdefaultReminders : TCalendarListEntrydefaultReminders;
    Fdeleted : boolean;
    Fdescription : string;
    Fetag : string;
    FforegroundColor : string;
    Fhidden : boolean;
    Fid : string;
    Fkind : string;
    Flocation : string;
    FnotificationSettings : TCalendarListEntrynotificationSettings;
    Fprimary : boolean;
    Fselected : boolean;
    Fsummary : string;
    FsummaryOverride : string;
    FtimeZone : string;
  Protected
    //Property setters
    Procedure SetaccessRole(AIndex : Integer; AValue : string); virtual;
    Procedure SetbackgroundColor(AIndex : Integer; AValue : string); virtual;
    Procedure SetcolorId(AIndex : Integer; AValue : string); virtual;
    Procedure SetdefaultReminders(AIndex : Integer; AValue : TCalendarListEntrydefaultReminders); virtual;
    Procedure Setdeleted(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure SetforegroundColor(AIndex : Integer; AValue : string); virtual;
    Procedure Sethidden(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : string); virtual;
    Procedure SetnotificationSettings(AIndex : Integer; AValue : TCalendarListEntrynotificationSettings); virtual;
    Procedure Setprimary(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setselected(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setsummary(AIndex : Integer; AValue : string); virtual;
    Procedure SetsummaryOverride(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeZone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accessRole : string Index 0 Read FaccessRole Write SetaccessRole;
    Property backgroundColor : string Index 8 Read FbackgroundColor Write SetbackgroundColor;
    Property colorId : string Index 16 Read FcolorId Write SetcolorId;
    Property defaultReminders : TCalendarListEntrydefaultReminders Index 24 Read FdefaultReminders Write SetdefaultReminders;
    Property deleted : boolean Index 32 Read Fdeleted Write Setdeleted;
    Property description : string Index 40 Read Fdescription Write Setdescription;
    Property etag : string Index 48 Read Fetag Write Setetag;
    Property foregroundColor : string Index 56 Read FforegroundColor Write SetforegroundColor;
    Property hidden : boolean Index 64 Read Fhidden Write Sethidden;
    Property id : string Index 72 Read Fid Write Setid;
    Property kind : string Index 80 Read Fkind Write Setkind;
    Property location : string Index 88 Read Flocation Write Setlocation;
    Property notificationSettings : TCalendarListEntrynotificationSettings Index 96 Read FnotificationSettings Write SetnotificationSettings;
    Property primary : boolean Index 104 Read Fprimary Write Setprimary;
    Property selected : boolean Index 112 Read Fselected Write Setselected;
    Property summary : string Index 120 Read Fsummary Write Setsummary;
    Property summaryOverride : string Index 128 Read FsummaryOverride Write SetsummaryOverride;
    Property timeZone : string Index 136 Read FtimeZone Write SettimeZone;
  end;
  TCalendarListEntryClass = Class of TCalendarListEntry;
  
  { --------------------------------------------------------------------
    TCalendarListEntrydefaultReminders
    --------------------------------------------------------------------}
  
  TCalendarListEntrydefaultReminders = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCalendarListEntrydefaultRemindersClass = Class of TCalendarListEntrydefaultReminders;
  
  { --------------------------------------------------------------------
    TCalendarListEntrynotificationSettings
    --------------------------------------------------------------------}
  
  TCalendarListEntrynotificationSettings = Class(TGoogleBaseObject)
  Private
    Fnotifications : TCalendarListEntrynotificationSettingsnotifications;
  Protected
    //Property setters
    Procedure Setnotifications(AIndex : Integer; AValue : TCalendarListEntrynotificationSettingsnotifications); virtual;
  Public
  Published
    Property notifications : TCalendarListEntrynotificationSettingsnotifications Index 0 Read Fnotifications Write Setnotifications;
  end;
  TCalendarListEntrynotificationSettingsClass = Class of TCalendarListEntrynotificationSettings;
  
  { --------------------------------------------------------------------
    TCalendarListEntrynotificationSettingsnotifications
    --------------------------------------------------------------------}
  
  TCalendarListEntrynotificationSettingsnotifications = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCalendarListEntrynotificationSettingsnotificationsClass = Class of TCalendarListEntrynotificationSettingsnotifications;
  
  { --------------------------------------------------------------------
    TCalendarNotification
    --------------------------------------------------------------------}
  
  TCalendarNotification = Class(TGoogleBaseObject)
  Private
    Fmethod : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setmethod(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property method : string Index 0 Read Fmethod Write Setmethod;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TCalendarNotificationClass = Class of TCalendarNotification;
  
  { --------------------------------------------------------------------
    TChannel
    --------------------------------------------------------------------}
  
  TChannel = Class(TGoogleBaseObject)
  Private
    Faddress : string;
    Fexpiration : string;
    Fid : string;
    Fkind : string;
    Fparams : TChannelparams;
    Fpayload : boolean;
    FresourceId : string;
    FresourceUri : string;
    Ftoken : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setaddress(AIndex : Integer; AValue : string); virtual;
    Procedure Setexpiration(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setparams(AIndex : Integer; AValue : TChannelparams); virtual;
    Procedure Setpayload(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetresourceId(AIndex : Integer; AValue : string); virtual;
    Procedure SetresourceUri(AIndex : Integer; AValue : string); virtual;
    Procedure Settoken(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property address : string Index 0 Read Faddress Write Setaddress;
    Property expiration : string Index 8 Read Fexpiration Write Setexpiration;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property params : TChannelparams Index 32 Read Fparams Write Setparams;
    Property payload : boolean Index 40 Read Fpayload Write Setpayload;
    Property resourceId : string Index 48 Read FresourceId Write SetresourceId;
    Property resourceUri : string Index 56 Read FresourceUri Write SetresourceUri;
    Property token : string Index 64 Read Ftoken Write Settoken;
    Property _type : string Index 72 Read F_type Write Set_type;
  end;
  TChannelClass = Class of TChannel;
  
  { --------------------------------------------------------------------
    TChannelparams
    --------------------------------------------------------------------}
  
  TChannelparams = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TChannelparamsClass = Class of TChannelparams;
  
  { --------------------------------------------------------------------
    TColorDefinition
    --------------------------------------------------------------------}
  
  TColorDefinition = Class(TGoogleBaseObject)
  Private
    Fbackground : string;
    Fforeground : string;
  Protected
    //Property setters
    Procedure Setbackground(AIndex : Integer; AValue : string); virtual;
    Procedure Setforeground(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property background : string Index 0 Read Fbackground Write Setbackground;
    Property foreground : string Index 8 Read Fforeground Write Setforeground;
  end;
  TColorDefinitionClass = Class of TColorDefinition;
  
  { --------------------------------------------------------------------
    TColors
    --------------------------------------------------------------------}
  
  TColors = Class(TGoogleBaseObject)
  Private
    Fcalendar : TColorscalendar;
    Fevent : TColorsevent;
    Fkind : string;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure Setcalendar(AIndex : Integer; AValue : TColorscalendar); virtual;
    Procedure Setevent(AIndex : Integer; AValue : TColorsevent); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property calendar : TColorscalendar Index 0 Read Fcalendar Write Setcalendar;
    Property event : TColorsevent Index 8 Read Fevent Write Setevent;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property updated : TDatetime Index 24 Read Fupdated Write Setupdated;
  end;
  TColorsClass = Class of TColors;
  
  { --------------------------------------------------------------------
    TColorscalendar
    --------------------------------------------------------------------}
  
  TColorscalendar = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TColorscalendarClass = Class of TColorscalendar;
  
  { --------------------------------------------------------------------
    TColorsevent
    --------------------------------------------------------------------}
  
  TColorsevent = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TColorseventClass = Class of TColorsevent;
  
  { --------------------------------------------------------------------
    TError
    --------------------------------------------------------------------}
  
  TError = Class(TGoogleBaseObject)
  Private
    Fdomain : string;
    Freason : string;
  Protected
    //Property setters
    Procedure Setdomain(AIndex : Integer; AValue : string); virtual;
    Procedure Setreason(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property domain : string Index 0 Read Fdomain Write Setdomain;
    Property reason : string Index 8 Read Freason Write Setreason;
  end;
  TErrorClass = Class of TError;
  
  { --------------------------------------------------------------------
    TEvent
    --------------------------------------------------------------------}
  
  TEvent = Class(TGoogleBaseObject)
  Private
    FanyoneCanAddSelf : boolean;
    Fattendees : TEventattendees;
    FattendeesOmitted : boolean;
    FcolorId : string;
    Fcreated : TDatetime;
    Fcreator : TEventcreator;
    Fdescription : string;
    F_end : TEventDateTime;
    FendTimeUnspecified : boolean;
    Fetag : string;
    FextendedProperties : TEventextendedProperties;
    Fgadget : TEventgadget;
    FguestsCanInviteOthers : boolean;
    FguestsCanModify : boolean;
    FguestsCanSeeOtherGuests : boolean;
    FhangoutLink : string;
    FhtmlLink : string;
    FiCalUID : string;
    Fid : string;
    Fkind : string;
    Flocation : string;
    Flocked : boolean;
    Forganizer : TEventorganizer;
    ForiginalStartTime : TEventDateTime;
    FprivateCopy : boolean;
    Frecurrence : TEventrecurrence;
    FrecurringEventId : string;
    Freminders : TEventreminders;
    Fsequence : integer;
    Fsource : TEventsource;
    Fstart : TEventDateTime;
    Fstatus : string;
    Fsummary : string;
    Ftransparency : string;
    Fupdated : TDatetime;
    Fvisibility : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetanyoneCanAddSelf(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setattendees(AIndex : Integer; AValue : TEventattendees); virtual;
    Procedure SetattendeesOmitted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcolorId(AIndex : Integer; AValue : string); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setcreator(AIndex : Integer; AValue : TEventcreator); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Set_end(AIndex : Integer; AValue : TEventDateTime); virtual;
    Procedure SetendTimeUnspecified(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure SetextendedProperties(AIndex : Integer; AValue : TEventextendedProperties); virtual;
    Procedure Setgadget(AIndex : Integer; AValue : TEventgadget); virtual;
    Procedure SetguestsCanInviteOthers(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetguestsCanModify(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetguestsCanSeeOtherGuests(AIndex : Integer; AValue : boolean); virtual;
    Procedure SethangoutLink(AIndex : Integer; AValue : string); virtual;
    Procedure SethtmlLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetiCalUID(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocked(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setorganizer(AIndex : Integer; AValue : TEventorganizer); virtual;
    Procedure SetoriginalStartTime(AIndex : Integer; AValue : TEventDateTime); virtual;
    Procedure SetprivateCopy(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setrecurrence(AIndex : Integer; AValue : TEventrecurrence); virtual;
    Procedure SetrecurringEventId(AIndex : Integer; AValue : string); virtual;
    Procedure Setreminders(AIndex : Integer; AValue : TEventreminders); virtual;
    Procedure Setsequence(AIndex : Integer; AValue : integer); virtual;
    Procedure Setsource(AIndex : Integer; AValue : TEventsource); virtual;
    Procedure Setstart(AIndex : Integer; AValue : TEventDateTime); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure Setsummary(AIndex : Integer; AValue : string); virtual;
    Procedure Settransparency(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setvisibility(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property anyoneCanAddSelf : boolean Index 0 Read FanyoneCanAddSelf Write SetanyoneCanAddSelf;
    Property attendees : TEventattendees Index 8 Read Fattendees Write Setattendees;
    Property attendeesOmitted : boolean Index 16 Read FattendeesOmitted Write SetattendeesOmitted;
    Property colorId : string Index 24 Read FcolorId Write SetcolorId;
    Property created : TDatetime Index 32 Read Fcreated Write Setcreated;
    Property creator : TEventcreator Index 40 Read Fcreator Write Setcreator;
    Property description : string Index 48 Read Fdescription Write Setdescription;
    Property _end : TEventDateTime Index 56 Read F_end Write Set_end;
    Property endTimeUnspecified : boolean Index 64 Read FendTimeUnspecified Write SetendTimeUnspecified;
    Property etag : string Index 72 Read Fetag Write Setetag;
    Property extendedProperties : TEventextendedProperties Index 80 Read FextendedProperties Write SetextendedProperties;
    Property gadget : TEventgadget Index 88 Read Fgadget Write Setgadget;
    Property guestsCanInviteOthers : boolean Index 96 Read FguestsCanInviteOthers Write SetguestsCanInviteOthers;
    Property guestsCanModify : boolean Index 104 Read FguestsCanModify Write SetguestsCanModify;
    Property guestsCanSeeOtherGuests : boolean Index 112 Read FguestsCanSeeOtherGuests Write SetguestsCanSeeOtherGuests;
    Property hangoutLink : string Index 120 Read FhangoutLink Write SethangoutLink;
    Property htmlLink : string Index 128 Read FhtmlLink Write SethtmlLink;
    Property iCalUID : string Index 136 Read FiCalUID Write SetiCalUID;
    Property id : string Index 144 Read Fid Write Setid;
    Property kind : string Index 152 Read Fkind Write Setkind;
    Property location : string Index 160 Read Flocation Write Setlocation;
    Property locked : boolean Index 168 Read Flocked Write Setlocked;
    Property organizer : TEventorganizer Index 176 Read Forganizer Write Setorganizer;
    Property originalStartTime : TEventDateTime Index 184 Read ForiginalStartTime Write SetoriginalStartTime;
    Property privateCopy : boolean Index 192 Read FprivateCopy Write SetprivateCopy;
    Property recurrence : TEventrecurrence Index 200 Read Frecurrence Write Setrecurrence;
    Property recurringEventId : string Index 208 Read FrecurringEventId Write SetrecurringEventId;
    Property reminders : TEventreminders Index 216 Read Freminders Write Setreminders;
    Property sequence : integer Index 224 Read Fsequence Write Setsequence;
    Property source : TEventsource Index 232 Read Fsource Write Setsource;
    Property start : TEventDateTime Index 240 Read Fstart Write Setstart;
    Property status : string Index 248 Read Fstatus Write Setstatus;
    Property summary : string Index 256 Read Fsummary Write Setsummary;
    Property transparency : string Index 264 Read Ftransparency Write Settransparency;
    Property updated : TDatetime Index 272 Read Fupdated Write Setupdated;
    Property visibility : string Index 280 Read Fvisibility Write Setvisibility;
  end;
  TEventClass = Class of TEvent;
  
  { --------------------------------------------------------------------
    TEventattendees
    --------------------------------------------------------------------}
  
  TEventattendees = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEventattendeesClass = Class of TEventattendees;
  
  { --------------------------------------------------------------------
    TEventcreator
    --------------------------------------------------------------------}
  
  TEventcreator = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    Femail : string;
    Fid : string;
    F_self : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Set_self(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property email : string Index 8 Read Femail Write Setemail;
    Property id : string Index 16 Read Fid Write Setid;
    Property _self : boolean Index 24 Read F_self Write Set_self;
  end;
  TEventcreatorClass = Class of TEventcreator;
  
  { --------------------------------------------------------------------
    TEventextendedProperties
    --------------------------------------------------------------------}
  
  TEventextendedProperties = Class(TGoogleBaseObject)
  Private
    F_private : TEventextendedPropertiesprivate;
    Fshared : TEventextendedPropertiesshared;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_private(AIndex : Integer; AValue : TEventextendedPropertiesprivate); virtual;
    Procedure Setshared(AIndex : Integer; AValue : TEventextendedPropertiesshared); virtual;
  Public
  Published
    Property _private : TEventextendedPropertiesprivate Index 0 Read F_private Write Set_private;
    Property shared : TEventextendedPropertiesshared Index 8 Read Fshared Write Setshared;
  end;
  TEventextendedPropertiesClass = Class of TEventextendedProperties;
  
  { --------------------------------------------------------------------
    TEventextendedPropertiesprivate
    --------------------------------------------------------------------}
  
  TEventextendedPropertiesprivate = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TEventextendedPropertiesprivateClass = Class of TEventextendedPropertiesprivate;
  
  { --------------------------------------------------------------------
    TEventextendedPropertiesshared
    --------------------------------------------------------------------}
  
  TEventextendedPropertiesshared = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TEventextendedPropertiessharedClass = Class of TEventextendedPropertiesshared;
  
  { --------------------------------------------------------------------
    TEventgadget
    --------------------------------------------------------------------}
  
  TEventgadget = Class(TGoogleBaseObject)
  Private
    Fdisplay : string;
    Fheight : integer;
    FiconLink : string;
    Flink : string;
    Fpreferences : TEventgadgetpreferences;
    Ftitle : string;
    F_type : string;
    Fwidth : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdisplay(AIndex : Integer; AValue : string); virtual;
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure SeticonLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setlink(AIndex : Integer; AValue : string); virtual;
    Procedure Setpreferences(AIndex : Integer; AValue : TEventgadgetpreferences); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property display : string Index 0 Read Fdisplay Write Setdisplay;
    Property height : integer Index 8 Read Fheight Write Setheight;
    Property iconLink : string Index 16 Read FiconLink Write SeticonLink;
    Property link : string Index 24 Read Flink Write Setlink;
    Property preferences : TEventgadgetpreferences Index 32 Read Fpreferences Write Setpreferences;
    Property title : string Index 40 Read Ftitle Write Settitle;
    Property _type : string Index 48 Read F_type Write Set_type;
    Property width : integer Index 56 Read Fwidth Write Setwidth;
  end;
  TEventgadgetClass = Class of TEventgadget;
  
  { --------------------------------------------------------------------
    TEventgadgetpreferences
    --------------------------------------------------------------------}
  
  TEventgadgetpreferences = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TEventgadgetpreferencesClass = Class of TEventgadgetpreferences;
  
  { --------------------------------------------------------------------
    TEventorganizer
    --------------------------------------------------------------------}
  
  TEventorganizer = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    Femail : string;
    Fid : string;
    F_self : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Set_self(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property email : string Index 8 Read Femail Write Setemail;
    Property id : string Index 16 Read Fid Write Setid;
    Property _self : boolean Index 24 Read F_self Write Set_self;
  end;
  TEventorganizerClass = Class of TEventorganizer;
  
  { --------------------------------------------------------------------
    TEventrecurrence
    --------------------------------------------------------------------}
  
  TEventrecurrence = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEventrecurrenceClass = Class of TEventrecurrence;
  
  { --------------------------------------------------------------------
    TEventreminders
    --------------------------------------------------------------------}
  
  TEventreminders = Class(TGoogleBaseObject)
  Private
    Foverrides : TEventremindersoverrides;
    FuseDefault : boolean;
  Protected
    //Property setters
    Procedure Setoverrides(AIndex : Integer; AValue : TEventremindersoverrides); virtual;
    Procedure SetuseDefault(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property overrides : TEventremindersoverrides Index 0 Read Foverrides Write Setoverrides;
    Property useDefault : boolean Index 8 Read FuseDefault Write SetuseDefault;
  end;
  TEventremindersClass = Class of TEventreminders;
  
  { --------------------------------------------------------------------
    TEventremindersoverrides
    --------------------------------------------------------------------}
  
  TEventremindersoverrides = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEventremindersoverridesClass = Class of TEventremindersoverrides;
  
  { --------------------------------------------------------------------
    TEventsource
    --------------------------------------------------------------------}
  
  TEventsource = Class(TGoogleBaseObject)
  Private
    Ftitle : string;
    Furl : string;
  Protected
    //Property setters
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property title : string Index 0 Read Ftitle Write Settitle;
    Property url : string Index 8 Read Furl Write Seturl;
  end;
  TEventsourceClass = Class of TEventsource;
  
  { --------------------------------------------------------------------
    TEventAttachment
    --------------------------------------------------------------------}
  
  TEventAttachment = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEventAttachmentClass = Class of TEventAttachment;
  
  { --------------------------------------------------------------------
    TEventAttendee
    --------------------------------------------------------------------}
  
  TEventAttendee = Class(TGoogleBaseObject)
  Private
    FadditionalGuests : integer;
    Fcomment : string;
    FdisplayName : string;
    Femail : string;
    Fid : string;
    Foptional : boolean;
    Forganizer : boolean;
    Fresource : boolean;
    FresponseStatus : string;
    F_self : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetadditionalGuests(AIndex : Integer; AValue : integer); virtual;
    Procedure Setcomment(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setoptional(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setorganizer(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setresource(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetresponseStatus(AIndex : Integer; AValue : string); virtual;
    Procedure Set_self(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property additionalGuests : integer Index 0 Read FadditionalGuests Write SetadditionalGuests;
    Property comment : string Index 8 Read Fcomment Write Setcomment;
    Property displayName : string Index 16 Read FdisplayName Write SetdisplayName;
    Property email : string Index 24 Read Femail Write Setemail;
    Property id : string Index 32 Read Fid Write Setid;
    Property optional : boolean Index 40 Read Foptional Write Setoptional;
    Property organizer : boolean Index 48 Read Forganizer Write Setorganizer;
    Property resource : boolean Index 56 Read Fresource Write Setresource;
    Property responseStatus : string Index 64 Read FresponseStatus Write SetresponseStatus;
    Property _self : boolean Index 72 Read F_self Write Set_self;
  end;
  TEventAttendeeClass = Class of TEventAttendee;
  
  { --------------------------------------------------------------------
    TEventDateTime
    --------------------------------------------------------------------}
  
  TEventDateTime = Class(TGoogleBaseObject)
  Private
    Fdate : TDate;
    FdateTime : TDatetime;
    FtimeZone : string;
  Protected
    //Property setters
    Procedure Setdate(AIndex : Integer; AValue : TDate); virtual;
    Procedure SetdateTime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SettimeZone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property date : TDate Index 0 Read Fdate Write Setdate;
    Property dateTime : TDatetime Index 8 Read FdateTime Write SetdateTime;
    Property timeZone : string Index 16 Read FtimeZone Write SettimeZone;
  end;
  TEventDateTimeClass = Class of TEventDateTime;
  
  { --------------------------------------------------------------------
    TEventReminder
    --------------------------------------------------------------------}
  
  TEventReminder = Class(TGoogleBaseObject)
  Private
    Fmethod : string;
    Fminutes : integer;
  Protected
    //Property setters
    Procedure Setmethod(AIndex : Integer; AValue : string); virtual;
    Procedure Setminutes(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property method : string Index 0 Read Fmethod Write Setmethod;
    Property minutes : integer Index 8 Read Fminutes Write Setminutes;
  end;
  TEventReminderClass = Class of TEventReminder;
  
  { --------------------------------------------------------------------
    TEvents
    --------------------------------------------------------------------}
  
  TEvents = Class(TGoogleBaseObject)
  Private
    FaccessRole : string;
    FdefaultReminders : TEventsdefaultReminders;
    Fdescription : string;
    Fetag : string;
    Fitems : TEventsitems;
    Fkind : string;
    FnextPageToken : string;
    FnextSyncToken : string;
    Fsummary : string;
    FtimeZone : string;
    Fupdated : TDatetime;
  Protected
    //Property setters
    Procedure SetaccessRole(AIndex : Integer; AValue : string); virtual;
    Procedure SetdefaultReminders(AIndex : Integer; AValue : TEventsdefaultReminders); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TEventsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextSyncToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setsummary(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeZone(AIndex : Integer; AValue : string); virtual;
    Procedure Setupdated(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property accessRole : string Index 0 Read FaccessRole Write SetaccessRole;
    Property defaultReminders : TEventsdefaultReminders Index 8 Read FdefaultReminders Write SetdefaultReminders;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property etag : string Index 24 Read Fetag Write Setetag;
    Property items : TEventsitems Index 32 Read Fitems Write Setitems;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property nextPageToken : string Index 48 Read FnextPageToken Write SetnextPageToken;
    Property nextSyncToken : string Index 56 Read FnextSyncToken Write SetnextSyncToken;
    Property summary : string Index 64 Read Fsummary Write Setsummary;
    Property timeZone : string Index 72 Read FtimeZone Write SettimeZone;
    Property updated : TDatetime Index 80 Read Fupdated Write Setupdated;
  end;
  TEventsClass = Class of TEvents;
  
  { --------------------------------------------------------------------
    TEventsdefaultReminders
    --------------------------------------------------------------------}
  
  TEventsdefaultReminders = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEventsdefaultRemindersClass = Class of TEventsdefaultReminders;
  
  { --------------------------------------------------------------------
    TEventsitems
    --------------------------------------------------------------------}
  
  TEventsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEventsitemsClass = Class of TEventsitems;
  
  { --------------------------------------------------------------------
    TFreeBusyCalendar
    --------------------------------------------------------------------}
  
  TFreeBusyCalendar = Class(TGoogleBaseObject)
  Private
    Fbusy : TFreeBusyCalendarbusy;
    Ferrors : TFreeBusyCalendarerrors;
  Protected
    //Property setters
    Procedure Setbusy(AIndex : Integer; AValue : TFreeBusyCalendarbusy); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TFreeBusyCalendarerrors); virtual;
  Public
  Published
    Property busy : TFreeBusyCalendarbusy Index 0 Read Fbusy Write Setbusy;
    Property errors : TFreeBusyCalendarerrors Index 8 Read Ferrors Write Seterrors;
  end;
  TFreeBusyCalendarClass = Class of TFreeBusyCalendar;
  
  { --------------------------------------------------------------------
    TFreeBusyCalendarbusy
    --------------------------------------------------------------------}
  
  TFreeBusyCalendarbusy = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFreeBusyCalendarbusyClass = Class of TFreeBusyCalendarbusy;
  
  { --------------------------------------------------------------------
    TFreeBusyCalendarerrors
    --------------------------------------------------------------------}
  
  TFreeBusyCalendarerrors = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFreeBusyCalendarerrorsClass = Class of TFreeBusyCalendarerrors;
  
  { --------------------------------------------------------------------
    TFreeBusyGroup
    --------------------------------------------------------------------}
  
  TFreeBusyGroup = Class(TGoogleBaseObject)
  Private
    Fcalendars : TFreeBusyGroupcalendars;
    Ferrors : TFreeBusyGrouperrors;
  Protected
    //Property setters
    Procedure Setcalendars(AIndex : Integer; AValue : TFreeBusyGroupcalendars); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TFreeBusyGrouperrors); virtual;
  Public
  Published
    Property calendars : TFreeBusyGroupcalendars Index 0 Read Fcalendars Write Setcalendars;
    Property errors : TFreeBusyGrouperrors Index 8 Read Ferrors Write Seterrors;
  end;
  TFreeBusyGroupClass = Class of TFreeBusyGroup;
  
  { --------------------------------------------------------------------
    TFreeBusyGroupcalendars
    --------------------------------------------------------------------}
  
  TFreeBusyGroupcalendars = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFreeBusyGroupcalendarsClass = Class of TFreeBusyGroupcalendars;
  
  { --------------------------------------------------------------------
    TFreeBusyGrouperrors
    --------------------------------------------------------------------}
  
  TFreeBusyGrouperrors = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFreeBusyGrouperrorsClass = Class of TFreeBusyGrouperrors;
  
  { --------------------------------------------------------------------
    TFreeBusyRequest
    --------------------------------------------------------------------}
  
  TFreeBusyRequest = Class(TGoogleBaseObject)
  Private
    FcalendarExpansionMax : integer;
    FgroupExpansionMax : integer;
    Fitems : TFreeBusyRequestitems;
    FtimeMax : TDatetime;
    FtimeMin : TDatetime;
    FtimeZone : string;
  Protected
    //Property setters
    Procedure SetcalendarExpansionMax(AIndex : Integer; AValue : integer); virtual;
    Procedure SetgroupExpansionMax(AIndex : Integer; AValue : integer); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TFreeBusyRequestitems); virtual;
    Procedure SettimeMax(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SettimeMin(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SettimeZone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property calendarExpansionMax : integer Index 0 Read FcalendarExpansionMax Write SetcalendarExpansionMax;
    Property groupExpansionMax : integer Index 8 Read FgroupExpansionMax Write SetgroupExpansionMax;
    Property items : TFreeBusyRequestitems Index 16 Read Fitems Write Setitems;
    Property timeMax : TDatetime Index 24 Read FtimeMax Write SettimeMax;
    Property timeMin : TDatetime Index 32 Read FtimeMin Write SettimeMin;
    Property timeZone : string Index 40 Read FtimeZone Write SettimeZone;
  end;
  TFreeBusyRequestClass = Class of TFreeBusyRequest;
  
  { --------------------------------------------------------------------
    TFreeBusyRequestitems
    --------------------------------------------------------------------}
  
  TFreeBusyRequestitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFreeBusyRequestitemsClass = Class of TFreeBusyRequestitems;
  
  { --------------------------------------------------------------------
    TFreeBusyRequestItem
    --------------------------------------------------------------------}
  
  TFreeBusyRequestItem = Class(TGoogleBaseObject)
  Private
    Fid : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
  end;
  TFreeBusyRequestItemClass = Class of TFreeBusyRequestItem;
  
  { --------------------------------------------------------------------
    TFreeBusyResponse
    --------------------------------------------------------------------}
  
  TFreeBusyResponse = Class(TGoogleBaseObject)
  Private
    Fcalendars : TFreeBusyResponsecalendars;
    Fgroups : TFreeBusyResponsegroups;
    Fkind : string;
    FtimeMax : TDatetime;
    FtimeMin : TDatetime;
  Protected
    //Property setters
    Procedure Setcalendars(AIndex : Integer; AValue : TFreeBusyResponsecalendars); virtual;
    Procedure Setgroups(AIndex : Integer; AValue : TFreeBusyResponsegroups); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SettimeMax(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SettimeMin(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property calendars : TFreeBusyResponsecalendars Index 0 Read Fcalendars Write Setcalendars;
    Property groups : TFreeBusyResponsegroups Index 8 Read Fgroups Write Setgroups;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property timeMax : TDatetime Index 24 Read FtimeMax Write SettimeMax;
    Property timeMin : TDatetime Index 32 Read FtimeMin Write SettimeMin;
  end;
  TFreeBusyResponseClass = Class of TFreeBusyResponse;
  
  { --------------------------------------------------------------------
    TFreeBusyResponsecalendars
    --------------------------------------------------------------------}
  
  TFreeBusyResponsecalendars = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TFreeBusyResponsecalendarsClass = Class of TFreeBusyResponsecalendars;
  
  { --------------------------------------------------------------------
    TFreeBusyResponsegroups
    --------------------------------------------------------------------}
  
  TFreeBusyResponsegroups = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TFreeBusyResponsegroupsClass = Class of TFreeBusyResponsegroups;
  
  { --------------------------------------------------------------------
    TSetting
    --------------------------------------------------------------------}
  
  TSetting = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fid : string;
    Fkind : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property value : string Index 24 Read Fvalue Write Setvalue;
  end;
  TSettingClass = Class of TSetting;
  
  { --------------------------------------------------------------------
    TSettings
    --------------------------------------------------------------------}
  
  TSettings = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TSettingsitems;
    Fkind : string;
    FnextPageToken : string;
    FnextSyncToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TSettingsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextSyncToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TSettingsitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property nextSyncToken : string Index 32 Read FnextSyncToken Write SetnextSyncToken;
  end;
  TSettingsClass = Class of TSettings;
  
  { --------------------------------------------------------------------
    TSettingsitems
    --------------------------------------------------------------------}
  
  TSettingsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSettingsitemsClass = Class of TSettingsitems;
  
  { --------------------------------------------------------------------
    TTimePeriod
    --------------------------------------------------------------------}
  
  TTimePeriod = Class(TGoogleBaseObject)
  Private
    F_end : TDatetime;
    Fstart : TDatetime;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setstart(AIndex : Integer; AValue : TDatetime); virtual;
  Public
  Published
    Property _end : TDatetime Index 0 Read F_end Write Set_end;
    Property start : TDatetime Index 8 Read Fstart Write Setstart;
  end;
  TTimePeriodClass = Class of TTimePeriod;
  
  { --------------------------------------------------------------------
    TAclResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAclResource, method List
  
  TAclListOptions = Record
    maxResults : integer;
    pageToken : string;
    showDeleted : boolean;
    syncToken : string;
  end;
  
  
  //Optional query Options for TAclResource, method Watch
  
  TAclWatchOptions = Record
    maxResults : integer;
    pageToken : string;
    showDeleted : boolean;
    syncToken : string;
  end;
  
  TAclResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(calendarId: string; ruleId: string);
    Function Get(calendarId: string; ruleId: string) : TAclRule;
    Function Insert(calendarId: string; aAclRule : TAclRule) : TAclRule;
    Function List(calendarId: string; AQuery : string  = '') : TAcl;
    Function List(calendarId: string; AQuery : TAcllistOptions) : TAcl;
    Function Patch(calendarId: string; ruleId: string; aAclRule : TAclRule) : TAclRule;
    Function Update(calendarId: string; ruleId: string; aAclRule : TAclRule) : TAclRule;
    Function Watch(calendarId: string; aChannel : TChannel; AQuery : string  = '') : TChannel;
    Function Watch(calendarId: string; aChannel : TChannel; AQuery : TAclwatchOptions) : TChannel;
  end;
  
  
  { --------------------------------------------------------------------
    TCalendarListResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCalendarListResource, method Insert
  
  TCalendarListInsertOptions = Record
    colorRgbFormat : boolean;
  end;
  
  
  //Optional query Options for TCalendarListResource, method List
  
  TCalendarListListOptions = Record
    maxResults : integer;
    minAccessRole : string;
    pageToken : string;
    showDeleted : boolean;
    showHidden : boolean;
    syncToken : string;
  end;
  
  
  //Optional query Options for TCalendarListResource, method Patch
  
  TCalendarListPatchOptions = Record
    colorRgbFormat : boolean;
  end;
  
  
  //Optional query Options for TCalendarListResource, method Update
  
  TCalendarListUpdateOptions = Record
    colorRgbFormat : boolean;
  end;
  
  
  //Optional query Options for TCalendarListResource, method Watch
  
  TCalendarListWatchOptions = Record
    maxResults : integer;
    minAccessRole : string;
    pageToken : string;
    showDeleted : boolean;
    showHidden : boolean;
    syncToken : string;
  end;
  
  TCalendarListResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(calendarId: string);
    Function Get(calendarId: string) : TCalendarListEntry;
    Function Insert(aCalendarListEntry : TCalendarListEntry; AQuery : string  = '') : TCalendarListEntry;
    Function Insert(aCalendarListEntry : TCalendarListEntry; AQuery : TCalendarListinsertOptions) : TCalendarListEntry;
    Function List(AQuery : string  = '') : TCalendarList;
    Function List(AQuery : TCalendarListlistOptions) : TCalendarList;
    Function Patch(calendarId: string; aCalendarListEntry : TCalendarListEntry; AQuery : string  = '') : TCalendarListEntry;
    Function Patch(calendarId: string; aCalendarListEntry : TCalendarListEntry; AQuery : TCalendarListpatchOptions) : TCalendarListEntry;
    Function Update(calendarId: string; aCalendarListEntry : TCalendarListEntry; AQuery : string  = '') : TCalendarListEntry;
    Function Update(calendarId: string; aCalendarListEntry : TCalendarListEntry; AQuery : TCalendarListupdateOptions) : TCalendarListEntry;
    Function Watch(aChannel : TChannel; AQuery : string  = '') : TChannel;
    Function Watch(aChannel : TChannel; AQuery : TCalendarListwatchOptions) : TChannel;
  end;
  
  
  { --------------------------------------------------------------------
    TCalendarsResource
    --------------------------------------------------------------------}
  
  TCalendarsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Clear(calendarId: string);
    Procedure Delete(calendarId: string);
    Function Get(calendarId: string) : TCalendar;
    Function Insert(aCalendar : TCalendar) : TCalendar;
    Function Patch(calendarId: string; aCalendar : TCalendar) : TCalendar;
    Function Update(calendarId: string; aCalendar : TCalendar) : TCalendar;
  end;
  
  
  { --------------------------------------------------------------------
    TChannelsResource
    --------------------------------------------------------------------}
  
  TChannelsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Stop(aChannel : TChannel);
  end;
  
  
  { --------------------------------------------------------------------
    TColorsResource
    --------------------------------------------------------------------}
  
  TColorsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get : TColors;
  end;
  
  
  { --------------------------------------------------------------------
    TEventsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TEventsResource, method Delete
  
  TEventsDeleteOptions = Record
    sendNotifications : boolean;
  end;
  
  
  //Optional query Options for TEventsResource, method Get
  
  TEventsGetOptions = Record
    alwaysIncludeEmail : boolean;
    maxAttendees : integer;
    timeZone : string;
  end;
  
  
  //Optional query Options for TEventsResource, method Insert
  
  TEventsInsertOptions = Record
    maxAttendees : integer;
    sendNotifications : boolean;
  end;
  
  
  //Optional query Options for TEventsResource, method Instances
  
  TEventsInstancesOptions = Record
    alwaysIncludeEmail : boolean;
    maxAttendees : integer;
    maxResults : integer;
    originalStart : string;
    pageToken : string;
    showDeleted : boolean;
    timeMax : TDatetime;
    timeMin : TDatetime;
    timeZone : string;
  end;
  
  
  //Optional query Options for TEventsResource, method List
  
  TEventsListOptions = Record
    alwaysIncludeEmail : boolean;
    iCalUID : string;
    maxAttendees : integer;
    maxResults : integer;
    orderBy : string;
    pageToken : string;
    privateExtendedProperty : string;
    q : string;
    sharedExtendedProperty : string;
    showDeleted : boolean;
    showHiddenInvitations : boolean;
    singleEvents : boolean;
    syncToken : string;
    timeMax : TDatetime;
    timeMin : TDatetime;
    timeZone : string;
    updatedMin : TDatetime;
  end;
  
  
  //Optional query Options for TEventsResource, method Move
  
  TEventsMoveOptions = Record
    destination : string;
    sendNotifications : boolean;
  end;
  
  
  //Optional query Options for TEventsResource, method Patch
  
  TEventsPatchOptions = Record
    alwaysIncludeEmail : boolean;
    maxAttendees : integer;
    sendNotifications : boolean;
  end;
  
  
  //Optional query Options for TEventsResource, method QuickAdd
  
  TEventsQuickAddOptions = Record
    sendNotifications : boolean;
    text : string;
  end;
  
  
  //Optional query Options for TEventsResource, method Update
  
  TEventsUpdateOptions = Record
    alwaysIncludeEmail : boolean;
    maxAttendees : integer;
    sendNotifications : boolean;
  end;
  
  
  //Optional query Options for TEventsResource, method Watch
  
  TEventsWatchOptions = Record
    alwaysIncludeEmail : boolean;
    iCalUID : string;
    maxAttendees : integer;
    maxResults : integer;
    orderBy : string;
    pageToken : string;
    privateExtendedProperty : string;
    q : string;
    sharedExtendedProperty : string;
    showDeleted : boolean;
    showHiddenInvitations : boolean;
    singleEvents : boolean;
    syncToken : string;
    timeMax : TDatetime;
    timeMin : TDatetime;
    timeZone : string;
    updatedMin : TDatetime;
  end;
  
  TEventsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(calendarId: string; eventId: string; AQuery : string  = '');
    Procedure Delete(calendarId: string; eventId: string; AQuery : TEventsdeleteOptions);
    Function Get(calendarId: string; eventId: string; AQuery : string  = '') : TEvent;
    Function Get(calendarId: string; eventId: string; AQuery : TEventsgetOptions) : TEvent;
    Function Import(calendarId: string; aEvent : TEvent) : TEvent;
    Function Insert(calendarId: string; aEvent : TEvent; AQuery : string  = '') : TEvent;
    Function Insert(calendarId: string; aEvent : TEvent; AQuery : TEventsinsertOptions) : TEvent;
    Function Instances(calendarId: string; eventId: string; AQuery : string  = '') : TEvents;
    Function Instances(calendarId: string; eventId: string; AQuery : TEventsinstancesOptions) : TEvents;
    Function List(calendarId: string; AQuery : string  = '') : TEvents;
    Function List(calendarId: string; AQuery : TEventslistOptions) : TEvents;
    Function Move(calendarId: string; eventId: string; AQuery : string  = '') : TEvent;
    Function Move(calendarId: string; eventId: string; AQuery : TEventsmoveOptions) : TEvent;
    Function Patch(calendarId: string; eventId: string; aEvent : TEvent; AQuery : string  = '') : TEvent;
    Function Patch(calendarId: string; eventId: string; aEvent : TEvent; AQuery : TEventspatchOptions) : TEvent;
    Function QuickAdd(calendarId: string; AQuery : string  = '') : TEvent;
    Function QuickAdd(calendarId: string; AQuery : TEventsquickAddOptions) : TEvent;
    Function Update(calendarId: string; eventId: string; aEvent : TEvent; AQuery : string  = '') : TEvent;
    Function Update(calendarId: string; eventId: string; aEvent : TEvent; AQuery : TEventsupdateOptions) : TEvent;
    Function Watch(calendarId: string; aChannel : TChannel; AQuery : string  = '') : TChannel;
    Function Watch(calendarId: string; aChannel : TChannel; AQuery : TEventswatchOptions) : TChannel;
  end;
  
  
  { --------------------------------------------------------------------
    TFreebusyResource
    --------------------------------------------------------------------}
  
  TFreebusyResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Query(aFreeBusyRequest : TFreeBusyRequest) : TFreeBusyResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TSettingsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSettingsResource, method List
  
  TSettingsListOptions = Record
    maxResults : integer;
    pageToken : string;
    syncToken : string;
  end;
  
  
  //Optional query Options for TSettingsResource, method Watch
  
  TSettingsWatchOptions = Record
    maxResults : integer;
    pageToken : string;
    syncToken : string;
  end;
  
  TSettingsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(setting: string) : TSetting;
    Function List(AQuery : string  = '') : TSettings;
    Function List(AQuery : TSettingslistOptions) : TSettings;
    Function Watch(aChannel : TChannel; AQuery : string  = '') : TChannel;
    Function Watch(aChannel : TChannel; AQuery : TSettingswatchOptions) : TChannel;
  end;
  
  
  { --------------------------------------------------------------------
    TCalendarAPI
    --------------------------------------------------------------------}
  
  TCalendarAPI = Class(TGoogleAPI)
  Private
    FAclInstance : TAclResource;
    FCalendarListInstance : TCalendarListResource;
    FCalendarsInstance : TCalendarsResource;
    FChannelsInstance : TChannelsResource;
    FColorsInstance : TColorsResource;
    FEventsInstance : TEventsResource;
    FFreebusyInstance : TFreebusyResource;
    FSettingsInstance : TSettingsResource;
    Function GetAclInstance : TAclResource;virtual;
    Function GetCalendarListInstance : TCalendarListResource;virtual;
    Function GetCalendarsInstance : TCalendarsResource;virtual;
    Function GetChannelsInstance : TChannelsResource;virtual;
    Function GetColorsInstance : TColorsResource;virtual;
    Function GetEventsInstance : TEventsResource;virtual;
    Function GetFreebusyInstance : TFreebusyResource;virtual;
    Function GetSettingsInstance : TSettingsResource;virtual;
  Public
    //Override class functions with API info
    Class Function APIName : String; override;
    Class Function APIVersion : String; override;
    Class Function APIRevision : String; override;
    Class Function APIID : String; override;
    Class Function APITitle : String; override;
    Class Function APIDescription : String; override;
    Class Function APIOwnerDomain : String; override;
    Class Function APIOwnerName : String; override;
    Class Function APIIcon16 : String; override;
    Class Function APIIcon32 : String; override;
    Class Function APIdocumentationLink : String; override;
    Class Function APIrootUrl : string; override;
    Class Function APIbasePath : string;override;
    Class Function APIbaseURL : String;override;
    Class Function APIProtocol : string;override;
    Class Function APIservicePath : string;override;
    Class Function APIbatchPath : String;override;
    Class Function APIAuthScopes : TScopeInfoArray;override;
    Class Function APINeedsAuth : Boolean;override;
    Class Procedure RegisterAPIResources; override;
    //Add create function for resources
    Function CreateAclResource(AOwner : TComponent) : TAclResource;virtual;overload;
    Function CreateAclResource : TAclResource;virtual;overload;
    Function CreateCalendarListResource(AOwner : TComponent) : TCalendarListResource;virtual;overload;
    Function CreateCalendarListResource : TCalendarListResource;virtual;overload;
    Function CreateCalendarsResource(AOwner : TComponent) : TCalendarsResource;virtual;overload;
    Function CreateCalendarsResource : TCalendarsResource;virtual;overload;
    Function CreateChannelsResource(AOwner : TComponent) : TChannelsResource;virtual;overload;
    Function CreateChannelsResource : TChannelsResource;virtual;overload;
    Function CreateColorsResource(AOwner : TComponent) : TColorsResource;virtual;overload;
    Function CreateColorsResource : TColorsResource;virtual;overload;
    Function CreateEventsResource(AOwner : TComponent) : TEventsResource;virtual;overload;
    Function CreateEventsResource : TEventsResource;virtual;overload;
    Function CreateFreebusyResource(AOwner : TComponent) : TFreebusyResource;virtual;overload;
    Function CreateFreebusyResource : TFreebusyResource;virtual;overload;
    Function CreateSettingsResource(AOwner : TComponent) : TSettingsResource;virtual;overload;
    Function CreateSettingsResource : TSettingsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AclResource : TAclResource Read GetAclInstance;
    Property CalendarListResource : TCalendarListResource Read GetCalendarListInstance;
    Property CalendarsResource : TCalendarsResource Read GetCalendarsInstance;
    Property ChannelsResource : TChannelsResource Read GetChannelsInstance;
    Property ColorsResource : TColorsResource Read GetColorsInstance;
    Property EventsResource : TEventsResource Read GetEventsInstance;
    Property FreebusyResource : TFreebusyResource Read GetFreebusyInstance;
    Property SettingsResource : TSettingsResource Read GetSettingsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAcl
  --------------------------------------------------------------------}


Procedure TAcl.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAcl.Setitems(AIndex : Integer; AValue : TAclitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAcl.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAcl.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAcl.SetnextSyncToken(AIndex : Integer; AValue : string); 

begin
  If (FnextSyncToken=AValue) then exit;
  FnextSyncToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAclitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAclRule
  --------------------------------------------------------------------}


Procedure TAclRule.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAclRule.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAclRule.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAclRule.Setrole(AIndex : Integer; AValue : string); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAclRule.Setscope(AIndex : Integer; AValue : TAclRulescope); 

begin
  If (Fscope=AValue) then exit;
  Fscope:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAclRulescope
  --------------------------------------------------------------------}


Procedure TAclRulescope.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAclRulescope.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAclRulescope.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCalendar
  --------------------------------------------------------------------}


Procedure TCalendar.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendar.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendar.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendar.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendar.Setlocation(AIndex : Integer; AValue : string); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendar.Setsummary(AIndex : Integer; AValue : string); 

begin
  If (Fsummary=AValue) then exit;
  Fsummary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendar.SettimeZone(AIndex : Integer; AValue : string); 

begin
  If (FtimeZone=AValue) then exit;
  FtimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCalendarList
  --------------------------------------------------------------------}


Procedure TCalendarList.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarList.Setitems(AIndex : Integer; AValue : TCalendarListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarList.SetnextSyncToken(AIndex : Integer; AValue : string); 

begin
  If (FnextSyncToken=AValue) then exit;
  FnextSyncToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCalendarListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCalendarListEntry
  --------------------------------------------------------------------}


Procedure TCalendarListEntry.SetaccessRole(AIndex : Integer; AValue : string); 

begin
  If (FaccessRole=AValue) then exit;
  FaccessRole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.SetbackgroundColor(AIndex : Integer; AValue : string); 

begin
  If (FbackgroundColor=AValue) then exit;
  FbackgroundColor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.SetcolorId(AIndex : Integer; AValue : string); 

begin
  If (FcolorId=AValue) then exit;
  FcolorId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.SetdefaultReminders(AIndex : Integer; AValue : TCalendarListEntrydefaultReminders); 

begin
  If (FdefaultReminders=AValue) then exit;
  FdefaultReminders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.Setdeleted(AIndex : Integer; AValue : boolean); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.SetforegroundColor(AIndex : Integer; AValue : string); 

begin
  If (FforegroundColor=AValue) then exit;
  FforegroundColor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.Sethidden(AIndex : Integer; AValue : boolean); 

begin
  If (Fhidden=AValue) then exit;
  Fhidden:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.Setlocation(AIndex : Integer; AValue : string); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.SetnotificationSettings(AIndex : Integer; AValue : TCalendarListEntrynotificationSettings); 

begin
  If (FnotificationSettings=AValue) then exit;
  FnotificationSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.Setprimary(AIndex : Integer; AValue : boolean); 

begin
  If (Fprimary=AValue) then exit;
  Fprimary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.Setselected(AIndex : Integer; AValue : boolean); 

begin
  If (Fselected=AValue) then exit;
  Fselected:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.Setsummary(AIndex : Integer; AValue : string); 

begin
  If (Fsummary=AValue) then exit;
  Fsummary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.SetsummaryOverride(AIndex : Integer; AValue : string); 

begin
  If (FsummaryOverride=AValue) then exit;
  FsummaryOverride:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarListEntry.SettimeZone(AIndex : Integer; AValue : string); 

begin
  If (FtimeZone=AValue) then exit;
  FtimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCalendarListEntrydefaultReminders
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCalendarListEntrynotificationSettings
  --------------------------------------------------------------------}


Procedure TCalendarListEntrynotificationSettings.Setnotifications(AIndex : Integer; AValue : TCalendarListEntrynotificationSettingsnotifications); 

begin
  If (Fnotifications=AValue) then exit;
  Fnotifications:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCalendarListEntrynotificationSettingsnotifications
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCalendarNotification
  --------------------------------------------------------------------}


Procedure TCalendarNotification.Setmethod(AIndex : Integer; AValue : string); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCalendarNotification.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCalendarNotification.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TChannel
  --------------------------------------------------------------------}


Procedure TChannel.Setaddress(AIndex : Integer; AValue : string); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setexpiration(AIndex : Integer; AValue : string); 

begin
  If (Fexpiration=AValue) then exit;
  Fexpiration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setparams(AIndex : Integer; AValue : TChannelparams); 

begin
  If (Fparams=AValue) then exit;
  Fparams:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Setpayload(AIndex : Integer; AValue : boolean); 

begin
  If (Fpayload=AValue) then exit;
  Fpayload:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.SetresourceId(AIndex : Integer; AValue : string); 

begin
  If (FresourceId=AValue) then exit;
  FresourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.SetresourceUri(AIndex : Integer; AValue : string); 

begin
  If (FresourceUri=AValue) then exit;
  FresourceUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Settoken(AIndex : Integer; AValue : string); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChannel.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TChannel.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TChannelparams
  --------------------------------------------------------------------}


Class Function TChannelparams.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TColorDefinition
  --------------------------------------------------------------------}


Procedure TColorDefinition.Setbackground(AIndex : Integer; AValue : string); 

begin
  If (Fbackground=AValue) then exit;
  Fbackground:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColorDefinition.Setforeground(AIndex : Integer; AValue : string); 

begin
  If (Fforeground=AValue) then exit;
  Fforeground:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TColors
  --------------------------------------------------------------------}


Procedure TColors.Setcalendar(AIndex : Integer; AValue : TColorscalendar); 

begin
  If (Fcalendar=AValue) then exit;
  Fcalendar:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColors.Setevent(AIndex : Integer; AValue : TColorsevent); 

begin
  If (Fevent=AValue) then exit;
  Fevent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColors.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TColors.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TColorscalendar
  --------------------------------------------------------------------}


Class Function TColorscalendar.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TColorsevent
  --------------------------------------------------------------------}


Class Function TColorsevent.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TError
  --------------------------------------------------------------------}


Procedure TError.Setdomain(AIndex : Integer; AValue : string); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TError.Setreason(AIndex : Integer; AValue : string); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEvent
  --------------------------------------------------------------------}


Procedure TEvent.SetanyoneCanAddSelf(AIndex : Integer; AValue : boolean); 

begin
  If (FanyoneCanAddSelf=AValue) then exit;
  FanyoneCanAddSelf:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setattendees(AIndex : Integer; AValue : TEventattendees); 

begin
  If (Fattendees=AValue) then exit;
  Fattendees:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetattendeesOmitted(AIndex : Integer; AValue : boolean); 

begin
  If (FattendeesOmitted=AValue) then exit;
  FattendeesOmitted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetcolorId(AIndex : Integer; AValue : string); 

begin
  If (FcolorId=AValue) then exit;
  FcolorId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setcreated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setcreator(AIndex : Integer; AValue : TEventcreator); 

begin
  If (Fcreator=AValue) then exit;
  Fcreator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Set_end(AIndex : Integer; AValue : TEventDateTime); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetendTimeUnspecified(AIndex : Integer; AValue : boolean); 

begin
  If (FendTimeUnspecified=AValue) then exit;
  FendTimeUnspecified:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetextendedProperties(AIndex : Integer; AValue : TEventextendedProperties); 

begin
  If (FextendedProperties=AValue) then exit;
  FextendedProperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setgadget(AIndex : Integer; AValue : TEventgadget); 

begin
  If (Fgadget=AValue) then exit;
  Fgadget:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetguestsCanInviteOthers(AIndex : Integer; AValue : boolean); 

begin
  If (FguestsCanInviteOthers=AValue) then exit;
  FguestsCanInviteOthers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetguestsCanModify(AIndex : Integer; AValue : boolean); 

begin
  If (FguestsCanModify=AValue) then exit;
  FguestsCanModify:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetguestsCanSeeOtherGuests(AIndex : Integer; AValue : boolean); 

begin
  If (FguestsCanSeeOtherGuests=AValue) then exit;
  FguestsCanSeeOtherGuests:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SethangoutLink(AIndex : Integer; AValue : string); 

begin
  If (FhangoutLink=AValue) then exit;
  FhangoutLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SethtmlLink(AIndex : Integer; AValue : string); 

begin
  If (FhtmlLink=AValue) then exit;
  FhtmlLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetiCalUID(AIndex : Integer; AValue : string); 

begin
  If (FiCalUID=AValue) then exit;
  FiCalUID:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setlocation(AIndex : Integer; AValue : string); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setlocked(AIndex : Integer; AValue : boolean); 

begin
  If (Flocked=AValue) then exit;
  Flocked:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setorganizer(AIndex : Integer; AValue : TEventorganizer); 

begin
  If (Forganizer=AValue) then exit;
  Forganizer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetoriginalStartTime(AIndex : Integer; AValue : TEventDateTime); 

begin
  If (ForiginalStartTime=AValue) then exit;
  ForiginalStartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetprivateCopy(AIndex : Integer; AValue : boolean); 

begin
  If (FprivateCopy=AValue) then exit;
  FprivateCopy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setrecurrence(AIndex : Integer; AValue : TEventrecurrence); 

begin
  If (Frecurrence=AValue) then exit;
  Frecurrence:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetrecurringEventId(AIndex : Integer; AValue : string); 

begin
  If (FrecurringEventId=AValue) then exit;
  FrecurringEventId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setreminders(AIndex : Integer; AValue : TEventreminders); 

begin
  If (Freminders=AValue) then exit;
  Freminders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setsequence(AIndex : Integer; AValue : integer); 

begin
  If (Fsequence=AValue) then exit;
  Fsequence:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setsource(AIndex : Integer; AValue : TEventsource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setstart(AIndex : Integer; AValue : TEventDateTime); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setsummary(AIndex : Integer; AValue : string); 

begin
  If (Fsummary=AValue) then exit;
  Fsummary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Settransparency(AIndex : Integer; AValue : string); 

begin
  If (Ftransparency=AValue) then exit;
  Ftransparency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setvisibility(AIndex : Integer; AValue : string); 

begin
  If (Fvisibility=AValue) then exit;
  Fvisibility:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TEvent.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TEventattendees
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEventcreator
  --------------------------------------------------------------------}


Procedure TEventcreator.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventcreator.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventcreator.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventcreator.Set_self(AIndex : Integer; AValue : boolean); 

begin
  If (F_self=AValue) then exit;
  F_self:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TEventcreator.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_self' : Result:='self';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TEventextendedProperties
  --------------------------------------------------------------------}


Procedure TEventextendedProperties.Set_private(AIndex : Integer; AValue : TEventextendedPropertiesprivate); 

begin
  If (F_private=AValue) then exit;
  F_private:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventextendedProperties.Setshared(AIndex : Integer; AValue : TEventextendedPropertiesshared); 

begin
  If (Fshared=AValue) then exit;
  Fshared:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TEventextendedProperties.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_private' : Result:='private';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TEventextendedPropertiesprivate
  --------------------------------------------------------------------}


Class Function TEventextendedPropertiesprivate.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TEventextendedPropertiesshared
  --------------------------------------------------------------------}


Class Function TEventextendedPropertiesshared.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TEventgadget
  --------------------------------------------------------------------}


Procedure TEventgadget.Setdisplay(AIndex : Integer; AValue : string); 

begin
  If (Fdisplay=AValue) then exit;
  Fdisplay:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventgadget.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventgadget.SeticonLink(AIndex : Integer; AValue : string); 

begin
  If (FiconLink=AValue) then exit;
  FiconLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventgadget.Setlink(AIndex : Integer; AValue : string); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventgadget.Setpreferences(AIndex : Integer; AValue : TEventgadgetpreferences); 

begin
  If (Fpreferences=AValue) then exit;
  Fpreferences:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventgadget.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventgadget.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventgadget.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TEventgadget.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TEventgadgetpreferences
  --------------------------------------------------------------------}


Class Function TEventgadgetpreferences.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TEventorganizer
  --------------------------------------------------------------------}


Procedure TEventorganizer.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventorganizer.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventorganizer.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventorganizer.Set_self(AIndex : Integer; AValue : boolean); 

begin
  If (F_self=AValue) then exit;
  F_self:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TEventorganizer.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_self' : Result:='self';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TEventrecurrence
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEventreminders
  --------------------------------------------------------------------}


Procedure TEventreminders.Setoverrides(AIndex : Integer; AValue : TEventremindersoverrides); 

begin
  If (Foverrides=AValue) then exit;
  Foverrides:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventreminders.SetuseDefault(AIndex : Integer; AValue : boolean); 

begin
  If (FuseDefault=AValue) then exit;
  FuseDefault:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventremindersoverrides
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEventsource
  --------------------------------------------------------------------}


Procedure TEventsource.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventsource.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventAttachment
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEventAttendee
  --------------------------------------------------------------------}


Procedure TEventAttendee.SetadditionalGuests(AIndex : Integer; AValue : integer); 

begin
  If (FadditionalGuests=AValue) then exit;
  FadditionalGuests:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventAttendee.Setcomment(AIndex : Integer; AValue : string); 

begin
  If (Fcomment=AValue) then exit;
  Fcomment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventAttendee.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventAttendee.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventAttendee.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventAttendee.Setoptional(AIndex : Integer; AValue : boolean); 

begin
  If (Foptional=AValue) then exit;
  Foptional:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventAttendee.Setorganizer(AIndex : Integer; AValue : boolean); 

begin
  If (Forganizer=AValue) then exit;
  Forganizer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventAttendee.Setresource(AIndex : Integer; AValue : boolean); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventAttendee.SetresponseStatus(AIndex : Integer; AValue : string); 

begin
  If (FresponseStatus=AValue) then exit;
  FresponseStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventAttendee.Set_self(AIndex : Integer; AValue : boolean); 

begin
  If (F_self=AValue) then exit;
  F_self:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TEventAttendee.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_self' : Result:='self';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TEventDateTime
  --------------------------------------------------------------------}


Procedure TEventDateTime.Setdate(AIndex : Integer; AValue : TDate); 

begin
  If (Fdate=AValue) then exit;
  Fdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventDateTime.SetdateTime(AIndex : Integer; AValue : TDatetime); 

begin
  If (FdateTime=AValue) then exit;
  FdateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventDateTime.SettimeZone(AIndex : Integer; AValue : string); 

begin
  If (FtimeZone=AValue) then exit;
  FtimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventReminder
  --------------------------------------------------------------------}


Procedure TEventReminder.Setmethod(AIndex : Integer; AValue : string); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventReminder.Setminutes(AIndex : Integer; AValue : integer); 

begin
  If (Fminutes=AValue) then exit;
  Fminutes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEvents
  --------------------------------------------------------------------}


Procedure TEvents.SetaccessRole(AIndex : Integer; AValue : string); 

begin
  If (FaccessRole=AValue) then exit;
  FaccessRole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvents.SetdefaultReminders(AIndex : Integer; AValue : TEventsdefaultReminders); 

begin
  If (FdefaultReminders=AValue) then exit;
  FdefaultReminders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvents.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvents.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvents.Setitems(AIndex : Integer; AValue : TEventsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvents.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvents.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvents.SetnextSyncToken(AIndex : Integer; AValue : string); 

begin
  If (FnextSyncToken=AValue) then exit;
  FnextSyncToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvents.Setsummary(AIndex : Integer; AValue : string); 

begin
  If (Fsummary=AValue) then exit;
  Fsummary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvents.SettimeZone(AIndex : Integer; AValue : string); 

begin
  If (FtimeZone=AValue) then exit;
  FtimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvents.Setupdated(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fupdated=AValue) then exit;
  Fupdated:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventsdefaultReminders
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEventsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFreeBusyCalendar
  --------------------------------------------------------------------}


Procedure TFreeBusyCalendar.Setbusy(AIndex : Integer; AValue : TFreeBusyCalendarbusy); 

begin
  If (Fbusy=AValue) then exit;
  Fbusy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFreeBusyCalendar.Seterrors(AIndex : Integer; AValue : TFreeBusyCalendarerrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFreeBusyCalendarbusy
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFreeBusyCalendarerrors
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFreeBusyGroup
  --------------------------------------------------------------------}


Procedure TFreeBusyGroup.Setcalendars(AIndex : Integer; AValue : TFreeBusyGroupcalendars); 

begin
  If (Fcalendars=AValue) then exit;
  Fcalendars:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFreeBusyGroup.Seterrors(AIndex : Integer; AValue : TFreeBusyGrouperrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFreeBusyGroupcalendars
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFreeBusyGrouperrors
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFreeBusyRequest
  --------------------------------------------------------------------}


Procedure TFreeBusyRequest.SetcalendarExpansionMax(AIndex : Integer; AValue : integer); 

begin
  If (FcalendarExpansionMax=AValue) then exit;
  FcalendarExpansionMax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFreeBusyRequest.SetgroupExpansionMax(AIndex : Integer; AValue : integer); 

begin
  If (FgroupExpansionMax=AValue) then exit;
  FgroupExpansionMax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFreeBusyRequest.Setitems(AIndex : Integer; AValue : TFreeBusyRequestitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFreeBusyRequest.SettimeMax(AIndex : Integer; AValue : TDatetime); 

begin
  If (FtimeMax=AValue) then exit;
  FtimeMax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFreeBusyRequest.SettimeMin(AIndex : Integer; AValue : TDatetime); 

begin
  If (FtimeMin=AValue) then exit;
  FtimeMin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFreeBusyRequest.SettimeZone(AIndex : Integer; AValue : string); 

begin
  If (FtimeZone=AValue) then exit;
  FtimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFreeBusyRequestitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFreeBusyRequestItem
  --------------------------------------------------------------------}


Procedure TFreeBusyRequestItem.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFreeBusyResponse
  --------------------------------------------------------------------}


Procedure TFreeBusyResponse.Setcalendars(AIndex : Integer; AValue : TFreeBusyResponsecalendars); 

begin
  If (Fcalendars=AValue) then exit;
  Fcalendars:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFreeBusyResponse.Setgroups(AIndex : Integer; AValue : TFreeBusyResponsegroups); 

begin
  If (Fgroups=AValue) then exit;
  Fgroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFreeBusyResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFreeBusyResponse.SettimeMax(AIndex : Integer; AValue : TDatetime); 

begin
  If (FtimeMax=AValue) then exit;
  FtimeMax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFreeBusyResponse.SettimeMin(AIndex : Integer; AValue : TDatetime); 

begin
  If (FtimeMin=AValue) then exit;
  FtimeMin:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFreeBusyResponsecalendars
  --------------------------------------------------------------------}


Class Function TFreeBusyResponsecalendars.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TFreeBusyResponsegroups
  --------------------------------------------------------------------}


Class Function TFreeBusyResponsegroups.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSetting
  --------------------------------------------------------------------}


Procedure TSetting.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetting.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetting.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSetting.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSettings
  --------------------------------------------------------------------}


Procedure TSettings.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSettings.Setitems(AIndex : Integer; AValue : TSettingsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSettings.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSettings.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSettings.SetnextSyncToken(AIndex : Integer; AValue : string); 

begin
  If (FnextSyncToken=AValue) then exit;
  FnextSyncToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSettingsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTimePeriod
  --------------------------------------------------------------------}


Procedure TTimePeriod.Set_end(AIndex : Integer; AValue : TDatetime); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimePeriod.Setstart(AIndex : Integer; AValue : TDatetime); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TTimePeriod.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAclResource
  --------------------------------------------------------------------}


Class Function TAclResource.ResourceName : String;

begin
  Result:='acl';
end;

Class Function TAclResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcalendarAPI;
end;

Procedure TAclResource.Delete(calendarId: string; ruleId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'calendars/{calendarId}/acl/{ruleId}';
  _Methodid   = 'calendar.acl.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId,'ruleId',ruleId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAclResource.Get(calendarId: string; ruleId: string) : TAclRule;

Const
  _HTTPMethod = 'GET';
  _Path       = 'calendars/{calendarId}/acl/{ruleId}';
  _Methodid   = 'calendar.acl.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId,'ruleId',ruleId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAclRule) as TAclRule;
end;

Function TAclResource.Insert(calendarId: string; aAclRule : TAclRule) : TAclRule;

Const
  _HTTPMethod = 'POST';
  _Path       = 'calendars/{calendarId}/acl';
  _Methodid   = 'calendar.acl.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAclRule,TAclRule) as TAclRule;
end;

Function TAclResource.List(calendarId: string; AQuery : string = '') : TAcl;

Const
  _HTTPMethod = 'GET';
  _Path       = 'calendars/{calendarId}/acl';
  _Methodid   = 'calendar.acl.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAcl) as TAcl;
end;


Function TAclResource.List(calendarId: string; AQuery : TAcllistOptions) : TAcl;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'showDeleted',AQuery.showDeleted);
  AddToQuery(_Q,'syncToken',AQuery.syncToken);
  Result:=List(calendarId,_Q);
end;

Function TAclResource.Patch(calendarId: string; ruleId: string; aAclRule : TAclRule) : TAclRule;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'calendars/{calendarId}/acl/{ruleId}';
  _Methodid   = 'calendar.acl.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId,'ruleId',ruleId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAclRule,TAclRule) as TAclRule;
end;

Function TAclResource.Update(calendarId: string; ruleId: string; aAclRule : TAclRule) : TAclRule;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'calendars/{calendarId}/acl/{ruleId}';
  _Methodid   = 'calendar.acl.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId,'ruleId',ruleId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAclRule,TAclRule) as TAclRule;
end;

Function TAclResource.Watch(calendarId: string; aChannel : TChannel; AQuery : string = '') : TChannel;

Const
  _HTTPMethod = 'POST';
  _Path       = 'calendars/{calendarId}/acl/watch';
  _Methodid   = 'calendar.acl.watch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aChannel,TChannel) as TChannel;
end;


Function TAclResource.Watch(calendarId: string; aChannel : TChannel; AQuery : TAclwatchOptions) : TChannel;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'showDeleted',AQuery.showDeleted);
  AddToQuery(_Q,'syncToken',AQuery.syncToken);
  Result:=Watch(calendarId,aChannel,_Q);
end;



{ --------------------------------------------------------------------
  TCalendarListResource
  --------------------------------------------------------------------}


Class Function TCalendarListResource.ResourceName : String;

begin
  Result:='calendarList';
end;

Class Function TCalendarListResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcalendarAPI;
end;

Procedure TCalendarListResource.Delete(calendarId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'users/me/calendarList/{calendarId}';
  _Methodid   = 'calendar.calendarList.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TCalendarListResource.Get(calendarId: string) : TCalendarListEntry;

Const
  _HTTPMethod = 'GET';
  _Path       = 'users/me/calendarList/{calendarId}';
  _Methodid   = 'calendar.calendarList.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCalendarListEntry) as TCalendarListEntry;
end;

Function TCalendarListResource.Insert(aCalendarListEntry : TCalendarListEntry; AQuery : string = '') : TCalendarListEntry;

Const
  _HTTPMethod = 'POST';
  _Path       = 'users/me/calendarList';
  _Methodid   = 'calendar.calendarList.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aCalendarListEntry,TCalendarListEntry) as TCalendarListEntry;
end;


Function TCalendarListResource.Insert(aCalendarListEntry : TCalendarListEntry; AQuery : TCalendarListinsertOptions) : TCalendarListEntry;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'colorRgbFormat',AQuery.colorRgbFormat);
  Result:=Insert(aCalendarListEntry,_Q);
end;

Function TCalendarListResource.List(AQuery : string = '') : TCalendarList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'users/me/calendarList';
  _Methodid   = 'calendar.calendarList.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TCalendarList) as TCalendarList;
end;


Function TCalendarListResource.List(AQuery : TCalendarListlistOptions) : TCalendarList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'minAccessRole',AQuery.minAccessRole);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'showDeleted',AQuery.showDeleted);
  AddToQuery(_Q,'showHidden',AQuery.showHidden);
  AddToQuery(_Q,'syncToken',AQuery.syncToken);
  Result:=List(_Q);
end;

Function TCalendarListResource.Patch(calendarId: string; aCalendarListEntry : TCalendarListEntry; AQuery : string = '') : TCalendarListEntry;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'users/me/calendarList/{calendarId}';
  _Methodid   = 'calendar.calendarList.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aCalendarListEntry,TCalendarListEntry) as TCalendarListEntry;
end;


Function TCalendarListResource.Patch(calendarId: string; aCalendarListEntry : TCalendarListEntry; AQuery : TCalendarListpatchOptions) : TCalendarListEntry;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'colorRgbFormat',AQuery.colorRgbFormat);
  Result:=Patch(calendarId,aCalendarListEntry,_Q);
end;

Function TCalendarListResource.Update(calendarId: string; aCalendarListEntry : TCalendarListEntry; AQuery : string = '') : TCalendarListEntry;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'users/me/calendarList/{calendarId}';
  _Methodid   = 'calendar.calendarList.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aCalendarListEntry,TCalendarListEntry) as TCalendarListEntry;
end;


Function TCalendarListResource.Update(calendarId: string; aCalendarListEntry : TCalendarListEntry; AQuery : TCalendarListupdateOptions) : TCalendarListEntry;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'colorRgbFormat',AQuery.colorRgbFormat);
  Result:=Update(calendarId,aCalendarListEntry,_Q);
end;

Function TCalendarListResource.Watch(aChannel : TChannel; AQuery : string = '') : TChannel;

Const
  _HTTPMethod = 'POST';
  _Path       = 'users/me/calendarList/watch';
  _Methodid   = 'calendar.calendarList.watch';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aChannel,TChannel) as TChannel;
end;


Function TCalendarListResource.Watch(aChannel : TChannel; AQuery : TCalendarListwatchOptions) : TChannel;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'minAccessRole',AQuery.minAccessRole);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'showDeleted',AQuery.showDeleted);
  AddToQuery(_Q,'showHidden',AQuery.showHidden);
  AddToQuery(_Q,'syncToken',AQuery.syncToken);
  Result:=Watch(aChannel,_Q);
end;



{ --------------------------------------------------------------------
  TCalendarsResource
  --------------------------------------------------------------------}


Class Function TCalendarsResource.ResourceName : String;

begin
  Result:='calendars';
end;

Class Function TCalendarsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcalendarAPI;
end;

Procedure TCalendarsResource.Clear(calendarId: string);

Const
  _HTTPMethod = 'POST';
  _Path       = 'calendars/{calendarId}/clear';
  _Methodid   = 'calendar.calendars.clear';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Procedure TCalendarsResource.Delete(calendarId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'calendars/{calendarId}';
  _Methodid   = 'calendar.calendars.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TCalendarsResource.Get(calendarId: string) : TCalendar;

Const
  _HTTPMethod = 'GET';
  _Path       = 'calendars/{calendarId}';
  _Methodid   = 'calendar.calendars.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCalendar) as TCalendar;
end;

Function TCalendarsResource.Insert(aCalendar : TCalendar) : TCalendar;

Const
  _HTTPMethod = 'POST';
  _Path       = 'calendars';
  _Methodid   = 'calendar.calendars.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aCalendar,TCalendar) as TCalendar;
end;

Function TCalendarsResource.Patch(calendarId: string; aCalendar : TCalendar) : TCalendar;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'calendars/{calendarId}';
  _Methodid   = 'calendar.calendars.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCalendar,TCalendar) as TCalendar;
end;

Function TCalendarsResource.Update(calendarId: string; aCalendar : TCalendar) : TCalendar;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'calendars/{calendarId}';
  _Methodid   = 'calendar.calendars.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCalendar,TCalendar) as TCalendar;
end;



{ --------------------------------------------------------------------
  TChannelsResource
  --------------------------------------------------------------------}


Class Function TChannelsResource.ResourceName : String;

begin
  Result:='channels';
end;

Class Function TChannelsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcalendarAPI;
end;

Procedure TChannelsResource.Stop(aChannel : TChannel);

Const
  _HTTPMethod = 'POST';
  _Path       = 'channels/stop';
  _Methodid   = 'calendar.channels.stop';

begin
  ServiceCall(_HTTPMethod,_Path,'',aChannel,Nil);
end;



{ --------------------------------------------------------------------
  TColorsResource
  --------------------------------------------------------------------}


Class Function TColorsResource.ResourceName : String;

begin
  Result:='colors';
end;

Class Function TColorsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcalendarAPI;
end;

Function TColorsResource.Get : TColors;

Const
  _HTTPMethod = 'GET';
  _Path       = 'colors';
  _Methodid   = 'calendar.colors.get';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TColors) as TColors;
end;



{ --------------------------------------------------------------------
  TEventsResource
  --------------------------------------------------------------------}


Class Function TEventsResource.ResourceName : String;

begin
  Result:='events';
end;

Class Function TEventsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcalendarAPI;
end;

Procedure TEventsResource.Delete(calendarId: string; eventId: string; AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'calendars/{calendarId}/events/{eventId}';
  _Methodid   = 'calendar.events.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId,'eventId',eventId]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TEventsResource.Delete(calendarId: string; eventId: string; AQuery : TEventsdeleteOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'sendNotifications',AQuery.sendNotifications);
  Delete(calendarId,eventId,_Q);
end;

Function TEventsResource.Get(calendarId: string; eventId: string; AQuery : string = '') : TEvent;

Const
  _HTTPMethod = 'GET';
  _Path       = 'calendars/{calendarId}/events/{eventId}';
  _Methodid   = 'calendar.events.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId,'eventId',eventId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEvent) as TEvent;
end;


Function TEventsResource.Get(calendarId: string; eventId: string; AQuery : TEventsgetOptions) : TEvent;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'alwaysIncludeEmail',AQuery.alwaysIncludeEmail);
  AddToQuery(_Q,'maxAttendees',AQuery.maxAttendees);
  AddToQuery(_Q,'timeZone',AQuery.timeZone);
  Result:=Get(calendarId,eventId,_Q);
end;

Function TEventsResource.Import(calendarId: string; aEvent : TEvent) : TEvent;

Const
  _HTTPMethod = 'POST';
  _Path       = 'calendars/{calendarId}/events/import';
  _Methodid   = 'calendar.events.import';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEvent,TEvent) as TEvent;
end;

Function TEventsResource.Insert(calendarId: string; aEvent : TEvent; AQuery : string = '') : TEvent;

Const
  _HTTPMethod = 'POST';
  _Path       = 'calendars/{calendarId}/events';
  _Methodid   = 'calendar.events.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aEvent,TEvent) as TEvent;
end;


Function TEventsResource.Insert(calendarId: string; aEvent : TEvent; AQuery : TEventsinsertOptions) : TEvent;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxAttendees',AQuery.maxAttendees);
  AddToQuery(_Q,'sendNotifications',AQuery.sendNotifications);
  Result:=Insert(calendarId,aEvent,_Q);
end;

Function TEventsResource.Instances(calendarId: string; eventId: string; AQuery : string = '') : TEvents;

Const
  _HTTPMethod = 'GET';
  _Path       = 'calendars/{calendarId}/events/{eventId}/instances';
  _Methodid   = 'calendar.events.instances';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId,'eventId',eventId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEvents) as TEvents;
end;


Function TEventsResource.Instances(calendarId: string; eventId: string; AQuery : TEventsinstancesOptions) : TEvents;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'alwaysIncludeEmail',AQuery.alwaysIncludeEmail);
  AddToQuery(_Q,'maxAttendees',AQuery.maxAttendees);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'originalStart',AQuery.originalStart);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'showDeleted',AQuery.showDeleted);
  AddToQuery(_Q,'timeMax',AQuery.timeMax);
  AddToQuery(_Q,'timeMin',AQuery.timeMin);
  AddToQuery(_Q,'timeZone',AQuery.timeZone);
  Result:=Instances(calendarId,eventId,_Q);
end;

Function TEventsResource.List(calendarId: string; AQuery : string = '') : TEvents;

Const
  _HTTPMethod = 'GET';
  _Path       = 'calendars/{calendarId}/events';
  _Methodid   = 'calendar.events.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEvents) as TEvents;
end;


Function TEventsResource.List(calendarId: string; AQuery : TEventslistOptions) : TEvents;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'alwaysIncludeEmail',AQuery.alwaysIncludeEmail);
  AddToQuery(_Q,'iCalUID',AQuery.iCalUID);
  AddToQuery(_Q,'maxAttendees',AQuery.maxAttendees);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'privateExtendedProperty',AQuery.privateExtendedProperty);
  AddToQuery(_Q,'q',AQuery.q);
  AddToQuery(_Q,'sharedExtendedProperty',AQuery.sharedExtendedProperty);
  AddToQuery(_Q,'showDeleted',AQuery.showDeleted);
  AddToQuery(_Q,'showHiddenInvitations',AQuery.showHiddenInvitations);
  AddToQuery(_Q,'singleEvents',AQuery.singleEvents);
  AddToQuery(_Q,'syncToken',AQuery.syncToken);
  AddToQuery(_Q,'timeMax',AQuery.timeMax);
  AddToQuery(_Q,'timeMin',AQuery.timeMin);
  AddToQuery(_Q,'timeZone',AQuery.timeZone);
  AddToQuery(_Q,'updatedMin',AQuery.updatedMin);
  Result:=List(calendarId,_Q);
end;

Function TEventsResource.Move(calendarId: string; eventId: string; AQuery : string = '') : TEvent;

Const
  _HTTPMethod = 'POST';
  _Path       = 'calendars/{calendarId}/events/{eventId}/move';
  _Methodid   = 'calendar.events.move';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId,'eventId',eventId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEvent) as TEvent;
end;


Function TEventsResource.Move(calendarId: string; eventId: string; AQuery : TEventsmoveOptions) : TEvent;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'destination',AQuery.destination);
  AddToQuery(_Q,'sendNotifications',AQuery.sendNotifications);
  Result:=Move(calendarId,eventId,_Q);
end;

Function TEventsResource.Patch(calendarId: string; eventId: string; aEvent : TEvent; AQuery : string = '') : TEvent;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'calendars/{calendarId}/events/{eventId}';
  _Methodid   = 'calendar.events.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId,'eventId',eventId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aEvent,TEvent) as TEvent;
end;


Function TEventsResource.Patch(calendarId: string; eventId: string; aEvent : TEvent; AQuery : TEventspatchOptions) : TEvent;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'alwaysIncludeEmail',AQuery.alwaysIncludeEmail);
  AddToQuery(_Q,'maxAttendees',AQuery.maxAttendees);
  AddToQuery(_Q,'sendNotifications',AQuery.sendNotifications);
  Result:=Patch(calendarId,eventId,aEvent,_Q);
end;

Function TEventsResource.QuickAdd(calendarId: string; AQuery : string = '') : TEvent;

Const
  _HTTPMethod = 'POST';
  _Path       = 'calendars/{calendarId}/events/quickAdd';
  _Methodid   = 'calendar.events.quickAdd';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEvent) as TEvent;
end;


Function TEventsResource.QuickAdd(calendarId: string; AQuery : TEventsquickAddOptions) : TEvent;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'sendNotifications',AQuery.sendNotifications);
  AddToQuery(_Q,'text',AQuery.text);
  Result:=QuickAdd(calendarId,_Q);
end;

Function TEventsResource.Update(calendarId: string; eventId: string; aEvent : TEvent; AQuery : string = '') : TEvent;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'calendars/{calendarId}/events/{eventId}';
  _Methodid   = 'calendar.events.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId,'eventId',eventId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aEvent,TEvent) as TEvent;
end;


Function TEventsResource.Update(calendarId: string; eventId: string; aEvent : TEvent; AQuery : TEventsupdateOptions) : TEvent;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'alwaysIncludeEmail',AQuery.alwaysIncludeEmail);
  AddToQuery(_Q,'maxAttendees',AQuery.maxAttendees);
  AddToQuery(_Q,'sendNotifications',AQuery.sendNotifications);
  Result:=Update(calendarId,eventId,aEvent,_Q);
end;

Function TEventsResource.Watch(calendarId: string; aChannel : TChannel; AQuery : string = '') : TChannel;

Const
  _HTTPMethod = 'POST';
  _Path       = 'calendars/{calendarId}/events/watch';
  _Methodid   = 'calendar.events.watch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['calendarId',calendarId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aChannel,TChannel) as TChannel;
end;


Function TEventsResource.Watch(calendarId: string; aChannel : TChannel; AQuery : TEventswatchOptions) : TChannel;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'alwaysIncludeEmail',AQuery.alwaysIncludeEmail);
  AddToQuery(_Q,'iCalUID',AQuery.iCalUID);
  AddToQuery(_Q,'maxAttendees',AQuery.maxAttendees);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'privateExtendedProperty',AQuery.privateExtendedProperty);
  AddToQuery(_Q,'q',AQuery.q);
  AddToQuery(_Q,'sharedExtendedProperty',AQuery.sharedExtendedProperty);
  AddToQuery(_Q,'showDeleted',AQuery.showDeleted);
  AddToQuery(_Q,'showHiddenInvitations',AQuery.showHiddenInvitations);
  AddToQuery(_Q,'singleEvents',AQuery.singleEvents);
  AddToQuery(_Q,'syncToken',AQuery.syncToken);
  AddToQuery(_Q,'timeMax',AQuery.timeMax);
  AddToQuery(_Q,'timeMin',AQuery.timeMin);
  AddToQuery(_Q,'timeZone',AQuery.timeZone);
  AddToQuery(_Q,'updatedMin',AQuery.updatedMin);
  Result:=Watch(calendarId,aChannel,_Q);
end;



{ --------------------------------------------------------------------
  TFreebusyResource
  --------------------------------------------------------------------}


Class Function TFreebusyResource.ResourceName : String;

begin
  Result:='freebusy';
end;

Class Function TFreebusyResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcalendarAPI;
end;

Function TFreebusyResource.Query(aFreeBusyRequest : TFreeBusyRequest) : TFreeBusyResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'freeBusy';
  _Methodid   = 'calendar.freebusy.query';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aFreeBusyRequest,TFreeBusyResponse) as TFreeBusyResponse;
end;



{ --------------------------------------------------------------------
  TSettingsResource
  --------------------------------------------------------------------}


Class Function TSettingsResource.ResourceName : String;

begin
  Result:='settings';
end;

Class Function TSettingsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcalendarAPI;
end;

Function TSettingsResource.Get(setting: string) : TSetting;

Const
  _HTTPMethod = 'GET';
  _Path       = 'users/me/settings/{setting}';
  _Methodid   = 'calendar.settings.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['setting',setting]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSetting) as TSetting;
end;

Function TSettingsResource.List(AQuery : string = '') : TSettings;

Const
  _HTTPMethod = 'GET';
  _Path       = 'users/me/settings';
  _Methodid   = 'calendar.settings.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TSettings) as TSettings;
end;


Function TSettingsResource.List(AQuery : TSettingslistOptions) : TSettings;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'syncToken',AQuery.syncToken);
  Result:=List(_Q);
end;

Function TSettingsResource.Watch(aChannel : TChannel; AQuery : string = '') : TChannel;

Const
  _HTTPMethod = 'POST';
  _Path       = 'users/me/settings/watch';
  _Methodid   = 'calendar.settings.watch';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aChannel,TChannel) as TChannel;
end;


Function TSettingsResource.Watch(aChannel : TChannel; AQuery : TSettingswatchOptions) : TChannel;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'syncToken',AQuery.syncToken);
  Result:=Watch(aChannel,_Q);
end;



{ --------------------------------------------------------------------
  TCalendarAPI
  --------------------------------------------------------------------}

Class Function TCalendarAPI.APIName : String;

begin
  Result:='calendar';
end;

Class Function TCalendarAPI.APIVersion : String;

begin
  Result:='v3';
end;

Class Function TCalendarAPI.APIRevision : String;

begin
  Result:='20150408';
end;

Class Function TCalendarAPI.APIID : String;

begin
  Result:='calendar:v3';
end;

Class Function TCalendarAPI.APITitle : String;

begin
  Result:='Calendar API';
end;

Class Function TCalendarAPI.APIDescription : String;

begin
  Result:='Lets you manipulate events and other calendar data.';
end;

Class Function TCalendarAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TCalendarAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TCalendarAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/calendar-16.png';
end;

Class Function TCalendarAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/calendar-32.png';
end;

Class Function TCalendarAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/google-apps/calendar/firstapp';
end;

Class Function TCalendarAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TCalendarAPI.APIbasePath : string;

begin
  Result:='/calendar/v3/';
end;

Class Function TCalendarAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/calendar/v3/';
end;

Class Function TCalendarAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TCalendarAPI.APIservicePath : string;

begin
  Result:='calendar/v3/';
end;

Class Function TCalendarAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TCalendarAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/calendar';
  Result[0].Description:='Manage your calendars';
  Result[1].Name:='https://www.googleapis.com/auth/calendar.readonly';
  Result[1].Description:='View your calendars';
  
end;

Class Function TCalendarAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TCalendarAPI.RegisterAPIResources;

begin
  TAcl.RegisterObject;
  TAclitems.RegisterObject;
  TAclRule.RegisterObject;
  TAclRulescope.RegisterObject;
  TCalendar.RegisterObject;
  TCalendarList.RegisterObject;
  TCalendarListitems.RegisterObject;
  TCalendarListEntry.RegisterObject;
  TCalendarListEntrydefaultReminders.RegisterObject;
  TCalendarListEntrynotificationSettings.RegisterObject;
  TCalendarListEntrynotificationSettingsnotifications.RegisterObject;
  TCalendarNotification.RegisterObject;
  TChannel.RegisterObject;
  TChannelparams.RegisterObject;
  TColorDefinition.RegisterObject;
  TColors.RegisterObject;
  TColorscalendar.RegisterObject;
  TColorsevent.RegisterObject;
  TError.RegisterObject;
  TEvent.RegisterObject;
  TEventattendees.RegisterObject;
  TEventcreator.RegisterObject;
  TEventextendedProperties.RegisterObject;
  TEventextendedPropertiesprivate.RegisterObject;
  TEventextendedPropertiesshared.RegisterObject;
  TEventgadget.RegisterObject;
  TEventgadgetpreferences.RegisterObject;
  TEventorganizer.RegisterObject;
  TEventrecurrence.RegisterObject;
  TEventreminders.RegisterObject;
  TEventremindersoverrides.RegisterObject;
  TEventsource.RegisterObject;
  TEventAttachment.RegisterObject;
  TEventAttendee.RegisterObject;
  TEventDateTime.RegisterObject;
  TEventReminder.RegisterObject;
  TEvents.RegisterObject;
  TEventsdefaultReminders.RegisterObject;
  TEventsitems.RegisterObject;
  TFreeBusyCalendar.RegisterObject;
  TFreeBusyCalendarbusy.RegisterObject;
  TFreeBusyCalendarerrors.RegisterObject;
  TFreeBusyGroup.RegisterObject;
  TFreeBusyGroupcalendars.RegisterObject;
  TFreeBusyGrouperrors.RegisterObject;
  TFreeBusyRequest.RegisterObject;
  TFreeBusyRequestitems.RegisterObject;
  TFreeBusyRequestItem.RegisterObject;
  TFreeBusyResponse.RegisterObject;
  TFreeBusyResponsecalendars.RegisterObject;
  TFreeBusyResponsegroups.RegisterObject;
  TSetting.RegisterObject;
  TSettings.RegisterObject;
  TSettingsitems.RegisterObject;
  TTimePeriod.RegisterObject;
end;


Function TCalendarAPI.GetAclInstance : TAclResource;

begin
  if (FAclInstance=Nil) then
    FAclInstance:=CreateAclResource;
  Result:=FAclInstance;
end;

Function TCalendarAPI.CreateAclResource : TAclResource;

begin
  Result:=CreateAclResource(Self);
end;


Function TCalendarAPI.CreateAclResource(AOwner : TComponent) : TAclResource;

begin
  Result:=TAclResource.Create(AOwner);
  Result.API:=Self;
end;



Function TCalendarAPI.GetCalendarListInstance : TCalendarListResource;

begin
  if (FCalendarListInstance=Nil) then
    FCalendarListInstance:=CreateCalendarListResource;
  Result:=FCalendarListInstance;
end;

Function TCalendarAPI.CreateCalendarListResource : TCalendarListResource;

begin
  Result:=CreateCalendarListResource(Self);
end;


Function TCalendarAPI.CreateCalendarListResource(AOwner : TComponent) : TCalendarListResource;

begin
  Result:=TCalendarListResource.Create(AOwner);
  Result.API:=Self;
end;



Function TCalendarAPI.GetCalendarsInstance : TCalendarsResource;

begin
  if (FCalendarsInstance=Nil) then
    FCalendarsInstance:=CreateCalendarsResource;
  Result:=FCalendarsInstance;
end;

Function TCalendarAPI.CreateCalendarsResource : TCalendarsResource;

begin
  Result:=CreateCalendarsResource(Self);
end;


Function TCalendarAPI.CreateCalendarsResource(AOwner : TComponent) : TCalendarsResource;

begin
  Result:=TCalendarsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TCalendarAPI.GetChannelsInstance : TChannelsResource;

begin
  if (FChannelsInstance=Nil) then
    FChannelsInstance:=CreateChannelsResource;
  Result:=FChannelsInstance;
end;

Function TCalendarAPI.CreateChannelsResource : TChannelsResource;

begin
  Result:=CreateChannelsResource(Self);
end;


Function TCalendarAPI.CreateChannelsResource(AOwner : TComponent) : TChannelsResource;

begin
  Result:=TChannelsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TCalendarAPI.GetColorsInstance : TColorsResource;

begin
  if (FColorsInstance=Nil) then
    FColorsInstance:=CreateColorsResource;
  Result:=FColorsInstance;
end;

Function TCalendarAPI.CreateColorsResource : TColorsResource;

begin
  Result:=CreateColorsResource(Self);
end;


Function TCalendarAPI.CreateColorsResource(AOwner : TComponent) : TColorsResource;

begin
  Result:=TColorsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TCalendarAPI.GetEventsInstance : TEventsResource;

begin
  if (FEventsInstance=Nil) then
    FEventsInstance:=CreateEventsResource;
  Result:=FEventsInstance;
end;

Function TCalendarAPI.CreateEventsResource : TEventsResource;

begin
  Result:=CreateEventsResource(Self);
end;


Function TCalendarAPI.CreateEventsResource(AOwner : TComponent) : TEventsResource;

begin
  Result:=TEventsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TCalendarAPI.GetFreebusyInstance : TFreebusyResource;

begin
  if (FFreebusyInstance=Nil) then
    FFreebusyInstance:=CreateFreebusyResource;
  Result:=FFreebusyInstance;
end;

Function TCalendarAPI.CreateFreebusyResource : TFreebusyResource;

begin
  Result:=CreateFreebusyResource(Self);
end;


Function TCalendarAPI.CreateFreebusyResource(AOwner : TComponent) : TFreebusyResource;

begin
  Result:=TFreebusyResource.Create(AOwner);
  Result.API:=Self;
end;



Function TCalendarAPI.GetSettingsInstance : TSettingsResource;

begin
  if (FSettingsInstance=Nil) then
    FSettingsInstance:=CreateSettingsResource;
  Result:=FSettingsInstance;
end;

Function TCalendarAPI.CreateSettingsResource : TSettingsResource;

begin
  Result:=CreateSettingsResource(Self);
end;


Function TCalendarAPI.CreateSettingsResource(AOwner : TComponent) : TSettingsResource;

begin
  Result:=TSettingsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TCalendarAPI.RegisterAPI;
end.
