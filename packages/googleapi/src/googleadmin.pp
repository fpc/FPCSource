unit googleadmin;
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
  TActivities = class;
  TActivitiesArray = Array of TActivities;
  TActivitiesitems = class;
  TActivitiesitemsArray = Array of TActivitiesitems;
  TActivity = class;
  TActivityArray = Array of TActivity;
  TActivityactor = class;
  TActivityactorArray = Array of TActivityactor;
  TActivityevents = class;
  TActivityeventsArray = Array of TActivityevents;
  TActivityeventsparameters = class;
  TActivityeventsparametersArray = Array of TActivityeventsparameters;
  TActivityeventsparametersmultiIntValue = class;
  TActivityeventsparametersmultiIntValueArray = Array of TActivityeventsparametersmultiIntValue;
  TActivityeventsparametersmultiValue = class;
  TActivityeventsparametersmultiValueArray = Array of TActivityeventsparametersmultiValue;
  TActivityid = class;
  TActivityidArray = Array of TActivityid;
  TChannel = class;
  TChannelArray = Array of TChannel;
  TChannelparams = class;
  TChannelparamsArray = Array of TChannelparams;
  TUsageReport = class;
  TUsageReportArray = Array of TUsageReport;
  TUsageReportentity = class;
  TUsageReportentityArray = Array of TUsageReportentity;
  TUsageReportparameters = class;
  TUsageReportparametersArray = Array of TUsageReportparameters;
  TUsageReportparametersmsgValue = class;
  TUsageReportparametersmsgValueArray = Array of TUsageReportparametersmsgValue;
  TUsageReports = class;
  TUsageReportsArray = Array of TUsageReports;
  TUsageReportsusageReports = class;
  TUsageReportsusageReportsArray = Array of TUsageReportsusageReports;
  TUsageReportswarnings = class;
  TUsageReportswarningsArray = Array of TUsageReportswarnings;
  TUsageReportswarningsdata = class;
  TUsageReportswarningsdataArray = Array of TUsageReportswarningsdata;
  
  { --------------------------------------------------------------------
    TActivities
    --------------------------------------------------------------------}
  
  TActivities = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TActivitiesitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TActivitiesitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TActivitiesitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TActivitiesClass = Class of TActivities;
  
  { --------------------------------------------------------------------
    TActivitiesitems
    --------------------------------------------------------------------}
  
  TActivitiesitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TActivitiesitemsClass = Class of TActivitiesitems;
  
  { --------------------------------------------------------------------
    TActivity
    --------------------------------------------------------------------}
  
  TActivity = Class(TGoogleBaseObject)
  Private
    Factor : TActivityactor;
    Fetag : string;
    Fevents : TActivityevents;
    Fid : TActivityid;
    FipAddress : string;
    Fkind : string;
    FownerDomain : string;
  Protected
    //Property setters
    Procedure Setactor(AIndex : Integer; AValue : TActivityactor); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setevents(AIndex : Integer; AValue : TActivityevents); virtual;
    Procedure Setid(AIndex : Integer; AValue : TActivityid); virtual;
    Procedure SetipAddress(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetownerDomain(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property actor : TActivityactor Index 0 Read Factor Write Setactor;
    Property etag : string Index 8 Read Fetag Write Setetag;
    Property events : TActivityevents Index 16 Read Fevents Write Setevents;
    Property id : TActivityid Index 24 Read Fid Write Setid;
    Property ipAddress : string Index 32 Read FipAddress Write SetipAddress;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property ownerDomain : string Index 48 Read FownerDomain Write SetownerDomain;
  end;
  TActivityClass = Class of TActivity;
  
  { --------------------------------------------------------------------
    TActivityactor
    --------------------------------------------------------------------}
  
  TActivityactor = Class(TGoogleBaseObject)
  Private
    FcallerType : string;
    Femail : string;
    Fkey : string;
    FprofileId : string;
  Protected
    //Property setters
    Procedure SetcallerType(AIndex : Integer; AValue : string); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure SetprofileId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property callerType : string Index 0 Read FcallerType Write SetcallerType;
    Property email : string Index 8 Read Femail Write Setemail;
    Property key : string Index 16 Read Fkey Write Setkey;
    Property profileId : string Index 24 Read FprofileId Write SetprofileId;
  end;
  TActivityactorClass = Class of TActivityactor;
  
  { --------------------------------------------------------------------
    TActivityevents
    --------------------------------------------------------------------}
  
  TActivityevents = Class(TGoogleBaseObject)
  Private
    Fname : string;
    Fparameters : TActivityeventsparameters;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setparameters(AIndex : Integer; AValue : TActivityeventsparameters); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property parameters : TActivityeventsparameters Index 8 Read Fparameters Write Setparameters;
    Property _type : string Index 16 Read F_type Write Set_type;
  end;
  TActivityeventsClass = Class of TActivityevents;
  
  { --------------------------------------------------------------------
    TActivityeventsparameters
    --------------------------------------------------------------------}
  
  TActivityeventsparameters = Class(TGoogleBaseObject)
  Private
    FboolValue : boolean;
    FintValue : string;
    FmultiIntValue : TActivityeventsparametersmultiIntValue;
    FmultiValue : TActivityeventsparametersmultiValue;
    Fname : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure SetboolValue(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetintValue(AIndex : Integer; AValue : string); virtual;
    Procedure SetmultiIntValue(AIndex : Integer; AValue : TActivityeventsparametersmultiIntValue); virtual;
    Procedure SetmultiValue(AIndex : Integer; AValue : TActivityeventsparametersmultiValue); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property boolValue : boolean Index 0 Read FboolValue Write SetboolValue;
    Property intValue : string Index 8 Read FintValue Write SetintValue;
    Property multiIntValue : TActivityeventsparametersmultiIntValue Index 16 Read FmultiIntValue Write SetmultiIntValue;
    Property multiValue : TActivityeventsparametersmultiValue Index 24 Read FmultiValue Write SetmultiValue;
    Property name : string Index 32 Read Fname Write Setname;
    Property value : string Index 40 Read Fvalue Write Setvalue;
  end;
  TActivityeventsparametersClass = Class of TActivityeventsparameters;
  
  { --------------------------------------------------------------------
    TActivityeventsparametersmultiIntValue
    --------------------------------------------------------------------}
  
  TActivityeventsparametersmultiIntValue = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TActivityeventsparametersmultiIntValueClass = Class of TActivityeventsparametersmultiIntValue;
  
  { --------------------------------------------------------------------
    TActivityeventsparametersmultiValue
    --------------------------------------------------------------------}
  
  TActivityeventsparametersmultiValue = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TActivityeventsparametersmultiValueClass = Class of TActivityeventsparametersmultiValue;
  
  { --------------------------------------------------------------------
    TActivityid
    --------------------------------------------------------------------}
  
  TActivityid = Class(TGoogleBaseObject)
  Private
    FapplicationName : string;
    FcustomerId : string;
    Ftime : TDatetime;
    FuniqueQualifier : string;
  Protected
    //Property setters
    Procedure SetapplicationName(AIndex : Integer; AValue : string); virtual;
    Procedure SetcustomerId(AIndex : Integer; AValue : string); virtual;
    Procedure Settime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetuniqueQualifier(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property applicationName : string Index 0 Read FapplicationName Write SetapplicationName;
    Property customerId : string Index 8 Read FcustomerId Write SetcustomerId;
    Property time : TDatetime Index 16 Read Ftime Write Settime;
    Property uniqueQualifier : string Index 24 Read FuniqueQualifier Write SetuniqueQualifier;
  end;
  TActivityidClass = Class of TActivityid;
  
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
    TUsageReport
    --------------------------------------------------------------------}
  
  TUsageReport = Class(TGoogleBaseObject)
  Private
    Fdate : string;
    Fentity : TUsageReportentity;
    Fetag : string;
    Fkind : string;
    Fparameters : TUsageReportparameters;
  Protected
    //Property setters
    Procedure Setdate(AIndex : Integer; AValue : string); virtual;
    Procedure Setentity(AIndex : Integer; AValue : TUsageReportentity); virtual;
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setparameters(AIndex : Integer; AValue : TUsageReportparameters); virtual;
  Public
  Published
    Property date : string Index 0 Read Fdate Write Setdate;
    Property entity : TUsageReportentity Index 8 Read Fentity Write Setentity;
    Property etag : string Index 16 Read Fetag Write Setetag;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property parameters : TUsageReportparameters Index 32 Read Fparameters Write Setparameters;
  end;
  TUsageReportClass = Class of TUsageReport;
  
  { --------------------------------------------------------------------
    TUsageReportentity
    --------------------------------------------------------------------}
  
  TUsageReportentity = Class(TGoogleBaseObject)
  Private
    FcustomerId : string;
    FprofileId : string;
    F_type : string;
    FuserEmail : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetcustomerId(AIndex : Integer; AValue : string); virtual;
    Procedure SetprofileId(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure SetuserEmail(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property customerId : string Index 0 Read FcustomerId Write SetcustomerId;
    Property profileId : string Index 8 Read FprofileId Write SetprofileId;
    Property _type : string Index 16 Read F_type Write Set_type;
    Property userEmail : string Index 24 Read FuserEmail Write SetuserEmail;
  end;
  TUsageReportentityClass = Class of TUsageReportentity;
  
  { --------------------------------------------------------------------
    TUsageReportparameters
    --------------------------------------------------------------------}
  
  TUsageReportparameters = Class(TGoogleBaseObject)
  Private
    FboolValue : boolean;
    FdatetimeValue : TDatetime;
    FintValue : string;
    FmsgValue : TUsageReportparametersmsgValue;
    Fname : string;
    FstringValue : string;
  Protected
    //Property setters
    Procedure SetboolValue(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdatetimeValue(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetintValue(AIndex : Integer; AValue : string); virtual;
    Procedure SetmsgValue(AIndex : Integer; AValue : TUsageReportparametersmsgValue); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetstringValue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property boolValue : boolean Index 0 Read FboolValue Write SetboolValue;
    Property datetimeValue : TDatetime Index 8 Read FdatetimeValue Write SetdatetimeValue;
    Property intValue : string Index 16 Read FintValue Write SetintValue;
    Property msgValue : TUsageReportparametersmsgValue Index 24 Read FmsgValue Write SetmsgValue;
    Property name : string Index 32 Read Fname Write Setname;
    Property stringValue : string Index 40 Read FstringValue Write SetstringValue;
  end;
  TUsageReportparametersClass = Class of TUsageReportparameters;
  
  { --------------------------------------------------------------------
    TUsageReportparametersmsgValue
    --------------------------------------------------------------------}
  
  TUsageReportparametersmsgValue = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUsageReportparametersmsgValueClass = Class of TUsageReportparametersmsgValue;
  
  { --------------------------------------------------------------------
    TUsageReports
    --------------------------------------------------------------------}
  
  TUsageReports = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fkind : string;
    FnextPageToken : string;
    FusageReports : TUsageReportsusageReports;
    Fwarnings : TUsageReportswarnings;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetusageReports(AIndex : Integer; AValue : TUsageReportsusageReports); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TUsageReportswarnings); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
    Property usageReports : TUsageReportsusageReports Index 24 Read FusageReports Write SetusageReports;
    Property warnings : TUsageReportswarnings Index 32 Read Fwarnings Write Setwarnings;
  end;
  TUsageReportsClass = Class of TUsageReports;
  
  { --------------------------------------------------------------------
    TUsageReportsusageReports
    --------------------------------------------------------------------}
  
  TUsageReportsusageReports = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUsageReportsusageReportsClass = Class of TUsageReportsusageReports;
  
  { --------------------------------------------------------------------
    TUsageReportswarnings
    --------------------------------------------------------------------}
  
  TUsageReportswarnings = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TUsageReportswarningsdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TUsageReportswarningsdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TUsageReportswarningsdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TUsageReportswarningsClass = Class of TUsageReportswarnings;
  
  { --------------------------------------------------------------------
    TUsageReportswarningsdata
    --------------------------------------------------------------------}
  
  TUsageReportswarningsdata = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TUsageReportswarningsdataClass = Class of TUsageReportswarningsdata;
  
  { --------------------------------------------------------------------
    TActivitiesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TActivitiesResource, method List
  
  TActivitiesListOptions = Record
    actorIpAddress : string;
    customerId : string;
    endTime : string;
    eventName : string;
    filters : string;
    maxResults : integer;
    pageToken : string;
    startTime : string;
  end;
  
  
  //Optional query Options for TActivitiesResource, method Watch
  
  TActivitiesWatchOptions = Record
    actorIpAddress : string;
    customerId : string;
    endTime : string;
    eventName : string;
    filters : string;
    maxResults : integer;
    pageToken : string;
    startTime : string;
  end;
  
  TActivitiesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(applicationName: string; userKey: string; AQuery : string  = '') : TActivities;
    Function List(applicationName: string; userKey: string; AQuery : TActivitieslistOptions) : TActivities;
    Function Watch(applicationName: string; userKey: string; aChannel : TChannel; AQuery : string  = '') : TChannel;
    Function Watch(applicationName: string; userKey: string; aChannel : TChannel; AQuery : TActivitieswatchOptions) : TChannel;
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
    TCustomerUsageReportsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCustomerUsageReportsResource, method Get
  
  TCustomerUsageReportsGetOptions = Record
    customerId : string;
    pageToken : string;
    parameters : string;
  end;
  
  TCustomerUsageReportsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(date: string; AQuery : string  = '') : TUsageReports;
    Function Get(date: string; AQuery : TCustomerUsageReportsgetOptions) : TUsageReports;
  end;
  
  
  { --------------------------------------------------------------------
    TUserUsageReportResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUserUsageReportResource, method Get
  
  TUserUsageReportGetOptions = Record
    customerId : string;
    filters : string;
    maxResults : integer;
    pageToken : string;
    parameters : string;
  end;
  
  TUserUsageReportResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(date: string; userKey: string; AQuery : string  = '') : TUsageReports;
    Function Get(date: string; userKey: string; AQuery : TUserUsageReportgetOptions) : TUsageReports;
  end;
  
  
  { --------------------------------------------------------------------
    TAdminAPI
    --------------------------------------------------------------------}
  
  TAdminAPI = Class(TGoogleAPI)
  Private
    FActivitiesInstance : TActivitiesResource;
    FChannelsInstance : TChannelsResource;
    FCustomerUsageReportsInstance : TCustomerUsageReportsResource;
    FUserUsageReportInstance : TUserUsageReportResource;
    Function GetActivitiesInstance : TActivitiesResource;virtual;
    Function GetChannelsInstance : TChannelsResource;virtual;
    Function GetCustomerUsageReportsInstance : TCustomerUsageReportsResource;virtual;
    Function GetUserUsageReportInstance : TUserUsageReportResource;virtual;
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
    Function CreateActivitiesResource(AOwner : TComponent) : TActivitiesResource;virtual;overload;
    Function CreateActivitiesResource : TActivitiesResource;virtual;overload;
    Function CreateChannelsResource(AOwner : TComponent) : TChannelsResource;virtual;overload;
    Function CreateChannelsResource : TChannelsResource;virtual;overload;
    Function CreateCustomerUsageReportsResource(AOwner : TComponent) : TCustomerUsageReportsResource;virtual;overload;
    Function CreateCustomerUsageReportsResource : TCustomerUsageReportsResource;virtual;overload;
    Function CreateUserUsageReportResource(AOwner : TComponent) : TUserUsageReportResource;virtual;overload;
    Function CreateUserUsageReportResource : TUserUsageReportResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ActivitiesResource : TActivitiesResource Read GetActivitiesInstance;
    Property ChannelsResource : TChannelsResource Read GetChannelsInstance;
    Property CustomerUsageReportsResource : TCustomerUsageReportsResource Read GetCustomerUsageReportsInstance;
    Property UserUsageReportResource : TUserUsageReportResource Read GetUserUsageReportInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TActivities
  --------------------------------------------------------------------}


Procedure TActivities.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivities.Setitems(AIndex : Integer; AValue : TActivitiesitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivities.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivities.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivitiesitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TActivity
  --------------------------------------------------------------------}


Procedure TActivity.Setactor(AIndex : Integer; AValue : TActivityactor); 

begin
  If (Factor=AValue) then exit;
  Factor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setevents(AIndex : Integer; AValue : TActivityevents); 

begin
  If (Fevents=AValue) then exit;
  Fevents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setid(AIndex : Integer; AValue : TActivityid); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.SetipAddress(AIndex : Integer; AValue : string); 

begin
  If (FipAddress=AValue) then exit;
  FipAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.SetownerDomain(AIndex : Integer; AValue : string); 

begin
  If (FownerDomain=AValue) then exit;
  FownerDomain:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityactor
  --------------------------------------------------------------------}


Procedure TActivityactor.SetcallerType(AIndex : Integer; AValue : string); 

begin
  If (FcallerType=AValue) then exit;
  FcallerType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityactor.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityactor.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityactor.SetprofileId(AIndex : Integer; AValue : string); 

begin
  If (FprofileId=AValue) then exit;
  FprofileId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityevents
  --------------------------------------------------------------------}


Procedure TActivityevents.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityevents.Setparameters(AIndex : Integer; AValue : TActivityeventsparameters); 

begin
  If (Fparameters=AValue) then exit;
  Fparameters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityevents.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TActivityevents.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TActivityeventsparameters
  --------------------------------------------------------------------}


Procedure TActivityeventsparameters.SetboolValue(AIndex : Integer; AValue : boolean); 

begin
  If (FboolValue=AValue) then exit;
  FboolValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityeventsparameters.SetintValue(AIndex : Integer; AValue : string); 

begin
  If (FintValue=AValue) then exit;
  FintValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityeventsparameters.SetmultiIntValue(AIndex : Integer; AValue : TActivityeventsparametersmultiIntValue); 

begin
  If (FmultiIntValue=AValue) then exit;
  FmultiIntValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityeventsparameters.SetmultiValue(AIndex : Integer; AValue : TActivityeventsparametersmultiValue); 

begin
  If (FmultiValue=AValue) then exit;
  FmultiValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityeventsparameters.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityeventsparameters.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityeventsparametersmultiIntValue
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TActivityeventsparametersmultiValue
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TActivityid
  --------------------------------------------------------------------}


Procedure TActivityid.SetapplicationName(AIndex : Integer; AValue : string); 

begin
  If (FapplicationName=AValue) then exit;
  FapplicationName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityid.SetcustomerId(AIndex : Integer; AValue : string); 

begin
  If (FcustomerId=AValue) then exit;
  FcustomerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityid.Settime(AIndex : Integer; AValue : TDatetime); 

begin
  If (Ftime=AValue) then exit;
  Ftime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityid.SetuniqueQualifier(AIndex : Integer; AValue : string); 

begin
  If (FuniqueQualifier=AValue) then exit;
  FuniqueQualifier:=AValue;
  MarkPropertyChanged(AIndex);
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
  TUsageReport
  --------------------------------------------------------------------}


Procedure TUsageReport.Setdate(AIndex : Integer; AValue : string); 

begin
  If (Fdate=AValue) then exit;
  Fdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReport.Setentity(AIndex : Integer; AValue : TUsageReportentity); 

begin
  If (Fentity=AValue) then exit;
  Fentity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReport.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReport.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReport.Setparameters(AIndex : Integer; AValue : TUsageReportparameters); 

begin
  If (Fparameters=AValue) then exit;
  Fparameters:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUsageReportentity
  --------------------------------------------------------------------}


Procedure TUsageReportentity.SetcustomerId(AIndex : Integer; AValue : string); 

begin
  If (FcustomerId=AValue) then exit;
  FcustomerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReportentity.SetprofileId(AIndex : Integer; AValue : string); 

begin
  If (FprofileId=AValue) then exit;
  FprofileId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReportentity.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReportentity.SetuserEmail(AIndex : Integer; AValue : string); 

begin
  If (FuserEmail=AValue) then exit;
  FuserEmail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TUsageReportentity.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TUsageReportparameters
  --------------------------------------------------------------------}


Procedure TUsageReportparameters.SetboolValue(AIndex : Integer; AValue : boolean); 

begin
  If (FboolValue=AValue) then exit;
  FboolValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReportparameters.SetdatetimeValue(AIndex : Integer; AValue : TDatetime); 

begin
  If (FdatetimeValue=AValue) then exit;
  FdatetimeValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReportparameters.SetintValue(AIndex : Integer; AValue : string); 

begin
  If (FintValue=AValue) then exit;
  FintValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReportparameters.SetmsgValue(AIndex : Integer; AValue : TUsageReportparametersmsgValue); 

begin
  If (FmsgValue=AValue) then exit;
  FmsgValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReportparameters.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReportparameters.SetstringValue(AIndex : Integer; AValue : string); 

begin
  If (FstringValue=AValue) then exit;
  FstringValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUsageReportparametersmsgValue
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUsageReports
  --------------------------------------------------------------------}


Procedure TUsageReports.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReports.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReports.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReports.SetusageReports(AIndex : Integer; AValue : TUsageReportsusageReports); 

begin
  If (FusageReports=AValue) then exit;
  FusageReports:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReports.Setwarnings(AIndex : Integer; AValue : TUsageReportswarnings); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUsageReportsusageReports
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUsageReportswarnings
  --------------------------------------------------------------------}


Procedure TUsageReportswarnings.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReportswarnings.Setdata(AIndex : Integer; AValue : TUsageReportswarningsdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReportswarnings.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUsageReportswarningsdata
  --------------------------------------------------------------------}


Procedure TUsageReportswarningsdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUsageReportswarningsdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivitiesResource
  --------------------------------------------------------------------}


Class Function TActivitiesResource.ResourceName : String;

begin
  Result:='activities';
end;

Class Function TActivitiesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadminAPI;
end;

Function TActivitiesResource.List(applicationName: string; userKey: string; AQuery : string = '') : TActivities;

Const
  _HTTPMethod = 'GET';
  _Path       = 'activity/users/{userKey}/applications/{applicationName}';
  _Methodid   = 'reports.activities.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['applicationName',applicationName,'userKey',userKey]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TActivities) as TActivities;
end;


Function TActivitiesResource.List(applicationName: string; userKey: string; AQuery : TActivitieslistOptions) : TActivities;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'actorIpAddress',AQuery.actorIpAddress);
  AddToQuery(_Q,'customerId',AQuery.customerId);
  AddToQuery(_Q,'endTime',AQuery.endTime);
  AddToQuery(_Q,'eventName',AQuery.eventName);
  AddToQuery(_Q,'filters',AQuery.filters);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startTime',AQuery.startTime);
  Result:=List(applicationName,userKey,_Q);
end;

Function TActivitiesResource.Watch(applicationName: string; userKey: string; aChannel : TChannel; AQuery : string = '') : TChannel;

Const
  _HTTPMethod = 'POST';
  _Path       = 'activity/users/{userKey}/applications/{applicationName}/watch';
  _Methodid   = 'reports.activities.watch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['applicationName',applicationName,'userKey',userKey]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aChannel,TChannel) as TChannel;
end;


Function TActivitiesResource.Watch(applicationName: string; userKey: string; aChannel : TChannel; AQuery : TActivitieswatchOptions) : TChannel;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'actorIpAddress',AQuery.actorIpAddress);
  AddToQuery(_Q,'customerId',AQuery.customerId);
  AddToQuery(_Q,'endTime',AQuery.endTime);
  AddToQuery(_Q,'eventName',AQuery.eventName);
  AddToQuery(_Q,'filters',AQuery.filters);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startTime',AQuery.startTime);
  Result:=Watch(applicationName,userKey,aChannel,_Q);
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
  Result:=TadminAPI;
end;

Procedure TChannelsResource.Stop(aChannel : TChannel);

Const
  _HTTPMethod = 'POST';
  _Path       = '/admin/reports_v1/channels/stop';
  _Methodid   = 'admin.channels.stop';

begin
  ServiceCall(_HTTPMethod,_Path,'',aChannel,Nil);
end;



{ --------------------------------------------------------------------
  TCustomerUsageReportsResource
  --------------------------------------------------------------------}


Class Function TCustomerUsageReportsResource.ResourceName : String;

begin
  Result:='customerUsageReports';
end;

Class Function TCustomerUsageReportsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadminAPI;
end;

Function TCustomerUsageReportsResource.Get(date: string; AQuery : string = '') : TUsageReports;

Const
  _HTTPMethod = 'GET';
  _Path       = 'usage/dates/{date}';
  _Methodid   = 'reports.customerUsageReports.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['date',date]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TUsageReports) as TUsageReports;
end;


Function TCustomerUsageReportsResource.Get(date: string; AQuery : TCustomerUsageReportsgetOptions) : TUsageReports;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'customerId',AQuery.customerId);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'parameters',AQuery.parameters);
  Result:=Get(date,_Q);
end;



{ --------------------------------------------------------------------
  TUserUsageReportResource
  --------------------------------------------------------------------}


Class Function TUserUsageReportResource.ResourceName : String;

begin
  Result:='userUsageReport';
end;

Class Function TUserUsageReportResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadminAPI;
end;

Function TUserUsageReportResource.Get(date: string; userKey: string; AQuery : string = '') : TUsageReports;

Const
  _HTTPMethod = 'GET';
  _Path       = 'usage/users/{userKey}/dates/{date}';
  _Methodid   = 'reports.userUsageReport.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['date',date,'userKey',userKey]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TUsageReports) as TUsageReports;
end;


Function TUserUsageReportResource.Get(date: string; userKey: string; AQuery : TUserUsageReportgetOptions) : TUsageReports;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'customerId',AQuery.customerId);
  AddToQuery(_Q,'filters',AQuery.filters);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'parameters',AQuery.parameters);
  Result:=Get(date,userKey,_Q);
end;



{ --------------------------------------------------------------------
  TAdminAPI
  --------------------------------------------------------------------}

Class Function TAdminAPI.APIName : String;

begin
  Result:='admin';
end;

Class Function TAdminAPI.APIVersion : String;

begin
  Result:='reports_v1';
end;

Class Function TAdminAPI.APIRevision : String;

begin
  Result:='20150429';
end;

Class Function TAdminAPI.APIID : String;

begin
  Result:='admin:reports_v1';
end;

Class Function TAdminAPI.APITitle : String;

begin
  Result:='Admin Reports API';
end;

Class Function TAdminAPI.APIDescription : String;

begin
  Result:='Allows the administrators of Google Apps customers to fetch reports about the usage, collaboration, security and risk for their users.';
end;

Class Function TAdminAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TAdminAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TAdminAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TAdminAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TAdminAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/admin-sdk/reports/';
end;

Class Function TAdminAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TAdminAPI.APIbasePath : string;

begin
  Result:='/admin/reports/v1/';
end;

Class Function TAdminAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/admin/reports/v1/';
end;

Class Function TAdminAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TAdminAPI.APIservicePath : string;

begin
  Result:='admin/reports/v1/';
end;

Class Function TAdminAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TAdminAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/admin.reports.audit.readonly';
  Result[0].Description:='View audit reports of Google Apps for your domain';
  Result[1].Name:='https://www.googleapis.com/auth/admin.reports.usage.readonly';
  Result[1].Description:='View usage reports of Google Apps for your domain';
  
end;

Class Function TAdminAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TAdminAPI.RegisterAPIResources;

begin
  TActivities.RegisterObject;
  TActivitiesitems.RegisterObject;
  TActivity.RegisterObject;
  TActivityactor.RegisterObject;
  TActivityevents.RegisterObject;
  TActivityeventsparameters.RegisterObject;
  TActivityeventsparametersmultiIntValue.RegisterObject;
  TActivityeventsparametersmultiValue.RegisterObject;
  TActivityid.RegisterObject;
  TChannel.RegisterObject;
  TChannelparams.RegisterObject;
  TUsageReport.RegisterObject;
  TUsageReportentity.RegisterObject;
  TUsageReportparameters.RegisterObject;
  TUsageReportparametersmsgValue.RegisterObject;
  TUsageReports.RegisterObject;
  TUsageReportsusageReports.RegisterObject;
  TUsageReportswarnings.RegisterObject;
  TUsageReportswarningsdata.RegisterObject;
end;


Function TAdminAPI.GetActivitiesInstance : TActivitiesResource;

begin
  if (FActivitiesInstance=Nil) then
    FActivitiesInstance:=CreateActivitiesResource;
  Result:=FActivitiesInstance;
end;

Function TAdminAPI.CreateActivitiesResource : TActivitiesResource;

begin
  Result:=CreateActivitiesResource(Self);
end;


Function TAdminAPI.CreateActivitiesResource(AOwner : TComponent) : TActivitiesResource;

begin
  Result:=TActivitiesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAdminAPI.GetChannelsInstance : TChannelsResource;

begin
  if (FChannelsInstance=Nil) then
    FChannelsInstance:=CreateChannelsResource;
  Result:=FChannelsInstance;
end;

Function TAdminAPI.CreateChannelsResource : TChannelsResource;

begin
  Result:=CreateChannelsResource(Self);
end;


Function TAdminAPI.CreateChannelsResource(AOwner : TComponent) : TChannelsResource;

begin
  Result:=TChannelsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAdminAPI.GetCustomerUsageReportsInstance : TCustomerUsageReportsResource;

begin
  if (FCustomerUsageReportsInstance=Nil) then
    FCustomerUsageReportsInstance:=CreateCustomerUsageReportsResource;
  Result:=FCustomerUsageReportsInstance;
end;

Function TAdminAPI.CreateCustomerUsageReportsResource : TCustomerUsageReportsResource;

begin
  Result:=CreateCustomerUsageReportsResource(Self);
end;


Function TAdminAPI.CreateCustomerUsageReportsResource(AOwner : TComponent) : TCustomerUsageReportsResource;

begin
  Result:=TCustomerUsageReportsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAdminAPI.GetUserUsageReportInstance : TUserUsageReportResource;

begin
  if (FUserUsageReportInstance=Nil) then
    FUserUsageReportInstance:=CreateUserUsageReportResource;
  Result:=FUserUsageReportInstance;
end;

Function TAdminAPI.CreateUserUsageReportResource : TUserUsageReportResource;

begin
  Result:=CreateUserUsageReportResource(Self);
end;


Function TAdminAPI.CreateUserUsageReportResource(AOwner : TComponent) : TUserUsageReportResource;

begin
  Result:=TUserUsageReportResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TAdminAPI.RegisterAPI;
end.
