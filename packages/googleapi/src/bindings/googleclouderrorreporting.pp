unit googleclouderrorreporting;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TSourceLocation = Class;
  TErrorGroupStats = Class;
  TErrorContext = Class;
  TServiceContext = Class;
  TErrorGroup = Class;
  TTrackingIssue = Class;
  TDeleteEventsResponse = Class;
  TErrorEvent = Class;
  TListEventsResponse = Class;
  TTimedCount = Class;
  THttpRequestContext = Class;
  TListGroupStatsResponse = Class;
  TSourceLocationArray = Array of TSourceLocation;
  TErrorGroupStatsArray = Array of TErrorGroupStats;
  TErrorContextArray = Array of TErrorContext;
  TServiceContextArray = Array of TServiceContext;
  TErrorGroupArray = Array of TErrorGroup;
  TTrackingIssueArray = Array of TTrackingIssue;
  TDeleteEventsResponseArray = Array of TDeleteEventsResponse;
  TErrorEventArray = Array of TErrorEvent;
  TListEventsResponseArray = Array of TListEventsResponse;
  TTimedCountArray = Array of TTimedCount;
  THttpRequestContextArray = Array of THttpRequestContext;
  TListGroupStatsResponseArray = Array of TListGroupStatsResponse;
  //Anonymous types, using auto-generated names
  TErrorGroupStatsTypeaffectedServicesArray = Array of TServiceContext;
  TErrorGroupStatsTypetimedCountsArray = Array of TTimedCount;
  TErrorGroupTypetrackingIssuesArray = Array of TTrackingIssue;
  TListEventsResponseTypeerrorEventsArray = Array of TErrorEvent;
  TListGroupStatsResponseTypeerrorGroupStatsArray = Array of TErrorGroupStats;
  
  { --------------------------------------------------------------------
    TSourceLocation
    --------------------------------------------------------------------}
  
  TSourceLocation = Class(TGoogleBaseObject)
  Private
    FfilePath : String;
    FfunctionName : String;
    FlineNumber : integer;
  Protected
    //Property setters
    Procedure SetfilePath(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfunctionName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlineNumber(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property filePath : String Index 0 Read FfilePath Write SetfilePath;
    Property functionName : String Index 8 Read FfunctionName Write SetfunctionName;
    Property lineNumber : integer Index 16 Read FlineNumber Write SetlineNumber;
  end;
  TSourceLocationClass = Class of TSourceLocation;
  
  { --------------------------------------------------------------------
    TErrorGroupStats
    --------------------------------------------------------------------}
  
  TErrorGroupStats = Class(TGoogleBaseObject)
  Private
    Frepresentative : TErrorEvent;
    FnumAffectedServices : integer;
    FaffectedUsersCount : String;
    Fcount : String;
    FfirstSeenTime : String;
    FlastSeenTime : String;
    Fgroup : TErrorGroup;
    FaffectedServices : TErrorGroupStatsTypeaffectedServicesArray;
    FtimedCounts : TErrorGroupStatsTypetimedCountsArray;
  Protected
    //Property setters
    Procedure Setrepresentative(AIndex : Integer; const AValue : TErrorEvent); virtual;
    Procedure SetnumAffectedServices(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetaffectedUsersCount(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcount(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfirstSeenTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlastSeenTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setgroup(AIndex : Integer; const AValue : TErrorGroup); virtual;
    Procedure SetaffectedServices(AIndex : Integer; const AValue : TErrorGroupStatsTypeaffectedServicesArray); virtual;
    Procedure SettimedCounts(AIndex : Integer; const AValue : TErrorGroupStatsTypetimedCountsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property representative : TErrorEvent Index 0 Read Frepresentative Write Setrepresentative;
    Property numAffectedServices : integer Index 8 Read FnumAffectedServices Write SetnumAffectedServices;
    Property affectedUsersCount : String Index 16 Read FaffectedUsersCount Write SetaffectedUsersCount;
    Property count : String Index 24 Read Fcount Write Setcount;
    Property firstSeenTime : String Index 32 Read FfirstSeenTime Write SetfirstSeenTime;
    Property lastSeenTime : String Index 40 Read FlastSeenTime Write SetlastSeenTime;
    Property group : TErrorGroup Index 48 Read Fgroup Write Setgroup;
    Property affectedServices : TErrorGroupStatsTypeaffectedServicesArray Index 56 Read FaffectedServices Write SetaffectedServices;
    Property timedCounts : TErrorGroupStatsTypetimedCountsArray Index 64 Read FtimedCounts Write SettimedCounts;
  end;
  TErrorGroupStatsClass = Class of TErrorGroupStats;
  
  { --------------------------------------------------------------------
    TErrorContext
    --------------------------------------------------------------------}
  
  TErrorContext = Class(TGoogleBaseObject)
  Private
    FhttpRequest : THttpRequestContext;
    FreportLocation : TSourceLocation;
    Fuser : String;
  Protected
    //Property setters
    Procedure SethttpRequest(AIndex : Integer; const AValue : THttpRequestContext); virtual;
    Procedure SetreportLocation(AIndex : Integer; const AValue : TSourceLocation); virtual;
    Procedure Setuser(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property httpRequest : THttpRequestContext Index 0 Read FhttpRequest Write SethttpRequest;
    Property reportLocation : TSourceLocation Index 8 Read FreportLocation Write SetreportLocation;
    Property user : String Index 16 Read Fuser Write Setuser;
  end;
  TErrorContextClass = Class of TErrorContext;
  
  { --------------------------------------------------------------------
    TServiceContext
    --------------------------------------------------------------------}
  
  TServiceContext = Class(TGoogleBaseObject)
  Private
    Fservice : String;
    Fversion : String;
  Protected
    //Property setters
    Procedure Setservice(AIndex : Integer; const AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property service : String Index 0 Read Fservice Write Setservice;
    Property version : String Index 8 Read Fversion Write Setversion;
  end;
  TServiceContextClass = Class of TServiceContext;
  
  { --------------------------------------------------------------------
    TErrorGroup
    --------------------------------------------------------------------}
  
  TErrorGroup = Class(TGoogleBaseObject)
  Private
    FgroupId : String;
    Fname : String;
    FtrackingIssues : TErrorGroupTypetrackingIssuesArray;
  Protected
    //Property setters
    Procedure SetgroupId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SettrackingIssues(AIndex : Integer; const AValue : TErrorGroupTypetrackingIssuesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property groupId : String Index 0 Read FgroupId Write SetgroupId;
    Property name : String Index 8 Read Fname Write Setname;
    Property trackingIssues : TErrorGroupTypetrackingIssuesArray Index 16 Read FtrackingIssues Write SettrackingIssues;
  end;
  TErrorGroupClass = Class of TErrorGroup;
  
  { --------------------------------------------------------------------
    TTrackingIssue
    --------------------------------------------------------------------}
  
  TTrackingIssue = Class(TGoogleBaseObject)
  Private
    Furl : String;
  Protected
    //Property setters
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property url : String Index 0 Read Furl Write Seturl;
  end;
  TTrackingIssueClass = Class of TTrackingIssue;
  
  { --------------------------------------------------------------------
    TDeleteEventsResponse
    --------------------------------------------------------------------}
  
  TDeleteEventsResponse = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDeleteEventsResponseClass = Class of TDeleteEventsResponse;
  
  { --------------------------------------------------------------------
    TErrorEvent
    --------------------------------------------------------------------}
  
  TErrorEvent = Class(TGoogleBaseObject)
  Private
    FserviceContext : TServiceContext;
    Fcontext : TErrorContext;
    FeventTime : String;
    Fmessage : String;
  Protected
    //Property setters
    Procedure SetserviceContext(AIndex : Integer; const AValue : TServiceContext); virtual;
    Procedure Setcontext(AIndex : Integer; const AValue : TErrorContext); virtual;
    Procedure SeteventTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property serviceContext : TServiceContext Index 0 Read FserviceContext Write SetserviceContext;
    Property context : TErrorContext Index 8 Read Fcontext Write Setcontext;
    Property eventTime : String Index 16 Read FeventTime Write SeteventTime;
    Property message : String Index 24 Read Fmessage Write Setmessage;
  end;
  TErrorEventClass = Class of TErrorEvent;
  
  { --------------------------------------------------------------------
    TListEventsResponse
    --------------------------------------------------------------------}
  
  TListEventsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    FerrorEvents : TListEventsResponseTypeerrorEventsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SeterrorEvents(AIndex : Integer; const AValue : TListEventsResponseTypeerrorEventsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property errorEvents : TListEventsResponseTypeerrorEventsArray Index 8 Read FerrorEvents Write SeterrorEvents;
  end;
  TListEventsResponseClass = Class of TListEventsResponse;
  
  { --------------------------------------------------------------------
    TTimedCount
    --------------------------------------------------------------------}
  
  TTimedCount = Class(TGoogleBaseObject)
  Private
    FstartTime : String;
    FendTime : String;
    Fcount : String;
  Protected
    //Property setters
    Procedure SetstartTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcount(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property startTime : String Index 0 Read FstartTime Write SetstartTime;
    Property endTime : String Index 8 Read FendTime Write SetendTime;
    Property count : String Index 16 Read Fcount Write Setcount;
  end;
  TTimedCountClass = Class of TTimedCount;
  
  { --------------------------------------------------------------------
    THttpRequestContext
    --------------------------------------------------------------------}
  
  THttpRequestContext = Class(TGoogleBaseObject)
  Private
    Fmethod : String;
    FresponseStatusCode : integer;
    FremoteIp : String;
    Furl : String;
    Freferrer : String;
    FuserAgent : String;
  Protected
    //Property setters
    Procedure Setmethod(AIndex : Integer; const AValue : String); virtual;
    Procedure SetresponseStatusCode(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetremoteIp(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setreferrer(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserAgent(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property method : String Index 0 Read Fmethod Write Setmethod;
    Property responseStatusCode : integer Index 8 Read FresponseStatusCode Write SetresponseStatusCode;
    Property remoteIp : String Index 16 Read FremoteIp Write SetremoteIp;
    Property url : String Index 24 Read Furl Write Seturl;
    Property referrer : String Index 32 Read Freferrer Write Setreferrer;
    Property userAgent : String Index 40 Read FuserAgent Write SetuserAgent;
  end;
  THttpRequestContextClass = Class of THttpRequestContext;
  
  { --------------------------------------------------------------------
    TListGroupStatsResponse
    --------------------------------------------------------------------}
  
  TListGroupStatsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    FerrorGroupStats : TListGroupStatsResponseTypeerrorGroupStatsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SeterrorGroupStats(AIndex : Integer; const AValue : TListGroupStatsResponseTypeerrorGroupStatsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property errorGroupStats : TListGroupStatsResponseTypeerrorGroupStatsArray Index 8 Read FerrorGroupStats Write SeterrorGroupStats;
  end;
  TListGroupStatsResponseClass = Class of TListGroupStatsResponse;
  
  { --------------------------------------------------------------------
    TProjectsEventsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsEventsResource, method List
  
  TProjectsEventsListOptions = Record
    timeRangeperiod : String;
    serviceFilterservice : String;
    groupId : String;
    serviceFilterversion : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  TProjectsEventsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(projectName: string; AQuery : string  = '') : TListEventsResponse;
    Function List(projectName: string; AQuery : TProjectsEventslistOptions) : TListEventsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsGroupsResource
    --------------------------------------------------------------------}
  
  TProjectsGroupsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Update(_name: string; aErrorGroup : TErrorGroup) : TErrorGroup;
    Function Get(groupName: string) : TErrorGroup;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsGroupStatsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsGroupStatsResource, method List
  
  TProjectsGroupStatsListOptions = Record
    alignment : String;
    timeRangeperiod : String;
    order : String;
    groupId : String;
    serviceFilterservice : String;
    alignmentTime : String;
    serviceFilterversion : String;
    pageSize : integer;
    timedCountDuration : String;
    pageToken : String;
  end;
  
  TProjectsGroupStatsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(projectName: string; AQuery : string  = '') : TListGroupStatsResponse;
    Function List(projectName: string; AQuery : TProjectsGroupStatslistOptions) : TListGroupStatsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Private
    FEventsInstance : TProjectsEventsResource;
    FGroupsInstance : TProjectsGroupsResource;
    FGroupStatsInstance : TProjectsGroupStatsResource;
    Function GetEventsInstance : TProjectsEventsResource;virtual;
    Function GetGroupsInstance : TProjectsGroupsResource;virtual;
    Function GetGroupStatsInstance : TProjectsGroupStatsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function DeleteEvents(projectName: string) : TDeleteEventsResponse;
    Function CreateEventsResource(AOwner : TComponent) : TProjectsEventsResource;virtual;overload;
    Function CreateEventsResource : TProjectsEventsResource;virtual;overload;
    Function CreateGroupsResource(AOwner : TComponent) : TProjectsGroupsResource;virtual;overload;
    Function CreateGroupsResource : TProjectsGroupsResource;virtual;overload;
    Function CreateGroupStatsResource(AOwner : TComponent) : TProjectsGroupStatsResource;virtual;overload;
    Function CreateGroupStatsResource : TProjectsGroupStatsResource;virtual;overload;
    Property EventsResource : TProjectsEventsResource Read GetEventsInstance;
    Property GroupsResource : TProjectsGroupsResource Read GetGroupsInstance;
    Property GroupStatsResource : TProjectsGroupStatsResource Read GetGroupStatsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TClouderrorreportingAPI
    --------------------------------------------------------------------}
  
  TClouderrorreportingAPI = Class(TGoogleAPI)
  Private
    FProjectsEventsInstance : TProjectsEventsResource;
    FProjectsGroupsInstance : TProjectsGroupsResource;
    FProjectsGroupStatsInstance : TProjectsGroupStatsResource;
    FProjectsInstance : TProjectsResource;
    Function GetProjectsEventsInstance : TProjectsEventsResource;virtual;
    Function GetProjectsGroupsInstance : TProjectsGroupsResource;virtual;
    Function GetProjectsGroupStatsInstance : TProjectsGroupStatsResource;virtual;
    Function GetProjectsInstance : TProjectsResource;virtual;
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
    Function CreateProjectsEventsResource(AOwner : TComponent) : TProjectsEventsResource;virtual;overload;
    Function CreateProjectsEventsResource : TProjectsEventsResource;virtual;overload;
    Function CreateProjectsGroupsResource(AOwner : TComponent) : TProjectsGroupsResource;virtual;overload;
    Function CreateProjectsGroupsResource : TProjectsGroupsResource;virtual;overload;
    Function CreateProjectsGroupStatsResource(AOwner : TComponent) : TProjectsGroupStatsResource;virtual;overload;
    Function CreateProjectsGroupStatsResource : TProjectsGroupStatsResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsEventsResource : TProjectsEventsResource Read GetProjectsEventsInstance;
    Property ProjectsGroupsResource : TProjectsGroupsResource Read GetProjectsGroupsInstance;
    Property ProjectsGroupStatsResource : TProjectsGroupStatsResource Read GetProjectsGroupStatsInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TSourceLocation
  --------------------------------------------------------------------}


Procedure TSourceLocation.SetfilePath(AIndex : Integer; const AValue : String); 

begin
  If (FfilePath=AValue) then exit;
  FfilePath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceLocation.SetfunctionName(AIndex : Integer; const AValue : String); 

begin
  If (FfunctionName=AValue) then exit;
  FfunctionName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceLocation.SetlineNumber(AIndex : Integer; const AValue : integer); 

begin
  If (FlineNumber=AValue) then exit;
  FlineNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TErrorGroupStats
  --------------------------------------------------------------------}


Procedure TErrorGroupStats.Setrepresentative(AIndex : Integer; const AValue : TErrorEvent); 

begin
  If (Frepresentative=AValue) then exit;
  Frepresentative:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorGroupStats.SetnumAffectedServices(AIndex : Integer; const AValue : integer); 

begin
  If (FnumAffectedServices=AValue) then exit;
  FnumAffectedServices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorGroupStats.SetaffectedUsersCount(AIndex : Integer; const AValue : String); 

begin
  If (FaffectedUsersCount=AValue) then exit;
  FaffectedUsersCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorGroupStats.Setcount(AIndex : Integer; const AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorGroupStats.SetfirstSeenTime(AIndex : Integer; const AValue : String); 

begin
  If (FfirstSeenTime=AValue) then exit;
  FfirstSeenTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorGroupStats.SetlastSeenTime(AIndex : Integer; const AValue : String); 

begin
  If (FlastSeenTime=AValue) then exit;
  FlastSeenTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorGroupStats.Setgroup(AIndex : Integer; const AValue : TErrorGroup); 

begin
  If (Fgroup=AValue) then exit;
  Fgroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorGroupStats.SetaffectedServices(AIndex : Integer; const AValue : TErrorGroupStatsTypeaffectedServicesArray); 

begin
  If (FaffectedServices=AValue) then exit;
  FaffectedServices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorGroupStats.SettimedCounts(AIndex : Integer; const AValue : TErrorGroupStatsTypetimedCountsArray); 

begin
  If (FtimedCounts=AValue) then exit;
  FtimedCounts:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TErrorGroupStats.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'affectedservices' : SetLength(FaffectedServices,ALength);
  'timedcounts' : SetLength(FtimedCounts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TErrorContext
  --------------------------------------------------------------------}


Procedure TErrorContext.SethttpRequest(AIndex : Integer; const AValue : THttpRequestContext); 

begin
  If (FhttpRequest=AValue) then exit;
  FhttpRequest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorContext.SetreportLocation(AIndex : Integer; const AValue : TSourceLocation); 

begin
  If (FreportLocation=AValue) then exit;
  FreportLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorContext.Setuser(AIndex : Integer; const AValue : String); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TServiceContext
  --------------------------------------------------------------------}


Procedure TServiceContext.Setservice(AIndex : Integer; const AValue : String); 

begin
  If (Fservice=AValue) then exit;
  Fservice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServiceContext.Setversion(AIndex : Integer; const AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TErrorGroup
  --------------------------------------------------------------------}


Procedure TErrorGroup.SetgroupId(AIndex : Integer; const AValue : String); 

begin
  If (FgroupId=AValue) then exit;
  FgroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorGroup.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorGroup.SettrackingIssues(AIndex : Integer; const AValue : TErrorGroupTypetrackingIssuesArray); 

begin
  If (FtrackingIssues=AValue) then exit;
  FtrackingIssues:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TErrorGroup.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'trackingissues' : SetLength(FtrackingIssues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTrackingIssue
  --------------------------------------------------------------------}


Procedure TTrackingIssue.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeleteEventsResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TErrorEvent
  --------------------------------------------------------------------}


Procedure TErrorEvent.SetserviceContext(AIndex : Integer; const AValue : TServiceContext); 

begin
  If (FserviceContext=AValue) then exit;
  FserviceContext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorEvent.Setcontext(AIndex : Integer; const AValue : TErrorContext); 

begin
  If (Fcontext=AValue) then exit;
  Fcontext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorEvent.SeteventTime(AIndex : Integer; const AValue : String); 

begin
  If (FeventTime=AValue) then exit;
  FeventTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorEvent.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListEventsResponse
  --------------------------------------------------------------------}


Procedure TListEventsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListEventsResponse.SeterrorEvents(AIndex : Integer; const AValue : TListEventsResponseTypeerrorEventsArray); 

begin
  If (FerrorEvents=AValue) then exit;
  FerrorEvents:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListEventsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errorevents' : SetLength(FerrorEvents,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTimedCount
  --------------------------------------------------------------------}


Procedure TTimedCount.SetstartTime(AIndex : Integer; const AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimedCount.SetendTime(AIndex : Integer; const AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimedCount.Setcount(AIndex : Integer; const AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THttpRequestContext
  --------------------------------------------------------------------}


Procedure THttpRequestContext.Setmethod(AIndex : Integer; const AValue : String); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpRequestContext.SetresponseStatusCode(AIndex : Integer; const AValue : integer); 

begin
  If (FresponseStatusCode=AValue) then exit;
  FresponseStatusCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpRequestContext.SetremoteIp(AIndex : Integer; const AValue : String); 

begin
  If (FremoteIp=AValue) then exit;
  FremoteIp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpRequestContext.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpRequestContext.Setreferrer(AIndex : Integer; const AValue : String); 

begin
  If (Freferrer=AValue) then exit;
  Freferrer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THttpRequestContext.SetuserAgent(AIndex : Integer; const AValue : String); 

begin
  If (FuserAgent=AValue) then exit;
  FuserAgent:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListGroupStatsResponse
  --------------------------------------------------------------------}


Procedure TListGroupStatsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListGroupStatsResponse.SeterrorGroupStats(AIndex : Integer; const AValue : TListGroupStatsResponseTypeerrorGroupStatsArray); 

begin
  If (FerrorGroupStats=AValue) then exit;
  FerrorGroupStats:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListGroupStatsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errorgroupstats' : SetLength(FerrorGroupStats,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProjectsEventsResource
  --------------------------------------------------------------------}


Class Function TProjectsEventsResource.ResourceName : String;

begin
  Result:='events';
end;

Class Function TProjectsEventsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclouderrorreportingAPI;
end;

Function TProjectsEventsResource.List(projectName: string; AQuery : string = '') : TListEventsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta1/{+projectName}/events';
  _Methodid   = 'clouderrorreporting.projects.events.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectName',projectName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListEventsResponse) as TListEventsResponse;
end;


Function TProjectsEventsResource.List(projectName: string; AQuery : TProjectsEventslistOptions) : TListEventsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'timeRange.period',AQuery.timeRangeperiod);
  AddToQuery(_Q,'serviceFilter.service',AQuery.serviceFilterservice);
  AddToQuery(_Q,'groupId',AQuery.groupId);
  AddToQuery(_Q,'serviceFilter.version',AQuery.serviceFilterversion);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(projectName,_Q);
end;



{ --------------------------------------------------------------------
  TProjectsGroupsResource
  --------------------------------------------------------------------}


Class Function TProjectsGroupsResource.ResourceName : String;

begin
  Result:='groups';
end;

Class Function TProjectsGroupsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclouderrorreportingAPI;
end;

Function TProjectsGroupsResource.Update(_name: string; aErrorGroup : TErrorGroup) : TErrorGroup;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v1beta1/{+name}';
  _Methodid   = 'clouderrorreporting.projects.groups.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aErrorGroup,TErrorGroup) as TErrorGroup;
end;

Function TProjectsGroupsResource.Get(groupName: string) : TErrorGroup;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta1/{+groupName}';
  _Methodid   = 'clouderrorreporting.projects.groups.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['groupName',groupName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TErrorGroup) as TErrorGroup;
end;



{ --------------------------------------------------------------------
  TProjectsGroupStatsResource
  --------------------------------------------------------------------}


Class Function TProjectsGroupStatsResource.ResourceName : String;

begin
  Result:='groupStats';
end;

Class Function TProjectsGroupStatsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclouderrorreportingAPI;
end;

Function TProjectsGroupStatsResource.List(projectName: string; AQuery : string = '') : TListGroupStatsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta1/{+projectName}/groupStats';
  _Methodid   = 'clouderrorreporting.projects.groupStats.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectName',projectName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListGroupStatsResponse) as TListGroupStatsResponse;
end;


Function TProjectsGroupStatsResource.List(projectName: string; AQuery : TProjectsGroupStatslistOptions) : TListGroupStatsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'alignment',AQuery.alignment);
  AddToQuery(_Q,'timeRange.period',AQuery.timeRangeperiod);
  AddToQuery(_Q,'order',AQuery.order);
  AddToQuery(_Q,'groupId',AQuery.groupId);
  AddToQuery(_Q,'serviceFilter.service',AQuery.serviceFilterservice);
  AddToQuery(_Q,'alignmentTime',AQuery.alignmentTime);
  AddToQuery(_Q,'serviceFilter.version',AQuery.serviceFilterversion);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'timedCountDuration',AQuery.timedCountDuration);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(projectName,_Q);
end;



{ --------------------------------------------------------------------
  TProjectsResource
  --------------------------------------------------------------------}


Class Function TProjectsResource.ResourceName : String;

begin
  Result:='projects';
end;

Class Function TProjectsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TclouderrorreportingAPI;
end;

Function TProjectsResource.DeleteEvents(projectName: string) : TDeleteEventsResponse;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1beta1/{+projectName}/events';
  _Methodid   = 'clouderrorreporting.projects.deleteEvents';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectName',projectName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDeleteEventsResponse) as TDeleteEventsResponse;
end;



Function TProjectsResource.GetEventsInstance : TProjectsEventsResource;

begin
  if (FEventsInstance=Nil) then
    FEventsInstance:=CreateEventsResource;
  Result:=FEventsInstance;
end;

Function TProjectsResource.CreateEventsResource : TProjectsEventsResource;

begin
  Result:=CreateEventsResource(Self);
end;


Function TProjectsResource.CreateEventsResource(AOwner : TComponent) : TProjectsEventsResource;

begin
  Result:=TProjectsEventsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetGroupsInstance : TProjectsGroupsResource;

begin
  if (FGroupsInstance=Nil) then
    FGroupsInstance:=CreateGroupsResource;
  Result:=FGroupsInstance;
end;

Function TProjectsResource.CreateGroupsResource : TProjectsGroupsResource;

begin
  Result:=CreateGroupsResource(Self);
end;


Function TProjectsResource.CreateGroupsResource(AOwner : TComponent) : TProjectsGroupsResource;

begin
  Result:=TProjectsGroupsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetGroupStatsInstance : TProjectsGroupStatsResource;

begin
  if (FGroupStatsInstance=Nil) then
    FGroupStatsInstance:=CreateGroupStatsResource;
  Result:=FGroupStatsInstance;
end;

Function TProjectsResource.CreateGroupStatsResource : TProjectsGroupStatsResource;

begin
  Result:=CreateGroupStatsResource(Self);
end;


Function TProjectsResource.CreateGroupStatsResource(AOwner : TComponent) : TProjectsGroupStatsResource;

begin
  Result:=TProjectsGroupStatsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TClouderrorreportingAPI
  --------------------------------------------------------------------}

Class Function TClouderrorreportingAPI.APIName : String;

begin
  Result:='clouderrorreporting';
end;

Class Function TClouderrorreportingAPI.APIVersion : String;

begin
  Result:='v1beta1';
end;

Class Function TClouderrorreportingAPI.APIRevision : String;

begin
  Result:='20160517';
end;

Class Function TClouderrorreportingAPI.APIID : String;

begin
  Result:='clouderrorreporting:v1beta1';
end;

Class Function TClouderrorreportingAPI.APITitle : String;

begin
  Result:='Stackdriver Error Reporting API';
end;

Class Function TClouderrorreportingAPI.APIDescription : String;

begin
  Result:='Stackdriver Error Reporting groups and counts similar errors from cloud services. The Stackdriver Error Reporting API provides read access to error groups and their associated errors.';
end;

Class Function TClouderrorreportingAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TClouderrorreportingAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TClouderrorreportingAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TClouderrorreportingAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TClouderrorreportingAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/error-reporting/';
end;

Class Function TClouderrorreportingAPI.APIrootUrl : string;

begin
  Result:='https://clouderrorreporting.googleapis.com/';
end;

Class Function TClouderrorreportingAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TClouderrorreportingAPI.APIbaseURL : String;

begin
  Result:='https://clouderrorreporting.googleapis.com/';
end;

Class Function TClouderrorreportingAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TClouderrorreportingAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TClouderrorreportingAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TClouderrorreportingAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  
end;

Class Function TClouderrorreportingAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TClouderrorreportingAPI.RegisterAPIResources;

begin
  TSourceLocation.RegisterObject;
  TErrorGroupStats.RegisterObject;
  TErrorContext.RegisterObject;
  TServiceContext.RegisterObject;
  TErrorGroup.RegisterObject;
  TTrackingIssue.RegisterObject;
  TDeleteEventsResponse.RegisterObject;
  TErrorEvent.RegisterObject;
  TListEventsResponse.RegisterObject;
  TTimedCount.RegisterObject;
  THttpRequestContext.RegisterObject;
  TListGroupStatsResponse.RegisterObject;
end;


Function TClouderrorreportingAPI.GetProjectsEventsInstance : TProjectsEventsResource;

begin
  if (FProjectsEventsInstance=Nil) then
    FProjectsEventsInstance:=CreateProjectsEventsResource;
  Result:=FProjectsEventsInstance;
end;

Function TClouderrorreportingAPI.CreateProjectsEventsResource : TProjectsEventsResource;

begin
  Result:=CreateProjectsEventsResource(Self);
end;


Function TClouderrorreportingAPI.CreateProjectsEventsResource(AOwner : TComponent) : TProjectsEventsResource;

begin
  Result:=TProjectsEventsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClouderrorreportingAPI.GetProjectsGroupsInstance : TProjectsGroupsResource;

begin
  if (FProjectsGroupsInstance=Nil) then
    FProjectsGroupsInstance:=CreateProjectsGroupsResource;
  Result:=FProjectsGroupsInstance;
end;

Function TClouderrorreportingAPI.CreateProjectsGroupsResource : TProjectsGroupsResource;

begin
  Result:=CreateProjectsGroupsResource(Self);
end;


Function TClouderrorreportingAPI.CreateProjectsGroupsResource(AOwner : TComponent) : TProjectsGroupsResource;

begin
  Result:=TProjectsGroupsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClouderrorreportingAPI.GetProjectsGroupStatsInstance : TProjectsGroupStatsResource;

begin
  if (FProjectsGroupStatsInstance=Nil) then
    FProjectsGroupStatsInstance:=CreateProjectsGroupStatsResource;
  Result:=FProjectsGroupStatsInstance;
end;

Function TClouderrorreportingAPI.CreateProjectsGroupStatsResource : TProjectsGroupStatsResource;

begin
  Result:=CreateProjectsGroupStatsResource(Self);
end;


Function TClouderrorreportingAPI.CreateProjectsGroupStatsResource(AOwner : TComponent) : TProjectsGroupStatsResource;

begin
  Result:=TProjectsGroupStatsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TClouderrorreportingAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TClouderrorreportingAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TClouderrorreportingAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TClouderrorreportingAPI.RegisterAPI;
end.
