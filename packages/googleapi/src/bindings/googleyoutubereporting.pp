unit googleyoutubereporting;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TMedia = Class;
  TListReportTypesResponse = Class;
  TReportType = Class;
  TJob = Class;
  TListJobsResponse = Class;
  TEmpty = Class;
  TListReportsResponse = Class;
  TReport = Class;
  TMediaArray = Array of TMedia;
  TListReportTypesResponseArray = Array of TListReportTypesResponse;
  TReportTypeArray = Array of TReportType;
  TJobArray = Array of TJob;
  TListJobsResponseArray = Array of TListJobsResponse;
  TEmptyArray = Array of TEmpty;
  TListReportsResponseArray = Array of TListReportsResponse;
  TReportArray = Array of TReport;
  //Anonymous types, using auto-generated names
  TListReportTypesResponseTypereportTypesArray = Array of TReportType;
  TListJobsResponseTypejobsArray = Array of TJob;
  TListReportsResponseTypereportsArray = Array of TReport;
  
  { --------------------------------------------------------------------
    TMedia
    --------------------------------------------------------------------}
  
  TMedia = Class(TGoogleBaseObject)
  Private
    FresourceName : String;
  Protected
    //Property setters
    Procedure SetresourceName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property resourceName : String Index 0 Read FresourceName Write SetresourceName;
  end;
  TMediaClass = Class of TMedia;
  
  { --------------------------------------------------------------------
    TListReportTypesResponse
    --------------------------------------------------------------------}
  
  TListReportTypesResponse = Class(TGoogleBaseObject)
  Private
    FreportTypes : TListReportTypesResponseTypereportTypesArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetreportTypes(AIndex : Integer; const AValue : TListReportTypesResponseTypereportTypesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property reportTypes : TListReportTypesResponseTypereportTypesArray Index 0 Read FreportTypes Write SetreportTypes;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListReportTypesResponseClass = Class of TListReportTypesResponse;
  
  { --------------------------------------------------------------------
    TReportType
    --------------------------------------------------------------------}
  
  TReportType = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fname : String;
    FdeprecateTime : String;
    FsystemManaged : boolean;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdeprecateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsystemManaged(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property name : String Index 8 Read Fname Write Setname;
    Property deprecateTime : String Index 16 Read FdeprecateTime Write SetdeprecateTime;
    Property systemManaged : boolean Index 24 Read FsystemManaged Write SetsystemManaged;
  end;
  TReportTypeClass = Class of TReportType;
  
  { --------------------------------------------------------------------
    TJob
    --------------------------------------------------------------------}
  
  TJob = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FreportTypeId : String;
    Fname : String;
    FcreateTime : String;
    FexpireTime : String;
    FsystemManaged : boolean;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreportTypeId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetexpireTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsystemManaged(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property reportTypeId : String Index 8 Read FreportTypeId Write SetreportTypeId;
    Property name : String Index 16 Read Fname Write Setname;
    Property createTime : String Index 24 Read FcreateTime Write SetcreateTime;
    Property expireTime : String Index 32 Read FexpireTime Write SetexpireTime;
    Property systemManaged : boolean Index 40 Read FsystemManaged Write SetsystemManaged;
  end;
  TJobClass = Class of TJob;
  
  { --------------------------------------------------------------------
    TListJobsResponse
    --------------------------------------------------------------------}
  
  TListJobsResponse = Class(TGoogleBaseObject)
  Private
    Fjobs : TListJobsResponseTypejobsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setjobs(AIndex : Integer; const AValue : TListJobsResponseTypejobsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property jobs : TListJobsResponseTypejobsArray Index 0 Read Fjobs Write Setjobs;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListJobsResponseClass = Class of TListJobsResponse;
  
  { --------------------------------------------------------------------
    TEmpty
    --------------------------------------------------------------------}
  
  TEmpty = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEmptyClass = Class of TEmpty;
  
  { --------------------------------------------------------------------
    TListReportsResponse
    --------------------------------------------------------------------}
  
  TListReportsResponse = Class(TGoogleBaseObject)
  Private
    Freports : TListReportsResponseTypereportsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setreports(AIndex : Integer; const AValue : TListReportsResponseTypereportsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property reports : TListReportsResponseTypereportsArray Index 0 Read Freports Write Setreports;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListReportsResponseClass = Class of TListReportsResponse;
  
  { --------------------------------------------------------------------
    TReport
    --------------------------------------------------------------------}
  
  TReport = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FjobId : String;
    FjobExpireTime : String;
    FstartTime : String;
    FendTime : String;
    FcreateTime : String;
    FdownloadUrl : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetjobId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetjobExpireTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdownloadUrl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property jobId : String Index 8 Read FjobId Write SetjobId;
    Property jobExpireTime : String Index 16 Read FjobExpireTime Write SetjobExpireTime;
    Property startTime : String Index 24 Read FstartTime Write SetstartTime;
    Property endTime : String Index 32 Read FendTime Write SetendTime;
    Property createTime : String Index 40 Read FcreateTime Write SetcreateTime;
    Property downloadUrl : String Index 48 Read FdownloadUrl Write SetdownloadUrl;
  end;
  TReportClass = Class of TReport;
  
  { --------------------------------------------------------------------
    TMediaResource
    --------------------------------------------------------------------}
  
  TMediaResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Download(_resourceName: string) : TMedia;
  end;
  
  
  { --------------------------------------------------------------------
    TReportTypesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TReportTypesResource, method List
  
  TReportTypesListOptions = Record
    onBehalfOfContentOwner : String;
    pageSize : integer;
    pageToken : String;
    includeSystemManaged : boolean;
  end;
  
  TReportTypesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TListReportTypesResponse;
    Function List(AQuery : TReportTypeslistOptions) : TListReportTypesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TJobsReportsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TJobsReportsResource, method List
  
  TJobsReportsListOptions = Record
    onBehalfOfContentOwner : String;
    pageSize : integer;
    pageToken : String;
    createdAfter : String;
    startTimeAtOrAfter : String;
    startTimeBefore : String;
  end;
  
  
  //Optional query Options for TJobsReportsResource, method Get
  
  TJobsReportsGetOptions = Record
    onBehalfOfContentOwner : String;
  end;
  
  TJobsReportsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(jobId: string; AQuery : string  = '') : TListReportsResponse;
    Function List(jobId: string; AQuery : TJobsReportslistOptions) : TListReportsResponse;
    Function Get(jobId: string; reportId: string; AQuery : string  = '') : TReport;
    Function Get(jobId: string; reportId: string; AQuery : TJobsReportsgetOptions) : TReport;
  end;
  
  
  { --------------------------------------------------------------------
    TJobsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TJobsResource, method Create
  
  TJobsCreateOptions = Record
    onBehalfOfContentOwner : String;
  end;
  
  
  //Optional query Options for TJobsResource, method List
  
  TJobsListOptions = Record
    onBehalfOfContentOwner : String;
    pageSize : integer;
    pageToken : String;
    includeSystemManaged : boolean;
  end;
  
  
  //Optional query Options for TJobsResource, method Get
  
  TJobsGetOptions = Record
    onBehalfOfContentOwner : String;
  end;
  
  
  //Optional query Options for TJobsResource, method Delete
  
  TJobsDeleteOptions = Record
    onBehalfOfContentOwner : String;
  end;
  
  TJobsResource = Class(TGoogleResource)
  Private
    FReportsInstance : TJobsReportsResource;
    Function GetReportsInstance : TJobsReportsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(aJob : TJob; AQuery : string  = '') : TJob;overload;
    Function Create(aJob : TJob; AQuery : TJobscreateOptions) : TJob;overload;
    Function List(AQuery : string  = '') : TListJobsResponse;
    Function List(AQuery : TJobslistOptions) : TListJobsResponse;
    Function Get(jobId: string; AQuery : string  = '') : TJob;
    Function Get(jobId: string; AQuery : TJobsgetOptions) : TJob;
    Function Delete(jobId: string; AQuery : string  = '') : TEmpty;
    Function Delete(jobId: string; AQuery : TJobsdeleteOptions) : TEmpty;
    Function CreateReportsResource(AOwner : TComponent) : TJobsReportsResource;virtual;overload;
    Function CreateReportsResource : TJobsReportsResource;virtual;overload;
    Property ReportsResource : TJobsReportsResource Read GetReportsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TYoutubereportingAPI
    --------------------------------------------------------------------}
  
  TYoutubereportingAPI = Class(TGoogleAPI)
  Private
    FMediaInstance : TMediaResource;
    FReportTypesInstance : TReportTypesResource;
    FJobsReportsInstance : TJobsReportsResource;
    FJobsInstance : TJobsResource;
    Function GetMediaInstance : TMediaResource;virtual;
    Function GetReportTypesInstance : TReportTypesResource;virtual;
    Function GetJobsReportsInstance : TJobsReportsResource;virtual;
    Function GetJobsInstance : TJobsResource;virtual;
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
    Function CreateMediaResource(AOwner : TComponent) : TMediaResource;virtual;overload;
    Function CreateMediaResource : TMediaResource;virtual;overload;
    Function CreateReportTypesResource(AOwner : TComponent) : TReportTypesResource;virtual;overload;
    Function CreateReportTypesResource : TReportTypesResource;virtual;overload;
    Function CreateJobsReportsResource(AOwner : TComponent) : TJobsReportsResource;virtual;overload;
    Function CreateJobsReportsResource : TJobsReportsResource;virtual;overload;
    Function CreateJobsResource(AOwner : TComponent) : TJobsResource;virtual;overload;
    Function CreateJobsResource : TJobsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property MediaResource : TMediaResource Read GetMediaInstance;
    Property ReportTypesResource : TReportTypesResource Read GetReportTypesInstance;
    Property JobsReportsResource : TJobsReportsResource Read GetJobsReportsInstance;
    Property JobsResource : TJobsResource Read GetJobsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TMedia
  --------------------------------------------------------------------}


Procedure TMedia.SetresourceName(AIndex : Integer; const AValue : String); 

begin
  If (FresourceName=AValue) then exit;
  FresourceName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListReportTypesResponse
  --------------------------------------------------------------------}


Procedure TListReportTypesResponse.SetreportTypes(AIndex : Integer; const AValue : TListReportTypesResponseTypereportTypesArray); 

begin
  If (FreportTypes=AValue) then exit;
  FreportTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListReportTypesResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListReportTypesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'reporttypes' : SetLength(FreportTypes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReportType
  --------------------------------------------------------------------}


Procedure TReportType.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportType.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportType.SetdeprecateTime(AIndex : Integer; const AValue : String); 

begin
  If (FdeprecateTime=AValue) then exit;
  FdeprecateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportType.SetsystemManaged(AIndex : Integer; const AValue : boolean); 

begin
  If (FsystemManaged=AValue) then exit;
  FsystemManaged:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJob
  --------------------------------------------------------------------}


Procedure TJob.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetreportTypeId(AIndex : Integer; const AValue : String); 

begin
  If (FreportTypeId=AValue) then exit;
  FreportTypeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetcreateTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreateTime=AValue) then exit;
  FcreateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetexpireTime(AIndex : Integer; const AValue : String); 

begin
  If (FexpireTime=AValue) then exit;
  FexpireTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetsystemManaged(AIndex : Integer; const AValue : boolean); 

begin
  If (FsystemManaged=AValue) then exit;
  FsystemManaged:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListJobsResponse
  --------------------------------------------------------------------}


Procedure TListJobsResponse.Setjobs(AIndex : Integer; const AValue : TListJobsResponseTypejobsArray); 

begin
  If (Fjobs=AValue) then exit;
  Fjobs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListJobsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListJobsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'jobs' : SetLength(Fjobs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListReportsResponse
  --------------------------------------------------------------------}


Procedure TListReportsResponse.Setreports(AIndex : Integer; const AValue : TListReportsResponseTypereportsArray); 

begin
  If (Freports=AValue) then exit;
  Freports:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListReportsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListReportsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'reports' : SetLength(Freports,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReport
  --------------------------------------------------------------------}


Procedure TReport.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetjobId(AIndex : Integer; const AValue : String); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetjobExpireTime(AIndex : Integer; const AValue : String); 

begin
  If (FjobExpireTime=AValue) then exit;
  FjobExpireTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetstartTime(AIndex : Integer; const AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetendTime(AIndex : Integer; const AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetcreateTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreateTime=AValue) then exit;
  FcreateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SetdownloadUrl(AIndex : Integer; const AValue : String); 

begin
  If (FdownloadUrl=AValue) then exit;
  FdownloadUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMediaResource
  --------------------------------------------------------------------}


Class Function TMediaResource.ResourceName : String;

begin
  Result:='media';
end;

Class Function TMediaResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubereportingAPI;
end;

Function TMediaResource.Download(_resourceName: string) : TMedia;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/media/{+resourceName}';
  _Methodid   = 'youtubereporting.media.download';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resourceName',_resourceName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TMedia) as TMedia;
end;



{ --------------------------------------------------------------------
  TReportTypesResource
  --------------------------------------------------------------------}


Class Function TReportTypesResource.ResourceName : String;

begin
  Result:='reportTypes';
end;

Class Function TReportTypesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubereportingAPI;
end;

Function TReportTypesResource.List(AQuery : string = '') : TListReportTypesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/reportTypes';
  _Methodid   = 'youtubereporting.reportTypes.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TListReportTypesResponse) as TListReportTypesResponse;
end;


Function TReportTypesResource.List(AQuery : TReportTypeslistOptions) : TListReportTypesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'includeSystemManaged',AQuery.includeSystemManaged);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TJobsReportsResource
  --------------------------------------------------------------------}


Class Function TJobsReportsResource.ResourceName : String;

begin
  Result:='reports';
end;

Class Function TJobsReportsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubereportingAPI;
end;

Function TJobsReportsResource.List(jobId: string; AQuery : string = '') : TListReportsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/jobs/{jobId}/reports';
  _Methodid   = 'youtubereporting.jobs.reports.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListReportsResponse) as TListReportsResponse;
end;


Function TJobsReportsResource.List(jobId: string; AQuery : TJobsReportslistOptions) : TListReportsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'createdAfter',AQuery.createdAfter);
  AddToQuery(_Q,'startTimeAtOrAfter',AQuery.startTimeAtOrAfter);
  AddToQuery(_Q,'startTimeBefore',AQuery.startTimeBefore);
  Result:=List(jobId,_Q);
end;

Function TJobsReportsResource.Get(jobId: string; reportId: string; AQuery : string = '') : TReport;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/jobs/{jobId}/reports/{reportId}';
  _Methodid   = 'youtubereporting.jobs.reports.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'reportId',reportId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TReport) as TReport;
end;


Function TJobsReportsResource.Get(jobId: string; reportId: string; AQuery : TJobsReportsgetOptions) : TReport;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Result:=Get(jobId,reportId,_Q);
end;



{ --------------------------------------------------------------------
  TJobsResource
  --------------------------------------------------------------------}


Class Function TJobsResource.ResourceName : String;

begin
  Result:='jobs';
end;

Class Function TJobsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TyoutubereportingAPI;
end;

Function TJobsResource.Create(aJob : TJob; AQuery : string = '') : TJob;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/jobs';
  _Methodid   = 'youtubereporting.jobs.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aJob,TJob) as TJob;
end;


Function TJobsResource.Create(aJob : TJob; AQuery : TJobscreateOptions) : TJob;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Result:=Create(aJob,_Q);
end;

Function TJobsResource.List(AQuery : string = '') : TListJobsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/jobs';
  _Methodid   = 'youtubereporting.jobs.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TListJobsResponse) as TListJobsResponse;
end;


Function TJobsResource.List(AQuery : TJobslistOptions) : TListJobsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'includeSystemManaged',AQuery.includeSystemManaged);
  Result:=List(_Q);
end;

Function TJobsResource.Get(jobId: string; AQuery : string = '') : TJob;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/jobs/{jobId}';
  _Methodid   = 'youtubereporting.jobs.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TJob) as TJob;
end;


Function TJobsResource.Get(jobId: string; AQuery : TJobsgetOptions) : TJob;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Result:=Get(jobId,_Q);
end;

Function TJobsResource.Delete(jobId: string; AQuery : string = '') : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/jobs/{jobId}';
  _Methodid   = 'youtubereporting.jobs.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEmpty) as TEmpty;
end;


Function TJobsResource.Delete(jobId: string; AQuery : TJobsdeleteOptions) : TEmpty;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'onBehalfOfContentOwner',AQuery.onBehalfOfContentOwner);
  Result:=Delete(jobId,_Q);
end;



Function TJobsResource.GetReportsInstance : TJobsReportsResource;

begin
  if (FReportsInstance=Nil) then
    FReportsInstance:=CreateReportsResource;
  Result:=FReportsInstance;
end;

Function TJobsResource.CreateReportsResource : TJobsReportsResource;

begin
  Result:=CreateReportsResource(Self);
end;


Function TJobsResource.CreateReportsResource(AOwner : TComponent) : TJobsReportsResource;

begin
  Result:=TJobsReportsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TYoutubereportingAPI
  --------------------------------------------------------------------}

Class Function TYoutubereportingAPI.APIName : String;

begin
  Result:='youtubereporting';
end;

Class Function TYoutubereportingAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TYoutubereportingAPI.APIRevision : String;

begin
  Result:='20160517';
end;

Class Function TYoutubereportingAPI.APIID : String;

begin
  Result:='youtubereporting:v1';
end;

Class Function TYoutubereportingAPI.APITitle : String;

begin
  Result:='YouTube Reporting API';
end;

Class Function TYoutubereportingAPI.APIDescription : String;

begin
  Result:='Schedules reporting jobs containing your YouTube Analytics data and downloads the resulting bulk data reports in the form of CSV files.';
end;

Class Function TYoutubereportingAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TYoutubereportingAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TYoutubereportingAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TYoutubereportingAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TYoutubereportingAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/youtube/reporting/v1/reports/';
end;

Class Function TYoutubereportingAPI.APIrootUrl : string;

begin
  Result:='https://youtubereporting.googleapis.com/';
end;

Class Function TYoutubereportingAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TYoutubereportingAPI.APIbaseURL : String;

begin
  Result:='https://youtubereporting.googleapis.com/';
end;

Class Function TYoutubereportingAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TYoutubereportingAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TYoutubereportingAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TYoutubereportingAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/yt-analytics-monetary.readonly';
  Result[0].Description:='View monetary and non-monetary YouTube Analytics reports for your YouTube content';
  Result[1].Name:='https://www.googleapis.com/auth/yt-analytics.readonly';
  Result[1].Description:='View YouTube Analytics reports for your YouTube content';
  
end;

Class Function TYoutubereportingAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TYoutubereportingAPI.RegisterAPIResources;

begin
  TMedia.RegisterObject;
  TListReportTypesResponse.RegisterObject;
  TReportType.RegisterObject;
  TJob.RegisterObject;
  TListJobsResponse.RegisterObject;
  TEmpty.RegisterObject;
  TListReportsResponse.RegisterObject;
  TReport.RegisterObject;
end;


Function TYoutubereportingAPI.GetMediaInstance : TMediaResource;

begin
  if (FMediaInstance=Nil) then
    FMediaInstance:=CreateMediaResource;
  Result:=FMediaInstance;
end;

Function TYoutubereportingAPI.CreateMediaResource : TMediaResource;

begin
  Result:=CreateMediaResource(Self);
end;


Function TYoutubereportingAPI.CreateMediaResource(AOwner : TComponent) : TMediaResource;

begin
  Result:=TMediaResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TYoutubereportingAPI.GetReportTypesInstance : TReportTypesResource;

begin
  if (FReportTypesInstance=Nil) then
    FReportTypesInstance:=CreateReportTypesResource;
  Result:=FReportTypesInstance;
end;

Function TYoutubereportingAPI.CreateReportTypesResource : TReportTypesResource;

begin
  Result:=CreateReportTypesResource(Self);
end;


Function TYoutubereportingAPI.CreateReportTypesResource(AOwner : TComponent) : TReportTypesResource;

begin
  Result:=TReportTypesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TYoutubereportingAPI.GetJobsReportsInstance : TJobsReportsResource;

begin
  if (FJobsReportsInstance=Nil) then
    FJobsReportsInstance:=CreateJobsReportsResource;
  Result:=FJobsReportsInstance;
end;

Function TYoutubereportingAPI.CreateJobsReportsResource : TJobsReportsResource;

begin
  Result:=CreateJobsReportsResource(Self);
end;


Function TYoutubereportingAPI.CreateJobsReportsResource(AOwner : TComponent) : TJobsReportsResource;

begin
  Result:=TJobsReportsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TYoutubereportingAPI.GetJobsInstance : TJobsResource;

begin
  if (FJobsInstance=Nil) then
    FJobsInstance:=CreateJobsResource;
  Result:=FJobsInstance;
end;

Function TYoutubereportingAPI.CreateJobsResource : TJobsResource;

begin
  Result:=CreateJobsResource(Self);
end;


Function TYoutubereportingAPI.CreateJobsResource(AOwner : TComponent) : TJobsResource;

begin
  Result:=TJobsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TYoutubereportingAPI.RegisterAPI;
end.
