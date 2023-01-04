unit googlecloudtrace;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TListTracesResponse = Class;
  TTrace = Class;
  TTraceSpan = Class;
  TTraces = Class;
  TEmpty = Class;
  TListTracesResponseArray = Array of TListTracesResponse;
  TTraceArray = Array of TTrace;
  TTraceSpanArray = Array of TTraceSpan;
  TTracesArray = Array of TTraces;
  TEmptyArray = Array of TEmpty;
  //Anonymous types, using auto-generated names
  TTraceSpanTypelabels = Class;
  TListTracesResponseTypetracesArray = Array of TTrace;
  TTraceTypespansArray = Array of TTraceSpan;
  TTracesTypetracesArray = Array of TTrace;
  
  { --------------------------------------------------------------------
    TListTracesResponse
    --------------------------------------------------------------------}
  
  TListTracesResponse = Class(TGoogleBaseObject)
  Private
    Ftraces : TListTracesResponseTypetracesArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Settraces(AIndex : Integer; const AValue : TListTracesResponseTypetracesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property traces : TListTracesResponseTypetracesArray Index 0 Read Ftraces Write Settraces;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListTracesResponseClass = Class of TListTracesResponse;
  
  { --------------------------------------------------------------------
    TTrace
    --------------------------------------------------------------------}
  
  TTrace = Class(TGoogleBaseObject)
  Private
    FprojectId : String;
    FtraceId : String;
    Fspans : TTraceTypespansArray;
  Protected
    //Property setters
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettraceId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setspans(AIndex : Integer; const AValue : TTraceTypespansArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property projectId : String Index 0 Read FprojectId Write SetprojectId;
    Property traceId : String Index 8 Read FtraceId Write SettraceId;
    Property spans : TTraceTypespansArray Index 16 Read Fspans Write Setspans;
  end;
  TTraceClass = Class of TTrace;
  
  { --------------------------------------------------------------------
    TTraceSpanTypelabels
    --------------------------------------------------------------------}
  
  TTraceSpanTypelabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TTraceSpanTypelabelsClass = Class of TTraceSpanTypelabels;
  
  { --------------------------------------------------------------------
    TTraceSpan
    --------------------------------------------------------------------}
  
  TTraceSpan = Class(TGoogleBaseObject)
  Private
    FspanId : String;
    Fkind : String;
    Fname : String;
    FstartTime : String;
    FendTime : String;
    FparentSpanId : String;
    Flabels : TTraceSpanTypelabels;
  Protected
    //Property setters
    Procedure SetspanId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetparentSpanId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; const AValue : TTraceSpanTypelabels); virtual;
  Public
  Published
    Property spanId : String Index 0 Read FspanId Write SetspanId;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property name : String Index 16 Read Fname Write Setname;
    Property startTime : String Index 24 Read FstartTime Write SetstartTime;
    Property endTime : String Index 32 Read FendTime Write SetendTime;
    Property parentSpanId : String Index 40 Read FparentSpanId Write SetparentSpanId;
    Property labels : TTraceSpanTypelabels Index 48 Read Flabels Write Setlabels;
  end;
  TTraceSpanClass = Class of TTraceSpan;
  
  { --------------------------------------------------------------------
    TTraces
    --------------------------------------------------------------------}
  
  TTraces = Class(TGoogleBaseObject)
  Private
    Ftraces : TTracesTypetracesArray;
  Protected
    //Property setters
    Procedure Settraces(AIndex : Integer; const AValue : TTracesTypetracesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property traces : TTracesTypetracesArray Index 0 Read Ftraces Write Settraces;
  end;
  TTracesClass = Class of TTraces;
  
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
    TProjectsTracesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsTracesResource, method List
  
  TProjectsTracesListOptions = Record
    view : String;
    pageSize : integer;
    pageToken : String;
    startTime : String;
    endTime : String;
    filter : String;
    orderBy : String;
  end;
  
  TProjectsTracesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(projectId: string; AQuery : string  = '') : TListTracesResponse;
    Function List(projectId: string; AQuery : TProjectsTraceslistOptions) : TListTracesResponse;
    Function Get(projectId: string; traceId: string) : TTrace;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Private
    FTracesInstance : TProjectsTracesResource;
    Function GetTracesInstance : TProjectsTracesResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function PatchTraces(projectId: string; aTraces : TTraces) : TEmpty;
    Function CreateTracesResource(AOwner : TComponent) : TProjectsTracesResource;virtual;overload;
    Function CreateTracesResource : TProjectsTracesResource;virtual;overload;
    Property TracesResource : TProjectsTracesResource Read GetTracesInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TCloudtraceAPI
    --------------------------------------------------------------------}
  
  TCloudtraceAPI = Class(TGoogleAPI)
  Private
    FProjectsTracesInstance : TProjectsTracesResource;
    FProjectsInstance : TProjectsResource;
    Function GetProjectsTracesInstance : TProjectsTracesResource;virtual;
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
    Function CreateProjectsTracesResource(AOwner : TComponent) : TProjectsTracesResource;virtual;overload;
    Function CreateProjectsTracesResource : TProjectsTracesResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsTracesResource : TProjectsTracesResource Read GetProjectsTracesInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TListTracesResponse
  --------------------------------------------------------------------}


Procedure TListTracesResponse.Settraces(AIndex : Integer; const AValue : TListTracesResponseTypetracesArray); 

begin
  If (Ftraces=AValue) then exit;
  Ftraces:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListTracesResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListTracesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'traces' : SetLength(Ftraces,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTrace
  --------------------------------------------------------------------}


Procedure TTrace.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrace.SettraceId(AIndex : Integer; const AValue : String); 

begin
  If (FtraceId=AValue) then exit;
  FtraceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrace.Setspans(AIndex : Integer; const AValue : TTraceTypespansArray); 

begin
  If (Fspans=AValue) then exit;
  Fspans:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTrace.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'spans' : SetLength(Fspans,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTraceSpanTypelabels
  --------------------------------------------------------------------}


Class Function TTraceSpanTypelabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TTraceSpan
  --------------------------------------------------------------------}


Procedure TTraceSpan.SetspanId(AIndex : Integer; const AValue : String); 

begin
  If (FspanId=AValue) then exit;
  FspanId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTraceSpan.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTraceSpan.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTraceSpan.SetstartTime(AIndex : Integer; const AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTraceSpan.SetendTime(AIndex : Integer; const AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTraceSpan.SetparentSpanId(AIndex : Integer; const AValue : String); 

begin
  If (FparentSpanId=AValue) then exit;
  FparentSpanId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTraceSpan.Setlabels(AIndex : Integer; const AValue : TTraceSpanTypelabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTraces
  --------------------------------------------------------------------}


Procedure TTraces.Settraces(AIndex : Integer; const AValue : TTracesTypetracesArray); 

begin
  If (Ftraces=AValue) then exit;
  Ftraces:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTraces.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'traces' : SetLength(Ftraces,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TProjectsTracesResource
  --------------------------------------------------------------------}


Class Function TProjectsTracesResource.ResourceName : String;

begin
  Result:='traces';
end;

Class Function TProjectsTracesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcloudtraceAPI;
end;

Function TProjectsTracesResource.List(projectId: string; AQuery : string = '') : TListTracesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects/{projectId}/traces';
  _Methodid   = 'cloudtrace.projects.traces.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListTracesResponse) as TListTracesResponse;
end;


Function TProjectsTracesResource.List(projectId: string; AQuery : TProjectsTraceslistOptions) : TListTracesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'view',AQuery.view);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startTime',AQuery.startTime);
  AddToQuery(_Q,'endTime',AQuery.endTime);
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  Result:=List(projectId,_Q);
end;

Function TProjectsTracesResource.Get(projectId: string; traceId: string) : TTrace;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects/{projectId}/traces/{traceId}';
  _Methodid   = 'cloudtrace.projects.traces.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'traceId',traceId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTrace) as TTrace;
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
  Result:=TcloudtraceAPI;
end;

Function TProjectsResource.PatchTraces(projectId: string; aTraces : TTraces) : TEmpty;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'v1/projects/{projectId}/traces';
  _Methodid   = 'cloudtrace.projects.patchTraces';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTraces,TEmpty) as TEmpty;
end;



Function TProjectsResource.GetTracesInstance : TProjectsTracesResource;

begin
  if (FTracesInstance=Nil) then
    FTracesInstance:=CreateTracesResource;
  Result:=FTracesInstance;
end;

Function TProjectsResource.CreateTracesResource : TProjectsTracesResource;

begin
  Result:=CreateTracesResource(Self);
end;


Function TProjectsResource.CreateTracesResource(AOwner : TComponent) : TProjectsTracesResource;

begin
  Result:=TProjectsTracesResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TCloudtraceAPI
  --------------------------------------------------------------------}

Class Function TCloudtraceAPI.APIName : String;

begin
  Result:='cloudtrace';
end;

Class Function TCloudtraceAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TCloudtraceAPI.APIRevision : String;

begin
  Result:='20160518';
end;

Class Function TCloudtraceAPI.APIID : String;

begin
  Result:='cloudtrace:v1';
end;

Class Function TCloudtraceAPI.APITitle : String;

begin
  Result:='Google Cloud Trace API';
end;

Class Function TCloudtraceAPI.APIDescription : String;

begin
  Result:='Send and retrieve trace data from Google Cloud Trace. Data is generated and available by default for all App Engine applications. Data from other applications can be written to Cloud Trace for display, reporting, and analysis.';
end;

Class Function TCloudtraceAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TCloudtraceAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TCloudtraceAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TCloudtraceAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TCloudtraceAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/tools/cloud-trace';
end;

Class Function TCloudtraceAPI.APIrootUrl : string;

begin
  Result:='https://cloudtrace.googleapis.com/';
end;

Class Function TCloudtraceAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TCloudtraceAPI.APIbaseURL : String;

begin
  Result:='https://cloudtrace.googleapis.com/';
end;

Class Function TCloudtraceAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TCloudtraceAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TCloudtraceAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TCloudtraceAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,3);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/trace.append';
  Result[1].Description:='Write Trace data for a project or application';
  Result[2].Name:='https://www.googleapis.com/auth/trace.readonly';
  Result[2].Description:='Read Trace data for a project or application';
  
end;

Class Function TCloudtraceAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TCloudtraceAPI.RegisterAPIResources;

begin
  TListTracesResponse.RegisterObject;
  TTrace.RegisterObject;
  TTraceSpanTypelabels.RegisterObject;
  TTraceSpan.RegisterObject;
  TTraces.RegisterObject;
  TEmpty.RegisterObject;
end;


Function TCloudtraceAPI.GetProjectsTracesInstance : TProjectsTracesResource;

begin
  if (FProjectsTracesInstance=Nil) then
    FProjectsTracesInstance:=CreateProjectsTracesResource;
  Result:=FProjectsTracesInstance;
end;

Function TCloudtraceAPI.CreateProjectsTracesResource : TProjectsTracesResource;

begin
  Result:=CreateProjectsTracesResource(Self);
end;


Function TCloudtraceAPI.CreateProjectsTracesResource(AOwner : TComponent) : TProjectsTracesResource;

begin
  Result:=TProjectsTracesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TCloudtraceAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TCloudtraceAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TCloudtraceAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TCloudtraceAPI.RegisterAPI;
end.
