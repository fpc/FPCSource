unit googlecloudresourcemanager;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TProject = Class;
  TResourceId = Class;
  TListProjectsResponse = Class;
  TEmpty = Class;
  TUndeleteProjectRequest = Class;
  TGetIamPolicyRequest = Class;
  TPolicy = Class;
  TBinding = Class;
  TSetIamPolicyRequest = Class;
  TTestIamPermissionsRequest = Class;
  TTestIamPermissionsResponse = Class;
  TProjectArray = Array of TProject;
  TResourceIdArray = Array of TResourceId;
  TListProjectsResponseArray = Array of TListProjectsResponse;
  TEmptyArray = Array of TEmpty;
  TUndeleteProjectRequestArray = Array of TUndeleteProjectRequest;
  TGetIamPolicyRequestArray = Array of TGetIamPolicyRequest;
  TPolicyArray = Array of TPolicy;
  TBindingArray = Array of TBinding;
  TSetIamPolicyRequestArray = Array of TSetIamPolicyRequest;
  TTestIamPermissionsRequestArray = Array of TTestIamPermissionsRequest;
  TTestIamPermissionsResponseArray = Array of TTestIamPermissionsResponse;
  //Anonymous types, using auto-generated names
  TProjectTypelabels = Class;
  TListProjectsResponseTypeprojectsArray = Array of TProject;
  TPolicyTypebindingsArray = Array of TBinding;
  
  { --------------------------------------------------------------------
    TProjectTypelabels
    --------------------------------------------------------------------}
  
  TProjectTypelabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TProjectTypelabelsClass = Class of TProjectTypelabels;
  
  { --------------------------------------------------------------------
    TProject
    --------------------------------------------------------------------}
  
  TProject = Class(TGoogleBaseObject)
  Private
    FprojectNumber : String;
    FprojectId : String;
    FlifecycleState : String;
    Fname : String;
    FcreateTime : String;
    Flabels : TProjectTypelabels;
    Fparent : TResourceId;
  Protected
    //Property setters
    Procedure SetprojectNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlifecycleState(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; const AValue : TProjectTypelabels); virtual;
    Procedure Setparent(AIndex : Integer; const AValue : TResourceId); virtual;
  Public
  Published
    Property projectNumber : String Index 0 Read FprojectNumber Write SetprojectNumber;
    Property projectId : String Index 8 Read FprojectId Write SetprojectId;
    Property lifecycleState : String Index 16 Read FlifecycleState Write SetlifecycleState;
    Property name : String Index 24 Read Fname Write Setname;
    Property createTime : String Index 32 Read FcreateTime Write SetcreateTime;
    Property labels : TProjectTypelabels Index 40 Read Flabels Write Setlabels;
    Property parent : TResourceId Index 48 Read Fparent Write Setparent;
  end;
  TProjectClass = Class of TProject;
  
  { --------------------------------------------------------------------
    TResourceId
    --------------------------------------------------------------------}
  
  TResourceId = Class(TGoogleBaseObject)
  Private
    F_type : String;
    Fid : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property id : String Index 8 Read Fid Write Setid;
  end;
  TResourceIdClass = Class of TResourceId;
  
  { --------------------------------------------------------------------
    TListProjectsResponse
    --------------------------------------------------------------------}
  
  TListProjectsResponse = Class(TGoogleBaseObject)
  Private
    Fprojects : TListProjectsResponseTypeprojectsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setprojects(AIndex : Integer; const AValue : TListProjectsResponseTypeprojectsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property projects : TListProjectsResponseTypeprojectsArray Index 0 Read Fprojects Write Setprojects;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListProjectsResponseClass = Class of TListProjectsResponse;
  
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
    TUndeleteProjectRequest
    --------------------------------------------------------------------}
  
  TUndeleteProjectRequest = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUndeleteProjectRequestClass = Class of TUndeleteProjectRequest;
  
  { --------------------------------------------------------------------
    TGetIamPolicyRequest
    --------------------------------------------------------------------}
  
  TGetIamPolicyRequest = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGetIamPolicyRequestClass = Class of TGetIamPolicyRequest;
  
  { --------------------------------------------------------------------
    TPolicy
    --------------------------------------------------------------------}
  
  TPolicy = Class(TGoogleBaseObject)
  Private
    Fversion : integer;
    Fbindings : TPolicyTypebindingsArray;
    Fetag : String;
  Protected
    //Property setters
    Procedure Setversion(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setbindings(AIndex : Integer; const AValue : TPolicyTypebindingsArray); virtual;
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property version : integer Index 0 Read Fversion Write Setversion;
    Property bindings : TPolicyTypebindingsArray Index 8 Read Fbindings Write Setbindings;
    Property etag : String Index 16 Read Fetag Write Setetag;
  end;
  TPolicyClass = Class of TPolicy;
  
  { --------------------------------------------------------------------
    TBinding
    --------------------------------------------------------------------}
  
  TBinding = Class(TGoogleBaseObject)
  Private
    Frole : String;
    Fmembers : TStringArray;
  Protected
    //Property setters
    Procedure Setrole(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmembers(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property role : String Index 0 Read Frole Write Setrole;
    Property members : TStringArray Index 8 Read Fmembers Write Setmembers;
  end;
  TBindingClass = Class of TBinding;
  
  { --------------------------------------------------------------------
    TSetIamPolicyRequest
    --------------------------------------------------------------------}
  
  TSetIamPolicyRequest = Class(TGoogleBaseObject)
  Private
    Fpolicy : TPolicy;
  Protected
    //Property setters
    Procedure Setpolicy(AIndex : Integer; const AValue : TPolicy); virtual;
  Public
  Published
    Property policy : TPolicy Index 0 Read Fpolicy Write Setpolicy;
  end;
  TSetIamPolicyRequestClass = Class of TSetIamPolicyRequest;
  
  { --------------------------------------------------------------------
    TTestIamPermissionsRequest
    --------------------------------------------------------------------}
  
  TTestIamPermissionsRequest = Class(TGoogleBaseObject)
  Private
    Fpermissions : TStringArray;
  Protected
    //Property setters
    Procedure Setpermissions(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property permissions : TStringArray Index 0 Read Fpermissions Write Setpermissions;
  end;
  TTestIamPermissionsRequestClass = Class of TTestIamPermissionsRequest;
  
  { --------------------------------------------------------------------
    TTestIamPermissionsResponse
    --------------------------------------------------------------------}
  
  TTestIamPermissionsResponse = Class(TGoogleBaseObject)
  Private
    Fpermissions : TStringArray;
  Protected
    //Property setters
    Procedure Setpermissions(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property permissions : TStringArray Index 0 Read Fpermissions Write Setpermissions;
  end;
  TTestIamPermissionsResponseClass = Class of TTestIamPermissionsResponse;
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsResource, method List
  
  TProjectsListOptions = Record
    pageToken : String;
    pageSize : integer;
    filter : String;
  end;
  
  TProjectsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(projectId: string) : TProject;
    Function List(AQuery : string  = '') : TListProjectsResponse;
    Function List(AQuery : TProjectslistOptions) : TListProjectsResponse;
    Function Update(projectId: string; aProject : TProject) : TProject;
    Function Delete(projectId: string) : TEmpty;
    Function Undelete(projectId: string; aUndeleteProjectRequest : TUndeleteProjectRequest) : TEmpty;
    Function GetIamPolicy(resource: string; aGetIamPolicyRequest : TGetIamPolicyRequest) : TPolicy;
    Function SetIamPolicy(resource: string; aSetIamPolicyRequest : TSetIamPolicyRequest) : TPolicy;
    Function TestIamPermissions(resource: string; aTestIamPermissionsRequest : TTestIamPermissionsRequest) : TTestIamPermissionsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TCloudresourcemanagerAPI
    --------------------------------------------------------------------}
  
  TCloudresourcemanagerAPI = Class(TGoogleAPI)
  Private
    FProjectsInstance : TProjectsResource;
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
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TProjectTypelabels
  --------------------------------------------------------------------}


Class Function TProjectTypelabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TProject
  --------------------------------------------------------------------}


Procedure TProject.SetprojectNumber(AIndex : Integer; const AValue : String); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SetlifecycleState(AIndex : Integer; const AValue : String); 

begin
  If (FlifecycleState=AValue) then exit;
  FlifecycleState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.SetcreateTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreateTime=AValue) then exit;
  FcreateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setlabels(AIndex : Integer; const AValue : TProjectTypelabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setparent(AIndex : Integer; const AValue : TResourceId); 

begin
  If (Fparent=AValue) then exit;
  Fparent:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResourceId
  --------------------------------------------------------------------}


Procedure TResourceId.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceId.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TResourceId.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TListProjectsResponse
  --------------------------------------------------------------------}


Procedure TListProjectsResponse.Setprojects(AIndex : Integer; const AValue : TListProjectsResponseTypeprojectsArray); 

begin
  If (Fprojects=AValue) then exit;
  Fprojects:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListProjectsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListProjectsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'projects' : SetLength(Fprojects,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUndeleteProjectRequest
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGetIamPolicyRequest
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPolicy
  --------------------------------------------------------------------}


Procedure TPolicy.Setversion(AIndex : Integer; const AValue : integer); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolicy.Setbindings(AIndex : Integer; const AValue : TPolicyTypebindingsArray); 

begin
  If (Fbindings=AValue) then exit;
  Fbindings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolicy.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPolicy.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'bindings' : SetLength(Fbindings,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBinding
  --------------------------------------------------------------------}


Procedure TBinding.Setrole(AIndex : Integer; const AValue : String); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBinding.Setmembers(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fmembers=AValue) then exit;
  Fmembers:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBinding.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'members' : SetLength(Fmembers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSetIamPolicyRequest
  --------------------------------------------------------------------}


Procedure TSetIamPolicyRequest.Setpolicy(AIndex : Integer; const AValue : TPolicy); 

begin
  If (Fpolicy=AValue) then exit;
  Fpolicy:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTestIamPermissionsRequest
  --------------------------------------------------------------------}


Procedure TTestIamPermissionsRequest.Setpermissions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTestIamPermissionsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'permissions' : SetLength(Fpermissions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTestIamPermissionsResponse
  --------------------------------------------------------------------}


Procedure TTestIamPermissionsResponse.Setpermissions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTestIamPermissionsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'permissions' : SetLength(Fpermissions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProjectsResource
  --------------------------------------------------------------------}


Class Function TProjectsResource.ResourceName : String;

begin
  Result:='projects';
end;

Class Function TProjectsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcloudresourcemanagerAPI;
end;

Function TProjectsResource.Get(projectId: string) : TProject;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects/{projectId}';
  _Methodid   = 'cloudresourcemanager.projects.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProject) as TProject;
end;

Function TProjectsResource.List(AQuery : string = '') : TListProjectsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects';
  _Methodid   = 'cloudresourcemanager.projects.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TListProjectsResponse) as TListProjectsResponse;
end;


Function TProjectsResource.List(AQuery : TProjectslistOptions) : TListProjectsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'filter',AQuery.filter);
  Result:=List(_Q);
end;

Function TProjectsResource.Update(projectId: string; aProject : TProject) : TProject;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v1/projects/{projectId}';
  _Methodid   = 'cloudresourcemanager.projects.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aProject,TProject) as TProject;
end;

Function TProjectsResource.Delete(projectId: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/projects/{projectId}';
  _Methodid   = 'cloudresourcemanager.projects.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TProjectsResource.Undelete(projectId: string; aUndeleteProjectRequest : TUndeleteProjectRequest) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/projects/{projectId}:undelete';
  _Methodid   = 'cloudresourcemanager.projects.undelete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUndeleteProjectRequest,TEmpty) as TEmpty;
end;

Function TProjectsResource.GetIamPolicy(resource: string; aGetIamPolicyRequest : TGetIamPolicyRequest) : TPolicy;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/projects/{resource}:getIamPolicy';
  _Methodid   = 'cloudresourcemanager.projects.getIamPolicy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aGetIamPolicyRequest,TPolicy) as TPolicy;
end;

Function TProjectsResource.SetIamPolicy(resource: string; aSetIamPolicyRequest : TSetIamPolicyRequest) : TPolicy;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/projects/{resource}:setIamPolicy';
  _Methodid   = 'cloudresourcemanager.projects.setIamPolicy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSetIamPolicyRequest,TPolicy) as TPolicy;
end;

Function TProjectsResource.TestIamPermissions(resource: string; aTestIamPermissionsRequest : TTestIamPermissionsRequest) : TTestIamPermissionsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/projects/{resource}:testIamPermissions';
  _Methodid   = 'cloudresourcemanager.projects.testIamPermissions';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTestIamPermissionsRequest,TTestIamPermissionsResponse) as TTestIamPermissionsResponse;
end;



{ --------------------------------------------------------------------
  TCloudresourcemanagerAPI
  --------------------------------------------------------------------}

Class Function TCloudresourcemanagerAPI.APIName : String;

begin
  Result:='cloudresourcemanager';
end;

Class Function TCloudresourcemanagerAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TCloudresourcemanagerAPI.APIRevision : String;

begin
  Result:='20160518';
end;

Class Function TCloudresourcemanagerAPI.APIID : String;

begin
  Result:='cloudresourcemanager:v1';
end;

Class Function TCloudresourcemanagerAPI.APITitle : String;

begin
  Result:='Google Cloud Resource Manager API';
end;

Class Function TCloudresourcemanagerAPI.APIDescription : String;

begin
  Result:='The Google Cloud Resource Manager API provides methods for creating, reading, and updating project metadata.';
end;

Class Function TCloudresourcemanagerAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TCloudresourcemanagerAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TCloudresourcemanagerAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TCloudresourcemanagerAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TCloudresourcemanagerAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/resource-manager';
end;

Class Function TCloudresourcemanagerAPI.APIrootUrl : string;

begin
  Result:='https://cloudresourcemanager.googleapis.com/';
end;

Class Function TCloudresourcemanagerAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TCloudresourcemanagerAPI.APIbaseURL : String;

begin
  Result:='https://cloudresourcemanager.googleapis.com/';
end;

Class Function TCloudresourcemanagerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TCloudresourcemanagerAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TCloudresourcemanagerAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TCloudresourcemanagerAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/cloud-platform.read-only';
  Result[1].Description:='View your data across Google Cloud Platform services';
  
end;

Class Function TCloudresourcemanagerAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TCloudresourcemanagerAPI.RegisterAPIResources;

begin
  TProjectTypelabels.RegisterObject;
  TProject.RegisterObject;
  TResourceId.RegisterObject;
  TListProjectsResponse.RegisterObject;
  TEmpty.RegisterObject;
  TUndeleteProjectRequest.RegisterObject;
  TGetIamPolicyRequest.RegisterObject;
  TPolicy.RegisterObject;
  TBinding.RegisterObject;
  TSetIamPolicyRequest.RegisterObject;
  TTestIamPermissionsRequest.RegisterObject;
  TTestIamPermissionsResponse.RegisterObject;
end;


Function TCloudresourcemanagerAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TCloudresourcemanagerAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TCloudresourcemanagerAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TCloudresourcemanagerAPI.RegisterAPI;
end.
