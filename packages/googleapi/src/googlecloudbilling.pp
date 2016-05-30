unit googlecloudbilling;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TBillingAccount = Class;
  TListBillingAccountsResponse = Class;
  TListProjectBillingInfoResponse = Class;
  TProjectBillingInfo = Class;
  TBillingAccountArray = Array of TBillingAccount;
  TListBillingAccountsResponseArray = Array of TListBillingAccountsResponse;
  TListProjectBillingInfoResponseArray = Array of TListProjectBillingInfoResponse;
  TProjectBillingInfoArray = Array of TProjectBillingInfo;
  //Anonymous types, using auto-generated names
  TListBillingAccountsResponseTypebillingAccountsArray = Array of TBillingAccount;
  TListProjectBillingInfoResponseTypeprojectBillingInfoArray = Array of TProjectBillingInfo;
  
  { --------------------------------------------------------------------
    TBillingAccount
    --------------------------------------------------------------------}
  
  TBillingAccount = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fopen : boolean;
    FdisplayName : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setopen(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property open : boolean Index 8 Read Fopen Write Setopen;
    Property displayName : String Index 16 Read FdisplayName Write SetdisplayName;
  end;
  TBillingAccountClass = Class of TBillingAccount;
  
  { --------------------------------------------------------------------
    TListBillingAccountsResponse
    --------------------------------------------------------------------}
  
  TListBillingAccountsResponse = Class(TGoogleBaseObject)
  Private
    FbillingAccounts : TListBillingAccountsResponseTypebillingAccountsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetbillingAccounts(AIndex : Integer; const AValue : TListBillingAccountsResponseTypebillingAccountsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property billingAccounts : TListBillingAccountsResponseTypebillingAccountsArray Index 0 Read FbillingAccounts Write SetbillingAccounts;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListBillingAccountsResponseClass = Class of TListBillingAccountsResponse;
  
  { --------------------------------------------------------------------
    TListProjectBillingInfoResponse
    --------------------------------------------------------------------}
  
  TListProjectBillingInfoResponse = Class(TGoogleBaseObject)
  Private
    FprojectBillingInfo : TListProjectBillingInfoResponseTypeprojectBillingInfoArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetprojectBillingInfo(AIndex : Integer; const AValue : TListProjectBillingInfoResponseTypeprojectBillingInfoArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property projectBillingInfo : TListProjectBillingInfoResponseTypeprojectBillingInfoArray Index 0 Read FprojectBillingInfo Write SetprojectBillingInfo;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListProjectBillingInfoResponseClass = Class of TListProjectBillingInfoResponse;
  
  { --------------------------------------------------------------------
    TProjectBillingInfo
    --------------------------------------------------------------------}
  
  TProjectBillingInfo = Class(TGoogleBaseObject)
  Private
    Fname : String;
    FprojectId : String;
    FbillingAccountName : String;
    FbillingEnabled : boolean;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbillingAccountName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbillingEnabled(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property projectId : String Index 8 Read FprojectId Write SetprojectId;
    Property billingAccountName : String Index 16 Read FbillingAccountName Write SetbillingAccountName;
    Property billingEnabled : boolean Index 24 Read FbillingEnabled Write SetbillingEnabled;
  end;
  TProjectBillingInfoClass = Class of TProjectBillingInfo;
  
  { --------------------------------------------------------------------
    TBillingAccountsProjectsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TBillingAccountsProjectsResource, method List
  
  TBillingAccountsProjectsListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TBillingAccountsProjectsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(_name: string; AQuery : string  = '') : TListProjectBillingInfoResponse;
    Function List(_name: string; AQuery : TBillingAccountsProjectslistOptions) : TListProjectBillingInfoResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TBillingAccountsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TBillingAccountsResource, method List
  
  TBillingAccountsListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TBillingAccountsResource = Class(TGoogleResource)
  Private
    FProjectsInstance : TBillingAccountsProjectsResource;
    Function GetProjectsInstance : TBillingAccountsProjectsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(_name: string) : TBillingAccount;
    Function List(AQuery : string  = '') : TListBillingAccountsResponse;
    Function List(AQuery : TBillingAccountslistOptions) : TListBillingAccountsResponse;
    Function CreateProjectsResource(AOwner : TComponent) : TBillingAccountsProjectsResource;virtual;overload;
    Function CreateProjectsResource : TBillingAccountsProjectsResource;virtual;overload;
    Property ProjectsResource : TBillingAccountsProjectsResource Read GetProjectsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function GetBillingInfo(_name: string) : TProjectBillingInfo;
    Function UpdateBillingInfo(_name: string; aProjectBillingInfo : TProjectBillingInfo) : TProjectBillingInfo;
  end;
  
  
  { --------------------------------------------------------------------
    TCloudbillingAPI
    --------------------------------------------------------------------}
  
  TCloudbillingAPI = Class(TGoogleAPI)
  Private
    FBillingAccountsProjectsInstance : TBillingAccountsProjectsResource;
    FBillingAccountsInstance : TBillingAccountsResource;
    FProjectsInstance : TProjectsResource;
    Function GetBillingAccountsProjectsInstance : TBillingAccountsProjectsResource;virtual;
    Function GetBillingAccountsInstance : TBillingAccountsResource;virtual;
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
    Function CreateBillingAccountsProjectsResource(AOwner : TComponent) : TBillingAccountsProjectsResource;virtual;overload;
    Function CreateBillingAccountsProjectsResource : TBillingAccountsProjectsResource;virtual;overload;
    Function CreateBillingAccountsResource(AOwner : TComponent) : TBillingAccountsResource;virtual;overload;
    Function CreateBillingAccountsResource : TBillingAccountsResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property BillingAccountsProjectsResource : TBillingAccountsProjectsResource Read GetBillingAccountsProjectsInstance;
    Property BillingAccountsResource : TBillingAccountsResource Read GetBillingAccountsInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TBillingAccount
  --------------------------------------------------------------------}


Procedure TBillingAccount.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBillingAccount.Setopen(AIndex : Integer; const AValue : boolean); 

begin
  If (Fopen=AValue) then exit;
  Fopen:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBillingAccount.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListBillingAccountsResponse
  --------------------------------------------------------------------}


Procedure TListBillingAccountsResponse.SetbillingAccounts(AIndex : Integer; const AValue : TListBillingAccountsResponseTypebillingAccountsArray); 

begin
  If (FbillingAccounts=AValue) then exit;
  FbillingAccounts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListBillingAccountsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListBillingAccountsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'billingaccounts' : SetLength(FbillingAccounts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListProjectBillingInfoResponse
  --------------------------------------------------------------------}


Procedure TListProjectBillingInfoResponse.SetprojectBillingInfo(AIndex : Integer; const AValue : TListProjectBillingInfoResponseTypeprojectBillingInfoArray); 

begin
  If (FprojectBillingInfo=AValue) then exit;
  FprojectBillingInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListProjectBillingInfoResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListProjectBillingInfoResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'projectbillinginfo' : SetLength(FprojectBillingInfo,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProjectBillingInfo
  --------------------------------------------------------------------}


Procedure TProjectBillingInfo.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProjectBillingInfo.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProjectBillingInfo.SetbillingAccountName(AIndex : Integer; const AValue : String); 

begin
  If (FbillingAccountName=AValue) then exit;
  FbillingAccountName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProjectBillingInfo.SetbillingEnabled(AIndex : Integer; const AValue : boolean); 

begin
  If (FbillingEnabled=AValue) then exit;
  FbillingEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBillingAccountsProjectsResource
  --------------------------------------------------------------------}


Class Function TBillingAccountsProjectsResource.ResourceName : String;

begin
  Result:='projects';
end;

Class Function TBillingAccountsProjectsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcloudbillingAPI;
end;

Function TBillingAccountsProjectsResource.List(_name: string; AQuery : string = '') : TListProjectBillingInfoResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}/projects';
  _Methodid   = 'cloudbilling.billingAccounts.projects.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListProjectBillingInfoResponse) as TListProjectBillingInfoResponse;
end;


Function TBillingAccountsProjectsResource.List(_name: string; AQuery : TBillingAccountsProjectslistOptions) : TListProjectBillingInfoResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_name,_Q);
end;



{ --------------------------------------------------------------------
  TBillingAccountsResource
  --------------------------------------------------------------------}


Class Function TBillingAccountsResource.ResourceName : String;

begin
  Result:='billingAccounts';
end;

Class Function TBillingAccountsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcloudbillingAPI;
end;

Function TBillingAccountsResource.Get(_name: string) : TBillingAccount;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}';
  _Methodid   = 'cloudbilling.billingAccounts.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TBillingAccount) as TBillingAccount;
end;

Function TBillingAccountsResource.List(AQuery : string = '') : TListBillingAccountsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/billingAccounts';
  _Methodid   = 'cloudbilling.billingAccounts.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TListBillingAccountsResponse) as TListBillingAccountsResponse;
end;


Function TBillingAccountsResource.List(AQuery : TBillingAccountslistOptions) : TListBillingAccountsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;



Function TBillingAccountsResource.GetProjectsInstance : TBillingAccountsProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TBillingAccountsResource.CreateProjectsResource : TBillingAccountsProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TBillingAccountsResource.CreateProjectsResource(AOwner : TComponent) : TBillingAccountsProjectsResource;

begin
  Result:=TBillingAccountsProjectsResource.Create(AOwner);
  Result.API:=Self.API;
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
  Result:=TcloudbillingAPI;
end;

Function TProjectsResource.GetBillingInfo(_name: string) : TProjectBillingInfo;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}/billingInfo';
  _Methodid   = 'cloudbilling.projects.getBillingInfo';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProjectBillingInfo) as TProjectBillingInfo;
end;

Function TProjectsResource.UpdateBillingInfo(_name: string; aProjectBillingInfo : TProjectBillingInfo) : TProjectBillingInfo;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v1/{+name}/billingInfo';
  _Methodid   = 'cloudbilling.projects.updateBillingInfo';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aProjectBillingInfo,TProjectBillingInfo) as TProjectBillingInfo;
end;



{ --------------------------------------------------------------------
  TCloudbillingAPI
  --------------------------------------------------------------------}

Class Function TCloudbillingAPI.APIName : String;

begin
  Result:='cloudbilling';
end;

Class Function TCloudbillingAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TCloudbillingAPI.APIRevision : String;

begin
  Result:='20151222';
end;

Class Function TCloudbillingAPI.APIID : String;

begin
  Result:='cloudbilling:v1';
end;

Class Function TCloudbillingAPI.APITitle : String;

begin
  Result:='Google Cloud Billing API';
end;

Class Function TCloudbillingAPI.APIDescription : String;

begin
  Result:='Retrieves Google Developers Console billing accounts and associates them with projects.';
end;

Class Function TCloudbillingAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TCloudbillingAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TCloudbillingAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TCloudbillingAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TCloudbillingAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/billing/';
end;

Class Function TCloudbillingAPI.APIrootUrl : string;

begin
  Result:='https://cloudbilling.googleapis.com/';
end;

Class Function TCloudbillingAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TCloudbillingAPI.APIbaseURL : String;

begin
  Result:='https://cloudbilling.googleapis.com/';
end;

Class Function TCloudbillingAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TCloudbillingAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TCloudbillingAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TCloudbillingAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  
end;

Class Function TCloudbillingAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TCloudbillingAPI.RegisterAPIResources;

begin
  TBillingAccount.RegisterObject;
  TListBillingAccountsResponse.RegisterObject;
  TListProjectBillingInfoResponse.RegisterObject;
  TProjectBillingInfo.RegisterObject;
end;


Function TCloudbillingAPI.GetBillingAccountsProjectsInstance : TBillingAccountsProjectsResource;

begin
  if (FBillingAccountsProjectsInstance=Nil) then
    FBillingAccountsProjectsInstance:=CreateBillingAccountsProjectsResource;
  Result:=FBillingAccountsProjectsInstance;
end;

Function TCloudbillingAPI.CreateBillingAccountsProjectsResource : TBillingAccountsProjectsResource;

begin
  Result:=CreateBillingAccountsProjectsResource(Self);
end;


Function TCloudbillingAPI.CreateBillingAccountsProjectsResource(AOwner : TComponent) : TBillingAccountsProjectsResource;

begin
  Result:=TBillingAccountsProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TCloudbillingAPI.GetBillingAccountsInstance : TBillingAccountsResource;

begin
  if (FBillingAccountsInstance=Nil) then
    FBillingAccountsInstance:=CreateBillingAccountsResource;
  Result:=FBillingAccountsInstance;
end;

Function TCloudbillingAPI.CreateBillingAccountsResource : TBillingAccountsResource;

begin
  Result:=CreateBillingAccountsResource(Self);
end;


Function TCloudbillingAPI.CreateBillingAccountsResource(AOwner : TComponent) : TBillingAccountsResource;

begin
  Result:=TBillingAccountsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TCloudbillingAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TCloudbillingAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TCloudbillingAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TCloudbillingAPI.RegisterAPI;
end.
