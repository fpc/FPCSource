unit googleruntimeconfig;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TStatus = Class;
  TListConfigsResponse = Class;
  TVariable = Class;
  TOperation = Class;
  TWaiter = Class;
  TRuntimeConfig = Class;
  TListWaitersResponse = Class;
  TEndCondition = Class;
  TCardinality = Class;
  TEmpty = Class;
  TWatchVariableRequest = Class;
  TListVariablesResponse = Class;
  TStatusArray = Array of TStatus;
  TListConfigsResponseArray = Array of TListConfigsResponse;
  TVariableArray = Array of TVariable;
  TOperationArray = Array of TOperation;
  TWaiterArray = Array of TWaiter;
  TRuntimeConfigArray = Array of TRuntimeConfig;
  TListWaitersResponseArray = Array of TListWaitersResponse;
  TEndConditionArray = Array of TEndCondition;
  TCardinalityArray = Array of TCardinality;
  TEmptyArray = Array of TEmpty;
  TWatchVariableRequestArray = Array of TWatchVariableRequest;
  TListVariablesResponseArray = Array of TListVariablesResponse;
  //Anonymous types, using auto-generated names
  TStatusTypedetailsItem = Class;
  TOperationTypemetadata = Class;
  TOperationTyperesponse = Class;
  TStatusTypedetailsArray = Array of TStatusTypedetailsItem;
  TListConfigsResponseTypeconfigsArray = Array of TRuntimeConfig;
  TListWaitersResponseTypewaitersArray = Array of TWaiter;
  TListVariablesResponseTypevariablesArray = Array of TVariable;
  
  { --------------------------------------------------------------------
    TStatusTypedetailsItem
    --------------------------------------------------------------------}
  
  TStatusTypedetailsItem = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TStatusTypedetailsItemClass = Class of TStatusTypedetailsItem;
  
  { --------------------------------------------------------------------
    TStatus
    --------------------------------------------------------------------}
  
  TStatus = Class(TGoogleBaseObject)
  Private
    Fcode : integer;
    Fdetails : TStatusTypedetailsArray;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setdetails(AIndex : Integer; const AValue : TStatusTypedetailsArray); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property code : integer Index 0 Read Fcode Write Setcode;
    Property details : TStatusTypedetailsArray Index 8 Read Fdetails Write Setdetails;
    Property message : String Index 16 Read Fmessage Write Setmessage;
  end;
  TStatusClass = Class of TStatus;
  
  { --------------------------------------------------------------------
    TListConfigsResponse
    --------------------------------------------------------------------}
  
  TListConfigsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Fconfigs : TListConfigsResponseTypeconfigsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setconfigs(AIndex : Integer; const AValue : TListConfigsResponseTypeconfigsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property configs : TListConfigsResponseTypeconfigsArray Index 8 Read Fconfigs Write Setconfigs;
  end;
  TListConfigsResponseClass = Class of TListConfigsResponse;
  
  { --------------------------------------------------------------------
    TVariable
    --------------------------------------------------------------------}
  
  TVariable = Class(TGoogleBaseObject)
  Private
    Fvalue : String;
    FupdateTime : String;
    Fstate : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetupdateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property value : String Index 0 Read Fvalue Write Setvalue;
    Property updateTime : String Index 8 Read FupdateTime Write SetupdateTime;
    Property state : String Index 16 Read Fstate Write Setstate;
    Property name : String Index 24 Read Fname Write Setname;
  end;
  TVariableClass = Class of TVariable;
  
  { --------------------------------------------------------------------
    TOperationTypemetadata
    --------------------------------------------------------------------}
  
  TOperationTypemetadata = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TOperationTypemetadataClass = Class of TOperationTypemetadata;
  
  { --------------------------------------------------------------------
    TOperationTyperesponse
    --------------------------------------------------------------------}
  
  TOperationTyperesponse = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TOperationTyperesponseClass = Class of TOperationTyperesponse;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    Ferror : TStatus;
    Fdone : boolean;
    Fmetadata : TOperationTypemetadata;
    Fresponse : TOperationTyperesponse;
    Fname : String;
  Protected
    //Property setters
    Procedure Seterror(AIndex : Integer; const AValue : TStatus); virtual;
    Procedure Setdone(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setmetadata(AIndex : Integer; const AValue : TOperationTypemetadata); virtual;
    Procedure Setresponse(AIndex : Integer; const AValue : TOperationTyperesponse); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property error : TStatus Index 0 Read Ferror Write Seterror;
    Property done : boolean Index 8 Read Fdone Write Setdone;
    Property metadata : TOperationTypemetadata Index 16 Read Fmetadata Write Setmetadata;
    Property response : TOperationTyperesponse Index 24 Read Fresponse Write Setresponse;
    Property name : String Index 32 Read Fname Write Setname;
  end;
  TOperationClass = Class of TOperation;
  
  { --------------------------------------------------------------------
    TWaiter
    --------------------------------------------------------------------}
  
  TWaiter = Class(TGoogleBaseObject)
  Private
    Ftimeout : String;
    Fsuccess : TEndCondition;
    Ffailure : TEndCondition;
    FcreateTime : String;
    Fname : String;
    Ferror : TStatus;
    Fdone : boolean;
  Protected
    //Property setters
    Procedure Settimeout(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsuccess(AIndex : Integer; const AValue : TEndCondition); virtual;
    Procedure Setfailure(AIndex : Integer; const AValue : TEndCondition); virtual;
    Procedure SetcreateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Seterror(AIndex : Integer; const AValue : TStatus); virtual;
    Procedure Setdone(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property timeout : String Index 0 Read Ftimeout Write Settimeout;
    Property success : TEndCondition Index 8 Read Fsuccess Write Setsuccess;
    Property failure : TEndCondition Index 16 Read Ffailure Write Setfailure;
    Property createTime : String Index 24 Read FcreateTime Write SetcreateTime;
    Property name : String Index 32 Read Fname Write Setname;
    Property error : TStatus Index 40 Read Ferror Write Seterror;
    Property done : boolean Index 48 Read Fdone Write Setdone;
  end;
  TWaiterClass = Class of TWaiter;
  
  { --------------------------------------------------------------------
    TRuntimeConfig
    --------------------------------------------------------------------}
  
  TRuntimeConfig = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TRuntimeConfigClass = Class of TRuntimeConfig;
  
  { --------------------------------------------------------------------
    TListWaitersResponse
    --------------------------------------------------------------------}
  
  TListWaitersResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Fwaiters : TListWaitersResponseTypewaitersArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setwaiters(AIndex : Integer; const AValue : TListWaitersResponseTypewaitersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property waiters : TListWaitersResponseTypewaitersArray Index 8 Read Fwaiters Write Setwaiters;
  end;
  TListWaitersResponseClass = Class of TListWaitersResponse;
  
  { --------------------------------------------------------------------
    TEndCondition
    --------------------------------------------------------------------}
  
  TEndCondition = Class(TGoogleBaseObject)
  Private
    Fcardinality : TCardinality;
  Protected
    //Property setters
    Procedure Setcardinality(AIndex : Integer; const AValue : TCardinality); virtual;
  Public
  Published
    Property cardinality : TCardinality Index 0 Read Fcardinality Write Setcardinality;
  end;
  TEndConditionClass = Class of TEndCondition;
  
  { --------------------------------------------------------------------
    TCardinality
    --------------------------------------------------------------------}
  
  TCardinality = Class(TGoogleBaseObject)
  Private
    Fpath : String;
    Fnumber : integer;
  Protected
    //Property setters
    Procedure Setpath(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnumber(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property path : String Index 0 Read Fpath Write Setpath;
    Property number : integer Index 8 Read Fnumber Write Setnumber;
  end;
  TCardinalityClass = Class of TCardinality;
  
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
    TWatchVariableRequest
    --------------------------------------------------------------------}
  
  TWatchVariableRequest = Class(TGoogleBaseObject)
  Private
    FnewerThan : String;
  Protected
    //Property setters
    Procedure SetnewerThan(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property newerThan : String Index 0 Read FnewerThan Write SetnewerThan;
  end;
  TWatchVariableRequestClass = Class of TWatchVariableRequest;
  
  { --------------------------------------------------------------------
    TListVariablesResponse
    --------------------------------------------------------------------}
  
  TListVariablesResponse = Class(TGoogleBaseObject)
  Private
    Fvariables : TListVariablesResponseTypevariablesArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setvariables(AIndex : Integer; const AValue : TListVariablesResponseTypevariablesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property variables : TListVariablesResponseTypevariablesArray Index 0 Read Fvariables Write Setvariables;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListVariablesResponseClass = Class of TListVariablesResponse;
  
  { --------------------------------------------------------------------
    TProjectsConfigsVariablesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsConfigsVariablesResource, method List
  
  TProjectsConfigsVariablesListOptions = Record
    pageSize : integer;
    filter : String;
    pageToken : String;
  end;
  
  
  //Optional query Options for TProjectsConfigsVariablesResource, method Delete
  
  TProjectsConfigsVariablesDeleteOptions = Record
    recursive : boolean;
  end;
  
  TProjectsConfigsVariablesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Watch(_name: string; aWatchVariableRequest : TWatchVariableRequest) : TVariable;
    Function List(parent: string; AQuery : string  = '') : TListVariablesResponse;
    Function List(parent: string; AQuery : TProjectsConfigsVariableslistOptions) : TListVariablesResponse;
    Function Get(_name: string) : TVariable;
    Function Create(parent: string; aVariable : TVariable) : TVariable;overload;
    Function Update(_name: string; aVariable : TVariable) : TVariable;
    Function Delete(_name: string; AQuery : string  = '') : TEmpty;
    Function Delete(_name: string; AQuery : TProjectsConfigsVariablesdeleteOptions) : TEmpty;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsConfigsWaitersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsConfigsWaitersResource, method List
  
  TProjectsConfigsWaitersListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TProjectsConfigsWaitersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(_name: string) : TWaiter;
    Function Create(parent: string; aWaiter : TWaiter) : TOperation;overload;
    Function List(parent: string; AQuery : string  = '') : TListWaitersResponse;
    Function List(parent: string; AQuery : TProjectsConfigsWaiterslistOptions) : TListWaitersResponse;
    Function Delete(_name: string) : TEmpty;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsConfigsOperationsResource
    --------------------------------------------------------------------}
  
  TProjectsConfigsOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(_name: string) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsConfigsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsConfigsResource, method List
  
  TProjectsConfigsListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TProjectsConfigsResource = Class(TGoogleResource)
  Private
    FVariablesInstance : TProjectsConfigsVariablesResource;
    FWaitersInstance : TProjectsConfigsWaitersResource;
    FOperationsInstance : TProjectsConfigsOperationsResource;
    Function GetVariablesInstance : TProjectsConfigsVariablesResource;virtual;
    Function GetWaitersInstance : TProjectsConfigsWaitersResource;virtual;
    Function GetOperationsInstance : TProjectsConfigsOperationsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Update(_name: string; aRuntimeConfig : TRuntimeConfig) : TRuntimeConfig;
    Function Get(_name: string) : TRuntimeConfig;
    Function Create(parent: string; aRuntimeConfig : TRuntimeConfig) : TRuntimeConfig;overload;
    Function List(parent: string; AQuery : string  = '') : TListConfigsResponse;
    Function List(parent: string; AQuery : TProjectsConfigslistOptions) : TListConfigsResponse;
    Function Delete(_name: string) : TEmpty;
    Function CreateVariablesResource(AOwner : TComponent) : TProjectsConfigsVariablesResource;virtual;overload;
    Function CreateVariablesResource : TProjectsConfigsVariablesResource;virtual;overload;
    Function CreateWaitersResource(AOwner : TComponent) : TProjectsConfigsWaitersResource;virtual;overload;
    Function CreateWaitersResource : TProjectsConfigsWaitersResource;virtual;overload;
    Function CreateOperationsResource(AOwner : TComponent) : TProjectsConfigsOperationsResource;virtual;overload;
    Function CreateOperationsResource : TProjectsConfigsOperationsResource;virtual;overload;
    Property VariablesResource : TProjectsConfigsVariablesResource Read GetVariablesInstance;
    Property WaitersResource : TProjectsConfigsWaitersResource Read GetWaitersInstance;
    Property OperationsResource : TProjectsConfigsOperationsResource Read GetOperationsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Private
    FConfigsVariablesInstance : TProjectsConfigsVariablesResource;
    FConfigsWaitersInstance : TProjectsConfigsWaitersResource;
    FConfigsOperationsInstance : TProjectsConfigsOperationsResource;
    FConfigsInstance : TProjectsConfigsResource;
    Function GetConfigsVariablesInstance : TProjectsConfigsVariablesResource;virtual;
    Function GetConfigsWaitersInstance : TProjectsConfigsWaitersResource;virtual;
    Function GetConfigsOperationsInstance : TProjectsConfigsOperationsResource;virtual;
    Function GetConfigsInstance : TProjectsConfigsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateConfigsVariablesResource(AOwner : TComponent) : TProjectsConfigsVariablesResource;virtual;overload;
    Function CreateConfigsVariablesResource : TProjectsConfigsVariablesResource;virtual;overload;
    Function CreateConfigsWaitersResource(AOwner : TComponent) : TProjectsConfigsWaitersResource;virtual;overload;
    Function CreateConfigsWaitersResource : TProjectsConfigsWaitersResource;virtual;overload;
    Function CreateConfigsOperationsResource(AOwner : TComponent) : TProjectsConfigsOperationsResource;virtual;overload;
    Function CreateConfigsOperationsResource : TProjectsConfigsOperationsResource;virtual;overload;
    Function CreateConfigsResource(AOwner : TComponent) : TProjectsConfigsResource;virtual;overload;
    Function CreateConfigsResource : TProjectsConfigsResource;virtual;overload;
    Property ConfigsVariablesResource : TProjectsConfigsVariablesResource Read GetConfigsVariablesInstance;
    Property ConfigsWaitersResource : TProjectsConfigsWaitersResource Read GetConfigsWaitersInstance;
    Property ConfigsOperationsResource : TProjectsConfigsOperationsResource Read GetConfigsOperationsInstance;
    Property ConfigsResource : TProjectsConfigsResource Read GetConfigsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TRuntimeconfigAPI
    --------------------------------------------------------------------}
  
  TRuntimeconfigAPI = Class(TGoogleAPI)
  Private
    FProjectsConfigsVariablesInstance : TProjectsConfigsVariablesResource;
    FProjectsConfigsWaitersInstance : TProjectsConfigsWaitersResource;
    FProjectsConfigsOperationsInstance : TProjectsConfigsOperationsResource;
    FProjectsConfigsInstance : TProjectsConfigsResource;
    FProjectsInstance : TProjectsResource;
    Function GetProjectsConfigsVariablesInstance : TProjectsConfigsVariablesResource;virtual;
    Function GetProjectsConfigsWaitersInstance : TProjectsConfigsWaitersResource;virtual;
    Function GetProjectsConfigsOperationsInstance : TProjectsConfigsOperationsResource;virtual;
    Function GetProjectsConfigsInstance : TProjectsConfigsResource;virtual;
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
    Function CreateProjectsConfigsVariablesResource(AOwner : TComponent) : TProjectsConfigsVariablesResource;virtual;overload;
    Function CreateProjectsConfigsVariablesResource : TProjectsConfigsVariablesResource;virtual;overload;
    Function CreateProjectsConfigsWaitersResource(AOwner : TComponent) : TProjectsConfigsWaitersResource;virtual;overload;
    Function CreateProjectsConfigsWaitersResource : TProjectsConfigsWaitersResource;virtual;overload;
    Function CreateProjectsConfigsOperationsResource(AOwner : TComponent) : TProjectsConfigsOperationsResource;virtual;overload;
    Function CreateProjectsConfigsOperationsResource : TProjectsConfigsOperationsResource;virtual;overload;
    Function CreateProjectsConfigsResource(AOwner : TComponent) : TProjectsConfigsResource;virtual;overload;
    Function CreateProjectsConfigsResource : TProjectsConfigsResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsConfigsVariablesResource : TProjectsConfigsVariablesResource Read GetProjectsConfigsVariablesInstance;
    Property ProjectsConfigsWaitersResource : TProjectsConfigsWaitersResource Read GetProjectsConfigsWaitersInstance;
    Property ProjectsConfigsOperationsResource : TProjectsConfigsOperationsResource Read GetProjectsConfigsOperationsInstance;
    Property ProjectsConfigsResource : TProjectsConfigsResource Read GetProjectsConfigsInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TStatusTypedetailsItem
  --------------------------------------------------------------------}


Class Function TStatusTypedetailsItem.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TStatus
  --------------------------------------------------------------------}


Procedure TStatus.Setcode(AIndex : Integer; const AValue : integer); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setdetails(AIndex : Integer; const AValue : TStatusTypedetailsArray); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TStatus.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'details' : SetLength(Fdetails,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListConfigsResponse
  --------------------------------------------------------------------}


Procedure TListConfigsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListConfigsResponse.Setconfigs(AIndex : Integer; const AValue : TListConfigsResponseTypeconfigsArray); 

begin
  If (Fconfigs=AValue) then exit;
  Fconfigs:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListConfigsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'configs' : SetLength(Fconfigs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVariable
  --------------------------------------------------------------------}


Procedure TVariable.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.SetupdateTime(AIndex : Integer; const AValue : String); 

begin
  If (FupdateTime=AValue) then exit;
  FupdateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariable.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypemetadata
  --------------------------------------------------------------------}


Class Function TOperationTypemetadata.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TOperationTyperesponse
  --------------------------------------------------------------------}


Class Function TOperationTyperesponse.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.Seterror(AIndex : Integer; const AValue : TStatus); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setdone(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdone=AValue) then exit;
  Fdone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setmetadata(AIndex : Integer; const AValue : TOperationTypemetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setresponse(AIndex : Integer; const AValue : TOperationTyperesponse); 

begin
  If (Fresponse=AValue) then exit;
  Fresponse:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWaiter
  --------------------------------------------------------------------}


Procedure TWaiter.Settimeout(AIndex : Integer; const AValue : String); 

begin
  If (Ftimeout=AValue) then exit;
  Ftimeout:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWaiter.Setsuccess(AIndex : Integer; const AValue : TEndCondition); 

begin
  If (Fsuccess=AValue) then exit;
  Fsuccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWaiter.Setfailure(AIndex : Integer; const AValue : TEndCondition); 

begin
  If (Ffailure=AValue) then exit;
  Ffailure:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWaiter.SetcreateTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreateTime=AValue) then exit;
  FcreateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWaiter.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWaiter.Seterror(AIndex : Integer; const AValue : TStatus); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWaiter.Setdone(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdone=AValue) then exit;
  Fdone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRuntimeConfig
  --------------------------------------------------------------------}


Procedure TRuntimeConfig.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRuntimeConfig.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListWaitersResponse
  --------------------------------------------------------------------}


Procedure TListWaitersResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListWaitersResponse.Setwaiters(AIndex : Integer; const AValue : TListWaitersResponseTypewaitersArray); 

begin
  If (Fwaiters=AValue) then exit;
  Fwaiters:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListWaitersResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'waiters' : SetLength(Fwaiters,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEndCondition
  --------------------------------------------------------------------}


Procedure TEndCondition.Setcardinality(AIndex : Integer; const AValue : TCardinality); 

begin
  If (Fcardinality=AValue) then exit;
  Fcardinality:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCardinality
  --------------------------------------------------------------------}


Procedure TCardinality.Setpath(AIndex : Integer; const AValue : String); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCardinality.Setnumber(AIndex : Integer; const AValue : integer); 

begin
  If (Fnumber=AValue) then exit;
  Fnumber:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWatchVariableRequest
  --------------------------------------------------------------------}


Procedure TWatchVariableRequest.SetnewerThan(AIndex : Integer; const AValue : String); 

begin
  If (FnewerThan=AValue) then exit;
  FnewerThan:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListVariablesResponse
  --------------------------------------------------------------------}


Procedure TListVariablesResponse.Setvariables(AIndex : Integer; const AValue : TListVariablesResponseTypevariablesArray); 

begin
  If (Fvariables=AValue) then exit;
  Fvariables:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListVariablesResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListVariablesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'variables' : SetLength(Fvariables,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProjectsConfigsVariablesResource
  --------------------------------------------------------------------}


Class Function TProjectsConfigsVariablesResource.ResourceName : String;

begin
  Result:='variables';
end;

Class Function TProjectsConfigsVariablesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TruntimeconfigAPI;
end;

Function TProjectsConfigsVariablesResource.Watch(_name: string; aWatchVariableRequest : TWatchVariableRequest) : TVariable;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta1/{+name}:watch';
  _Methodid   = 'runtimeconfig.projects.configs.variables.watch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aWatchVariableRequest,TVariable) as TVariable;
end;

Function TProjectsConfigsVariablesResource.List(parent: string; AQuery : string = '') : TListVariablesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta1/{+parent}/variables';
  _Methodid   = 'runtimeconfig.projects.configs.variables.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['parent',parent]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListVariablesResponse) as TListVariablesResponse;
end;


Function TProjectsConfigsVariablesResource.List(parent: string; AQuery : TProjectsConfigsVariableslistOptions) : TListVariablesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(parent,_Q);
end;

Function TProjectsConfigsVariablesResource.Get(_name: string) : TVariable;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta1/{+name}';
  _Methodid   = 'runtimeconfig.projects.configs.variables.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TVariable) as TVariable;
end;

Function TProjectsConfigsVariablesResource.Create(parent: string; aVariable : TVariable) : TVariable;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta1/{+parent}/variables';
  _Methodid   = 'runtimeconfig.projects.configs.variables.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['parent',parent]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aVariable,TVariable) as TVariable;
end;

Function TProjectsConfigsVariablesResource.Update(_name: string; aVariable : TVariable) : TVariable;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v1beta1/{+name}';
  _Methodid   = 'runtimeconfig.projects.configs.variables.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aVariable,TVariable) as TVariable;
end;

Function TProjectsConfigsVariablesResource.Delete(_name: string; AQuery : string = '') : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1beta1/{+name}';
  _Methodid   = 'runtimeconfig.projects.configs.variables.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEmpty) as TEmpty;
end;


Function TProjectsConfigsVariablesResource.Delete(_name: string; AQuery : TProjectsConfigsVariablesdeleteOptions) : TEmpty;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'recursive',AQuery.recursive);
  Result:=Delete(_name,_Q);
end;



{ --------------------------------------------------------------------
  TProjectsConfigsWaitersResource
  --------------------------------------------------------------------}


Class Function TProjectsConfigsWaitersResource.ResourceName : String;

begin
  Result:='waiters';
end;

Class Function TProjectsConfigsWaitersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TruntimeconfigAPI;
end;

Function TProjectsConfigsWaitersResource.Get(_name: string) : TWaiter;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta1/{+name}';
  _Methodid   = 'runtimeconfig.projects.configs.waiters.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TWaiter) as TWaiter;
end;

Function TProjectsConfigsWaitersResource.Create(parent: string; aWaiter : TWaiter) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta1/{+parent}/waiters';
  _Methodid   = 'runtimeconfig.projects.configs.waiters.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['parent',parent]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aWaiter,TOperation) as TOperation;
end;

Function TProjectsConfigsWaitersResource.List(parent: string; AQuery : string = '') : TListWaitersResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta1/{+parent}/waiters';
  _Methodid   = 'runtimeconfig.projects.configs.waiters.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['parent',parent]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListWaitersResponse) as TListWaitersResponse;
end;


Function TProjectsConfigsWaitersResource.List(parent: string; AQuery : TProjectsConfigsWaiterslistOptions) : TListWaitersResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(parent,_Q);
end;

Function TProjectsConfigsWaitersResource.Delete(_name: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1beta1/{+name}';
  _Methodid   = 'runtimeconfig.projects.configs.waiters.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;



{ --------------------------------------------------------------------
  TProjectsConfigsOperationsResource
  --------------------------------------------------------------------}


Class Function TProjectsConfigsOperationsResource.ResourceName : String;

begin
  Result:='operations';
end;

Class Function TProjectsConfigsOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TruntimeconfigAPI;
end;

Function TProjectsConfigsOperationsResource.Get(_name: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta1/{+name}';
  _Methodid   = 'runtimeconfig.projects.configs.operations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TProjectsConfigsResource
  --------------------------------------------------------------------}


Class Function TProjectsConfigsResource.ResourceName : String;

begin
  Result:='configs';
end;

Class Function TProjectsConfigsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TruntimeconfigAPI;
end;

Function TProjectsConfigsResource.Update(_name: string; aRuntimeConfig : TRuntimeConfig) : TRuntimeConfig;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v1beta1/{+name}';
  _Methodid   = 'runtimeconfig.projects.configs.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRuntimeConfig,TRuntimeConfig) as TRuntimeConfig;
end;

Function TProjectsConfigsResource.Get(_name: string) : TRuntimeConfig;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta1/{+name}';
  _Methodid   = 'runtimeconfig.projects.configs.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TRuntimeConfig) as TRuntimeConfig;
end;

Function TProjectsConfigsResource.Create(parent: string; aRuntimeConfig : TRuntimeConfig) : TRuntimeConfig;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta1/{+parent}/configs';
  _Methodid   = 'runtimeconfig.projects.configs.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['parent',parent]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRuntimeConfig,TRuntimeConfig) as TRuntimeConfig;
end;

Function TProjectsConfigsResource.List(parent: string; AQuery : string = '') : TListConfigsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta1/{+parent}/configs';
  _Methodid   = 'runtimeconfig.projects.configs.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['parent',parent]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListConfigsResponse) as TListConfigsResponse;
end;


Function TProjectsConfigsResource.List(parent: string; AQuery : TProjectsConfigslistOptions) : TListConfigsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(parent,_Q);
end;

Function TProjectsConfigsResource.Delete(_name: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1beta1/{+name}';
  _Methodid   = 'runtimeconfig.projects.configs.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;



Function TProjectsConfigsResource.GetVariablesInstance : TProjectsConfigsVariablesResource;

begin
  if (FVariablesInstance=Nil) then
    FVariablesInstance:=CreateVariablesResource;
  Result:=FVariablesInstance;
end;

Function TProjectsConfigsResource.CreateVariablesResource : TProjectsConfigsVariablesResource;

begin
  Result:=CreateVariablesResource(Self);
end;


Function TProjectsConfigsResource.CreateVariablesResource(AOwner : TComponent) : TProjectsConfigsVariablesResource;

begin
  Result:=TProjectsConfigsVariablesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsConfigsResource.GetWaitersInstance : TProjectsConfigsWaitersResource;

begin
  if (FWaitersInstance=Nil) then
    FWaitersInstance:=CreateWaitersResource;
  Result:=FWaitersInstance;
end;

Function TProjectsConfigsResource.CreateWaitersResource : TProjectsConfigsWaitersResource;

begin
  Result:=CreateWaitersResource(Self);
end;


Function TProjectsConfigsResource.CreateWaitersResource(AOwner : TComponent) : TProjectsConfigsWaitersResource;

begin
  Result:=TProjectsConfigsWaitersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsConfigsResource.GetOperationsInstance : TProjectsConfigsOperationsResource;

begin
  if (FOperationsInstance=Nil) then
    FOperationsInstance:=CreateOperationsResource;
  Result:=FOperationsInstance;
end;

Function TProjectsConfigsResource.CreateOperationsResource : TProjectsConfigsOperationsResource;

begin
  Result:=CreateOperationsResource(Self);
end;


Function TProjectsConfigsResource.CreateOperationsResource(AOwner : TComponent) : TProjectsConfigsOperationsResource;

begin
  Result:=TProjectsConfigsOperationsResource.Create(AOwner);
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
  Result:=TruntimeconfigAPI;
end;



Function TProjectsResource.GetConfigsVariablesInstance : TProjectsConfigsVariablesResource;

begin
  if (FConfigsVariablesInstance=Nil) then
    FConfigsVariablesInstance:=CreateConfigsVariablesResource;
  Result:=FConfigsVariablesInstance;
end;

Function TProjectsResource.CreateConfigsVariablesResource : TProjectsConfigsVariablesResource;

begin
  Result:=CreateConfigsVariablesResource(Self);
end;


Function TProjectsResource.CreateConfigsVariablesResource(AOwner : TComponent) : TProjectsConfigsVariablesResource;

begin
  Result:=TProjectsConfigsVariablesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetConfigsWaitersInstance : TProjectsConfigsWaitersResource;

begin
  if (FConfigsWaitersInstance=Nil) then
    FConfigsWaitersInstance:=CreateConfigsWaitersResource;
  Result:=FConfigsWaitersInstance;
end;

Function TProjectsResource.CreateConfigsWaitersResource : TProjectsConfigsWaitersResource;

begin
  Result:=CreateConfigsWaitersResource(Self);
end;


Function TProjectsResource.CreateConfigsWaitersResource(AOwner : TComponent) : TProjectsConfigsWaitersResource;

begin
  Result:=TProjectsConfigsWaitersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetConfigsOperationsInstance : TProjectsConfigsOperationsResource;

begin
  if (FConfigsOperationsInstance=Nil) then
    FConfigsOperationsInstance:=CreateConfigsOperationsResource;
  Result:=FConfigsOperationsInstance;
end;

Function TProjectsResource.CreateConfigsOperationsResource : TProjectsConfigsOperationsResource;

begin
  Result:=CreateConfigsOperationsResource(Self);
end;


Function TProjectsResource.CreateConfigsOperationsResource(AOwner : TComponent) : TProjectsConfigsOperationsResource;

begin
  Result:=TProjectsConfigsOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetConfigsInstance : TProjectsConfigsResource;

begin
  if (FConfigsInstance=Nil) then
    FConfigsInstance:=CreateConfigsResource;
  Result:=FConfigsInstance;
end;

Function TProjectsResource.CreateConfigsResource : TProjectsConfigsResource;

begin
  Result:=CreateConfigsResource(Self);
end;


Function TProjectsResource.CreateConfigsResource(AOwner : TComponent) : TProjectsConfigsResource;

begin
  Result:=TProjectsConfigsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TRuntimeconfigAPI
  --------------------------------------------------------------------}

Class Function TRuntimeconfigAPI.APIName : String;

begin
  Result:='runtimeconfig';
end;

Class Function TRuntimeconfigAPI.APIVersion : String;

begin
  Result:='v1beta1';
end;

Class Function TRuntimeconfigAPI.APIRevision : String;

begin
  Result:='20160518';
end;

Class Function TRuntimeconfigAPI.APIID : String;

begin
  Result:='runtimeconfig:v1beta1';
end;

Class Function TRuntimeconfigAPI.APITitle : String;

begin
  Result:='Google Cloud RuntimeConfig API';
end;

Class Function TRuntimeconfigAPI.APIDescription : String;

begin
  Result:='Provides capabilities for dynamic configuration and coordination for applications running on Google Cloud Platform.';
end;

Class Function TRuntimeconfigAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TRuntimeconfigAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TRuntimeconfigAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TRuntimeconfigAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TRuntimeconfigAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/deployment-manager/docs/';
end;

Class Function TRuntimeconfigAPI.APIrootUrl : string;

begin
  Result:='https://runtimeconfig.googleapis.com/';
end;

Class Function TRuntimeconfigAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TRuntimeconfigAPI.APIbaseURL : String;

begin
  Result:='https://runtimeconfig.googleapis.com/';
end;

Class Function TRuntimeconfigAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TRuntimeconfigAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TRuntimeconfigAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TRuntimeconfigAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/cloudruntimeconfig';
  Result[1].Description:='Manage your Google Cloud Platform services'' runtime configuration';
  
end;

Class Function TRuntimeconfigAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TRuntimeconfigAPI.RegisterAPIResources;

begin
  TStatusTypedetailsItem.RegisterObject;
  TStatus.RegisterObject;
  TListConfigsResponse.RegisterObject;
  TVariable.RegisterObject;
  TOperationTypemetadata.RegisterObject;
  TOperationTyperesponse.RegisterObject;
  TOperation.RegisterObject;
  TWaiter.RegisterObject;
  TRuntimeConfig.RegisterObject;
  TListWaitersResponse.RegisterObject;
  TEndCondition.RegisterObject;
  TCardinality.RegisterObject;
  TEmpty.RegisterObject;
  TWatchVariableRequest.RegisterObject;
  TListVariablesResponse.RegisterObject;
end;


Function TRuntimeconfigAPI.GetProjectsConfigsVariablesInstance : TProjectsConfigsVariablesResource;

begin
  if (FProjectsConfigsVariablesInstance=Nil) then
    FProjectsConfigsVariablesInstance:=CreateProjectsConfigsVariablesResource;
  Result:=FProjectsConfigsVariablesInstance;
end;

Function TRuntimeconfigAPI.CreateProjectsConfigsVariablesResource : TProjectsConfigsVariablesResource;

begin
  Result:=CreateProjectsConfigsVariablesResource(Self);
end;


Function TRuntimeconfigAPI.CreateProjectsConfigsVariablesResource(AOwner : TComponent) : TProjectsConfigsVariablesResource;

begin
  Result:=TProjectsConfigsVariablesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TRuntimeconfigAPI.GetProjectsConfigsWaitersInstance : TProjectsConfigsWaitersResource;

begin
  if (FProjectsConfigsWaitersInstance=Nil) then
    FProjectsConfigsWaitersInstance:=CreateProjectsConfigsWaitersResource;
  Result:=FProjectsConfigsWaitersInstance;
end;

Function TRuntimeconfigAPI.CreateProjectsConfigsWaitersResource : TProjectsConfigsWaitersResource;

begin
  Result:=CreateProjectsConfigsWaitersResource(Self);
end;


Function TRuntimeconfigAPI.CreateProjectsConfigsWaitersResource(AOwner : TComponent) : TProjectsConfigsWaitersResource;

begin
  Result:=TProjectsConfigsWaitersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TRuntimeconfigAPI.GetProjectsConfigsOperationsInstance : TProjectsConfigsOperationsResource;

begin
  if (FProjectsConfigsOperationsInstance=Nil) then
    FProjectsConfigsOperationsInstance:=CreateProjectsConfigsOperationsResource;
  Result:=FProjectsConfigsOperationsInstance;
end;

Function TRuntimeconfigAPI.CreateProjectsConfigsOperationsResource : TProjectsConfigsOperationsResource;

begin
  Result:=CreateProjectsConfigsOperationsResource(Self);
end;


Function TRuntimeconfigAPI.CreateProjectsConfigsOperationsResource(AOwner : TComponent) : TProjectsConfigsOperationsResource;

begin
  Result:=TProjectsConfigsOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TRuntimeconfigAPI.GetProjectsConfigsInstance : TProjectsConfigsResource;

begin
  if (FProjectsConfigsInstance=Nil) then
    FProjectsConfigsInstance:=CreateProjectsConfigsResource;
  Result:=FProjectsConfigsInstance;
end;

Function TRuntimeconfigAPI.CreateProjectsConfigsResource : TProjectsConfigsResource;

begin
  Result:=CreateProjectsConfigsResource(Self);
end;


Function TRuntimeconfigAPI.CreateProjectsConfigsResource(AOwner : TComponent) : TProjectsConfigsResource;

begin
  Result:=TProjectsConfigsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TRuntimeconfigAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TRuntimeconfigAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TRuntimeconfigAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TRuntimeconfigAPI.RegisterAPI;
end.
