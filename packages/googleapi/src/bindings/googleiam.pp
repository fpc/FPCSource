unit googleiam;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TListServiceAccountsResponse = Class;
  TServiceAccount = Class;
  TCreateServiceAccountRequest = Class;
  TEmpty = Class;
  TListServiceAccountKeysResponse = Class;
  TServiceAccountKey = Class;
  TCreateServiceAccountKeyRequest = Class;
  TSignBlobRequest = Class;
  TSignBlobResponse = Class;
  TPolicy = Class;
  TBinding = Class;
  TRule = Class;
  TCondition = Class;
  TLogConfig = Class;
  TCounterOptions = Class;
  TDataAccessOptions = Class;
  TCloudAuditOptions = Class;
  TSetIamPolicyRequest = Class;
  TTestIamPermissionsRequest = Class;
  TTestIamPermissionsResponse = Class;
  TListServiceAccountsResponseArray = Array of TListServiceAccountsResponse;
  TServiceAccountArray = Array of TServiceAccount;
  TCreateServiceAccountRequestArray = Array of TCreateServiceAccountRequest;
  TEmptyArray = Array of TEmpty;
  TListServiceAccountKeysResponseArray = Array of TListServiceAccountKeysResponse;
  TServiceAccountKeyArray = Array of TServiceAccountKey;
  TCreateServiceAccountKeyRequestArray = Array of TCreateServiceAccountKeyRequest;
  TSignBlobRequestArray = Array of TSignBlobRequest;
  TSignBlobResponseArray = Array of TSignBlobResponse;
  TPolicyArray = Array of TPolicy;
  TBindingArray = Array of TBinding;
  TRuleArray = Array of TRule;
  TConditionArray = Array of TCondition;
  TLogConfigArray = Array of TLogConfig;
  TCounterOptionsArray = Array of TCounterOptions;
  TDataAccessOptionsArray = Array of TDataAccessOptions;
  TCloudAuditOptionsArray = Array of TCloudAuditOptions;
  TSetIamPolicyRequestArray = Array of TSetIamPolicyRequest;
  TTestIamPermissionsRequestArray = Array of TTestIamPermissionsRequest;
  TTestIamPermissionsResponseArray = Array of TTestIamPermissionsResponse;
  //Anonymous types, using auto-generated names
  TListServiceAccountsResponseTypeaccountsArray = Array of TServiceAccount;
  TListServiceAccountKeysResponseTypekeysArray = Array of TServiceAccountKey;
  TPolicyTypebindingsArray = Array of TBinding;
  TPolicyTyperulesArray = Array of TRule;
  TRuleTypeconditionsArray = Array of TCondition;
  TRuleTypelogConfigArray = Array of TLogConfig;
  
  { --------------------------------------------------------------------
    TListServiceAccountsResponse
    --------------------------------------------------------------------}
  
  TListServiceAccountsResponse = Class(TGoogleBaseObject)
  Private
    Faccounts : TListServiceAccountsResponseTypeaccountsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setaccounts(AIndex : Integer; const AValue : TListServiceAccountsResponseTypeaccountsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accounts : TListServiceAccountsResponseTypeaccountsArray Index 0 Read Faccounts Write Setaccounts;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListServiceAccountsResponseClass = Class of TListServiceAccountsResponse;
  
  { --------------------------------------------------------------------
    TServiceAccount
    --------------------------------------------------------------------}
  
  TServiceAccount = Class(TGoogleBaseObject)
  Private
    Fname : String;
    FprojectId : String;
    FuniqueId : String;
    Femail : String;
    FdisplayName : String;
    Fetag : String;
    Foauth2ClientId : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuniqueId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setoauth2ClientId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property projectId : String Index 8 Read FprojectId Write SetprojectId;
    Property uniqueId : String Index 16 Read FuniqueId Write SetuniqueId;
    Property email : String Index 24 Read Femail Write Setemail;
    Property displayName : String Index 32 Read FdisplayName Write SetdisplayName;
    Property etag : String Index 40 Read Fetag Write Setetag;
    Property oauth2ClientId : String Index 48 Read Foauth2ClientId Write Setoauth2ClientId;
  end;
  TServiceAccountClass = Class of TServiceAccount;
  
  { --------------------------------------------------------------------
    TCreateServiceAccountRequest
    --------------------------------------------------------------------}
  
  TCreateServiceAccountRequest = Class(TGoogleBaseObject)
  Private
    FaccountId : String;
    FserviceAccount : TServiceAccount;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetserviceAccount(AIndex : Integer; const AValue : TServiceAccount); virtual;
  Public
  Published
    Property accountId : String Index 0 Read FaccountId Write SetaccountId;
    Property serviceAccount : TServiceAccount Index 8 Read FserviceAccount Write SetserviceAccount;
  end;
  TCreateServiceAccountRequestClass = Class of TCreateServiceAccountRequest;
  
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
    TListServiceAccountKeysResponse
    --------------------------------------------------------------------}
  
  TListServiceAccountKeysResponse = Class(TGoogleBaseObject)
  Private
    Fkeys : TListServiceAccountKeysResponseTypekeysArray;
  Protected
    //Property setters
    Procedure Setkeys(AIndex : Integer; const AValue : TListServiceAccountKeysResponseTypekeysArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property keys : TListServiceAccountKeysResponseTypekeysArray Index 0 Read Fkeys Write Setkeys;
  end;
  TListServiceAccountKeysResponseClass = Class of TListServiceAccountKeysResponse;
  
  { --------------------------------------------------------------------
    TServiceAccountKey
    --------------------------------------------------------------------}
  
  TServiceAccountKey = Class(TGoogleBaseObject)
  Private
    Fname : String;
    FprivateKeyType : String;
    FprivateKeyData : String;
    FvalidAfterTime : String;
    FvalidBeforeTime : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprivateKeyType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprivateKeyData(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvalidAfterTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvalidBeforeTime(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property privateKeyType : String Index 8 Read FprivateKeyType Write SetprivateKeyType;
    Property privateKeyData : String Index 16 Read FprivateKeyData Write SetprivateKeyData;
    Property validAfterTime : String Index 24 Read FvalidAfterTime Write SetvalidAfterTime;
    Property validBeforeTime : String Index 32 Read FvalidBeforeTime Write SetvalidBeforeTime;
  end;
  TServiceAccountKeyClass = Class of TServiceAccountKey;
  
  { --------------------------------------------------------------------
    TCreateServiceAccountKeyRequest
    --------------------------------------------------------------------}
  
  TCreateServiceAccountKeyRequest = Class(TGoogleBaseObject)
  Private
    FprivateKeyType : String;
  Protected
    //Property setters
    Procedure SetprivateKeyType(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property privateKeyType : String Index 0 Read FprivateKeyType Write SetprivateKeyType;
  end;
  TCreateServiceAccountKeyRequestClass = Class of TCreateServiceAccountKeyRequest;
  
  { --------------------------------------------------------------------
    TSignBlobRequest
    --------------------------------------------------------------------}
  
  TSignBlobRequest = Class(TGoogleBaseObject)
  Private
    FbytesToSign : String;
  Protected
    //Property setters
    Procedure SetbytesToSign(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property bytesToSign : String Index 0 Read FbytesToSign Write SetbytesToSign;
  end;
  TSignBlobRequestClass = Class of TSignBlobRequest;
  
  { --------------------------------------------------------------------
    TSignBlobResponse
    --------------------------------------------------------------------}
  
  TSignBlobResponse = Class(TGoogleBaseObject)
  Private
    FkeyId : String;
    Fsignature : String;
  Protected
    //Property setters
    Procedure SetkeyId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsignature(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property keyId : String Index 0 Read FkeyId Write SetkeyId;
    Property signature : String Index 8 Read Fsignature Write Setsignature;
  end;
  TSignBlobResponseClass = Class of TSignBlobResponse;
  
  { --------------------------------------------------------------------
    TPolicy
    --------------------------------------------------------------------}
  
  TPolicy = Class(TGoogleBaseObject)
  Private
    Fversion : integer;
    Fbindings : TPolicyTypebindingsArray;
    Frules : TPolicyTyperulesArray;
    Fetag : String;
  Protected
    //Property setters
    Procedure Setversion(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setbindings(AIndex : Integer; const AValue : TPolicyTypebindingsArray); virtual;
    Procedure Setrules(AIndex : Integer; const AValue : TPolicyTyperulesArray); virtual;
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property version : integer Index 0 Read Fversion Write Setversion;
    Property bindings : TPolicyTypebindingsArray Index 8 Read Fbindings Write Setbindings;
    Property rules : TPolicyTyperulesArray Index 16 Read Frules Write Setrules;
    Property etag : String Index 24 Read Fetag Write Setetag;
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
    TRule
    --------------------------------------------------------------------}
  
  TRule = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    Fpermissions : TStringArray;
    Faction : String;
    F_in : TStringArray;
    FnotIn : TStringArray;
    Fconditions : TRuleTypeconditionsArray;
    FlogConfig : TRuleTypelogConfigArray;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpermissions(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setaction(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_in(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetnotIn(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setconditions(AIndex : Integer; const AValue : TRuleTypeconditionsArray); virtual;
    Procedure SetlogConfig(AIndex : Integer; const AValue : TRuleTypelogConfigArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property permissions : TStringArray Index 8 Read Fpermissions Write Setpermissions;
    Property action : String Index 16 Read Faction Write Setaction;
    Property _in : TStringArray Index 24 Read F_in Write Set_in;
    Property notIn : TStringArray Index 32 Read FnotIn Write SetnotIn;
    Property conditions : TRuleTypeconditionsArray Index 40 Read Fconditions Write Setconditions;
    Property logConfig : TRuleTypelogConfigArray Index 48 Read FlogConfig Write SetlogConfig;
  end;
  TRuleClass = Class of TRule;
  
  { --------------------------------------------------------------------
    TCondition
    --------------------------------------------------------------------}
  
  TCondition = Class(TGoogleBaseObject)
  Private
    Fiam : String;
    Fsys : String;
    Fsvc : String;
    Fop : String;
    Fvalue : String;
    Fvalues : TStringArray;
  Protected
    //Property setters
    Procedure Setiam(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsys(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsvc(AIndex : Integer; const AValue : String); virtual;
    Procedure Setop(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalues(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property iam : String Index 0 Read Fiam Write Setiam;
    Property sys : String Index 8 Read Fsys Write Setsys;
    Property svc : String Index 16 Read Fsvc Write Setsvc;
    Property op : String Index 24 Read Fop Write Setop;
    Property value : String Index 32 Read Fvalue Write Setvalue;
    Property values : TStringArray Index 40 Read Fvalues Write Setvalues;
  end;
  TConditionClass = Class of TCondition;
  
  { --------------------------------------------------------------------
    TLogConfig
    --------------------------------------------------------------------}
  
  TLogConfig = Class(TGoogleBaseObject)
  Private
    Fcounter : TCounterOptions;
    FdataAccess : TDataAccessOptions;
    FcloudAudit : TCloudAuditOptions;
  Protected
    //Property setters
    Procedure Setcounter(AIndex : Integer; const AValue : TCounterOptions); virtual;
    Procedure SetdataAccess(AIndex : Integer; const AValue : TDataAccessOptions); virtual;
    Procedure SetcloudAudit(AIndex : Integer; const AValue : TCloudAuditOptions); virtual;
  Public
  Published
    Property counter : TCounterOptions Index 0 Read Fcounter Write Setcounter;
    Property dataAccess : TDataAccessOptions Index 8 Read FdataAccess Write SetdataAccess;
    Property cloudAudit : TCloudAuditOptions Index 16 Read FcloudAudit Write SetcloudAudit;
  end;
  TLogConfigClass = Class of TLogConfig;
  
  { --------------------------------------------------------------------
    TCounterOptions
    --------------------------------------------------------------------}
  
  TCounterOptions = Class(TGoogleBaseObject)
  Private
    Fmetric : String;
    Ffield : String;
  Protected
    //Property setters
    Procedure Setmetric(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfield(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metric : String Index 0 Read Fmetric Write Setmetric;
    Property field : String Index 8 Read Ffield Write Setfield;
  end;
  TCounterOptionsClass = Class of TCounterOptions;
  
  { --------------------------------------------------------------------
    TDataAccessOptions
    --------------------------------------------------------------------}
  
  TDataAccessOptions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDataAccessOptionsClass = Class of TDataAccessOptions;
  
  { --------------------------------------------------------------------
    TCloudAuditOptions
    --------------------------------------------------------------------}
  
  TCloudAuditOptions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCloudAuditOptionsClass = Class of TCloudAuditOptions;
  
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
    TProjectsServiceAccountsKeysResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsServiceAccountsKeysResource, method List
  
  TProjectsServiceAccountsKeysListOptions = Record
    keyTypes : String;
  end;
  
  TProjectsServiceAccountsKeysResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(_name: string; AQuery : string  = '') : TListServiceAccountKeysResponse;
    Function List(_name: string; AQuery : TProjectsServiceAccountsKeyslistOptions) : TListServiceAccountKeysResponse;
    Function Get(_name: string) : TServiceAccountKey;
    Function Create(_name: string; aCreateServiceAccountKeyRequest : TCreateServiceAccountKeyRequest) : TServiceAccountKey;overload;
    Function Delete(_name: string) : TEmpty;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsServiceAccountsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsServiceAccountsResource, method List
  
  TProjectsServiceAccountsListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TProjectsServiceAccountsResource = Class(TGoogleResource)
  Private
    FKeysInstance : TProjectsServiceAccountsKeysResource;
    Function GetKeysInstance : TProjectsServiceAccountsKeysResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(_name: string; AQuery : string  = '') : TListServiceAccountsResponse;
    Function List(_name: string; AQuery : TProjectsServiceAccountslistOptions) : TListServiceAccountsResponse;
    Function Get(_name: string) : TServiceAccount;
    Function Create(_name: string; aCreateServiceAccountRequest : TCreateServiceAccountRequest) : TServiceAccount;overload;
    Function Update(_name: string; aServiceAccount : TServiceAccount) : TServiceAccount;
    Function Delete(_name: string) : TEmpty;
    Function SignBlob(_name: string; aSignBlobRequest : TSignBlobRequest) : TSignBlobResponse;
    Function GetIamPolicy(resource: string) : TPolicy;
    Function SetIamPolicy(resource: string; aSetIamPolicyRequest : TSetIamPolicyRequest) : TPolicy;
    Function TestIamPermissions(resource: string; aTestIamPermissionsRequest : TTestIamPermissionsRequest) : TTestIamPermissionsResponse;
    Function CreateKeysResource(AOwner : TComponent) : TProjectsServiceAccountsKeysResource;virtual;overload;
    Function CreateKeysResource : TProjectsServiceAccountsKeysResource;virtual;overload;
    Property KeysResource : TProjectsServiceAccountsKeysResource Read GetKeysInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Private
    FServiceAccountsKeysInstance : TProjectsServiceAccountsKeysResource;
    FServiceAccountsInstance : TProjectsServiceAccountsResource;
    Function GetServiceAccountsKeysInstance : TProjectsServiceAccountsKeysResource;virtual;
    Function GetServiceAccountsInstance : TProjectsServiceAccountsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateServiceAccountsKeysResource(AOwner : TComponent) : TProjectsServiceAccountsKeysResource;virtual;overload;
    Function CreateServiceAccountsKeysResource : TProjectsServiceAccountsKeysResource;virtual;overload;
    Function CreateServiceAccountsResource(AOwner : TComponent) : TProjectsServiceAccountsResource;virtual;overload;
    Function CreateServiceAccountsResource : TProjectsServiceAccountsResource;virtual;overload;
    Property ServiceAccountsKeysResource : TProjectsServiceAccountsKeysResource Read GetServiceAccountsKeysInstance;
    Property ServiceAccountsResource : TProjectsServiceAccountsResource Read GetServiceAccountsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TIamAPI
    --------------------------------------------------------------------}
  
  TIamAPI = Class(TGoogleAPI)
  Private
    FProjectsServiceAccountsKeysInstance : TProjectsServiceAccountsKeysResource;
    FProjectsServiceAccountsInstance : TProjectsServiceAccountsResource;
    FProjectsInstance : TProjectsResource;
    Function GetProjectsServiceAccountsKeysInstance : TProjectsServiceAccountsKeysResource;virtual;
    Function GetProjectsServiceAccountsInstance : TProjectsServiceAccountsResource;virtual;
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
    Function CreateProjectsServiceAccountsKeysResource(AOwner : TComponent) : TProjectsServiceAccountsKeysResource;virtual;overload;
    Function CreateProjectsServiceAccountsKeysResource : TProjectsServiceAccountsKeysResource;virtual;overload;
    Function CreateProjectsServiceAccountsResource(AOwner : TComponent) : TProjectsServiceAccountsResource;virtual;overload;
    Function CreateProjectsServiceAccountsResource : TProjectsServiceAccountsResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsServiceAccountsKeysResource : TProjectsServiceAccountsKeysResource Read GetProjectsServiceAccountsKeysInstance;
    Property ProjectsServiceAccountsResource : TProjectsServiceAccountsResource Read GetProjectsServiceAccountsInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TListServiceAccountsResponse
  --------------------------------------------------------------------}


Procedure TListServiceAccountsResponse.Setaccounts(AIndex : Integer; const AValue : TListServiceAccountsResponseTypeaccountsArray); 

begin
  If (Faccounts=AValue) then exit;
  Faccounts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListServiceAccountsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListServiceAccountsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'accounts' : SetLength(Faccounts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TServiceAccount
  --------------------------------------------------------------------}


Procedure TServiceAccount.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServiceAccount.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServiceAccount.SetuniqueId(AIndex : Integer; const AValue : String); 

begin
  If (FuniqueId=AValue) then exit;
  FuniqueId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServiceAccount.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServiceAccount.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServiceAccount.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServiceAccount.Setoauth2ClientId(AIndex : Integer; const AValue : String); 

begin
  If (Foauth2ClientId=AValue) then exit;
  Foauth2ClientId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreateServiceAccountRequest
  --------------------------------------------------------------------}


Procedure TCreateServiceAccountRequest.SetaccountId(AIndex : Integer; const AValue : String); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateServiceAccountRequest.SetserviceAccount(AIndex : Integer; const AValue : TServiceAccount); 

begin
  If (FserviceAccount=AValue) then exit;
  FserviceAccount:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListServiceAccountKeysResponse
  --------------------------------------------------------------------}


Procedure TListServiceAccountKeysResponse.Setkeys(AIndex : Integer; const AValue : TListServiceAccountKeysResponseTypekeysArray); 

begin
  If (Fkeys=AValue) then exit;
  Fkeys:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListServiceAccountKeysResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'keys' : SetLength(Fkeys,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TServiceAccountKey
  --------------------------------------------------------------------}


Procedure TServiceAccountKey.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServiceAccountKey.SetprivateKeyType(AIndex : Integer; const AValue : String); 

begin
  If (FprivateKeyType=AValue) then exit;
  FprivateKeyType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServiceAccountKey.SetprivateKeyData(AIndex : Integer; const AValue : String); 

begin
  If (FprivateKeyData=AValue) then exit;
  FprivateKeyData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServiceAccountKey.SetvalidAfterTime(AIndex : Integer; const AValue : String); 

begin
  If (FvalidAfterTime=AValue) then exit;
  FvalidAfterTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServiceAccountKey.SetvalidBeforeTime(AIndex : Integer; const AValue : String); 

begin
  If (FvalidBeforeTime=AValue) then exit;
  FvalidBeforeTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreateServiceAccountKeyRequest
  --------------------------------------------------------------------}


Procedure TCreateServiceAccountKeyRequest.SetprivateKeyType(AIndex : Integer; const AValue : String); 

begin
  If (FprivateKeyType=AValue) then exit;
  FprivateKeyType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSignBlobRequest
  --------------------------------------------------------------------}


Procedure TSignBlobRequest.SetbytesToSign(AIndex : Integer; const AValue : String); 

begin
  If (FbytesToSign=AValue) then exit;
  FbytesToSign:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSignBlobResponse
  --------------------------------------------------------------------}


Procedure TSignBlobResponse.SetkeyId(AIndex : Integer; const AValue : String); 

begin
  If (FkeyId=AValue) then exit;
  FkeyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSignBlobResponse.Setsignature(AIndex : Integer; const AValue : String); 

begin
  If (Fsignature=AValue) then exit;
  Fsignature:=AValue;
  MarkPropertyChanged(AIndex);
end;





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



Procedure TPolicy.Setrules(AIndex : Integer; const AValue : TPolicyTyperulesArray); 

begin
  If (Frules=AValue) then exit;
  Frules:=AValue;
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
  'rules' : SetLength(Frules,ALength);
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
  TRule
  --------------------------------------------------------------------}


Procedure TRule.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setpermissions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setaction(AIndex : Integer; const AValue : String); 

begin
  If (Faction=AValue) then exit;
  Faction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Set_in(AIndex : Integer; const AValue : TStringArray); 

begin
  If (F_in=AValue) then exit;
  F_in:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.SetnotIn(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FnotIn=AValue) then exit;
  FnotIn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.Setconditions(AIndex : Integer; const AValue : TRuleTypeconditionsArray); 

begin
  If (Fconditions=AValue) then exit;
  Fconditions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRule.SetlogConfig(AIndex : Integer; const AValue : TRuleTypelogConfigArray); 

begin
  If (FlogConfig=AValue) then exit;
  FlogConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TRule.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_in' : Result:='in';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TRule.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'permissions' : SetLength(Fpermissions,ALength);
  '_in' : SetLength(F_in,ALength);
  'notin' : SetLength(FnotIn,ALength);
  'conditions' : SetLength(Fconditions,ALength);
  'logconfig' : SetLength(FlogConfig,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCondition
  --------------------------------------------------------------------}


Procedure TCondition.Setiam(AIndex : Integer; const AValue : String); 

begin
  If (Fiam=AValue) then exit;
  Fiam:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCondition.Setsys(AIndex : Integer; const AValue : String); 

begin
  If (Fsys=AValue) then exit;
  Fsys:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCondition.Setsvc(AIndex : Integer; const AValue : String); 

begin
  If (Fsvc=AValue) then exit;
  Fsvc:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCondition.Setop(AIndex : Integer; const AValue : String); 

begin
  If (Fop=AValue) then exit;
  Fop:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCondition.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCondition.Setvalues(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCondition.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'values' : SetLength(Fvalues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLogConfig
  --------------------------------------------------------------------}


Procedure TLogConfig.Setcounter(AIndex : Integer; const AValue : TCounterOptions); 

begin
  If (Fcounter=AValue) then exit;
  Fcounter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogConfig.SetdataAccess(AIndex : Integer; const AValue : TDataAccessOptions); 

begin
  If (FdataAccess=AValue) then exit;
  FdataAccess:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogConfig.SetcloudAudit(AIndex : Integer; const AValue : TCloudAuditOptions); 

begin
  If (FcloudAudit=AValue) then exit;
  FcloudAudit:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCounterOptions
  --------------------------------------------------------------------}


Procedure TCounterOptions.Setmetric(AIndex : Integer; const AValue : String); 

begin
  If (Fmetric=AValue) then exit;
  Fmetric:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCounterOptions.Setfield(AIndex : Integer; const AValue : String); 

begin
  If (Ffield=AValue) then exit;
  Ffield:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDataAccessOptions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCloudAuditOptions
  --------------------------------------------------------------------}




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
  TProjectsServiceAccountsKeysResource
  --------------------------------------------------------------------}


Class Function TProjectsServiceAccountsKeysResource.ResourceName : String;

begin
  Result:='keys';
end;

Class Function TProjectsServiceAccountsKeysResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TiamAPI;
end;

Function TProjectsServiceAccountsKeysResource.List(_name: string; AQuery : string = '') : TListServiceAccountKeysResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}/keys';
  _Methodid   = 'iam.projects.serviceAccounts.keys.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListServiceAccountKeysResponse) as TListServiceAccountKeysResponse;
end;


Function TProjectsServiceAccountsKeysResource.List(_name: string; AQuery : TProjectsServiceAccountsKeyslistOptions) : TListServiceAccountKeysResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'keyTypes',AQuery.keyTypes);
  Result:=List(_name,_Q);
end;

Function TProjectsServiceAccountsKeysResource.Get(_name: string) : TServiceAccountKey;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}';
  _Methodid   = 'iam.projects.serviceAccounts.keys.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TServiceAccountKey) as TServiceAccountKey;
end;

Function TProjectsServiceAccountsKeysResource.Create(_name: string; aCreateServiceAccountKeyRequest : TCreateServiceAccountKeyRequest) : TServiceAccountKey;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+name}/keys';
  _Methodid   = 'iam.projects.serviceAccounts.keys.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreateServiceAccountKeyRequest,TServiceAccountKey) as TServiceAccountKey;
end;

Function TProjectsServiceAccountsKeysResource.Delete(_name: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/{+name}';
  _Methodid   = 'iam.projects.serviceAccounts.keys.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;



{ --------------------------------------------------------------------
  TProjectsServiceAccountsResource
  --------------------------------------------------------------------}


Class Function TProjectsServiceAccountsResource.ResourceName : String;

begin
  Result:='serviceAccounts';
end;

Class Function TProjectsServiceAccountsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TiamAPI;
end;

Function TProjectsServiceAccountsResource.List(_name: string; AQuery : string = '') : TListServiceAccountsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}/serviceAccounts';
  _Methodid   = 'iam.projects.serviceAccounts.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListServiceAccountsResponse) as TListServiceAccountsResponse;
end;


Function TProjectsServiceAccountsResource.List(_name: string; AQuery : TProjectsServiceAccountslistOptions) : TListServiceAccountsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_name,_Q);
end;

Function TProjectsServiceAccountsResource.Get(_name: string) : TServiceAccount;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}';
  _Methodid   = 'iam.projects.serviceAccounts.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TServiceAccount) as TServiceAccount;
end;

Function TProjectsServiceAccountsResource.Create(_name: string; aCreateServiceAccountRequest : TCreateServiceAccountRequest) : TServiceAccount;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+name}/serviceAccounts';
  _Methodid   = 'iam.projects.serviceAccounts.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreateServiceAccountRequest,TServiceAccount) as TServiceAccount;
end;

Function TProjectsServiceAccountsResource.Update(_name: string; aServiceAccount : TServiceAccount) : TServiceAccount;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v1/{+name}';
  _Methodid   = 'iam.projects.serviceAccounts.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aServiceAccount,TServiceAccount) as TServiceAccount;
end;

Function TProjectsServiceAccountsResource.Delete(_name: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/{+name}';
  _Methodid   = 'iam.projects.serviceAccounts.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TProjectsServiceAccountsResource.SignBlob(_name: string; aSignBlobRequest : TSignBlobRequest) : TSignBlobResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+name}:signBlob';
  _Methodid   = 'iam.projects.serviceAccounts.signBlob';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSignBlobRequest,TSignBlobResponse) as TSignBlobResponse;
end;

Function TProjectsServiceAccountsResource.GetIamPolicy(resource: string) : TPolicy;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+resource}:getIamPolicy';
  _Methodid   = 'iam.projects.serviceAccounts.getIamPolicy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPolicy) as TPolicy;
end;

Function TProjectsServiceAccountsResource.SetIamPolicy(resource: string; aSetIamPolicyRequest : TSetIamPolicyRequest) : TPolicy;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+resource}:setIamPolicy';
  _Methodid   = 'iam.projects.serviceAccounts.setIamPolicy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSetIamPolicyRequest,TPolicy) as TPolicy;
end;

Function TProjectsServiceAccountsResource.TestIamPermissions(resource: string; aTestIamPermissionsRequest : TTestIamPermissionsRequest) : TTestIamPermissionsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+resource}:testIamPermissions';
  _Methodid   = 'iam.projects.serviceAccounts.testIamPermissions';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTestIamPermissionsRequest,TTestIamPermissionsResponse) as TTestIamPermissionsResponse;
end;



Function TProjectsServiceAccountsResource.GetKeysInstance : TProjectsServiceAccountsKeysResource;

begin
  if (FKeysInstance=Nil) then
    FKeysInstance:=CreateKeysResource;
  Result:=FKeysInstance;
end;

Function TProjectsServiceAccountsResource.CreateKeysResource : TProjectsServiceAccountsKeysResource;

begin
  Result:=CreateKeysResource(Self);
end;


Function TProjectsServiceAccountsResource.CreateKeysResource(AOwner : TComponent) : TProjectsServiceAccountsKeysResource;

begin
  Result:=TProjectsServiceAccountsKeysResource.Create(AOwner);
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
  Result:=TiamAPI;
end;



Function TProjectsResource.GetServiceAccountsKeysInstance : TProjectsServiceAccountsKeysResource;

begin
  if (FServiceAccountsKeysInstance=Nil) then
    FServiceAccountsKeysInstance:=CreateServiceAccountsKeysResource;
  Result:=FServiceAccountsKeysInstance;
end;

Function TProjectsResource.CreateServiceAccountsKeysResource : TProjectsServiceAccountsKeysResource;

begin
  Result:=CreateServiceAccountsKeysResource(Self);
end;


Function TProjectsResource.CreateServiceAccountsKeysResource(AOwner : TComponent) : TProjectsServiceAccountsKeysResource;

begin
  Result:=TProjectsServiceAccountsKeysResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetServiceAccountsInstance : TProjectsServiceAccountsResource;

begin
  if (FServiceAccountsInstance=Nil) then
    FServiceAccountsInstance:=CreateServiceAccountsResource;
  Result:=FServiceAccountsInstance;
end;

Function TProjectsResource.CreateServiceAccountsResource : TProjectsServiceAccountsResource;

begin
  Result:=CreateServiceAccountsResource(Self);
end;


Function TProjectsResource.CreateServiceAccountsResource(AOwner : TComponent) : TProjectsServiceAccountsResource;

begin
  Result:=TProjectsServiceAccountsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TIamAPI
  --------------------------------------------------------------------}

Class Function TIamAPI.APIName : String;

begin
  Result:='iam';
end;

Class Function TIamAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TIamAPI.APIRevision : String;

begin
  Result:='20160129';
end;

Class Function TIamAPI.APIID : String;

begin
  Result:='iam:v1';
end;

Class Function TIamAPI.APITitle : String;

begin
  Result:='Google Identity and Access Management API';
end;

Class Function TIamAPI.APIDescription : String;

begin
  Result:='Manages identity and access control for Google Cloud Platform resources, including the creation of service accounts, which you can use to authenticate to Google and make API calls.';
end;

Class Function TIamAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TIamAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TIamAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TIamAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TIamAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/iam/';
end;

Class Function TIamAPI.APIrootUrl : string;

begin
  Result:='https://iam.googleapis.com/';
end;

Class Function TIamAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TIamAPI.APIbaseURL : String;

begin
  Result:='https://iam.googleapis.com/';
end;

Class Function TIamAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TIamAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TIamAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TIamAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  
end;

Class Function TIamAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TIamAPI.RegisterAPIResources;

begin
  TListServiceAccountsResponse.RegisterObject;
  TServiceAccount.RegisterObject;
  TCreateServiceAccountRequest.RegisterObject;
  TEmpty.RegisterObject;
  TListServiceAccountKeysResponse.RegisterObject;
  TServiceAccountKey.RegisterObject;
  TCreateServiceAccountKeyRequest.RegisterObject;
  TSignBlobRequest.RegisterObject;
  TSignBlobResponse.RegisterObject;
  TPolicy.RegisterObject;
  TBinding.RegisterObject;
  TRule.RegisterObject;
  TCondition.RegisterObject;
  TLogConfig.RegisterObject;
  TCounterOptions.RegisterObject;
  TDataAccessOptions.RegisterObject;
  TCloudAuditOptions.RegisterObject;
  TSetIamPolicyRequest.RegisterObject;
  TTestIamPermissionsRequest.RegisterObject;
  TTestIamPermissionsResponse.RegisterObject;
end;


Function TIamAPI.GetProjectsServiceAccountsKeysInstance : TProjectsServiceAccountsKeysResource;

begin
  if (FProjectsServiceAccountsKeysInstance=Nil) then
    FProjectsServiceAccountsKeysInstance:=CreateProjectsServiceAccountsKeysResource;
  Result:=FProjectsServiceAccountsKeysInstance;
end;

Function TIamAPI.CreateProjectsServiceAccountsKeysResource : TProjectsServiceAccountsKeysResource;

begin
  Result:=CreateProjectsServiceAccountsKeysResource(Self);
end;


Function TIamAPI.CreateProjectsServiceAccountsKeysResource(AOwner : TComponent) : TProjectsServiceAccountsKeysResource;

begin
  Result:=TProjectsServiceAccountsKeysResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TIamAPI.GetProjectsServiceAccountsInstance : TProjectsServiceAccountsResource;

begin
  if (FProjectsServiceAccountsInstance=Nil) then
    FProjectsServiceAccountsInstance:=CreateProjectsServiceAccountsResource;
  Result:=FProjectsServiceAccountsInstance;
end;

Function TIamAPI.CreateProjectsServiceAccountsResource : TProjectsServiceAccountsResource;

begin
  Result:=CreateProjectsServiceAccountsResource(Self);
end;


Function TIamAPI.CreateProjectsServiceAccountsResource(AOwner : TComponent) : TProjectsServiceAccountsResource;

begin
  Result:=TProjectsServiceAccountsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TIamAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TIamAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TIamAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TIamAPI.RegisterAPI;
end.
