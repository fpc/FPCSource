unit googleserviceregistry;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TEndpoint = Class;
  TEndpointEndpointVisibility = Class;
  TEndpointsListResponse = Class;
  TOperation = Class;
  TOperationsListResponse = Class;
  TEndpointArray = Array of TEndpoint;
  TEndpointEndpointVisibilityArray = Array of TEndpointEndpointVisibility;
  TEndpointsListResponseArray = Array of TEndpointsListResponse;
  TOperationArray = Array of TOperation;
  TOperationsListResponseArray = Array of TOperationsListResponse;
  //Anonymous types, using auto-generated names
  TOperationTypeerrorTypeerrorsItem = Class;
  TOperationTypeerror = Class;
  TOperationTypewarningsItemTypedataItem = Class;
  TOperationTypewarningsItem = Class;
  TEndpointsListResponseTypeendpointsArray = Array of TEndpoint;
  TOperationTypeerrorTypeerrorsArray = Array of TOperationTypeerrorTypeerrorsItem;
  TOperationTypewarningsItemTypedataArray = Array of TOperationTypewarningsItemTypedataItem;
  TOperationTypewarningsArray = Array of TOperationTypewarningsItem;
  TOperationsListResponseTypeoperationsArray = Array of TOperation;
  
  { --------------------------------------------------------------------
    TEndpoint
    --------------------------------------------------------------------}
  
  TEndpoint = Class(TGoogleBaseObject)
  Private
    Faddress : String;
    FcreationTimestamp : String;
    Fdescription : String;
    Ffingerprint : String;
    Fid : String;
    Fname : String;
    Fport : integer;
    FselfLink : String;
    Fstate : String;
    Fvisibility : TEndpointEndpointVisibility;
  Protected
    //Property setters
    Procedure Setaddress(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfingerprint(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setport(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvisibility(AIndex : Integer; const AValue : TEndpointEndpointVisibility); virtual;
  Public
  Published
    Property address : String Index 0 Read Faddress Write Setaddress;
    Property creationTimestamp : String Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property fingerprint : String Index 24 Read Ffingerprint Write Setfingerprint;
    Property id : String Index 32 Read Fid Write Setid;
    Property name : String Index 40 Read Fname Write Setname;
    Property port : integer Index 48 Read Fport Write Setport;
    Property selfLink : String Index 56 Read FselfLink Write SetselfLink;
    Property state : String Index 64 Read Fstate Write Setstate;
    Property visibility : TEndpointEndpointVisibility Index 72 Read Fvisibility Write Setvisibility;
  end;
  TEndpointClass = Class of TEndpoint;
  
  { --------------------------------------------------------------------
    TEndpointEndpointVisibility
    --------------------------------------------------------------------}
  
  TEndpointEndpointVisibility = Class(TGoogleBaseObject)
  Private
    FinternalDnsName : String;
    Fnetworks : TStringArray;
  Protected
    //Property setters
    Procedure SetinternalDnsName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnetworks(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property internalDnsName : String Index 0 Read FinternalDnsName Write SetinternalDnsName;
    Property networks : TStringArray Index 8 Read Fnetworks Write Setnetworks;
  end;
  TEndpointEndpointVisibilityClass = Class of TEndpointEndpointVisibility;
  
  { --------------------------------------------------------------------
    TEndpointsListResponse
    --------------------------------------------------------------------}
  
  TEndpointsListResponse = Class(TGoogleBaseObject)
  Private
    Fendpoints : TEndpointsListResponseTypeendpointsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setendpoints(AIndex : Integer; const AValue : TEndpointsListResponseTypeendpointsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property endpoints : TEndpointsListResponseTypeendpointsArray Index 0 Read Fendpoints Write Setendpoints;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TEndpointsListResponseClass = Class of TEndpointsListResponse;
  
  { --------------------------------------------------------------------
    TOperationTypeerrorTypeerrorsItem
    --------------------------------------------------------------------}
  
  TOperationTypeerrorTypeerrorsItem = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    Flocation : String;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property location : String Index 8 Read Flocation Write Setlocation;
    Property message : String Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationTypeerrorTypeerrorsItemClass = Class of TOperationTypeerrorTypeerrorsItem;
  
  { --------------------------------------------------------------------
    TOperationTypeerror
    --------------------------------------------------------------------}
  
  TOperationTypeerror = Class(TGoogleBaseObject)
  Private
    Ferrors : TOperationTypeerrorTypeerrorsArray;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; const AValue : TOperationTypeerrorTypeerrorsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property errors : TOperationTypeerrorTypeerrorsArray Index 0 Read Ferrors Write Seterrors;
  end;
  TOperationTypeerrorClass = Class of TOperationTypeerror;
  
  { --------------------------------------------------------------------
    TOperationTypewarningsItemTypedataItem
    --------------------------------------------------------------------}
  
  TOperationTypewarningsItemTypedataItem = Class(TGoogleBaseObject)
  Private
    Fkey : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property key : String Index 0 Read Fkey Write Setkey;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TOperationTypewarningsItemTypedataItemClass = Class of TOperationTypewarningsItemTypedataItem;
  
  { --------------------------------------------------------------------
    TOperationTypewarningsItem
    --------------------------------------------------------------------}
  
  TOperationTypewarningsItem = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    Fdata : TOperationTypewarningsItemTypedataArray;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdata(AIndex : Integer; const AValue : TOperationTypewarningsItemTypedataArray); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property data : TOperationTypewarningsItemTypedataArray Index 8 Read Fdata Write Setdata;
    Property message : String Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationTypewarningsItemClass = Class of TOperationTypewarningsItem;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    FclientOperationId : String;
    FcreationTimestamp : String;
    Fdescription : String;
    FendTime : String;
    Ferror : TOperationTypeerror;
    FhttpErrorMessage : String;
    FhttpErrorStatusCode : integer;
    Fid : String;
    FinsertTime : String;
    Fkind : String;
    Fname : String;
    FoperationType : String;
    Fprogress : integer;
    Fregion : String;
    FselfLink : String;
    FstartTime : String;
    Fstatus : String;
    FstatusMessage : String;
    FtargetId : String;
    FtargetLink : String;
    Fuser : String;
    Fwarnings : TOperationTypewarningsArray;
    Fzone : String;
  Protected
    //Property setters
    Procedure SetclientOperationId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Seterror(AIndex : Integer; const AValue : TOperationTypeerror); virtual;
    Procedure SethttpErrorMessage(AIndex : Integer; const AValue : String); virtual;
    Procedure SethttpErrorStatusCode(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinsertTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoperationType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setprogress(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setregion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstatusMessage(AIndex : Integer; const AValue : String); virtual;
    Procedure SettargetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettargetLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setuser(AIndex : Integer; const AValue : String); virtual;
    Procedure Setwarnings(AIndex : Integer; const AValue : TOperationTypewarningsArray); virtual;
    Procedure Setzone(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property clientOperationId : String Index 0 Read FclientOperationId Write SetclientOperationId;
    Property creationTimestamp : String Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property endTime : String Index 24 Read FendTime Write SetendTime;
    Property error : TOperationTypeerror Index 32 Read Ferror Write Seterror;
    Property httpErrorMessage : String Index 40 Read FhttpErrorMessage Write SethttpErrorMessage;
    Property httpErrorStatusCode : integer Index 48 Read FhttpErrorStatusCode Write SethttpErrorStatusCode;
    Property id : String Index 56 Read Fid Write Setid;
    Property insertTime : String Index 64 Read FinsertTime Write SetinsertTime;
    Property kind : String Index 72 Read Fkind Write Setkind;
    Property name : String Index 80 Read Fname Write Setname;
    Property operationType : String Index 88 Read FoperationType Write SetoperationType;
    Property progress : integer Index 96 Read Fprogress Write Setprogress;
    Property region : String Index 104 Read Fregion Write Setregion;
    Property selfLink : String Index 112 Read FselfLink Write SetselfLink;
    Property startTime : String Index 120 Read FstartTime Write SetstartTime;
    Property status : String Index 128 Read Fstatus Write Setstatus;
    Property statusMessage : String Index 136 Read FstatusMessage Write SetstatusMessage;
    Property targetId : String Index 144 Read FtargetId Write SettargetId;
    Property targetLink : String Index 152 Read FtargetLink Write SettargetLink;
    Property user : String Index 160 Read Fuser Write Setuser;
    Property warnings : TOperationTypewarningsArray Index 168 Read Fwarnings Write Setwarnings;
    Property zone : String Index 176 Read Fzone Write Setzone;
  end;
  TOperationClass = Class of TOperation;
  
  { --------------------------------------------------------------------
    TOperationsListResponse
    --------------------------------------------------------------------}
  
  TOperationsListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Foperations : TOperationsListResponseTypeoperationsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure Setoperations(AIndex : Integer; const AValue : TOperationsListResponseTypeoperationsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property operations : TOperationsListResponseTypeoperationsArray Index 8 Read Foperations Write Setoperations;
  end;
  TOperationsListResponseClass = Class of TOperationsListResponse;
  
  { --------------------------------------------------------------------
    TEndpointsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TEndpointsResource, method List
  
  TEndpointsListOptions = Record
    filter : String;
    maxResults : integer;
    orderBy : String;
    pageToken : String;
  end;
  
  TEndpointsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(endpoint: string; project: string) : TOperation;
    Function Get(endpoint: string; project: string) : TEndpoint;
    Function Insert(project: string; aEndpoint : TEndpoint) : TOperation;
    Function List(project: string; AQuery : string  = '') : TEndpointsListResponse;
    Function List(project: string; AQuery : TEndpointslistOptions) : TEndpointsListResponse;
    Function Patch(endpoint: string; project: string; aEndpoint : TEndpoint) : TOperation;
    Function Update(endpoint: string; project: string; aEndpoint : TEndpoint) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TOperationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TOperationsResource, method List
  
  TOperationsListOptions = Record
    filter : String;
    maxResults : integer;
    orderBy : String;
    pageToken : String;
  end;
  
  TOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(operation: string; project: string) : TOperation;
    Function List(project: string; AQuery : string  = '') : TOperationsListResponse;
    Function List(project: string; AQuery : TOperationslistOptions) : TOperationsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TServiceregistryAPI
    --------------------------------------------------------------------}
  
  TServiceregistryAPI = Class(TGoogleAPI)
  Private
    FEndpointsInstance : TEndpointsResource;
    FOperationsInstance : TOperationsResource;
    Function GetEndpointsInstance : TEndpointsResource;virtual;
    Function GetOperationsInstance : TOperationsResource;virtual;
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
    Function CreateEndpointsResource(AOwner : TComponent) : TEndpointsResource;virtual;overload;
    Function CreateEndpointsResource : TEndpointsResource;virtual;overload;
    Function CreateOperationsResource(AOwner : TComponent) : TOperationsResource;virtual;overload;
    Function CreateOperationsResource : TOperationsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property EndpointsResource : TEndpointsResource Read GetEndpointsInstance;
    Property OperationsResource : TOperationsResource Read GetOperationsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TEndpoint
  --------------------------------------------------------------------}


Procedure TEndpoint.Setaddress(AIndex : Integer; const AValue : String); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEndpoint.SetcreationTimestamp(AIndex : Integer; const AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEndpoint.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEndpoint.Setfingerprint(AIndex : Integer; const AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEndpoint.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEndpoint.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEndpoint.Setport(AIndex : Integer; const AValue : integer); 

begin
  If (Fport=AValue) then exit;
  Fport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEndpoint.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEndpoint.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEndpoint.Setvisibility(AIndex : Integer; const AValue : TEndpointEndpointVisibility); 

begin
  If (Fvisibility=AValue) then exit;
  Fvisibility:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEndpointEndpointVisibility
  --------------------------------------------------------------------}


Procedure TEndpointEndpointVisibility.SetinternalDnsName(AIndex : Integer; const AValue : String); 

begin
  If (FinternalDnsName=AValue) then exit;
  FinternalDnsName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEndpointEndpointVisibility.Setnetworks(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fnetworks=AValue) then exit;
  Fnetworks:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TEndpointEndpointVisibility.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'networks' : SetLength(Fnetworks,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEndpointsListResponse
  --------------------------------------------------------------------}


Procedure TEndpointsListResponse.Setendpoints(AIndex : Integer; const AValue : TEndpointsListResponseTypeendpointsArray); 

begin
  If (Fendpoints=AValue) then exit;
  Fendpoints:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEndpointsListResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TEndpointsListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'endpoints' : SetLength(Fendpoints,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOperationTypeerrorTypeerrorsItem
  --------------------------------------------------------------------}


Procedure TOperationTypeerrorTypeerrorsItem.Setcode(AIndex : Integer; const AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypeerrorTypeerrorsItem.Setlocation(AIndex : Integer; const AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypeerrorTypeerrorsItem.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypeerror
  --------------------------------------------------------------------}


Procedure TOperationTypeerror.Seterrors(AIndex : Integer; const AValue : TOperationTypeerrorTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperationTypeerror.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOperationTypewarningsItemTypedataItem
  --------------------------------------------------------------------}


Procedure TOperationTypewarningsItemTypedataItem.Setkey(AIndex : Integer; const AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypewarningsItemTypedataItem.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypewarningsItem
  --------------------------------------------------------------------}


Procedure TOperationTypewarningsItem.Setcode(AIndex : Integer; const AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypewarningsItem.Setdata(AIndex : Integer; const AValue : TOperationTypewarningsItemTypedataArray); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypewarningsItem.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperationTypewarningsItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'data' : SetLength(Fdata,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.SetclientOperationId(AIndex : Integer; const AValue : String); 

begin
  If (FclientOperationId=AValue) then exit;
  FclientOperationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetcreationTimestamp(AIndex : Integer; const AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetendTime(AIndex : Integer; const AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Seterror(AIndex : Integer; const AValue : TOperationTypeerror); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SethttpErrorMessage(AIndex : Integer; const AValue : String); 

begin
  If (FhttpErrorMessage=AValue) then exit;
  FhttpErrorMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SethttpErrorStatusCode(AIndex : Integer; const AValue : integer); 

begin
  If (FhttpErrorStatusCode=AValue) then exit;
  FhttpErrorStatusCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetinsertTime(AIndex : Integer; const AValue : String); 

begin
  If (FinsertTime=AValue) then exit;
  FinsertTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetoperationType(AIndex : Integer; const AValue : String); 

begin
  If (FoperationType=AValue) then exit;
  FoperationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setprogress(AIndex : Integer; const AValue : integer); 

begin
  If (Fprogress=AValue) then exit;
  Fprogress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setregion(AIndex : Integer; const AValue : String); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstartTime(AIndex : Integer; const AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setstatus(AIndex : Integer; const AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstatusMessage(AIndex : Integer; const AValue : String); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetId(AIndex : Integer; const AValue : String); 

begin
  If (FtargetId=AValue) then exit;
  FtargetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetLink(AIndex : Integer; const AValue : String); 

begin
  If (FtargetLink=AValue) then exit;
  FtargetLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setuser(AIndex : Integer; const AValue : String); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setwarnings(AIndex : Integer; const AValue : TOperationTypewarningsArray); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setzone(AIndex : Integer; const AValue : String); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperation.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'warnings' : SetLength(Fwarnings,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOperationsListResponse
  --------------------------------------------------------------------}


Procedure TOperationsListResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationsListResponse.Setoperations(AIndex : Integer; const AValue : TOperationsListResponseTypeoperationsArray); 

begin
  If (Foperations=AValue) then exit;
  Foperations:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperationsListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'operations' : SetLength(Foperations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEndpointsResource
  --------------------------------------------------------------------}


Class Function TEndpointsResource.ResourceName : String;

begin
  Result:='endpoints';
end;

Class Function TEndpointsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TserviceregistryAPI;
end;

Function TEndpointsResource.Delete(endpoint: string; project: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/global/endpoints/{endpoint}';
  _Methodid   = 'serviceregistry.endpoints.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['endpoint',endpoint,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TEndpointsResource.Get(endpoint: string; project: string) : TEndpoint;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/endpoints/{endpoint}';
  _Methodid   = 'serviceregistry.endpoints.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['endpoint',endpoint,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEndpoint) as TEndpoint;
end;

Function TEndpointsResource.Insert(project: string; aEndpoint : TEndpoint) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/global/endpoints';
  _Methodid   = 'serviceregistry.endpoints.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEndpoint,TOperation) as TOperation;
end;

Function TEndpointsResource.List(project: string; AQuery : string = '') : TEndpointsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/endpoints';
  _Methodid   = 'serviceregistry.endpoints.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEndpointsListResponse) as TEndpointsListResponse;
end;


Function TEndpointsResource.List(project: string; AQuery : TEndpointslistOptions) : TEndpointsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;

Function TEndpointsResource.Patch(endpoint: string; project: string; aEndpoint : TEndpoint) : TOperation;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{project}/global/endpoints/{endpoint}';
  _Methodid   = 'serviceregistry.endpoints.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['endpoint',endpoint,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEndpoint,TOperation) as TOperation;
end;

Function TEndpointsResource.Update(endpoint: string; project: string; aEndpoint : TEndpoint) : TOperation;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{project}/global/endpoints/{endpoint}';
  _Methodid   = 'serviceregistry.endpoints.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['endpoint',endpoint,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aEndpoint,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TOperationsResource
  --------------------------------------------------------------------}


Class Function TOperationsResource.ResourceName : String;

begin
  Result:='operations';
end;

Class Function TOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TserviceregistryAPI;
end;

Function TOperationsResource.Get(operation: string; project: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/operations/{operation}';
  _Methodid   = 'serviceregistry.operations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operation',operation,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TOperationsResource.List(project: string; AQuery : string = '') : TOperationsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/global/operations';
  _Methodid   = 'serviceregistry.operations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperationsListResponse) as TOperationsListResponse;
end;


Function TOperationsResource.List(project: string; AQuery : TOperationslistOptions) : TOperationsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;



{ --------------------------------------------------------------------
  TServiceregistryAPI
  --------------------------------------------------------------------}

Class Function TServiceregistryAPI.APIName : String;

begin
  Result:='serviceregistry';
end;

Class Function TServiceregistryAPI.APIVersion : String;

begin
  Result:='alpha';
end;

Class Function TServiceregistryAPI.APIRevision : String;

begin
  Result:='20160516';
end;

Class Function TServiceregistryAPI.APIID : String;

begin
  Result:='serviceregistry:alpha';
end;

Class Function TServiceregistryAPI.APITitle : String;

begin
  Result:='Google Cloud Service Registry API';
end;

Class Function TServiceregistryAPI.APIDescription : String;

begin
  Result:='Manages service endpoints in Service Registry and provides integration with DNS for service discovery and name resolution.';
end;

Class Function TServiceregistryAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TServiceregistryAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TServiceregistryAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TServiceregistryAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TServiceregistryAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/service-registry/';
end;

Class Function TServiceregistryAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TServiceregistryAPI.APIbasePath : string;

begin
  Result:='/serviceregistry/alpha/projects/';
end;

Class Function TServiceregistryAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/serviceregistry/alpha/projects/';
end;

Class Function TServiceregistryAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TServiceregistryAPI.APIservicePath : string;

begin
  Result:='serviceregistry/alpha/projects/';
end;

Class Function TServiceregistryAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TServiceregistryAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,4);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/cloud-platform.read-only';
  Result[1].Description:='View your data across Google Cloud Platform services';
  Result[2].Name:='https://www.googleapis.com/auth/ndev.cloudman';
  Result[2].Description:='View and manage your Google Cloud Platform management resources and deployment status information';
  Result[3].Name:='https://www.googleapis.com/auth/ndev.cloudman.readonly';
  Result[3].Description:='View your Google Cloud Platform management resources and deployment status information';
  
end;

Class Function TServiceregistryAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TServiceregistryAPI.RegisterAPIResources;

begin
  TEndpoint.RegisterObject;
  TEndpointEndpointVisibility.RegisterObject;
  TEndpointsListResponse.RegisterObject;
  TOperationTypeerrorTypeerrorsItem.RegisterObject;
  TOperationTypeerror.RegisterObject;
  TOperationTypewarningsItemTypedataItem.RegisterObject;
  TOperationTypewarningsItem.RegisterObject;
  TOperation.RegisterObject;
  TOperationsListResponse.RegisterObject;
end;


Function TServiceregistryAPI.GetEndpointsInstance : TEndpointsResource;

begin
  if (FEndpointsInstance=Nil) then
    FEndpointsInstance:=CreateEndpointsResource;
  Result:=FEndpointsInstance;
end;

Function TServiceregistryAPI.CreateEndpointsResource : TEndpointsResource;

begin
  Result:=CreateEndpointsResource(Self);
end;


Function TServiceregistryAPI.CreateEndpointsResource(AOwner : TComponent) : TEndpointsResource;

begin
  Result:=TEndpointsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TServiceregistryAPI.GetOperationsInstance : TOperationsResource;

begin
  if (FOperationsInstance=Nil) then
    FOperationsInstance:=CreateOperationsResource;
  Result:=FOperationsInstance;
end;

Function TServiceregistryAPI.CreateOperationsResource : TOperationsResource;

begin
  Result:=CreateOperationsResource(Self);
end;


Function TServiceregistryAPI.CreateOperationsResource(AOwner : TComponent) : TOperationsResource;

begin
  Result:=TOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TServiceregistryAPI.RegisterAPI;
end.
