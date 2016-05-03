unit googlecontainer;
{
   **********************************************************************
      This file is part of the Free Component Library (FCL)
      Copyright (c) 2015 The free pascal team.
  
      See the file COPYING.FPC, included in this distribution,
      for details about the copyright.
  
      This program is distributed in the hope that it will be useful,
      but WITHOUT ANY WARRANTY; without even the implied warranty of
      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  
   **********************************************************************
}
//Generated on: 16-5-15 08:53:01
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TCluster = Class;
  TCreateClusterRequest = Class;
  TListAggregatedClustersResponse = Class;
  TListAggregatedOperationsResponse = Class;
  TListClustersResponse = Class;
  TListOperationsResponse = Class;
  TMasterAuth = Class;
  TNodeConfig = Class;
  TOperation = Class;
  TServiceAccount = Class;
  TClusterArray = Array of TCluster;
  TCreateClusterRequestArray = Array of TCreateClusterRequest;
  TListAggregatedClustersResponseArray = Array of TListAggregatedClustersResponse;
  TListAggregatedOperationsResponseArray = Array of TListAggregatedOperationsResponse;
  TListClustersResponseArray = Array of TListClustersResponse;
  TListOperationsResponseArray = Array of TListOperationsResponse;
  TMasterAuthArray = Array of TMasterAuth;
  TNodeConfigArray = Array of TNodeConfig;
  TOperationArray = Array of TOperation;
  TServiceAccountArray = Array of TServiceAccount;
  //Anonymous types, using auto-generated names
  TListAggregatedClustersResponseTypeclustersArray = Array of TCluster;
  TListAggregatedOperationsResponseTypeoperationsArray = Array of TOperation;
  TListClustersResponseTypeclustersArray = Array of TCluster;
  TListOperationsResponseTypeoperationsArray = Array of TOperation;
  TNodeConfigTypeserviceAccountsArray = Array of TServiceAccount;
  
  { --------------------------------------------------------------------
    TCluster
    --------------------------------------------------------------------}
  
  TCluster = Class(TGoogleBaseObject)
  Private
    FclusterApiVersion : String;
    FcontainerIpv4Cidr : String;
    FcreationTimestamp : String;
    Fdescription : String;
    FenableCloudLogging : boolean;
    Fendpoint : String;
    FmasterAuth : TMasterAuth;
    Fname : String;
    Fnetwork : String;
    FnodeConfig : TNodeConfig;
    FnodeRoutingPrefixSize : integer;
    FnumNodes : integer;
    FselfLink : String;
    FservicesIpv4Cidr : String;
    Fstatus : String;
    FstatusMessage : String;
    Fzone : String;
  Protected
    //Property setters
    Procedure SetclusterApiVersion(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontainerIpv4Cidr(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetenableCloudLogging(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setendpoint(AIndex : Integer; AValue : String); virtual;
    Procedure SetmasterAuth(AIndex : Integer; AValue : TMasterAuth); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setnetwork(AIndex : Integer; AValue : String); virtual;
    Procedure SetnodeConfig(AIndex : Integer; AValue : TNodeConfig); virtual;
    Procedure SetnodeRoutingPrefixSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumNodes(AIndex : Integer; AValue : integer); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetservicesIpv4Cidr(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetstatusMessage(AIndex : Integer; AValue : String); virtual;
    Procedure Setzone(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property clusterApiVersion : String Index 0 Read FclusterApiVersion Write SetclusterApiVersion;
    Property containerIpv4Cidr : String Index 8 Read FcontainerIpv4Cidr Write SetcontainerIpv4Cidr;
    Property creationTimestamp : String Index 16 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property enableCloudLogging : boolean Index 32 Read FenableCloudLogging Write SetenableCloudLogging;
    Property endpoint : String Index 40 Read Fendpoint Write Setendpoint;
    Property masterAuth : TMasterAuth Index 48 Read FmasterAuth Write SetmasterAuth;
    Property name : String Index 56 Read Fname Write Setname;
    Property network : String Index 64 Read Fnetwork Write Setnetwork;
    Property nodeConfig : TNodeConfig Index 72 Read FnodeConfig Write SetnodeConfig;
    Property nodeRoutingPrefixSize : integer Index 80 Read FnodeRoutingPrefixSize Write SetnodeRoutingPrefixSize;
    Property numNodes : integer Index 88 Read FnumNodes Write SetnumNodes;
    Property selfLink : String Index 96 Read FselfLink Write SetselfLink;
    Property servicesIpv4Cidr : String Index 104 Read FservicesIpv4Cidr Write SetservicesIpv4Cidr;
    Property status : String Index 112 Read Fstatus Write Setstatus;
    Property statusMessage : String Index 120 Read FstatusMessage Write SetstatusMessage;
    Property zone : String Index 128 Read Fzone Write Setzone;
  end;
  TClusterClass = Class of TCluster;
  
  { --------------------------------------------------------------------
    TCreateClusterRequest
    --------------------------------------------------------------------}
  
  TCreateClusterRequest = Class(TGoogleBaseObject)
  Private
    Fcluster : TCluster;
  Protected
    //Property setters
    Procedure Setcluster(AIndex : Integer; AValue : TCluster); virtual;
  Public
  Published
    Property cluster : TCluster Index 0 Read Fcluster Write Setcluster;
  end;
  TCreateClusterRequestClass = Class of TCreateClusterRequest;
  
  { --------------------------------------------------------------------
    TListAggregatedClustersResponse
    --------------------------------------------------------------------}
  
  TListAggregatedClustersResponse = Class(TGoogleBaseObject)
  Private
    Fclusters : TListAggregatedClustersResponseTypeclustersArray;
  Protected
    //Property setters
    Procedure Setclusters(AIndex : Integer; AValue : TListAggregatedClustersResponseTypeclustersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property clusters : TListAggregatedClustersResponseTypeclustersArray Index 0 Read Fclusters Write Setclusters;
  end;
  TListAggregatedClustersResponseClass = Class of TListAggregatedClustersResponse;
  
  { --------------------------------------------------------------------
    TListAggregatedOperationsResponse
    --------------------------------------------------------------------}
  
  TListAggregatedOperationsResponse = Class(TGoogleBaseObject)
  Private
    Foperations : TListAggregatedOperationsResponseTypeoperationsArray;
  Protected
    //Property setters
    Procedure Setoperations(AIndex : Integer; AValue : TListAggregatedOperationsResponseTypeoperationsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property operations : TListAggregatedOperationsResponseTypeoperationsArray Index 0 Read Foperations Write Setoperations;
  end;
  TListAggregatedOperationsResponseClass = Class of TListAggregatedOperationsResponse;
  
  { --------------------------------------------------------------------
    TListClustersResponse
    --------------------------------------------------------------------}
  
  TListClustersResponse = Class(TGoogleBaseObject)
  Private
    Fclusters : TListClustersResponseTypeclustersArray;
  Protected
    //Property setters
    Procedure Setclusters(AIndex : Integer; AValue : TListClustersResponseTypeclustersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property clusters : TListClustersResponseTypeclustersArray Index 0 Read Fclusters Write Setclusters;
  end;
  TListClustersResponseClass = Class of TListClustersResponse;
  
  { --------------------------------------------------------------------
    TListOperationsResponse
    --------------------------------------------------------------------}
  
  TListOperationsResponse = Class(TGoogleBaseObject)
  Private
    Foperations : TListOperationsResponseTypeoperationsArray;
  Protected
    //Property setters
    Procedure Setoperations(AIndex : Integer; AValue : TListOperationsResponseTypeoperationsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property operations : TListOperationsResponseTypeoperationsArray Index 0 Read Foperations Write Setoperations;
  end;
  TListOperationsResponseClass = Class of TListOperationsResponse;
  
  { --------------------------------------------------------------------
    TMasterAuth
    --------------------------------------------------------------------}
  
  TMasterAuth = Class(TGoogleBaseObject)
  Private
    FbearerToken : String;
    Fpassword : String;
    Fuser : String;
  Protected
    //Property setters
    Procedure SetbearerToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setpassword(AIndex : Integer; AValue : String); virtual;
    Procedure Setuser(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property bearerToken : String Index 0 Read FbearerToken Write SetbearerToken;
    Property password : String Index 8 Read Fpassword Write Setpassword;
    Property user : String Index 16 Read Fuser Write Setuser;
  end;
  TMasterAuthClass = Class of TMasterAuth;
  
  { --------------------------------------------------------------------
    TNodeConfig
    --------------------------------------------------------------------}
  
  TNodeConfig = Class(TGoogleBaseObject)
  Private
    FmachineType : String;
    FserviceAccounts : TNodeConfigTypeserviceAccountsArray;
    FsourceImage : String;
  Protected
    //Property setters
    Procedure SetmachineType(AIndex : Integer; AValue : String); virtual;
    Procedure SetserviceAccounts(AIndex : Integer; AValue : TNodeConfigTypeserviceAccountsArray); virtual;
    Procedure SetsourceImage(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property machineType : String Index 0 Read FmachineType Write SetmachineType;
    Property serviceAccounts : TNodeConfigTypeserviceAccountsArray Index 8 Read FserviceAccounts Write SetserviceAccounts;
    Property sourceImage : String Index 16 Read FsourceImage Write SetsourceImage;
  end;
  TNodeConfigClass = Class of TNodeConfig;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    FerrorMessage : String;
    Fname : String;
    FoperationType : String;
    FselfLink : String;
    Fstatus : String;
    Ftarget : String;
    FtargetLink : String;
    Fzone : String;
  Protected
    //Property setters
    Procedure SeterrorMessage(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetoperationType(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure Settarget(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setzone(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property errorMessage : String Index 0 Read FerrorMessage Write SeterrorMessage;
    Property name : String Index 8 Read Fname Write Setname;
    Property operationType : String Index 16 Read FoperationType Write SetoperationType;
    Property selfLink : String Index 24 Read FselfLink Write SetselfLink;
    Property status : String Index 32 Read Fstatus Write Setstatus;
    Property target : String Index 40 Read Ftarget Write Settarget;
    Property targetLink : String Index 48 Read FtargetLink Write SettargetLink;
    Property zone : String Index 56 Read Fzone Write Setzone;
  end;
  TOperationClass = Class of TOperation;
  
  { --------------------------------------------------------------------
    TServiceAccount
    --------------------------------------------------------------------}
  
  TServiceAccount = Class(TGoogleBaseObject)
  Private
    Femail : String;
    Fscopes : TStringArray;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure Setscopes(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property email : String Index 0 Read Femail Write Setemail;
    Property scopes : TStringArray Index 8 Read Fscopes Write Setscopes;
  end;
  TServiceAccountClass = Class of TServiceAccount;
  
  { --------------------------------------------------------------------
    TProjectsClustersResource
    --------------------------------------------------------------------}
  
  TProjectsClustersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(projectId: string) : TListAggregatedClustersResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsOperationsResource
    --------------------------------------------------------------------}
  
  TProjectsOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(projectId: string) : TListAggregatedOperationsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsZonesClustersResource
    --------------------------------------------------------------------}
  
  TProjectsZonesClustersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(projectId: string; zoneId: string; aCreateClusterRequest : TCreateClusterRequest) : TOperation;overload;
    Function Delete(clusterId: string; projectId: string; zoneId: string) : TOperation;
    Function Get(clusterId: string; projectId: string; zoneId: string) : TCluster;
    Function List(projectId: string; zoneId: string) : TListClustersResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsZonesOperationsResource
    --------------------------------------------------------------------}
  
  TProjectsZonesOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(operationId: string; projectId: string; zoneId: string) : TOperation;
    Function List(projectId: string; zoneId: string) : TListOperationsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsZonesResource
    --------------------------------------------------------------------}
  
  TProjectsZonesResource = Class(TGoogleResource)
  Private
    FClustersInstance : TProjectsZonesClustersResource;
    FOperationsInstance : TProjectsZonesOperationsResource;
    Function GetClustersInstance : TProjectsZonesClustersResource;virtual;
    Function GetOperationsInstance : TProjectsZonesOperationsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateClustersResource(AOwner : TComponent) : TProjectsZonesClustersResource;virtual;overload;
    Function CreateClustersResource : TProjectsZonesClustersResource;virtual;overload;
    Function CreateOperationsResource(AOwner : TComponent) : TProjectsZonesOperationsResource;virtual;overload;
    Function CreateOperationsResource : TProjectsZonesOperationsResource;virtual;overload;
    Property ClustersResource : TProjectsZonesClustersResource Read GetClustersInstance;
    Property OperationsResource : TProjectsZonesOperationsResource Read GetOperationsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Private
    FClustersInstance : TProjectsClustersResource;
    FOperationsInstance : TProjectsOperationsResource;
    FZonesClustersInstance : TProjectsZonesClustersResource;
    FZonesOperationsInstance : TProjectsZonesOperationsResource;
    FZonesInstance : TProjectsZonesResource;
    Function GetClustersInstance : TProjectsClustersResource;virtual;
    Function GetOperationsInstance : TProjectsOperationsResource;virtual;
    Function GetZonesClustersInstance : TProjectsZonesClustersResource;virtual;
    Function GetZonesOperationsInstance : TProjectsZonesOperationsResource;virtual;
    Function GetZonesInstance : TProjectsZonesResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateClustersResource(AOwner : TComponent) : TProjectsClustersResource;virtual;overload;
    Function CreateClustersResource : TProjectsClustersResource;virtual;overload;
    Function CreateOperationsResource(AOwner : TComponent) : TProjectsOperationsResource;virtual;overload;
    Function CreateOperationsResource : TProjectsOperationsResource;virtual;overload;
    Function CreateZonesClustersResource(AOwner : TComponent) : TProjectsZonesClustersResource;virtual;overload;
    Function CreateZonesClustersResource : TProjectsZonesClustersResource;virtual;overload;
    Function CreateZonesOperationsResource(AOwner : TComponent) : TProjectsZonesOperationsResource;virtual;overload;
    Function CreateZonesOperationsResource : TProjectsZonesOperationsResource;virtual;overload;
    Function CreateZonesResource(AOwner : TComponent) : TProjectsZonesResource;virtual;overload;
    Function CreateZonesResource : TProjectsZonesResource;virtual;overload;
    Property ClustersResource : TProjectsClustersResource Read GetClustersInstance;
    Property OperationsResource : TProjectsOperationsResource Read GetOperationsInstance;
    Property ZonesClustersResource : TProjectsZonesClustersResource Read GetZonesClustersInstance;
    Property ZonesOperationsResource : TProjectsZonesOperationsResource Read GetZonesOperationsInstance;
    Property ZonesResource : TProjectsZonesResource Read GetZonesInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TContainerAPI
    --------------------------------------------------------------------}
  
  TContainerAPI = Class(TGoogleAPI)
  Private
    FProjectsClustersInstance : TProjectsClustersResource;
    FProjectsOperationsInstance : TProjectsOperationsResource;
    FProjectsZonesClustersInstance : TProjectsZonesClustersResource;
    FProjectsZonesOperationsInstance : TProjectsZonesOperationsResource;
    FProjectsZonesInstance : TProjectsZonesResource;
    FProjectsInstance : TProjectsResource;
    Function GetProjectsClustersInstance : TProjectsClustersResource;virtual;
    Function GetProjectsOperationsInstance : TProjectsOperationsResource;virtual;
    Function GetProjectsZonesClustersInstance : TProjectsZonesClustersResource;virtual;
    Function GetProjectsZonesOperationsInstance : TProjectsZonesOperationsResource;virtual;
    Function GetProjectsZonesInstance : TProjectsZonesResource;virtual;
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
    Function CreateProjectsClustersResource(AOwner : TComponent) : TProjectsClustersResource;virtual;overload;
    Function CreateProjectsClustersResource : TProjectsClustersResource;virtual;overload;
    Function CreateProjectsOperationsResource(AOwner : TComponent) : TProjectsOperationsResource;virtual;overload;
    Function CreateProjectsOperationsResource : TProjectsOperationsResource;virtual;overload;
    Function CreateProjectsZonesClustersResource(AOwner : TComponent) : TProjectsZonesClustersResource;virtual;overload;
    Function CreateProjectsZonesClustersResource : TProjectsZonesClustersResource;virtual;overload;
    Function CreateProjectsZonesOperationsResource(AOwner : TComponent) : TProjectsZonesOperationsResource;virtual;overload;
    Function CreateProjectsZonesOperationsResource : TProjectsZonesOperationsResource;virtual;overload;
    Function CreateProjectsZonesResource(AOwner : TComponent) : TProjectsZonesResource;virtual;overload;
    Function CreateProjectsZonesResource : TProjectsZonesResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsClustersResource : TProjectsClustersResource Read GetProjectsClustersInstance;
    Property ProjectsOperationsResource : TProjectsOperationsResource Read GetProjectsOperationsInstance;
    Property ProjectsZonesClustersResource : TProjectsZonesClustersResource Read GetProjectsZonesClustersInstance;
    Property ProjectsZonesOperationsResource : TProjectsZonesOperationsResource Read GetProjectsZonesOperationsInstance;
    Property ProjectsZonesResource : TProjectsZonesResource Read GetProjectsZonesInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TCluster
  --------------------------------------------------------------------}


Procedure TCluster.SetclusterApiVersion(AIndex : Integer; AValue : String); 

begin
  If (FclusterApiVersion=AValue) then exit;
  FclusterApiVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetcontainerIpv4Cidr(AIndex : Integer; AValue : String); 

begin
  If (FcontainerIpv4Cidr=AValue) then exit;
  FcontainerIpv4Cidr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetcreationTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetenableCloudLogging(AIndex : Integer; AValue : boolean); 

begin
  If (FenableCloudLogging=AValue) then exit;
  FenableCloudLogging:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setendpoint(AIndex : Integer; AValue : String); 

begin
  If (Fendpoint=AValue) then exit;
  Fendpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetmasterAuth(AIndex : Integer; AValue : TMasterAuth); 

begin
  If (FmasterAuth=AValue) then exit;
  FmasterAuth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setnetwork(AIndex : Integer; AValue : String); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetnodeConfig(AIndex : Integer; AValue : TNodeConfig); 

begin
  If (FnodeConfig=AValue) then exit;
  FnodeConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetnodeRoutingPrefixSize(AIndex : Integer; AValue : integer); 

begin
  If (FnodeRoutingPrefixSize=AValue) then exit;
  FnodeRoutingPrefixSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetnumNodes(AIndex : Integer; AValue : integer); 

begin
  If (FnumNodes=AValue) then exit;
  FnumNodes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetservicesIpv4Cidr(AIndex : Integer; AValue : String); 

begin
  If (FservicesIpv4Cidr=AValue) then exit;
  FservicesIpv4Cidr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetstatusMessage(AIndex : Integer; AValue : String); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setzone(AIndex : Integer; AValue : String); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreateClusterRequest
  --------------------------------------------------------------------}


Procedure TCreateClusterRequest.Setcluster(AIndex : Integer; AValue : TCluster); 

begin
  If (Fcluster=AValue) then exit;
  Fcluster:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListAggregatedClustersResponse
  --------------------------------------------------------------------}


Procedure TListAggregatedClustersResponse.Setclusters(AIndex : Integer; AValue : TListAggregatedClustersResponseTypeclustersArray); 

begin
  If (Fclusters=AValue) then exit;
  Fclusters:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListAggregatedClustersResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'clusters' : SetLength(Fclusters,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListAggregatedOperationsResponse
  --------------------------------------------------------------------}


Procedure TListAggregatedOperationsResponse.Setoperations(AIndex : Integer; AValue : TListAggregatedOperationsResponseTypeoperationsArray); 

begin
  If (Foperations=AValue) then exit;
  Foperations:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListAggregatedOperationsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'operations' : SetLength(Foperations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListClustersResponse
  --------------------------------------------------------------------}


Procedure TListClustersResponse.Setclusters(AIndex : Integer; AValue : TListClustersResponseTypeclustersArray); 

begin
  If (Fclusters=AValue) then exit;
  Fclusters:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListClustersResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'clusters' : SetLength(Fclusters,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListOperationsResponse
  --------------------------------------------------------------------}


Procedure TListOperationsResponse.Setoperations(AIndex : Integer; AValue : TListOperationsResponseTypeoperationsArray); 

begin
  If (Foperations=AValue) then exit;
  Foperations:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListOperationsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'operations' : SetLength(Foperations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMasterAuth
  --------------------------------------------------------------------}


Procedure TMasterAuth.SetbearerToken(AIndex : Integer; AValue : String); 

begin
  If (FbearerToken=AValue) then exit;
  FbearerToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMasterAuth.Setpassword(AIndex : Integer; AValue : String); 

begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMasterAuth.Setuser(AIndex : Integer; AValue : String); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNodeConfig
  --------------------------------------------------------------------}


Procedure TNodeConfig.SetmachineType(AIndex : Integer; AValue : String); 

begin
  If (FmachineType=AValue) then exit;
  FmachineType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNodeConfig.SetserviceAccounts(AIndex : Integer; AValue : TNodeConfigTypeserviceAccountsArray); 

begin
  If (FserviceAccounts=AValue) then exit;
  FserviceAccounts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNodeConfig.SetsourceImage(AIndex : Integer; AValue : String); 

begin
  If (FsourceImage=AValue) then exit;
  FsourceImage:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TNodeConfig.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'serviceaccounts' : SetLength(FserviceAccounts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.SeterrorMessage(AIndex : Integer; AValue : String); 

begin
  If (FerrorMessage=AValue) then exit;
  FerrorMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetoperationType(AIndex : Integer; AValue : String); 

begin
  If (FoperationType=AValue) then exit;
  FoperationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Settarget(AIndex : Integer; AValue : String); 

begin
  If (Ftarget=AValue) then exit;
  Ftarget:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetLink(AIndex : Integer; AValue : String); 

begin
  If (FtargetLink=AValue) then exit;
  FtargetLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setzone(AIndex : Integer; AValue : String); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TServiceAccount
  --------------------------------------------------------------------}


Procedure TServiceAccount.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServiceAccount.Setscopes(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fscopes=AValue) then exit;
  Fscopes:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TServiceAccount.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'scopes' : SetLength(Fscopes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProjectsClustersResource
  --------------------------------------------------------------------}


Class Function TProjectsClustersResource.ResourceName : String;

begin
  Result:='clusters';
end;

Class Function TProjectsClustersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcontainerAPI;
end;

Function TProjectsClustersResource.List(projectId: string) : TListAggregatedClustersResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/clusters';
  _Methodid   = 'container.projects.clusters.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListAggregatedClustersResponse) as TListAggregatedClustersResponse;
end;



{ --------------------------------------------------------------------
  TProjectsOperationsResource
  --------------------------------------------------------------------}


Class Function TProjectsOperationsResource.ResourceName : String;

begin
  Result:='operations';
end;

Class Function TProjectsOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcontainerAPI;
end;

Function TProjectsOperationsResource.List(projectId: string) : TListAggregatedOperationsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/operations';
  _Methodid   = 'container.projects.operations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListAggregatedOperationsResponse) as TListAggregatedOperationsResponse;
end;



{ --------------------------------------------------------------------
  TProjectsZonesClustersResource
  --------------------------------------------------------------------}


Class Function TProjectsZonesClustersResource.ResourceName : String;

begin
  Result:='clusters';
end;

Class Function TProjectsZonesClustersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcontainerAPI;
end;

Function TProjectsZonesClustersResource.Create(projectId: string; zoneId: string; aCreateClusterRequest : TCreateClusterRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{projectId}/zones/{zoneId}/clusters';
  _Methodid   = 'container.projects.zones.clusters.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'zoneId',zoneId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreateClusterRequest,TOperation) as TOperation;
end;

Function TProjectsZonesClustersResource.Delete(clusterId: string; projectId: string; zoneId: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{projectId}/zones/{zoneId}/clusters/{clusterId}';
  _Methodid   = 'container.projects.zones.clusters.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['clusterId',clusterId,'projectId',projectId,'zoneId',zoneId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TProjectsZonesClustersResource.Get(clusterId: string; projectId: string; zoneId: string) : TCluster;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/zones/{zoneId}/clusters/{clusterId}';
  _Methodid   = 'container.projects.zones.clusters.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['clusterId',clusterId,'projectId',projectId,'zoneId',zoneId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCluster) as TCluster;
end;

Function TProjectsZonesClustersResource.List(projectId: string; zoneId: string) : TListClustersResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/zones/{zoneId}/clusters';
  _Methodid   = 'container.projects.zones.clusters.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'zoneId',zoneId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListClustersResponse) as TListClustersResponse;
end;



{ --------------------------------------------------------------------
  TProjectsZonesOperationsResource
  --------------------------------------------------------------------}


Class Function TProjectsZonesOperationsResource.ResourceName : String;

begin
  Result:='operations';
end;

Class Function TProjectsZonesOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcontainerAPI;
end;

Function TProjectsZonesOperationsResource.Get(operationId: string; projectId: string; zoneId: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/zones/{zoneId}/operations/{operationId}';
  _Methodid   = 'container.projects.zones.operations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operationId',operationId,'projectId',projectId,'zoneId',zoneId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TProjectsZonesOperationsResource.List(projectId: string; zoneId: string) : TListOperationsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/zones/{zoneId}/operations';
  _Methodid   = 'container.projects.zones.operations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'zoneId',zoneId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListOperationsResponse) as TListOperationsResponse;
end;



{ --------------------------------------------------------------------
  TProjectsZonesResource
  --------------------------------------------------------------------}


Class Function TProjectsZonesResource.ResourceName : String;

begin
  Result:='zones';
end;

Class Function TProjectsZonesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcontainerAPI;
end;



Function TProjectsZonesResource.GetClustersInstance : TProjectsZonesClustersResource;

begin
  if (FClustersInstance=Nil) then
    FClustersInstance:=CreateClustersResource;
  Result:=FClustersInstance;
end;

Function TProjectsZonesResource.CreateClustersResource : TProjectsZonesClustersResource;

begin
  Result:=CreateClustersResource(Self);
end;


Function TProjectsZonesResource.CreateClustersResource(AOwner : TComponent) : TProjectsZonesClustersResource;

begin
  Result:=TProjectsZonesClustersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsZonesResource.GetOperationsInstance : TProjectsZonesOperationsResource;

begin
  if (FOperationsInstance=Nil) then
    FOperationsInstance:=CreateOperationsResource;
  Result:=FOperationsInstance;
end;

Function TProjectsZonesResource.CreateOperationsResource : TProjectsZonesOperationsResource;

begin
  Result:=CreateOperationsResource(Self);
end;


Function TProjectsZonesResource.CreateOperationsResource(AOwner : TComponent) : TProjectsZonesOperationsResource;

begin
  Result:=TProjectsZonesOperationsResource.Create(AOwner);
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
  Result:=TcontainerAPI;
end;



Function TProjectsResource.GetClustersInstance : TProjectsClustersResource;

begin
  if (FClustersInstance=Nil) then
    FClustersInstance:=CreateClustersResource;
  Result:=FClustersInstance;
end;

Function TProjectsResource.CreateClustersResource : TProjectsClustersResource;

begin
  Result:=CreateClustersResource(Self);
end;


Function TProjectsResource.CreateClustersResource(AOwner : TComponent) : TProjectsClustersResource;

begin
  Result:=TProjectsClustersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetOperationsInstance : TProjectsOperationsResource;

begin
  if (FOperationsInstance=Nil) then
    FOperationsInstance:=CreateOperationsResource;
  Result:=FOperationsInstance;
end;

Function TProjectsResource.CreateOperationsResource : TProjectsOperationsResource;

begin
  Result:=CreateOperationsResource(Self);
end;


Function TProjectsResource.CreateOperationsResource(AOwner : TComponent) : TProjectsOperationsResource;

begin
  Result:=TProjectsOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetZonesClustersInstance : TProjectsZonesClustersResource;

begin
  if (FZonesClustersInstance=Nil) then
    FZonesClustersInstance:=CreateZonesClustersResource;
  Result:=FZonesClustersInstance;
end;

Function TProjectsResource.CreateZonesClustersResource : TProjectsZonesClustersResource;

begin
  Result:=CreateZonesClustersResource(Self);
end;


Function TProjectsResource.CreateZonesClustersResource(AOwner : TComponent) : TProjectsZonesClustersResource;

begin
  Result:=TProjectsZonesClustersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetZonesOperationsInstance : TProjectsZonesOperationsResource;

begin
  if (FZonesOperationsInstance=Nil) then
    FZonesOperationsInstance:=CreateZonesOperationsResource;
  Result:=FZonesOperationsInstance;
end;

Function TProjectsResource.CreateZonesOperationsResource : TProjectsZonesOperationsResource;

begin
  Result:=CreateZonesOperationsResource(Self);
end;


Function TProjectsResource.CreateZonesOperationsResource(AOwner : TComponent) : TProjectsZonesOperationsResource;

begin
  Result:=TProjectsZonesOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetZonesInstance : TProjectsZonesResource;

begin
  if (FZonesInstance=Nil) then
    FZonesInstance:=CreateZonesResource;
  Result:=FZonesInstance;
end;

Function TProjectsResource.CreateZonesResource : TProjectsZonesResource;

begin
  Result:=CreateZonesResource(Self);
end;


Function TProjectsResource.CreateZonesResource(AOwner : TComponent) : TProjectsZonesResource;

begin
  Result:=TProjectsZonesResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TContainerAPI
  --------------------------------------------------------------------}

Class Function TContainerAPI.APIName : String;

begin
  Result:='container';
end;

Class Function TContainerAPI.APIVersion : String;

begin
  Result:='v1beta1';
end;

Class Function TContainerAPI.APIRevision : String;

begin
  Result:='20150504';
end;

Class Function TContainerAPI.APIID : String;

begin
  Result:='container:v1beta1';
end;

Class Function TContainerAPI.APITitle : String;

begin
  Result:='Google Container Engine API';
end;

Class Function TContainerAPI.APIDescription : String;

begin
  Result:='The Google Container Engine API is used for building and managing container based applications, powered by the open source Kubernetes technology.';
end;

Class Function TContainerAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TContainerAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TContainerAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TContainerAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TContainerAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/container-engine/docs/v1beta1/';
end;

Class Function TContainerAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TContainerAPI.APIbasePath : string;

begin
  Result:='/container/v1beta1/projects/';
end;

Class Function TContainerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/container/v1beta1/projects/';
end;

Class Function TContainerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TContainerAPI.APIservicePath : string;

begin
  Result:='container/v1beta1/projects/';
end;

Class Function TContainerAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TContainerAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  
end;

Class Function TContainerAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TContainerAPI.RegisterAPIResources;

begin
  TCluster.RegisterObject;
  TCreateClusterRequest.RegisterObject;
  TListAggregatedClustersResponse.RegisterObject;
  TListAggregatedOperationsResponse.RegisterObject;
  TListClustersResponse.RegisterObject;
  TListOperationsResponse.RegisterObject;
  TMasterAuth.RegisterObject;
  TNodeConfig.RegisterObject;
  TOperation.RegisterObject;
  TServiceAccount.RegisterObject;
end;


Function TContainerAPI.GetProjectsClustersInstance : TProjectsClustersResource;

begin
  if (FProjectsClustersInstance=Nil) then
    FProjectsClustersInstance:=CreateProjectsClustersResource;
  Result:=FProjectsClustersInstance;
end;

Function TContainerAPI.CreateProjectsClustersResource : TProjectsClustersResource;

begin
  Result:=CreateProjectsClustersResource(Self);
end;


Function TContainerAPI.CreateProjectsClustersResource(AOwner : TComponent) : TProjectsClustersResource;

begin
  Result:=TProjectsClustersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TContainerAPI.GetProjectsOperationsInstance : TProjectsOperationsResource;

begin
  if (FProjectsOperationsInstance=Nil) then
    FProjectsOperationsInstance:=CreateProjectsOperationsResource;
  Result:=FProjectsOperationsInstance;
end;

Function TContainerAPI.CreateProjectsOperationsResource : TProjectsOperationsResource;

begin
  Result:=CreateProjectsOperationsResource(Self);
end;


Function TContainerAPI.CreateProjectsOperationsResource(AOwner : TComponent) : TProjectsOperationsResource;

begin
  Result:=TProjectsOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TContainerAPI.GetProjectsZonesClustersInstance : TProjectsZonesClustersResource;

begin
  if (FProjectsZonesClustersInstance=Nil) then
    FProjectsZonesClustersInstance:=CreateProjectsZonesClustersResource;
  Result:=FProjectsZonesClustersInstance;
end;

Function TContainerAPI.CreateProjectsZonesClustersResource : TProjectsZonesClustersResource;

begin
  Result:=CreateProjectsZonesClustersResource(Self);
end;


Function TContainerAPI.CreateProjectsZonesClustersResource(AOwner : TComponent) : TProjectsZonesClustersResource;

begin
  Result:=TProjectsZonesClustersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TContainerAPI.GetProjectsZonesOperationsInstance : TProjectsZonesOperationsResource;

begin
  if (FProjectsZonesOperationsInstance=Nil) then
    FProjectsZonesOperationsInstance:=CreateProjectsZonesOperationsResource;
  Result:=FProjectsZonesOperationsInstance;
end;

Function TContainerAPI.CreateProjectsZonesOperationsResource : TProjectsZonesOperationsResource;

begin
  Result:=CreateProjectsZonesOperationsResource(Self);
end;


Function TContainerAPI.CreateProjectsZonesOperationsResource(AOwner : TComponent) : TProjectsZonesOperationsResource;

begin
  Result:=TProjectsZonesOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TContainerAPI.GetProjectsZonesInstance : TProjectsZonesResource;

begin
  if (FProjectsZonesInstance=Nil) then
    FProjectsZonesInstance:=CreateProjectsZonesResource;
  Result:=FProjectsZonesInstance;
end;

Function TContainerAPI.CreateProjectsZonesResource : TProjectsZonesResource;

begin
  Result:=CreateProjectsZonesResource(Self);
end;


Function TContainerAPI.CreateProjectsZonesResource(AOwner : TComponent) : TProjectsZonesResource;

begin
  Result:=TProjectsZonesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TContainerAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TContainerAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TContainerAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TContainerAPI.RegisterAPI;
end.
