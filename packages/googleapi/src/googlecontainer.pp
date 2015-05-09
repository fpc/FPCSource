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
//Generated on: 9-5-15 13:22:51
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TCluster = class;
  TCreateClusterRequest = class;
  TListAggregatedClustersResponse = class;
  TListAggregatedOperationsResponse = class;
  TListClustersResponse = class;
  TListOperationsResponse = class;
  TMasterAuth = class;
  TNodeConfig = class;
  TOperation = class;
  TServiceAccount = class;
  TToken = class;
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
  TTokenArray = Array of TToken;
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
  Public
  Published
    Property email : String Index 0 Read Femail Write Setemail;
    Property scopes : TStringArray Index 8 Read Fscopes Write Setscopes;
  end;
  TServiceAccountClass = Class of TServiceAccount;
  
  { --------------------------------------------------------------------
    TToken
    --------------------------------------------------------------------}
  
  TToken = Class(TGoogleBaseObject)
  Private
    FaccessToken : String;
    FexpiryTimeSeconds : String;
  Protected
    //Property setters
    Procedure SetaccessToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetexpiryTimeSeconds(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property accessToken : String Index 0 Read FaccessToken Write SetaccessToken;
    Property expiryTimeSeconds : String Index 8 Read FexpiryTimeSeconds Write SetexpiryTimeSeconds;
  end;
  TTokenClass = Class of TToken;
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
  end;
  
  
  { --------------------------------------------------------------------
    TContainerAPI
    --------------------------------------------------------------------}
  
  TContainerAPI = Class(TGoogleAPI)
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





{ --------------------------------------------------------------------
  TListAggregatedOperationsResponse
  --------------------------------------------------------------------}


Procedure TListAggregatedOperationsResponse.Setoperations(AIndex : Integer; AValue : TListAggregatedOperationsResponseTypeoperationsArray); 

begin
  If (Foperations=AValue) then exit;
  Foperations:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListClustersResponse
  --------------------------------------------------------------------}


Procedure TListClustersResponse.Setclusters(AIndex : Integer; AValue : TListClustersResponseTypeclustersArray); 

begin
  If (Fclusters=AValue) then exit;
  Fclusters:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListOperationsResponse
  --------------------------------------------------------------------}


Procedure TListOperationsResponse.Setoperations(AIndex : Integer; AValue : TListOperationsResponseTypeoperationsArray); 

begin
  If (Foperations=AValue) then exit;
  Foperations:=AValue;
  MarkPropertyChanged(AIndex);
end;





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





{ --------------------------------------------------------------------
  TToken
  --------------------------------------------------------------------}


Procedure TToken.SetaccessToken(AIndex : Integer; AValue : String); 

begin
  If (FaccessToken=AValue) then exit;
  FaccessToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TToken.SetexpiryTimeSeconds(AIndex : Integer; AValue : String); 

begin
  If (FexpiryTimeSeconds=AValue) then exit;
  FexpiryTimeSeconds:=AValue;
  MarkPropertyChanged(AIndex);
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
  Result:='20150420';
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
  Result:='https://www.googleapis.com/';
end;

Class Function TContainerAPI.APIbasePath : string;

begin
  Result:='/container/v1beta1/projects/';
end;

Class Function TContainerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/container/v1beta1/projects/';
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
  TToken.RegisterObject;
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
  Result.API:=Self;
end;



initialization
  TContainerAPI.RegisterAPI;
end.
