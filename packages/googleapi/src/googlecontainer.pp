unit googlecontainer;
{
  This is the file COPYING.FPC, it applies to the Free Pascal Run-Time Library 
  (RTL) and packages (packages) distributed by members of the Free Pascal 
  Development Team.
  
  The source code of the Free Pascal Runtime Libraries and packages are 
  distributed under the Library GNU General Public License 
  (see the file COPYING) with the following modification:
  
  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,
  and to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a module
  which is not derived from or based on this library. If you modify this
  library, you may extend this exception to your version of the library, but you are
  not obligated to do so. If you do not wish to do so, delete this exception
  statement from your version.
  
  If you didn't receive a copy of the file COPYING, contact:
        Free Software Foundation
        675 Mass Ave
        Cambridge, MA  02139
        USA
  
}
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  //
  TCluster = class;
  TClusterArray = Array of TCluster;
  TCreateClusterRequest = class;
  TCreateClusterRequestArray = Array of TCreateClusterRequest;
  TListAggregatedClustersResponse = class;
  TListAggregatedClustersResponseArray = Array of TListAggregatedClustersResponse;
  TListAggregatedClustersResponseclusters = class;
  TListAggregatedClustersResponseclustersArray = Array of TListAggregatedClustersResponseclusters;
  TListAggregatedOperationsResponse = class;
  TListAggregatedOperationsResponseArray = Array of TListAggregatedOperationsResponse;
  TListAggregatedOperationsResponseoperations = class;
  TListAggregatedOperationsResponseoperationsArray = Array of TListAggregatedOperationsResponseoperations;
  TListClustersResponse = class;
  TListClustersResponseArray = Array of TListClustersResponse;
  TListClustersResponseclusters = class;
  TListClustersResponseclustersArray = Array of TListClustersResponseclusters;
  TListOperationsResponse = class;
  TListOperationsResponseArray = Array of TListOperationsResponse;
  TListOperationsResponseoperations = class;
  TListOperationsResponseoperationsArray = Array of TListOperationsResponseoperations;
  TMasterAuth = class;
  TMasterAuthArray = Array of TMasterAuth;
  TNodeConfig = class;
  TNodeConfigArray = Array of TNodeConfig;
  TNodeConfigserviceAccounts = class;
  TNodeConfigserviceAccountsArray = Array of TNodeConfigserviceAccounts;
  TOperation = class;
  TOperationArray = Array of TOperation;
  TServiceAccount = class;
  TServiceAccountArray = Array of TServiceAccount;
  TServiceAccountscopes = class;
  TServiceAccountscopesArray = Array of TServiceAccountscopes;
  TToken = class;
  TTokenArray = Array of TToken;
  
  { --------------------------------------------------------------------
    TCluster
    --------------------------------------------------------------------}
  
  TCluster = Class(TGoogleBaseObject)
  Private
    FclusterApiVersion : string;
    FcontainerIpv4Cidr : string;
    FcreationTimestamp : string;
    Fdescription : string;
    FenableCloudLogging : boolean;
    Fendpoint : string;
    FmasterAuth : TMasterAuth;
    Fname : string;
    Fnetwork : string;
    FnodeConfig : TNodeConfig;
    FnodeRoutingPrefixSize : integer;
    FnumNodes : integer;
    FselfLink : string;
    FservicesIpv4Cidr : string;
    Fstatus : string;
    FstatusMessage : string;
    Fzone : string;
  Protected
    //Property setters
    Procedure SetclusterApiVersion(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontainerIpv4Cidr(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetenableCloudLogging(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setendpoint(AIndex : Integer; AValue : string); virtual;
    Procedure SetmasterAuth(AIndex : Integer; AValue : TMasterAuth); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnetwork(AIndex : Integer; AValue : string); virtual;
    Procedure SetnodeConfig(AIndex : Integer; AValue : TNodeConfig); virtual;
    Procedure SetnodeRoutingPrefixSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumNodes(AIndex : Integer; AValue : integer); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetservicesIpv4Cidr(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetstatusMessage(AIndex : Integer; AValue : string); virtual;
    Procedure Setzone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property clusterApiVersion : string Index 0 Read FclusterApiVersion Write SetclusterApiVersion;
    Property containerIpv4Cidr : string Index 8 Read FcontainerIpv4Cidr Write SetcontainerIpv4Cidr;
    Property creationTimestamp : string Index 16 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 24 Read Fdescription Write Setdescription;
    Property enableCloudLogging : boolean Index 32 Read FenableCloudLogging Write SetenableCloudLogging;
    Property endpoint : string Index 40 Read Fendpoint Write Setendpoint;
    Property masterAuth : TMasterAuth Index 48 Read FmasterAuth Write SetmasterAuth;
    Property name : string Index 56 Read Fname Write Setname;
    Property network : string Index 64 Read Fnetwork Write Setnetwork;
    Property nodeConfig : TNodeConfig Index 72 Read FnodeConfig Write SetnodeConfig;
    Property nodeRoutingPrefixSize : integer Index 80 Read FnodeRoutingPrefixSize Write SetnodeRoutingPrefixSize;
    Property numNodes : integer Index 88 Read FnumNodes Write SetnumNodes;
    Property selfLink : string Index 96 Read FselfLink Write SetselfLink;
    Property servicesIpv4Cidr : string Index 104 Read FservicesIpv4Cidr Write SetservicesIpv4Cidr;
    Property status : string Index 112 Read Fstatus Write Setstatus;
    Property statusMessage : string Index 120 Read FstatusMessage Write SetstatusMessage;
    Property zone : string Index 128 Read Fzone Write Setzone;
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
    Fclusters : TListAggregatedClustersResponseclusters;
  Protected
    //Property setters
    Procedure Setclusters(AIndex : Integer; AValue : TListAggregatedClustersResponseclusters); virtual;
  Public
  Published
    Property clusters : TListAggregatedClustersResponseclusters Index 0 Read Fclusters Write Setclusters;
  end;
  TListAggregatedClustersResponseClass = Class of TListAggregatedClustersResponse;
  
  { --------------------------------------------------------------------
    TListAggregatedClustersResponseclusters
    --------------------------------------------------------------------}
  
  TListAggregatedClustersResponseclusters = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListAggregatedClustersResponseclustersClass = Class of TListAggregatedClustersResponseclusters;
  
  { --------------------------------------------------------------------
    TListAggregatedOperationsResponse
    --------------------------------------------------------------------}
  
  TListAggregatedOperationsResponse = Class(TGoogleBaseObject)
  Private
    Foperations : TListAggregatedOperationsResponseoperations;
  Protected
    //Property setters
    Procedure Setoperations(AIndex : Integer; AValue : TListAggregatedOperationsResponseoperations); virtual;
  Public
  Published
    Property operations : TListAggregatedOperationsResponseoperations Index 0 Read Foperations Write Setoperations;
  end;
  TListAggregatedOperationsResponseClass = Class of TListAggregatedOperationsResponse;
  
  { --------------------------------------------------------------------
    TListAggregatedOperationsResponseoperations
    --------------------------------------------------------------------}
  
  TListAggregatedOperationsResponseoperations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListAggregatedOperationsResponseoperationsClass = Class of TListAggregatedOperationsResponseoperations;
  
  { --------------------------------------------------------------------
    TListClustersResponse
    --------------------------------------------------------------------}
  
  TListClustersResponse = Class(TGoogleBaseObject)
  Private
    Fclusters : TListClustersResponseclusters;
  Protected
    //Property setters
    Procedure Setclusters(AIndex : Integer; AValue : TListClustersResponseclusters); virtual;
  Public
  Published
    Property clusters : TListClustersResponseclusters Index 0 Read Fclusters Write Setclusters;
  end;
  TListClustersResponseClass = Class of TListClustersResponse;
  
  { --------------------------------------------------------------------
    TListClustersResponseclusters
    --------------------------------------------------------------------}
  
  TListClustersResponseclusters = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListClustersResponseclustersClass = Class of TListClustersResponseclusters;
  
  { --------------------------------------------------------------------
    TListOperationsResponse
    --------------------------------------------------------------------}
  
  TListOperationsResponse = Class(TGoogleBaseObject)
  Private
    Foperations : TListOperationsResponseoperations;
  Protected
    //Property setters
    Procedure Setoperations(AIndex : Integer; AValue : TListOperationsResponseoperations); virtual;
  Public
  Published
    Property operations : TListOperationsResponseoperations Index 0 Read Foperations Write Setoperations;
  end;
  TListOperationsResponseClass = Class of TListOperationsResponse;
  
  { --------------------------------------------------------------------
    TListOperationsResponseoperations
    --------------------------------------------------------------------}
  
  TListOperationsResponseoperations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListOperationsResponseoperationsClass = Class of TListOperationsResponseoperations;
  
  { --------------------------------------------------------------------
    TMasterAuth
    --------------------------------------------------------------------}
  
  TMasterAuth = Class(TGoogleBaseObject)
  Private
    FbearerToken : string;
    Fpassword : string;
    Fuser : string;
  Protected
    //Property setters
    Procedure SetbearerToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setpassword(AIndex : Integer; AValue : string); virtual;
    Procedure Setuser(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property bearerToken : string Index 0 Read FbearerToken Write SetbearerToken;
    Property password : string Index 8 Read Fpassword Write Setpassword;
    Property user : string Index 16 Read Fuser Write Setuser;
  end;
  TMasterAuthClass = Class of TMasterAuth;
  
  { --------------------------------------------------------------------
    TNodeConfig
    --------------------------------------------------------------------}
  
  TNodeConfig = Class(TGoogleBaseObject)
  Private
    FmachineType : string;
    FserviceAccounts : TNodeConfigserviceAccounts;
    FsourceImage : string;
  Protected
    //Property setters
    Procedure SetmachineType(AIndex : Integer; AValue : string); virtual;
    Procedure SetserviceAccounts(AIndex : Integer; AValue : TNodeConfigserviceAccounts); virtual;
    Procedure SetsourceImage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property machineType : string Index 0 Read FmachineType Write SetmachineType;
    Property serviceAccounts : TNodeConfigserviceAccounts Index 8 Read FserviceAccounts Write SetserviceAccounts;
    Property sourceImage : string Index 16 Read FsourceImage Write SetsourceImage;
  end;
  TNodeConfigClass = Class of TNodeConfig;
  
  { --------------------------------------------------------------------
    TNodeConfigserviceAccounts
    --------------------------------------------------------------------}
  
  TNodeConfigserviceAccounts = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TNodeConfigserviceAccountsClass = Class of TNodeConfigserviceAccounts;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    FerrorMessage : string;
    Fname : string;
    FoperationType : string;
    FselfLink : string;
    Fstatus : string;
    Ftarget : string;
    FtargetLink : string;
    Fzone : string;
  Protected
    //Property setters
    Procedure SeterrorMessage(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetoperationType(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure Settarget(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setzone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property errorMessage : string Index 0 Read FerrorMessage Write SeterrorMessage;
    Property name : string Index 8 Read Fname Write Setname;
    Property operationType : string Index 16 Read FoperationType Write SetoperationType;
    Property selfLink : string Index 24 Read FselfLink Write SetselfLink;
    Property status : string Index 32 Read Fstatus Write Setstatus;
    Property target : string Index 40 Read Ftarget Write Settarget;
    Property targetLink : string Index 48 Read FtargetLink Write SettargetLink;
    Property zone : string Index 56 Read Fzone Write Setzone;
  end;
  TOperationClass = Class of TOperation;
  
  { --------------------------------------------------------------------
    TServiceAccount
    --------------------------------------------------------------------}
  
  TServiceAccount = Class(TGoogleBaseObject)
  Private
    Femail : string;
    Fscopes : TServiceAccountscopes;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure Setscopes(AIndex : Integer; AValue : TServiceAccountscopes); virtual;
  Public
  Published
    Property email : string Index 0 Read Femail Write Setemail;
    Property scopes : TServiceAccountscopes Index 8 Read Fscopes Write Setscopes;
  end;
  TServiceAccountClass = Class of TServiceAccount;
  
  { --------------------------------------------------------------------
    TServiceAccountscopes
    --------------------------------------------------------------------}
  
  TServiceAccountscopes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TServiceAccountscopesClass = Class of TServiceAccountscopes;
  
  { --------------------------------------------------------------------
    TToken
    --------------------------------------------------------------------}
  
  TToken = Class(TGoogleBaseObject)
  Private
    FaccessToken : string;
    FexpiryTimeSeconds : string;
  Protected
    //Property setters
    Procedure SetaccessToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetexpiryTimeSeconds(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accessToken : string Index 0 Read FaccessToken Write SetaccessToken;
    Property expiryTimeSeconds : string Index 8 Read FexpiryTimeSeconds Write SetexpiryTimeSeconds;
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


Procedure TCluster.SetclusterApiVersion(AIndex : Integer; AValue : string); 

begin
  If (FclusterApiVersion=AValue) then exit;
  FclusterApiVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetcontainerIpv4Cidr(AIndex : Integer; AValue : string); 

begin
  If (FcontainerIpv4Cidr=AValue) then exit;
  FcontainerIpv4Cidr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setdescription(AIndex : Integer; AValue : string); 

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



Procedure TCluster.Setendpoint(AIndex : Integer; AValue : string); 

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



Procedure TCluster.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setnetwork(AIndex : Integer; AValue : string); 

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



Procedure TCluster.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetservicesIpv4Cidr(AIndex : Integer; AValue : string); 

begin
  If (FservicesIpv4Cidr=AValue) then exit;
  FservicesIpv4Cidr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetstatusMessage(AIndex : Integer; AValue : string); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setzone(AIndex : Integer; AValue : string); 

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


Procedure TListAggregatedClustersResponse.Setclusters(AIndex : Integer; AValue : TListAggregatedClustersResponseclusters); 

begin
  If (Fclusters=AValue) then exit;
  Fclusters:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListAggregatedClustersResponseclusters
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListAggregatedOperationsResponse
  --------------------------------------------------------------------}


Procedure TListAggregatedOperationsResponse.Setoperations(AIndex : Integer; AValue : TListAggregatedOperationsResponseoperations); 

begin
  If (Foperations=AValue) then exit;
  Foperations:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListAggregatedOperationsResponseoperations
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListClustersResponse
  --------------------------------------------------------------------}


Procedure TListClustersResponse.Setclusters(AIndex : Integer; AValue : TListClustersResponseclusters); 

begin
  If (Fclusters=AValue) then exit;
  Fclusters:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListClustersResponseclusters
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListOperationsResponse
  --------------------------------------------------------------------}


Procedure TListOperationsResponse.Setoperations(AIndex : Integer; AValue : TListOperationsResponseoperations); 

begin
  If (Foperations=AValue) then exit;
  Foperations:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListOperationsResponseoperations
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMasterAuth
  --------------------------------------------------------------------}


Procedure TMasterAuth.SetbearerToken(AIndex : Integer; AValue : string); 

begin
  If (FbearerToken=AValue) then exit;
  FbearerToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMasterAuth.Setpassword(AIndex : Integer; AValue : string); 

begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMasterAuth.Setuser(AIndex : Integer; AValue : string); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNodeConfig
  --------------------------------------------------------------------}


Procedure TNodeConfig.SetmachineType(AIndex : Integer; AValue : string); 

begin
  If (FmachineType=AValue) then exit;
  FmachineType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNodeConfig.SetserviceAccounts(AIndex : Integer; AValue : TNodeConfigserviceAccounts); 

begin
  If (FserviceAccounts=AValue) then exit;
  FserviceAccounts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNodeConfig.SetsourceImage(AIndex : Integer; AValue : string); 

begin
  If (FsourceImage=AValue) then exit;
  FsourceImage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNodeConfigserviceAccounts
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.SeterrorMessage(AIndex : Integer; AValue : string); 

begin
  If (FerrorMessage=AValue) then exit;
  FerrorMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetoperationType(AIndex : Integer; AValue : string); 

begin
  If (FoperationType=AValue) then exit;
  FoperationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Settarget(AIndex : Integer; AValue : string); 

begin
  If (Ftarget=AValue) then exit;
  Ftarget:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetLink(AIndex : Integer; AValue : string); 

begin
  If (FtargetLink=AValue) then exit;
  FtargetLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setzone(AIndex : Integer; AValue : string); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TServiceAccount
  --------------------------------------------------------------------}


Procedure TServiceAccount.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServiceAccount.Setscopes(AIndex : Integer; AValue : TServiceAccountscopes); 

begin
  If (Fscopes=AValue) then exit;
  Fscopes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TServiceAccountscopes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TToken
  --------------------------------------------------------------------}


Procedure TToken.SetaccessToken(AIndex : Integer; AValue : string); 

begin
  If (FaccessToken=AValue) then exit;
  FaccessToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TToken.SetexpiryTimeSeconds(AIndex : Integer; AValue : string); 

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
  TListAggregatedClustersResponseclusters.RegisterObject;
  TListAggregatedOperationsResponse.RegisterObject;
  TListAggregatedOperationsResponseoperations.RegisterObject;
  TListClustersResponse.RegisterObject;
  TListClustersResponseclusters.RegisterObject;
  TListOperationsResponse.RegisterObject;
  TListOperationsResponseoperations.RegisterObject;
  TMasterAuth.RegisterObject;
  TNodeConfig.RegisterObject;
  TNodeConfigserviceAccounts.RegisterObject;
  TOperation.RegisterObject;
  TServiceAccount.RegisterObject;
  TServiceAccountscopes.RegisterObject;
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
