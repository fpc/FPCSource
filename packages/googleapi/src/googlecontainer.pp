unit googlecontainer;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TListClustersResponse = Class;
  TCluster = Class;
  TNodeConfig = Class;
  TMasterAuth = Class;
  TAddonsConfig = Class;
  THttpLoadBalancing = Class;
  THorizontalPodAutoscaling = Class;
  TNodePool = Class;
  TCreateClusterRequest = Class;
  TOperation = Class;
  TUpdateClusterRequest = Class;
  TClusterUpdate = Class;
  TListOperationsResponse = Class;
  TServerConfig = Class;
  TListNodePoolsResponse = Class;
  TCreateNodePoolRequest = Class;
  TListClustersResponseArray = Array of TListClustersResponse;
  TClusterArray = Array of TCluster;
  TNodeConfigArray = Array of TNodeConfig;
  TMasterAuthArray = Array of TMasterAuth;
  TAddonsConfigArray = Array of TAddonsConfig;
  THttpLoadBalancingArray = Array of THttpLoadBalancing;
  THorizontalPodAutoscalingArray = Array of THorizontalPodAutoscaling;
  TNodePoolArray = Array of TNodePool;
  TCreateClusterRequestArray = Array of TCreateClusterRequest;
  TOperationArray = Array of TOperation;
  TUpdateClusterRequestArray = Array of TUpdateClusterRequest;
  TClusterUpdateArray = Array of TClusterUpdate;
  TListOperationsResponseArray = Array of TListOperationsResponse;
  TServerConfigArray = Array of TServerConfig;
  TListNodePoolsResponseArray = Array of TListNodePoolsResponse;
  TCreateNodePoolRequestArray = Array of TCreateNodePoolRequest;
  //Anonymous types, using auto-generated names
  TNodeConfigTypemetadata = Class;
  TListClustersResponseTypeclustersArray = Array of TCluster;
  TClusterTypenodePoolsArray = Array of TNodePool;
  TListOperationsResponseTypeoperationsArray = Array of TOperation;
  TListNodePoolsResponseTypenodePoolsArray = Array of TNodePool;
  
  { --------------------------------------------------------------------
    TListClustersResponse
    --------------------------------------------------------------------}
  
  TListClustersResponse = Class(TGoogleBaseObject)
  Private
    Fclusters : TListClustersResponseTypeclustersArray;
    FmissingZones : TStringArray;
  Protected
    //Property setters
    Procedure Setclusters(AIndex : Integer; const AValue : TListClustersResponseTypeclustersArray); virtual;
    Procedure SetmissingZones(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property clusters : TListClustersResponseTypeclustersArray Index 0 Read Fclusters Write Setclusters;
    Property missingZones : TStringArray Index 8 Read FmissingZones Write SetmissingZones;
  end;
  TListClustersResponseClass = Class of TListClustersResponse;
  
  { --------------------------------------------------------------------
    TCluster
    --------------------------------------------------------------------}
  
  TCluster = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fdescription : String;
    FinitialNodeCount : integer;
    FnodeConfig : TNodeConfig;
    FmasterAuth : TMasterAuth;
    FloggingService : String;
    FmonitoringService : String;
    Fnetwork : String;
    FclusterIpv4Cidr : String;
    FaddonsConfig : TAddonsConfig;
    Fsubnetwork : String;
    FnodePools : TClusterTypenodePoolsArray;
    Flocations : TStringArray;
    FselfLink : String;
    Fzone : String;
    Fendpoint : String;
    FinitialClusterVersion : String;
    FcurrentMasterVersion : String;
    FcurrentNodeVersion : String;
    FcreateTime : String;
    Fstatus : String;
    FstatusMessage : String;
    FnodeIpv4CidrSize : integer;
    FservicesIpv4Cidr : String;
    FinstanceGroupUrls : TStringArray;
    FcurrentNodeCount : integer;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinitialNodeCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetnodeConfig(AIndex : Integer; const AValue : TNodeConfig); virtual;
    Procedure SetmasterAuth(AIndex : Integer; const AValue : TMasterAuth); virtual;
    Procedure SetloggingService(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmonitoringService(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnetwork(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclusterIpv4Cidr(AIndex : Integer; const AValue : String); virtual;
    Procedure SetaddonsConfig(AIndex : Integer; const AValue : TAddonsConfig); virtual;
    Procedure Setsubnetwork(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnodePools(AIndex : Integer; const AValue : TClusterTypenodePoolsArray); virtual;
    Procedure Setlocations(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setzone(AIndex : Integer; const AValue : String); virtual;
    Procedure Setendpoint(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinitialClusterVersion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcurrentMasterVersion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcurrentNodeVersion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstatusMessage(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnodeIpv4CidrSize(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetservicesIpv4Cidr(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinstanceGroupUrls(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetcurrentNodeCount(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property initialNodeCount : integer Index 16 Read FinitialNodeCount Write SetinitialNodeCount;
    Property nodeConfig : TNodeConfig Index 24 Read FnodeConfig Write SetnodeConfig;
    Property masterAuth : TMasterAuth Index 32 Read FmasterAuth Write SetmasterAuth;
    Property loggingService : String Index 40 Read FloggingService Write SetloggingService;
    Property monitoringService : String Index 48 Read FmonitoringService Write SetmonitoringService;
    Property network : String Index 56 Read Fnetwork Write Setnetwork;
    Property clusterIpv4Cidr : String Index 64 Read FclusterIpv4Cidr Write SetclusterIpv4Cidr;
    Property addonsConfig : TAddonsConfig Index 72 Read FaddonsConfig Write SetaddonsConfig;
    Property subnetwork : String Index 80 Read Fsubnetwork Write Setsubnetwork;
    Property nodePools : TClusterTypenodePoolsArray Index 88 Read FnodePools Write SetnodePools;
    Property locations : TStringArray Index 96 Read Flocations Write Setlocations;
    Property selfLink : String Index 104 Read FselfLink Write SetselfLink;
    Property zone : String Index 112 Read Fzone Write Setzone;
    Property endpoint : String Index 120 Read Fendpoint Write Setendpoint;
    Property initialClusterVersion : String Index 128 Read FinitialClusterVersion Write SetinitialClusterVersion;
    Property currentMasterVersion : String Index 136 Read FcurrentMasterVersion Write SetcurrentMasterVersion;
    Property currentNodeVersion : String Index 144 Read FcurrentNodeVersion Write SetcurrentNodeVersion;
    Property createTime : String Index 152 Read FcreateTime Write SetcreateTime;
    Property status : String Index 160 Read Fstatus Write Setstatus;
    Property statusMessage : String Index 168 Read FstatusMessage Write SetstatusMessage;
    Property nodeIpv4CidrSize : integer Index 176 Read FnodeIpv4CidrSize Write SetnodeIpv4CidrSize;
    Property servicesIpv4Cidr : String Index 184 Read FservicesIpv4Cidr Write SetservicesIpv4Cidr;
    Property instanceGroupUrls : TStringArray Index 192 Read FinstanceGroupUrls Write SetinstanceGroupUrls;
    Property currentNodeCount : integer Index 200 Read FcurrentNodeCount Write SetcurrentNodeCount;
  end;
  TClusterClass = Class of TCluster;
  
  { --------------------------------------------------------------------
    TNodeConfigTypemetadata
    --------------------------------------------------------------------}
  
  TNodeConfigTypemetadata = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TNodeConfigTypemetadataClass = Class of TNodeConfigTypemetadata;
  
  { --------------------------------------------------------------------
    TNodeConfig
    --------------------------------------------------------------------}
  
  TNodeConfig = Class(TGoogleBaseObject)
  Private
    FmachineType : String;
    FdiskSizeGb : integer;
    FoauthScopes : TStringArray;
    Fmetadata : TNodeConfigTypemetadata;
  Protected
    //Property setters
    Procedure SetmachineType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdiskSizeGb(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetoauthScopes(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setmetadata(AIndex : Integer; const AValue : TNodeConfigTypemetadata); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property machineType : String Index 0 Read FmachineType Write SetmachineType;
    Property diskSizeGb : integer Index 8 Read FdiskSizeGb Write SetdiskSizeGb;
    Property oauthScopes : TStringArray Index 16 Read FoauthScopes Write SetoauthScopes;
    Property metadata : TNodeConfigTypemetadata Index 24 Read Fmetadata Write Setmetadata;
  end;
  TNodeConfigClass = Class of TNodeConfig;
  
  { --------------------------------------------------------------------
    TMasterAuth
    --------------------------------------------------------------------}
  
  TMasterAuth = Class(TGoogleBaseObject)
  Private
    Fusername : String;
    Fpassword : String;
    FclusterCaCertificate : String;
    FclientCertificate : String;
    FclientKey : String;
  Protected
    //Property setters
    Procedure Setusername(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpassword(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclusterCaCertificate(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclientCertificate(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclientKey(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property username : String Index 0 Read Fusername Write Setusername;
    Property password : String Index 8 Read Fpassword Write Setpassword;
    Property clusterCaCertificate : String Index 16 Read FclusterCaCertificate Write SetclusterCaCertificate;
    Property clientCertificate : String Index 24 Read FclientCertificate Write SetclientCertificate;
    Property clientKey : String Index 32 Read FclientKey Write SetclientKey;
  end;
  TMasterAuthClass = Class of TMasterAuth;
  
  { --------------------------------------------------------------------
    TAddonsConfig
    --------------------------------------------------------------------}
  
  TAddonsConfig = Class(TGoogleBaseObject)
  Private
    FhttpLoadBalancing : THttpLoadBalancing;
    FhorizontalPodAutoscaling : THorizontalPodAutoscaling;
  Protected
    //Property setters
    Procedure SethttpLoadBalancing(AIndex : Integer; const AValue : THttpLoadBalancing); virtual;
    Procedure SethorizontalPodAutoscaling(AIndex : Integer; const AValue : THorizontalPodAutoscaling); virtual;
  Public
  Published
    Property httpLoadBalancing : THttpLoadBalancing Index 0 Read FhttpLoadBalancing Write SethttpLoadBalancing;
    Property horizontalPodAutoscaling : THorizontalPodAutoscaling Index 8 Read FhorizontalPodAutoscaling Write SethorizontalPodAutoscaling;
  end;
  TAddonsConfigClass = Class of TAddonsConfig;
  
  { --------------------------------------------------------------------
    THttpLoadBalancing
    --------------------------------------------------------------------}
  
  THttpLoadBalancing = Class(TGoogleBaseObject)
  Private
    Fdisabled : boolean;
  Protected
    //Property setters
    Procedure Setdisabled(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property disabled : boolean Index 0 Read Fdisabled Write Setdisabled;
  end;
  THttpLoadBalancingClass = Class of THttpLoadBalancing;
  
  { --------------------------------------------------------------------
    THorizontalPodAutoscaling
    --------------------------------------------------------------------}
  
  THorizontalPodAutoscaling = Class(TGoogleBaseObject)
  Private
    Fdisabled : boolean;
  Protected
    //Property setters
    Procedure Setdisabled(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property disabled : boolean Index 0 Read Fdisabled Write Setdisabled;
  end;
  THorizontalPodAutoscalingClass = Class of THorizontalPodAutoscaling;
  
  { --------------------------------------------------------------------
    TNodePool
    --------------------------------------------------------------------}
  
  TNodePool = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fconfig : TNodeConfig;
    FinitialNodeCount : integer;
    FselfLink : String;
    Fversion : String;
    FinstanceGroupUrls : TStringArray;
    Fstatus : String;
    FstatusMessage : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setconfig(AIndex : Integer; const AValue : TNodeConfig); virtual;
    Procedure SetinitialNodeCount(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinstanceGroupUrls(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstatusMessage(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property config : TNodeConfig Index 8 Read Fconfig Write Setconfig;
    Property initialNodeCount : integer Index 16 Read FinitialNodeCount Write SetinitialNodeCount;
    Property selfLink : String Index 24 Read FselfLink Write SetselfLink;
    Property version : String Index 32 Read Fversion Write Setversion;
    Property instanceGroupUrls : TStringArray Index 40 Read FinstanceGroupUrls Write SetinstanceGroupUrls;
    Property status : String Index 48 Read Fstatus Write Setstatus;
    Property statusMessage : String Index 56 Read FstatusMessage Write SetstatusMessage;
  end;
  TNodePoolClass = Class of TNodePool;
  
  { --------------------------------------------------------------------
    TCreateClusterRequest
    --------------------------------------------------------------------}
  
  TCreateClusterRequest = Class(TGoogleBaseObject)
  Private
    Fcluster : TCluster;
  Protected
    //Property setters
    Procedure Setcluster(AIndex : Integer; const AValue : TCluster); virtual;
  Public
  Published
    Property cluster : TCluster Index 0 Read Fcluster Write Setcluster;
  end;
  TCreateClusterRequestClass = Class of TCreateClusterRequest;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fzone : String;
    FoperationType : String;
    Fstatus : String;
    Fdetail : String;
    FstatusMessage : String;
    FselfLink : String;
    FtargetLink : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setzone(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoperationType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdetail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstatusMessage(AIndex : Integer; const AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; const AValue : String); virtual;
    Procedure SettargetLink(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property zone : String Index 8 Read Fzone Write Setzone;
    Property operationType : String Index 16 Read FoperationType Write SetoperationType;
    Property status : String Index 24 Read Fstatus Write Setstatus;
    Property detail : String Index 32 Read Fdetail Write Setdetail;
    Property statusMessage : String Index 40 Read FstatusMessage Write SetstatusMessage;
    Property selfLink : String Index 48 Read FselfLink Write SetselfLink;
    Property targetLink : String Index 56 Read FtargetLink Write SettargetLink;
  end;
  TOperationClass = Class of TOperation;
  
  { --------------------------------------------------------------------
    TUpdateClusterRequest
    --------------------------------------------------------------------}
  
  TUpdateClusterRequest = Class(TGoogleBaseObject)
  Private
    Fupdate : TClusterUpdate;
  Protected
    //Property setters
    Procedure Setupdate(AIndex : Integer; const AValue : TClusterUpdate); virtual;
  Public
  Published
    Property update : TClusterUpdate Index 0 Read Fupdate Write Setupdate;
  end;
  TUpdateClusterRequestClass = Class of TUpdateClusterRequest;
  
  { --------------------------------------------------------------------
    TClusterUpdate
    --------------------------------------------------------------------}
  
  TClusterUpdate = Class(TGoogleBaseObject)
  Private
    FdesiredNodeVersion : String;
    FdesiredMonitoringService : String;
    FdesiredAddonsConfig : TAddonsConfig;
    FdesiredNodePoolId : String;
    FdesiredMasterVersion : String;
  Protected
    //Property setters
    Procedure SetdesiredNodeVersion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdesiredMonitoringService(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdesiredAddonsConfig(AIndex : Integer; const AValue : TAddonsConfig); virtual;
    Procedure SetdesiredNodePoolId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdesiredMasterVersion(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property desiredNodeVersion : String Index 0 Read FdesiredNodeVersion Write SetdesiredNodeVersion;
    Property desiredMonitoringService : String Index 8 Read FdesiredMonitoringService Write SetdesiredMonitoringService;
    Property desiredAddonsConfig : TAddonsConfig Index 16 Read FdesiredAddonsConfig Write SetdesiredAddonsConfig;
    Property desiredNodePoolId : String Index 24 Read FdesiredNodePoolId Write SetdesiredNodePoolId;
    Property desiredMasterVersion : String Index 32 Read FdesiredMasterVersion Write SetdesiredMasterVersion;
  end;
  TClusterUpdateClass = Class of TClusterUpdate;
  
  { --------------------------------------------------------------------
    TListOperationsResponse
    --------------------------------------------------------------------}
  
  TListOperationsResponse = Class(TGoogleBaseObject)
  Private
    Foperations : TListOperationsResponseTypeoperationsArray;
    FmissingZones : TStringArray;
  Protected
    //Property setters
    Procedure Setoperations(AIndex : Integer; const AValue : TListOperationsResponseTypeoperationsArray); virtual;
    Procedure SetmissingZones(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property operations : TListOperationsResponseTypeoperationsArray Index 0 Read Foperations Write Setoperations;
    Property missingZones : TStringArray Index 8 Read FmissingZones Write SetmissingZones;
  end;
  TListOperationsResponseClass = Class of TListOperationsResponse;
  
  { --------------------------------------------------------------------
    TServerConfig
    --------------------------------------------------------------------}
  
  TServerConfig = Class(TGoogleBaseObject)
  Private
    FdefaultClusterVersion : String;
    FvalidNodeVersions : TStringArray;
    FdefaultImageFamily : String;
    FvalidImageFamilies : TStringArray;
  Protected
    //Property setters
    Procedure SetdefaultClusterVersion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvalidNodeVersions(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetdefaultImageFamily(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvalidImageFamilies(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property defaultClusterVersion : String Index 0 Read FdefaultClusterVersion Write SetdefaultClusterVersion;
    Property validNodeVersions : TStringArray Index 8 Read FvalidNodeVersions Write SetvalidNodeVersions;
    Property defaultImageFamily : String Index 16 Read FdefaultImageFamily Write SetdefaultImageFamily;
    Property validImageFamilies : TStringArray Index 24 Read FvalidImageFamilies Write SetvalidImageFamilies;
  end;
  TServerConfigClass = Class of TServerConfig;
  
  { --------------------------------------------------------------------
    TListNodePoolsResponse
    --------------------------------------------------------------------}
  
  TListNodePoolsResponse = Class(TGoogleBaseObject)
  Private
    FnodePools : TListNodePoolsResponseTypenodePoolsArray;
  Protected
    //Property setters
    Procedure SetnodePools(AIndex : Integer; const AValue : TListNodePoolsResponseTypenodePoolsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nodePools : TListNodePoolsResponseTypenodePoolsArray Index 0 Read FnodePools Write SetnodePools;
  end;
  TListNodePoolsResponseClass = Class of TListNodePoolsResponse;
  
  { --------------------------------------------------------------------
    TCreateNodePoolRequest
    --------------------------------------------------------------------}
  
  TCreateNodePoolRequest = Class(TGoogleBaseObject)
  Private
    FnodePool : TNodePool;
  Protected
    //Property setters
    Procedure SetnodePool(AIndex : Integer; const AValue : TNodePool); virtual;
  Public
  Published
    Property nodePool : TNodePool Index 0 Read FnodePool Write SetnodePool;
  end;
  TCreateNodePoolRequestClass = Class of TCreateNodePoolRequest;
  
  { --------------------------------------------------------------------
    TProjectsZonesClustersNodePoolsResource
    --------------------------------------------------------------------}
  
  TProjectsZonesClustersNodePoolsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(projectId: string; zone: string; clusterId: string) : TListNodePoolsResponse;
    Function Get(projectId: string; zone: string; clusterId: string; nodePoolId: string) : TNodePool;
    Function Create(projectId: string; zone: string; clusterId: string; aCreateNodePoolRequest : TCreateNodePoolRequest) : TOperation;overload;
    Function Delete(projectId: string; zone: string; clusterId: string; nodePoolId: string) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsZonesClustersResource
    --------------------------------------------------------------------}
  
  TProjectsZonesClustersResource = Class(TGoogleResource)
  Private
    FNodePoolsInstance : TProjectsZonesClustersNodePoolsResource;
    Function GetNodePoolsInstance : TProjectsZonesClustersNodePoolsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(projectId: string; zone: string) : TListClustersResponse;
    Function Get(projectId: string; zone: string; clusterId: string) : TCluster;
    Function Create(projectId: string; zone: string; aCreateClusterRequest : TCreateClusterRequest) : TOperation;overload;
    Function Update(projectId: string; zone: string; clusterId: string; aUpdateClusterRequest : TUpdateClusterRequest) : TOperation;
    Function Delete(projectId: string; zone: string; clusterId: string) : TOperation;
    Function CreateNodePoolsResource(AOwner : TComponent) : TProjectsZonesClustersNodePoolsResource;virtual;overload;
    Function CreateNodePoolsResource : TProjectsZonesClustersNodePoolsResource;virtual;overload;
    Property NodePoolsResource : TProjectsZonesClustersNodePoolsResource Read GetNodePoolsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsZonesOperationsResource
    --------------------------------------------------------------------}
  
  TProjectsZonesOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(projectId: string; zone: string) : TListOperationsResponse;
    Function Get(projectId: string; zone: string; operationId: string) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsZonesResource
    --------------------------------------------------------------------}
  
  TProjectsZonesResource = Class(TGoogleResource)
  Private
    FClustersNodePoolsInstance : TProjectsZonesClustersNodePoolsResource;
    FClustersInstance : TProjectsZonesClustersResource;
    FOperationsInstance : TProjectsZonesOperationsResource;
    Function GetClustersNodePoolsInstance : TProjectsZonesClustersNodePoolsResource;virtual;
    Function GetClustersInstance : TProjectsZonesClustersResource;virtual;
    Function GetOperationsInstance : TProjectsZonesOperationsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function GetServerconfig(projectId: string; zone: string) : TServerConfig;
    Function CreateClustersNodePoolsResource(AOwner : TComponent) : TProjectsZonesClustersNodePoolsResource;virtual;overload;
    Function CreateClustersNodePoolsResource : TProjectsZonesClustersNodePoolsResource;virtual;overload;
    Function CreateClustersResource(AOwner : TComponent) : TProjectsZonesClustersResource;virtual;overload;
    Function CreateClustersResource : TProjectsZonesClustersResource;virtual;overload;
    Function CreateOperationsResource(AOwner : TComponent) : TProjectsZonesOperationsResource;virtual;overload;
    Function CreateOperationsResource : TProjectsZonesOperationsResource;virtual;overload;
    Property ClustersNodePoolsResource : TProjectsZonesClustersNodePoolsResource Read GetClustersNodePoolsInstance;
    Property ClustersResource : TProjectsZonesClustersResource Read GetClustersInstance;
    Property OperationsResource : TProjectsZonesOperationsResource Read GetOperationsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Private
    FZonesClustersNodePoolsInstance : TProjectsZonesClustersNodePoolsResource;
    FZonesClustersInstance : TProjectsZonesClustersResource;
    FZonesOperationsInstance : TProjectsZonesOperationsResource;
    FZonesInstance : TProjectsZonesResource;
    Function GetZonesClustersNodePoolsInstance : TProjectsZonesClustersNodePoolsResource;virtual;
    Function GetZonesClustersInstance : TProjectsZonesClustersResource;virtual;
    Function GetZonesOperationsInstance : TProjectsZonesOperationsResource;virtual;
    Function GetZonesInstance : TProjectsZonesResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateZonesClustersNodePoolsResource(AOwner : TComponent) : TProjectsZonesClustersNodePoolsResource;virtual;overload;
    Function CreateZonesClustersNodePoolsResource : TProjectsZonesClustersNodePoolsResource;virtual;overload;
    Function CreateZonesClustersResource(AOwner : TComponent) : TProjectsZonesClustersResource;virtual;overload;
    Function CreateZonesClustersResource : TProjectsZonesClustersResource;virtual;overload;
    Function CreateZonesOperationsResource(AOwner : TComponent) : TProjectsZonesOperationsResource;virtual;overload;
    Function CreateZonesOperationsResource : TProjectsZonesOperationsResource;virtual;overload;
    Function CreateZonesResource(AOwner : TComponent) : TProjectsZonesResource;virtual;overload;
    Function CreateZonesResource : TProjectsZonesResource;virtual;overload;
    Property ZonesClustersNodePoolsResource : TProjectsZonesClustersNodePoolsResource Read GetZonesClustersNodePoolsInstance;
    Property ZonesClustersResource : TProjectsZonesClustersResource Read GetZonesClustersInstance;
    Property ZonesOperationsResource : TProjectsZonesOperationsResource Read GetZonesOperationsInstance;
    Property ZonesResource : TProjectsZonesResource Read GetZonesInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TContainerAPI
    --------------------------------------------------------------------}
  
  TContainerAPI = Class(TGoogleAPI)
  Private
    FProjectsZonesClustersNodePoolsInstance : TProjectsZonesClustersNodePoolsResource;
    FProjectsZonesClustersInstance : TProjectsZonesClustersResource;
    FProjectsZonesOperationsInstance : TProjectsZonesOperationsResource;
    FProjectsZonesInstance : TProjectsZonesResource;
    FProjectsInstance : TProjectsResource;
    Function GetProjectsZonesClustersNodePoolsInstance : TProjectsZonesClustersNodePoolsResource;virtual;
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
    Function CreateProjectsZonesClustersNodePoolsResource(AOwner : TComponent) : TProjectsZonesClustersNodePoolsResource;virtual;overload;
    Function CreateProjectsZonesClustersNodePoolsResource : TProjectsZonesClustersNodePoolsResource;virtual;overload;
    Function CreateProjectsZonesClustersResource(AOwner : TComponent) : TProjectsZonesClustersResource;virtual;overload;
    Function CreateProjectsZonesClustersResource : TProjectsZonesClustersResource;virtual;overload;
    Function CreateProjectsZonesOperationsResource(AOwner : TComponent) : TProjectsZonesOperationsResource;virtual;overload;
    Function CreateProjectsZonesOperationsResource : TProjectsZonesOperationsResource;virtual;overload;
    Function CreateProjectsZonesResource(AOwner : TComponent) : TProjectsZonesResource;virtual;overload;
    Function CreateProjectsZonesResource : TProjectsZonesResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsZonesClustersNodePoolsResource : TProjectsZonesClustersNodePoolsResource Read GetProjectsZonesClustersNodePoolsInstance;
    Property ProjectsZonesClustersResource : TProjectsZonesClustersResource Read GetProjectsZonesClustersInstance;
    Property ProjectsZonesOperationsResource : TProjectsZonesOperationsResource Read GetProjectsZonesOperationsInstance;
    Property ProjectsZonesResource : TProjectsZonesResource Read GetProjectsZonesInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TListClustersResponse
  --------------------------------------------------------------------}


Procedure TListClustersResponse.Setclusters(AIndex : Integer; const AValue : TListClustersResponseTypeclustersArray); 

begin
  If (Fclusters=AValue) then exit;
  Fclusters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListClustersResponse.SetmissingZones(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FmissingZones=AValue) then exit;
  FmissingZones:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListClustersResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'clusters' : SetLength(Fclusters,ALength);
  'missingzones' : SetLength(FmissingZones,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCluster
  --------------------------------------------------------------------}


Procedure TCluster.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetinitialNodeCount(AIndex : Integer; const AValue : integer); 

begin
  If (FinitialNodeCount=AValue) then exit;
  FinitialNodeCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetnodeConfig(AIndex : Integer; const AValue : TNodeConfig); 

begin
  If (FnodeConfig=AValue) then exit;
  FnodeConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetmasterAuth(AIndex : Integer; const AValue : TMasterAuth); 

begin
  If (FmasterAuth=AValue) then exit;
  FmasterAuth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetloggingService(AIndex : Integer; const AValue : String); 

begin
  If (FloggingService=AValue) then exit;
  FloggingService:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetmonitoringService(AIndex : Integer; const AValue : String); 

begin
  If (FmonitoringService=AValue) then exit;
  FmonitoringService:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setnetwork(AIndex : Integer; const AValue : String); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetclusterIpv4Cidr(AIndex : Integer; const AValue : String); 

begin
  If (FclusterIpv4Cidr=AValue) then exit;
  FclusterIpv4Cidr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetaddonsConfig(AIndex : Integer; const AValue : TAddonsConfig); 

begin
  If (FaddonsConfig=AValue) then exit;
  FaddonsConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setsubnetwork(AIndex : Integer; const AValue : String); 

begin
  If (Fsubnetwork=AValue) then exit;
  Fsubnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetnodePools(AIndex : Integer; const AValue : TClusterTypenodePoolsArray); 

begin
  If (FnodePools=AValue) then exit;
  FnodePools:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setlocations(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Flocations=AValue) then exit;
  Flocations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setzone(AIndex : Integer; const AValue : String); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setendpoint(AIndex : Integer; const AValue : String); 

begin
  If (Fendpoint=AValue) then exit;
  Fendpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetinitialClusterVersion(AIndex : Integer; const AValue : String); 

begin
  If (FinitialClusterVersion=AValue) then exit;
  FinitialClusterVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetcurrentMasterVersion(AIndex : Integer; const AValue : String); 

begin
  If (FcurrentMasterVersion=AValue) then exit;
  FcurrentMasterVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetcurrentNodeVersion(AIndex : Integer; const AValue : String); 

begin
  If (FcurrentNodeVersion=AValue) then exit;
  FcurrentNodeVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetcreateTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreateTime=AValue) then exit;
  FcreateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setstatus(AIndex : Integer; const AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetstatusMessage(AIndex : Integer; const AValue : String); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetnodeIpv4CidrSize(AIndex : Integer; const AValue : integer); 

begin
  If (FnodeIpv4CidrSize=AValue) then exit;
  FnodeIpv4CidrSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetservicesIpv4Cidr(AIndex : Integer; const AValue : String); 

begin
  If (FservicesIpv4Cidr=AValue) then exit;
  FservicesIpv4Cidr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetinstanceGroupUrls(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FinstanceGroupUrls=AValue) then exit;
  FinstanceGroupUrls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetcurrentNodeCount(AIndex : Integer; const AValue : integer); 

begin
  If (FcurrentNodeCount=AValue) then exit;
  FcurrentNodeCount:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCluster.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'nodepools' : SetLength(FnodePools,ALength);
  'locations' : SetLength(Flocations,ALength);
  'instancegroupurls' : SetLength(FinstanceGroupUrls,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TNodeConfigTypemetadata
  --------------------------------------------------------------------}


Class Function TNodeConfigTypemetadata.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TNodeConfig
  --------------------------------------------------------------------}


Procedure TNodeConfig.SetmachineType(AIndex : Integer; const AValue : String); 

begin
  If (FmachineType=AValue) then exit;
  FmachineType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNodeConfig.SetdiskSizeGb(AIndex : Integer; const AValue : integer); 

begin
  If (FdiskSizeGb=AValue) then exit;
  FdiskSizeGb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNodeConfig.SetoauthScopes(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FoauthScopes=AValue) then exit;
  FoauthScopes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNodeConfig.Setmetadata(AIndex : Integer; const AValue : TNodeConfigTypemetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TNodeConfig.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'oauthscopes' : SetLength(FoauthScopes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMasterAuth
  --------------------------------------------------------------------}


Procedure TMasterAuth.Setusername(AIndex : Integer; const AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMasterAuth.Setpassword(AIndex : Integer; const AValue : String); 

begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMasterAuth.SetclusterCaCertificate(AIndex : Integer; const AValue : String); 

begin
  If (FclusterCaCertificate=AValue) then exit;
  FclusterCaCertificate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMasterAuth.SetclientCertificate(AIndex : Integer; const AValue : String); 

begin
  If (FclientCertificate=AValue) then exit;
  FclientCertificate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMasterAuth.SetclientKey(AIndex : Integer; const AValue : String); 

begin
  If (FclientKey=AValue) then exit;
  FclientKey:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAddonsConfig
  --------------------------------------------------------------------}


Procedure TAddonsConfig.SethttpLoadBalancing(AIndex : Integer; const AValue : THttpLoadBalancing); 

begin
  If (FhttpLoadBalancing=AValue) then exit;
  FhttpLoadBalancing:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddonsConfig.SethorizontalPodAutoscaling(AIndex : Integer; const AValue : THorizontalPodAutoscaling); 

begin
  If (FhorizontalPodAutoscaling=AValue) then exit;
  FhorizontalPodAutoscaling:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THttpLoadBalancing
  --------------------------------------------------------------------}


Procedure THttpLoadBalancing.Setdisabled(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdisabled=AValue) then exit;
  Fdisabled:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THorizontalPodAutoscaling
  --------------------------------------------------------------------}


Procedure THorizontalPodAutoscaling.Setdisabled(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdisabled=AValue) then exit;
  Fdisabled:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNodePool
  --------------------------------------------------------------------}


Procedure TNodePool.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNodePool.Setconfig(AIndex : Integer; const AValue : TNodeConfig); 

begin
  If (Fconfig=AValue) then exit;
  Fconfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNodePool.SetinitialNodeCount(AIndex : Integer; const AValue : integer); 

begin
  If (FinitialNodeCount=AValue) then exit;
  FinitialNodeCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNodePool.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNodePool.Setversion(AIndex : Integer; const AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNodePool.SetinstanceGroupUrls(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FinstanceGroupUrls=AValue) then exit;
  FinstanceGroupUrls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNodePool.Setstatus(AIndex : Integer; const AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNodePool.SetstatusMessage(AIndex : Integer; const AValue : String); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TNodePool.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'instancegroupurls' : SetLength(FinstanceGroupUrls,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCreateClusterRequest
  --------------------------------------------------------------------}


Procedure TCreateClusterRequest.Setcluster(AIndex : Integer; const AValue : TCluster); 

begin
  If (Fcluster=AValue) then exit;
  Fcluster:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setzone(AIndex : Integer; const AValue : String); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetoperationType(AIndex : Integer; const AValue : String); 

begin
  If (FoperationType=AValue) then exit;
  FoperationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setstatus(AIndex : Integer; const AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setdetail(AIndex : Integer; const AValue : String); 

begin
  If (Fdetail=AValue) then exit;
  Fdetail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstatusMessage(AIndex : Integer; const AValue : String); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetselfLink(AIndex : Integer; const AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetLink(AIndex : Integer; const AValue : String); 

begin
  If (FtargetLink=AValue) then exit;
  FtargetLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUpdateClusterRequest
  --------------------------------------------------------------------}


Procedure TUpdateClusterRequest.Setupdate(AIndex : Integer; const AValue : TClusterUpdate); 

begin
  If (Fupdate=AValue) then exit;
  Fupdate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TClusterUpdate
  --------------------------------------------------------------------}


Procedure TClusterUpdate.SetdesiredNodeVersion(AIndex : Integer; const AValue : String); 

begin
  If (FdesiredNodeVersion=AValue) then exit;
  FdesiredNodeVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterUpdate.SetdesiredMonitoringService(AIndex : Integer; const AValue : String); 

begin
  If (FdesiredMonitoringService=AValue) then exit;
  FdesiredMonitoringService:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterUpdate.SetdesiredAddonsConfig(AIndex : Integer; const AValue : TAddonsConfig); 

begin
  If (FdesiredAddonsConfig=AValue) then exit;
  FdesiredAddonsConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterUpdate.SetdesiredNodePoolId(AIndex : Integer; const AValue : String); 

begin
  If (FdesiredNodePoolId=AValue) then exit;
  FdesiredNodePoolId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterUpdate.SetdesiredMasterVersion(AIndex : Integer; const AValue : String); 

begin
  If (FdesiredMasterVersion=AValue) then exit;
  FdesiredMasterVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListOperationsResponse
  --------------------------------------------------------------------}


Procedure TListOperationsResponse.Setoperations(AIndex : Integer; const AValue : TListOperationsResponseTypeoperationsArray); 

begin
  If (Foperations=AValue) then exit;
  Foperations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListOperationsResponse.SetmissingZones(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FmissingZones=AValue) then exit;
  FmissingZones:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListOperationsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'operations' : SetLength(Foperations,ALength);
  'missingzones' : SetLength(FmissingZones,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TServerConfig
  --------------------------------------------------------------------}


Procedure TServerConfig.SetdefaultClusterVersion(AIndex : Integer; const AValue : String); 

begin
  If (FdefaultClusterVersion=AValue) then exit;
  FdefaultClusterVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServerConfig.SetvalidNodeVersions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FvalidNodeVersions=AValue) then exit;
  FvalidNodeVersions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServerConfig.SetdefaultImageFamily(AIndex : Integer; const AValue : String); 

begin
  If (FdefaultImageFamily=AValue) then exit;
  FdefaultImageFamily:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TServerConfig.SetvalidImageFamilies(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FvalidImageFamilies=AValue) then exit;
  FvalidImageFamilies:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TServerConfig.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'validnodeversions' : SetLength(FvalidNodeVersions,ALength);
  'validimagefamilies' : SetLength(FvalidImageFamilies,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListNodePoolsResponse
  --------------------------------------------------------------------}


Procedure TListNodePoolsResponse.SetnodePools(AIndex : Integer; const AValue : TListNodePoolsResponseTypenodePoolsArray); 

begin
  If (FnodePools=AValue) then exit;
  FnodePools:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListNodePoolsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'nodepools' : SetLength(FnodePools,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCreateNodePoolRequest
  --------------------------------------------------------------------}


Procedure TCreateNodePoolRequest.SetnodePool(AIndex : Integer; const AValue : TNodePool); 

begin
  If (FnodePool=AValue) then exit;
  FnodePool:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectsZonesClustersNodePoolsResource
  --------------------------------------------------------------------}


Class Function TProjectsZonesClustersNodePoolsResource.ResourceName : String;

begin
  Result:='nodePools';
end;

Class Function TProjectsZonesClustersNodePoolsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcontainerAPI;
end;

Function TProjectsZonesClustersNodePoolsResource.List(projectId: string; zone: string; clusterId: string) : TListNodePoolsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects/{projectId}/zones/{zone}/clusters/{clusterId}/nodePools';
  _Methodid   = 'container.projects.zones.clusters.nodePools.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'zone',zone,'clusterId',clusterId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListNodePoolsResponse) as TListNodePoolsResponse;
end;

Function TProjectsZonesClustersNodePoolsResource.Get(projectId: string; zone: string; clusterId: string; nodePoolId: string) : TNodePool;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects/{projectId}/zones/{zone}/clusters/{clusterId}/nodePools/{nodePoolId}';
  _Methodid   = 'container.projects.zones.clusters.nodePools.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'zone',zone,'clusterId',clusterId,'nodePoolId',nodePoolId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TNodePool) as TNodePool;
end;

Function TProjectsZonesClustersNodePoolsResource.Create(projectId: string; zone: string; clusterId: string; aCreateNodePoolRequest : TCreateNodePoolRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/projects/{projectId}/zones/{zone}/clusters/{clusterId}/nodePools';
  _Methodid   = 'container.projects.zones.clusters.nodePools.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'zone',zone,'clusterId',clusterId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreateNodePoolRequest,TOperation) as TOperation;
end;

Function TProjectsZonesClustersNodePoolsResource.Delete(projectId: string; zone: string; clusterId: string; nodePoolId: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/projects/{projectId}/zones/{zone}/clusters/{clusterId}/nodePools/{nodePoolId}';
  _Methodid   = 'container.projects.zones.clusters.nodePools.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'zone',zone,'clusterId',clusterId,'nodePoolId',nodePoolId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
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

Function TProjectsZonesClustersResource.List(projectId: string; zone: string) : TListClustersResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects/{projectId}/zones/{zone}/clusters';
  _Methodid   = 'container.projects.zones.clusters.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListClustersResponse) as TListClustersResponse;
end;

Function TProjectsZonesClustersResource.Get(projectId: string; zone: string; clusterId: string) : TCluster;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects/{projectId}/zones/{zone}/clusters/{clusterId}';
  _Methodid   = 'container.projects.zones.clusters.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'zone',zone,'clusterId',clusterId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCluster) as TCluster;
end;

Function TProjectsZonesClustersResource.Create(projectId: string; zone: string; aCreateClusterRequest : TCreateClusterRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/projects/{projectId}/zones/{zone}/clusters';
  _Methodid   = 'container.projects.zones.clusters.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreateClusterRequest,TOperation) as TOperation;
end;

Function TProjectsZonesClustersResource.Update(projectId: string; zone: string; clusterId: string; aUpdateClusterRequest : TUpdateClusterRequest) : TOperation;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v1/projects/{projectId}/zones/{zone}/clusters/{clusterId}';
  _Methodid   = 'container.projects.zones.clusters.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'zone',zone,'clusterId',clusterId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUpdateClusterRequest,TOperation) as TOperation;
end;

Function TProjectsZonesClustersResource.Delete(projectId: string; zone: string; clusterId: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/projects/{projectId}/zones/{zone}/clusters/{clusterId}';
  _Methodid   = 'container.projects.zones.clusters.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'zone',zone,'clusterId',clusterId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;



Function TProjectsZonesClustersResource.GetNodePoolsInstance : TProjectsZonesClustersNodePoolsResource;

begin
  if (FNodePoolsInstance=Nil) then
    FNodePoolsInstance:=CreateNodePoolsResource;
  Result:=FNodePoolsInstance;
end;

Function TProjectsZonesClustersResource.CreateNodePoolsResource : TProjectsZonesClustersNodePoolsResource;

begin
  Result:=CreateNodePoolsResource(Self);
end;


Function TProjectsZonesClustersResource.CreateNodePoolsResource(AOwner : TComponent) : TProjectsZonesClustersNodePoolsResource;

begin
  Result:=TProjectsZonesClustersNodePoolsResource.Create(AOwner);
  Result.API:=Self.API;
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

Function TProjectsZonesOperationsResource.List(projectId: string; zone: string) : TListOperationsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects/{projectId}/zones/{zone}/operations';
  _Methodid   = 'container.projects.zones.operations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TListOperationsResponse) as TListOperationsResponse;
end;

Function TProjectsZonesOperationsResource.Get(projectId: string; zone: string; operationId: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects/{projectId}/zones/{zone}/operations/{operationId}';
  _Methodid   = 'container.projects.zones.operations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'zone',zone,'operationId',operationId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
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

Function TProjectsZonesResource.GetServerconfig(projectId: string; zone: string) : TServerConfig;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects/{projectId}/zones/{zone}/serverconfig';
  _Methodid   = 'container.projects.zones.getServerconfig';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TServerConfig) as TServerConfig;
end;



Function TProjectsZonesResource.GetClustersNodePoolsInstance : TProjectsZonesClustersNodePoolsResource;

begin
  if (FClustersNodePoolsInstance=Nil) then
    FClustersNodePoolsInstance:=CreateClustersNodePoolsResource;
  Result:=FClustersNodePoolsInstance;
end;

Function TProjectsZonesResource.CreateClustersNodePoolsResource : TProjectsZonesClustersNodePoolsResource;

begin
  Result:=CreateClustersNodePoolsResource(Self);
end;


Function TProjectsZonesResource.CreateClustersNodePoolsResource(AOwner : TComponent) : TProjectsZonesClustersNodePoolsResource;

begin
  Result:=TProjectsZonesClustersNodePoolsResource.Create(AOwner);
  Result.API:=Self.API;
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



Function TProjectsResource.GetZonesClustersNodePoolsInstance : TProjectsZonesClustersNodePoolsResource;

begin
  if (FZonesClustersNodePoolsInstance=Nil) then
    FZonesClustersNodePoolsInstance:=CreateZonesClustersNodePoolsResource;
  Result:=FZonesClustersNodePoolsInstance;
end;

Function TProjectsResource.CreateZonesClustersNodePoolsResource : TProjectsZonesClustersNodePoolsResource;

begin
  Result:=CreateZonesClustersNodePoolsResource(Self);
end;


Function TProjectsResource.CreateZonesClustersNodePoolsResource(AOwner : TComponent) : TProjectsZonesClustersNodePoolsResource;

begin
  Result:=TProjectsZonesClustersNodePoolsResource.Create(AOwner);
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
  Result:='v1';
end;

Class Function TContainerAPI.APIRevision : String;

begin
  Result:='20160421';
end;

Class Function TContainerAPI.APIID : String;

begin
  Result:='container:v1';
end;

Class Function TContainerAPI.APITitle : String;

begin
  Result:='Google Container Engine API';
end;

Class Function TContainerAPI.APIDescription : String;

begin
  Result:='Builds and manages clusters that run container-based applications, powered by open source Kubernetes technology.';
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
  Result:='https://cloud.google.com/container-engine/';
end;

Class Function TContainerAPI.APIrootUrl : string;

begin
  Result:='https://container.googleapis.com/';
end;

Class Function TContainerAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TContainerAPI.APIbaseURL : String;

begin
  Result:='https://container.googleapis.com/';
end;

Class Function TContainerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TContainerAPI.APIservicePath : string;

begin
  Result:='';
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
  TListClustersResponse.RegisterObject;
  TCluster.RegisterObject;
  TNodeConfigTypemetadata.RegisterObject;
  TNodeConfig.RegisterObject;
  TMasterAuth.RegisterObject;
  TAddonsConfig.RegisterObject;
  THttpLoadBalancing.RegisterObject;
  THorizontalPodAutoscaling.RegisterObject;
  TNodePool.RegisterObject;
  TCreateClusterRequest.RegisterObject;
  TOperation.RegisterObject;
  TUpdateClusterRequest.RegisterObject;
  TClusterUpdate.RegisterObject;
  TListOperationsResponse.RegisterObject;
  TServerConfig.RegisterObject;
  TListNodePoolsResponse.RegisterObject;
  TCreateNodePoolRequest.RegisterObject;
end;


Function TContainerAPI.GetProjectsZonesClustersNodePoolsInstance : TProjectsZonesClustersNodePoolsResource;

begin
  if (FProjectsZonesClustersNodePoolsInstance=Nil) then
    FProjectsZonesClustersNodePoolsInstance:=CreateProjectsZonesClustersNodePoolsResource;
  Result:=FProjectsZonesClustersNodePoolsInstance;
end;

Function TContainerAPI.CreateProjectsZonesClustersNodePoolsResource : TProjectsZonesClustersNodePoolsResource;

begin
  Result:=CreateProjectsZonesClustersNodePoolsResource(Self);
end;


Function TContainerAPI.CreateProjectsZonesClustersNodePoolsResource(AOwner : TComponent) : TProjectsZonesClustersNodePoolsResource;

begin
  Result:=TProjectsZonesClustersNodePoolsResource.Create(AOwner);
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
