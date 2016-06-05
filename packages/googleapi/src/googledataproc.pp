unit googledataproc;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TCluster = Class;
  TClusterConfig = Class;
  TGceClusterConfig = Class;
  TInstanceGroupConfig = Class;
  TDiskConfig = Class;
  TManagedGroupConfig = Class;
  TSoftwareConfig = Class;
  TNodeInitializationAction = Class;
  TClusterStatus = Class;
  TOperation = Class;
  TStatus = Class;
  TListClustersResponse = Class;
  TDiagnoseClusterRequest = Class;
  TSubmitJobRequest = Class;
  TJob = Class;
  TJobReference = Class;
  TJobPlacement = Class;
  THadoopJob = Class;
  TLoggingConfig = Class;
  TSparkJob = Class;
  TPySparkJob = Class;
  THiveJob = Class;
  TQueryList = Class;
  TPigJob = Class;
  TSparkSqlJob = Class;
  TJobStatus = Class;
  TListJobsResponse = Class;
  TCancelJobRequest = Class;
  TEmpty = Class;
  TListOperationsResponse = Class;
  TDiagnoseClusterResults = Class;
  TClusterOperationMetadata = Class;
  TClusterOperationStatus = Class;
  TDiagnoseClusterOutputLocation = Class;
  TOperationMetadata = Class;
  TOperationStatus = Class;
  TClusterArray = Array of TCluster;
  TClusterConfigArray = Array of TClusterConfig;
  TGceClusterConfigArray = Array of TGceClusterConfig;
  TInstanceGroupConfigArray = Array of TInstanceGroupConfig;
  TDiskConfigArray = Array of TDiskConfig;
  TManagedGroupConfigArray = Array of TManagedGroupConfig;
  TSoftwareConfigArray = Array of TSoftwareConfig;
  TNodeInitializationActionArray = Array of TNodeInitializationAction;
  TClusterStatusArray = Array of TClusterStatus;
  TOperationArray = Array of TOperation;
  TStatusArray = Array of TStatus;
  TListClustersResponseArray = Array of TListClustersResponse;
  TDiagnoseClusterRequestArray = Array of TDiagnoseClusterRequest;
  TSubmitJobRequestArray = Array of TSubmitJobRequest;
  TJobArray = Array of TJob;
  TJobReferenceArray = Array of TJobReference;
  TJobPlacementArray = Array of TJobPlacement;
  THadoopJobArray = Array of THadoopJob;
  TLoggingConfigArray = Array of TLoggingConfig;
  TSparkJobArray = Array of TSparkJob;
  TPySparkJobArray = Array of TPySparkJob;
  THiveJobArray = Array of THiveJob;
  TQueryListArray = Array of TQueryList;
  TPigJobArray = Array of TPigJob;
  TSparkSqlJobArray = Array of TSparkSqlJob;
  TJobStatusArray = Array of TJobStatus;
  TListJobsResponseArray = Array of TListJobsResponse;
  TCancelJobRequestArray = Array of TCancelJobRequest;
  TEmptyArray = Array of TEmpty;
  TListOperationsResponseArray = Array of TListOperationsResponse;
  TDiagnoseClusterResultsArray = Array of TDiagnoseClusterResults;
  TClusterOperationMetadataArray = Array of TClusterOperationMetadata;
  TClusterOperationStatusArray = Array of TClusterOperationStatus;
  TDiagnoseClusterOutputLocationArray = Array of TDiagnoseClusterOutputLocation;
  TOperationMetadataArray = Array of TOperationMetadata;
  TOperationStatusArray = Array of TOperationStatus;
  //Anonymous types, using auto-generated names
  TGceClusterConfigTypemetadata = Class;
  TSoftwareConfigTypeproperties = Class;
  TOperationTypemetadata = Class;
  TOperationTyperesponse = Class;
  TStatusTypedetailsItem = Class;
  THadoopJobTypeproperties = Class;
  TLoggingConfigTypedriverLogLevels = Class;
  TSparkJobTypeproperties = Class;
  TPySparkJobTypeproperties = Class;
  THiveJobTypescriptVariables = Class;
  THiveJobTypeproperties = Class;
  TPigJobTypescriptVariables = Class;
  TPigJobTypeproperties = Class;
  TSparkSqlJobTypescriptVariables = Class;
  TSparkSqlJobTypeproperties = Class;
  TClusterTypestatusHistoryArray = Array of TClusterStatus;
  TClusterConfigTypeinitializationActionsArray = Array of TNodeInitializationAction;
  TStatusTypedetailsArray = Array of TStatusTypedetailsItem;
  TListClustersResponseTypeclustersArray = Array of TCluster;
  TJobTypestatusHistoryArray = Array of TJobStatus;
  TListJobsResponseTypejobsArray = Array of TJob;
  TListOperationsResponseTypeoperationsArray = Array of TOperation;
  TClusterOperationMetadataTypestatusHistoryArray = Array of TClusterOperationStatus;
  TOperationMetadataTypestatusHistoryArray = Array of TOperationStatus;
  
  { --------------------------------------------------------------------
    TCluster
    --------------------------------------------------------------------}
  
  TCluster = Class(TGoogleBaseObject)
  Private
    FprojectId : String;
    FclusterName : String;
    Fconfig : TClusterConfig;
    Fstatus : TClusterStatus;
    FstatusHistory : TClusterTypestatusHistoryArray;
    FclusterUuid : String;
  Protected
    //Property setters
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclusterName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setconfig(AIndex : Integer; const AValue : TClusterConfig); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : TClusterStatus); virtual;
    Procedure SetstatusHistory(AIndex : Integer; const AValue : TClusterTypestatusHistoryArray); virtual;
    Procedure SetclusterUuid(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property projectId : String Index 0 Read FprojectId Write SetprojectId;
    Property clusterName : String Index 8 Read FclusterName Write SetclusterName;
    Property config : TClusterConfig Index 16 Read Fconfig Write Setconfig;
    Property status : TClusterStatus Index 24 Read Fstatus Write Setstatus;
    Property statusHistory : TClusterTypestatusHistoryArray Index 32 Read FstatusHistory Write SetstatusHistory;
    Property clusterUuid : String Index 40 Read FclusterUuid Write SetclusterUuid;
  end;
  TClusterClass = Class of TCluster;
  
  { --------------------------------------------------------------------
    TClusterConfig
    --------------------------------------------------------------------}
  
  TClusterConfig = Class(TGoogleBaseObject)
  Private
    FconfigBucket : String;
    FgceClusterConfig : TGceClusterConfig;
    FmasterConfig : TInstanceGroupConfig;
    FworkerConfig : TInstanceGroupConfig;
    FsecondaryWorkerConfig : TInstanceGroupConfig;
    FsoftwareConfig : TSoftwareConfig;
    FinitializationActions : TClusterConfigTypeinitializationActionsArray;
  Protected
    //Property setters
    Procedure SetconfigBucket(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgceClusterConfig(AIndex : Integer; const AValue : TGceClusterConfig); virtual;
    Procedure SetmasterConfig(AIndex : Integer; const AValue : TInstanceGroupConfig); virtual;
    Procedure SetworkerConfig(AIndex : Integer; const AValue : TInstanceGroupConfig); virtual;
    Procedure SetsecondaryWorkerConfig(AIndex : Integer; const AValue : TInstanceGroupConfig); virtual;
    Procedure SetsoftwareConfig(AIndex : Integer; const AValue : TSoftwareConfig); virtual;
    Procedure SetinitializationActions(AIndex : Integer; const AValue : TClusterConfigTypeinitializationActionsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property configBucket : String Index 0 Read FconfigBucket Write SetconfigBucket;
    Property gceClusterConfig : TGceClusterConfig Index 8 Read FgceClusterConfig Write SetgceClusterConfig;
    Property masterConfig : TInstanceGroupConfig Index 16 Read FmasterConfig Write SetmasterConfig;
    Property workerConfig : TInstanceGroupConfig Index 24 Read FworkerConfig Write SetworkerConfig;
    Property secondaryWorkerConfig : TInstanceGroupConfig Index 32 Read FsecondaryWorkerConfig Write SetsecondaryWorkerConfig;
    Property softwareConfig : TSoftwareConfig Index 40 Read FsoftwareConfig Write SetsoftwareConfig;
    Property initializationActions : TClusterConfigTypeinitializationActionsArray Index 48 Read FinitializationActions Write SetinitializationActions;
  end;
  TClusterConfigClass = Class of TClusterConfig;
  
  { --------------------------------------------------------------------
    TGceClusterConfigTypemetadata
    --------------------------------------------------------------------}
  
  TGceClusterConfigTypemetadata = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TGceClusterConfigTypemetadataClass = Class of TGceClusterConfigTypemetadata;
  
  { --------------------------------------------------------------------
    TGceClusterConfig
    --------------------------------------------------------------------}
  
  TGceClusterConfig = Class(TGoogleBaseObject)
  Private
    FzoneUri : String;
    FnetworkUri : String;
    FsubnetworkUri : String;
    FserviceAccountScopes : TStringArray;
    Ftags : TStringArray;
    Fmetadata : TGceClusterConfigTypemetadata;
  Protected
    //Property setters
    Procedure SetzoneUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnetworkUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsubnetworkUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetserviceAccountScopes(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Settags(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setmetadata(AIndex : Integer; const AValue : TGceClusterConfigTypemetadata); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property zoneUri : String Index 0 Read FzoneUri Write SetzoneUri;
    Property networkUri : String Index 8 Read FnetworkUri Write SetnetworkUri;
    Property subnetworkUri : String Index 16 Read FsubnetworkUri Write SetsubnetworkUri;
    Property serviceAccountScopes : TStringArray Index 24 Read FserviceAccountScopes Write SetserviceAccountScopes;
    Property tags : TStringArray Index 32 Read Ftags Write Settags;
    Property metadata : TGceClusterConfigTypemetadata Index 40 Read Fmetadata Write Setmetadata;
  end;
  TGceClusterConfigClass = Class of TGceClusterConfig;
  
  { --------------------------------------------------------------------
    TInstanceGroupConfig
    --------------------------------------------------------------------}
  
  TInstanceGroupConfig = Class(TGoogleBaseObject)
  Private
    FnumInstances : integer;
    FinstanceNames : TStringArray;
    FimageUri : String;
    FmachineTypeUri : String;
    FdiskConfig : TDiskConfig;
    FisPreemptible : boolean;
    FmanagedGroupConfig : TManagedGroupConfig;
  Protected
    //Property setters
    Procedure SetnumInstances(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetinstanceNames(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetimageUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmachineTypeUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdiskConfig(AIndex : Integer; const AValue : TDiskConfig); virtual;
    Procedure SetisPreemptible(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetmanagedGroupConfig(AIndex : Integer; const AValue : TManagedGroupConfig); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property numInstances : integer Index 0 Read FnumInstances Write SetnumInstances;
    Property instanceNames : TStringArray Index 8 Read FinstanceNames Write SetinstanceNames;
    Property imageUri : String Index 16 Read FimageUri Write SetimageUri;
    Property machineTypeUri : String Index 24 Read FmachineTypeUri Write SetmachineTypeUri;
    Property diskConfig : TDiskConfig Index 32 Read FdiskConfig Write SetdiskConfig;
    Property isPreemptible : boolean Index 40 Read FisPreemptible Write SetisPreemptible;
    Property managedGroupConfig : TManagedGroupConfig Index 48 Read FmanagedGroupConfig Write SetmanagedGroupConfig;
  end;
  TInstanceGroupConfigClass = Class of TInstanceGroupConfig;
  
  { --------------------------------------------------------------------
    TDiskConfig
    --------------------------------------------------------------------}
  
  TDiskConfig = Class(TGoogleBaseObject)
  Private
    FbootDiskSizeGb : integer;
    FnumLocalSsds : integer;
  Protected
    //Property setters
    Procedure SetbootDiskSizeGb(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetnumLocalSsds(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property bootDiskSizeGb : integer Index 0 Read FbootDiskSizeGb Write SetbootDiskSizeGb;
    Property numLocalSsds : integer Index 8 Read FnumLocalSsds Write SetnumLocalSsds;
  end;
  TDiskConfigClass = Class of TDiskConfig;
  
  { --------------------------------------------------------------------
    TManagedGroupConfig
    --------------------------------------------------------------------}
  
  TManagedGroupConfig = Class(TGoogleBaseObject)
  Private
    FinstanceTemplateName : String;
    FinstanceGroupManagerName : String;
  Protected
    //Property setters
    Procedure SetinstanceTemplateName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinstanceGroupManagerName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property instanceTemplateName : String Index 0 Read FinstanceTemplateName Write SetinstanceTemplateName;
    Property instanceGroupManagerName : String Index 8 Read FinstanceGroupManagerName Write SetinstanceGroupManagerName;
  end;
  TManagedGroupConfigClass = Class of TManagedGroupConfig;
  
  { --------------------------------------------------------------------
    TSoftwareConfigTypeproperties
    --------------------------------------------------------------------}
  
  TSoftwareConfigTypeproperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSoftwareConfigTypepropertiesClass = Class of TSoftwareConfigTypeproperties;
  
  { --------------------------------------------------------------------
    TSoftwareConfig
    --------------------------------------------------------------------}
  
  TSoftwareConfig = Class(TGoogleBaseObject)
  Private
    FimageVersion : String;
    Fproperties : TSoftwareConfigTypeproperties;
  Protected
    //Property setters
    Procedure SetimageVersion(AIndex : Integer; const AValue : String); virtual;
    Procedure Setproperties(AIndex : Integer; const AValue : TSoftwareConfigTypeproperties); virtual;
  Public
  Published
    Property imageVersion : String Index 0 Read FimageVersion Write SetimageVersion;
    Property properties : TSoftwareConfigTypeproperties Index 8 Read Fproperties Write Setproperties;
  end;
  TSoftwareConfigClass = Class of TSoftwareConfig;
  
  { --------------------------------------------------------------------
    TNodeInitializationAction
    --------------------------------------------------------------------}
  
  TNodeInitializationAction = Class(TGoogleBaseObject)
  Private
    FexecutableFile : String;
    FexecutionTimeout : String;
  Protected
    //Property setters
    Procedure SetexecutableFile(AIndex : Integer; const AValue : String); virtual;
    Procedure SetexecutionTimeout(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property executableFile : String Index 0 Read FexecutableFile Write SetexecutableFile;
    Property executionTimeout : String Index 8 Read FexecutionTimeout Write SetexecutionTimeout;
  end;
  TNodeInitializationActionClass = Class of TNodeInitializationAction;
  
  { --------------------------------------------------------------------
    TClusterStatus
    --------------------------------------------------------------------}
  
  TClusterStatus = Class(TGoogleBaseObject)
  Private
    Fstate : String;
    Fdetail : String;
    FstateStartTime : String;
  Protected
    //Property setters
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdetail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstateStartTime(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property state : String Index 0 Read Fstate Write Setstate;
    Property detail : String Index 8 Read Fdetail Write Setdetail;
    Property stateStartTime : String Index 16 Read FstateStartTime Write SetstateStartTime;
  end;
  TClusterStatusClass = Class of TClusterStatus;
  
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
    Fname : String;
    Fmetadata : TOperationTypemetadata;
    Fdone : boolean;
    Ferror : TStatus;
    Fresponse : TOperationTyperesponse;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmetadata(AIndex : Integer; const AValue : TOperationTypemetadata); virtual;
    Procedure Setdone(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Seterror(AIndex : Integer; const AValue : TStatus); virtual;
    Procedure Setresponse(AIndex : Integer; const AValue : TOperationTyperesponse); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property metadata : TOperationTypemetadata Index 8 Read Fmetadata Write Setmetadata;
    Property done : boolean Index 16 Read Fdone Write Setdone;
    Property error : TStatus Index 24 Read Ferror Write Seterror;
    Property response : TOperationTyperesponse Index 32 Read Fresponse Write Setresponse;
  end;
  TOperationClass = Class of TOperation;
  
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
    Fmessage : String;
    Fdetails : TStatusTypedetailsArray;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdetails(AIndex : Integer; const AValue : TStatusTypedetailsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property code : integer Index 0 Read Fcode Write Setcode;
    Property message : String Index 8 Read Fmessage Write Setmessage;
    Property details : TStatusTypedetailsArray Index 16 Read Fdetails Write Setdetails;
  end;
  TStatusClass = Class of TStatus;
  
  { --------------------------------------------------------------------
    TListClustersResponse
    --------------------------------------------------------------------}
  
  TListClustersResponse = Class(TGoogleBaseObject)
  Private
    Fclusters : TListClustersResponseTypeclustersArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setclusters(AIndex : Integer; const AValue : TListClustersResponseTypeclustersArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property clusters : TListClustersResponseTypeclustersArray Index 0 Read Fclusters Write Setclusters;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListClustersResponseClass = Class of TListClustersResponse;
  
  { --------------------------------------------------------------------
    TDiagnoseClusterRequest
    --------------------------------------------------------------------}
  
  TDiagnoseClusterRequest = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDiagnoseClusterRequestClass = Class of TDiagnoseClusterRequest;
  
  { --------------------------------------------------------------------
    TSubmitJobRequest
    --------------------------------------------------------------------}
  
  TSubmitJobRequest = Class(TGoogleBaseObject)
  Private
    Fjob : TJob;
  Protected
    //Property setters
    Procedure Setjob(AIndex : Integer; const AValue : TJob); virtual;
  Public
  Published
    Property job : TJob Index 0 Read Fjob Write Setjob;
  end;
  TSubmitJobRequestClass = Class of TSubmitJobRequest;
  
  { --------------------------------------------------------------------
    TJob
    --------------------------------------------------------------------}
  
  TJob = Class(TGoogleBaseObject)
  Private
    Freference : TJobReference;
    Fplacement : TJobPlacement;
    FhadoopJob : THadoopJob;
    FsparkJob : TSparkJob;
    FpysparkJob : TPySparkJob;
    FhiveJob : THiveJob;
    FpigJob : TPigJob;
    FsparkSqlJob : TSparkSqlJob;
    Fstatus : TJobStatus;
    FstatusHistory : TJobTypestatusHistoryArray;
    FdriverOutputResourceUri : String;
    FdriverControlFilesUri : String;
  Protected
    //Property setters
    Procedure Setreference(AIndex : Integer; const AValue : TJobReference); virtual;
    Procedure Setplacement(AIndex : Integer; const AValue : TJobPlacement); virtual;
    Procedure SethadoopJob(AIndex : Integer; const AValue : THadoopJob); virtual;
    Procedure SetsparkJob(AIndex : Integer; const AValue : TSparkJob); virtual;
    Procedure SetpysparkJob(AIndex : Integer; const AValue : TPySparkJob); virtual;
    Procedure SethiveJob(AIndex : Integer; const AValue : THiveJob); virtual;
    Procedure SetpigJob(AIndex : Integer; const AValue : TPigJob); virtual;
    Procedure SetsparkSqlJob(AIndex : Integer; const AValue : TSparkSqlJob); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : TJobStatus); virtual;
    Procedure SetstatusHistory(AIndex : Integer; const AValue : TJobTypestatusHistoryArray); virtual;
    Procedure SetdriverOutputResourceUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdriverControlFilesUri(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property reference : TJobReference Index 0 Read Freference Write Setreference;
    Property placement : TJobPlacement Index 8 Read Fplacement Write Setplacement;
    Property hadoopJob : THadoopJob Index 16 Read FhadoopJob Write SethadoopJob;
    Property sparkJob : TSparkJob Index 24 Read FsparkJob Write SetsparkJob;
    Property pysparkJob : TPySparkJob Index 32 Read FpysparkJob Write SetpysparkJob;
    Property hiveJob : THiveJob Index 40 Read FhiveJob Write SethiveJob;
    Property pigJob : TPigJob Index 48 Read FpigJob Write SetpigJob;
    Property sparkSqlJob : TSparkSqlJob Index 56 Read FsparkSqlJob Write SetsparkSqlJob;
    Property status : TJobStatus Index 64 Read Fstatus Write Setstatus;
    Property statusHistory : TJobTypestatusHistoryArray Index 72 Read FstatusHistory Write SetstatusHistory;
    Property driverOutputResourceUri : String Index 80 Read FdriverOutputResourceUri Write SetdriverOutputResourceUri;
    Property driverControlFilesUri : String Index 88 Read FdriverControlFilesUri Write SetdriverControlFilesUri;
  end;
  TJobClass = Class of TJob;
  
  { --------------------------------------------------------------------
    TJobReference
    --------------------------------------------------------------------}
  
  TJobReference = Class(TGoogleBaseObject)
  Private
    FprojectId : String;
    FjobId : String;
  Protected
    //Property setters
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetjobId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property projectId : String Index 0 Read FprojectId Write SetprojectId;
    Property jobId : String Index 8 Read FjobId Write SetjobId;
  end;
  TJobReferenceClass = Class of TJobReference;
  
  { --------------------------------------------------------------------
    TJobPlacement
    --------------------------------------------------------------------}
  
  TJobPlacement = Class(TGoogleBaseObject)
  Private
    FclusterName : String;
    FclusterUuid : String;
  Protected
    //Property setters
    Procedure SetclusterName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclusterUuid(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property clusterName : String Index 0 Read FclusterName Write SetclusterName;
    Property clusterUuid : String Index 8 Read FclusterUuid Write SetclusterUuid;
  end;
  TJobPlacementClass = Class of TJobPlacement;
  
  { --------------------------------------------------------------------
    THadoopJobTypeproperties
    --------------------------------------------------------------------}
  
  THadoopJobTypeproperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  THadoopJobTypepropertiesClass = Class of THadoopJobTypeproperties;
  
  { --------------------------------------------------------------------
    THadoopJob
    --------------------------------------------------------------------}
  
  THadoopJob = Class(TGoogleBaseObject)
  Private
    FmainJarFileUri : String;
    FmainClass : String;
    Fargs : TStringArray;
    FjarFileUris : TStringArray;
    FfileUris : TStringArray;
    FarchiveUris : TStringArray;
    Fproperties : THadoopJobTypeproperties;
    FloggingConfig : TLoggingConfig;
  Protected
    //Property setters
    Procedure SetmainJarFileUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmainClass(AIndex : Integer; const AValue : String); virtual;
    Procedure Setargs(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetjarFileUris(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetfileUris(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetarchiveUris(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setproperties(AIndex : Integer; const AValue : THadoopJobTypeproperties); virtual;
    Procedure SetloggingConfig(AIndex : Integer; const AValue : TLoggingConfig); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property mainJarFileUri : String Index 0 Read FmainJarFileUri Write SetmainJarFileUri;
    Property mainClass : String Index 8 Read FmainClass Write SetmainClass;
    Property args : TStringArray Index 16 Read Fargs Write Setargs;
    Property jarFileUris : TStringArray Index 24 Read FjarFileUris Write SetjarFileUris;
    Property fileUris : TStringArray Index 32 Read FfileUris Write SetfileUris;
    Property archiveUris : TStringArray Index 40 Read FarchiveUris Write SetarchiveUris;
    Property properties : THadoopJobTypeproperties Index 48 Read Fproperties Write Setproperties;
    Property loggingConfig : TLoggingConfig Index 56 Read FloggingConfig Write SetloggingConfig;
  end;
  THadoopJobClass = Class of THadoopJob;
  
  { --------------------------------------------------------------------
    TLoggingConfigTypedriverLogLevels
    --------------------------------------------------------------------}
  
  TLoggingConfigTypedriverLogLevels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TLoggingConfigTypedriverLogLevelsClass = Class of TLoggingConfigTypedriverLogLevels;
  
  { --------------------------------------------------------------------
    TLoggingConfig
    --------------------------------------------------------------------}
  
  TLoggingConfig = Class(TGoogleBaseObject)
  Private
    FdriverLogLevels : TLoggingConfigTypedriverLogLevels;
  Protected
    //Property setters
    Procedure SetdriverLogLevels(AIndex : Integer; const AValue : TLoggingConfigTypedriverLogLevels); virtual;
  Public
  Published
    Property driverLogLevels : TLoggingConfigTypedriverLogLevels Index 0 Read FdriverLogLevels Write SetdriverLogLevels;
  end;
  TLoggingConfigClass = Class of TLoggingConfig;
  
  { --------------------------------------------------------------------
    TSparkJobTypeproperties
    --------------------------------------------------------------------}
  
  TSparkJobTypeproperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSparkJobTypepropertiesClass = Class of TSparkJobTypeproperties;
  
  { --------------------------------------------------------------------
    TSparkJob
    --------------------------------------------------------------------}
  
  TSparkJob = Class(TGoogleBaseObject)
  Private
    FmainJarFileUri : String;
    FmainClass : String;
    Fargs : TStringArray;
    FjarFileUris : TStringArray;
    FfileUris : TStringArray;
    FarchiveUris : TStringArray;
    Fproperties : TSparkJobTypeproperties;
    FloggingConfig : TLoggingConfig;
  Protected
    //Property setters
    Procedure SetmainJarFileUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmainClass(AIndex : Integer; const AValue : String); virtual;
    Procedure Setargs(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetjarFileUris(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetfileUris(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetarchiveUris(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setproperties(AIndex : Integer; const AValue : TSparkJobTypeproperties); virtual;
    Procedure SetloggingConfig(AIndex : Integer; const AValue : TLoggingConfig); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property mainJarFileUri : String Index 0 Read FmainJarFileUri Write SetmainJarFileUri;
    Property mainClass : String Index 8 Read FmainClass Write SetmainClass;
    Property args : TStringArray Index 16 Read Fargs Write Setargs;
    Property jarFileUris : TStringArray Index 24 Read FjarFileUris Write SetjarFileUris;
    Property fileUris : TStringArray Index 32 Read FfileUris Write SetfileUris;
    Property archiveUris : TStringArray Index 40 Read FarchiveUris Write SetarchiveUris;
    Property properties : TSparkJobTypeproperties Index 48 Read Fproperties Write Setproperties;
    Property loggingConfig : TLoggingConfig Index 56 Read FloggingConfig Write SetloggingConfig;
  end;
  TSparkJobClass = Class of TSparkJob;
  
  { --------------------------------------------------------------------
    TPySparkJobTypeproperties
    --------------------------------------------------------------------}
  
  TPySparkJobTypeproperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TPySparkJobTypepropertiesClass = Class of TPySparkJobTypeproperties;
  
  { --------------------------------------------------------------------
    TPySparkJob
    --------------------------------------------------------------------}
  
  TPySparkJob = Class(TGoogleBaseObject)
  Private
    FmainPythonFileUri : String;
    Fargs : TStringArray;
    FpythonFileUris : TStringArray;
    FjarFileUris : TStringArray;
    FfileUris : TStringArray;
    FarchiveUris : TStringArray;
    Fproperties : TPySparkJobTypeproperties;
    FloggingConfig : TLoggingConfig;
  Protected
    //Property setters
    Procedure SetmainPythonFileUri(AIndex : Integer; const AValue : String); virtual;
    Procedure Setargs(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetpythonFileUris(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetjarFileUris(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetfileUris(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetarchiveUris(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setproperties(AIndex : Integer; const AValue : TPySparkJobTypeproperties); virtual;
    Procedure SetloggingConfig(AIndex : Integer; const AValue : TLoggingConfig); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property mainPythonFileUri : String Index 0 Read FmainPythonFileUri Write SetmainPythonFileUri;
    Property args : TStringArray Index 8 Read Fargs Write Setargs;
    Property pythonFileUris : TStringArray Index 16 Read FpythonFileUris Write SetpythonFileUris;
    Property jarFileUris : TStringArray Index 24 Read FjarFileUris Write SetjarFileUris;
    Property fileUris : TStringArray Index 32 Read FfileUris Write SetfileUris;
    Property archiveUris : TStringArray Index 40 Read FarchiveUris Write SetarchiveUris;
    Property properties : TPySparkJobTypeproperties Index 48 Read Fproperties Write Setproperties;
    Property loggingConfig : TLoggingConfig Index 56 Read FloggingConfig Write SetloggingConfig;
  end;
  TPySparkJobClass = Class of TPySparkJob;
  
  { --------------------------------------------------------------------
    THiveJobTypescriptVariables
    --------------------------------------------------------------------}
  
  THiveJobTypescriptVariables = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  THiveJobTypescriptVariablesClass = Class of THiveJobTypescriptVariables;
  
  { --------------------------------------------------------------------
    THiveJobTypeproperties
    --------------------------------------------------------------------}
  
  THiveJobTypeproperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  THiveJobTypepropertiesClass = Class of THiveJobTypeproperties;
  
  { --------------------------------------------------------------------
    THiveJob
    --------------------------------------------------------------------}
  
  THiveJob = Class(TGoogleBaseObject)
  Private
    FqueryFileUri : String;
    FqueryList : TQueryList;
    FcontinueOnFailure : boolean;
    FscriptVariables : THiveJobTypescriptVariables;
    Fproperties : THiveJobTypeproperties;
    FjarFileUris : TStringArray;
  Protected
    //Property setters
    Procedure SetqueryFileUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetqueryList(AIndex : Integer; const AValue : TQueryList); virtual;
    Procedure SetcontinueOnFailure(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetscriptVariables(AIndex : Integer; const AValue : THiveJobTypescriptVariables); virtual;
    Procedure Setproperties(AIndex : Integer; const AValue : THiveJobTypeproperties); virtual;
    Procedure SetjarFileUris(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property queryFileUri : String Index 0 Read FqueryFileUri Write SetqueryFileUri;
    Property queryList : TQueryList Index 8 Read FqueryList Write SetqueryList;
    Property continueOnFailure : boolean Index 16 Read FcontinueOnFailure Write SetcontinueOnFailure;
    Property scriptVariables : THiveJobTypescriptVariables Index 24 Read FscriptVariables Write SetscriptVariables;
    Property properties : THiveJobTypeproperties Index 32 Read Fproperties Write Setproperties;
    Property jarFileUris : TStringArray Index 40 Read FjarFileUris Write SetjarFileUris;
  end;
  THiveJobClass = Class of THiveJob;
  
  { --------------------------------------------------------------------
    TQueryList
    --------------------------------------------------------------------}
  
  TQueryList = Class(TGoogleBaseObject)
  Private
    Fqueries : TStringArray;
  Protected
    //Property setters
    Procedure Setqueries(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property queries : TStringArray Index 0 Read Fqueries Write Setqueries;
  end;
  TQueryListClass = Class of TQueryList;
  
  { --------------------------------------------------------------------
    TPigJobTypescriptVariables
    --------------------------------------------------------------------}
  
  TPigJobTypescriptVariables = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TPigJobTypescriptVariablesClass = Class of TPigJobTypescriptVariables;
  
  { --------------------------------------------------------------------
    TPigJobTypeproperties
    --------------------------------------------------------------------}
  
  TPigJobTypeproperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TPigJobTypepropertiesClass = Class of TPigJobTypeproperties;
  
  { --------------------------------------------------------------------
    TPigJob
    --------------------------------------------------------------------}
  
  TPigJob = Class(TGoogleBaseObject)
  Private
    FqueryFileUri : String;
    FqueryList : TQueryList;
    FcontinueOnFailure : boolean;
    FscriptVariables : TPigJobTypescriptVariables;
    Fproperties : TPigJobTypeproperties;
    FjarFileUris : TStringArray;
    FloggingConfig : TLoggingConfig;
  Protected
    //Property setters
    Procedure SetqueryFileUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetqueryList(AIndex : Integer; const AValue : TQueryList); virtual;
    Procedure SetcontinueOnFailure(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetscriptVariables(AIndex : Integer; const AValue : TPigJobTypescriptVariables); virtual;
    Procedure Setproperties(AIndex : Integer; const AValue : TPigJobTypeproperties); virtual;
    Procedure SetjarFileUris(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetloggingConfig(AIndex : Integer; const AValue : TLoggingConfig); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property queryFileUri : String Index 0 Read FqueryFileUri Write SetqueryFileUri;
    Property queryList : TQueryList Index 8 Read FqueryList Write SetqueryList;
    Property continueOnFailure : boolean Index 16 Read FcontinueOnFailure Write SetcontinueOnFailure;
    Property scriptVariables : TPigJobTypescriptVariables Index 24 Read FscriptVariables Write SetscriptVariables;
    Property properties : TPigJobTypeproperties Index 32 Read Fproperties Write Setproperties;
    Property jarFileUris : TStringArray Index 40 Read FjarFileUris Write SetjarFileUris;
    Property loggingConfig : TLoggingConfig Index 48 Read FloggingConfig Write SetloggingConfig;
  end;
  TPigJobClass = Class of TPigJob;
  
  { --------------------------------------------------------------------
    TSparkSqlJobTypescriptVariables
    --------------------------------------------------------------------}
  
  TSparkSqlJobTypescriptVariables = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSparkSqlJobTypescriptVariablesClass = Class of TSparkSqlJobTypescriptVariables;
  
  { --------------------------------------------------------------------
    TSparkSqlJobTypeproperties
    --------------------------------------------------------------------}
  
  TSparkSqlJobTypeproperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSparkSqlJobTypepropertiesClass = Class of TSparkSqlJobTypeproperties;
  
  { --------------------------------------------------------------------
    TSparkSqlJob
    --------------------------------------------------------------------}
  
  TSparkSqlJob = Class(TGoogleBaseObject)
  Private
    FqueryFileUri : String;
    FqueryList : TQueryList;
    FscriptVariables : TSparkSqlJobTypescriptVariables;
    Fproperties : TSparkSqlJobTypeproperties;
    FjarFileUris : TStringArray;
    FloggingConfig : TLoggingConfig;
  Protected
    //Property setters
    Procedure SetqueryFileUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetqueryList(AIndex : Integer; const AValue : TQueryList); virtual;
    Procedure SetscriptVariables(AIndex : Integer; const AValue : TSparkSqlJobTypescriptVariables); virtual;
    Procedure Setproperties(AIndex : Integer; const AValue : TSparkSqlJobTypeproperties); virtual;
    Procedure SetjarFileUris(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetloggingConfig(AIndex : Integer; const AValue : TLoggingConfig); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property queryFileUri : String Index 0 Read FqueryFileUri Write SetqueryFileUri;
    Property queryList : TQueryList Index 8 Read FqueryList Write SetqueryList;
    Property scriptVariables : TSparkSqlJobTypescriptVariables Index 16 Read FscriptVariables Write SetscriptVariables;
    Property properties : TSparkSqlJobTypeproperties Index 24 Read Fproperties Write Setproperties;
    Property jarFileUris : TStringArray Index 32 Read FjarFileUris Write SetjarFileUris;
    Property loggingConfig : TLoggingConfig Index 40 Read FloggingConfig Write SetloggingConfig;
  end;
  TSparkSqlJobClass = Class of TSparkSqlJob;
  
  { --------------------------------------------------------------------
    TJobStatus
    --------------------------------------------------------------------}
  
  TJobStatus = Class(TGoogleBaseObject)
  Private
    Fstate : String;
    Fdetails : String;
    FstateStartTime : String;
  Protected
    //Property setters
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdetails(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstateStartTime(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property state : String Index 0 Read Fstate Write Setstate;
    Property details : String Index 8 Read Fdetails Write Setdetails;
    Property stateStartTime : String Index 16 Read FstateStartTime Write SetstateStartTime;
  end;
  TJobStatusClass = Class of TJobStatus;
  
  { --------------------------------------------------------------------
    TListJobsResponse
    --------------------------------------------------------------------}
  
  TListJobsResponse = Class(TGoogleBaseObject)
  Private
    Fjobs : TListJobsResponseTypejobsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setjobs(AIndex : Integer; const AValue : TListJobsResponseTypejobsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property jobs : TListJobsResponseTypejobsArray Index 0 Read Fjobs Write Setjobs;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListJobsResponseClass = Class of TListJobsResponse;
  
  { --------------------------------------------------------------------
    TCancelJobRequest
    --------------------------------------------------------------------}
  
  TCancelJobRequest = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCancelJobRequestClass = Class of TCancelJobRequest;
  
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
    TListOperationsResponse
    --------------------------------------------------------------------}
  
  TListOperationsResponse = Class(TGoogleBaseObject)
  Private
    Foperations : TListOperationsResponseTypeoperationsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setoperations(AIndex : Integer; const AValue : TListOperationsResponseTypeoperationsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property operations : TListOperationsResponseTypeoperationsArray Index 0 Read Foperations Write Setoperations;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListOperationsResponseClass = Class of TListOperationsResponse;
  
  { --------------------------------------------------------------------
    TDiagnoseClusterResults
    --------------------------------------------------------------------}
  
  TDiagnoseClusterResults = Class(TGoogleBaseObject)
  Private
    FoutputUri : String;
  Protected
    //Property setters
    Procedure SetoutputUri(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property outputUri : String Index 0 Read FoutputUri Write SetoutputUri;
  end;
  TDiagnoseClusterResultsClass = Class of TDiagnoseClusterResults;
  
  { --------------------------------------------------------------------
    TClusterOperationMetadata
    --------------------------------------------------------------------}
  
  TClusterOperationMetadata = Class(TGoogleBaseObject)
  Private
    FclusterName : String;
    FclusterUuid : String;
    Fstatus : TClusterOperationStatus;
    FstatusHistory : TClusterOperationMetadataTypestatusHistoryArray;
    FoperationType : String;
    Fdescription : String;
  Protected
    //Property setters
    Procedure SetclusterName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclusterUuid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : TClusterOperationStatus); virtual;
    Procedure SetstatusHistory(AIndex : Integer; const AValue : TClusterOperationMetadataTypestatusHistoryArray); virtual;
    Procedure SetoperationType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property clusterName : String Index 0 Read FclusterName Write SetclusterName;
    Property clusterUuid : String Index 8 Read FclusterUuid Write SetclusterUuid;
    Property status : TClusterOperationStatus Index 16 Read Fstatus Write Setstatus;
    Property statusHistory : TClusterOperationMetadataTypestatusHistoryArray Index 24 Read FstatusHistory Write SetstatusHistory;
    Property operationType : String Index 32 Read FoperationType Write SetoperationType;
    Property description : String Index 40 Read Fdescription Write Setdescription;
  end;
  TClusterOperationMetadataClass = Class of TClusterOperationMetadata;
  
  { --------------------------------------------------------------------
    TClusterOperationStatus
    --------------------------------------------------------------------}
  
  TClusterOperationStatus = Class(TGoogleBaseObject)
  Private
    Fstate : String;
    FinnerState : String;
    Fdetails : String;
    FstateStartTime : String;
  Protected
    //Property setters
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinnerState(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdetails(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstateStartTime(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property state : String Index 0 Read Fstate Write Setstate;
    Property innerState : String Index 8 Read FinnerState Write SetinnerState;
    Property details : String Index 16 Read Fdetails Write Setdetails;
    Property stateStartTime : String Index 24 Read FstateStartTime Write SetstateStartTime;
  end;
  TClusterOperationStatusClass = Class of TClusterOperationStatus;
  
  { --------------------------------------------------------------------
    TDiagnoseClusterOutputLocation
    --------------------------------------------------------------------}
  
  TDiagnoseClusterOutputLocation = Class(TGoogleBaseObject)
  Private
    FoutputUri : String;
  Protected
    //Property setters
    Procedure SetoutputUri(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property outputUri : String Index 0 Read FoutputUri Write SetoutputUri;
  end;
  TDiagnoseClusterOutputLocationClass = Class of TDiagnoseClusterOutputLocation;
  
  { --------------------------------------------------------------------
    TOperationMetadata
    --------------------------------------------------------------------}
  
  TOperationMetadata = Class(TGoogleBaseObject)
  Private
    Fstate : String;
    FinnerState : String;
    Fdetails : String;
    FinsertTime : String;
    FstartTime : String;
    FendTime : String;
    FclusterName : String;
    FclusterUuid : String;
    Fstatus : TOperationStatus;
    FstatusHistory : TOperationMetadataTypestatusHistoryArray;
    FoperationType : String;
    Fdescription : String;
  Protected
    //Property setters
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinnerState(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdetails(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinsertTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclusterName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclusterUuid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : TOperationStatus); virtual;
    Procedure SetstatusHistory(AIndex : Integer; const AValue : TOperationMetadataTypestatusHistoryArray); virtual;
    Procedure SetoperationType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property state : String Index 0 Read Fstate Write Setstate;
    Property innerState : String Index 8 Read FinnerState Write SetinnerState;
    Property details : String Index 16 Read Fdetails Write Setdetails;
    Property insertTime : String Index 24 Read FinsertTime Write SetinsertTime;
    Property startTime : String Index 32 Read FstartTime Write SetstartTime;
    Property endTime : String Index 40 Read FendTime Write SetendTime;
    Property clusterName : String Index 48 Read FclusterName Write SetclusterName;
    Property clusterUuid : String Index 56 Read FclusterUuid Write SetclusterUuid;
    Property status : TOperationStatus Index 64 Read Fstatus Write Setstatus;
    Property statusHistory : TOperationMetadataTypestatusHistoryArray Index 72 Read FstatusHistory Write SetstatusHistory;
    Property operationType : String Index 80 Read FoperationType Write SetoperationType;
    Property description : String Index 88 Read Fdescription Write Setdescription;
  end;
  TOperationMetadataClass = Class of TOperationMetadata;
  
  { --------------------------------------------------------------------
    TOperationStatus
    --------------------------------------------------------------------}
  
  TOperationStatus = Class(TGoogleBaseObject)
  Private
    Fstate : String;
    FinnerState : String;
    Fdetails : String;
    FstateStartTime : String;
  Protected
    //Property setters
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinnerState(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdetails(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstateStartTime(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property state : String Index 0 Read Fstate Write Setstate;
    Property innerState : String Index 8 Read FinnerState Write SetinnerState;
    Property details : String Index 16 Read Fdetails Write Setdetails;
    Property stateStartTime : String Index 24 Read FstateStartTime Write SetstateStartTime;
  end;
  TOperationStatusClass = Class of TOperationStatus;
  
  { --------------------------------------------------------------------
    TProjectsRegionsClustersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsRegionsClustersResource, method Patch
  
  TProjectsRegionsClustersPatchOptions = Record
    updateMask : String;
  end;
  
  
  //Optional query Options for TProjectsRegionsClustersResource, method List
  
  TProjectsRegionsClustersListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TProjectsRegionsClustersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(projectId: string; region: string; aCluster : TCluster) : TOperation;overload;
    Function Patch(projectId: string; region: string; clusterName: string; aCluster : TCluster; AQuery : string  = '') : TOperation;
    Function Patch(projectId: string; region: string; clusterName: string; aCluster : TCluster; AQuery : TProjectsRegionsClusterspatchOptions) : TOperation;
    Function Delete(projectId: string; region: string; clusterName: string) : TOperation;
    Function Get(projectId: string; region: string; clusterName: string) : TCluster;
    Function List(projectId: string; region: string; AQuery : string  = '') : TListClustersResponse;
    Function List(projectId: string; region: string; AQuery : TProjectsRegionsClusterslistOptions) : TListClustersResponse;
    Function Diagnose(projectId: string; region: string; clusterName: string; aDiagnoseClusterRequest : TDiagnoseClusterRequest) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsRegionsJobsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsRegionsJobsResource, method List
  
  TProjectsRegionsJobsListOptions = Record
    pageSize : integer;
    pageToken : String;
    clusterName : String;
    jobStateMatcher : String;
  end;
  
  TProjectsRegionsJobsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Submit(projectId: string; region: string; aSubmitJobRequest : TSubmitJobRequest) : TJob;
    Function Get(projectId: string; region: string; jobId: string) : TJob;
    Function List(projectId: string; region: string; AQuery : string  = '') : TListJobsResponse;
    Function List(projectId: string; region: string; AQuery : TProjectsRegionsJobslistOptions) : TListJobsResponse;
    Function Cancel(projectId: string; region: string; jobId: string; aCancelJobRequest : TCancelJobRequest) : TJob;
    Function Delete(projectId: string; region: string; jobId: string) : TEmpty;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsRegionsOperationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsRegionsOperationsResource, method List
  
  TProjectsRegionsOperationsListOptions = Record
    filter : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  TProjectsRegionsOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(_name: string) : TOperation;
    Function List(_name: string; AQuery : string  = '') : TListOperationsResponse;
    Function List(_name: string; AQuery : TProjectsRegionsOperationslistOptions) : TListOperationsResponse;
    Function Cancel(_name: string) : TEmpty;
    Function Delete(_name: string) : TEmpty;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsRegionsResource
    --------------------------------------------------------------------}
  
  TProjectsRegionsResource = Class(TGoogleResource)
  Private
    FClustersInstance : TProjectsRegionsClustersResource;
    FJobsInstance : TProjectsRegionsJobsResource;
    FOperationsInstance : TProjectsRegionsOperationsResource;
    Function GetClustersInstance : TProjectsRegionsClustersResource;virtual;
    Function GetJobsInstance : TProjectsRegionsJobsResource;virtual;
    Function GetOperationsInstance : TProjectsRegionsOperationsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateClustersResource(AOwner : TComponent) : TProjectsRegionsClustersResource;virtual;overload;
    Function CreateClustersResource : TProjectsRegionsClustersResource;virtual;overload;
    Function CreateJobsResource(AOwner : TComponent) : TProjectsRegionsJobsResource;virtual;overload;
    Function CreateJobsResource : TProjectsRegionsJobsResource;virtual;overload;
    Function CreateOperationsResource(AOwner : TComponent) : TProjectsRegionsOperationsResource;virtual;overload;
    Function CreateOperationsResource : TProjectsRegionsOperationsResource;virtual;overload;
    Property ClustersResource : TProjectsRegionsClustersResource Read GetClustersInstance;
    Property JobsResource : TProjectsRegionsJobsResource Read GetJobsInstance;
    Property OperationsResource : TProjectsRegionsOperationsResource Read GetOperationsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Private
    FRegionsClustersInstance : TProjectsRegionsClustersResource;
    FRegionsJobsInstance : TProjectsRegionsJobsResource;
    FRegionsOperationsInstance : TProjectsRegionsOperationsResource;
    FRegionsInstance : TProjectsRegionsResource;
    Function GetRegionsClustersInstance : TProjectsRegionsClustersResource;virtual;
    Function GetRegionsJobsInstance : TProjectsRegionsJobsResource;virtual;
    Function GetRegionsOperationsInstance : TProjectsRegionsOperationsResource;virtual;
    Function GetRegionsInstance : TProjectsRegionsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateRegionsClustersResource(AOwner : TComponent) : TProjectsRegionsClustersResource;virtual;overload;
    Function CreateRegionsClustersResource : TProjectsRegionsClustersResource;virtual;overload;
    Function CreateRegionsJobsResource(AOwner : TComponent) : TProjectsRegionsJobsResource;virtual;overload;
    Function CreateRegionsJobsResource : TProjectsRegionsJobsResource;virtual;overload;
    Function CreateRegionsOperationsResource(AOwner : TComponent) : TProjectsRegionsOperationsResource;virtual;overload;
    Function CreateRegionsOperationsResource : TProjectsRegionsOperationsResource;virtual;overload;
    Function CreateRegionsResource(AOwner : TComponent) : TProjectsRegionsResource;virtual;overload;
    Function CreateRegionsResource : TProjectsRegionsResource;virtual;overload;
    Property RegionsClustersResource : TProjectsRegionsClustersResource Read GetRegionsClustersInstance;
    Property RegionsJobsResource : TProjectsRegionsJobsResource Read GetRegionsJobsInstance;
    Property RegionsOperationsResource : TProjectsRegionsOperationsResource Read GetRegionsOperationsInstance;
    Property RegionsResource : TProjectsRegionsResource Read GetRegionsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TDataprocAPI
    --------------------------------------------------------------------}
  
  TDataprocAPI = Class(TGoogleAPI)
  Private
    FProjectsRegionsClustersInstance : TProjectsRegionsClustersResource;
    FProjectsRegionsJobsInstance : TProjectsRegionsJobsResource;
    FProjectsRegionsOperationsInstance : TProjectsRegionsOperationsResource;
    FProjectsRegionsInstance : TProjectsRegionsResource;
    FProjectsInstance : TProjectsResource;
    Function GetProjectsRegionsClustersInstance : TProjectsRegionsClustersResource;virtual;
    Function GetProjectsRegionsJobsInstance : TProjectsRegionsJobsResource;virtual;
    Function GetProjectsRegionsOperationsInstance : TProjectsRegionsOperationsResource;virtual;
    Function GetProjectsRegionsInstance : TProjectsRegionsResource;virtual;
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
    Function CreateProjectsRegionsClustersResource(AOwner : TComponent) : TProjectsRegionsClustersResource;virtual;overload;
    Function CreateProjectsRegionsClustersResource : TProjectsRegionsClustersResource;virtual;overload;
    Function CreateProjectsRegionsJobsResource(AOwner : TComponent) : TProjectsRegionsJobsResource;virtual;overload;
    Function CreateProjectsRegionsJobsResource : TProjectsRegionsJobsResource;virtual;overload;
    Function CreateProjectsRegionsOperationsResource(AOwner : TComponent) : TProjectsRegionsOperationsResource;virtual;overload;
    Function CreateProjectsRegionsOperationsResource : TProjectsRegionsOperationsResource;virtual;overload;
    Function CreateProjectsRegionsResource(AOwner : TComponent) : TProjectsRegionsResource;virtual;overload;
    Function CreateProjectsRegionsResource : TProjectsRegionsResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsRegionsClustersResource : TProjectsRegionsClustersResource Read GetProjectsRegionsClustersInstance;
    Property ProjectsRegionsJobsResource : TProjectsRegionsJobsResource Read GetProjectsRegionsJobsInstance;
    Property ProjectsRegionsOperationsResource : TProjectsRegionsOperationsResource Read GetProjectsRegionsOperationsInstance;
    Property ProjectsRegionsResource : TProjectsRegionsResource Read GetProjectsRegionsInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TCluster
  --------------------------------------------------------------------}


Procedure TCluster.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetclusterName(AIndex : Integer; const AValue : String); 

begin
  If (FclusterName=AValue) then exit;
  FclusterName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setconfig(AIndex : Integer; const AValue : TClusterConfig); 

begin
  If (Fconfig=AValue) then exit;
  Fconfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.Setstatus(AIndex : Integer; const AValue : TClusterStatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetstatusHistory(AIndex : Integer; const AValue : TClusterTypestatusHistoryArray); 

begin
  If (FstatusHistory=AValue) then exit;
  FstatusHistory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCluster.SetclusterUuid(AIndex : Integer; const AValue : String); 

begin
  If (FclusterUuid=AValue) then exit;
  FclusterUuid:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCluster.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'statushistory' : SetLength(FstatusHistory,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TClusterConfig
  --------------------------------------------------------------------}


Procedure TClusterConfig.SetconfigBucket(AIndex : Integer; const AValue : String); 

begin
  If (FconfigBucket=AValue) then exit;
  FconfigBucket:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterConfig.SetgceClusterConfig(AIndex : Integer; const AValue : TGceClusterConfig); 

begin
  If (FgceClusterConfig=AValue) then exit;
  FgceClusterConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterConfig.SetmasterConfig(AIndex : Integer; const AValue : TInstanceGroupConfig); 

begin
  If (FmasterConfig=AValue) then exit;
  FmasterConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterConfig.SetworkerConfig(AIndex : Integer; const AValue : TInstanceGroupConfig); 

begin
  If (FworkerConfig=AValue) then exit;
  FworkerConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterConfig.SetsecondaryWorkerConfig(AIndex : Integer; const AValue : TInstanceGroupConfig); 

begin
  If (FsecondaryWorkerConfig=AValue) then exit;
  FsecondaryWorkerConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterConfig.SetsoftwareConfig(AIndex : Integer; const AValue : TSoftwareConfig); 

begin
  If (FsoftwareConfig=AValue) then exit;
  FsoftwareConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterConfig.SetinitializationActions(AIndex : Integer; const AValue : TClusterConfigTypeinitializationActionsArray); 

begin
  If (FinitializationActions=AValue) then exit;
  FinitializationActions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TClusterConfig.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'initializationactions' : SetLength(FinitializationActions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGceClusterConfigTypemetadata
  --------------------------------------------------------------------}


Class Function TGceClusterConfigTypemetadata.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TGceClusterConfig
  --------------------------------------------------------------------}


Procedure TGceClusterConfig.SetzoneUri(AIndex : Integer; const AValue : String); 

begin
  If (FzoneUri=AValue) then exit;
  FzoneUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGceClusterConfig.SetnetworkUri(AIndex : Integer; const AValue : String); 

begin
  If (FnetworkUri=AValue) then exit;
  FnetworkUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGceClusterConfig.SetsubnetworkUri(AIndex : Integer; const AValue : String); 

begin
  If (FsubnetworkUri=AValue) then exit;
  FsubnetworkUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGceClusterConfig.SetserviceAccountScopes(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FserviceAccountScopes=AValue) then exit;
  FserviceAccountScopes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGceClusterConfig.Settags(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGceClusterConfig.Setmetadata(AIndex : Integer; const AValue : TGceClusterConfigTypemetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGceClusterConfig.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'serviceaccountscopes' : SetLength(FserviceAccountScopes,ALength);
  'tags' : SetLength(Ftags,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TInstanceGroupConfig
  --------------------------------------------------------------------}


Procedure TInstanceGroupConfig.SetnumInstances(AIndex : Integer; const AValue : integer); 

begin
  If (FnumInstances=AValue) then exit;
  FnumInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupConfig.SetinstanceNames(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FinstanceNames=AValue) then exit;
  FinstanceNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupConfig.SetimageUri(AIndex : Integer; const AValue : String); 

begin
  If (FimageUri=AValue) then exit;
  FimageUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupConfig.SetmachineTypeUri(AIndex : Integer; const AValue : String); 

begin
  If (FmachineTypeUri=AValue) then exit;
  FmachineTypeUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupConfig.SetdiskConfig(AIndex : Integer; const AValue : TDiskConfig); 

begin
  If (FdiskConfig=AValue) then exit;
  FdiskConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupConfig.SetisPreemptible(AIndex : Integer; const AValue : boolean); 

begin
  If (FisPreemptible=AValue) then exit;
  FisPreemptible:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupConfig.SetmanagedGroupConfig(AIndex : Integer; const AValue : TManagedGroupConfig); 

begin
  If (FmanagedGroupConfig=AValue) then exit;
  FmanagedGroupConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TInstanceGroupConfig.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'instancenames' : SetLength(FinstanceNames,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDiskConfig
  --------------------------------------------------------------------}


Procedure TDiskConfig.SetbootDiskSizeGb(AIndex : Integer; const AValue : integer); 

begin
  If (FbootDiskSizeGb=AValue) then exit;
  FbootDiskSizeGb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskConfig.SetnumLocalSsds(AIndex : Integer; const AValue : integer); 

begin
  If (FnumLocalSsds=AValue) then exit;
  FnumLocalSsds:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TManagedGroupConfig
  --------------------------------------------------------------------}


Procedure TManagedGroupConfig.SetinstanceTemplateName(AIndex : Integer; const AValue : String); 

begin
  If (FinstanceTemplateName=AValue) then exit;
  FinstanceTemplateName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManagedGroupConfig.SetinstanceGroupManagerName(AIndex : Integer; const AValue : String); 

begin
  If (FinstanceGroupManagerName=AValue) then exit;
  FinstanceGroupManagerName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSoftwareConfigTypeproperties
  --------------------------------------------------------------------}


Class Function TSoftwareConfigTypeproperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSoftwareConfig
  --------------------------------------------------------------------}


Procedure TSoftwareConfig.SetimageVersion(AIndex : Integer; const AValue : String); 

begin
  If (FimageVersion=AValue) then exit;
  FimageVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSoftwareConfig.Setproperties(AIndex : Integer; const AValue : TSoftwareConfigTypeproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNodeInitializationAction
  --------------------------------------------------------------------}


Procedure TNodeInitializationAction.SetexecutableFile(AIndex : Integer; const AValue : String); 

begin
  If (FexecutableFile=AValue) then exit;
  FexecutableFile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNodeInitializationAction.SetexecutionTimeout(AIndex : Integer; const AValue : String); 

begin
  If (FexecutionTimeout=AValue) then exit;
  FexecutionTimeout:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TClusterStatus
  --------------------------------------------------------------------}


Procedure TClusterStatus.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterStatus.Setdetail(AIndex : Integer; const AValue : String); 

begin
  If (Fdetail=AValue) then exit;
  Fdetail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterStatus.SetstateStartTime(AIndex : Integer; const AValue : String); 

begin
  If (FstateStartTime=AValue) then exit;
  FstateStartTime:=AValue;
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


Procedure TOperation.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setmetadata(AIndex : Integer; const AValue : TOperationTypemetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setdone(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdone=AValue) then exit;
  Fdone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Seterror(AIndex : Integer; const AValue : TStatus); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setresponse(AIndex : Integer; const AValue : TOperationTyperesponse); 

begin
  If (Fresponse=AValue) then exit;
  Fresponse:=AValue;
  MarkPropertyChanged(AIndex);
end;





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



Procedure TStatus.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setdetails(AIndex : Integer; const AValue : TStatusTypedetailsArray); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
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
  TListClustersResponse
  --------------------------------------------------------------------}


Procedure TListClustersResponse.Setclusters(AIndex : Integer; const AValue : TListClustersResponseTypeclustersArray); 

begin
  If (Fclusters=AValue) then exit;
  Fclusters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListClustersResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
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
  TDiagnoseClusterRequest
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSubmitJobRequest
  --------------------------------------------------------------------}


Procedure TSubmitJobRequest.Setjob(AIndex : Integer; const AValue : TJob); 

begin
  If (Fjob=AValue) then exit;
  Fjob:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJob
  --------------------------------------------------------------------}


Procedure TJob.Setreference(AIndex : Integer; const AValue : TJobReference); 

begin
  If (Freference=AValue) then exit;
  Freference:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setplacement(AIndex : Integer; const AValue : TJobPlacement); 

begin
  If (Fplacement=AValue) then exit;
  Fplacement:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SethadoopJob(AIndex : Integer; const AValue : THadoopJob); 

begin
  If (FhadoopJob=AValue) then exit;
  FhadoopJob:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetsparkJob(AIndex : Integer; const AValue : TSparkJob); 

begin
  If (FsparkJob=AValue) then exit;
  FsparkJob:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetpysparkJob(AIndex : Integer; const AValue : TPySparkJob); 

begin
  If (FpysparkJob=AValue) then exit;
  FpysparkJob:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SethiveJob(AIndex : Integer; const AValue : THiveJob); 

begin
  If (FhiveJob=AValue) then exit;
  FhiveJob:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetpigJob(AIndex : Integer; const AValue : TPigJob); 

begin
  If (FpigJob=AValue) then exit;
  FpigJob:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetsparkSqlJob(AIndex : Integer; const AValue : TSparkSqlJob); 

begin
  If (FsparkSqlJob=AValue) then exit;
  FsparkSqlJob:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setstatus(AIndex : Integer; const AValue : TJobStatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetstatusHistory(AIndex : Integer; const AValue : TJobTypestatusHistoryArray); 

begin
  If (FstatusHistory=AValue) then exit;
  FstatusHistory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetdriverOutputResourceUri(AIndex : Integer; const AValue : String); 

begin
  If (FdriverOutputResourceUri=AValue) then exit;
  FdriverOutputResourceUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetdriverControlFilesUri(AIndex : Integer; const AValue : String); 

begin
  If (FdriverControlFilesUri=AValue) then exit;
  FdriverControlFilesUri:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TJob.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'statushistory' : SetLength(FstatusHistory,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TJobReference
  --------------------------------------------------------------------}


Procedure TJobReference.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobReference.SetjobId(AIndex : Integer; const AValue : String); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobPlacement
  --------------------------------------------------------------------}


Procedure TJobPlacement.SetclusterName(AIndex : Integer; const AValue : String); 

begin
  If (FclusterName=AValue) then exit;
  FclusterName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobPlacement.SetclusterUuid(AIndex : Integer; const AValue : String); 

begin
  If (FclusterUuid=AValue) then exit;
  FclusterUuid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THadoopJobTypeproperties
  --------------------------------------------------------------------}


Class Function THadoopJobTypeproperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  THadoopJob
  --------------------------------------------------------------------}


Procedure THadoopJob.SetmainJarFileUri(AIndex : Integer; const AValue : String); 

begin
  If (FmainJarFileUri=AValue) then exit;
  FmainJarFileUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THadoopJob.SetmainClass(AIndex : Integer; const AValue : String); 

begin
  If (FmainClass=AValue) then exit;
  FmainClass:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THadoopJob.Setargs(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fargs=AValue) then exit;
  Fargs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THadoopJob.SetjarFileUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FjarFileUris=AValue) then exit;
  FjarFileUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THadoopJob.SetfileUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FfileUris=AValue) then exit;
  FfileUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THadoopJob.SetarchiveUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FarchiveUris=AValue) then exit;
  FarchiveUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THadoopJob.Setproperties(AIndex : Integer; const AValue : THadoopJobTypeproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THadoopJob.SetloggingConfig(AIndex : Integer; const AValue : TLoggingConfig); 

begin
  If (FloggingConfig=AValue) then exit;
  FloggingConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure THadoopJob.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'args' : SetLength(Fargs,ALength);
  'jarfileuris' : SetLength(FjarFileUris,ALength);
  'fileuris' : SetLength(FfileUris,ALength);
  'archiveuris' : SetLength(FarchiveUris,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLoggingConfigTypedriverLogLevels
  --------------------------------------------------------------------}


Class Function TLoggingConfigTypedriverLogLevels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TLoggingConfig
  --------------------------------------------------------------------}


Procedure TLoggingConfig.SetdriverLogLevels(AIndex : Integer; const AValue : TLoggingConfigTypedriverLogLevels); 

begin
  If (FdriverLogLevels=AValue) then exit;
  FdriverLogLevels:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSparkJobTypeproperties
  --------------------------------------------------------------------}


Class Function TSparkJobTypeproperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSparkJob
  --------------------------------------------------------------------}


Procedure TSparkJob.SetmainJarFileUri(AIndex : Integer; const AValue : String); 

begin
  If (FmainJarFileUri=AValue) then exit;
  FmainJarFileUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSparkJob.SetmainClass(AIndex : Integer; const AValue : String); 

begin
  If (FmainClass=AValue) then exit;
  FmainClass:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSparkJob.Setargs(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fargs=AValue) then exit;
  Fargs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSparkJob.SetjarFileUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FjarFileUris=AValue) then exit;
  FjarFileUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSparkJob.SetfileUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FfileUris=AValue) then exit;
  FfileUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSparkJob.SetarchiveUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FarchiveUris=AValue) then exit;
  FarchiveUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSparkJob.Setproperties(AIndex : Integer; const AValue : TSparkJobTypeproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSparkJob.SetloggingConfig(AIndex : Integer; const AValue : TLoggingConfig); 

begin
  If (FloggingConfig=AValue) then exit;
  FloggingConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSparkJob.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'args' : SetLength(Fargs,ALength);
  'jarfileuris' : SetLength(FjarFileUris,ALength);
  'fileuris' : SetLength(FfileUris,ALength);
  'archiveuris' : SetLength(FarchiveUris,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPySparkJobTypeproperties
  --------------------------------------------------------------------}


Class Function TPySparkJobTypeproperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TPySparkJob
  --------------------------------------------------------------------}


Procedure TPySparkJob.SetmainPythonFileUri(AIndex : Integer; const AValue : String); 

begin
  If (FmainPythonFileUri=AValue) then exit;
  FmainPythonFileUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPySparkJob.Setargs(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fargs=AValue) then exit;
  Fargs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPySparkJob.SetpythonFileUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FpythonFileUris=AValue) then exit;
  FpythonFileUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPySparkJob.SetjarFileUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FjarFileUris=AValue) then exit;
  FjarFileUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPySparkJob.SetfileUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FfileUris=AValue) then exit;
  FfileUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPySparkJob.SetarchiveUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FarchiveUris=AValue) then exit;
  FarchiveUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPySparkJob.Setproperties(AIndex : Integer; const AValue : TPySparkJobTypeproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPySparkJob.SetloggingConfig(AIndex : Integer; const AValue : TLoggingConfig); 

begin
  If (FloggingConfig=AValue) then exit;
  FloggingConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPySparkJob.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'args' : SetLength(Fargs,ALength);
  'pythonfileuris' : SetLength(FpythonFileUris,ALength);
  'jarfileuris' : SetLength(FjarFileUris,ALength);
  'fileuris' : SetLength(FfileUris,ALength);
  'archiveuris' : SetLength(FarchiveUris,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  THiveJobTypescriptVariables
  --------------------------------------------------------------------}


Class Function THiveJobTypescriptVariables.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  THiveJobTypeproperties
  --------------------------------------------------------------------}


Class Function THiveJobTypeproperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  THiveJob
  --------------------------------------------------------------------}


Procedure THiveJob.SetqueryFileUri(AIndex : Integer; const AValue : String); 

begin
  If (FqueryFileUri=AValue) then exit;
  FqueryFileUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THiveJob.SetqueryList(AIndex : Integer; const AValue : TQueryList); 

begin
  If (FqueryList=AValue) then exit;
  FqueryList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THiveJob.SetcontinueOnFailure(AIndex : Integer; const AValue : boolean); 

begin
  If (FcontinueOnFailure=AValue) then exit;
  FcontinueOnFailure:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THiveJob.SetscriptVariables(AIndex : Integer; const AValue : THiveJobTypescriptVariables); 

begin
  If (FscriptVariables=AValue) then exit;
  FscriptVariables:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THiveJob.Setproperties(AIndex : Integer; const AValue : THiveJobTypeproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THiveJob.SetjarFileUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FjarFileUris=AValue) then exit;
  FjarFileUris:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure THiveJob.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'jarfileuris' : SetLength(FjarFileUris,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TQueryList
  --------------------------------------------------------------------}


Procedure TQueryList.Setqueries(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fqueries=AValue) then exit;
  Fqueries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TQueryList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'queries' : SetLength(Fqueries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPigJobTypescriptVariables
  --------------------------------------------------------------------}


Class Function TPigJobTypescriptVariables.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TPigJobTypeproperties
  --------------------------------------------------------------------}


Class Function TPigJobTypeproperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TPigJob
  --------------------------------------------------------------------}


Procedure TPigJob.SetqueryFileUri(AIndex : Integer; const AValue : String); 

begin
  If (FqueryFileUri=AValue) then exit;
  FqueryFileUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPigJob.SetqueryList(AIndex : Integer; const AValue : TQueryList); 

begin
  If (FqueryList=AValue) then exit;
  FqueryList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPigJob.SetcontinueOnFailure(AIndex : Integer; const AValue : boolean); 

begin
  If (FcontinueOnFailure=AValue) then exit;
  FcontinueOnFailure:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPigJob.SetscriptVariables(AIndex : Integer; const AValue : TPigJobTypescriptVariables); 

begin
  If (FscriptVariables=AValue) then exit;
  FscriptVariables:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPigJob.Setproperties(AIndex : Integer; const AValue : TPigJobTypeproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPigJob.SetjarFileUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FjarFileUris=AValue) then exit;
  FjarFileUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPigJob.SetloggingConfig(AIndex : Integer; const AValue : TLoggingConfig); 

begin
  If (FloggingConfig=AValue) then exit;
  FloggingConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPigJob.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'jarfileuris' : SetLength(FjarFileUris,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSparkSqlJobTypescriptVariables
  --------------------------------------------------------------------}


Class Function TSparkSqlJobTypescriptVariables.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSparkSqlJobTypeproperties
  --------------------------------------------------------------------}


Class Function TSparkSqlJobTypeproperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSparkSqlJob
  --------------------------------------------------------------------}


Procedure TSparkSqlJob.SetqueryFileUri(AIndex : Integer; const AValue : String); 

begin
  If (FqueryFileUri=AValue) then exit;
  FqueryFileUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSparkSqlJob.SetqueryList(AIndex : Integer; const AValue : TQueryList); 

begin
  If (FqueryList=AValue) then exit;
  FqueryList:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSparkSqlJob.SetscriptVariables(AIndex : Integer; const AValue : TSparkSqlJobTypescriptVariables); 

begin
  If (FscriptVariables=AValue) then exit;
  FscriptVariables:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSparkSqlJob.Setproperties(AIndex : Integer; const AValue : TSparkSqlJobTypeproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSparkSqlJob.SetjarFileUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FjarFileUris=AValue) then exit;
  FjarFileUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSparkSqlJob.SetloggingConfig(AIndex : Integer; const AValue : TLoggingConfig); 

begin
  If (FloggingConfig=AValue) then exit;
  FloggingConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSparkSqlJob.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'jarfileuris' : SetLength(FjarFileUris,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TJobStatus
  --------------------------------------------------------------------}


Procedure TJobStatus.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatus.Setdetails(AIndex : Integer; const AValue : String); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobStatus.SetstateStartTime(AIndex : Integer; const AValue : String); 

begin
  If (FstateStartTime=AValue) then exit;
  FstateStartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListJobsResponse
  --------------------------------------------------------------------}


Procedure TListJobsResponse.Setjobs(AIndex : Integer; const AValue : TListJobsResponseTypejobsArray); 

begin
  If (Fjobs=AValue) then exit;
  Fjobs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListJobsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListJobsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'jobs' : SetLength(Fjobs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCancelJobRequest
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListOperationsResponse
  --------------------------------------------------------------------}


Procedure TListOperationsResponse.Setoperations(AIndex : Integer; const AValue : TListOperationsResponseTypeoperationsArray); 

begin
  If (Foperations=AValue) then exit;
  Foperations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListOperationsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
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
  TDiagnoseClusterResults
  --------------------------------------------------------------------}


Procedure TDiagnoseClusterResults.SetoutputUri(AIndex : Integer; const AValue : String); 

begin
  If (FoutputUri=AValue) then exit;
  FoutputUri:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TClusterOperationMetadata
  --------------------------------------------------------------------}


Procedure TClusterOperationMetadata.SetclusterName(AIndex : Integer; const AValue : String); 

begin
  If (FclusterName=AValue) then exit;
  FclusterName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterOperationMetadata.SetclusterUuid(AIndex : Integer; const AValue : String); 

begin
  If (FclusterUuid=AValue) then exit;
  FclusterUuid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterOperationMetadata.Setstatus(AIndex : Integer; const AValue : TClusterOperationStatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterOperationMetadata.SetstatusHistory(AIndex : Integer; const AValue : TClusterOperationMetadataTypestatusHistoryArray); 

begin
  If (FstatusHistory=AValue) then exit;
  FstatusHistory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterOperationMetadata.SetoperationType(AIndex : Integer; const AValue : String); 

begin
  If (FoperationType=AValue) then exit;
  FoperationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterOperationMetadata.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TClusterOperationMetadata.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'statushistory' : SetLength(FstatusHistory,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TClusterOperationStatus
  --------------------------------------------------------------------}


Procedure TClusterOperationStatus.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterOperationStatus.SetinnerState(AIndex : Integer; const AValue : String); 

begin
  If (FinnerState=AValue) then exit;
  FinnerState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterOperationStatus.Setdetails(AIndex : Integer; const AValue : String); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClusterOperationStatus.SetstateStartTime(AIndex : Integer; const AValue : String); 

begin
  If (FstateStartTime=AValue) then exit;
  FstateStartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDiagnoseClusterOutputLocation
  --------------------------------------------------------------------}


Procedure TDiagnoseClusterOutputLocation.SetoutputUri(AIndex : Integer; const AValue : String); 

begin
  If (FoutputUri=AValue) then exit;
  FoutputUri:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationMetadata
  --------------------------------------------------------------------}


Procedure TOperationMetadata.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.SetinnerState(AIndex : Integer; const AValue : String); 

begin
  If (FinnerState=AValue) then exit;
  FinnerState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.Setdetails(AIndex : Integer; const AValue : String); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.SetinsertTime(AIndex : Integer; const AValue : String); 

begin
  If (FinsertTime=AValue) then exit;
  FinsertTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.SetstartTime(AIndex : Integer; const AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.SetendTime(AIndex : Integer; const AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.SetclusterName(AIndex : Integer; const AValue : String); 

begin
  If (FclusterName=AValue) then exit;
  FclusterName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.SetclusterUuid(AIndex : Integer; const AValue : String); 

begin
  If (FclusterUuid=AValue) then exit;
  FclusterUuid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.Setstatus(AIndex : Integer; const AValue : TOperationStatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.SetstatusHistory(AIndex : Integer; const AValue : TOperationMetadataTypestatusHistoryArray); 

begin
  If (FstatusHistory=AValue) then exit;
  FstatusHistory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.SetoperationType(AIndex : Integer; const AValue : String); 

begin
  If (FoperationType=AValue) then exit;
  FoperationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperationMetadata.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'statushistory' : SetLength(FstatusHistory,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOperationStatus
  --------------------------------------------------------------------}


Procedure TOperationStatus.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationStatus.SetinnerState(AIndex : Integer; const AValue : String); 

begin
  If (FinnerState=AValue) then exit;
  FinnerState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationStatus.Setdetails(AIndex : Integer; const AValue : String); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationStatus.SetstateStartTime(AIndex : Integer; const AValue : String); 

begin
  If (FstateStartTime=AValue) then exit;
  FstateStartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectsRegionsClustersResource
  --------------------------------------------------------------------}


Class Function TProjectsRegionsClustersResource.ResourceName : String;

begin
  Result:='clusters';
end;

Class Function TProjectsRegionsClustersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdataprocAPI;
end;

Function TProjectsRegionsClustersResource.Create(projectId: string; region: string; aCluster : TCluster) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/projects/{projectId}/regions/{region}/clusters';
  _Methodid   = 'dataproc.projects.regions.clusters.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCluster,TOperation) as TOperation;
end;

Function TProjectsRegionsClustersResource.Patch(projectId: string; region: string; clusterName: string; aCluster : TCluster; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'v1/projects/{projectId}/regions/{region}/clusters/{clusterName}';
  _Methodid   = 'dataproc.projects.regions.clusters.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'region',region,'clusterName',clusterName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aCluster,TOperation) as TOperation;
end;


Function TProjectsRegionsClustersResource.Patch(projectId: string; region: string; clusterName: string; aCluster : TCluster; AQuery : TProjectsRegionsClusterspatchOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'updateMask',AQuery.updateMask);
  Result:=Patch(projectId,region,clusterName,aCluster,_Q);
end;

Function TProjectsRegionsClustersResource.Delete(projectId: string; region: string; clusterName: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/projects/{projectId}/regions/{region}/clusters/{clusterName}';
  _Methodid   = 'dataproc.projects.regions.clusters.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'region',region,'clusterName',clusterName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TProjectsRegionsClustersResource.Get(projectId: string; region: string; clusterName: string) : TCluster;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects/{projectId}/regions/{region}/clusters/{clusterName}';
  _Methodid   = 'dataproc.projects.regions.clusters.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'region',region,'clusterName',clusterName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCluster) as TCluster;
end;

Function TProjectsRegionsClustersResource.List(projectId: string; region: string; AQuery : string = '') : TListClustersResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects/{projectId}/regions/{region}/clusters';
  _Methodid   = 'dataproc.projects.regions.clusters.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListClustersResponse) as TListClustersResponse;
end;


Function TProjectsRegionsClustersResource.List(projectId: string; region: string; AQuery : TProjectsRegionsClusterslistOptions) : TListClustersResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(projectId,region,_Q);
end;

Function TProjectsRegionsClustersResource.Diagnose(projectId: string; region: string; clusterName: string; aDiagnoseClusterRequest : TDiagnoseClusterRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/projects/{projectId}/regions/{region}/clusters/{clusterName}:diagnose';
  _Methodid   = 'dataproc.projects.regions.clusters.diagnose';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'region',region,'clusterName',clusterName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDiagnoseClusterRequest,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TProjectsRegionsJobsResource
  --------------------------------------------------------------------}


Class Function TProjectsRegionsJobsResource.ResourceName : String;

begin
  Result:='jobs';
end;

Class Function TProjectsRegionsJobsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdataprocAPI;
end;

Function TProjectsRegionsJobsResource.Submit(projectId: string; region: string; aSubmitJobRequest : TSubmitJobRequest) : TJob;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/projects/{projectId}/regions/{region}/jobs:submit';
  _Methodid   = 'dataproc.projects.regions.jobs.submit';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSubmitJobRequest,TJob) as TJob;
end;

Function TProjectsRegionsJobsResource.Get(projectId: string; region: string; jobId: string) : TJob;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects/{projectId}/regions/{region}/jobs/{jobId}';
  _Methodid   = 'dataproc.projects.regions.jobs.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'region',region,'jobId',jobId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TJob) as TJob;
end;

Function TProjectsRegionsJobsResource.List(projectId: string; region: string; AQuery : string = '') : TListJobsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/projects/{projectId}/regions/{region}/jobs';
  _Methodid   = 'dataproc.projects.regions.jobs.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListJobsResponse) as TListJobsResponse;
end;


Function TProjectsRegionsJobsResource.List(projectId: string; region: string; AQuery : TProjectsRegionsJobslistOptions) : TListJobsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'clusterName',AQuery.clusterName);
  AddToQuery(_Q,'jobStateMatcher',AQuery.jobStateMatcher);
  Result:=List(projectId,region,_Q);
end;

Function TProjectsRegionsJobsResource.Cancel(projectId: string; region: string; jobId: string; aCancelJobRequest : TCancelJobRequest) : TJob;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/projects/{projectId}/regions/{region}/jobs/{jobId}:cancel';
  _Methodid   = 'dataproc.projects.regions.jobs.cancel';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'region',region,'jobId',jobId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCancelJobRequest,TJob) as TJob;
end;

Function TProjectsRegionsJobsResource.Delete(projectId: string; region: string; jobId: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/projects/{projectId}/regions/{region}/jobs/{jobId}';
  _Methodid   = 'dataproc.projects.regions.jobs.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'region',region,'jobId',jobId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;



{ --------------------------------------------------------------------
  TProjectsRegionsOperationsResource
  --------------------------------------------------------------------}


Class Function TProjectsRegionsOperationsResource.ResourceName : String;

begin
  Result:='operations';
end;

Class Function TProjectsRegionsOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdataprocAPI;
end;

Function TProjectsRegionsOperationsResource.Get(_name: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}';
  _Methodid   = 'dataproc.projects.regions.operations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TProjectsRegionsOperationsResource.List(_name: string; AQuery : string = '') : TListOperationsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}';
  _Methodid   = 'dataproc.projects.regions.operations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListOperationsResponse) as TListOperationsResponse;
end;


Function TProjectsRegionsOperationsResource.List(_name: string; AQuery : TProjectsRegionsOperationslistOptions) : TListOperationsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_name,_Q);
end;

Function TProjectsRegionsOperationsResource.Cancel(_name: string) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+name}:cancel';
  _Methodid   = 'dataproc.projects.regions.operations.cancel';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TProjectsRegionsOperationsResource.Delete(_name: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/{+name}';
  _Methodid   = 'dataproc.projects.regions.operations.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;



{ --------------------------------------------------------------------
  TProjectsRegionsResource
  --------------------------------------------------------------------}


Class Function TProjectsRegionsResource.ResourceName : String;

begin
  Result:='regions';
end;

Class Function TProjectsRegionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdataprocAPI;
end;



Function TProjectsRegionsResource.GetClustersInstance : TProjectsRegionsClustersResource;

begin
  if (FClustersInstance=Nil) then
    FClustersInstance:=CreateClustersResource;
  Result:=FClustersInstance;
end;

Function TProjectsRegionsResource.CreateClustersResource : TProjectsRegionsClustersResource;

begin
  Result:=CreateClustersResource(Self);
end;


Function TProjectsRegionsResource.CreateClustersResource(AOwner : TComponent) : TProjectsRegionsClustersResource;

begin
  Result:=TProjectsRegionsClustersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsRegionsResource.GetJobsInstance : TProjectsRegionsJobsResource;

begin
  if (FJobsInstance=Nil) then
    FJobsInstance:=CreateJobsResource;
  Result:=FJobsInstance;
end;

Function TProjectsRegionsResource.CreateJobsResource : TProjectsRegionsJobsResource;

begin
  Result:=CreateJobsResource(Self);
end;


Function TProjectsRegionsResource.CreateJobsResource(AOwner : TComponent) : TProjectsRegionsJobsResource;

begin
  Result:=TProjectsRegionsJobsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsRegionsResource.GetOperationsInstance : TProjectsRegionsOperationsResource;

begin
  if (FOperationsInstance=Nil) then
    FOperationsInstance:=CreateOperationsResource;
  Result:=FOperationsInstance;
end;

Function TProjectsRegionsResource.CreateOperationsResource : TProjectsRegionsOperationsResource;

begin
  Result:=CreateOperationsResource(Self);
end;


Function TProjectsRegionsResource.CreateOperationsResource(AOwner : TComponent) : TProjectsRegionsOperationsResource;

begin
  Result:=TProjectsRegionsOperationsResource.Create(AOwner);
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
  Result:=TdataprocAPI;
end;



Function TProjectsResource.GetRegionsClustersInstance : TProjectsRegionsClustersResource;

begin
  if (FRegionsClustersInstance=Nil) then
    FRegionsClustersInstance:=CreateRegionsClustersResource;
  Result:=FRegionsClustersInstance;
end;

Function TProjectsResource.CreateRegionsClustersResource : TProjectsRegionsClustersResource;

begin
  Result:=CreateRegionsClustersResource(Self);
end;


Function TProjectsResource.CreateRegionsClustersResource(AOwner : TComponent) : TProjectsRegionsClustersResource;

begin
  Result:=TProjectsRegionsClustersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetRegionsJobsInstance : TProjectsRegionsJobsResource;

begin
  if (FRegionsJobsInstance=Nil) then
    FRegionsJobsInstance:=CreateRegionsJobsResource;
  Result:=FRegionsJobsInstance;
end;

Function TProjectsResource.CreateRegionsJobsResource : TProjectsRegionsJobsResource;

begin
  Result:=CreateRegionsJobsResource(Self);
end;


Function TProjectsResource.CreateRegionsJobsResource(AOwner : TComponent) : TProjectsRegionsJobsResource;

begin
  Result:=TProjectsRegionsJobsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetRegionsOperationsInstance : TProjectsRegionsOperationsResource;

begin
  if (FRegionsOperationsInstance=Nil) then
    FRegionsOperationsInstance:=CreateRegionsOperationsResource;
  Result:=FRegionsOperationsInstance;
end;

Function TProjectsResource.CreateRegionsOperationsResource : TProjectsRegionsOperationsResource;

begin
  Result:=CreateRegionsOperationsResource(Self);
end;


Function TProjectsResource.CreateRegionsOperationsResource(AOwner : TComponent) : TProjectsRegionsOperationsResource;

begin
  Result:=TProjectsRegionsOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetRegionsInstance : TProjectsRegionsResource;

begin
  if (FRegionsInstance=Nil) then
    FRegionsInstance:=CreateRegionsResource;
  Result:=FRegionsInstance;
end;

Function TProjectsResource.CreateRegionsResource : TProjectsRegionsResource;

begin
  Result:=CreateRegionsResource(Self);
end;


Function TProjectsResource.CreateRegionsResource(AOwner : TComponent) : TProjectsRegionsResource;

begin
  Result:=TProjectsRegionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TDataprocAPI
  --------------------------------------------------------------------}

Class Function TDataprocAPI.APIName : String;

begin
  Result:='dataproc';
end;

Class Function TDataprocAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TDataprocAPI.APIRevision : String;

begin
  Result:='20160503';
end;

Class Function TDataprocAPI.APIID : String;

begin
  Result:='dataproc:v1';
end;

Class Function TDataprocAPI.APITitle : String;

begin
  Result:='Google Cloud Dataproc API';
end;

Class Function TDataprocAPI.APIDescription : String;

begin
  Result:='Manages Hadoop-based clusters and jobs on Google Cloud Platform.';
end;

Class Function TDataprocAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TDataprocAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TDataprocAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TDataprocAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TDataprocAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/dataproc/';
end;

Class Function TDataprocAPI.APIrootUrl : string;

begin
  Result:='https://dataproc.googleapis.com/';
end;

Class Function TDataprocAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TDataprocAPI.APIbaseURL : String;

begin
  Result:='https://dataproc.googleapis.com/';
end;

Class Function TDataprocAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TDataprocAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TDataprocAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TDataprocAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  
end;

Class Function TDataprocAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TDataprocAPI.RegisterAPIResources;

begin
  TCluster.RegisterObject;
  TClusterConfig.RegisterObject;
  TGceClusterConfigTypemetadata.RegisterObject;
  TGceClusterConfig.RegisterObject;
  TInstanceGroupConfig.RegisterObject;
  TDiskConfig.RegisterObject;
  TManagedGroupConfig.RegisterObject;
  TSoftwareConfigTypeproperties.RegisterObject;
  TSoftwareConfig.RegisterObject;
  TNodeInitializationAction.RegisterObject;
  TClusterStatus.RegisterObject;
  TOperationTypemetadata.RegisterObject;
  TOperationTyperesponse.RegisterObject;
  TOperation.RegisterObject;
  TStatusTypedetailsItem.RegisterObject;
  TStatus.RegisterObject;
  TListClustersResponse.RegisterObject;
  TDiagnoseClusterRequest.RegisterObject;
  TSubmitJobRequest.RegisterObject;
  TJob.RegisterObject;
  TJobReference.RegisterObject;
  TJobPlacement.RegisterObject;
  THadoopJobTypeproperties.RegisterObject;
  THadoopJob.RegisterObject;
  TLoggingConfigTypedriverLogLevels.RegisterObject;
  TLoggingConfig.RegisterObject;
  TSparkJobTypeproperties.RegisterObject;
  TSparkJob.RegisterObject;
  TPySparkJobTypeproperties.RegisterObject;
  TPySparkJob.RegisterObject;
  THiveJobTypescriptVariables.RegisterObject;
  THiveJobTypeproperties.RegisterObject;
  THiveJob.RegisterObject;
  TQueryList.RegisterObject;
  TPigJobTypescriptVariables.RegisterObject;
  TPigJobTypeproperties.RegisterObject;
  TPigJob.RegisterObject;
  TSparkSqlJobTypescriptVariables.RegisterObject;
  TSparkSqlJobTypeproperties.RegisterObject;
  TSparkSqlJob.RegisterObject;
  TJobStatus.RegisterObject;
  TListJobsResponse.RegisterObject;
  TCancelJobRequest.RegisterObject;
  TEmpty.RegisterObject;
  TListOperationsResponse.RegisterObject;
  TDiagnoseClusterResults.RegisterObject;
  TClusterOperationMetadata.RegisterObject;
  TClusterOperationStatus.RegisterObject;
  TDiagnoseClusterOutputLocation.RegisterObject;
  TOperationMetadata.RegisterObject;
  TOperationStatus.RegisterObject;
end;


Function TDataprocAPI.GetProjectsRegionsClustersInstance : TProjectsRegionsClustersResource;

begin
  if (FProjectsRegionsClustersInstance=Nil) then
    FProjectsRegionsClustersInstance:=CreateProjectsRegionsClustersResource;
  Result:=FProjectsRegionsClustersInstance;
end;

Function TDataprocAPI.CreateProjectsRegionsClustersResource : TProjectsRegionsClustersResource;

begin
  Result:=CreateProjectsRegionsClustersResource(Self);
end;


Function TDataprocAPI.CreateProjectsRegionsClustersResource(AOwner : TComponent) : TProjectsRegionsClustersResource;

begin
  Result:=TProjectsRegionsClustersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDataprocAPI.GetProjectsRegionsJobsInstance : TProjectsRegionsJobsResource;

begin
  if (FProjectsRegionsJobsInstance=Nil) then
    FProjectsRegionsJobsInstance:=CreateProjectsRegionsJobsResource;
  Result:=FProjectsRegionsJobsInstance;
end;

Function TDataprocAPI.CreateProjectsRegionsJobsResource : TProjectsRegionsJobsResource;

begin
  Result:=CreateProjectsRegionsJobsResource(Self);
end;


Function TDataprocAPI.CreateProjectsRegionsJobsResource(AOwner : TComponent) : TProjectsRegionsJobsResource;

begin
  Result:=TProjectsRegionsJobsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDataprocAPI.GetProjectsRegionsOperationsInstance : TProjectsRegionsOperationsResource;

begin
  if (FProjectsRegionsOperationsInstance=Nil) then
    FProjectsRegionsOperationsInstance:=CreateProjectsRegionsOperationsResource;
  Result:=FProjectsRegionsOperationsInstance;
end;

Function TDataprocAPI.CreateProjectsRegionsOperationsResource : TProjectsRegionsOperationsResource;

begin
  Result:=CreateProjectsRegionsOperationsResource(Self);
end;


Function TDataprocAPI.CreateProjectsRegionsOperationsResource(AOwner : TComponent) : TProjectsRegionsOperationsResource;

begin
  Result:=TProjectsRegionsOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDataprocAPI.GetProjectsRegionsInstance : TProjectsRegionsResource;

begin
  if (FProjectsRegionsInstance=Nil) then
    FProjectsRegionsInstance:=CreateProjectsRegionsResource;
  Result:=FProjectsRegionsInstance;
end;

Function TDataprocAPI.CreateProjectsRegionsResource : TProjectsRegionsResource;

begin
  Result:=CreateProjectsRegionsResource(Self);
end;


Function TDataprocAPI.CreateProjectsRegionsResource(AOwner : TComponent) : TProjectsRegionsResource;

begin
  Result:=TProjectsRegionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDataprocAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TDataprocAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TDataprocAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TDataprocAPI.RegisterAPI;
end.
