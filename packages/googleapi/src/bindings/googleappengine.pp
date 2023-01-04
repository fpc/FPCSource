unit googleappengine;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TOperation = Class;
  TStatus = Class;
  TListOperationsResponse = Class;
  TApplication = Class;
  TUrlDispatchRule = Class;
  TVersion = Class;
  TAutomaticScaling = Class;
  TCpuUtilization = Class;
  TRequestUtilization = Class;
  TDiskUtilization = Class;
  TNetworkUtilization = Class;
  TBasicScaling = Class;
  TManualScaling = Class;
  TNetwork = Class;
  TResources = Class;
  TUrlMap = Class;
  TStaticFilesHandler = Class;
  TScriptHandler = Class;
  TApiEndpointHandler = Class;
  TErrorHandler = Class;
  TLibrary = Class;
  TApiConfigHandler = Class;
  THealthCheck = Class;
  TDeployment = Class;
  TFileInfo = Class;
  TContainerInfo = Class;
  TSourceReference = Class;
  TListVersionsResponse = Class;
  TService = Class;
  TTrafficSplit = Class;
  TListServicesResponse = Class;
  TListInstancesResponse = Class;
  TInstance = Class;
  TOperationMetadata = Class;
  TOperationMetadataV1Beta5 = Class;
  TOperationArray = Array of TOperation;
  TStatusArray = Array of TStatus;
  TListOperationsResponseArray = Array of TListOperationsResponse;
  TApplicationArray = Array of TApplication;
  TUrlDispatchRuleArray = Array of TUrlDispatchRule;
  TVersionArray = Array of TVersion;
  TAutomaticScalingArray = Array of TAutomaticScaling;
  TCpuUtilizationArray = Array of TCpuUtilization;
  TRequestUtilizationArray = Array of TRequestUtilization;
  TDiskUtilizationArray = Array of TDiskUtilization;
  TNetworkUtilizationArray = Array of TNetworkUtilization;
  TBasicScalingArray = Array of TBasicScaling;
  TManualScalingArray = Array of TManualScaling;
  TNetworkArray = Array of TNetwork;
  TResourcesArray = Array of TResources;
  TUrlMapArray = Array of TUrlMap;
  TStaticFilesHandlerArray = Array of TStaticFilesHandler;
  TScriptHandlerArray = Array of TScriptHandler;
  TApiEndpointHandlerArray = Array of TApiEndpointHandler;
  TErrorHandlerArray = Array of TErrorHandler;
  TLibraryArray = Array of TLibrary;
  TApiConfigHandlerArray = Array of TApiConfigHandler;
  THealthCheckArray = Array of THealthCheck;
  TDeploymentArray = Array of TDeployment;
  TFileInfoArray = Array of TFileInfo;
  TContainerInfoArray = Array of TContainerInfo;
  TSourceReferenceArray = Array of TSourceReference;
  TListVersionsResponseArray = Array of TListVersionsResponse;
  TServiceArray = Array of TService;
  TTrafficSplitArray = Array of TTrafficSplit;
  TListServicesResponseArray = Array of TListServicesResponse;
  TListInstancesResponseArray = Array of TListInstancesResponse;
  TInstanceArray = Array of TInstance;
  TOperationMetadataArray = Array of TOperationMetadata;
  TOperationMetadataV1Beta5Array = Array of TOperationMetadataV1Beta5;
  //Anonymous types, using auto-generated names
  TOperationTypemetadata = Class;
  TOperationTyperesponse = Class;
  TStatusTypedetailsItem = Class;
  TVersionTypebetaSettings = Class;
  TVersionTypeenvVariables = Class;
  TStaticFilesHandlerTypehttpHeaders = Class;
  TDeploymentTypefiles = Class;
  TTrafficSplitTypeallocations = Class;
  TStatusTypedetailsArray = Array of TStatusTypedetailsItem;
  TListOperationsResponseTypeoperationsArray = Array of TOperation;
  TApplicationTypedispatchRulesArray = Array of TUrlDispatchRule;
  TVersionTypehandlersArray = Array of TUrlMap;
  TVersionTypeerrorHandlersArray = Array of TErrorHandler;
  TVersionTypelibrariesArray = Array of TLibrary;
  TDeploymentTypesourceReferencesArray = Array of TSourceReference;
  TListVersionsResponseTypeversionsArray = Array of TVersion;
  TListServicesResponseTypeservicesArray = Array of TService;
  TListInstancesResponseTypeinstancesArray = Array of TInstance;
  
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
    TApplication
    --------------------------------------------------------------------}
  
  TApplication = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fid : String;
    FdispatchRules : TApplicationTypedispatchRulesArray;
    FauthDomain : String;
    Flocation : String;
    FcodeBucket : String;
    FdefaultCookieExpiration : String;
    FdefaultHostname : String;
    FdefaultBucket : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdispatchRules(AIndex : Integer; const AValue : TApplicationTypedispatchRulesArray); virtual;
    Procedure SetauthDomain(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcodeBucket(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdefaultCookieExpiration(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdefaultHostname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdefaultBucket(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property id : String Index 8 Read Fid Write Setid;
    Property dispatchRules : TApplicationTypedispatchRulesArray Index 16 Read FdispatchRules Write SetdispatchRules;
    Property authDomain : String Index 24 Read FauthDomain Write SetauthDomain;
    Property location : String Index 32 Read Flocation Write Setlocation;
    Property codeBucket : String Index 40 Read FcodeBucket Write SetcodeBucket;
    Property defaultCookieExpiration : String Index 48 Read FdefaultCookieExpiration Write SetdefaultCookieExpiration;
    Property defaultHostname : String Index 56 Read FdefaultHostname Write SetdefaultHostname;
    Property defaultBucket : String Index 64 Read FdefaultBucket Write SetdefaultBucket;
  end;
  TApplicationClass = Class of TApplication;
  
  { --------------------------------------------------------------------
    TUrlDispatchRule
    --------------------------------------------------------------------}
  
  TUrlDispatchRule = Class(TGoogleBaseObject)
  Private
    Fdomain : String;
    Fpath : String;
    Fservice : String;
  Protected
    //Property setters
    Procedure Setdomain(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpath(AIndex : Integer; const AValue : String); virtual;
    Procedure Setservice(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property domain : String Index 0 Read Fdomain Write Setdomain;
    Property path : String Index 8 Read Fpath Write Setpath;
    Property service : String Index 16 Read Fservice Write Setservice;
  end;
  TUrlDispatchRuleClass = Class of TUrlDispatchRule;
  
  { --------------------------------------------------------------------
    TVersionTypebetaSettings
    --------------------------------------------------------------------}
  
  TVersionTypebetaSettings = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TVersionTypebetaSettingsClass = Class of TVersionTypebetaSettings;
  
  { --------------------------------------------------------------------
    TVersionTypeenvVariables
    --------------------------------------------------------------------}
  
  TVersionTypeenvVariables = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TVersionTypeenvVariablesClass = Class of TVersionTypeenvVariables;
  
  { --------------------------------------------------------------------
    TVersion
    --------------------------------------------------------------------}
  
  TVersion = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fid : String;
    FautomaticScaling : TAutomaticScaling;
    FbasicScaling : TBasicScaling;
    FmanualScaling : TManualScaling;
    FinboundServices : TStringArray;
    FinstanceClass : String;
    Fnetwork : TNetwork;
    Fresources : TResources;
    Fruntime : String;
    Fthreadsafe : boolean;
    Fvm : boolean;
    FbetaSettings : TVersionTypebetaSettings;
    Fenv : String;
    FservingStatus : String;
    Fdeployer : String;
    FcreationTime : String;
    FdiskUsageBytes : String;
    Fhandlers : TVersionTypehandlersArray;
    FerrorHandlers : TVersionTypeerrorHandlersArray;
    Flibraries : TVersionTypelibrariesArray;
    FapiConfig : TApiConfigHandler;
    FenvVariables : TVersionTypeenvVariables;
    FdefaultExpiration : String;
    FhealthCheck : THealthCheck;
    FnobuildFilesRegex : String;
    Fdeployment : TDeployment;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetautomaticScaling(AIndex : Integer; const AValue : TAutomaticScaling); virtual;
    Procedure SetbasicScaling(AIndex : Integer; const AValue : TBasicScaling); virtual;
    Procedure SetmanualScaling(AIndex : Integer; const AValue : TManualScaling); virtual;
    Procedure SetinboundServices(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetinstanceClass(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnetwork(AIndex : Integer; const AValue : TNetwork); virtual;
    Procedure Setresources(AIndex : Integer; const AValue : TResources); virtual;
    Procedure Setruntime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setthreadsafe(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setvm(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetbetaSettings(AIndex : Integer; const AValue : TVersionTypebetaSettings); virtual;
    Procedure Setenv(AIndex : Integer; const AValue : String); virtual;
    Procedure SetservingStatus(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdeployer(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreationTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdiskUsageBytes(AIndex : Integer; const AValue : String); virtual;
    Procedure Sethandlers(AIndex : Integer; const AValue : TVersionTypehandlersArray); virtual;
    Procedure SeterrorHandlers(AIndex : Integer; const AValue : TVersionTypeerrorHandlersArray); virtual;
    Procedure Setlibraries(AIndex : Integer; const AValue : TVersionTypelibrariesArray); virtual;
    Procedure SetapiConfig(AIndex : Integer; const AValue : TApiConfigHandler); virtual;
    Procedure SetenvVariables(AIndex : Integer; const AValue : TVersionTypeenvVariables); virtual;
    Procedure SetdefaultExpiration(AIndex : Integer; const AValue : String); virtual;
    Procedure SethealthCheck(AIndex : Integer; const AValue : THealthCheck); virtual;
    Procedure SetnobuildFilesRegex(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdeployment(AIndex : Integer; const AValue : TDeployment); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property id : String Index 8 Read Fid Write Setid;
    Property automaticScaling : TAutomaticScaling Index 16 Read FautomaticScaling Write SetautomaticScaling;
    Property basicScaling : TBasicScaling Index 24 Read FbasicScaling Write SetbasicScaling;
    Property manualScaling : TManualScaling Index 32 Read FmanualScaling Write SetmanualScaling;
    Property inboundServices : TStringArray Index 40 Read FinboundServices Write SetinboundServices;
    Property instanceClass : String Index 48 Read FinstanceClass Write SetinstanceClass;
    Property network : TNetwork Index 56 Read Fnetwork Write Setnetwork;
    Property resources : TResources Index 64 Read Fresources Write Setresources;
    Property runtime : String Index 72 Read Fruntime Write Setruntime;
    Property threadsafe : boolean Index 80 Read Fthreadsafe Write Setthreadsafe;
    Property vm : boolean Index 88 Read Fvm Write Setvm;
    Property betaSettings : TVersionTypebetaSettings Index 96 Read FbetaSettings Write SetbetaSettings;
    Property env : String Index 104 Read Fenv Write Setenv;
    Property servingStatus : String Index 112 Read FservingStatus Write SetservingStatus;
    Property deployer : String Index 120 Read Fdeployer Write Setdeployer;
    Property creationTime : String Index 128 Read FcreationTime Write SetcreationTime;
    Property diskUsageBytes : String Index 136 Read FdiskUsageBytes Write SetdiskUsageBytes;
    Property handlers : TVersionTypehandlersArray Index 144 Read Fhandlers Write Sethandlers;
    Property errorHandlers : TVersionTypeerrorHandlersArray Index 152 Read FerrorHandlers Write SeterrorHandlers;
    Property libraries : TVersionTypelibrariesArray Index 160 Read Flibraries Write Setlibraries;
    Property apiConfig : TApiConfigHandler Index 168 Read FapiConfig Write SetapiConfig;
    Property envVariables : TVersionTypeenvVariables Index 176 Read FenvVariables Write SetenvVariables;
    Property defaultExpiration : String Index 184 Read FdefaultExpiration Write SetdefaultExpiration;
    Property healthCheck : THealthCheck Index 192 Read FhealthCheck Write SethealthCheck;
    Property nobuildFilesRegex : String Index 200 Read FnobuildFilesRegex Write SetnobuildFilesRegex;
    Property deployment : TDeployment Index 208 Read Fdeployment Write Setdeployment;
  end;
  TVersionClass = Class of TVersion;
  
  { --------------------------------------------------------------------
    TAutomaticScaling
    --------------------------------------------------------------------}
  
  TAutomaticScaling = Class(TGoogleBaseObject)
  Private
    FcoolDownPeriod : String;
    FcpuUtilization : TCpuUtilization;
    FmaxConcurrentRequests : integer;
    FmaxIdleInstances : integer;
    FmaxTotalInstances : integer;
    FmaxPendingLatency : String;
    FminIdleInstances : integer;
    FminTotalInstances : integer;
    FminPendingLatency : String;
    FrequestUtilization : TRequestUtilization;
    FdiskUtilization : TDiskUtilization;
    FnetworkUtilization : TNetworkUtilization;
  Protected
    //Property setters
    Procedure SetcoolDownPeriod(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcpuUtilization(AIndex : Integer; const AValue : TCpuUtilization); virtual;
    Procedure SetmaxConcurrentRequests(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetmaxIdleInstances(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetmaxTotalInstances(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetmaxPendingLatency(AIndex : Integer; const AValue : String); virtual;
    Procedure SetminIdleInstances(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetminTotalInstances(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetminPendingLatency(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrequestUtilization(AIndex : Integer; const AValue : TRequestUtilization); virtual;
    Procedure SetdiskUtilization(AIndex : Integer; const AValue : TDiskUtilization); virtual;
    Procedure SetnetworkUtilization(AIndex : Integer; const AValue : TNetworkUtilization); virtual;
  Public
  Published
    Property coolDownPeriod : String Index 0 Read FcoolDownPeriod Write SetcoolDownPeriod;
    Property cpuUtilization : TCpuUtilization Index 8 Read FcpuUtilization Write SetcpuUtilization;
    Property maxConcurrentRequests : integer Index 16 Read FmaxConcurrentRequests Write SetmaxConcurrentRequests;
    Property maxIdleInstances : integer Index 24 Read FmaxIdleInstances Write SetmaxIdleInstances;
    Property maxTotalInstances : integer Index 32 Read FmaxTotalInstances Write SetmaxTotalInstances;
    Property maxPendingLatency : String Index 40 Read FmaxPendingLatency Write SetmaxPendingLatency;
    Property minIdleInstances : integer Index 48 Read FminIdleInstances Write SetminIdleInstances;
    Property minTotalInstances : integer Index 56 Read FminTotalInstances Write SetminTotalInstances;
    Property minPendingLatency : String Index 64 Read FminPendingLatency Write SetminPendingLatency;
    Property requestUtilization : TRequestUtilization Index 72 Read FrequestUtilization Write SetrequestUtilization;
    Property diskUtilization : TDiskUtilization Index 80 Read FdiskUtilization Write SetdiskUtilization;
    Property networkUtilization : TNetworkUtilization Index 88 Read FnetworkUtilization Write SetnetworkUtilization;
  end;
  TAutomaticScalingClass = Class of TAutomaticScaling;
  
  { --------------------------------------------------------------------
    TCpuUtilization
    --------------------------------------------------------------------}
  
  TCpuUtilization = Class(TGoogleBaseObject)
  Private
    FaggregationWindowLength : String;
    FtargetUtilization : double;
  Protected
    //Property setters
    Procedure SetaggregationWindowLength(AIndex : Integer; const AValue : String); virtual;
    Procedure SettargetUtilization(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property aggregationWindowLength : String Index 0 Read FaggregationWindowLength Write SetaggregationWindowLength;
    Property targetUtilization : double Index 8 Read FtargetUtilization Write SettargetUtilization;
  end;
  TCpuUtilizationClass = Class of TCpuUtilization;
  
  { --------------------------------------------------------------------
    TRequestUtilization
    --------------------------------------------------------------------}
  
  TRequestUtilization = Class(TGoogleBaseObject)
  Private
    FtargetRequestCountPerSec : integer;
    FtargetConcurrentRequests : integer;
  Protected
    //Property setters
    Procedure SettargetRequestCountPerSec(AIndex : Integer; const AValue : integer); virtual;
    Procedure SettargetConcurrentRequests(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property targetRequestCountPerSec : integer Index 0 Read FtargetRequestCountPerSec Write SettargetRequestCountPerSec;
    Property targetConcurrentRequests : integer Index 8 Read FtargetConcurrentRequests Write SettargetConcurrentRequests;
  end;
  TRequestUtilizationClass = Class of TRequestUtilization;
  
  { --------------------------------------------------------------------
    TDiskUtilization
    --------------------------------------------------------------------}
  
  TDiskUtilization = Class(TGoogleBaseObject)
  Private
    FtargetWriteBytesPerSec : integer;
    FtargetWriteOpsPerSec : integer;
    FtargetReadBytesPerSec : integer;
    FtargetReadOpsPerSec : integer;
  Protected
    //Property setters
    Procedure SettargetWriteBytesPerSec(AIndex : Integer; const AValue : integer); virtual;
    Procedure SettargetWriteOpsPerSec(AIndex : Integer; const AValue : integer); virtual;
    Procedure SettargetReadBytesPerSec(AIndex : Integer; const AValue : integer); virtual;
    Procedure SettargetReadOpsPerSec(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property targetWriteBytesPerSec : integer Index 0 Read FtargetWriteBytesPerSec Write SettargetWriteBytesPerSec;
    Property targetWriteOpsPerSec : integer Index 8 Read FtargetWriteOpsPerSec Write SettargetWriteOpsPerSec;
    Property targetReadBytesPerSec : integer Index 16 Read FtargetReadBytesPerSec Write SettargetReadBytesPerSec;
    Property targetReadOpsPerSec : integer Index 24 Read FtargetReadOpsPerSec Write SettargetReadOpsPerSec;
  end;
  TDiskUtilizationClass = Class of TDiskUtilization;
  
  { --------------------------------------------------------------------
    TNetworkUtilization
    --------------------------------------------------------------------}
  
  TNetworkUtilization = Class(TGoogleBaseObject)
  Private
    FtargetSentBytesPerSec : integer;
    FtargetSentPacketsPerSec : integer;
    FtargetReceivedBytesPerSec : integer;
    FtargetReceivedPacketsPerSec : integer;
  Protected
    //Property setters
    Procedure SettargetSentBytesPerSec(AIndex : Integer; const AValue : integer); virtual;
    Procedure SettargetSentPacketsPerSec(AIndex : Integer; const AValue : integer); virtual;
    Procedure SettargetReceivedBytesPerSec(AIndex : Integer; const AValue : integer); virtual;
    Procedure SettargetReceivedPacketsPerSec(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property targetSentBytesPerSec : integer Index 0 Read FtargetSentBytesPerSec Write SettargetSentBytesPerSec;
    Property targetSentPacketsPerSec : integer Index 8 Read FtargetSentPacketsPerSec Write SettargetSentPacketsPerSec;
    Property targetReceivedBytesPerSec : integer Index 16 Read FtargetReceivedBytesPerSec Write SettargetReceivedBytesPerSec;
    Property targetReceivedPacketsPerSec : integer Index 24 Read FtargetReceivedPacketsPerSec Write SettargetReceivedPacketsPerSec;
  end;
  TNetworkUtilizationClass = Class of TNetworkUtilization;
  
  { --------------------------------------------------------------------
    TBasicScaling
    --------------------------------------------------------------------}
  
  TBasicScaling = Class(TGoogleBaseObject)
  Private
    FidleTimeout : String;
    FmaxInstances : integer;
  Protected
    //Property setters
    Procedure SetidleTimeout(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmaxInstances(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property idleTimeout : String Index 0 Read FidleTimeout Write SetidleTimeout;
    Property maxInstances : integer Index 8 Read FmaxInstances Write SetmaxInstances;
  end;
  TBasicScalingClass = Class of TBasicScaling;
  
  { --------------------------------------------------------------------
    TManualScaling
    --------------------------------------------------------------------}
  
  TManualScaling = Class(TGoogleBaseObject)
  Private
    Finstances : integer;
  Protected
    //Property setters
    Procedure Setinstances(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property instances : integer Index 0 Read Finstances Write Setinstances;
  end;
  TManualScalingClass = Class of TManualScaling;
  
  { --------------------------------------------------------------------
    TNetwork
    --------------------------------------------------------------------}
  
  TNetwork = Class(TGoogleBaseObject)
  Private
    FforwardedPorts : TStringArray;
    FinstanceTag : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetforwardedPorts(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetinstanceTag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property forwardedPorts : TStringArray Index 0 Read FforwardedPorts Write SetforwardedPorts;
    Property instanceTag : String Index 8 Read FinstanceTag Write SetinstanceTag;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TNetworkClass = Class of TNetwork;
  
  { --------------------------------------------------------------------
    TResources
    --------------------------------------------------------------------}
  
  TResources = Class(TGoogleBaseObject)
  Private
    Fcpu : double;
    FdiskGb : double;
    FmemoryGb : double;
  Protected
    //Property setters
    Procedure Setcpu(AIndex : Integer; const AValue : double); virtual;
    Procedure SetdiskGb(AIndex : Integer; const AValue : double); virtual;
    Procedure SetmemoryGb(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property cpu : double Index 0 Read Fcpu Write Setcpu;
    Property diskGb : double Index 8 Read FdiskGb Write SetdiskGb;
    Property memoryGb : double Index 16 Read FmemoryGb Write SetmemoryGb;
  end;
  TResourcesClass = Class of TResources;
  
  { --------------------------------------------------------------------
    TUrlMap
    --------------------------------------------------------------------}
  
  TUrlMap = Class(TGoogleBaseObject)
  Private
    FurlRegex : String;
    FstaticFiles : TStaticFilesHandler;
    Fscript : TScriptHandler;
    FapiEndpoint : TApiEndpointHandler;
    FsecurityLevel : String;
    Flogin : String;
    FauthFailAction : String;
    FredirectHttpResponseCode : String;
  Protected
    //Property setters
    Procedure SeturlRegex(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstaticFiles(AIndex : Integer; const AValue : TStaticFilesHandler); virtual;
    Procedure Setscript(AIndex : Integer; const AValue : TScriptHandler); virtual;
    Procedure SetapiEndpoint(AIndex : Integer; const AValue : TApiEndpointHandler); virtual;
    Procedure SetsecurityLevel(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlogin(AIndex : Integer; const AValue : String); virtual;
    Procedure SetauthFailAction(AIndex : Integer; const AValue : String); virtual;
    Procedure SetredirectHttpResponseCode(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property urlRegex : String Index 0 Read FurlRegex Write SeturlRegex;
    Property staticFiles : TStaticFilesHandler Index 8 Read FstaticFiles Write SetstaticFiles;
    Property script : TScriptHandler Index 16 Read Fscript Write Setscript;
    Property apiEndpoint : TApiEndpointHandler Index 24 Read FapiEndpoint Write SetapiEndpoint;
    Property securityLevel : String Index 32 Read FsecurityLevel Write SetsecurityLevel;
    Property login : String Index 40 Read Flogin Write Setlogin;
    Property authFailAction : String Index 48 Read FauthFailAction Write SetauthFailAction;
    Property redirectHttpResponseCode : String Index 56 Read FredirectHttpResponseCode Write SetredirectHttpResponseCode;
  end;
  TUrlMapClass = Class of TUrlMap;
  
  { --------------------------------------------------------------------
    TStaticFilesHandlerTypehttpHeaders
    --------------------------------------------------------------------}
  
  TStaticFilesHandlerTypehttpHeaders = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TStaticFilesHandlerTypehttpHeadersClass = Class of TStaticFilesHandlerTypehttpHeaders;
  
  { --------------------------------------------------------------------
    TStaticFilesHandler
    --------------------------------------------------------------------}
  
  TStaticFilesHandler = Class(TGoogleBaseObject)
  Private
    Fpath : String;
    FuploadPathRegex : String;
    FhttpHeaders : TStaticFilesHandlerTypehttpHeaders;
    FmimeType : String;
    Fexpiration : String;
    FrequireMatchingFile : boolean;
    FapplicationReadable : boolean;
  Protected
    //Property setters
    Procedure Setpath(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuploadPathRegex(AIndex : Integer; const AValue : String); virtual;
    Procedure SethttpHeaders(AIndex : Integer; const AValue : TStaticFilesHandlerTypehttpHeaders); virtual;
    Procedure SetmimeType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setexpiration(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrequireMatchingFile(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetapplicationReadable(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property path : String Index 0 Read Fpath Write Setpath;
    Property uploadPathRegex : String Index 8 Read FuploadPathRegex Write SetuploadPathRegex;
    Property httpHeaders : TStaticFilesHandlerTypehttpHeaders Index 16 Read FhttpHeaders Write SethttpHeaders;
    Property mimeType : String Index 24 Read FmimeType Write SetmimeType;
    Property expiration : String Index 32 Read Fexpiration Write Setexpiration;
    Property requireMatchingFile : boolean Index 40 Read FrequireMatchingFile Write SetrequireMatchingFile;
    Property applicationReadable : boolean Index 48 Read FapplicationReadable Write SetapplicationReadable;
  end;
  TStaticFilesHandlerClass = Class of TStaticFilesHandler;
  
  { --------------------------------------------------------------------
    TScriptHandler
    --------------------------------------------------------------------}
  
  TScriptHandler = Class(TGoogleBaseObject)
  Private
    FscriptPath : String;
  Protected
    //Property setters
    Procedure SetscriptPath(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property scriptPath : String Index 0 Read FscriptPath Write SetscriptPath;
  end;
  TScriptHandlerClass = Class of TScriptHandler;
  
  { --------------------------------------------------------------------
    TApiEndpointHandler
    --------------------------------------------------------------------}
  
  TApiEndpointHandler = Class(TGoogleBaseObject)
  Private
    FscriptPath : String;
  Protected
    //Property setters
    Procedure SetscriptPath(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property scriptPath : String Index 0 Read FscriptPath Write SetscriptPath;
  end;
  TApiEndpointHandlerClass = Class of TApiEndpointHandler;
  
  { --------------------------------------------------------------------
    TErrorHandler
    --------------------------------------------------------------------}
  
  TErrorHandler = Class(TGoogleBaseObject)
  Private
    FerrorCode : String;
    FstaticFile : String;
    FmimeType : String;
  Protected
    //Property setters
    Procedure SeterrorCode(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstaticFile(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmimeType(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property errorCode : String Index 0 Read FerrorCode Write SeterrorCode;
    Property staticFile : String Index 8 Read FstaticFile Write SetstaticFile;
    Property mimeType : String Index 16 Read FmimeType Write SetmimeType;
  end;
  TErrorHandlerClass = Class of TErrorHandler;
  
  { --------------------------------------------------------------------
    TLibrary
    --------------------------------------------------------------------}
  
  TLibrary = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fversion : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property version : String Index 8 Read Fversion Write Setversion;
  end;
  TLibraryClass = Class of TLibrary;
  
  { --------------------------------------------------------------------
    TApiConfigHandler
    --------------------------------------------------------------------}
  
  TApiConfigHandler = Class(TGoogleBaseObject)
  Private
    FauthFailAction : String;
    Flogin : String;
    Fscript : String;
    FsecurityLevel : String;
    Furl : String;
  Protected
    //Property setters
    Procedure SetauthFailAction(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlogin(AIndex : Integer; const AValue : String); virtual;
    Procedure Setscript(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsecurityLevel(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property authFailAction : String Index 0 Read FauthFailAction Write SetauthFailAction;
    Property login : String Index 8 Read Flogin Write Setlogin;
    Property script : String Index 16 Read Fscript Write Setscript;
    Property securityLevel : String Index 24 Read FsecurityLevel Write SetsecurityLevel;
    Property url : String Index 32 Read Furl Write Seturl;
  end;
  TApiConfigHandlerClass = Class of TApiConfigHandler;
  
  { --------------------------------------------------------------------
    THealthCheck
    --------------------------------------------------------------------}
  
  THealthCheck = Class(TGoogleBaseObject)
  Private
    FdisableHealthCheck : boolean;
    Fhost : String;
    FhealthyThreshold : integer;
    FunhealthyThreshold : integer;
    FrestartThreshold : integer;
    FcheckInterval : String;
    Ftimeout : String;
  Protected
    //Property setters
    Procedure SetdisableHealthCheck(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Sethost(AIndex : Integer; const AValue : String); virtual;
    Procedure SethealthyThreshold(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetunhealthyThreshold(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetrestartThreshold(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetcheckInterval(AIndex : Integer; const AValue : String); virtual;
    Procedure Settimeout(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property disableHealthCheck : boolean Index 0 Read FdisableHealthCheck Write SetdisableHealthCheck;
    Property host : String Index 8 Read Fhost Write Sethost;
    Property healthyThreshold : integer Index 16 Read FhealthyThreshold Write SethealthyThreshold;
    Property unhealthyThreshold : integer Index 24 Read FunhealthyThreshold Write SetunhealthyThreshold;
    Property restartThreshold : integer Index 32 Read FrestartThreshold Write SetrestartThreshold;
    Property checkInterval : String Index 40 Read FcheckInterval Write SetcheckInterval;
    Property timeout : String Index 48 Read Ftimeout Write Settimeout;
  end;
  THealthCheckClass = Class of THealthCheck;
  
  { --------------------------------------------------------------------
    TDeploymentTypefiles
    --------------------------------------------------------------------}
  
  TDeploymentTypefiles = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TDeploymentTypefilesClass = Class of TDeploymentTypefiles;
  
  { --------------------------------------------------------------------
    TDeployment
    --------------------------------------------------------------------}
  
  TDeployment = Class(TGoogleBaseObject)
  Private
    Ffiles : TDeploymentTypefiles;
    Fcontainer : TContainerInfo;
    FsourceReferences : TDeploymentTypesourceReferencesArray;
  Protected
    //Property setters
    Procedure Setfiles(AIndex : Integer; const AValue : TDeploymentTypefiles); virtual;
    Procedure Setcontainer(AIndex : Integer; const AValue : TContainerInfo); virtual;
    Procedure SetsourceReferences(AIndex : Integer; const AValue : TDeploymentTypesourceReferencesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property files : TDeploymentTypefiles Index 0 Read Ffiles Write Setfiles;
    Property container : TContainerInfo Index 8 Read Fcontainer Write Setcontainer;
    Property sourceReferences : TDeploymentTypesourceReferencesArray Index 16 Read FsourceReferences Write SetsourceReferences;
  end;
  TDeploymentClass = Class of TDeployment;
  
  { --------------------------------------------------------------------
    TFileInfo
    --------------------------------------------------------------------}
  
  TFileInfo = Class(TGoogleBaseObject)
  Private
    FsourceUrl : String;
    Fsha1Sum : String;
    FmimeType : String;
  Protected
    //Property setters
    Procedure SetsourceUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsha1Sum(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmimeType(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property sourceUrl : String Index 0 Read FsourceUrl Write SetsourceUrl;
    Property sha1Sum : String Index 8 Read Fsha1Sum Write Setsha1Sum;
    Property mimeType : String Index 16 Read FmimeType Write SetmimeType;
  end;
  TFileInfoClass = Class of TFileInfo;
  
  { --------------------------------------------------------------------
    TContainerInfo
    --------------------------------------------------------------------}
  
  TContainerInfo = Class(TGoogleBaseObject)
  Private
    Fimage : String;
  Protected
    //Property setters
    Procedure Setimage(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property image : String Index 0 Read Fimage Write Setimage;
  end;
  TContainerInfoClass = Class of TContainerInfo;
  
  { --------------------------------------------------------------------
    TSourceReference
    --------------------------------------------------------------------}
  
  TSourceReference = Class(TGoogleBaseObject)
  Private
    Frepository : String;
    FrevisionId : String;
  Protected
    //Property setters
    Procedure Setrepository(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrevisionId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property repository : String Index 0 Read Frepository Write Setrepository;
    Property revisionId : String Index 8 Read FrevisionId Write SetrevisionId;
  end;
  TSourceReferenceClass = Class of TSourceReference;
  
  { --------------------------------------------------------------------
    TListVersionsResponse
    --------------------------------------------------------------------}
  
  TListVersionsResponse = Class(TGoogleBaseObject)
  Private
    Fversions : TListVersionsResponseTypeversionsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setversions(AIndex : Integer; const AValue : TListVersionsResponseTypeversionsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property versions : TListVersionsResponseTypeversionsArray Index 0 Read Fversions Write Setversions;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListVersionsResponseClass = Class of TListVersionsResponse;
  
  { --------------------------------------------------------------------
    TService
    --------------------------------------------------------------------}
  
  TService = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fid : String;
    Fsplit : TTrafficSplit;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsplit(AIndex : Integer; const AValue : TTrafficSplit); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property id : String Index 8 Read Fid Write Setid;
    Property split : TTrafficSplit Index 16 Read Fsplit Write Setsplit;
  end;
  TServiceClass = Class of TService;
  
  { --------------------------------------------------------------------
    TTrafficSplitTypeallocations
    --------------------------------------------------------------------}
  
  TTrafficSplitTypeallocations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TTrafficSplitTypeallocationsClass = Class of TTrafficSplitTypeallocations;
  
  { --------------------------------------------------------------------
    TTrafficSplit
    --------------------------------------------------------------------}
  
  TTrafficSplit = Class(TGoogleBaseObject)
  Private
    FshardBy : String;
    Fallocations : TTrafficSplitTypeallocations;
  Protected
    //Property setters
    Procedure SetshardBy(AIndex : Integer; const AValue : String); virtual;
    Procedure Setallocations(AIndex : Integer; const AValue : TTrafficSplitTypeallocations); virtual;
  Public
  Published
    Property shardBy : String Index 0 Read FshardBy Write SetshardBy;
    Property allocations : TTrafficSplitTypeallocations Index 8 Read Fallocations Write Setallocations;
  end;
  TTrafficSplitClass = Class of TTrafficSplit;
  
  { --------------------------------------------------------------------
    TListServicesResponse
    --------------------------------------------------------------------}
  
  TListServicesResponse = Class(TGoogleBaseObject)
  Private
    Fservices : TListServicesResponseTypeservicesArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setservices(AIndex : Integer; const AValue : TListServicesResponseTypeservicesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property services : TListServicesResponseTypeservicesArray Index 0 Read Fservices Write Setservices;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListServicesResponseClass = Class of TListServicesResponse;
  
  { --------------------------------------------------------------------
    TListInstancesResponse
    --------------------------------------------------------------------}
  
  TListInstancesResponse = Class(TGoogleBaseObject)
  Private
    Finstances : TListInstancesResponseTypeinstancesArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setinstances(AIndex : Integer; const AValue : TListInstancesResponseTypeinstancesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property instances : TListInstancesResponseTypeinstancesArray Index 0 Read Finstances Write Setinstances;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListInstancesResponseClass = Class of TListInstancesResponse;
  
  { --------------------------------------------------------------------
    TInstance
    --------------------------------------------------------------------}
  
  TInstance = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fid : String;
    FappEngineRelease : String;
    Favailability : String;
    FvmName : String;
    FvmZoneName : String;
    FvmId : String;
    FstartTimestamp : String;
    Frequests : integer;
    Ferrors : integer;
    Fqps : integer;
    FaverageLatency : integer;
    FmemoryUsage : String;
    FvmStatus : String;
    FvmUnlocked : boolean;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetappEngineRelease(AIndex : Integer; const AValue : String); virtual;
    Procedure Setavailability(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvmName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvmZoneName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvmId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartTimestamp(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrequests(AIndex : Integer; const AValue : integer); virtual;
    Procedure Seterrors(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setqps(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetaverageLatency(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetmemoryUsage(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvmStatus(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvmUnlocked(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property id : String Index 8 Read Fid Write Setid;
    Property appEngineRelease : String Index 16 Read FappEngineRelease Write SetappEngineRelease;
    Property availability : String Index 24 Read Favailability Write Setavailability;
    Property vmName : String Index 32 Read FvmName Write SetvmName;
    Property vmZoneName : String Index 40 Read FvmZoneName Write SetvmZoneName;
    Property vmId : String Index 48 Read FvmId Write SetvmId;
    Property startTimestamp : String Index 56 Read FstartTimestamp Write SetstartTimestamp;
    Property requests : integer Index 64 Read Frequests Write Setrequests;
    Property errors : integer Index 72 Read Ferrors Write Seterrors;
    Property qps : integer Index 80 Read Fqps Write Setqps;
    Property averageLatency : integer Index 88 Read FaverageLatency Write SetaverageLatency;
    Property memoryUsage : String Index 96 Read FmemoryUsage Write SetmemoryUsage;
    Property vmStatus : String Index 104 Read FvmStatus Write SetvmStatus;
    Property vmUnlocked : boolean Index 112 Read FvmUnlocked Write SetvmUnlocked;
  end;
  TInstanceClass = Class of TInstance;
  
  { --------------------------------------------------------------------
    TOperationMetadata
    --------------------------------------------------------------------}
  
  TOperationMetadata = Class(TGoogleBaseObject)
  Private
    FoperationType : String;
    FinsertTime : String;
    FendTime : String;
    Fuser : String;
    Ftarget : String;
    Fmethod : String;
  Protected
    //Property setters
    Procedure SetoperationType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinsertTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setuser(AIndex : Integer; const AValue : String); virtual;
    Procedure Settarget(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmethod(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property operationType : String Index 0 Read FoperationType Write SetoperationType;
    Property insertTime : String Index 8 Read FinsertTime Write SetinsertTime;
    Property endTime : String Index 16 Read FendTime Write SetendTime;
    Property user : String Index 24 Read Fuser Write Setuser;
    Property target : String Index 32 Read Ftarget Write Settarget;
    Property method : String Index 40 Read Fmethod Write Setmethod;
  end;
  TOperationMetadataClass = Class of TOperationMetadata;
  
  { --------------------------------------------------------------------
    TOperationMetadataV1Beta5
    --------------------------------------------------------------------}
  
  TOperationMetadataV1Beta5 = Class(TGoogleBaseObject)
  Private
    Fmethod : String;
    FinsertTime : String;
    FendTime : String;
    Fuser : String;
    Ftarget : String;
  Protected
    //Property setters
    Procedure Setmethod(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinsertTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setuser(AIndex : Integer; const AValue : String); virtual;
    Procedure Settarget(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property method : String Index 0 Read Fmethod Write Setmethod;
    Property insertTime : String Index 8 Read FinsertTime Write SetinsertTime;
    Property endTime : String Index 16 Read FendTime Write SetendTime;
    Property user : String Index 24 Read Fuser Write Setuser;
    Property target : String Index 32 Read Ftarget Write Settarget;
  end;
  TOperationMetadataV1Beta5Class = Class of TOperationMetadataV1Beta5;
  
  { --------------------------------------------------------------------
    TAppsOperationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAppsOperationsResource, method List
  
  TAppsOperationsListOptions = Record
    filter : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  TAppsOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(appsId: string; operationsId: string) : TOperation;
    Function List(appsId: string; AQuery : string  = '') : TListOperationsResponse;
    Function List(appsId: string; AQuery : TAppsOperationslistOptions) : TListOperationsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TAppsServicesVersionsInstancesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAppsServicesVersionsInstancesResource, method List
  
  TAppsServicesVersionsInstancesListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  TAppsServicesVersionsInstancesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(appsId: string; servicesId: string; versionsId: string; AQuery : string  = '') : TListInstancesResponse;
    Function List(appsId: string; servicesId: string; versionsId: string; AQuery : TAppsServicesVersionsInstanceslistOptions) : TListInstancesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TAppsServicesVersionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAppsServicesVersionsResource, method Get
  
  TAppsServicesVersionsGetOptions = Record
    view : String;
  end;
  
  
  //Optional query Options for TAppsServicesVersionsResource, method List
  
  TAppsServicesVersionsListOptions = Record
    view : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TAppsServicesVersionsResource, method Patch
  
  TAppsServicesVersionsPatchOptions = Record
    mask : String;
  end;
  
  TAppsServicesVersionsResource = Class(TGoogleResource)
  Private
    FInstancesInstance : TAppsServicesVersionsInstancesResource;
    Function GetInstancesInstance : TAppsServicesVersionsInstancesResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(appsId: string; servicesId: string; aVersion : TVersion) : TOperation;overload;
    Function Delete(appsId: string; servicesId: string; versionsId: string) : TOperation;
    Function Get(appsId: string; servicesId: string; versionsId: string; AQuery : string  = '') : TVersion;
    Function Get(appsId: string; servicesId: string; versionsId: string; AQuery : TAppsServicesVersionsgetOptions) : TVersion;
    Function List(appsId: string; servicesId: string; AQuery : string  = '') : TListVersionsResponse;
    Function List(appsId: string; servicesId: string; AQuery : TAppsServicesVersionslistOptions) : TListVersionsResponse;
    Function Patch(appsId: string; servicesId: string; versionsId: string; aVersion : TVersion; AQuery : string  = '') : TOperation;
    Function Patch(appsId: string; servicesId: string; versionsId: string; aVersion : TVersion; AQuery : TAppsServicesVersionspatchOptions) : TOperation;
    Function CreateInstancesResource(AOwner : TComponent) : TAppsServicesVersionsInstancesResource;virtual;overload;
    Function CreateInstancesResource : TAppsServicesVersionsInstancesResource;virtual;overload;
    Property InstancesResource : TAppsServicesVersionsInstancesResource Read GetInstancesInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAppsServicesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAppsServicesResource, method List
  
  TAppsServicesListOptions = Record
    pageSize : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TAppsServicesResource, method Patch
  
  TAppsServicesPatchOptions = Record
    mask : String;
    migrateTraffic : boolean;
  end;
  
  TAppsServicesResource = Class(TGoogleResource)
  Private
    FVersionsInstancesInstance : TAppsServicesVersionsInstancesResource;
    FVersionsInstance : TAppsServicesVersionsResource;
    Function GetVersionsInstancesInstance : TAppsServicesVersionsInstancesResource;virtual;
    Function GetVersionsInstance : TAppsServicesVersionsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(appsId: string; servicesId: string) : TOperation;
    Function Get(appsId: string; servicesId: string) : TService;
    Function List(appsId: string; AQuery : string  = '') : TListServicesResponse;
    Function List(appsId: string; AQuery : TAppsServiceslistOptions) : TListServicesResponse;
    Function Patch(appsId: string; servicesId: string; aService : TService; AQuery : string  = '') : TOperation;
    Function Patch(appsId: string; servicesId: string; aService : TService; AQuery : TAppsServicespatchOptions) : TOperation;
    Function CreateVersionsInstancesResource(AOwner : TComponent) : TAppsServicesVersionsInstancesResource;virtual;overload;
    Function CreateVersionsInstancesResource : TAppsServicesVersionsInstancesResource;virtual;overload;
    Function CreateVersionsResource(AOwner : TComponent) : TAppsServicesVersionsResource;virtual;overload;
    Function CreateVersionsResource : TAppsServicesVersionsResource;virtual;overload;
    Property VersionsInstancesResource : TAppsServicesVersionsInstancesResource Read GetVersionsInstancesInstance;
    Property VersionsResource : TAppsServicesVersionsResource Read GetVersionsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAppsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAppsResource, method Get
  
  TAppsGetOptions = Record
    ensureResourcesExist : boolean;
  end;
  
  TAppsResource = Class(TGoogleResource)
  Private
    FOperationsInstance : TAppsOperationsResource;
    FServicesVersionsInstancesInstance : TAppsServicesVersionsInstancesResource;
    FServicesVersionsInstance : TAppsServicesVersionsResource;
    FServicesInstance : TAppsServicesResource;
    Function GetOperationsInstance : TAppsOperationsResource;virtual;
    Function GetServicesVersionsInstancesInstance : TAppsServicesVersionsInstancesResource;virtual;
    Function GetServicesVersionsInstance : TAppsServicesVersionsResource;virtual;
    Function GetServicesInstance : TAppsServicesResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(appsId: string; AQuery : string  = '') : TApplication;
    Function Get(appsId: string; AQuery : TAppsgetOptions) : TApplication;
    Function CreateOperationsResource(AOwner : TComponent) : TAppsOperationsResource;virtual;overload;
    Function CreateOperationsResource : TAppsOperationsResource;virtual;overload;
    Function CreateServicesVersionsInstancesResource(AOwner : TComponent) : TAppsServicesVersionsInstancesResource;virtual;overload;
    Function CreateServicesVersionsInstancesResource : TAppsServicesVersionsInstancesResource;virtual;overload;
    Function CreateServicesVersionsResource(AOwner : TComponent) : TAppsServicesVersionsResource;virtual;overload;
    Function CreateServicesVersionsResource : TAppsServicesVersionsResource;virtual;overload;
    Function CreateServicesResource(AOwner : TComponent) : TAppsServicesResource;virtual;overload;
    Function CreateServicesResource : TAppsServicesResource;virtual;overload;
    Property OperationsResource : TAppsOperationsResource Read GetOperationsInstance;
    Property ServicesVersionsInstancesResource : TAppsServicesVersionsInstancesResource Read GetServicesVersionsInstancesInstance;
    Property ServicesVersionsResource : TAppsServicesVersionsResource Read GetServicesVersionsInstance;
    Property ServicesResource : TAppsServicesResource Read GetServicesInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAppengineAPI
    --------------------------------------------------------------------}
  
  TAppengineAPI = Class(TGoogleAPI)
  Private
    FAppsOperationsInstance : TAppsOperationsResource;
    FAppsServicesVersionsInstancesInstance : TAppsServicesVersionsInstancesResource;
    FAppsServicesVersionsInstance : TAppsServicesVersionsResource;
    FAppsServicesInstance : TAppsServicesResource;
    FAppsInstance : TAppsResource;
    Function GetAppsOperationsInstance : TAppsOperationsResource;virtual;
    Function GetAppsServicesVersionsInstancesInstance : TAppsServicesVersionsInstancesResource;virtual;
    Function GetAppsServicesVersionsInstance : TAppsServicesVersionsResource;virtual;
    Function GetAppsServicesInstance : TAppsServicesResource;virtual;
    Function GetAppsInstance : TAppsResource;virtual;
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
    Function CreateAppsOperationsResource(AOwner : TComponent) : TAppsOperationsResource;virtual;overload;
    Function CreateAppsOperationsResource : TAppsOperationsResource;virtual;overload;
    Function CreateAppsServicesVersionsInstancesResource(AOwner : TComponent) : TAppsServicesVersionsInstancesResource;virtual;overload;
    Function CreateAppsServicesVersionsInstancesResource : TAppsServicesVersionsInstancesResource;virtual;overload;
    Function CreateAppsServicesVersionsResource(AOwner : TComponent) : TAppsServicesVersionsResource;virtual;overload;
    Function CreateAppsServicesVersionsResource : TAppsServicesVersionsResource;virtual;overload;
    Function CreateAppsServicesResource(AOwner : TComponent) : TAppsServicesResource;virtual;overload;
    Function CreateAppsServicesResource : TAppsServicesResource;virtual;overload;
    Function CreateAppsResource(AOwner : TComponent) : TAppsResource;virtual;overload;
    Function CreateAppsResource : TAppsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AppsOperationsResource : TAppsOperationsResource Read GetAppsOperationsInstance;
    Property AppsServicesVersionsInstancesResource : TAppsServicesVersionsInstancesResource Read GetAppsServicesVersionsInstancesInstance;
    Property AppsServicesVersionsResource : TAppsServicesVersionsResource Read GetAppsServicesVersionsInstance;
    Property AppsServicesResource : TAppsServicesResource Read GetAppsServicesInstance;
    Property AppsResource : TAppsResource Read GetAppsInstance;
  end;

implementation


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
  TApplication
  --------------------------------------------------------------------}


Procedure TApplication.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.SetdispatchRules(AIndex : Integer; const AValue : TApplicationTypedispatchRulesArray); 

begin
  If (FdispatchRules=AValue) then exit;
  FdispatchRules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.SetauthDomain(AIndex : Integer; const AValue : String); 

begin
  If (FauthDomain=AValue) then exit;
  FauthDomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.Setlocation(AIndex : Integer; const AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.SetcodeBucket(AIndex : Integer; const AValue : String); 

begin
  If (FcodeBucket=AValue) then exit;
  FcodeBucket:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.SetdefaultCookieExpiration(AIndex : Integer; const AValue : String); 

begin
  If (FdefaultCookieExpiration=AValue) then exit;
  FdefaultCookieExpiration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.SetdefaultHostname(AIndex : Integer; const AValue : String); 

begin
  If (FdefaultHostname=AValue) then exit;
  FdefaultHostname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApplication.SetdefaultBucket(AIndex : Integer; const AValue : String); 

begin
  If (FdefaultBucket=AValue) then exit;
  FdefaultBucket:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TApplication.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'dispatchrules' : SetLength(FdispatchRules,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUrlDispatchRule
  --------------------------------------------------------------------}


Procedure TUrlDispatchRule.Setdomain(AIndex : Integer; const AValue : String); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlDispatchRule.Setpath(AIndex : Integer; const AValue : String); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlDispatchRule.Setservice(AIndex : Integer; const AValue : String); 

begin
  If (Fservice=AValue) then exit;
  Fservice:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVersionTypebetaSettings
  --------------------------------------------------------------------}


Class Function TVersionTypebetaSettings.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TVersionTypeenvVariables
  --------------------------------------------------------------------}


Class Function TVersionTypeenvVariables.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TVersion
  --------------------------------------------------------------------}


Procedure TVersion.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.SetautomaticScaling(AIndex : Integer; const AValue : TAutomaticScaling); 

begin
  If (FautomaticScaling=AValue) then exit;
  FautomaticScaling:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.SetbasicScaling(AIndex : Integer; const AValue : TBasicScaling); 

begin
  If (FbasicScaling=AValue) then exit;
  FbasicScaling:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.SetmanualScaling(AIndex : Integer; const AValue : TManualScaling); 

begin
  If (FmanualScaling=AValue) then exit;
  FmanualScaling:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.SetinboundServices(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FinboundServices=AValue) then exit;
  FinboundServices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.SetinstanceClass(AIndex : Integer; const AValue : String); 

begin
  If (FinstanceClass=AValue) then exit;
  FinstanceClass:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.Setnetwork(AIndex : Integer; const AValue : TNetwork); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.Setresources(AIndex : Integer; const AValue : TResources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.Setruntime(AIndex : Integer; const AValue : String); 

begin
  If (Fruntime=AValue) then exit;
  Fruntime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.Setthreadsafe(AIndex : Integer; const AValue : boolean); 

begin
  If (Fthreadsafe=AValue) then exit;
  Fthreadsafe:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.Setvm(AIndex : Integer; const AValue : boolean); 

begin
  If (Fvm=AValue) then exit;
  Fvm:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.SetbetaSettings(AIndex : Integer; const AValue : TVersionTypebetaSettings); 

begin
  If (FbetaSettings=AValue) then exit;
  FbetaSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.Setenv(AIndex : Integer; const AValue : String); 

begin
  If (Fenv=AValue) then exit;
  Fenv:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.SetservingStatus(AIndex : Integer; const AValue : String); 

begin
  If (FservingStatus=AValue) then exit;
  FservingStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.Setdeployer(AIndex : Integer; const AValue : String); 

begin
  If (Fdeployer=AValue) then exit;
  Fdeployer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.SetcreationTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.SetdiskUsageBytes(AIndex : Integer; const AValue : String); 

begin
  If (FdiskUsageBytes=AValue) then exit;
  FdiskUsageBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.Sethandlers(AIndex : Integer; const AValue : TVersionTypehandlersArray); 

begin
  If (Fhandlers=AValue) then exit;
  Fhandlers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.SeterrorHandlers(AIndex : Integer; const AValue : TVersionTypeerrorHandlersArray); 

begin
  If (FerrorHandlers=AValue) then exit;
  FerrorHandlers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.Setlibraries(AIndex : Integer; const AValue : TVersionTypelibrariesArray); 

begin
  If (Flibraries=AValue) then exit;
  Flibraries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.SetapiConfig(AIndex : Integer; const AValue : TApiConfigHandler); 

begin
  If (FapiConfig=AValue) then exit;
  FapiConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.SetenvVariables(AIndex : Integer; const AValue : TVersionTypeenvVariables); 

begin
  If (FenvVariables=AValue) then exit;
  FenvVariables:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.SetdefaultExpiration(AIndex : Integer; const AValue : String); 

begin
  If (FdefaultExpiration=AValue) then exit;
  FdefaultExpiration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.SethealthCheck(AIndex : Integer; const AValue : THealthCheck); 

begin
  If (FhealthCheck=AValue) then exit;
  FhealthCheck:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.SetnobuildFilesRegex(AIndex : Integer; const AValue : String); 

begin
  If (FnobuildFilesRegex=AValue) then exit;
  FnobuildFilesRegex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVersion.Setdeployment(AIndex : Integer; const AValue : TDeployment); 

begin
  If (Fdeployment=AValue) then exit;
  Fdeployment:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVersion.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'inboundservices' : SetLength(FinboundServices,ALength);
  'handlers' : SetLength(Fhandlers,ALength);
  'errorhandlers' : SetLength(FerrorHandlers,ALength);
  'libraries' : SetLength(Flibraries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAutomaticScaling
  --------------------------------------------------------------------}


Procedure TAutomaticScaling.SetcoolDownPeriod(AIndex : Integer; const AValue : String); 

begin
  If (FcoolDownPeriod=AValue) then exit;
  FcoolDownPeriod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutomaticScaling.SetcpuUtilization(AIndex : Integer; const AValue : TCpuUtilization); 

begin
  If (FcpuUtilization=AValue) then exit;
  FcpuUtilization:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutomaticScaling.SetmaxConcurrentRequests(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxConcurrentRequests=AValue) then exit;
  FmaxConcurrentRequests:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutomaticScaling.SetmaxIdleInstances(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxIdleInstances=AValue) then exit;
  FmaxIdleInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutomaticScaling.SetmaxTotalInstances(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxTotalInstances=AValue) then exit;
  FmaxTotalInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutomaticScaling.SetmaxPendingLatency(AIndex : Integer; const AValue : String); 

begin
  If (FmaxPendingLatency=AValue) then exit;
  FmaxPendingLatency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutomaticScaling.SetminIdleInstances(AIndex : Integer; const AValue : integer); 

begin
  If (FminIdleInstances=AValue) then exit;
  FminIdleInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutomaticScaling.SetminTotalInstances(AIndex : Integer; const AValue : integer); 

begin
  If (FminTotalInstances=AValue) then exit;
  FminTotalInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutomaticScaling.SetminPendingLatency(AIndex : Integer; const AValue : String); 

begin
  If (FminPendingLatency=AValue) then exit;
  FminPendingLatency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutomaticScaling.SetrequestUtilization(AIndex : Integer; const AValue : TRequestUtilization); 

begin
  If (FrequestUtilization=AValue) then exit;
  FrequestUtilization:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutomaticScaling.SetdiskUtilization(AIndex : Integer; const AValue : TDiskUtilization); 

begin
  If (FdiskUtilization=AValue) then exit;
  FdiskUtilization:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutomaticScaling.SetnetworkUtilization(AIndex : Integer; const AValue : TNetworkUtilization); 

begin
  If (FnetworkUtilization=AValue) then exit;
  FnetworkUtilization:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCpuUtilization
  --------------------------------------------------------------------}


Procedure TCpuUtilization.SetaggregationWindowLength(AIndex : Integer; const AValue : String); 

begin
  If (FaggregationWindowLength=AValue) then exit;
  FaggregationWindowLength:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCpuUtilization.SettargetUtilization(AIndex : Integer; const AValue : double); 

begin
  If (FtargetUtilization=AValue) then exit;
  FtargetUtilization:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRequestUtilization
  --------------------------------------------------------------------}


Procedure TRequestUtilization.SettargetRequestCountPerSec(AIndex : Integer; const AValue : integer); 

begin
  If (FtargetRequestCountPerSec=AValue) then exit;
  FtargetRequestCountPerSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestUtilization.SettargetConcurrentRequests(AIndex : Integer; const AValue : integer); 

begin
  If (FtargetConcurrentRequests=AValue) then exit;
  FtargetConcurrentRequests:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDiskUtilization
  --------------------------------------------------------------------}


Procedure TDiskUtilization.SettargetWriteBytesPerSec(AIndex : Integer; const AValue : integer); 

begin
  If (FtargetWriteBytesPerSec=AValue) then exit;
  FtargetWriteBytesPerSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskUtilization.SettargetWriteOpsPerSec(AIndex : Integer; const AValue : integer); 

begin
  If (FtargetWriteOpsPerSec=AValue) then exit;
  FtargetWriteOpsPerSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskUtilization.SettargetReadBytesPerSec(AIndex : Integer; const AValue : integer); 

begin
  If (FtargetReadBytesPerSec=AValue) then exit;
  FtargetReadBytesPerSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskUtilization.SettargetReadOpsPerSec(AIndex : Integer; const AValue : integer); 

begin
  If (FtargetReadOpsPerSec=AValue) then exit;
  FtargetReadOpsPerSec:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNetworkUtilization
  --------------------------------------------------------------------}


Procedure TNetworkUtilization.SettargetSentBytesPerSec(AIndex : Integer; const AValue : integer); 

begin
  If (FtargetSentBytesPerSec=AValue) then exit;
  FtargetSentBytesPerSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkUtilization.SettargetSentPacketsPerSec(AIndex : Integer; const AValue : integer); 

begin
  If (FtargetSentPacketsPerSec=AValue) then exit;
  FtargetSentPacketsPerSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkUtilization.SettargetReceivedBytesPerSec(AIndex : Integer; const AValue : integer); 

begin
  If (FtargetReceivedBytesPerSec=AValue) then exit;
  FtargetReceivedBytesPerSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkUtilization.SettargetReceivedPacketsPerSec(AIndex : Integer; const AValue : integer); 

begin
  If (FtargetReceivedPacketsPerSec=AValue) then exit;
  FtargetReceivedPacketsPerSec:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBasicScaling
  --------------------------------------------------------------------}


Procedure TBasicScaling.SetidleTimeout(AIndex : Integer; const AValue : String); 

begin
  If (FidleTimeout=AValue) then exit;
  FidleTimeout:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBasicScaling.SetmaxInstances(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxInstances=AValue) then exit;
  FmaxInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TManualScaling
  --------------------------------------------------------------------}


Procedure TManualScaling.Setinstances(AIndex : Integer; const AValue : integer); 

begin
  If (Finstances=AValue) then exit;
  Finstances:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNetwork
  --------------------------------------------------------------------}


Procedure TNetwork.SetforwardedPorts(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FforwardedPorts=AValue) then exit;
  FforwardedPorts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetwork.SetinstanceTag(AIndex : Integer; const AValue : String); 

begin
  If (FinstanceTag=AValue) then exit;
  FinstanceTag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetwork.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TNetwork.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'forwardedports' : SetLength(FforwardedPorts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TResources
  --------------------------------------------------------------------}


Procedure TResources.Setcpu(AIndex : Integer; const AValue : double); 

begin
  If (Fcpu=AValue) then exit;
  Fcpu:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResources.SetdiskGb(AIndex : Integer; const AValue : double); 

begin
  If (FdiskGb=AValue) then exit;
  FdiskGb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResources.SetmemoryGb(AIndex : Integer; const AValue : double); 

begin
  If (FmemoryGb=AValue) then exit;
  FmemoryGb:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlMap
  --------------------------------------------------------------------}


Procedure TUrlMap.SeturlRegex(AIndex : Integer; const AValue : String); 

begin
  If (FurlRegex=AValue) then exit;
  FurlRegex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.SetstaticFiles(AIndex : Integer; const AValue : TStaticFilesHandler); 

begin
  If (FstaticFiles=AValue) then exit;
  FstaticFiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.Setscript(AIndex : Integer; const AValue : TScriptHandler); 

begin
  If (Fscript=AValue) then exit;
  Fscript:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.SetapiEndpoint(AIndex : Integer; const AValue : TApiEndpointHandler); 

begin
  If (FapiEndpoint=AValue) then exit;
  FapiEndpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.SetsecurityLevel(AIndex : Integer; const AValue : String); 

begin
  If (FsecurityLevel=AValue) then exit;
  FsecurityLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.Setlogin(AIndex : Integer; const AValue : String); 

begin
  If (Flogin=AValue) then exit;
  Flogin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.SetauthFailAction(AIndex : Integer; const AValue : String); 

begin
  If (FauthFailAction=AValue) then exit;
  FauthFailAction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlMap.SetredirectHttpResponseCode(AIndex : Integer; const AValue : String); 

begin
  If (FredirectHttpResponseCode=AValue) then exit;
  FredirectHttpResponseCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStaticFilesHandlerTypehttpHeaders
  --------------------------------------------------------------------}


Class Function TStaticFilesHandlerTypehttpHeaders.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TStaticFilesHandler
  --------------------------------------------------------------------}


Procedure TStaticFilesHandler.Setpath(AIndex : Integer; const AValue : String); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStaticFilesHandler.SetuploadPathRegex(AIndex : Integer; const AValue : String); 

begin
  If (FuploadPathRegex=AValue) then exit;
  FuploadPathRegex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStaticFilesHandler.SethttpHeaders(AIndex : Integer; const AValue : TStaticFilesHandlerTypehttpHeaders); 

begin
  If (FhttpHeaders=AValue) then exit;
  FhttpHeaders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStaticFilesHandler.SetmimeType(AIndex : Integer; const AValue : String); 

begin
  If (FmimeType=AValue) then exit;
  FmimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStaticFilesHandler.Setexpiration(AIndex : Integer; const AValue : String); 

begin
  If (Fexpiration=AValue) then exit;
  Fexpiration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStaticFilesHandler.SetrequireMatchingFile(AIndex : Integer; const AValue : boolean); 

begin
  If (FrequireMatchingFile=AValue) then exit;
  FrequireMatchingFile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStaticFilesHandler.SetapplicationReadable(AIndex : Integer; const AValue : boolean); 

begin
  If (FapplicationReadable=AValue) then exit;
  FapplicationReadable:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TScriptHandler
  --------------------------------------------------------------------}


Procedure TScriptHandler.SetscriptPath(AIndex : Integer; const AValue : String); 

begin
  If (FscriptPath=AValue) then exit;
  FscriptPath:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApiEndpointHandler
  --------------------------------------------------------------------}


Procedure TApiEndpointHandler.SetscriptPath(AIndex : Integer; const AValue : String); 

begin
  If (FscriptPath=AValue) then exit;
  FscriptPath:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TErrorHandler
  --------------------------------------------------------------------}


Procedure TErrorHandler.SeterrorCode(AIndex : Integer; const AValue : String); 

begin
  If (FerrorCode=AValue) then exit;
  FerrorCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorHandler.SetstaticFile(AIndex : Integer; const AValue : String); 

begin
  If (FstaticFile=AValue) then exit;
  FstaticFile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TErrorHandler.SetmimeType(AIndex : Integer; const AValue : String); 

begin
  If (FmimeType=AValue) then exit;
  FmimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLibrary
  --------------------------------------------------------------------}


Procedure TLibrary.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLibrary.Setversion(AIndex : Integer; const AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApiConfigHandler
  --------------------------------------------------------------------}


Procedure TApiConfigHandler.SetauthFailAction(AIndex : Integer; const AValue : String); 

begin
  If (FauthFailAction=AValue) then exit;
  FauthFailAction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApiConfigHandler.Setlogin(AIndex : Integer; const AValue : String); 

begin
  If (Flogin=AValue) then exit;
  Flogin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApiConfigHandler.Setscript(AIndex : Integer; const AValue : String); 

begin
  If (Fscript=AValue) then exit;
  Fscript:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApiConfigHandler.SetsecurityLevel(AIndex : Integer; const AValue : String); 

begin
  If (FsecurityLevel=AValue) then exit;
  FsecurityLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApiConfigHandler.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THealthCheck
  --------------------------------------------------------------------}


Procedure THealthCheck.SetdisableHealthCheck(AIndex : Integer; const AValue : boolean); 

begin
  If (FdisableHealthCheck=AValue) then exit;
  FdisableHealthCheck:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthCheck.Sethost(AIndex : Integer; const AValue : String); 

begin
  If (Fhost=AValue) then exit;
  Fhost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthCheck.SethealthyThreshold(AIndex : Integer; const AValue : integer); 

begin
  If (FhealthyThreshold=AValue) then exit;
  FhealthyThreshold:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthCheck.SetunhealthyThreshold(AIndex : Integer; const AValue : integer); 

begin
  If (FunhealthyThreshold=AValue) then exit;
  FunhealthyThreshold:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthCheck.SetrestartThreshold(AIndex : Integer; const AValue : integer); 

begin
  If (FrestartThreshold=AValue) then exit;
  FrestartThreshold:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthCheck.SetcheckInterval(AIndex : Integer; const AValue : String); 

begin
  If (FcheckInterval=AValue) then exit;
  FcheckInterval:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthCheck.Settimeout(AIndex : Integer; const AValue : String); 

begin
  If (Ftimeout=AValue) then exit;
  Ftimeout:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeploymentTypefiles
  --------------------------------------------------------------------}


Class Function TDeploymentTypefiles.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TDeployment
  --------------------------------------------------------------------}


Procedure TDeployment.Setfiles(AIndex : Integer; const AValue : TDeploymentTypefiles); 

begin
  If (Ffiles=AValue) then exit;
  Ffiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setcontainer(AIndex : Integer; const AValue : TContainerInfo); 

begin
  If (Fcontainer=AValue) then exit;
  Fcontainer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.SetsourceReferences(AIndex : Integer; const AValue : TDeploymentTypesourceReferencesArray); 

begin
  If (FsourceReferences=AValue) then exit;
  FsourceReferences:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDeployment.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sourcereferences' : SetLength(FsourceReferences,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TFileInfo
  --------------------------------------------------------------------}


Procedure TFileInfo.SetsourceUrl(AIndex : Integer; const AValue : String); 

begin
  If (FsourceUrl=AValue) then exit;
  FsourceUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileInfo.Setsha1Sum(AIndex : Integer; const AValue : String); 

begin
  If (Fsha1Sum=AValue) then exit;
  Fsha1Sum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFileInfo.SetmimeType(AIndex : Integer; const AValue : String); 

begin
  If (FmimeType=AValue) then exit;
  FmimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TContainerInfo
  --------------------------------------------------------------------}


Procedure TContainerInfo.Setimage(AIndex : Integer; const AValue : String); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceReference
  --------------------------------------------------------------------}


Procedure TSourceReference.Setrepository(AIndex : Integer; const AValue : String); 

begin
  If (Frepository=AValue) then exit;
  Frepository:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceReference.SetrevisionId(AIndex : Integer; const AValue : String); 

begin
  If (FrevisionId=AValue) then exit;
  FrevisionId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListVersionsResponse
  --------------------------------------------------------------------}


Procedure TListVersionsResponse.Setversions(AIndex : Integer; const AValue : TListVersionsResponseTypeversionsArray); 

begin
  If (Fversions=AValue) then exit;
  Fversions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListVersionsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListVersionsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'versions' : SetLength(Fversions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TService
  --------------------------------------------------------------------}


Procedure TService.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TService.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TService.Setsplit(AIndex : Integer; const AValue : TTrafficSplit); 

begin
  If (Fsplit=AValue) then exit;
  Fsplit:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTrafficSplitTypeallocations
  --------------------------------------------------------------------}


Class Function TTrafficSplitTypeallocations.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TTrafficSplit
  --------------------------------------------------------------------}


Procedure TTrafficSplit.SetshardBy(AIndex : Integer; const AValue : String); 

begin
  If (FshardBy=AValue) then exit;
  FshardBy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrafficSplit.Setallocations(AIndex : Integer; const AValue : TTrafficSplitTypeallocations); 

begin
  If (Fallocations=AValue) then exit;
  Fallocations:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListServicesResponse
  --------------------------------------------------------------------}


Procedure TListServicesResponse.Setservices(AIndex : Integer; const AValue : TListServicesResponseTypeservicesArray); 

begin
  If (Fservices=AValue) then exit;
  Fservices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListServicesResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListServicesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'services' : SetLength(Fservices,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListInstancesResponse
  --------------------------------------------------------------------}


Procedure TListInstancesResponse.Setinstances(AIndex : Integer; const AValue : TListInstancesResponseTypeinstancesArray); 

begin
  If (Finstances=AValue) then exit;
  Finstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListInstancesResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListInstancesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'instances' : SetLength(Finstances,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TInstance
  --------------------------------------------------------------------}


Procedure TInstance.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetappEngineRelease(AIndex : Integer; const AValue : String); 

begin
  If (FappEngineRelease=AValue) then exit;
  FappEngineRelease:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Setavailability(AIndex : Integer; const AValue : String); 

begin
  If (Favailability=AValue) then exit;
  Favailability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetvmName(AIndex : Integer; const AValue : String); 

begin
  If (FvmName=AValue) then exit;
  FvmName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetvmZoneName(AIndex : Integer; const AValue : String); 

begin
  If (FvmZoneName=AValue) then exit;
  FvmZoneName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetvmId(AIndex : Integer; const AValue : String); 

begin
  If (FvmId=AValue) then exit;
  FvmId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetstartTimestamp(AIndex : Integer; const AValue : String); 

begin
  If (FstartTimestamp=AValue) then exit;
  FstartTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Setrequests(AIndex : Integer; const AValue : integer); 

begin
  If (Frequests=AValue) then exit;
  Frequests:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Seterrors(AIndex : Integer; const AValue : integer); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.Setqps(AIndex : Integer; const AValue : integer); 

begin
  If (Fqps=AValue) then exit;
  Fqps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetaverageLatency(AIndex : Integer; const AValue : integer); 

begin
  If (FaverageLatency=AValue) then exit;
  FaverageLatency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetmemoryUsage(AIndex : Integer; const AValue : String); 

begin
  If (FmemoryUsage=AValue) then exit;
  FmemoryUsage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetvmStatus(AIndex : Integer; const AValue : String); 

begin
  If (FvmStatus=AValue) then exit;
  FvmStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstance.SetvmUnlocked(AIndex : Integer; const AValue : boolean); 

begin
  If (FvmUnlocked=AValue) then exit;
  FvmUnlocked:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationMetadata
  --------------------------------------------------------------------}


Procedure TOperationMetadata.SetoperationType(AIndex : Integer; const AValue : String); 

begin
  If (FoperationType=AValue) then exit;
  FoperationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.SetinsertTime(AIndex : Integer; const AValue : String); 

begin
  If (FinsertTime=AValue) then exit;
  FinsertTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.SetendTime(AIndex : Integer; const AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.Setuser(AIndex : Integer; const AValue : String); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.Settarget(AIndex : Integer; const AValue : String); 

begin
  If (Ftarget=AValue) then exit;
  Ftarget:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.Setmethod(AIndex : Integer; const AValue : String); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationMetadataV1Beta5
  --------------------------------------------------------------------}


Procedure TOperationMetadataV1Beta5.Setmethod(AIndex : Integer; const AValue : String); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadataV1Beta5.SetinsertTime(AIndex : Integer; const AValue : String); 

begin
  If (FinsertTime=AValue) then exit;
  FinsertTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadataV1Beta5.SetendTime(AIndex : Integer; const AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadataV1Beta5.Setuser(AIndex : Integer; const AValue : String); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadataV1Beta5.Settarget(AIndex : Integer; const AValue : String); 

begin
  If (Ftarget=AValue) then exit;
  Ftarget:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAppsOperationsResource
  --------------------------------------------------------------------}


Class Function TAppsOperationsResource.ResourceName : String;

begin
  Result:='operations';
end;

Class Function TAppsOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TappengineAPI;
end;

Function TAppsOperationsResource.Get(appsId: string; operationsId: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta5/apps/{appsId}/operations/{operationsId}';
  _Methodid   = 'appengine.apps.operations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['appsId',appsId,'operationsId',operationsId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TAppsOperationsResource.List(appsId: string; AQuery : string = '') : TListOperationsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta5/apps/{appsId}/operations';
  _Methodid   = 'appengine.apps.operations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['appsId',appsId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListOperationsResponse) as TListOperationsResponse;
end;


Function TAppsOperationsResource.List(appsId: string; AQuery : TAppsOperationslistOptions) : TListOperationsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(appsId,_Q);
end;



{ --------------------------------------------------------------------
  TAppsServicesVersionsInstancesResource
  --------------------------------------------------------------------}


Class Function TAppsServicesVersionsInstancesResource.ResourceName : String;

begin
  Result:='instances';
end;

Class Function TAppsServicesVersionsInstancesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TappengineAPI;
end;

Function TAppsServicesVersionsInstancesResource.List(appsId: string; servicesId: string; versionsId: string; AQuery : string = '') : TListInstancesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta5/apps/{appsId}/services/{servicesId}/versions/{versionsId}/instances';
  _Methodid   = 'appengine.apps.services.versions.instances.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['appsId',appsId,'servicesId',servicesId,'versionsId',versionsId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListInstancesResponse) as TListInstancesResponse;
end;


Function TAppsServicesVersionsInstancesResource.List(appsId: string; servicesId: string; versionsId: string; AQuery : TAppsServicesVersionsInstanceslistOptions) : TListInstancesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(appsId,servicesId,versionsId,_Q);
end;



{ --------------------------------------------------------------------
  TAppsServicesVersionsResource
  --------------------------------------------------------------------}


Class Function TAppsServicesVersionsResource.ResourceName : String;

begin
  Result:='versions';
end;

Class Function TAppsServicesVersionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TappengineAPI;
end;

Function TAppsServicesVersionsResource.Create(appsId: string; servicesId: string; aVersion : TVersion) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta5/apps/{appsId}/services/{servicesId}/versions';
  _Methodid   = 'appengine.apps.services.versions.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['appsId',appsId,'servicesId',servicesId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aVersion,TOperation) as TOperation;
end;

Function TAppsServicesVersionsResource.Delete(appsId: string; servicesId: string; versionsId: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1beta5/apps/{appsId}/services/{servicesId}/versions/{versionsId}';
  _Methodid   = 'appengine.apps.services.versions.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['appsId',appsId,'servicesId',servicesId,'versionsId',versionsId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TAppsServicesVersionsResource.Get(appsId: string; servicesId: string; versionsId: string; AQuery : string = '') : TVersion;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta5/apps/{appsId}/services/{servicesId}/versions/{versionsId}';
  _Methodid   = 'appengine.apps.services.versions.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['appsId',appsId,'servicesId',servicesId,'versionsId',versionsId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TVersion) as TVersion;
end;


Function TAppsServicesVersionsResource.Get(appsId: string; servicesId: string; versionsId: string; AQuery : TAppsServicesVersionsgetOptions) : TVersion;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'view',AQuery.view);
  Result:=Get(appsId,servicesId,versionsId,_Q);
end;

Function TAppsServicesVersionsResource.List(appsId: string; servicesId: string; AQuery : string = '') : TListVersionsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta5/apps/{appsId}/services/{servicesId}/versions';
  _Methodid   = 'appengine.apps.services.versions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['appsId',appsId,'servicesId',servicesId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListVersionsResponse) as TListVersionsResponse;
end;


Function TAppsServicesVersionsResource.List(appsId: string; servicesId: string; AQuery : TAppsServicesVersionslistOptions) : TListVersionsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'view',AQuery.view);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(appsId,servicesId,_Q);
end;

Function TAppsServicesVersionsResource.Patch(appsId: string; servicesId: string; versionsId: string; aVersion : TVersion; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'v1beta5/apps/{appsId}/services/{servicesId}/versions/{versionsId}';
  _Methodid   = 'appengine.apps.services.versions.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['appsId',appsId,'servicesId',servicesId,'versionsId',versionsId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aVersion,TOperation) as TOperation;
end;


Function TAppsServicesVersionsResource.Patch(appsId: string; servicesId: string; versionsId: string; aVersion : TVersion; AQuery : TAppsServicesVersionspatchOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'mask',AQuery.mask);
  Result:=Patch(appsId,servicesId,versionsId,aVersion,_Q);
end;



Function TAppsServicesVersionsResource.GetInstancesInstance : TAppsServicesVersionsInstancesResource;

begin
  if (FInstancesInstance=Nil) then
    FInstancesInstance:=CreateInstancesResource;
  Result:=FInstancesInstance;
end;

Function TAppsServicesVersionsResource.CreateInstancesResource : TAppsServicesVersionsInstancesResource;

begin
  Result:=CreateInstancesResource(Self);
end;


Function TAppsServicesVersionsResource.CreateInstancesResource(AOwner : TComponent) : TAppsServicesVersionsInstancesResource;

begin
  Result:=TAppsServicesVersionsInstancesResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TAppsServicesResource
  --------------------------------------------------------------------}


Class Function TAppsServicesResource.ResourceName : String;

begin
  Result:='services';
end;

Class Function TAppsServicesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TappengineAPI;
end;

Function TAppsServicesResource.Delete(appsId: string; servicesId: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1beta5/apps/{appsId}/services/{servicesId}';
  _Methodid   = 'appengine.apps.services.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['appsId',appsId,'servicesId',servicesId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TAppsServicesResource.Get(appsId: string; servicesId: string) : TService;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta5/apps/{appsId}/services/{servicesId}';
  _Methodid   = 'appengine.apps.services.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['appsId',appsId,'servicesId',servicesId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TService) as TService;
end;

Function TAppsServicesResource.List(appsId: string; AQuery : string = '') : TListServicesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta5/apps/{appsId}/services';
  _Methodid   = 'appengine.apps.services.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['appsId',appsId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListServicesResponse) as TListServicesResponse;
end;


Function TAppsServicesResource.List(appsId: string; AQuery : TAppsServiceslistOptions) : TListServicesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(appsId,_Q);
end;

Function TAppsServicesResource.Patch(appsId: string; servicesId: string; aService : TService; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'v1beta5/apps/{appsId}/services/{servicesId}';
  _Methodid   = 'appengine.apps.services.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['appsId',appsId,'servicesId',servicesId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aService,TOperation) as TOperation;
end;


Function TAppsServicesResource.Patch(appsId: string; servicesId: string; aService : TService; AQuery : TAppsServicespatchOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'mask',AQuery.mask);
  AddToQuery(_Q,'migrateTraffic',AQuery.migrateTraffic);
  Result:=Patch(appsId,servicesId,aService,_Q);
end;



Function TAppsServicesResource.GetVersionsInstancesInstance : TAppsServicesVersionsInstancesResource;

begin
  if (FVersionsInstancesInstance=Nil) then
    FVersionsInstancesInstance:=CreateVersionsInstancesResource;
  Result:=FVersionsInstancesInstance;
end;

Function TAppsServicesResource.CreateVersionsInstancesResource : TAppsServicesVersionsInstancesResource;

begin
  Result:=CreateVersionsInstancesResource(Self);
end;


Function TAppsServicesResource.CreateVersionsInstancesResource(AOwner : TComponent) : TAppsServicesVersionsInstancesResource;

begin
  Result:=TAppsServicesVersionsInstancesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAppsServicesResource.GetVersionsInstance : TAppsServicesVersionsResource;

begin
  if (FVersionsInstance=Nil) then
    FVersionsInstance:=CreateVersionsResource;
  Result:=FVersionsInstance;
end;

Function TAppsServicesResource.CreateVersionsResource : TAppsServicesVersionsResource;

begin
  Result:=CreateVersionsResource(Self);
end;


Function TAppsServicesResource.CreateVersionsResource(AOwner : TComponent) : TAppsServicesVersionsResource;

begin
  Result:=TAppsServicesVersionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TAppsResource
  --------------------------------------------------------------------}


Class Function TAppsResource.ResourceName : String;

begin
  Result:='apps';
end;

Class Function TAppsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TappengineAPI;
end;

Function TAppsResource.Get(appsId: string; AQuery : string = '') : TApplication;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta5/apps/{appsId}';
  _Methodid   = 'appengine.apps.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['appsId',appsId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TApplication) as TApplication;
end;


Function TAppsResource.Get(appsId: string; AQuery : TAppsgetOptions) : TApplication;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'ensureResourcesExist',AQuery.ensureResourcesExist);
  Result:=Get(appsId,_Q);
end;



Function TAppsResource.GetOperationsInstance : TAppsOperationsResource;

begin
  if (FOperationsInstance=Nil) then
    FOperationsInstance:=CreateOperationsResource;
  Result:=FOperationsInstance;
end;

Function TAppsResource.CreateOperationsResource : TAppsOperationsResource;

begin
  Result:=CreateOperationsResource(Self);
end;


Function TAppsResource.CreateOperationsResource(AOwner : TComponent) : TAppsOperationsResource;

begin
  Result:=TAppsOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAppsResource.GetServicesVersionsInstancesInstance : TAppsServicesVersionsInstancesResource;

begin
  if (FServicesVersionsInstancesInstance=Nil) then
    FServicesVersionsInstancesInstance:=CreateServicesVersionsInstancesResource;
  Result:=FServicesVersionsInstancesInstance;
end;

Function TAppsResource.CreateServicesVersionsInstancesResource : TAppsServicesVersionsInstancesResource;

begin
  Result:=CreateServicesVersionsInstancesResource(Self);
end;


Function TAppsResource.CreateServicesVersionsInstancesResource(AOwner : TComponent) : TAppsServicesVersionsInstancesResource;

begin
  Result:=TAppsServicesVersionsInstancesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAppsResource.GetServicesVersionsInstance : TAppsServicesVersionsResource;

begin
  if (FServicesVersionsInstance=Nil) then
    FServicesVersionsInstance:=CreateServicesVersionsResource;
  Result:=FServicesVersionsInstance;
end;

Function TAppsResource.CreateServicesVersionsResource : TAppsServicesVersionsResource;

begin
  Result:=CreateServicesVersionsResource(Self);
end;


Function TAppsResource.CreateServicesVersionsResource(AOwner : TComponent) : TAppsServicesVersionsResource;

begin
  Result:=TAppsServicesVersionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAppsResource.GetServicesInstance : TAppsServicesResource;

begin
  if (FServicesInstance=Nil) then
    FServicesInstance:=CreateServicesResource;
  Result:=FServicesInstance;
end;

Function TAppsResource.CreateServicesResource : TAppsServicesResource;

begin
  Result:=CreateServicesResource(Self);
end;


Function TAppsResource.CreateServicesResource(AOwner : TComponent) : TAppsServicesResource;

begin
  Result:=TAppsServicesResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TAppengineAPI
  --------------------------------------------------------------------}

Class Function TAppengineAPI.APIName : String;

begin
  Result:='appengine';
end;

Class Function TAppengineAPI.APIVersion : String;

begin
  Result:='v1beta5';
end;

Class Function TAppengineAPI.APIRevision : String;

begin
  Result:='20160407';
end;

Class Function TAppengineAPI.APIID : String;

begin
  Result:='appengine:v1beta5';
end;

Class Function TAppengineAPI.APITitle : String;

begin
  Result:='Google App Engine Admin API';
end;

Class Function TAppengineAPI.APIDescription : String;

begin
  Result:='Provisions and manages App Engine applications.';
end;

Class Function TAppengineAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TAppengineAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TAppengineAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TAppengineAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TAppengineAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/appengine/docs/admin-api/';
end;

Class Function TAppengineAPI.APIrootUrl : string;

begin
  Result:='https://appengine.googleapis.com/';
end;

Class Function TAppengineAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TAppengineAPI.APIbaseURL : String;

begin
  Result:='https://appengine.googleapis.com/';
end;

Class Function TAppengineAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TAppengineAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TAppengineAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TAppengineAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  
end;

Class Function TAppengineAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TAppengineAPI.RegisterAPIResources;

begin
  TOperationTypemetadata.RegisterObject;
  TOperationTyperesponse.RegisterObject;
  TOperation.RegisterObject;
  TStatusTypedetailsItem.RegisterObject;
  TStatus.RegisterObject;
  TListOperationsResponse.RegisterObject;
  TApplication.RegisterObject;
  TUrlDispatchRule.RegisterObject;
  TVersionTypebetaSettings.RegisterObject;
  TVersionTypeenvVariables.RegisterObject;
  TVersion.RegisterObject;
  TAutomaticScaling.RegisterObject;
  TCpuUtilization.RegisterObject;
  TRequestUtilization.RegisterObject;
  TDiskUtilization.RegisterObject;
  TNetworkUtilization.RegisterObject;
  TBasicScaling.RegisterObject;
  TManualScaling.RegisterObject;
  TNetwork.RegisterObject;
  TResources.RegisterObject;
  TUrlMap.RegisterObject;
  TStaticFilesHandlerTypehttpHeaders.RegisterObject;
  TStaticFilesHandler.RegisterObject;
  TScriptHandler.RegisterObject;
  TApiEndpointHandler.RegisterObject;
  TErrorHandler.RegisterObject;
  TLibrary.RegisterObject;
  TApiConfigHandler.RegisterObject;
  THealthCheck.RegisterObject;
  TDeploymentTypefiles.RegisterObject;
  TDeployment.RegisterObject;
  TFileInfo.RegisterObject;
  TContainerInfo.RegisterObject;
  TSourceReference.RegisterObject;
  TListVersionsResponse.RegisterObject;
  TService.RegisterObject;
  TTrafficSplitTypeallocations.RegisterObject;
  TTrafficSplit.RegisterObject;
  TListServicesResponse.RegisterObject;
  TListInstancesResponse.RegisterObject;
  TInstance.RegisterObject;
  TOperationMetadata.RegisterObject;
  TOperationMetadataV1Beta5.RegisterObject;
end;


Function TAppengineAPI.GetAppsOperationsInstance : TAppsOperationsResource;

begin
  if (FAppsOperationsInstance=Nil) then
    FAppsOperationsInstance:=CreateAppsOperationsResource;
  Result:=FAppsOperationsInstance;
end;

Function TAppengineAPI.CreateAppsOperationsResource : TAppsOperationsResource;

begin
  Result:=CreateAppsOperationsResource(Self);
end;


Function TAppengineAPI.CreateAppsOperationsResource(AOwner : TComponent) : TAppsOperationsResource;

begin
  Result:=TAppsOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAppengineAPI.GetAppsServicesVersionsInstancesInstance : TAppsServicesVersionsInstancesResource;

begin
  if (FAppsServicesVersionsInstancesInstance=Nil) then
    FAppsServicesVersionsInstancesInstance:=CreateAppsServicesVersionsInstancesResource;
  Result:=FAppsServicesVersionsInstancesInstance;
end;

Function TAppengineAPI.CreateAppsServicesVersionsInstancesResource : TAppsServicesVersionsInstancesResource;

begin
  Result:=CreateAppsServicesVersionsInstancesResource(Self);
end;


Function TAppengineAPI.CreateAppsServicesVersionsInstancesResource(AOwner : TComponent) : TAppsServicesVersionsInstancesResource;

begin
  Result:=TAppsServicesVersionsInstancesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAppengineAPI.GetAppsServicesVersionsInstance : TAppsServicesVersionsResource;

begin
  if (FAppsServicesVersionsInstance=Nil) then
    FAppsServicesVersionsInstance:=CreateAppsServicesVersionsResource;
  Result:=FAppsServicesVersionsInstance;
end;

Function TAppengineAPI.CreateAppsServicesVersionsResource : TAppsServicesVersionsResource;

begin
  Result:=CreateAppsServicesVersionsResource(Self);
end;


Function TAppengineAPI.CreateAppsServicesVersionsResource(AOwner : TComponent) : TAppsServicesVersionsResource;

begin
  Result:=TAppsServicesVersionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAppengineAPI.GetAppsServicesInstance : TAppsServicesResource;

begin
  if (FAppsServicesInstance=Nil) then
    FAppsServicesInstance:=CreateAppsServicesResource;
  Result:=FAppsServicesInstance;
end;

Function TAppengineAPI.CreateAppsServicesResource : TAppsServicesResource;

begin
  Result:=CreateAppsServicesResource(Self);
end;


Function TAppengineAPI.CreateAppsServicesResource(AOwner : TComponent) : TAppsServicesResource;

begin
  Result:=TAppsServicesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAppengineAPI.GetAppsInstance : TAppsResource;

begin
  if (FAppsInstance=Nil) then
    FAppsInstance:=CreateAppsResource;
  Result:=FAppsInstance;
end;

Function TAppengineAPI.CreateAppsResource : TAppsResource;

begin
  Result:=CreateAppsResource(Self);
end;


Function TAppengineAPI.CreateAppsResource(AOwner : TComponent) : TAppsResource;

begin
  Result:=TAppsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TAppengineAPI.RegisterAPI;
end.
