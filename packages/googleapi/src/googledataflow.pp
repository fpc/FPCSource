unit googledataflow;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TJob = Class;
  TEnvironment = Class;
  TWorkerPool = Class;
  TPackage = Class;
  TTaskRunnerSettings = Class;
  TWorkerSettings = Class;
  TDisk = Class;
  TAutoscalingSettings = Class;
  TStep = Class;
  TJobExecutionInfo = Class;
  TJobExecutionStageInfo = Class;
  TListJobsResponse = Class;
  TListJobMessagesResponse = Class;
  TJobMessage = Class;
  TJobMetrics = Class;
  TMetricUpdate = Class;
  TMetricStructuredName = Class;
  TReportWorkItemStatusRequest = Class;
  TWorkItemStatus = Class;
  TStatus = Class;
  TApproximateReportedProgress = Class;
  TPosition = Class;
  TConcatPosition = Class;
  TReportedParallelism = Class;
  TDynamicSourceSplit = Class;
  TDerivedSource = Class;
  TSource = Class;
  TSourceMetadata = Class;
  TSourceOperationResponse = Class;
  TSourceSplitResponse = Class;
  TSourceSplitShard = Class;
  TSourceGetMetadataResponse = Class;
  TSourceFork = Class;
  TApproximateProgress = Class;
  TReportWorkItemStatusResponse = Class;
  TWorkItemServiceState = Class;
  TApproximateSplitRequest = Class;
  TLeaseWorkItemRequest = Class;
  TLeaseWorkItemResponse = Class;
  TWorkItem = Class;
  TMapTask = Class;
  TParallelInstruction = Class;
  TReadInstruction = Class;
  TWriteInstruction = Class;
  TInstructionInput = Class;
  TSink = Class;
  TParDoInstruction = Class;
  TSideInputInfo = Class;
  TMultiOutputInfo = Class;
  TPartialGroupByKeyInstruction = Class;
  TFlattenInstruction = Class;
  TInstructionOutput = Class;
  TSeqMapTask = Class;
  TSeqMapTaskOutputInfo = Class;
  TShellTask = Class;
  TStreamingSetupTask = Class;
  TTopologyConfig = Class;
  TComputationTopology = Class;
  TKeyRangeLocation = Class;
  TStreamLocation = Class;
  TStreamingStageLocation = Class;
  TPubsubLocation = Class;
  TStreamingSideInputLocation = Class;
  TCustomSourceLocation = Class;
  TStateFamilyConfig = Class;
  TDataDiskAssignment = Class;
  TSourceOperationRequest = Class;
  TSourceSplitRequest = Class;
  TSourceSplitOptions = Class;
  TSourceGetMetadataRequest = Class;
  TStreamingComputationTask = Class;
  TMountedDataDisk = Class;
  TStreamingComputationRanges = Class;
  TKeyRangeDataDiskAssignment = Class;
  TSendWorkerMessagesRequest = Class;
  TWorkerMessage = Class;
  TWorkerHealthReport = Class;
  TWorkerMessageCode = Class;
  TSendWorkerMessagesResponse = Class;
  TWorkerMessageResponse = Class;
  TWorkerHealthReportResponse = Class;
  TJobArray = Array of TJob;
  TEnvironmentArray = Array of TEnvironment;
  TWorkerPoolArray = Array of TWorkerPool;
  TPackageArray = Array of TPackage;
  TTaskRunnerSettingsArray = Array of TTaskRunnerSettings;
  TWorkerSettingsArray = Array of TWorkerSettings;
  TDiskArray = Array of TDisk;
  TAutoscalingSettingsArray = Array of TAutoscalingSettings;
  TStepArray = Array of TStep;
  TJobExecutionInfoArray = Array of TJobExecutionInfo;
  TJobExecutionStageInfoArray = Array of TJobExecutionStageInfo;
  TListJobsResponseArray = Array of TListJobsResponse;
  TListJobMessagesResponseArray = Array of TListJobMessagesResponse;
  TJobMessageArray = Array of TJobMessage;
  TJobMetricsArray = Array of TJobMetrics;
  TMetricUpdateArray = Array of TMetricUpdate;
  TMetricStructuredNameArray = Array of TMetricStructuredName;
  TReportWorkItemStatusRequestArray = Array of TReportWorkItemStatusRequest;
  TWorkItemStatusArray = Array of TWorkItemStatus;
  TStatusArray = Array of TStatus;
  TApproximateReportedProgressArray = Array of TApproximateReportedProgress;
  TPositionArray = Array of TPosition;
  TConcatPositionArray = Array of TConcatPosition;
  TReportedParallelismArray = Array of TReportedParallelism;
  TDynamicSourceSplitArray = Array of TDynamicSourceSplit;
  TDerivedSourceArray = Array of TDerivedSource;
  TSourceArray = Array of TSource;
  TSourceMetadataArray = Array of TSourceMetadata;
  TSourceOperationResponseArray = Array of TSourceOperationResponse;
  TSourceSplitResponseArray = Array of TSourceSplitResponse;
  TSourceSplitShardArray = Array of TSourceSplitShard;
  TSourceGetMetadataResponseArray = Array of TSourceGetMetadataResponse;
  TSourceForkArray = Array of TSourceFork;
  TApproximateProgressArray = Array of TApproximateProgress;
  TReportWorkItemStatusResponseArray = Array of TReportWorkItemStatusResponse;
  TWorkItemServiceStateArray = Array of TWorkItemServiceState;
  TApproximateSplitRequestArray = Array of TApproximateSplitRequest;
  TLeaseWorkItemRequestArray = Array of TLeaseWorkItemRequest;
  TLeaseWorkItemResponseArray = Array of TLeaseWorkItemResponse;
  TWorkItemArray = Array of TWorkItem;
  TMapTaskArray = Array of TMapTask;
  TParallelInstructionArray = Array of TParallelInstruction;
  TReadInstructionArray = Array of TReadInstruction;
  TWriteInstructionArray = Array of TWriteInstruction;
  TInstructionInputArray = Array of TInstructionInput;
  TSinkArray = Array of TSink;
  TParDoInstructionArray = Array of TParDoInstruction;
  TSideInputInfoArray = Array of TSideInputInfo;
  TMultiOutputInfoArray = Array of TMultiOutputInfo;
  TPartialGroupByKeyInstructionArray = Array of TPartialGroupByKeyInstruction;
  TFlattenInstructionArray = Array of TFlattenInstruction;
  TInstructionOutputArray = Array of TInstructionOutput;
  TSeqMapTaskArray = Array of TSeqMapTask;
  TSeqMapTaskOutputInfoArray = Array of TSeqMapTaskOutputInfo;
  TShellTaskArray = Array of TShellTask;
  TStreamingSetupTaskArray = Array of TStreamingSetupTask;
  TTopologyConfigArray = Array of TTopologyConfig;
  TComputationTopologyArray = Array of TComputationTopology;
  TKeyRangeLocationArray = Array of TKeyRangeLocation;
  TStreamLocationArray = Array of TStreamLocation;
  TStreamingStageLocationArray = Array of TStreamingStageLocation;
  TPubsubLocationArray = Array of TPubsubLocation;
  TStreamingSideInputLocationArray = Array of TStreamingSideInputLocation;
  TCustomSourceLocationArray = Array of TCustomSourceLocation;
  TStateFamilyConfigArray = Array of TStateFamilyConfig;
  TDataDiskAssignmentArray = Array of TDataDiskAssignment;
  TSourceOperationRequestArray = Array of TSourceOperationRequest;
  TSourceSplitRequestArray = Array of TSourceSplitRequest;
  TSourceSplitOptionsArray = Array of TSourceSplitOptions;
  TSourceGetMetadataRequestArray = Array of TSourceGetMetadataRequest;
  TStreamingComputationTaskArray = Array of TStreamingComputationTask;
  TMountedDataDiskArray = Array of TMountedDataDisk;
  TStreamingComputationRangesArray = Array of TStreamingComputationRanges;
  TKeyRangeDataDiskAssignmentArray = Array of TKeyRangeDataDiskAssignment;
  TSendWorkerMessagesRequestArray = Array of TSendWorkerMessagesRequest;
  TWorkerMessageArray = Array of TWorkerMessage;
  TWorkerHealthReportArray = Array of TWorkerHealthReport;
  TWorkerMessageCodeArray = Array of TWorkerMessageCode;
  TSendWorkerMessagesResponseArray = Array of TSendWorkerMessagesResponse;
  TWorkerMessageResponseArray = Array of TWorkerMessageResponse;
  TWorkerHealthReportResponseArray = Array of TWorkerHealthReportResponse;
  //Anonymous types, using auto-generated names
  TJobTypetransformNameMapping = Class;
  TEnvironmentTypeuserAgent = Class;
  TEnvironmentTypeversion = Class;
  TEnvironmentTypesdkPipelineOptions = Class;
  TEnvironmentTypeinternalExperiments = Class;
  TWorkerPoolTypemetadata = Class;
  TWorkerPoolTypepoolArgs = Class;
  TStepTypeproperties = Class;
  TJobExecutionInfoTypestages = Class;
  TMetricStructuredNameTypecontext = Class;
  TStatusTypedetailsItem = Class;
  TSourceTypespec = Class;
  TSourceTypecodec = Class;
  TSourceTypebaseSpecsItem = Class;
  TWorkItemServiceStateTypeharnessData = Class;
  TSinkTypespec = Class;
  TSinkTypecodec = Class;
  TParDoInstructionTypeuserFn = Class;
  TSideInputInfoTypekind = Class;
  TPartialGroupByKeyInstructionTypeinputElementCodec = Class;
  TPartialGroupByKeyInstructionTypevalueCombiningFn = Class;
  TInstructionOutputTypecodec = Class;
  TSeqMapTaskTypeuserFn = Class;
  TTopologyConfigTypeuserStageToComputationNameMap = Class;
  TWorkerMessageTypelabels = Class;
  TWorkerHealthReportTypepodsItem = Class;
  TWorkerMessageCodeTypeparameters = Class;
  TJobTypestepsArray = Array of TStep;
  TEnvironmentTypeworkerPoolsArray = Array of TWorkerPool;
  TWorkerPoolTypepackagesArray = Array of TPackage;
  TWorkerPoolTypedataDisksArray = Array of TDisk;
  TListJobsResponseTypejobsArray = Array of TJob;
  TListJobMessagesResponseTypejobMessagesArray = Array of TJobMessage;
  TJobMetricsTypemetricsArray = Array of TMetricUpdate;
  TReportWorkItemStatusRequestTypeworkItemStatusesArray = Array of TWorkItemStatus;
  TWorkItemStatusTypeerrorsArray = Array of TStatus;
  TWorkItemStatusTypemetricUpdatesArray = Array of TMetricUpdate;
  TStatusTypedetailsArray = Array of TStatusTypedetailsItem;
  TSourceTypebaseSpecsArray = Array of TSourceTypebaseSpecsItem;
  TSourceSplitResponseTypebundlesArray = Array of TDerivedSource;
  TSourceSplitResponseTypeshardsArray = Array of TSourceSplitShard;
  TReportWorkItemStatusResponseTypeworkItemServiceStatesArray = Array of TWorkItemServiceState;
  TLeaseWorkItemResponseTypeworkItemsArray = Array of TWorkItem;
  TWorkItemTypepackagesArray = Array of TPackage;
  TMapTaskTypeinstructionsArray = Array of TParallelInstruction;
  TParallelInstructionTypeoutputsArray = Array of TInstructionOutput;
  TParDoInstructionTypesideInputsArray = Array of TSideInputInfo;
  TParDoInstructionTypemultiOutputInfosArray = Array of TMultiOutputInfo;
  TSideInputInfoTypesourcesArray = Array of TSource;
  TPartialGroupByKeyInstructionTypesideInputsArray = Array of TSideInputInfo;
  TFlattenInstructionTypeinputsArray = Array of TInstructionInput;
  TSeqMapTaskTypeinputsArray = Array of TSideInputInfo;
  TSeqMapTaskTypeoutputInfosArray = Array of TSeqMapTaskOutputInfo;
  TTopologyConfigTypecomputationsArray = Array of TComputationTopology;
  TTopologyConfigTypedataDiskAssignmentsArray = Array of TDataDiskAssignment;
  TComputationTopologyTypekeyRangesArray = Array of TKeyRangeLocation;
  TComputationTopologyTypeinputsArray = Array of TStreamLocation;
  TComputationTopologyTypeoutputsArray = Array of TStreamLocation;
  TComputationTopologyTypestateFamiliesArray = Array of TStateFamilyConfig;
  TStreamingComputationTaskTypedataDisksArray = Array of TMountedDataDisk;
  TStreamingComputationTaskTypecomputationRangesArray = Array of TStreamingComputationRanges;
  TStreamingComputationRangesTyperangeAssignmentsArray = Array of TKeyRangeDataDiskAssignment;
  TSendWorkerMessagesRequestTypeworkerMessagesArray = Array of TWorkerMessage;
  TWorkerHealthReportTypepodsArray = Array of TWorkerHealthReportTypepodsItem;
  TSendWorkerMessagesResponseTypeworkerMessageResponsesArray = Array of TWorkerMessageResponse;
  
  { --------------------------------------------------------------------
    TJobTypetransformNameMapping
    --------------------------------------------------------------------}
  
  TJobTypetransformNameMapping = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TJobTypetransformNameMappingClass = Class of TJobTypetransformNameMapping;
  
  { --------------------------------------------------------------------
    TJob
    --------------------------------------------------------------------}
  
  TJob = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FprojectId : String;
    Fname : String;
    F_type : String;
    Fenvironment : TEnvironment;
    Fsteps : TJobTypestepsArray;
    FcurrentState : String;
    FcurrentStateTime : String;
    FrequestedState : String;
    FexecutionInfo : TJobExecutionInfo;
    FcreateTime : String;
    FreplaceJobId : String;
    FtransformNameMapping : TJobTypetransformNameMapping;
    FclientRequestId : String;
    FreplacedByJobId : String;
    FtempFiles : TStringArray;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setenvironment(AIndex : Integer; const AValue : TEnvironment); virtual;
    Procedure Setsteps(AIndex : Integer; const AValue : TJobTypestepsArray); virtual;
    Procedure SetcurrentState(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcurrentStateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrequestedState(AIndex : Integer; const AValue : String); virtual;
    Procedure SetexecutionInfo(AIndex : Integer; const AValue : TJobExecutionInfo); virtual;
    Procedure SetcreateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreplaceJobId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettransformNameMapping(AIndex : Integer; const AValue : TJobTypetransformNameMapping); virtual;
    Procedure SetclientRequestId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreplacedByJobId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettempFiles(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property projectId : String Index 8 Read FprojectId Write SetprojectId;
    Property name : String Index 16 Read Fname Write Setname;
    Property _type : String Index 24 Read F_type Write Set_type;
    Property environment : TEnvironment Index 32 Read Fenvironment Write Setenvironment;
    Property steps : TJobTypestepsArray Index 40 Read Fsteps Write Setsteps;
    Property currentState : String Index 48 Read FcurrentState Write SetcurrentState;
    Property currentStateTime : String Index 56 Read FcurrentStateTime Write SetcurrentStateTime;
    Property requestedState : String Index 64 Read FrequestedState Write SetrequestedState;
    Property executionInfo : TJobExecutionInfo Index 72 Read FexecutionInfo Write SetexecutionInfo;
    Property createTime : String Index 80 Read FcreateTime Write SetcreateTime;
    Property replaceJobId : String Index 88 Read FreplaceJobId Write SetreplaceJobId;
    Property transformNameMapping : TJobTypetransformNameMapping Index 96 Read FtransformNameMapping Write SettransformNameMapping;
    Property clientRequestId : String Index 104 Read FclientRequestId Write SetclientRequestId;
    Property replacedByJobId : String Index 112 Read FreplacedByJobId Write SetreplacedByJobId;
    Property tempFiles : TStringArray Index 120 Read FtempFiles Write SettempFiles;
  end;
  TJobClass = Class of TJob;
  
  { --------------------------------------------------------------------
    TEnvironmentTypeuserAgent
    --------------------------------------------------------------------}
  
  TEnvironmentTypeuserAgent = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TEnvironmentTypeuserAgentClass = Class of TEnvironmentTypeuserAgent;
  
  { --------------------------------------------------------------------
    TEnvironmentTypeversion
    --------------------------------------------------------------------}
  
  TEnvironmentTypeversion = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TEnvironmentTypeversionClass = Class of TEnvironmentTypeversion;
  
  { --------------------------------------------------------------------
    TEnvironmentTypesdkPipelineOptions
    --------------------------------------------------------------------}
  
  TEnvironmentTypesdkPipelineOptions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TEnvironmentTypesdkPipelineOptionsClass = Class of TEnvironmentTypesdkPipelineOptions;
  
  { --------------------------------------------------------------------
    TEnvironmentTypeinternalExperiments
    --------------------------------------------------------------------}
  
  TEnvironmentTypeinternalExperiments = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TEnvironmentTypeinternalExperimentsClass = Class of TEnvironmentTypeinternalExperiments;
  
  { --------------------------------------------------------------------
    TEnvironment
    --------------------------------------------------------------------}
  
  TEnvironment = Class(TGoogleBaseObject)
  Private
    FtempStoragePrefix : String;
    FclusterManagerApiService : String;
    Fexperiments : TStringArray;
    FworkerPools : TEnvironmentTypeworkerPoolsArray;
    FuserAgent : TEnvironmentTypeuserAgent;
    Fversion : TEnvironmentTypeversion;
    Fdataset : String;
    FsdkPipelineOptions : TEnvironmentTypesdkPipelineOptions;
    FinternalExperiments : TEnvironmentTypeinternalExperiments;
  Protected
    //Property setters
    Procedure SettempStoragePrefix(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclusterManagerApiService(AIndex : Integer; const AValue : String); virtual;
    Procedure Setexperiments(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetworkerPools(AIndex : Integer; const AValue : TEnvironmentTypeworkerPoolsArray); virtual;
    Procedure SetuserAgent(AIndex : Integer; const AValue : TEnvironmentTypeuserAgent); virtual;
    Procedure Setversion(AIndex : Integer; const AValue : TEnvironmentTypeversion); virtual;
    Procedure Setdataset(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsdkPipelineOptions(AIndex : Integer; const AValue : TEnvironmentTypesdkPipelineOptions); virtual;
    Procedure SetinternalExperiments(AIndex : Integer; const AValue : TEnvironmentTypeinternalExperiments); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property tempStoragePrefix : String Index 0 Read FtempStoragePrefix Write SettempStoragePrefix;
    Property clusterManagerApiService : String Index 8 Read FclusterManagerApiService Write SetclusterManagerApiService;
    Property experiments : TStringArray Index 16 Read Fexperiments Write Setexperiments;
    Property workerPools : TEnvironmentTypeworkerPoolsArray Index 24 Read FworkerPools Write SetworkerPools;
    Property userAgent : TEnvironmentTypeuserAgent Index 32 Read FuserAgent Write SetuserAgent;
    Property version : TEnvironmentTypeversion Index 40 Read Fversion Write Setversion;
    Property dataset : String Index 48 Read Fdataset Write Setdataset;
    Property sdkPipelineOptions : TEnvironmentTypesdkPipelineOptions Index 56 Read FsdkPipelineOptions Write SetsdkPipelineOptions;
    Property internalExperiments : TEnvironmentTypeinternalExperiments Index 64 Read FinternalExperiments Write SetinternalExperiments;
  end;
  TEnvironmentClass = Class of TEnvironment;
  
  { --------------------------------------------------------------------
    TWorkerPoolTypemetadata
    --------------------------------------------------------------------}
  
  TWorkerPoolTypemetadata = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TWorkerPoolTypemetadataClass = Class of TWorkerPoolTypemetadata;
  
  { --------------------------------------------------------------------
    TWorkerPoolTypepoolArgs
    --------------------------------------------------------------------}
  
  TWorkerPoolTypepoolArgs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TWorkerPoolTypepoolArgsClass = Class of TWorkerPoolTypepoolArgs;
  
  { --------------------------------------------------------------------
    TWorkerPool
    --------------------------------------------------------------------}
  
  TWorkerPool = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnumWorkers : integer;
    Fpackages : TWorkerPoolTypepackagesArray;
    FdefaultPackageSet : String;
    FmachineType : String;
    FteardownPolicy : String;
    FdiskSizeGb : integer;
    FdiskType : String;
    FdiskSourceImage : String;
    Fzone : String;
    FtaskrunnerSettings : TTaskRunnerSettings;
    FonHostMaintenance : String;
    FdataDisks : TWorkerPoolTypedataDisksArray;
    Fmetadata : TWorkerPoolTypemetadata;
    FautoscalingSettings : TAutoscalingSettings;
    FpoolArgs : TWorkerPoolTypepoolArgs;
    Fnetwork : String;
    Fsubnetwork : String;
    FworkerHarnessContainerImage : String;
    FnumThreadsPerWorker : integer;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnumWorkers(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setpackages(AIndex : Integer; const AValue : TWorkerPoolTypepackagesArray); virtual;
    Procedure SetdefaultPackageSet(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmachineType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetteardownPolicy(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdiskSizeGb(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetdiskType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdiskSourceImage(AIndex : Integer; const AValue : String); virtual;
    Procedure Setzone(AIndex : Integer; const AValue : String); virtual;
    Procedure SettaskrunnerSettings(AIndex : Integer; const AValue : TTaskRunnerSettings); virtual;
    Procedure SetonHostMaintenance(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdataDisks(AIndex : Integer; const AValue : TWorkerPoolTypedataDisksArray); virtual;
    Procedure Setmetadata(AIndex : Integer; const AValue : TWorkerPoolTypemetadata); virtual;
    Procedure SetautoscalingSettings(AIndex : Integer; const AValue : TAutoscalingSettings); virtual;
    Procedure SetpoolArgs(AIndex : Integer; const AValue : TWorkerPoolTypepoolArgs); virtual;
    Procedure Setnetwork(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsubnetwork(AIndex : Integer; const AValue : String); virtual;
    Procedure SetworkerHarnessContainerImage(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnumThreadsPerWorker(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property numWorkers : integer Index 8 Read FnumWorkers Write SetnumWorkers;
    Property packages : TWorkerPoolTypepackagesArray Index 16 Read Fpackages Write Setpackages;
    Property defaultPackageSet : String Index 24 Read FdefaultPackageSet Write SetdefaultPackageSet;
    Property machineType : String Index 32 Read FmachineType Write SetmachineType;
    Property teardownPolicy : String Index 40 Read FteardownPolicy Write SetteardownPolicy;
    Property diskSizeGb : integer Index 48 Read FdiskSizeGb Write SetdiskSizeGb;
    Property diskType : String Index 56 Read FdiskType Write SetdiskType;
    Property diskSourceImage : String Index 64 Read FdiskSourceImage Write SetdiskSourceImage;
    Property zone : String Index 72 Read Fzone Write Setzone;
    Property taskrunnerSettings : TTaskRunnerSettings Index 80 Read FtaskrunnerSettings Write SettaskrunnerSettings;
    Property onHostMaintenance : String Index 88 Read FonHostMaintenance Write SetonHostMaintenance;
    Property dataDisks : TWorkerPoolTypedataDisksArray Index 96 Read FdataDisks Write SetdataDisks;
    Property metadata : TWorkerPoolTypemetadata Index 104 Read Fmetadata Write Setmetadata;
    Property autoscalingSettings : TAutoscalingSettings Index 112 Read FautoscalingSettings Write SetautoscalingSettings;
    Property poolArgs : TWorkerPoolTypepoolArgs Index 120 Read FpoolArgs Write SetpoolArgs;
    Property network : String Index 128 Read Fnetwork Write Setnetwork;
    Property subnetwork : String Index 136 Read Fsubnetwork Write Setsubnetwork;
    Property workerHarnessContainerImage : String Index 144 Read FworkerHarnessContainerImage Write SetworkerHarnessContainerImage;
    Property numThreadsPerWorker : integer Index 152 Read FnumThreadsPerWorker Write SetnumThreadsPerWorker;
  end;
  TWorkerPoolClass = Class of TWorkerPool;
  
  { --------------------------------------------------------------------
    TPackage
    --------------------------------------------------------------------}
  
  TPackage = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Flocation : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property location : String Index 8 Read Flocation Write Setlocation;
  end;
  TPackageClass = Class of TPackage;
  
  { --------------------------------------------------------------------
    TTaskRunnerSettings
    --------------------------------------------------------------------}
  
  TTaskRunnerSettings = Class(TGoogleBaseObject)
  Private
    FtaskUser : String;
    FtaskGroup : String;
    FoauthScopes : TStringArray;
    FbaseUrl : String;
    FdataflowApiVersion : String;
    FparallelWorkerSettings : TWorkerSettings;
    FbaseTaskDir : String;
    FcontinueOnException : boolean;
    FlogToSerialconsole : boolean;
    Falsologtostderr : boolean;
    FlogUploadLocation : String;
    FlogDir : String;
    FtempStoragePrefix : String;
    FharnessCommand : String;
    FworkflowFileName : String;
    FcommandlinesFileName : String;
    FvmId : String;
    FlanguageHint : String;
    FstreamingWorkerMainClass : String;
  Protected
    //Property setters
    Procedure SettaskUser(AIndex : Integer; const AValue : String); virtual;
    Procedure SettaskGroup(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoauthScopes(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetbaseUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdataflowApiVersion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetparallelWorkerSettings(AIndex : Integer; const AValue : TWorkerSettings); virtual;
    Procedure SetbaseTaskDir(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcontinueOnException(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetlogToSerialconsole(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setalsologtostderr(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetlogUploadLocation(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlogDir(AIndex : Integer; const AValue : String); virtual;
    Procedure SettempStoragePrefix(AIndex : Integer; const AValue : String); virtual;
    Procedure SetharnessCommand(AIndex : Integer; const AValue : String); virtual;
    Procedure SetworkflowFileName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcommandlinesFileName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvmId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlanguageHint(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstreamingWorkerMainClass(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property taskUser : String Index 0 Read FtaskUser Write SettaskUser;
    Property taskGroup : String Index 8 Read FtaskGroup Write SettaskGroup;
    Property oauthScopes : TStringArray Index 16 Read FoauthScopes Write SetoauthScopes;
    Property baseUrl : String Index 24 Read FbaseUrl Write SetbaseUrl;
    Property dataflowApiVersion : String Index 32 Read FdataflowApiVersion Write SetdataflowApiVersion;
    Property parallelWorkerSettings : TWorkerSettings Index 40 Read FparallelWorkerSettings Write SetparallelWorkerSettings;
    Property baseTaskDir : String Index 48 Read FbaseTaskDir Write SetbaseTaskDir;
    Property continueOnException : boolean Index 56 Read FcontinueOnException Write SetcontinueOnException;
    Property logToSerialconsole : boolean Index 64 Read FlogToSerialconsole Write SetlogToSerialconsole;
    Property alsologtostderr : boolean Index 72 Read Falsologtostderr Write Setalsologtostderr;
    Property logUploadLocation : String Index 80 Read FlogUploadLocation Write SetlogUploadLocation;
    Property logDir : String Index 88 Read FlogDir Write SetlogDir;
    Property tempStoragePrefix : String Index 96 Read FtempStoragePrefix Write SettempStoragePrefix;
    Property harnessCommand : String Index 104 Read FharnessCommand Write SetharnessCommand;
    Property workflowFileName : String Index 112 Read FworkflowFileName Write SetworkflowFileName;
    Property commandlinesFileName : String Index 120 Read FcommandlinesFileName Write SetcommandlinesFileName;
    Property vmId : String Index 128 Read FvmId Write SetvmId;
    Property languageHint : String Index 136 Read FlanguageHint Write SetlanguageHint;
    Property streamingWorkerMainClass : String Index 144 Read FstreamingWorkerMainClass Write SetstreamingWorkerMainClass;
  end;
  TTaskRunnerSettingsClass = Class of TTaskRunnerSettings;
  
  { --------------------------------------------------------------------
    TWorkerSettings
    --------------------------------------------------------------------}
  
  TWorkerSettings = Class(TGoogleBaseObject)
  Private
    FbaseUrl : String;
    FreportingEnabled : boolean;
    FservicePath : String;
    FshuffleServicePath : String;
    FworkerId : String;
    FtempStoragePrefix : String;
  Protected
    //Property setters
    Procedure SetbaseUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreportingEnabled(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetservicePath(AIndex : Integer; const AValue : String); virtual;
    Procedure SetshuffleServicePath(AIndex : Integer; const AValue : String); virtual;
    Procedure SetworkerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettempStoragePrefix(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property baseUrl : String Index 0 Read FbaseUrl Write SetbaseUrl;
    Property reportingEnabled : boolean Index 8 Read FreportingEnabled Write SetreportingEnabled;
    Property servicePath : String Index 16 Read FservicePath Write SetservicePath;
    Property shuffleServicePath : String Index 24 Read FshuffleServicePath Write SetshuffleServicePath;
    Property workerId : String Index 32 Read FworkerId Write SetworkerId;
    Property tempStoragePrefix : String Index 40 Read FtempStoragePrefix Write SettempStoragePrefix;
  end;
  TWorkerSettingsClass = Class of TWorkerSettings;
  
  { --------------------------------------------------------------------
    TDisk
    --------------------------------------------------------------------}
  
  TDisk = Class(TGoogleBaseObject)
  Private
    FsizeGb : integer;
    FdiskType : String;
    FmountPoint : String;
  Protected
    //Property setters
    Procedure SetsizeGb(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetdiskType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmountPoint(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property sizeGb : integer Index 0 Read FsizeGb Write SetsizeGb;
    Property diskType : String Index 8 Read FdiskType Write SetdiskType;
    Property mountPoint : String Index 16 Read FmountPoint Write SetmountPoint;
  end;
  TDiskClass = Class of TDisk;
  
  { --------------------------------------------------------------------
    TAutoscalingSettings
    --------------------------------------------------------------------}
  
  TAutoscalingSettings = Class(TGoogleBaseObject)
  Private
    Falgorithm : String;
    FmaxNumWorkers : integer;
  Protected
    //Property setters
    Procedure Setalgorithm(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmaxNumWorkers(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property algorithm : String Index 0 Read Falgorithm Write Setalgorithm;
    Property maxNumWorkers : integer Index 8 Read FmaxNumWorkers Write SetmaxNumWorkers;
  end;
  TAutoscalingSettingsClass = Class of TAutoscalingSettings;
  
  { --------------------------------------------------------------------
    TStepTypeproperties
    --------------------------------------------------------------------}
  
  TStepTypeproperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TStepTypepropertiesClass = Class of TStepTypeproperties;
  
  { --------------------------------------------------------------------
    TStep
    --------------------------------------------------------------------}
  
  TStep = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fname : String;
    Fproperties : TStepTypeproperties;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setproperties(AIndex : Integer; const AValue : TStepTypeproperties); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property name : String Index 8 Read Fname Write Setname;
    Property properties : TStepTypeproperties Index 16 Read Fproperties Write Setproperties;
  end;
  TStepClass = Class of TStep;
  
  { --------------------------------------------------------------------
    TJobExecutionInfoTypestages
    --------------------------------------------------------------------}
  
  TJobExecutionInfoTypestages = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TJobExecutionInfoTypestagesClass = Class of TJobExecutionInfoTypestages;
  
  { --------------------------------------------------------------------
    TJobExecutionInfo
    --------------------------------------------------------------------}
  
  TJobExecutionInfo = Class(TGoogleBaseObject)
  Private
    Fstages : TJobExecutionInfoTypestages;
  Protected
    //Property setters
    Procedure Setstages(AIndex : Integer; const AValue : TJobExecutionInfoTypestages); virtual;
  Public
  Published
    Property stages : TJobExecutionInfoTypestages Index 0 Read Fstages Write Setstages;
  end;
  TJobExecutionInfoClass = Class of TJobExecutionInfo;
  
  { --------------------------------------------------------------------
    TJobExecutionStageInfo
    --------------------------------------------------------------------}
  
  TJobExecutionStageInfo = Class(TGoogleBaseObject)
  Private
    FstepName : TStringArray;
  Protected
    //Property setters
    Procedure SetstepName(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property stepName : TStringArray Index 0 Read FstepName Write SetstepName;
  end;
  TJobExecutionStageInfoClass = Class of TJobExecutionStageInfo;
  
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
    TListJobMessagesResponse
    --------------------------------------------------------------------}
  
  TListJobMessagesResponse = Class(TGoogleBaseObject)
  Private
    FjobMessages : TListJobMessagesResponseTypejobMessagesArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetjobMessages(AIndex : Integer; const AValue : TListJobMessagesResponseTypejobMessagesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property jobMessages : TListJobMessagesResponseTypejobMessagesArray Index 0 Read FjobMessages Write SetjobMessages;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListJobMessagesResponseClass = Class of TListJobMessagesResponse;
  
  { --------------------------------------------------------------------
    TJobMessage
    --------------------------------------------------------------------}
  
  TJobMessage = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Ftime : String;
    FmessageText : String;
    FmessageImportance : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Settime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmessageText(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmessageImportance(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property time : String Index 8 Read Ftime Write Settime;
    Property messageText : String Index 16 Read FmessageText Write SetmessageText;
    Property messageImportance : String Index 24 Read FmessageImportance Write SetmessageImportance;
  end;
  TJobMessageClass = Class of TJobMessage;
  
  { --------------------------------------------------------------------
    TJobMetrics
    --------------------------------------------------------------------}
  
  TJobMetrics = Class(TGoogleBaseObject)
  Private
    FmetricTime : String;
    Fmetrics : TJobMetricsTypemetricsArray;
  Protected
    //Property setters
    Procedure SetmetricTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmetrics(AIndex : Integer; const AValue : TJobMetricsTypemetricsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property metricTime : String Index 0 Read FmetricTime Write SetmetricTime;
    Property metrics : TJobMetricsTypemetricsArray Index 8 Read Fmetrics Write Setmetrics;
  end;
  TJobMetricsClass = Class of TJobMetrics;
  
  { --------------------------------------------------------------------
    TMetricUpdate
    --------------------------------------------------------------------}
  
  TMetricUpdate = Class(TGoogleBaseObject)
  Private
    Fname : TMetricStructuredName;
    Fkind : String;
    Fcumulative : boolean;
    Fscalar : TJSONSchema;
    FmeanSum : TJSONSchema;
    FmeanCount : TJSONSchema;
    F_set : TJSONSchema;
    Finternal : TJSONSchema;
    FupdateTime : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : TMetricStructuredName); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcumulative(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setscalar(AIndex : Integer; const AValue : TJSONSchema); virtual;
    Procedure SetmeanSum(AIndex : Integer; const AValue : TJSONSchema); virtual;
    Procedure SetmeanCount(AIndex : Integer; const AValue : TJSONSchema); virtual;
    Procedure Set_set(AIndex : Integer; const AValue : TJSONSchema); virtual;
    Procedure Setinternal(AIndex : Integer; const AValue : TJSONSchema); virtual;
    Procedure SetupdateTime(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property name : TMetricStructuredName Index 0 Read Fname Write Setname;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property cumulative : boolean Index 16 Read Fcumulative Write Setcumulative;
    Property scalar : TJSONSchema Index 24 Read Fscalar Write Setscalar;
    Property meanSum : TJSONSchema Index 32 Read FmeanSum Write SetmeanSum;
    Property meanCount : TJSONSchema Index 40 Read FmeanCount Write SetmeanCount;
    Property _set : TJSONSchema Index 48 Read F_set Write Set_set;
    Property internal : TJSONSchema Index 56 Read Finternal Write Setinternal;
    Property updateTime : String Index 64 Read FupdateTime Write SetupdateTime;
  end;
  TMetricUpdateClass = Class of TMetricUpdate;
  
  { --------------------------------------------------------------------
    TMetricStructuredNameTypecontext
    --------------------------------------------------------------------}
  
  TMetricStructuredNameTypecontext = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TMetricStructuredNameTypecontextClass = Class of TMetricStructuredNameTypecontext;
  
  { --------------------------------------------------------------------
    TMetricStructuredName
    --------------------------------------------------------------------}
  
  TMetricStructuredName = Class(TGoogleBaseObject)
  Private
    Forigin : String;
    Fname : String;
    Fcontext : TMetricStructuredNameTypecontext;
  Protected
    //Property setters
    Procedure Setorigin(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcontext(AIndex : Integer; const AValue : TMetricStructuredNameTypecontext); virtual;
  Public
  Published
    Property origin : String Index 0 Read Forigin Write Setorigin;
    Property name : String Index 8 Read Fname Write Setname;
    Property context : TMetricStructuredNameTypecontext Index 16 Read Fcontext Write Setcontext;
  end;
  TMetricStructuredNameClass = Class of TMetricStructuredName;
  
  { --------------------------------------------------------------------
    TReportWorkItemStatusRequest
    --------------------------------------------------------------------}
  
  TReportWorkItemStatusRequest = Class(TGoogleBaseObject)
  Private
    FworkerId : String;
    FworkItemStatuses : TReportWorkItemStatusRequestTypeworkItemStatusesArray;
    FcurrentWorkerTime : String;
  Protected
    //Property setters
    Procedure SetworkerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetworkItemStatuses(AIndex : Integer; const AValue : TReportWorkItemStatusRequestTypeworkItemStatusesArray); virtual;
    Procedure SetcurrentWorkerTime(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property workerId : String Index 0 Read FworkerId Write SetworkerId;
    Property workItemStatuses : TReportWorkItemStatusRequestTypeworkItemStatusesArray Index 8 Read FworkItemStatuses Write SetworkItemStatuses;
    Property currentWorkerTime : String Index 16 Read FcurrentWorkerTime Write SetcurrentWorkerTime;
  end;
  TReportWorkItemStatusRequestClass = Class of TReportWorkItemStatusRequest;
  
  { --------------------------------------------------------------------
    TWorkItemStatus
    --------------------------------------------------------------------}
  
  TWorkItemStatus = Class(TGoogleBaseObject)
  Private
    FworkItemId : String;
    FreportIndex : String;
    FrequestedLeaseDuration : String;
    Fcompleted : boolean;
    Ferrors : TWorkItemStatusTypeerrorsArray;
    FmetricUpdates : TWorkItemStatusTypemetricUpdatesArray;
    FreportedProgress : TApproximateReportedProgress;
    FstopPosition : TPosition;
    FdynamicSourceSplit : TDynamicSourceSplit;
    FsourceOperationResponse : TSourceOperationResponse;
    FsourceFork : TSourceFork;
    Fprogress : TApproximateProgress;
  Protected
    //Property setters
    Procedure SetworkItemId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreportIndex(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrequestedLeaseDuration(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcompleted(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Seterrors(AIndex : Integer; const AValue : TWorkItemStatusTypeerrorsArray); virtual;
    Procedure SetmetricUpdates(AIndex : Integer; const AValue : TWorkItemStatusTypemetricUpdatesArray); virtual;
    Procedure SetreportedProgress(AIndex : Integer; const AValue : TApproximateReportedProgress); virtual;
    Procedure SetstopPosition(AIndex : Integer; const AValue : TPosition); virtual;
    Procedure SetdynamicSourceSplit(AIndex : Integer; const AValue : TDynamicSourceSplit); virtual;
    Procedure SetsourceOperationResponse(AIndex : Integer; const AValue : TSourceOperationResponse); virtual;
    Procedure SetsourceFork(AIndex : Integer; const AValue : TSourceFork); virtual;
    Procedure Setprogress(AIndex : Integer; const AValue : TApproximateProgress); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property workItemId : String Index 0 Read FworkItemId Write SetworkItemId;
    Property reportIndex : String Index 8 Read FreportIndex Write SetreportIndex;
    Property requestedLeaseDuration : String Index 16 Read FrequestedLeaseDuration Write SetrequestedLeaseDuration;
    Property completed : boolean Index 24 Read Fcompleted Write Setcompleted;
    Property errors : TWorkItemStatusTypeerrorsArray Index 32 Read Ferrors Write Seterrors;
    Property metricUpdates : TWorkItemStatusTypemetricUpdatesArray Index 40 Read FmetricUpdates Write SetmetricUpdates;
    Property reportedProgress : TApproximateReportedProgress Index 48 Read FreportedProgress Write SetreportedProgress;
    Property stopPosition : TPosition Index 56 Read FstopPosition Write SetstopPosition;
    Property dynamicSourceSplit : TDynamicSourceSplit Index 64 Read FdynamicSourceSplit Write SetdynamicSourceSplit;
    Property sourceOperationResponse : TSourceOperationResponse Index 72 Read FsourceOperationResponse Write SetsourceOperationResponse;
    Property sourceFork : TSourceFork Index 80 Read FsourceFork Write SetsourceFork;
    Property progress : TApproximateProgress Index 88 Read Fprogress Write Setprogress;
  end;
  TWorkItemStatusClass = Class of TWorkItemStatus;
  
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
    TApproximateReportedProgress
    --------------------------------------------------------------------}
  
  TApproximateReportedProgress = Class(TGoogleBaseObject)
  Private
    Fposition : TPosition;
    FfractionConsumed : double;
    FremainingParallelism : TReportedParallelism;
    FconsumedParallelism : TReportedParallelism;
  Protected
    //Property setters
    Procedure Setposition(AIndex : Integer; const AValue : TPosition); virtual;
    Procedure SetfractionConsumed(AIndex : Integer; const AValue : double); virtual;
    Procedure SetremainingParallelism(AIndex : Integer; const AValue : TReportedParallelism); virtual;
    Procedure SetconsumedParallelism(AIndex : Integer; const AValue : TReportedParallelism); virtual;
  Public
  Published
    Property position : TPosition Index 0 Read Fposition Write Setposition;
    Property fractionConsumed : double Index 8 Read FfractionConsumed Write SetfractionConsumed;
    Property remainingParallelism : TReportedParallelism Index 16 Read FremainingParallelism Write SetremainingParallelism;
    Property consumedParallelism : TReportedParallelism Index 24 Read FconsumedParallelism Write SetconsumedParallelism;
  end;
  TApproximateReportedProgressClass = Class of TApproximateReportedProgress;
  
  { --------------------------------------------------------------------
    TPosition
    --------------------------------------------------------------------}
  
  TPosition = Class(TGoogleBaseObject)
  Private
    F_end : boolean;
    Fkey : String;
    FbyteOffset : String;
    FrecordIndex : String;
    FshufflePosition : String;
    FconcatPosition : TConcatPosition;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setkey(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbyteOffset(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrecordIndex(AIndex : Integer; const AValue : String); virtual;
    Procedure SetshufflePosition(AIndex : Integer; const AValue : String); virtual;
    Procedure SetconcatPosition(AIndex : Integer; const AValue : TConcatPosition); virtual;
  Public
  Published
    Property _end : boolean Index 0 Read F_end Write Set_end;
    Property key : String Index 8 Read Fkey Write Setkey;
    Property byteOffset : String Index 16 Read FbyteOffset Write SetbyteOffset;
    Property recordIndex : String Index 24 Read FrecordIndex Write SetrecordIndex;
    Property shufflePosition : String Index 32 Read FshufflePosition Write SetshufflePosition;
    Property concatPosition : TConcatPosition Index 40 Read FconcatPosition Write SetconcatPosition;
  end;
  TPositionClass = Class of TPosition;
  
  { --------------------------------------------------------------------
    TConcatPosition
    --------------------------------------------------------------------}
  
  TConcatPosition = Class(TGoogleBaseObject)
  Private
    Findex : integer;
    Fposition : TPosition;
  Protected
    //Property setters
    Procedure Setindex(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setposition(AIndex : Integer; const AValue : TPosition); virtual;
  Public
  Published
    Property index : integer Index 0 Read Findex Write Setindex;
    Property position : TPosition Index 8 Read Fposition Write Setposition;
  end;
  TConcatPositionClass = Class of TConcatPosition;
  
  { --------------------------------------------------------------------
    TReportedParallelism
    --------------------------------------------------------------------}
  
  TReportedParallelism = Class(TGoogleBaseObject)
  Private
    FisInfinite : boolean;
    Fvalue : double;
  Protected
    //Property setters
    Procedure SetisInfinite(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property isInfinite : boolean Index 0 Read FisInfinite Write SetisInfinite;
    Property value : double Index 8 Read Fvalue Write Setvalue;
  end;
  TReportedParallelismClass = Class of TReportedParallelism;
  
  { --------------------------------------------------------------------
    TDynamicSourceSplit
    --------------------------------------------------------------------}
  
  TDynamicSourceSplit = Class(TGoogleBaseObject)
  Private
    Fprimary : TDerivedSource;
    Fresidual : TDerivedSource;
  Protected
    //Property setters
    Procedure Setprimary(AIndex : Integer; const AValue : TDerivedSource); virtual;
    Procedure Setresidual(AIndex : Integer; const AValue : TDerivedSource); virtual;
  Public
  Published
    Property primary : TDerivedSource Index 0 Read Fprimary Write Setprimary;
    Property residual : TDerivedSource Index 8 Read Fresidual Write Setresidual;
  end;
  TDynamicSourceSplitClass = Class of TDynamicSourceSplit;
  
  { --------------------------------------------------------------------
    TDerivedSource
    --------------------------------------------------------------------}
  
  TDerivedSource = Class(TGoogleBaseObject)
  Private
    Fsource : TSource;
    FderivationMode : String;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; const AValue : TSource); virtual;
    Procedure SetderivationMode(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property source : TSource Index 0 Read Fsource Write Setsource;
    Property derivationMode : String Index 8 Read FderivationMode Write SetderivationMode;
  end;
  TDerivedSourceClass = Class of TDerivedSource;
  
  { --------------------------------------------------------------------
    TSourceTypespec
    --------------------------------------------------------------------}
  
  TSourceTypespec = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSourceTypespecClass = Class of TSourceTypespec;
  
  { --------------------------------------------------------------------
    TSourceTypecodec
    --------------------------------------------------------------------}
  
  TSourceTypecodec = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSourceTypecodecClass = Class of TSourceTypecodec;
  
  { --------------------------------------------------------------------
    TSourceTypebaseSpecsItem
    --------------------------------------------------------------------}
  
  TSourceTypebaseSpecsItem = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSourceTypebaseSpecsItemClass = Class of TSourceTypebaseSpecsItem;
  
  { --------------------------------------------------------------------
    TSource
    --------------------------------------------------------------------}
  
  TSource = Class(TGoogleBaseObject)
  Private
    Fspec : TSourceTypespec;
    Fcodec : TSourceTypecodec;
    FbaseSpecs : TSourceTypebaseSpecsArray;
    Fmetadata : TSourceMetadata;
    FdoesNotNeedSplitting : boolean;
  Protected
    //Property setters
    Procedure Setspec(AIndex : Integer; const AValue : TSourceTypespec); virtual;
    Procedure Setcodec(AIndex : Integer; const AValue : TSourceTypecodec); virtual;
    Procedure SetbaseSpecs(AIndex : Integer; const AValue : TSourceTypebaseSpecsArray); virtual;
    Procedure Setmetadata(AIndex : Integer; const AValue : TSourceMetadata); virtual;
    Procedure SetdoesNotNeedSplitting(AIndex : Integer; const AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property spec : TSourceTypespec Index 0 Read Fspec Write Setspec;
    Property codec : TSourceTypecodec Index 8 Read Fcodec Write Setcodec;
    Property baseSpecs : TSourceTypebaseSpecsArray Index 16 Read FbaseSpecs Write SetbaseSpecs;
    Property metadata : TSourceMetadata Index 24 Read Fmetadata Write Setmetadata;
    Property doesNotNeedSplitting : boolean Index 32 Read FdoesNotNeedSplitting Write SetdoesNotNeedSplitting;
  end;
  TSourceClass = Class of TSource;
  
  { --------------------------------------------------------------------
    TSourceMetadata
    --------------------------------------------------------------------}
  
  TSourceMetadata = Class(TGoogleBaseObject)
  Private
    FproducesSortedKeys : boolean;
    Finfinite : boolean;
    FestimatedSizeBytes : String;
  Protected
    //Property setters
    Procedure SetproducesSortedKeys(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setinfinite(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetestimatedSizeBytes(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property producesSortedKeys : boolean Index 0 Read FproducesSortedKeys Write SetproducesSortedKeys;
    Property infinite : boolean Index 8 Read Finfinite Write Setinfinite;
    Property estimatedSizeBytes : String Index 16 Read FestimatedSizeBytes Write SetestimatedSizeBytes;
  end;
  TSourceMetadataClass = Class of TSourceMetadata;
  
  { --------------------------------------------------------------------
    TSourceOperationResponse
    --------------------------------------------------------------------}
  
  TSourceOperationResponse = Class(TGoogleBaseObject)
  Private
    Fsplit : TSourceSplitResponse;
    FgetMetadata : TSourceGetMetadataResponse;
  Protected
    //Property setters
    Procedure Setsplit(AIndex : Integer; const AValue : TSourceSplitResponse); virtual;
    Procedure SetgetMetadata(AIndex : Integer; const AValue : TSourceGetMetadataResponse); virtual;
  Public
  Published
    Property split : TSourceSplitResponse Index 0 Read Fsplit Write Setsplit;
    Property getMetadata : TSourceGetMetadataResponse Index 8 Read FgetMetadata Write SetgetMetadata;
  end;
  TSourceOperationResponseClass = Class of TSourceOperationResponse;
  
  { --------------------------------------------------------------------
    TSourceSplitResponse
    --------------------------------------------------------------------}
  
  TSourceSplitResponse = Class(TGoogleBaseObject)
  Private
    Foutcome : String;
    Fbundles : TSourceSplitResponseTypebundlesArray;
    Fshards : TSourceSplitResponseTypeshardsArray;
  Protected
    //Property setters
    Procedure Setoutcome(AIndex : Integer; const AValue : String); virtual;
    Procedure Setbundles(AIndex : Integer; const AValue : TSourceSplitResponseTypebundlesArray); virtual;
    Procedure Setshards(AIndex : Integer; const AValue : TSourceSplitResponseTypeshardsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property outcome : String Index 0 Read Foutcome Write Setoutcome;
    Property bundles : TSourceSplitResponseTypebundlesArray Index 8 Read Fbundles Write Setbundles;
    Property shards : TSourceSplitResponseTypeshardsArray Index 16 Read Fshards Write Setshards;
  end;
  TSourceSplitResponseClass = Class of TSourceSplitResponse;
  
  { --------------------------------------------------------------------
    TSourceSplitShard
    --------------------------------------------------------------------}
  
  TSourceSplitShard = Class(TGoogleBaseObject)
  Private
    Fsource : TSource;
    FderivationMode : String;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; const AValue : TSource); virtual;
    Procedure SetderivationMode(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property source : TSource Index 0 Read Fsource Write Setsource;
    Property derivationMode : String Index 8 Read FderivationMode Write SetderivationMode;
  end;
  TSourceSplitShardClass = Class of TSourceSplitShard;
  
  { --------------------------------------------------------------------
    TSourceGetMetadataResponse
    --------------------------------------------------------------------}
  
  TSourceGetMetadataResponse = Class(TGoogleBaseObject)
  Private
    Fmetadata : TSourceMetadata;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TSourceMetadata); virtual;
  Public
  Published
    Property metadata : TSourceMetadata Index 0 Read Fmetadata Write Setmetadata;
  end;
  TSourceGetMetadataResponseClass = Class of TSourceGetMetadataResponse;
  
  { --------------------------------------------------------------------
    TSourceFork
    --------------------------------------------------------------------}
  
  TSourceFork = Class(TGoogleBaseObject)
  Private
    Fprimary : TSourceSplitShard;
    Fresidual : TSourceSplitShard;
    FprimarySource : TDerivedSource;
    FresidualSource : TDerivedSource;
  Protected
    //Property setters
    Procedure Setprimary(AIndex : Integer; const AValue : TSourceSplitShard); virtual;
    Procedure Setresidual(AIndex : Integer; const AValue : TSourceSplitShard); virtual;
    Procedure SetprimarySource(AIndex : Integer; const AValue : TDerivedSource); virtual;
    Procedure SetresidualSource(AIndex : Integer; const AValue : TDerivedSource); virtual;
  Public
  Published
    Property primary : TSourceSplitShard Index 0 Read Fprimary Write Setprimary;
    Property residual : TSourceSplitShard Index 8 Read Fresidual Write Setresidual;
    Property primarySource : TDerivedSource Index 16 Read FprimarySource Write SetprimarySource;
    Property residualSource : TDerivedSource Index 24 Read FresidualSource Write SetresidualSource;
  end;
  TSourceForkClass = Class of TSourceFork;
  
  { --------------------------------------------------------------------
    TApproximateProgress
    --------------------------------------------------------------------}
  
  TApproximateProgress = Class(TGoogleBaseObject)
  Private
    Fposition : TPosition;
    FpercentComplete : integer;
    FremainingTime : String;
  Protected
    //Property setters
    Procedure Setposition(AIndex : Integer; const AValue : TPosition); virtual;
    Procedure SetpercentComplete(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetremainingTime(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property position : TPosition Index 0 Read Fposition Write Setposition;
    Property percentComplete : integer Index 8 Read FpercentComplete Write SetpercentComplete;
    Property remainingTime : String Index 16 Read FremainingTime Write SetremainingTime;
  end;
  TApproximateProgressClass = Class of TApproximateProgress;
  
  { --------------------------------------------------------------------
    TReportWorkItemStatusResponse
    --------------------------------------------------------------------}
  
  TReportWorkItemStatusResponse = Class(TGoogleBaseObject)
  Private
    FworkItemServiceStates : TReportWorkItemStatusResponseTypeworkItemServiceStatesArray;
  Protected
    //Property setters
    Procedure SetworkItemServiceStates(AIndex : Integer; const AValue : TReportWorkItemStatusResponseTypeworkItemServiceStatesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property workItemServiceStates : TReportWorkItemStatusResponseTypeworkItemServiceStatesArray Index 0 Read FworkItemServiceStates Write SetworkItemServiceStates;
  end;
  TReportWorkItemStatusResponseClass = Class of TReportWorkItemStatusResponse;
  
  { --------------------------------------------------------------------
    TWorkItemServiceStateTypeharnessData
    --------------------------------------------------------------------}
  
  TWorkItemServiceStateTypeharnessData = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TWorkItemServiceStateTypeharnessDataClass = Class of TWorkItemServiceStateTypeharnessData;
  
  { --------------------------------------------------------------------
    TWorkItemServiceState
    --------------------------------------------------------------------}
  
  TWorkItemServiceState = Class(TGoogleBaseObject)
  Private
    FsplitRequest : TApproximateSplitRequest;
    FleaseExpireTime : String;
    FreportStatusInterval : String;
    FharnessData : TWorkItemServiceStateTypeharnessData;
    FnextReportIndex : String;
    FsuggestedStopPosition : TPosition;
    FsuggestedStopPoint : TApproximateProgress;
  Protected
    //Property setters
    Procedure SetsplitRequest(AIndex : Integer; const AValue : TApproximateSplitRequest); virtual;
    Procedure SetleaseExpireTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreportStatusInterval(AIndex : Integer; const AValue : String); virtual;
    Procedure SetharnessData(AIndex : Integer; const AValue : TWorkItemServiceStateTypeharnessData); virtual;
    Procedure SetnextReportIndex(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsuggestedStopPosition(AIndex : Integer; const AValue : TPosition); virtual;
    Procedure SetsuggestedStopPoint(AIndex : Integer; const AValue : TApproximateProgress); virtual;
  Public
  Published
    Property splitRequest : TApproximateSplitRequest Index 0 Read FsplitRequest Write SetsplitRequest;
    Property leaseExpireTime : String Index 8 Read FleaseExpireTime Write SetleaseExpireTime;
    Property reportStatusInterval : String Index 16 Read FreportStatusInterval Write SetreportStatusInterval;
    Property harnessData : TWorkItemServiceStateTypeharnessData Index 24 Read FharnessData Write SetharnessData;
    Property nextReportIndex : String Index 32 Read FnextReportIndex Write SetnextReportIndex;
    Property suggestedStopPosition : TPosition Index 40 Read FsuggestedStopPosition Write SetsuggestedStopPosition;
    Property suggestedStopPoint : TApproximateProgress Index 48 Read FsuggestedStopPoint Write SetsuggestedStopPoint;
  end;
  TWorkItemServiceStateClass = Class of TWorkItemServiceState;
  
  { --------------------------------------------------------------------
    TApproximateSplitRequest
    --------------------------------------------------------------------}
  
  TApproximateSplitRequest = Class(TGoogleBaseObject)
  Private
    Fposition : TPosition;
    FfractionConsumed : double;
  Protected
    //Property setters
    Procedure Setposition(AIndex : Integer; const AValue : TPosition); virtual;
    Procedure SetfractionConsumed(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property position : TPosition Index 0 Read Fposition Write Setposition;
    Property fractionConsumed : double Index 8 Read FfractionConsumed Write SetfractionConsumed;
  end;
  TApproximateSplitRequestClass = Class of TApproximateSplitRequest;
  
  { --------------------------------------------------------------------
    TLeaseWorkItemRequest
    --------------------------------------------------------------------}
  
  TLeaseWorkItemRequest = Class(TGoogleBaseObject)
  Private
    FworkItemTypes : TStringArray;
    FworkerCapabilities : TStringArray;
    FrequestedLeaseDuration : String;
    FcurrentWorkerTime : String;
    FworkerId : String;
  Protected
    //Property setters
    Procedure SetworkItemTypes(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetworkerCapabilities(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetrequestedLeaseDuration(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcurrentWorkerTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetworkerId(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property workItemTypes : TStringArray Index 0 Read FworkItemTypes Write SetworkItemTypes;
    Property workerCapabilities : TStringArray Index 8 Read FworkerCapabilities Write SetworkerCapabilities;
    Property requestedLeaseDuration : String Index 16 Read FrequestedLeaseDuration Write SetrequestedLeaseDuration;
    Property currentWorkerTime : String Index 24 Read FcurrentWorkerTime Write SetcurrentWorkerTime;
    Property workerId : String Index 32 Read FworkerId Write SetworkerId;
  end;
  TLeaseWorkItemRequestClass = Class of TLeaseWorkItemRequest;
  
  { --------------------------------------------------------------------
    TLeaseWorkItemResponse
    --------------------------------------------------------------------}
  
  TLeaseWorkItemResponse = Class(TGoogleBaseObject)
  Private
    FworkItems : TLeaseWorkItemResponseTypeworkItemsArray;
  Protected
    //Property setters
    Procedure SetworkItems(AIndex : Integer; const AValue : TLeaseWorkItemResponseTypeworkItemsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property workItems : TLeaseWorkItemResponseTypeworkItemsArray Index 0 Read FworkItems Write SetworkItems;
  end;
  TLeaseWorkItemResponseClass = Class of TLeaseWorkItemResponse;
  
  { --------------------------------------------------------------------
    TWorkItem
    --------------------------------------------------------------------}
  
  TWorkItem = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FprojectId : String;
    FjobId : String;
    Fpackages : TWorkItemTypepackagesArray;
    FmapTask : TMapTask;
    FseqMapTask : TSeqMapTask;
    FshellTask : TShellTask;
    FstreamingSetupTask : TStreamingSetupTask;
    FsourceOperationTask : TSourceOperationRequest;
    FstreamingComputationTask : TStreamingComputationTask;
    FreportStatusInterval : String;
    FleaseExpireTime : String;
    Fconfiguration : String;
    FinitialReportIndex : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetjobId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpackages(AIndex : Integer; const AValue : TWorkItemTypepackagesArray); virtual;
    Procedure SetmapTask(AIndex : Integer; const AValue : TMapTask); virtual;
    Procedure SetseqMapTask(AIndex : Integer; const AValue : TSeqMapTask); virtual;
    Procedure SetshellTask(AIndex : Integer; const AValue : TShellTask); virtual;
    Procedure SetstreamingSetupTask(AIndex : Integer; const AValue : TStreamingSetupTask); virtual;
    Procedure SetsourceOperationTask(AIndex : Integer; const AValue : TSourceOperationRequest); virtual;
    Procedure SetstreamingComputationTask(AIndex : Integer; const AValue : TStreamingComputationTask); virtual;
    Procedure SetreportStatusInterval(AIndex : Integer; const AValue : String); virtual;
    Procedure SetleaseExpireTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setconfiguration(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinitialReportIndex(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property projectId : String Index 8 Read FprojectId Write SetprojectId;
    Property jobId : String Index 16 Read FjobId Write SetjobId;
    Property packages : TWorkItemTypepackagesArray Index 24 Read Fpackages Write Setpackages;
    Property mapTask : TMapTask Index 32 Read FmapTask Write SetmapTask;
    Property seqMapTask : TSeqMapTask Index 40 Read FseqMapTask Write SetseqMapTask;
    Property shellTask : TShellTask Index 48 Read FshellTask Write SetshellTask;
    Property streamingSetupTask : TStreamingSetupTask Index 56 Read FstreamingSetupTask Write SetstreamingSetupTask;
    Property sourceOperationTask : TSourceOperationRequest Index 64 Read FsourceOperationTask Write SetsourceOperationTask;
    Property streamingComputationTask : TStreamingComputationTask Index 72 Read FstreamingComputationTask Write SetstreamingComputationTask;
    Property reportStatusInterval : String Index 80 Read FreportStatusInterval Write SetreportStatusInterval;
    Property leaseExpireTime : String Index 88 Read FleaseExpireTime Write SetleaseExpireTime;
    Property configuration : String Index 96 Read Fconfiguration Write Setconfiguration;
    Property initialReportIndex : String Index 104 Read FinitialReportIndex Write SetinitialReportIndex;
  end;
  TWorkItemClass = Class of TWorkItem;
  
  { --------------------------------------------------------------------
    TMapTask
    --------------------------------------------------------------------}
  
  TMapTask = Class(TGoogleBaseObject)
  Private
    Finstructions : TMapTaskTypeinstructionsArray;
    FsystemName : String;
    FstageName : String;
  Protected
    //Property setters
    Procedure Setinstructions(AIndex : Integer; const AValue : TMapTaskTypeinstructionsArray); virtual;
    Procedure SetsystemName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstageName(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property instructions : TMapTaskTypeinstructionsArray Index 0 Read Finstructions Write Setinstructions;
    Property systemName : String Index 8 Read FsystemName Write SetsystemName;
    Property stageName : String Index 16 Read FstageName Write SetstageName;
  end;
  TMapTaskClass = Class of TMapTask;
  
  { --------------------------------------------------------------------
    TParallelInstruction
    --------------------------------------------------------------------}
  
  TParallelInstruction = Class(TGoogleBaseObject)
  Private
    FsystemName : String;
    Fname : String;
    Fread : TReadInstruction;
    Fwrite : TWriteInstruction;
    FparDo : TParDoInstruction;
    FpartialGroupByKey : TPartialGroupByKeyInstruction;
    Fflatten : TFlattenInstruction;
    Foutputs : TParallelInstructionTypeoutputsArray;
  Protected
    //Property setters
    Procedure SetsystemName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setread(AIndex : Integer; const AValue : TReadInstruction); virtual;
    Procedure Setwrite(AIndex : Integer; const AValue : TWriteInstruction); virtual;
    Procedure SetparDo(AIndex : Integer; const AValue : TParDoInstruction); virtual;
    Procedure SetpartialGroupByKey(AIndex : Integer; const AValue : TPartialGroupByKeyInstruction); virtual;
    Procedure Setflatten(AIndex : Integer; const AValue : TFlattenInstruction); virtual;
    Procedure Setoutputs(AIndex : Integer; const AValue : TParallelInstructionTypeoutputsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property systemName : String Index 0 Read FsystemName Write SetsystemName;
    Property name : String Index 8 Read Fname Write Setname;
    Property read : TReadInstruction Index 16 Read Fread Write Setread;
    Property write : TWriteInstruction Index 24 Read Fwrite Write Setwrite;
    Property parDo : TParDoInstruction Index 32 Read FparDo Write SetparDo;
    Property partialGroupByKey : TPartialGroupByKeyInstruction Index 40 Read FpartialGroupByKey Write SetpartialGroupByKey;
    Property flatten : TFlattenInstruction Index 48 Read Fflatten Write Setflatten;
    Property outputs : TParallelInstructionTypeoutputsArray Index 56 Read Foutputs Write Setoutputs;
  end;
  TParallelInstructionClass = Class of TParallelInstruction;
  
  { --------------------------------------------------------------------
    TReadInstruction
    --------------------------------------------------------------------}
  
  TReadInstruction = Class(TGoogleBaseObject)
  Private
    Fsource : TSource;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; const AValue : TSource); virtual;
  Public
  Published
    Property source : TSource Index 0 Read Fsource Write Setsource;
  end;
  TReadInstructionClass = Class of TReadInstruction;
  
  { --------------------------------------------------------------------
    TWriteInstruction
    --------------------------------------------------------------------}
  
  TWriteInstruction = Class(TGoogleBaseObject)
  Private
    Finput : TInstructionInput;
    Fsink : TSink;
  Protected
    //Property setters
    Procedure Setinput(AIndex : Integer; const AValue : TInstructionInput); virtual;
    Procedure Setsink(AIndex : Integer; const AValue : TSink); virtual;
  Public
  Published
    Property input : TInstructionInput Index 0 Read Finput Write Setinput;
    Property sink : TSink Index 8 Read Fsink Write Setsink;
  end;
  TWriteInstructionClass = Class of TWriteInstruction;
  
  { --------------------------------------------------------------------
    TInstructionInput
    --------------------------------------------------------------------}
  
  TInstructionInput = Class(TGoogleBaseObject)
  Private
    FproducerInstructionIndex : integer;
    FoutputNum : integer;
  Protected
    //Property setters
    Procedure SetproducerInstructionIndex(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetoutputNum(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property producerInstructionIndex : integer Index 0 Read FproducerInstructionIndex Write SetproducerInstructionIndex;
    Property outputNum : integer Index 8 Read FoutputNum Write SetoutputNum;
  end;
  TInstructionInputClass = Class of TInstructionInput;
  
  { --------------------------------------------------------------------
    TSinkTypespec
    --------------------------------------------------------------------}
  
  TSinkTypespec = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSinkTypespecClass = Class of TSinkTypespec;
  
  { --------------------------------------------------------------------
    TSinkTypecodec
    --------------------------------------------------------------------}
  
  TSinkTypecodec = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSinkTypecodecClass = Class of TSinkTypecodec;
  
  { --------------------------------------------------------------------
    TSink
    --------------------------------------------------------------------}
  
  TSink = Class(TGoogleBaseObject)
  Private
    Fspec : TSinkTypespec;
    Fcodec : TSinkTypecodec;
  Protected
    //Property setters
    Procedure Setspec(AIndex : Integer; const AValue : TSinkTypespec); virtual;
    Procedure Setcodec(AIndex : Integer; const AValue : TSinkTypecodec); virtual;
  Public
  Published
    Property spec : TSinkTypespec Index 0 Read Fspec Write Setspec;
    Property codec : TSinkTypecodec Index 8 Read Fcodec Write Setcodec;
  end;
  TSinkClass = Class of TSink;
  
  { --------------------------------------------------------------------
    TParDoInstructionTypeuserFn
    --------------------------------------------------------------------}
  
  TParDoInstructionTypeuserFn = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TParDoInstructionTypeuserFnClass = Class of TParDoInstructionTypeuserFn;
  
  { --------------------------------------------------------------------
    TParDoInstruction
    --------------------------------------------------------------------}
  
  TParDoInstruction = Class(TGoogleBaseObject)
  Private
    Finput : TInstructionInput;
    FsideInputs : TParDoInstructionTypesideInputsArray;
    FuserFn : TParDoInstructionTypeuserFn;
    FnumOutputs : integer;
    FmultiOutputInfos : TParDoInstructionTypemultiOutputInfosArray;
  Protected
    //Property setters
    Procedure Setinput(AIndex : Integer; const AValue : TInstructionInput); virtual;
    Procedure SetsideInputs(AIndex : Integer; const AValue : TParDoInstructionTypesideInputsArray); virtual;
    Procedure SetuserFn(AIndex : Integer; const AValue : TParDoInstructionTypeuserFn); virtual;
    Procedure SetnumOutputs(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetmultiOutputInfos(AIndex : Integer; const AValue : TParDoInstructionTypemultiOutputInfosArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property input : TInstructionInput Index 0 Read Finput Write Setinput;
    Property sideInputs : TParDoInstructionTypesideInputsArray Index 8 Read FsideInputs Write SetsideInputs;
    Property userFn : TParDoInstructionTypeuserFn Index 16 Read FuserFn Write SetuserFn;
    Property numOutputs : integer Index 24 Read FnumOutputs Write SetnumOutputs;
    Property multiOutputInfos : TParDoInstructionTypemultiOutputInfosArray Index 32 Read FmultiOutputInfos Write SetmultiOutputInfos;
  end;
  TParDoInstructionClass = Class of TParDoInstruction;
  
  { --------------------------------------------------------------------
    TSideInputInfoTypekind
    --------------------------------------------------------------------}
  
  TSideInputInfoTypekind = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSideInputInfoTypekindClass = Class of TSideInputInfoTypekind;
  
  { --------------------------------------------------------------------
    TSideInputInfo
    --------------------------------------------------------------------}
  
  TSideInputInfo = Class(TGoogleBaseObject)
  Private
    Fsources : TSideInputInfoTypesourcesArray;
    Fkind : TSideInputInfoTypekind;
    Ftag : String;
  Protected
    //Property setters
    Procedure Setsources(AIndex : Integer; const AValue : TSideInputInfoTypesourcesArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : TSideInputInfoTypekind); virtual;
    Procedure Settag(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property sources : TSideInputInfoTypesourcesArray Index 0 Read Fsources Write Setsources;
    Property kind : TSideInputInfoTypekind Index 8 Read Fkind Write Setkind;
    Property tag : String Index 16 Read Ftag Write Settag;
  end;
  TSideInputInfoClass = Class of TSideInputInfo;
  
  { --------------------------------------------------------------------
    TMultiOutputInfo
    --------------------------------------------------------------------}
  
  TMultiOutputInfo = Class(TGoogleBaseObject)
  Private
    Ftag : String;
  Protected
    //Property setters
    Procedure Settag(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property tag : String Index 0 Read Ftag Write Settag;
  end;
  TMultiOutputInfoClass = Class of TMultiOutputInfo;
  
  { --------------------------------------------------------------------
    TPartialGroupByKeyInstructionTypeinputElementCodec
    --------------------------------------------------------------------}
  
  TPartialGroupByKeyInstructionTypeinputElementCodec = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TPartialGroupByKeyInstructionTypeinputElementCodecClass = Class of TPartialGroupByKeyInstructionTypeinputElementCodec;
  
  { --------------------------------------------------------------------
    TPartialGroupByKeyInstructionTypevalueCombiningFn
    --------------------------------------------------------------------}
  
  TPartialGroupByKeyInstructionTypevalueCombiningFn = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TPartialGroupByKeyInstructionTypevalueCombiningFnClass = Class of TPartialGroupByKeyInstructionTypevalueCombiningFn;
  
  { --------------------------------------------------------------------
    TPartialGroupByKeyInstruction
    --------------------------------------------------------------------}
  
  TPartialGroupByKeyInstruction = Class(TGoogleBaseObject)
  Private
    Finput : TInstructionInput;
    FinputElementCodec : TPartialGroupByKeyInstructionTypeinputElementCodec;
    FvalueCombiningFn : TPartialGroupByKeyInstructionTypevalueCombiningFn;
    FsideInputs : TPartialGroupByKeyInstructionTypesideInputsArray;
  Protected
    //Property setters
    Procedure Setinput(AIndex : Integer; const AValue : TInstructionInput); virtual;
    Procedure SetinputElementCodec(AIndex : Integer; const AValue : TPartialGroupByKeyInstructionTypeinputElementCodec); virtual;
    Procedure SetvalueCombiningFn(AIndex : Integer; const AValue : TPartialGroupByKeyInstructionTypevalueCombiningFn); virtual;
    Procedure SetsideInputs(AIndex : Integer; const AValue : TPartialGroupByKeyInstructionTypesideInputsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property input : TInstructionInput Index 0 Read Finput Write Setinput;
    Property inputElementCodec : TPartialGroupByKeyInstructionTypeinputElementCodec Index 8 Read FinputElementCodec Write SetinputElementCodec;
    Property valueCombiningFn : TPartialGroupByKeyInstructionTypevalueCombiningFn Index 16 Read FvalueCombiningFn Write SetvalueCombiningFn;
    Property sideInputs : TPartialGroupByKeyInstructionTypesideInputsArray Index 24 Read FsideInputs Write SetsideInputs;
  end;
  TPartialGroupByKeyInstructionClass = Class of TPartialGroupByKeyInstruction;
  
  { --------------------------------------------------------------------
    TFlattenInstruction
    --------------------------------------------------------------------}
  
  TFlattenInstruction = Class(TGoogleBaseObject)
  Private
    Finputs : TFlattenInstructionTypeinputsArray;
  Protected
    //Property setters
    Procedure Setinputs(AIndex : Integer; const AValue : TFlattenInstructionTypeinputsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property inputs : TFlattenInstructionTypeinputsArray Index 0 Read Finputs Write Setinputs;
  end;
  TFlattenInstructionClass = Class of TFlattenInstruction;
  
  { --------------------------------------------------------------------
    TInstructionOutputTypecodec
    --------------------------------------------------------------------}
  
  TInstructionOutputTypecodec = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TInstructionOutputTypecodecClass = Class of TInstructionOutputTypecodec;
  
  { --------------------------------------------------------------------
    TInstructionOutput
    --------------------------------------------------------------------}
  
  TInstructionOutput = Class(TGoogleBaseObject)
  Private
    Fname : String;
    FsystemName : String;
    Fcodec : TInstructionOutputTypecodec;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsystemName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcodec(AIndex : Integer; const AValue : TInstructionOutputTypecodec); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property systemName : String Index 8 Read FsystemName Write SetsystemName;
    Property codec : TInstructionOutputTypecodec Index 16 Read Fcodec Write Setcodec;
  end;
  TInstructionOutputClass = Class of TInstructionOutput;
  
  { --------------------------------------------------------------------
    TSeqMapTaskTypeuserFn
    --------------------------------------------------------------------}
  
  TSeqMapTaskTypeuserFn = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSeqMapTaskTypeuserFnClass = Class of TSeqMapTaskTypeuserFn;
  
  { --------------------------------------------------------------------
    TSeqMapTask
    --------------------------------------------------------------------}
  
  TSeqMapTask = Class(TGoogleBaseObject)
  Private
    Finputs : TSeqMapTaskTypeinputsArray;
    FuserFn : TSeqMapTaskTypeuserFn;
    FoutputInfos : TSeqMapTaskTypeoutputInfosArray;
    Fname : String;
    FsystemName : String;
    FstageName : String;
  Protected
    //Property setters
    Procedure Setinputs(AIndex : Integer; const AValue : TSeqMapTaskTypeinputsArray); virtual;
    Procedure SetuserFn(AIndex : Integer; const AValue : TSeqMapTaskTypeuserFn); virtual;
    Procedure SetoutputInfos(AIndex : Integer; const AValue : TSeqMapTaskTypeoutputInfosArray); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsystemName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstageName(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property inputs : TSeqMapTaskTypeinputsArray Index 0 Read Finputs Write Setinputs;
    Property userFn : TSeqMapTaskTypeuserFn Index 8 Read FuserFn Write SetuserFn;
    Property outputInfos : TSeqMapTaskTypeoutputInfosArray Index 16 Read FoutputInfos Write SetoutputInfos;
    Property name : String Index 24 Read Fname Write Setname;
    Property systemName : String Index 32 Read FsystemName Write SetsystemName;
    Property stageName : String Index 40 Read FstageName Write SetstageName;
  end;
  TSeqMapTaskClass = Class of TSeqMapTask;
  
  { --------------------------------------------------------------------
    TSeqMapTaskOutputInfo
    --------------------------------------------------------------------}
  
  TSeqMapTaskOutputInfo = Class(TGoogleBaseObject)
  Private
    Ftag : String;
    Fsink : TSink;
  Protected
    //Property setters
    Procedure Settag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsink(AIndex : Integer; const AValue : TSink); virtual;
  Public
  Published
    Property tag : String Index 0 Read Ftag Write Settag;
    Property sink : TSink Index 8 Read Fsink Write Setsink;
  end;
  TSeqMapTaskOutputInfoClass = Class of TSeqMapTaskOutputInfo;
  
  { --------------------------------------------------------------------
    TShellTask
    --------------------------------------------------------------------}
  
  TShellTask = Class(TGoogleBaseObject)
  Private
    Fcommand : String;
    FexitCode : integer;
  Protected
    //Property setters
    Procedure Setcommand(AIndex : Integer; const AValue : String); virtual;
    Procedure SetexitCode(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property command : String Index 0 Read Fcommand Write Setcommand;
    Property exitCode : integer Index 8 Read FexitCode Write SetexitCode;
  end;
  TShellTaskClass = Class of TShellTask;
  
  { --------------------------------------------------------------------
    TStreamingSetupTask
    --------------------------------------------------------------------}
  
  TStreamingSetupTask = Class(TGoogleBaseObject)
  Private
    FreceiveWorkPort : integer;
    FworkerHarnessPort : integer;
    FstreamingComputationTopology : TTopologyConfig;
    Fdrain : boolean;
  Protected
    //Property setters
    Procedure SetreceiveWorkPort(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetworkerHarnessPort(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetstreamingComputationTopology(AIndex : Integer; const AValue : TTopologyConfig); virtual;
    Procedure Setdrain(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property receiveWorkPort : integer Index 0 Read FreceiveWorkPort Write SetreceiveWorkPort;
    Property workerHarnessPort : integer Index 8 Read FworkerHarnessPort Write SetworkerHarnessPort;
    Property streamingComputationTopology : TTopologyConfig Index 16 Read FstreamingComputationTopology Write SetstreamingComputationTopology;
    Property drain : boolean Index 24 Read Fdrain Write Setdrain;
  end;
  TStreamingSetupTaskClass = Class of TStreamingSetupTask;
  
  { --------------------------------------------------------------------
    TTopologyConfigTypeuserStageToComputationNameMap
    --------------------------------------------------------------------}
  
  TTopologyConfigTypeuserStageToComputationNameMap = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TTopologyConfigTypeuserStageToComputationNameMapClass = Class of TTopologyConfigTypeuserStageToComputationNameMap;
  
  { --------------------------------------------------------------------
    TTopologyConfig
    --------------------------------------------------------------------}
  
  TTopologyConfig = Class(TGoogleBaseObject)
  Private
    Fcomputations : TTopologyConfigTypecomputationsArray;
    FdataDiskAssignments : TTopologyConfigTypedataDiskAssignmentsArray;
    FuserStageToComputationNameMap : TTopologyConfigTypeuserStageToComputationNameMap;
    FforwardingKeyBits : integer;
    FpersistentStateVersion : integer;
  Protected
    //Property setters
    Procedure Setcomputations(AIndex : Integer; const AValue : TTopologyConfigTypecomputationsArray); virtual;
    Procedure SetdataDiskAssignments(AIndex : Integer; const AValue : TTopologyConfigTypedataDiskAssignmentsArray); virtual;
    Procedure SetuserStageToComputationNameMap(AIndex : Integer; const AValue : TTopologyConfigTypeuserStageToComputationNameMap); virtual;
    Procedure SetforwardingKeyBits(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetpersistentStateVersion(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property computations : TTopologyConfigTypecomputationsArray Index 0 Read Fcomputations Write Setcomputations;
    Property dataDiskAssignments : TTopologyConfigTypedataDiskAssignmentsArray Index 8 Read FdataDiskAssignments Write SetdataDiskAssignments;
    Property userStageToComputationNameMap : TTopologyConfigTypeuserStageToComputationNameMap Index 16 Read FuserStageToComputationNameMap Write SetuserStageToComputationNameMap;
    Property forwardingKeyBits : integer Index 24 Read FforwardingKeyBits Write SetforwardingKeyBits;
    Property persistentStateVersion : integer Index 32 Read FpersistentStateVersion Write SetpersistentStateVersion;
  end;
  TTopologyConfigClass = Class of TTopologyConfig;
  
  { --------------------------------------------------------------------
    TComputationTopology
    --------------------------------------------------------------------}
  
  TComputationTopology = Class(TGoogleBaseObject)
  Private
    FsystemStageName : String;
    FcomputationId : String;
    FuserStageName : String;
    FkeyRanges : TComputationTopologyTypekeyRangesArray;
    Finputs : TComputationTopologyTypeinputsArray;
    Foutputs : TComputationTopologyTypeoutputsArray;
    FstateFamilies : TComputationTopologyTypestateFamiliesArray;
  Protected
    //Property setters
    Procedure SetsystemStageName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcomputationId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserStageName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetkeyRanges(AIndex : Integer; const AValue : TComputationTopologyTypekeyRangesArray); virtual;
    Procedure Setinputs(AIndex : Integer; const AValue : TComputationTopologyTypeinputsArray); virtual;
    Procedure Setoutputs(AIndex : Integer; const AValue : TComputationTopologyTypeoutputsArray); virtual;
    Procedure SetstateFamilies(AIndex : Integer; const AValue : TComputationTopologyTypestateFamiliesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property systemStageName : String Index 0 Read FsystemStageName Write SetsystemStageName;
    Property computationId : String Index 8 Read FcomputationId Write SetcomputationId;
    Property userStageName : String Index 16 Read FuserStageName Write SetuserStageName;
    Property keyRanges : TComputationTopologyTypekeyRangesArray Index 24 Read FkeyRanges Write SetkeyRanges;
    Property inputs : TComputationTopologyTypeinputsArray Index 32 Read Finputs Write Setinputs;
    Property outputs : TComputationTopologyTypeoutputsArray Index 40 Read Foutputs Write Setoutputs;
    Property stateFamilies : TComputationTopologyTypestateFamiliesArray Index 48 Read FstateFamilies Write SetstateFamilies;
  end;
  TComputationTopologyClass = Class of TComputationTopology;
  
  { --------------------------------------------------------------------
    TKeyRangeLocation
    --------------------------------------------------------------------}
  
  TKeyRangeLocation = Class(TGoogleBaseObject)
  Private
    Fstart : String;
    F_end : String;
    FdeliveryEndpoint : String;
    FpersistentDirectory : String;
    FdataDisk : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setstart(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdeliveryEndpoint(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpersistentDirectory(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdataDisk(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property start : String Index 0 Read Fstart Write Setstart;
    Property _end : String Index 8 Read F_end Write Set_end;
    Property deliveryEndpoint : String Index 16 Read FdeliveryEndpoint Write SetdeliveryEndpoint;
    Property persistentDirectory : String Index 24 Read FpersistentDirectory Write SetpersistentDirectory;
    Property dataDisk : String Index 32 Read FdataDisk Write SetdataDisk;
  end;
  TKeyRangeLocationClass = Class of TKeyRangeLocation;
  
  { --------------------------------------------------------------------
    TStreamLocation
    --------------------------------------------------------------------}
  
  TStreamLocation = Class(TGoogleBaseObject)
  Private
    FstreamingStageLocation : TStreamingStageLocation;
    FpubsubLocation : TPubsubLocation;
    FsideInputLocation : TStreamingSideInputLocation;
    FcustomSourceLocation : TCustomSourceLocation;
  Protected
    //Property setters
    Procedure SetstreamingStageLocation(AIndex : Integer; const AValue : TStreamingStageLocation); virtual;
    Procedure SetpubsubLocation(AIndex : Integer; const AValue : TPubsubLocation); virtual;
    Procedure SetsideInputLocation(AIndex : Integer; const AValue : TStreamingSideInputLocation); virtual;
    Procedure SetcustomSourceLocation(AIndex : Integer; const AValue : TCustomSourceLocation); virtual;
  Public
  Published
    Property streamingStageLocation : TStreamingStageLocation Index 0 Read FstreamingStageLocation Write SetstreamingStageLocation;
    Property pubsubLocation : TPubsubLocation Index 8 Read FpubsubLocation Write SetpubsubLocation;
    Property sideInputLocation : TStreamingSideInputLocation Index 16 Read FsideInputLocation Write SetsideInputLocation;
    Property customSourceLocation : TCustomSourceLocation Index 24 Read FcustomSourceLocation Write SetcustomSourceLocation;
  end;
  TStreamLocationClass = Class of TStreamLocation;
  
  { --------------------------------------------------------------------
    TStreamingStageLocation
    --------------------------------------------------------------------}
  
  TStreamingStageLocation = Class(TGoogleBaseObject)
  Private
    FstreamId : String;
  Protected
    //Property setters
    Procedure SetstreamId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property streamId : String Index 0 Read FstreamId Write SetstreamId;
  end;
  TStreamingStageLocationClass = Class of TStreamingStageLocation;
  
  { --------------------------------------------------------------------
    TPubsubLocation
    --------------------------------------------------------------------}
  
  TPubsubLocation = Class(TGoogleBaseObject)
  Private
    Ftopic : String;
    Fsubscription : String;
    FtimestampLabel : String;
    FidLabel : String;
    FdropLateData : boolean;
    FtrackingSubscription : String;
  Protected
    //Property setters
    Procedure Settopic(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsubscription(AIndex : Integer; const AValue : String); virtual;
    Procedure SettimestampLabel(AIndex : Integer; const AValue : String); virtual;
    Procedure SetidLabel(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdropLateData(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SettrackingSubscription(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property topic : String Index 0 Read Ftopic Write Settopic;
    Property subscription : String Index 8 Read Fsubscription Write Setsubscription;
    Property timestampLabel : String Index 16 Read FtimestampLabel Write SettimestampLabel;
    Property idLabel : String Index 24 Read FidLabel Write SetidLabel;
    Property dropLateData : boolean Index 32 Read FdropLateData Write SetdropLateData;
    Property trackingSubscription : String Index 40 Read FtrackingSubscription Write SettrackingSubscription;
  end;
  TPubsubLocationClass = Class of TPubsubLocation;
  
  { --------------------------------------------------------------------
    TStreamingSideInputLocation
    --------------------------------------------------------------------}
  
  TStreamingSideInputLocation = Class(TGoogleBaseObject)
  Private
    Ftag : String;
    FstateFamily : String;
  Protected
    //Property setters
    Procedure Settag(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstateFamily(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property tag : String Index 0 Read Ftag Write Settag;
    Property stateFamily : String Index 8 Read FstateFamily Write SetstateFamily;
  end;
  TStreamingSideInputLocationClass = Class of TStreamingSideInputLocation;
  
  { --------------------------------------------------------------------
    TCustomSourceLocation
    --------------------------------------------------------------------}
  
  TCustomSourceLocation = Class(TGoogleBaseObject)
  Private
    Fstateful : boolean;
  Protected
    //Property setters
    Procedure Setstateful(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property stateful : boolean Index 0 Read Fstateful Write Setstateful;
  end;
  TCustomSourceLocationClass = Class of TCustomSourceLocation;
  
  { --------------------------------------------------------------------
    TStateFamilyConfig
    --------------------------------------------------------------------}
  
  TStateFamilyConfig = Class(TGoogleBaseObject)
  Private
    FstateFamily : String;
    FisRead : boolean;
  Protected
    //Property setters
    Procedure SetstateFamily(AIndex : Integer; const AValue : String); virtual;
    Procedure SetisRead(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property stateFamily : String Index 0 Read FstateFamily Write SetstateFamily;
    Property isRead : boolean Index 8 Read FisRead Write SetisRead;
  end;
  TStateFamilyConfigClass = Class of TStateFamilyConfig;
  
  { --------------------------------------------------------------------
    TDataDiskAssignment
    --------------------------------------------------------------------}
  
  TDataDiskAssignment = Class(TGoogleBaseObject)
  Private
    FvmInstance : String;
    FdataDisks : TStringArray;
  Protected
    //Property setters
    Procedure SetvmInstance(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdataDisks(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property vmInstance : String Index 0 Read FvmInstance Write SetvmInstance;
    Property dataDisks : TStringArray Index 8 Read FdataDisks Write SetdataDisks;
  end;
  TDataDiskAssignmentClass = Class of TDataDiskAssignment;
  
  { --------------------------------------------------------------------
    TSourceOperationRequest
    --------------------------------------------------------------------}
  
  TSourceOperationRequest = Class(TGoogleBaseObject)
  Private
    Fsplit : TSourceSplitRequest;
    FgetMetadata : TSourceGetMetadataRequest;
  Protected
    //Property setters
    Procedure Setsplit(AIndex : Integer; const AValue : TSourceSplitRequest); virtual;
    Procedure SetgetMetadata(AIndex : Integer; const AValue : TSourceGetMetadataRequest); virtual;
  Public
  Published
    Property split : TSourceSplitRequest Index 0 Read Fsplit Write Setsplit;
    Property getMetadata : TSourceGetMetadataRequest Index 8 Read FgetMetadata Write SetgetMetadata;
  end;
  TSourceOperationRequestClass = Class of TSourceOperationRequest;
  
  { --------------------------------------------------------------------
    TSourceSplitRequest
    --------------------------------------------------------------------}
  
  TSourceSplitRequest = Class(TGoogleBaseObject)
  Private
    Fsource : TSource;
    Foptions : TSourceSplitOptions;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; const AValue : TSource); virtual;
    Procedure Setoptions(AIndex : Integer; const AValue : TSourceSplitOptions); virtual;
  Public
  Published
    Property source : TSource Index 0 Read Fsource Write Setsource;
    Property options : TSourceSplitOptions Index 8 Read Foptions Write Setoptions;
  end;
  TSourceSplitRequestClass = Class of TSourceSplitRequest;
  
  { --------------------------------------------------------------------
    TSourceSplitOptions
    --------------------------------------------------------------------}
  
  TSourceSplitOptions = Class(TGoogleBaseObject)
  Private
    FdesiredBundleSizeBytes : String;
    FdesiredShardSizeBytes : String;
  Protected
    //Property setters
    Procedure SetdesiredBundleSizeBytes(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdesiredShardSizeBytes(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property desiredBundleSizeBytes : String Index 0 Read FdesiredBundleSizeBytes Write SetdesiredBundleSizeBytes;
    Property desiredShardSizeBytes : String Index 8 Read FdesiredShardSizeBytes Write SetdesiredShardSizeBytes;
  end;
  TSourceSplitOptionsClass = Class of TSourceSplitOptions;
  
  { --------------------------------------------------------------------
    TSourceGetMetadataRequest
    --------------------------------------------------------------------}
  
  TSourceGetMetadataRequest = Class(TGoogleBaseObject)
  Private
    Fsource : TSource;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; const AValue : TSource); virtual;
  Public
  Published
    Property source : TSource Index 0 Read Fsource Write Setsource;
  end;
  TSourceGetMetadataRequestClass = Class of TSourceGetMetadataRequest;
  
  { --------------------------------------------------------------------
    TStreamingComputationTask
    --------------------------------------------------------------------}
  
  TStreamingComputationTask = Class(TGoogleBaseObject)
  Private
    FtaskType : String;
    FdataDisks : TStreamingComputationTaskTypedataDisksArray;
    FcomputationRanges : TStreamingComputationTaskTypecomputationRangesArray;
  Protected
    //Property setters
    Procedure SettaskType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdataDisks(AIndex : Integer; const AValue : TStreamingComputationTaskTypedataDisksArray); virtual;
    Procedure SetcomputationRanges(AIndex : Integer; const AValue : TStreamingComputationTaskTypecomputationRangesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property taskType : String Index 0 Read FtaskType Write SettaskType;
    Property dataDisks : TStreamingComputationTaskTypedataDisksArray Index 8 Read FdataDisks Write SetdataDisks;
    Property computationRanges : TStreamingComputationTaskTypecomputationRangesArray Index 16 Read FcomputationRanges Write SetcomputationRanges;
  end;
  TStreamingComputationTaskClass = Class of TStreamingComputationTask;
  
  { --------------------------------------------------------------------
    TMountedDataDisk
    --------------------------------------------------------------------}
  
  TMountedDataDisk = Class(TGoogleBaseObject)
  Private
    FdataDisk : String;
  Protected
    //Property setters
    Procedure SetdataDisk(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property dataDisk : String Index 0 Read FdataDisk Write SetdataDisk;
  end;
  TMountedDataDiskClass = Class of TMountedDataDisk;
  
  { --------------------------------------------------------------------
    TStreamingComputationRanges
    --------------------------------------------------------------------}
  
  TStreamingComputationRanges = Class(TGoogleBaseObject)
  Private
    FcomputationId : String;
    FrangeAssignments : TStreamingComputationRangesTyperangeAssignmentsArray;
  Protected
    //Property setters
    Procedure SetcomputationId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrangeAssignments(AIndex : Integer; const AValue : TStreamingComputationRangesTyperangeAssignmentsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property computationId : String Index 0 Read FcomputationId Write SetcomputationId;
    Property rangeAssignments : TStreamingComputationRangesTyperangeAssignmentsArray Index 8 Read FrangeAssignments Write SetrangeAssignments;
  end;
  TStreamingComputationRangesClass = Class of TStreamingComputationRanges;
  
  { --------------------------------------------------------------------
    TKeyRangeDataDiskAssignment
    --------------------------------------------------------------------}
  
  TKeyRangeDataDiskAssignment = Class(TGoogleBaseObject)
  Private
    Fstart : String;
    F_end : String;
    FdataDisk : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setstart(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdataDisk(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property start : String Index 0 Read Fstart Write Setstart;
    Property _end : String Index 8 Read F_end Write Set_end;
    Property dataDisk : String Index 16 Read FdataDisk Write SetdataDisk;
  end;
  TKeyRangeDataDiskAssignmentClass = Class of TKeyRangeDataDiskAssignment;
  
  { --------------------------------------------------------------------
    TSendWorkerMessagesRequest
    --------------------------------------------------------------------}
  
  TSendWorkerMessagesRequest = Class(TGoogleBaseObject)
  Private
    FworkerMessages : TSendWorkerMessagesRequestTypeworkerMessagesArray;
  Protected
    //Property setters
    Procedure SetworkerMessages(AIndex : Integer; const AValue : TSendWorkerMessagesRequestTypeworkerMessagesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property workerMessages : TSendWorkerMessagesRequestTypeworkerMessagesArray Index 0 Read FworkerMessages Write SetworkerMessages;
  end;
  TSendWorkerMessagesRequestClass = Class of TSendWorkerMessagesRequest;
  
  { --------------------------------------------------------------------
    TWorkerMessageTypelabels
    --------------------------------------------------------------------}
  
  TWorkerMessageTypelabels = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TWorkerMessageTypelabelsClass = Class of TWorkerMessageTypelabels;
  
  { --------------------------------------------------------------------
    TWorkerMessage
    --------------------------------------------------------------------}
  
  TWorkerMessage = Class(TGoogleBaseObject)
  Private
    Flabels : TWorkerMessageTypelabels;
    Ftime : String;
    FworkerHealthReport : TWorkerHealthReport;
    FworkerMessageCode : TWorkerMessageCode;
  Protected
    //Property setters
    Procedure Setlabels(AIndex : Integer; const AValue : TWorkerMessageTypelabels); virtual;
    Procedure Settime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetworkerHealthReport(AIndex : Integer; const AValue : TWorkerHealthReport); virtual;
    Procedure SetworkerMessageCode(AIndex : Integer; const AValue : TWorkerMessageCode); virtual;
  Public
  Published
    Property labels : TWorkerMessageTypelabels Index 0 Read Flabels Write Setlabels;
    Property time : String Index 8 Read Ftime Write Settime;
    Property workerHealthReport : TWorkerHealthReport Index 16 Read FworkerHealthReport Write SetworkerHealthReport;
    Property workerMessageCode : TWorkerMessageCode Index 24 Read FworkerMessageCode Write SetworkerMessageCode;
  end;
  TWorkerMessageClass = Class of TWorkerMessage;
  
  { --------------------------------------------------------------------
    TWorkerHealthReportTypepodsItem
    --------------------------------------------------------------------}
  
  TWorkerHealthReportTypepodsItem = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TWorkerHealthReportTypepodsItemClass = Class of TWorkerHealthReportTypepodsItem;
  
  { --------------------------------------------------------------------
    TWorkerHealthReport
    --------------------------------------------------------------------}
  
  TWorkerHealthReport = Class(TGoogleBaseObject)
  Private
    FvmIsHealthy : boolean;
    FvmStartupTime : String;
    FreportInterval : String;
    Fpods : TWorkerHealthReportTypepodsArray;
  Protected
    //Property setters
    Procedure SetvmIsHealthy(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetvmStartupTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreportInterval(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpods(AIndex : Integer; const AValue : TWorkerHealthReportTypepodsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property vmIsHealthy : boolean Index 0 Read FvmIsHealthy Write SetvmIsHealthy;
    Property vmStartupTime : String Index 8 Read FvmStartupTime Write SetvmStartupTime;
    Property reportInterval : String Index 16 Read FreportInterval Write SetreportInterval;
    Property pods : TWorkerHealthReportTypepodsArray Index 24 Read Fpods Write Setpods;
  end;
  TWorkerHealthReportClass = Class of TWorkerHealthReport;
  
  { --------------------------------------------------------------------
    TWorkerMessageCodeTypeparameters
    --------------------------------------------------------------------}
  
  TWorkerMessageCodeTypeparameters = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TWorkerMessageCodeTypeparametersClass = Class of TWorkerMessageCodeTypeparameters;
  
  { --------------------------------------------------------------------
    TWorkerMessageCode
    --------------------------------------------------------------------}
  
  TWorkerMessageCode = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    Fparameters : TWorkerMessageCodeTypeparameters;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; const AValue : String); virtual;
    Procedure Setparameters(AIndex : Integer; const AValue : TWorkerMessageCodeTypeparameters); virtual;
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property parameters : TWorkerMessageCodeTypeparameters Index 8 Read Fparameters Write Setparameters;
  end;
  TWorkerMessageCodeClass = Class of TWorkerMessageCode;
  
  { --------------------------------------------------------------------
    TSendWorkerMessagesResponse
    --------------------------------------------------------------------}
  
  TSendWorkerMessagesResponse = Class(TGoogleBaseObject)
  Private
    FworkerMessageResponses : TSendWorkerMessagesResponseTypeworkerMessageResponsesArray;
  Protected
    //Property setters
    Procedure SetworkerMessageResponses(AIndex : Integer; const AValue : TSendWorkerMessagesResponseTypeworkerMessageResponsesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property workerMessageResponses : TSendWorkerMessagesResponseTypeworkerMessageResponsesArray Index 0 Read FworkerMessageResponses Write SetworkerMessageResponses;
  end;
  TSendWorkerMessagesResponseClass = Class of TSendWorkerMessagesResponse;
  
  { --------------------------------------------------------------------
    TWorkerMessageResponse
    --------------------------------------------------------------------}
  
  TWorkerMessageResponse = Class(TGoogleBaseObject)
  Private
    FworkerHealthReportResponse : TWorkerHealthReportResponse;
  Protected
    //Property setters
    Procedure SetworkerHealthReportResponse(AIndex : Integer; const AValue : TWorkerHealthReportResponse); virtual;
  Public
  Published
    Property workerHealthReportResponse : TWorkerHealthReportResponse Index 0 Read FworkerHealthReportResponse Write SetworkerHealthReportResponse;
  end;
  TWorkerMessageResponseClass = Class of TWorkerMessageResponse;
  
  { --------------------------------------------------------------------
    TWorkerHealthReportResponse
    --------------------------------------------------------------------}
  
  TWorkerHealthReportResponse = Class(TGoogleBaseObject)
  Private
    FreportInterval : String;
  Protected
    //Property setters
    Procedure SetreportInterval(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property reportInterval : String Index 0 Read FreportInterval Write SetreportInterval;
  end;
  TWorkerHealthReportResponseClass = Class of TWorkerHealthReportResponse;
  
  { --------------------------------------------------------------------
    TProjectsJobsMessagesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsJobsMessagesResource, method List
  
  TProjectsJobsMessagesListOptions = Record
    minimumImportance : String;
    pageSize : integer;
    pageToken : String;
    startTime : String;
    endTime : String;
  end;
  
  TProjectsJobsMessagesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(projectId: string; jobId: string; AQuery : string  = '') : TListJobMessagesResponse;
    Function List(projectId: string; jobId: string; AQuery : TProjectsJobsMessageslistOptions) : TListJobMessagesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsJobsWorkItemsResource
    --------------------------------------------------------------------}
  
  TProjectsJobsWorkItemsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function ReportStatus(projectId: string; jobId: string; aReportWorkItemStatusRequest : TReportWorkItemStatusRequest) : TReportWorkItemStatusResponse;
    Function Lease(projectId: string; jobId: string; aLeaseWorkItemRequest : TLeaseWorkItemRequest) : TLeaseWorkItemResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsJobsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsJobsResource, method Create
  
  TProjectsJobsCreateOptions = Record
    view : String;
    replaceJobId : String;
  end;
  
  
  //Optional query Options for TProjectsJobsResource, method Get
  
  TProjectsJobsGetOptions = Record
    view : String;
  end;
  
  
  //Optional query Options for TProjectsJobsResource, method List
  
  TProjectsJobsListOptions = Record
    filter : String;
    view : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TProjectsJobsResource, method GetMetrics
  
  TProjectsJobsGetMetricsOptions = Record
    startTime : String;
  end;
  
  TProjectsJobsResource = Class(TGoogleResource)
  Private
    FMessagesInstance : TProjectsJobsMessagesResource;
    FWorkItemsInstance : TProjectsJobsWorkItemsResource;
    Function GetMessagesInstance : TProjectsJobsMessagesResource;virtual;
    Function GetWorkItemsInstance : TProjectsJobsWorkItemsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(projectId: string; aJob : TJob; AQuery : string  = '') : TJob;overload;
    Function Create(projectId: string; aJob : TJob; AQuery : TProjectsJobscreateOptions) : TJob;overload;
    Function Get(projectId: string; jobId: string; AQuery : string  = '') : TJob;
    Function Get(projectId: string; jobId: string; AQuery : TProjectsJobsgetOptions) : TJob;
    Function Update(projectId: string; jobId: string; aJob : TJob) : TJob;
    Function List(projectId: string; AQuery : string  = '') : TListJobsResponse;
    Function List(projectId: string; AQuery : TProjectsJobslistOptions) : TListJobsResponse;
    Function GetMetrics(projectId: string; jobId: string; AQuery : string  = '') : TJobMetrics;
    Function GetMetrics(projectId: string; jobId: string; AQuery : TProjectsJobsgetMetricsOptions) : TJobMetrics;
    Function CreateMessagesResource(AOwner : TComponent) : TProjectsJobsMessagesResource;virtual;overload;
    Function CreateMessagesResource : TProjectsJobsMessagesResource;virtual;overload;
    Function CreateWorkItemsResource(AOwner : TComponent) : TProjectsJobsWorkItemsResource;virtual;overload;
    Function CreateWorkItemsResource : TProjectsJobsWorkItemsResource;virtual;overload;
    Property MessagesResource : TProjectsJobsMessagesResource Read GetMessagesInstance;
    Property WorkItemsResource : TProjectsJobsWorkItemsResource Read GetWorkItemsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Private
    FJobsMessagesInstance : TProjectsJobsMessagesResource;
    FJobsWorkItemsInstance : TProjectsJobsWorkItemsResource;
    FJobsInstance : TProjectsJobsResource;
    Function GetJobsMessagesInstance : TProjectsJobsMessagesResource;virtual;
    Function GetJobsWorkItemsInstance : TProjectsJobsWorkItemsResource;virtual;
    Function GetJobsInstance : TProjectsJobsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function WorkerMessages(projectId: string; aSendWorkerMessagesRequest : TSendWorkerMessagesRequest) : TSendWorkerMessagesResponse;
    Function CreateJobsMessagesResource(AOwner : TComponent) : TProjectsJobsMessagesResource;virtual;overload;
    Function CreateJobsMessagesResource : TProjectsJobsMessagesResource;virtual;overload;
    Function CreateJobsWorkItemsResource(AOwner : TComponent) : TProjectsJobsWorkItemsResource;virtual;overload;
    Function CreateJobsWorkItemsResource : TProjectsJobsWorkItemsResource;virtual;overload;
    Function CreateJobsResource(AOwner : TComponent) : TProjectsJobsResource;virtual;overload;
    Function CreateJobsResource : TProjectsJobsResource;virtual;overload;
    Property JobsMessagesResource : TProjectsJobsMessagesResource Read GetJobsMessagesInstance;
    Property JobsWorkItemsResource : TProjectsJobsWorkItemsResource Read GetJobsWorkItemsInstance;
    Property JobsResource : TProjectsJobsResource Read GetJobsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TDataflowAPI
    --------------------------------------------------------------------}
  
  TDataflowAPI = Class(TGoogleAPI)
  Private
    FProjectsJobsMessagesInstance : TProjectsJobsMessagesResource;
    FProjectsJobsWorkItemsInstance : TProjectsJobsWorkItemsResource;
    FProjectsJobsInstance : TProjectsJobsResource;
    FProjectsInstance : TProjectsResource;
    Function GetProjectsJobsMessagesInstance : TProjectsJobsMessagesResource;virtual;
    Function GetProjectsJobsWorkItemsInstance : TProjectsJobsWorkItemsResource;virtual;
    Function GetProjectsJobsInstance : TProjectsJobsResource;virtual;
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
    Function CreateProjectsJobsMessagesResource(AOwner : TComponent) : TProjectsJobsMessagesResource;virtual;overload;
    Function CreateProjectsJobsMessagesResource : TProjectsJobsMessagesResource;virtual;overload;
    Function CreateProjectsJobsWorkItemsResource(AOwner : TComponent) : TProjectsJobsWorkItemsResource;virtual;overload;
    Function CreateProjectsJobsWorkItemsResource : TProjectsJobsWorkItemsResource;virtual;overload;
    Function CreateProjectsJobsResource(AOwner : TComponent) : TProjectsJobsResource;virtual;overload;
    Function CreateProjectsJobsResource : TProjectsJobsResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsJobsMessagesResource : TProjectsJobsMessagesResource Read GetProjectsJobsMessagesInstance;
    Property ProjectsJobsWorkItemsResource : TProjectsJobsWorkItemsResource Read GetProjectsJobsWorkItemsInstance;
    Property ProjectsJobsResource : TProjectsJobsResource Read GetProjectsJobsInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TJobTypetransformNameMapping
  --------------------------------------------------------------------}


Class Function TJobTypetransformNameMapping.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TJob
  --------------------------------------------------------------------}


Procedure TJob.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setenvironment(AIndex : Integer; const AValue : TEnvironment); 

begin
  If (Fenvironment=AValue) then exit;
  Fenvironment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setsteps(AIndex : Integer; const AValue : TJobTypestepsArray); 

begin
  If (Fsteps=AValue) then exit;
  Fsteps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetcurrentState(AIndex : Integer; const AValue : String); 

begin
  If (FcurrentState=AValue) then exit;
  FcurrentState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetcurrentStateTime(AIndex : Integer; const AValue : String); 

begin
  If (FcurrentStateTime=AValue) then exit;
  FcurrentStateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetrequestedState(AIndex : Integer; const AValue : String); 

begin
  If (FrequestedState=AValue) then exit;
  FrequestedState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetexecutionInfo(AIndex : Integer; const AValue : TJobExecutionInfo); 

begin
  If (FexecutionInfo=AValue) then exit;
  FexecutionInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetcreateTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreateTime=AValue) then exit;
  FcreateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetreplaceJobId(AIndex : Integer; const AValue : String); 

begin
  If (FreplaceJobId=AValue) then exit;
  FreplaceJobId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SettransformNameMapping(AIndex : Integer; const AValue : TJobTypetransformNameMapping); 

begin
  If (FtransformNameMapping=AValue) then exit;
  FtransformNameMapping:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetclientRequestId(AIndex : Integer; const AValue : String); 

begin
  If (FclientRequestId=AValue) then exit;
  FclientRequestId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetreplacedByJobId(AIndex : Integer; const AValue : String); 

begin
  If (FreplacedByJobId=AValue) then exit;
  FreplacedByJobId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SettempFiles(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FtempFiles=AValue) then exit;
  FtempFiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TJob.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TJob.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'steps' : SetLength(Fsteps,ALength);
  'tempfiles' : SetLength(FtempFiles,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEnvironmentTypeuserAgent
  --------------------------------------------------------------------}


Class Function TEnvironmentTypeuserAgent.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TEnvironmentTypeversion
  --------------------------------------------------------------------}


Class Function TEnvironmentTypeversion.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TEnvironmentTypesdkPipelineOptions
  --------------------------------------------------------------------}


Class Function TEnvironmentTypesdkPipelineOptions.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TEnvironmentTypeinternalExperiments
  --------------------------------------------------------------------}


Class Function TEnvironmentTypeinternalExperiments.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TEnvironment
  --------------------------------------------------------------------}


Procedure TEnvironment.SettempStoragePrefix(AIndex : Integer; const AValue : String); 

begin
  If (FtempStoragePrefix=AValue) then exit;
  FtempStoragePrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetclusterManagerApiService(AIndex : Integer; const AValue : String); 

begin
  If (FclusterManagerApiService=AValue) then exit;
  FclusterManagerApiService:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.Setexperiments(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fexperiments=AValue) then exit;
  Fexperiments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetworkerPools(AIndex : Integer; const AValue : TEnvironmentTypeworkerPoolsArray); 

begin
  If (FworkerPools=AValue) then exit;
  FworkerPools:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetuserAgent(AIndex : Integer; const AValue : TEnvironmentTypeuserAgent); 

begin
  If (FuserAgent=AValue) then exit;
  FuserAgent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.Setversion(AIndex : Integer; const AValue : TEnvironmentTypeversion); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.Setdataset(AIndex : Integer; const AValue : String); 

begin
  If (Fdataset=AValue) then exit;
  Fdataset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetsdkPipelineOptions(AIndex : Integer; const AValue : TEnvironmentTypesdkPipelineOptions); 

begin
  If (FsdkPipelineOptions=AValue) then exit;
  FsdkPipelineOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetinternalExperiments(AIndex : Integer; const AValue : TEnvironmentTypeinternalExperiments); 

begin
  If (FinternalExperiments=AValue) then exit;
  FinternalExperiments:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TEnvironment.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'experiments' : SetLength(Fexperiments,ALength);
  'workerpools' : SetLength(FworkerPools,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWorkerPoolTypemetadata
  --------------------------------------------------------------------}


Class Function TWorkerPoolTypemetadata.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWorkerPoolTypepoolArgs
  --------------------------------------------------------------------}


Class Function TWorkerPoolTypepoolArgs.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWorkerPool
  --------------------------------------------------------------------}


Procedure TWorkerPool.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetnumWorkers(AIndex : Integer; const AValue : integer); 

begin
  If (FnumWorkers=AValue) then exit;
  FnumWorkers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.Setpackages(AIndex : Integer; const AValue : TWorkerPoolTypepackagesArray); 

begin
  If (Fpackages=AValue) then exit;
  Fpackages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetdefaultPackageSet(AIndex : Integer; const AValue : String); 

begin
  If (FdefaultPackageSet=AValue) then exit;
  FdefaultPackageSet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetmachineType(AIndex : Integer; const AValue : String); 

begin
  If (FmachineType=AValue) then exit;
  FmachineType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetteardownPolicy(AIndex : Integer; const AValue : String); 

begin
  If (FteardownPolicy=AValue) then exit;
  FteardownPolicy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetdiskSizeGb(AIndex : Integer; const AValue : integer); 

begin
  If (FdiskSizeGb=AValue) then exit;
  FdiskSizeGb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetdiskType(AIndex : Integer; const AValue : String); 

begin
  If (FdiskType=AValue) then exit;
  FdiskType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetdiskSourceImage(AIndex : Integer; const AValue : String); 

begin
  If (FdiskSourceImage=AValue) then exit;
  FdiskSourceImage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.Setzone(AIndex : Integer; const AValue : String); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SettaskrunnerSettings(AIndex : Integer; const AValue : TTaskRunnerSettings); 

begin
  If (FtaskrunnerSettings=AValue) then exit;
  FtaskrunnerSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetonHostMaintenance(AIndex : Integer; const AValue : String); 

begin
  If (FonHostMaintenance=AValue) then exit;
  FonHostMaintenance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetdataDisks(AIndex : Integer; const AValue : TWorkerPoolTypedataDisksArray); 

begin
  If (FdataDisks=AValue) then exit;
  FdataDisks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.Setmetadata(AIndex : Integer; const AValue : TWorkerPoolTypemetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetautoscalingSettings(AIndex : Integer; const AValue : TAutoscalingSettings); 

begin
  If (FautoscalingSettings=AValue) then exit;
  FautoscalingSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetpoolArgs(AIndex : Integer; const AValue : TWorkerPoolTypepoolArgs); 

begin
  If (FpoolArgs=AValue) then exit;
  FpoolArgs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.Setnetwork(AIndex : Integer; const AValue : String); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.Setsubnetwork(AIndex : Integer; const AValue : String); 

begin
  If (Fsubnetwork=AValue) then exit;
  Fsubnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetworkerHarnessContainerImage(AIndex : Integer; const AValue : String); 

begin
  If (FworkerHarnessContainerImage=AValue) then exit;
  FworkerHarnessContainerImage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetnumThreadsPerWorker(AIndex : Integer; const AValue : integer); 

begin
  If (FnumThreadsPerWorker=AValue) then exit;
  FnumThreadsPerWorker:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TWorkerPool.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'packages' : SetLength(Fpackages,ALength);
  'datadisks' : SetLength(FdataDisks,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPackage
  --------------------------------------------------------------------}


Procedure TPackage.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPackage.Setlocation(AIndex : Integer; const AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTaskRunnerSettings
  --------------------------------------------------------------------}


Procedure TTaskRunnerSettings.SettaskUser(AIndex : Integer; const AValue : String); 

begin
  If (FtaskUser=AValue) then exit;
  FtaskUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SettaskGroup(AIndex : Integer; const AValue : String); 

begin
  If (FtaskGroup=AValue) then exit;
  FtaskGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetoauthScopes(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FoauthScopes=AValue) then exit;
  FoauthScopes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetbaseUrl(AIndex : Integer; const AValue : String); 

begin
  If (FbaseUrl=AValue) then exit;
  FbaseUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetdataflowApiVersion(AIndex : Integer; const AValue : String); 

begin
  If (FdataflowApiVersion=AValue) then exit;
  FdataflowApiVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetparallelWorkerSettings(AIndex : Integer; const AValue : TWorkerSettings); 

begin
  If (FparallelWorkerSettings=AValue) then exit;
  FparallelWorkerSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetbaseTaskDir(AIndex : Integer; const AValue : String); 

begin
  If (FbaseTaskDir=AValue) then exit;
  FbaseTaskDir:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetcontinueOnException(AIndex : Integer; const AValue : boolean); 

begin
  If (FcontinueOnException=AValue) then exit;
  FcontinueOnException:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetlogToSerialconsole(AIndex : Integer; const AValue : boolean); 

begin
  If (FlogToSerialconsole=AValue) then exit;
  FlogToSerialconsole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.Setalsologtostderr(AIndex : Integer; const AValue : boolean); 

begin
  If (Falsologtostderr=AValue) then exit;
  Falsologtostderr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetlogUploadLocation(AIndex : Integer; const AValue : String); 

begin
  If (FlogUploadLocation=AValue) then exit;
  FlogUploadLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetlogDir(AIndex : Integer; const AValue : String); 

begin
  If (FlogDir=AValue) then exit;
  FlogDir:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SettempStoragePrefix(AIndex : Integer; const AValue : String); 

begin
  If (FtempStoragePrefix=AValue) then exit;
  FtempStoragePrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetharnessCommand(AIndex : Integer; const AValue : String); 

begin
  If (FharnessCommand=AValue) then exit;
  FharnessCommand:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetworkflowFileName(AIndex : Integer; const AValue : String); 

begin
  If (FworkflowFileName=AValue) then exit;
  FworkflowFileName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetcommandlinesFileName(AIndex : Integer; const AValue : String); 

begin
  If (FcommandlinesFileName=AValue) then exit;
  FcommandlinesFileName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetvmId(AIndex : Integer; const AValue : String); 

begin
  If (FvmId=AValue) then exit;
  FvmId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetlanguageHint(AIndex : Integer; const AValue : String); 

begin
  If (FlanguageHint=AValue) then exit;
  FlanguageHint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetstreamingWorkerMainClass(AIndex : Integer; const AValue : String); 

begin
  If (FstreamingWorkerMainClass=AValue) then exit;
  FstreamingWorkerMainClass:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTaskRunnerSettings.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'oauthscopes' : SetLength(FoauthScopes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWorkerSettings
  --------------------------------------------------------------------}


Procedure TWorkerSettings.SetbaseUrl(AIndex : Integer; const AValue : String); 

begin
  If (FbaseUrl=AValue) then exit;
  FbaseUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerSettings.SetreportingEnabled(AIndex : Integer; const AValue : boolean); 

begin
  If (FreportingEnabled=AValue) then exit;
  FreportingEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerSettings.SetservicePath(AIndex : Integer; const AValue : String); 

begin
  If (FservicePath=AValue) then exit;
  FservicePath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerSettings.SetshuffleServicePath(AIndex : Integer; const AValue : String); 

begin
  If (FshuffleServicePath=AValue) then exit;
  FshuffleServicePath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerSettings.SetworkerId(AIndex : Integer; const AValue : String); 

begin
  If (FworkerId=AValue) then exit;
  FworkerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerSettings.SettempStoragePrefix(AIndex : Integer; const AValue : String); 

begin
  If (FtempStoragePrefix=AValue) then exit;
  FtempStoragePrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDisk
  --------------------------------------------------------------------}


Procedure TDisk.SetsizeGb(AIndex : Integer; const AValue : integer); 

begin
  If (FsizeGb=AValue) then exit;
  FsizeGb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.SetdiskType(AIndex : Integer; const AValue : String); 

begin
  If (FdiskType=AValue) then exit;
  FdiskType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.SetmountPoint(AIndex : Integer; const AValue : String); 

begin
  If (FmountPoint=AValue) then exit;
  FmountPoint:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAutoscalingSettings
  --------------------------------------------------------------------}


Procedure TAutoscalingSettings.Setalgorithm(AIndex : Integer; const AValue : String); 

begin
  If (Falgorithm=AValue) then exit;
  Falgorithm:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingSettings.SetmaxNumWorkers(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxNumWorkers=AValue) then exit;
  FmaxNumWorkers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStepTypeproperties
  --------------------------------------------------------------------}


Class Function TStepTypeproperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TStep
  --------------------------------------------------------------------}


Procedure TStep.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.Setproperties(AIndex : Integer; const AValue : TStepTypeproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobExecutionInfoTypestages
  --------------------------------------------------------------------}


Class Function TJobExecutionInfoTypestages.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TJobExecutionInfo
  --------------------------------------------------------------------}


Procedure TJobExecutionInfo.Setstages(AIndex : Integer; const AValue : TJobExecutionInfoTypestages); 

begin
  If (Fstages=AValue) then exit;
  Fstages:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobExecutionStageInfo
  --------------------------------------------------------------------}


Procedure TJobExecutionStageInfo.SetstepName(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FstepName=AValue) then exit;
  FstepName:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TJobExecutionStageInfo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'stepname' : SetLength(FstepName,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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
  TListJobMessagesResponse
  --------------------------------------------------------------------}


Procedure TListJobMessagesResponse.SetjobMessages(AIndex : Integer; const AValue : TListJobMessagesResponseTypejobMessagesArray); 

begin
  If (FjobMessages=AValue) then exit;
  FjobMessages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListJobMessagesResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListJobMessagesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'jobmessages' : SetLength(FjobMessages,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TJobMessage
  --------------------------------------------------------------------}


Procedure TJobMessage.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobMessage.Settime(AIndex : Integer; const AValue : String); 

begin
  If (Ftime=AValue) then exit;
  Ftime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobMessage.SetmessageText(AIndex : Integer; const AValue : String); 

begin
  If (FmessageText=AValue) then exit;
  FmessageText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobMessage.SetmessageImportance(AIndex : Integer; const AValue : String); 

begin
  If (FmessageImportance=AValue) then exit;
  FmessageImportance:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobMetrics
  --------------------------------------------------------------------}


Procedure TJobMetrics.SetmetricTime(AIndex : Integer; const AValue : String); 

begin
  If (FmetricTime=AValue) then exit;
  FmetricTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobMetrics.Setmetrics(AIndex : Integer; const AValue : TJobMetricsTypemetricsArray); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TJobMetrics.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'metrics' : SetLength(Fmetrics,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMetricUpdate
  --------------------------------------------------------------------}


Procedure TMetricUpdate.Setname(AIndex : Integer; const AValue : TMetricStructuredName); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricUpdate.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricUpdate.Setcumulative(AIndex : Integer; const AValue : boolean); 

begin
  If (Fcumulative=AValue) then exit;
  Fcumulative:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricUpdate.Setscalar(AIndex : Integer; const AValue : TJSONSchema); 

begin
  If (Fscalar=AValue) then exit;
  Fscalar:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricUpdate.SetmeanSum(AIndex : Integer; const AValue : TJSONSchema); 

begin
  If (FmeanSum=AValue) then exit;
  FmeanSum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricUpdate.SetmeanCount(AIndex : Integer; const AValue : TJSONSchema); 

begin
  If (FmeanCount=AValue) then exit;
  FmeanCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricUpdate.Set_set(AIndex : Integer; const AValue : TJSONSchema); 

begin
  If (F_set=AValue) then exit;
  F_set:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricUpdate.Setinternal(AIndex : Integer; const AValue : TJSONSchema); 

begin
  If (Finternal=AValue) then exit;
  Finternal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricUpdate.SetupdateTime(AIndex : Integer; const AValue : String); 

begin
  If (FupdateTime=AValue) then exit;
  FupdateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMetricUpdate.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_set' : Result:='set';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TMetricStructuredNameTypecontext
  --------------------------------------------------------------------}


Class Function TMetricStructuredNameTypecontext.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TMetricStructuredName
  --------------------------------------------------------------------}


Procedure TMetricStructuredName.Setorigin(AIndex : Integer; const AValue : String); 

begin
  If (Forigin=AValue) then exit;
  Forigin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricStructuredName.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricStructuredName.Setcontext(AIndex : Integer; const AValue : TMetricStructuredNameTypecontext); 

begin
  If (Fcontext=AValue) then exit;
  Fcontext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportWorkItemStatusRequest
  --------------------------------------------------------------------}


Procedure TReportWorkItemStatusRequest.SetworkerId(AIndex : Integer; const AValue : String); 

begin
  If (FworkerId=AValue) then exit;
  FworkerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportWorkItemStatusRequest.SetworkItemStatuses(AIndex : Integer; const AValue : TReportWorkItemStatusRequestTypeworkItemStatusesArray); 

begin
  If (FworkItemStatuses=AValue) then exit;
  FworkItemStatuses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportWorkItemStatusRequest.SetcurrentWorkerTime(AIndex : Integer; const AValue : String); 

begin
  If (FcurrentWorkerTime=AValue) then exit;
  FcurrentWorkerTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TReportWorkItemStatusRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'workitemstatuses' : SetLength(FworkItemStatuses,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWorkItemStatus
  --------------------------------------------------------------------}


Procedure TWorkItemStatus.SetworkItemId(AIndex : Integer; const AValue : String); 

begin
  If (FworkItemId=AValue) then exit;
  FworkItemId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetreportIndex(AIndex : Integer; const AValue : String); 

begin
  If (FreportIndex=AValue) then exit;
  FreportIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetrequestedLeaseDuration(AIndex : Integer; const AValue : String); 

begin
  If (FrequestedLeaseDuration=AValue) then exit;
  FrequestedLeaseDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.Setcompleted(AIndex : Integer; const AValue : boolean); 

begin
  If (Fcompleted=AValue) then exit;
  Fcompleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.Seterrors(AIndex : Integer; const AValue : TWorkItemStatusTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetmetricUpdates(AIndex : Integer; const AValue : TWorkItemStatusTypemetricUpdatesArray); 

begin
  If (FmetricUpdates=AValue) then exit;
  FmetricUpdates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetreportedProgress(AIndex : Integer; const AValue : TApproximateReportedProgress); 

begin
  If (FreportedProgress=AValue) then exit;
  FreportedProgress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetstopPosition(AIndex : Integer; const AValue : TPosition); 

begin
  If (FstopPosition=AValue) then exit;
  FstopPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetdynamicSourceSplit(AIndex : Integer; const AValue : TDynamicSourceSplit); 

begin
  If (FdynamicSourceSplit=AValue) then exit;
  FdynamicSourceSplit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetsourceOperationResponse(AIndex : Integer; const AValue : TSourceOperationResponse); 

begin
  If (FsourceOperationResponse=AValue) then exit;
  FsourceOperationResponse:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetsourceFork(AIndex : Integer; const AValue : TSourceFork); 

begin
  If (FsourceFork=AValue) then exit;
  FsourceFork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.Setprogress(AIndex : Integer; const AValue : TApproximateProgress); 

begin
  If (Fprogress=AValue) then exit;
  Fprogress:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TWorkItemStatus.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  'metricupdates' : SetLength(FmetricUpdates,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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
  TApproximateReportedProgress
  --------------------------------------------------------------------}


Procedure TApproximateReportedProgress.Setposition(AIndex : Integer; const AValue : TPosition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApproximateReportedProgress.SetfractionConsumed(AIndex : Integer; const AValue : double); 

begin
  If (FfractionConsumed=AValue) then exit;
  FfractionConsumed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApproximateReportedProgress.SetremainingParallelism(AIndex : Integer; const AValue : TReportedParallelism); 

begin
  If (FremainingParallelism=AValue) then exit;
  FremainingParallelism:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApproximateReportedProgress.SetconsumedParallelism(AIndex : Integer; const AValue : TReportedParallelism); 

begin
  If (FconsumedParallelism=AValue) then exit;
  FconsumedParallelism:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPosition
  --------------------------------------------------------------------}


Procedure TPosition.Set_end(AIndex : Integer; const AValue : boolean); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.Setkey(AIndex : Integer; const AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.SetbyteOffset(AIndex : Integer; const AValue : String); 

begin
  If (FbyteOffset=AValue) then exit;
  FbyteOffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.SetrecordIndex(AIndex : Integer; const AValue : String); 

begin
  If (FrecordIndex=AValue) then exit;
  FrecordIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.SetshufflePosition(AIndex : Integer; const AValue : String); 

begin
  If (FshufflePosition=AValue) then exit;
  FshufflePosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.SetconcatPosition(AIndex : Integer; const AValue : TConcatPosition); 

begin
  If (FconcatPosition=AValue) then exit;
  FconcatPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPosition.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TConcatPosition
  --------------------------------------------------------------------}


Procedure TConcatPosition.Setindex(AIndex : Integer; const AValue : integer); 

begin
  If (Findex=AValue) then exit;
  Findex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConcatPosition.Setposition(AIndex : Integer; const AValue : TPosition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportedParallelism
  --------------------------------------------------------------------}


Procedure TReportedParallelism.SetisInfinite(AIndex : Integer; const AValue : boolean); 

begin
  If (FisInfinite=AValue) then exit;
  FisInfinite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportedParallelism.Setvalue(AIndex : Integer; const AValue : double); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDynamicSourceSplit
  --------------------------------------------------------------------}


Procedure TDynamicSourceSplit.Setprimary(AIndex : Integer; const AValue : TDerivedSource); 

begin
  If (Fprimary=AValue) then exit;
  Fprimary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDynamicSourceSplit.Setresidual(AIndex : Integer; const AValue : TDerivedSource); 

begin
  If (Fresidual=AValue) then exit;
  Fresidual:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDerivedSource
  --------------------------------------------------------------------}


Procedure TDerivedSource.Setsource(AIndex : Integer; const AValue : TSource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDerivedSource.SetderivationMode(AIndex : Integer; const AValue : String); 

begin
  If (FderivationMode=AValue) then exit;
  FderivationMode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceTypespec
  --------------------------------------------------------------------}


Class Function TSourceTypespec.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSourceTypecodec
  --------------------------------------------------------------------}


Class Function TSourceTypecodec.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSourceTypebaseSpecsItem
  --------------------------------------------------------------------}


Class Function TSourceTypebaseSpecsItem.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSource
  --------------------------------------------------------------------}


Procedure TSource.Setspec(AIndex : Integer; const AValue : TSourceTypespec); 

begin
  If (Fspec=AValue) then exit;
  Fspec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSource.Setcodec(AIndex : Integer; const AValue : TSourceTypecodec); 

begin
  If (Fcodec=AValue) then exit;
  Fcodec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSource.SetbaseSpecs(AIndex : Integer; const AValue : TSourceTypebaseSpecsArray); 

begin
  If (FbaseSpecs=AValue) then exit;
  FbaseSpecs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSource.Setmetadata(AIndex : Integer; const AValue : TSourceMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSource.SetdoesNotNeedSplitting(AIndex : Integer; const AValue : boolean); 

begin
  If (FdoesNotNeedSplitting=AValue) then exit;
  FdoesNotNeedSplitting:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSource.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'basespecs' : SetLength(FbaseSpecs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSourceMetadata
  --------------------------------------------------------------------}


Procedure TSourceMetadata.SetproducesSortedKeys(AIndex : Integer; const AValue : boolean); 

begin
  If (FproducesSortedKeys=AValue) then exit;
  FproducesSortedKeys:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceMetadata.Setinfinite(AIndex : Integer; const AValue : boolean); 

begin
  If (Finfinite=AValue) then exit;
  Finfinite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceMetadata.SetestimatedSizeBytes(AIndex : Integer; const AValue : String); 

begin
  If (FestimatedSizeBytes=AValue) then exit;
  FestimatedSizeBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceOperationResponse
  --------------------------------------------------------------------}


Procedure TSourceOperationResponse.Setsplit(AIndex : Integer; const AValue : TSourceSplitResponse); 

begin
  If (Fsplit=AValue) then exit;
  Fsplit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceOperationResponse.SetgetMetadata(AIndex : Integer; const AValue : TSourceGetMetadataResponse); 

begin
  If (FgetMetadata=AValue) then exit;
  FgetMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceSplitResponse
  --------------------------------------------------------------------}


Procedure TSourceSplitResponse.Setoutcome(AIndex : Integer; const AValue : String); 

begin
  If (Foutcome=AValue) then exit;
  Foutcome:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceSplitResponse.Setbundles(AIndex : Integer; const AValue : TSourceSplitResponseTypebundlesArray); 

begin
  If (Fbundles=AValue) then exit;
  Fbundles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceSplitResponse.Setshards(AIndex : Integer; const AValue : TSourceSplitResponseTypeshardsArray); 

begin
  If (Fshards=AValue) then exit;
  Fshards:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSourceSplitResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'bundles' : SetLength(Fbundles,ALength);
  'shards' : SetLength(Fshards,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSourceSplitShard
  --------------------------------------------------------------------}


Procedure TSourceSplitShard.Setsource(AIndex : Integer; const AValue : TSource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceSplitShard.SetderivationMode(AIndex : Integer; const AValue : String); 

begin
  If (FderivationMode=AValue) then exit;
  FderivationMode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceGetMetadataResponse
  --------------------------------------------------------------------}


Procedure TSourceGetMetadataResponse.Setmetadata(AIndex : Integer; const AValue : TSourceMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceFork
  --------------------------------------------------------------------}


Procedure TSourceFork.Setprimary(AIndex : Integer; const AValue : TSourceSplitShard); 

begin
  If (Fprimary=AValue) then exit;
  Fprimary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceFork.Setresidual(AIndex : Integer; const AValue : TSourceSplitShard); 

begin
  If (Fresidual=AValue) then exit;
  Fresidual:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceFork.SetprimarySource(AIndex : Integer; const AValue : TDerivedSource); 

begin
  If (FprimarySource=AValue) then exit;
  FprimarySource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceFork.SetresidualSource(AIndex : Integer; const AValue : TDerivedSource); 

begin
  If (FresidualSource=AValue) then exit;
  FresidualSource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApproximateProgress
  --------------------------------------------------------------------}


Procedure TApproximateProgress.Setposition(AIndex : Integer; const AValue : TPosition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApproximateProgress.SetpercentComplete(AIndex : Integer; const AValue : integer); 

begin
  If (FpercentComplete=AValue) then exit;
  FpercentComplete:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApproximateProgress.SetremainingTime(AIndex : Integer; const AValue : String); 

begin
  If (FremainingTime=AValue) then exit;
  FremainingTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportWorkItemStatusResponse
  --------------------------------------------------------------------}


Procedure TReportWorkItemStatusResponse.SetworkItemServiceStates(AIndex : Integer; const AValue : TReportWorkItemStatusResponseTypeworkItemServiceStatesArray); 

begin
  If (FworkItemServiceStates=AValue) then exit;
  FworkItemServiceStates:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TReportWorkItemStatusResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'workitemservicestates' : SetLength(FworkItemServiceStates,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWorkItemServiceStateTypeharnessData
  --------------------------------------------------------------------}


Class Function TWorkItemServiceStateTypeharnessData.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWorkItemServiceState
  --------------------------------------------------------------------}


Procedure TWorkItemServiceState.SetsplitRequest(AIndex : Integer; const AValue : TApproximateSplitRequest); 

begin
  If (FsplitRequest=AValue) then exit;
  FsplitRequest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemServiceState.SetleaseExpireTime(AIndex : Integer; const AValue : String); 

begin
  If (FleaseExpireTime=AValue) then exit;
  FleaseExpireTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemServiceState.SetreportStatusInterval(AIndex : Integer; const AValue : String); 

begin
  If (FreportStatusInterval=AValue) then exit;
  FreportStatusInterval:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemServiceState.SetharnessData(AIndex : Integer; const AValue : TWorkItemServiceStateTypeharnessData); 

begin
  If (FharnessData=AValue) then exit;
  FharnessData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemServiceState.SetnextReportIndex(AIndex : Integer; const AValue : String); 

begin
  If (FnextReportIndex=AValue) then exit;
  FnextReportIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemServiceState.SetsuggestedStopPosition(AIndex : Integer; const AValue : TPosition); 

begin
  If (FsuggestedStopPosition=AValue) then exit;
  FsuggestedStopPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemServiceState.SetsuggestedStopPoint(AIndex : Integer; const AValue : TApproximateProgress); 

begin
  If (FsuggestedStopPoint=AValue) then exit;
  FsuggestedStopPoint:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TApproximateSplitRequest
  --------------------------------------------------------------------}


Procedure TApproximateSplitRequest.Setposition(AIndex : Integer; const AValue : TPosition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApproximateSplitRequest.SetfractionConsumed(AIndex : Integer; const AValue : double); 

begin
  If (FfractionConsumed=AValue) then exit;
  FfractionConsumed:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLeaseWorkItemRequest
  --------------------------------------------------------------------}


Procedure TLeaseWorkItemRequest.SetworkItemTypes(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FworkItemTypes=AValue) then exit;
  FworkItemTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaseWorkItemRequest.SetworkerCapabilities(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FworkerCapabilities=AValue) then exit;
  FworkerCapabilities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaseWorkItemRequest.SetrequestedLeaseDuration(AIndex : Integer; const AValue : String); 

begin
  If (FrequestedLeaseDuration=AValue) then exit;
  FrequestedLeaseDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaseWorkItemRequest.SetcurrentWorkerTime(AIndex : Integer; const AValue : String); 

begin
  If (FcurrentWorkerTime=AValue) then exit;
  FcurrentWorkerTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaseWorkItemRequest.SetworkerId(AIndex : Integer; const AValue : String); 

begin
  If (FworkerId=AValue) then exit;
  FworkerId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TLeaseWorkItemRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'workitemtypes' : SetLength(FworkItemTypes,ALength);
  'workercapabilities' : SetLength(FworkerCapabilities,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLeaseWorkItemResponse
  --------------------------------------------------------------------}


Procedure TLeaseWorkItemResponse.SetworkItems(AIndex : Integer; const AValue : TLeaseWorkItemResponseTypeworkItemsArray); 

begin
  If (FworkItems=AValue) then exit;
  FworkItems:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TLeaseWorkItemResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'workitems' : SetLength(FworkItems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWorkItem
  --------------------------------------------------------------------}


Procedure TWorkItem.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetjobId(AIndex : Integer; const AValue : String); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.Setpackages(AIndex : Integer; const AValue : TWorkItemTypepackagesArray); 

begin
  If (Fpackages=AValue) then exit;
  Fpackages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetmapTask(AIndex : Integer; const AValue : TMapTask); 

begin
  If (FmapTask=AValue) then exit;
  FmapTask:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetseqMapTask(AIndex : Integer; const AValue : TSeqMapTask); 

begin
  If (FseqMapTask=AValue) then exit;
  FseqMapTask:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetshellTask(AIndex : Integer; const AValue : TShellTask); 

begin
  If (FshellTask=AValue) then exit;
  FshellTask:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetstreamingSetupTask(AIndex : Integer; const AValue : TStreamingSetupTask); 

begin
  If (FstreamingSetupTask=AValue) then exit;
  FstreamingSetupTask:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetsourceOperationTask(AIndex : Integer; const AValue : TSourceOperationRequest); 

begin
  If (FsourceOperationTask=AValue) then exit;
  FsourceOperationTask:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetstreamingComputationTask(AIndex : Integer; const AValue : TStreamingComputationTask); 

begin
  If (FstreamingComputationTask=AValue) then exit;
  FstreamingComputationTask:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetreportStatusInterval(AIndex : Integer; const AValue : String); 

begin
  If (FreportStatusInterval=AValue) then exit;
  FreportStatusInterval:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetleaseExpireTime(AIndex : Integer; const AValue : String); 

begin
  If (FleaseExpireTime=AValue) then exit;
  FleaseExpireTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.Setconfiguration(AIndex : Integer; const AValue : String); 

begin
  If (Fconfiguration=AValue) then exit;
  Fconfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetinitialReportIndex(AIndex : Integer; const AValue : String); 

begin
  If (FinitialReportIndex=AValue) then exit;
  FinitialReportIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TWorkItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'packages' : SetLength(Fpackages,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMapTask
  --------------------------------------------------------------------}


Procedure TMapTask.Setinstructions(AIndex : Integer; const AValue : TMapTaskTypeinstructionsArray); 

begin
  If (Finstructions=AValue) then exit;
  Finstructions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapTask.SetsystemName(AIndex : Integer; const AValue : String); 

begin
  If (FsystemName=AValue) then exit;
  FsystemName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapTask.SetstageName(AIndex : Integer; const AValue : String); 

begin
  If (FstageName=AValue) then exit;
  FstageName:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMapTask.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'instructions' : SetLength(Finstructions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TParallelInstruction
  --------------------------------------------------------------------}


Procedure TParallelInstruction.SetsystemName(AIndex : Integer; const AValue : String); 

begin
  If (FsystemName=AValue) then exit;
  FsystemName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParallelInstruction.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParallelInstruction.Setread(AIndex : Integer; const AValue : TReadInstruction); 

begin
  If (Fread=AValue) then exit;
  Fread:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParallelInstruction.Setwrite(AIndex : Integer; const AValue : TWriteInstruction); 

begin
  If (Fwrite=AValue) then exit;
  Fwrite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParallelInstruction.SetparDo(AIndex : Integer; const AValue : TParDoInstruction); 

begin
  If (FparDo=AValue) then exit;
  FparDo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParallelInstruction.SetpartialGroupByKey(AIndex : Integer; const AValue : TPartialGroupByKeyInstruction); 

begin
  If (FpartialGroupByKey=AValue) then exit;
  FpartialGroupByKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParallelInstruction.Setflatten(AIndex : Integer; const AValue : TFlattenInstruction); 

begin
  If (Fflatten=AValue) then exit;
  Fflatten:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParallelInstruction.Setoutputs(AIndex : Integer; const AValue : TParallelInstructionTypeoutputsArray); 

begin
  If (Foutputs=AValue) then exit;
  Foutputs:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TParallelInstruction.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'outputs' : SetLength(Foutputs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReadInstruction
  --------------------------------------------------------------------}


Procedure TReadInstruction.Setsource(AIndex : Integer; const AValue : TSource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWriteInstruction
  --------------------------------------------------------------------}


Procedure TWriteInstruction.Setinput(AIndex : Integer; const AValue : TInstructionInput); 

begin
  If (Finput=AValue) then exit;
  Finput:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWriteInstruction.Setsink(AIndex : Integer; const AValue : TSink); 

begin
  If (Fsink=AValue) then exit;
  Fsink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstructionInput
  --------------------------------------------------------------------}


Procedure TInstructionInput.SetproducerInstructionIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FproducerInstructionIndex=AValue) then exit;
  FproducerInstructionIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstructionInput.SetoutputNum(AIndex : Integer; const AValue : integer); 

begin
  If (FoutputNum=AValue) then exit;
  FoutputNum:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSinkTypespec
  --------------------------------------------------------------------}


Class Function TSinkTypespec.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSinkTypecodec
  --------------------------------------------------------------------}


Class Function TSinkTypecodec.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSink
  --------------------------------------------------------------------}


Procedure TSink.Setspec(AIndex : Integer; const AValue : TSinkTypespec); 

begin
  If (Fspec=AValue) then exit;
  Fspec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSink.Setcodec(AIndex : Integer; const AValue : TSinkTypecodec); 

begin
  If (Fcodec=AValue) then exit;
  Fcodec:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TParDoInstructionTypeuserFn
  --------------------------------------------------------------------}


Class Function TParDoInstructionTypeuserFn.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TParDoInstruction
  --------------------------------------------------------------------}


Procedure TParDoInstruction.Setinput(AIndex : Integer; const AValue : TInstructionInput); 

begin
  If (Finput=AValue) then exit;
  Finput:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParDoInstruction.SetsideInputs(AIndex : Integer; const AValue : TParDoInstructionTypesideInputsArray); 

begin
  If (FsideInputs=AValue) then exit;
  FsideInputs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParDoInstruction.SetuserFn(AIndex : Integer; const AValue : TParDoInstructionTypeuserFn); 

begin
  If (FuserFn=AValue) then exit;
  FuserFn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParDoInstruction.SetnumOutputs(AIndex : Integer; const AValue : integer); 

begin
  If (FnumOutputs=AValue) then exit;
  FnumOutputs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParDoInstruction.SetmultiOutputInfos(AIndex : Integer; const AValue : TParDoInstructionTypemultiOutputInfosArray); 

begin
  If (FmultiOutputInfos=AValue) then exit;
  FmultiOutputInfos:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TParDoInstruction.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sideinputs' : SetLength(FsideInputs,ALength);
  'multioutputinfos' : SetLength(FmultiOutputInfos,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSideInputInfoTypekind
  --------------------------------------------------------------------}


Class Function TSideInputInfoTypekind.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSideInputInfo
  --------------------------------------------------------------------}


Procedure TSideInputInfo.Setsources(AIndex : Integer; const AValue : TSideInputInfoTypesourcesArray); 

begin
  If (Fsources=AValue) then exit;
  Fsources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSideInputInfo.Setkind(AIndex : Integer; const AValue : TSideInputInfoTypekind); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSideInputInfo.Settag(AIndex : Integer; const AValue : String); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSideInputInfo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sources' : SetLength(Fsources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMultiOutputInfo
  --------------------------------------------------------------------}


Procedure TMultiOutputInfo.Settag(AIndex : Integer; const AValue : String); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPartialGroupByKeyInstructionTypeinputElementCodec
  --------------------------------------------------------------------}


Class Function TPartialGroupByKeyInstructionTypeinputElementCodec.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TPartialGroupByKeyInstructionTypevalueCombiningFn
  --------------------------------------------------------------------}


Class Function TPartialGroupByKeyInstructionTypevalueCombiningFn.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TPartialGroupByKeyInstruction
  --------------------------------------------------------------------}


Procedure TPartialGroupByKeyInstruction.Setinput(AIndex : Integer; const AValue : TInstructionInput); 

begin
  If (Finput=AValue) then exit;
  Finput:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPartialGroupByKeyInstruction.SetinputElementCodec(AIndex : Integer; const AValue : TPartialGroupByKeyInstructionTypeinputElementCodec); 

begin
  If (FinputElementCodec=AValue) then exit;
  FinputElementCodec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPartialGroupByKeyInstruction.SetvalueCombiningFn(AIndex : Integer; const AValue : TPartialGroupByKeyInstructionTypevalueCombiningFn); 

begin
  If (FvalueCombiningFn=AValue) then exit;
  FvalueCombiningFn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPartialGroupByKeyInstruction.SetsideInputs(AIndex : Integer; const AValue : TPartialGroupByKeyInstructionTypesideInputsArray); 

begin
  If (FsideInputs=AValue) then exit;
  FsideInputs:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPartialGroupByKeyInstruction.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sideinputs' : SetLength(FsideInputs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TFlattenInstruction
  --------------------------------------------------------------------}


Procedure TFlattenInstruction.Setinputs(AIndex : Integer; const AValue : TFlattenInstructionTypeinputsArray); 

begin
  If (Finputs=AValue) then exit;
  Finputs:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFlattenInstruction.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'inputs' : SetLength(Finputs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TInstructionOutputTypecodec
  --------------------------------------------------------------------}


Class Function TInstructionOutputTypecodec.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TInstructionOutput
  --------------------------------------------------------------------}


Procedure TInstructionOutput.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstructionOutput.SetsystemName(AIndex : Integer; const AValue : String); 

begin
  If (FsystemName=AValue) then exit;
  FsystemName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstructionOutput.Setcodec(AIndex : Integer; const AValue : TInstructionOutputTypecodec); 

begin
  If (Fcodec=AValue) then exit;
  Fcodec:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSeqMapTaskTypeuserFn
  --------------------------------------------------------------------}


Class Function TSeqMapTaskTypeuserFn.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSeqMapTask
  --------------------------------------------------------------------}


Procedure TSeqMapTask.Setinputs(AIndex : Integer; const AValue : TSeqMapTaskTypeinputsArray); 

begin
  If (Finputs=AValue) then exit;
  Finputs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTask.SetuserFn(AIndex : Integer; const AValue : TSeqMapTaskTypeuserFn); 

begin
  If (FuserFn=AValue) then exit;
  FuserFn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTask.SetoutputInfos(AIndex : Integer; const AValue : TSeqMapTaskTypeoutputInfosArray); 

begin
  If (FoutputInfos=AValue) then exit;
  FoutputInfos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTask.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTask.SetsystemName(AIndex : Integer; const AValue : String); 

begin
  If (FsystemName=AValue) then exit;
  FsystemName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTask.SetstageName(AIndex : Integer; const AValue : String); 

begin
  If (FstageName=AValue) then exit;
  FstageName:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSeqMapTask.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'inputs' : SetLength(Finputs,ALength);
  'outputinfos' : SetLength(FoutputInfos,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSeqMapTaskOutputInfo
  --------------------------------------------------------------------}


Procedure TSeqMapTaskOutputInfo.Settag(AIndex : Integer; const AValue : String); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTaskOutputInfo.Setsink(AIndex : Integer; const AValue : TSink); 

begin
  If (Fsink=AValue) then exit;
  Fsink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TShellTask
  --------------------------------------------------------------------}


Procedure TShellTask.Setcommand(AIndex : Integer; const AValue : String); 

begin
  If (Fcommand=AValue) then exit;
  Fcommand:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TShellTask.SetexitCode(AIndex : Integer; const AValue : integer); 

begin
  If (FexitCode=AValue) then exit;
  FexitCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStreamingSetupTask
  --------------------------------------------------------------------}


Procedure TStreamingSetupTask.SetreceiveWorkPort(AIndex : Integer; const AValue : integer); 

begin
  If (FreceiveWorkPort=AValue) then exit;
  FreceiveWorkPort:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingSetupTask.SetworkerHarnessPort(AIndex : Integer; const AValue : integer); 

begin
  If (FworkerHarnessPort=AValue) then exit;
  FworkerHarnessPort:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingSetupTask.SetstreamingComputationTopology(AIndex : Integer; const AValue : TTopologyConfig); 

begin
  If (FstreamingComputationTopology=AValue) then exit;
  FstreamingComputationTopology:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingSetupTask.Setdrain(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdrain=AValue) then exit;
  Fdrain:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTopologyConfigTypeuserStageToComputationNameMap
  --------------------------------------------------------------------}


Class Function TTopologyConfigTypeuserStageToComputationNameMap.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TTopologyConfig
  --------------------------------------------------------------------}


Procedure TTopologyConfig.Setcomputations(AIndex : Integer; const AValue : TTopologyConfigTypecomputationsArray); 

begin
  If (Fcomputations=AValue) then exit;
  Fcomputations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTopologyConfig.SetdataDiskAssignments(AIndex : Integer; const AValue : TTopologyConfigTypedataDiskAssignmentsArray); 

begin
  If (FdataDiskAssignments=AValue) then exit;
  FdataDiskAssignments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTopologyConfig.SetuserStageToComputationNameMap(AIndex : Integer; const AValue : TTopologyConfigTypeuserStageToComputationNameMap); 

begin
  If (FuserStageToComputationNameMap=AValue) then exit;
  FuserStageToComputationNameMap:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTopologyConfig.SetforwardingKeyBits(AIndex : Integer; const AValue : integer); 

begin
  If (FforwardingKeyBits=AValue) then exit;
  FforwardingKeyBits:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTopologyConfig.SetpersistentStateVersion(AIndex : Integer; const AValue : integer); 

begin
  If (FpersistentStateVersion=AValue) then exit;
  FpersistentStateVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTopologyConfig.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'computations' : SetLength(Fcomputations,ALength);
  'datadiskassignments' : SetLength(FdataDiskAssignments,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TComputationTopology
  --------------------------------------------------------------------}


Procedure TComputationTopology.SetsystemStageName(AIndex : Integer; const AValue : String); 

begin
  If (FsystemStageName=AValue) then exit;
  FsystemStageName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComputationTopology.SetcomputationId(AIndex : Integer; const AValue : String); 

begin
  If (FcomputationId=AValue) then exit;
  FcomputationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComputationTopology.SetuserStageName(AIndex : Integer; const AValue : String); 

begin
  If (FuserStageName=AValue) then exit;
  FuserStageName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComputationTopology.SetkeyRanges(AIndex : Integer; const AValue : TComputationTopologyTypekeyRangesArray); 

begin
  If (FkeyRanges=AValue) then exit;
  FkeyRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComputationTopology.Setinputs(AIndex : Integer; const AValue : TComputationTopologyTypeinputsArray); 

begin
  If (Finputs=AValue) then exit;
  Finputs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComputationTopology.Setoutputs(AIndex : Integer; const AValue : TComputationTopologyTypeoutputsArray); 

begin
  If (Foutputs=AValue) then exit;
  Foutputs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComputationTopology.SetstateFamilies(AIndex : Integer; const AValue : TComputationTopologyTypestateFamiliesArray); 

begin
  If (FstateFamilies=AValue) then exit;
  FstateFamilies:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TComputationTopology.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'keyranges' : SetLength(FkeyRanges,ALength);
  'inputs' : SetLength(Finputs,ALength);
  'outputs' : SetLength(Foutputs,ALength);
  'statefamilies' : SetLength(FstateFamilies,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TKeyRangeLocation
  --------------------------------------------------------------------}


Procedure TKeyRangeLocation.Setstart(AIndex : Integer; const AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeLocation.Set_end(AIndex : Integer; const AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeLocation.SetdeliveryEndpoint(AIndex : Integer; const AValue : String); 

begin
  If (FdeliveryEndpoint=AValue) then exit;
  FdeliveryEndpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeLocation.SetpersistentDirectory(AIndex : Integer; const AValue : String); 

begin
  If (FpersistentDirectory=AValue) then exit;
  FpersistentDirectory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeLocation.SetdataDisk(AIndex : Integer; const AValue : String); 

begin
  If (FdataDisk=AValue) then exit;
  FdataDisk:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TKeyRangeLocation.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TStreamLocation
  --------------------------------------------------------------------}


Procedure TStreamLocation.SetstreamingStageLocation(AIndex : Integer; const AValue : TStreamingStageLocation); 

begin
  If (FstreamingStageLocation=AValue) then exit;
  FstreamingStageLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamLocation.SetpubsubLocation(AIndex : Integer; const AValue : TPubsubLocation); 

begin
  If (FpubsubLocation=AValue) then exit;
  FpubsubLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamLocation.SetsideInputLocation(AIndex : Integer; const AValue : TStreamingSideInputLocation); 

begin
  If (FsideInputLocation=AValue) then exit;
  FsideInputLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamLocation.SetcustomSourceLocation(AIndex : Integer; const AValue : TCustomSourceLocation); 

begin
  If (FcustomSourceLocation=AValue) then exit;
  FcustomSourceLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStreamingStageLocation
  --------------------------------------------------------------------}


Procedure TStreamingStageLocation.SetstreamId(AIndex : Integer; const AValue : String); 

begin
  If (FstreamId=AValue) then exit;
  FstreamId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPubsubLocation
  --------------------------------------------------------------------}


Procedure TPubsubLocation.Settopic(AIndex : Integer; const AValue : String); 

begin
  If (Ftopic=AValue) then exit;
  Ftopic:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubLocation.Setsubscription(AIndex : Integer; const AValue : String); 

begin
  If (Fsubscription=AValue) then exit;
  Fsubscription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubLocation.SettimestampLabel(AIndex : Integer; const AValue : String); 

begin
  If (FtimestampLabel=AValue) then exit;
  FtimestampLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubLocation.SetidLabel(AIndex : Integer; const AValue : String); 

begin
  If (FidLabel=AValue) then exit;
  FidLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubLocation.SetdropLateData(AIndex : Integer; const AValue : boolean); 

begin
  If (FdropLateData=AValue) then exit;
  FdropLateData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubLocation.SettrackingSubscription(AIndex : Integer; const AValue : String); 

begin
  If (FtrackingSubscription=AValue) then exit;
  FtrackingSubscription:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStreamingSideInputLocation
  --------------------------------------------------------------------}


Procedure TStreamingSideInputLocation.Settag(AIndex : Integer; const AValue : String); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingSideInputLocation.SetstateFamily(AIndex : Integer; const AValue : String); 

begin
  If (FstateFamily=AValue) then exit;
  FstateFamily:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomSourceLocation
  --------------------------------------------------------------------}


Procedure TCustomSourceLocation.Setstateful(AIndex : Integer; const AValue : boolean); 

begin
  If (Fstateful=AValue) then exit;
  Fstateful:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStateFamilyConfig
  --------------------------------------------------------------------}


Procedure TStateFamilyConfig.SetstateFamily(AIndex : Integer; const AValue : String); 

begin
  If (FstateFamily=AValue) then exit;
  FstateFamily:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStateFamilyConfig.SetisRead(AIndex : Integer; const AValue : boolean); 

begin
  If (FisRead=AValue) then exit;
  FisRead:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDataDiskAssignment
  --------------------------------------------------------------------}


Procedure TDataDiskAssignment.SetvmInstance(AIndex : Integer; const AValue : String); 

begin
  If (FvmInstance=AValue) then exit;
  FvmInstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataDiskAssignment.SetdataDisks(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FdataDisks=AValue) then exit;
  FdataDisks:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDataDiskAssignment.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'datadisks' : SetLength(FdataDisks,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSourceOperationRequest
  --------------------------------------------------------------------}


Procedure TSourceOperationRequest.Setsplit(AIndex : Integer; const AValue : TSourceSplitRequest); 

begin
  If (Fsplit=AValue) then exit;
  Fsplit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceOperationRequest.SetgetMetadata(AIndex : Integer; const AValue : TSourceGetMetadataRequest); 

begin
  If (FgetMetadata=AValue) then exit;
  FgetMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceSplitRequest
  --------------------------------------------------------------------}


Procedure TSourceSplitRequest.Setsource(AIndex : Integer; const AValue : TSource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceSplitRequest.Setoptions(AIndex : Integer; const AValue : TSourceSplitOptions); 

begin
  If (Foptions=AValue) then exit;
  Foptions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceSplitOptions
  --------------------------------------------------------------------}


Procedure TSourceSplitOptions.SetdesiredBundleSizeBytes(AIndex : Integer; const AValue : String); 

begin
  If (FdesiredBundleSizeBytes=AValue) then exit;
  FdesiredBundleSizeBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceSplitOptions.SetdesiredShardSizeBytes(AIndex : Integer; const AValue : String); 

begin
  If (FdesiredShardSizeBytes=AValue) then exit;
  FdesiredShardSizeBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceGetMetadataRequest
  --------------------------------------------------------------------}


Procedure TSourceGetMetadataRequest.Setsource(AIndex : Integer; const AValue : TSource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStreamingComputationTask
  --------------------------------------------------------------------}


Procedure TStreamingComputationTask.SettaskType(AIndex : Integer; const AValue : String); 

begin
  If (FtaskType=AValue) then exit;
  FtaskType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingComputationTask.SetdataDisks(AIndex : Integer; const AValue : TStreamingComputationTaskTypedataDisksArray); 

begin
  If (FdataDisks=AValue) then exit;
  FdataDisks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingComputationTask.SetcomputationRanges(AIndex : Integer; const AValue : TStreamingComputationTaskTypecomputationRangesArray); 

begin
  If (FcomputationRanges=AValue) then exit;
  FcomputationRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TStreamingComputationTask.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'datadisks' : SetLength(FdataDisks,ALength);
  'computationranges' : SetLength(FcomputationRanges,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMountedDataDisk
  --------------------------------------------------------------------}


Procedure TMountedDataDisk.SetdataDisk(AIndex : Integer; const AValue : String); 

begin
  If (FdataDisk=AValue) then exit;
  FdataDisk:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStreamingComputationRanges
  --------------------------------------------------------------------}


Procedure TStreamingComputationRanges.SetcomputationId(AIndex : Integer; const AValue : String); 

begin
  If (FcomputationId=AValue) then exit;
  FcomputationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingComputationRanges.SetrangeAssignments(AIndex : Integer; const AValue : TStreamingComputationRangesTyperangeAssignmentsArray); 

begin
  If (FrangeAssignments=AValue) then exit;
  FrangeAssignments:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TStreamingComputationRanges.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'rangeassignments' : SetLength(FrangeAssignments,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TKeyRangeDataDiskAssignment
  --------------------------------------------------------------------}


Procedure TKeyRangeDataDiskAssignment.Setstart(AIndex : Integer; const AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeDataDiskAssignment.Set_end(AIndex : Integer; const AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeDataDiskAssignment.SetdataDisk(AIndex : Integer; const AValue : String); 

begin
  If (FdataDisk=AValue) then exit;
  FdataDisk:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TKeyRangeDataDiskAssignment.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TSendWorkerMessagesRequest
  --------------------------------------------------------------------}


Procedure TSendWorkerMessagesRequest.SetworkerMessages(AIndex : Integer; const AValue : TSendWorkerMessagesRequestTypeworkerMessagesArray); 

begin
  If (FworkerMessages=AValue) then exit;
  FworkerMessages:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSendWorkerMessagesRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'workermessages' : SetLength(FworkerMessages,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWorkerMessageTypelabels
  --------------------------------------------------------------------}


Class Function TWorkerMessageTypelabels.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWorkerMessage
  --------------------------------------------------------------------}


Procedure TWorkerMessage.Setlabels(AIndex : Integer; const AValue : TWorkerMessageTypelabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerMessage.Settime(AIndex : Integer; const AValue : String); 

begin
  If (Ftime=AValue) then exit;
  Ftime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerMessage.SetworkerHealthReport(AIndex : Integer; const AValue : TWorkerHealthReport); 

begin
  If (FworkerHealthReport=AValue) then exit;
  FworkerHealthReport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerMessage.SetworkerMessageCode(AIndex : Integer; const AValue : TWorkerMessageCode); 

begin
  If (FworkerMessageCode=AValue) then exit;
  FworkerMessageCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWorkerHealthReportTypepodsItem
  --------------------------------------------------------------------}


Class Function TWorkerHealthReportTypepodsItem.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWorkerHealthReport
  --------------------------------------------------------------------}


Procedure TWorkerHealthReport.SetvmIsHealthy(AIndex : Integer; const AValue : boolean); 

begin
  If (FvmIsHealthy=AValue) then exit;
  FvmIsHealthy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerHealthReport.SetvmStartupTime(AIndex : Integer; const AValue : String); 

begin
  If (FvmStartupTime=AValue) then exit;
  FvmStartupTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerHealthReport.SetreportInterval(AIndex : Integer; const AValue : String); 

begin
  If (FreportInterval=AValue) then exit;
  FreportInterval:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerHealthReport.Setpods(AIndex : Integer; const AValue : TWorkerHealthReportTypepodsArray); 

begin
  If (Fpods=AValue) then exit;
  Fpods:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TWorkerHealthReport.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'pods' : SetLength(Fpods,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWorkerMessageCodeTypeparameters
  --------------------------------------------------------------------}


Class Function TWorkerMessageCodeTypeparameters.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWorkerMessageCode
  --------------------------------------------------------------------}


Procedure TWorkerMessageCode.Setcode(AIndex : Integer; const AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerMessageCode.Setparameters(AIndex : Integer; const AValue : TWorkerMessageCodeTypeparameters); 

begin
  If (Fparameters=AValue) then exit;
  Fparameters:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSendWorkerMessagesResponse
  --------------------------------------------------------------------}


Procedure TSendWorkerMessagesResponse.SetworkerMessageResponses(AIndex : Integer; const AValue : TSendWorkerMessagesResponseTypeworkerMessageResponsesArray); 

begin
  If (FworkerMessageResponses=AValue) then exit;
  FworkerMessageResponses:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSendWorkerMessagesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'workermessageresponses' : SetLength(FworkerMessageResponses,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWorkerMessageResponse
  --------------------------------------------------------------------}


Procedure TWorkerMessageResponse.SetworkerHealthReportResponse(AIndex : Integer; const AValue : TWorkerHealthReportResponse); 

begin
  If (FworkerHealthReportResponse=AValue) then exit;
  FworkerHealthReportResponse:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWorkerHealthReportResponse
  --------------------------------------------------------------------}


Procedure TWorkerHealthReportResponse.SetreportInterval(AIndex : Integer; const AValue : String); 

begin
  If (FreportInterval=AValue) then exit;
  FreportInterval:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjectsJobsMessagesResource
  --------------------------------------------------------------------}


Class Function TProjectsJobsMessagesResource.ResourceName : String;

begin
  Result:='messages';
end;

Class Function TProjectsJobsMessagesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdataflowAPI;
end;

Function TProjectsJobsMessagesResource.List(projectId: string; jobId: string; AQuery : string = '') : TListJobMessagesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1b3/projects/{projectId}/jobs/{jobId}/messages';
  _Methodid   = 'dataflow.projects.jobs.messages.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'jobId',jobId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListJobMessagesResponse) as TListJobMessagesResponse;
end;


Function TProjectsJobsMessagesResource.List(projectId: string; jobId: string; AQuery : TProjectsJobsMessageslistOptions) : TListJobMessagesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'minimumImportance',AQuery.minimumImportance);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startTime',AQuery.startTime);
  AddToQuery(_Q,'endTime',AQuery.endTime);
  Result:=List(projectId,jobId,_Q);
end;



{ --------------------------------------------------------------------
  TProjectsJobsWorkItemsResource
  --------------------------------------------------------------------}


Class Function TProjectsJobsWorkItemsResource.ResourceName : String;

begin
  Result:='workItems';
end;

Class Function TProjectsJobsWorkItemsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdataflowAPI;
end;

Function TProjectsJobsWorkItemsResource.ReportStatus(projectId: string; jobId: string; aReportWorkItemStatusRequest : TReportWorkItemStatusRequest) : TReportWorkItemStatusResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1b3/projects/{projectId}/jobs/{jobId}/workItems:reportStatus';
  _Methodid   = 'dataflow.projects.jobs.workItems.reportStatus';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'jobId',jobId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aReportWorkItemStatusRequest,TReportWorkItemStatusResponse) as TReportWorkItemStatusResponse;
end;

Function TProjectsJobsWorkItemsResource.Lease(projectId: string; jobId: string; aLeaseWorkItemRequest : TLeaseWorkItemRequest) : TLeaseWorkItemResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1b3/projects/{projectId}/jobs/{jobId}/workItems:lease';
  _Methodid   = 'dataflow.projects.jobs.workItems.lease';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'jobId',jobId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLeaseWorkItemRequest,TLeaseWorkItemResponse) as TLeaseWorkItemResponse;
end;



{ --------------------------------------------------------------------
  TProjectsJobsResource
  --------------------------------------------------------------------}


Class Function TProjectsJobsResource.ResourceName : String;

begin
  Result:='jobs';
end;

Class Function TProjectsJobsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdataflowAPI;
end;

Function TProjectsJobsResource.Create(projectId: string; aJob : TJob; AQuery : string = '') : TJob;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1b3/projects/{projectId}/jobs';
  _Methodid   = 'dataflow.projects.jobs.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aJob,TJob) as TJob;
end;


Function TProjectsJobsResource.Create(projectId: string; aJob : TJob; AQuery : TProjectsJobscreateOptions) : TJob;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'view',AQuery.view);
  AddToQuery(_Q,'replaceJobId',AQuery.replaceJobId);
  Result:=Create(projectId,aJob,_Q);
end;

Function TProjectsJobsResource.Get(projectId: string; jobId: string; AQuery : string = '') : TJob;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1b3/projects/{projectId}/jobs/{jobId}';
  _Methodid   = 'dataflow.projects.jobs.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'jobId',jobId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TJob) as TJob;
end;


Function TProjectsJobsResource.Get(projectId: string; jobId: string; AQuery : TProjectsJobsgetOptions) : TJob;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'view',AQuery.view);
  Result:=Get(projectId,jobId,_Q);
end;

Function TProjectsJobsResource.Update(projectId: string; jobId: string; aJob : TJob) : TJob;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v1b3/projects/{projectId}/jobs/{jobId}';
  _Methodid   = 'dataflow.projects.jobs.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'jobId',jobId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aJob,TJob) as TJob;
end;

Function TProjectsJobsResource.List(projectId: string; AQuery : string = '') : TListJobsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1b3/projects/{projectId}/jobs';
  _Methodid   = 'dataflow.projects.jobs.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListJobsResponse) as TListJobsResponse;
end;


Function TProjectsJobsResource.List(projectId: string; AQuery : TProjectsJobslistOptions) : TListJobsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'view',AQuery.view);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(projectId,_Q);
end;

Function TProjectsJobsResource.GetMetrics(projectId: string; jobId: string; AQuery : string = '') : TJobMetrics;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1b3/projects/{projectId}/jobs/{jobId}/metrics';
  _Methodid   = 'dataflow.projects.jobs.getMetrics';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'jobId',jobId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TJobMetrics) as TJobMetrics;
end;


Function TProjectsJobsResource.GetMetrics(projectId: string; jobId: string; AQuery : TProjectsJobsgetMetricsOptions) : TJobMetrics;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'startTime',AQuery.startTime);
  Result:=GetMetrics(projectId,jobId,_Q);
end;



Function TProjectsJobsResource.GetMessagesInstance : TProjectsJobsMessagesResource;

begin
  if (FMessagesInstance=Nil) then
    FMessagesInstance:=CreateMessagesResource;
  Result:=FMessagesInstance;
end;

Function TProjectsJobsResource.CreateMessagesResource : TProjectsJobsMessagesResource;

begin
  Result:=CreateMessagesResource(Self);
end;


Function TProjectsJobsResource.CreateMessagesResource(AOwner : TComponent) : TProjectsJobsMessagesResource;

begin
  Result:=TProjectsJobsMessagesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsJobsResource.GetWorkItemsInstance : TProjectsJobsWorkItemsResource;

begin
  if (FWorkItemsInstance=Nil) then
    FWorkItemsInstance:=CreateWorkItemsResource;
  Result:=FWorkItemsInstance;
end;

Function TProjectsJobsResource.CreateWorkItemsResource : TProjectsJobsWorkItemsResource;

begin
  Result:=CreateWorkItemsResource(Self);
end;


Function TProjectsJobsResource.CreateWorkItemsResource(AOwner : TComponent) : TProjectsJobsWorkItemsResource;

begin
  Result:=TProjectsJobsWorkItemsResource.Create(AOwner);
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
  Result:=TdataflowAPI;
end;

Function TProjectsResource.WorkerMessages(projectId: string; aSendWorkerMessagesRequest : TSendWorkerMessagesRequest) : TSendWorkerMessagesResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1b3/projects/{projectId}/WorkerMessages';
  _Methodid   = 'dataflow.projects.workerMessages';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSendWorkerMessagesRequest,TSendWorkerMessagesResponse) as TSendWorkerMessagesResponse;
end;



Function TProjectsResource.GetJobsMessagesInstance : TProjectsJobsMessagesResource;

begin
  if (FJobsMessagesInstance=Nil) then
    FJobsMessagesInstance:=CreateJobsMessagesResource;
  Result:=FJobsMessagesInstance;
end;

Function TProjectsResource.CreateJobsMessagesResource : TProjectsJobsMessagesResource;

begin
  Result:=CreateJobsMessagesResource(Self);
end;


Function TProjectsResource.CreateJobsMessagesResource(AOwner : TComponent) : TProjectsJobsMessagesResource;

begin
  Result:=TProjectsJobsMessagesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetJobsWorkItemsInstance : TProjectsJobsWorkItemsResource;

begin
  if (FJobsWorkItemsInstance=Nil) then
    FJobsWorkItemsInstance:=CreateJobsWorkItemsResource;
  Result:=FJobsWorkItemsInstance;
end;

Function TProjectsResource.CreateJobsWorkItemsResource : TProjectsJobsWorkItemsResource;

begin
  Result:=CreateJobsWorkItemsResource(Self);
end;


Function TProjectsResource.CreateJobsWorkItemsResource(AOwner : TComponent) : TProjectsJobsWorkItemsResource;

begin
  Result:=TProjectsJobsWorkItemsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProjectsResource.GetJobsInstance : TProjectsJobsResource;

begin
  if (FJobsInstance=Nil) then
    FJobsInstance:=CreateJobsResource;
  Result:=FJobsInstance;
end;

Function TProjectsResource.CreateJobsResource : TProjectsJobsResource;

begin
  Result:=CreateJobsResource(Self);
end;


Function TProjectsResource.CreateJobsResource(AOwner : TComponent) : TProjectsJobsResource;

begin
  Result:=TProjectsJobsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TDataflowAPI
  --------------------------------------------------------------------}

Class Function TDataflowAPI.APIName : String;

begin
  Result:='dataflow';
end;

Class Function TDataflowAPI.APIVersion : String;

begin
  Result:='v1b3';
end;

Class Function TDataflowAPI.APIRevision : String;

begin
  Result:='20160331';
end;

Class Function TDataflowAPI.APIID : String;

begin
  Result:='dataflow:v1b3';
end;

Class Function TDataflowAPI.APITitle : String;

begin
  Result:='Google Dataflow API';
end;

Class Function TDataflowAPI.APIDescription : String;

begin
  Result:='Develops and executes data processing patterns like ETL, batch computation, and continuous computation.';
end;

Class Function TDataflowAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TDataflowAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TDataflowAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TDataflowAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TDataflowAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/dataflow';
end;

Class Function TDataflowAPI.APIrootUrl : string;

begin
  Result:='https://dataflow.googleapis.com/';
end;

Class Function TDataflowAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TDataflowAPI.APIbaseURL : String;

begin
  Result:='https://dataflow.googleapis.com/';
end;

Class Function TDataflowAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TDataflowAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TDataflowAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TDataflowAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/userinfo.email';
  Result[1].Description:='View your email address';
  
end;

Class Function TDataflowAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TDataflowAPI.RegisterAPIResources;

begin
  TJobTypetransformNameMapping.RegisterObject;
  TJob.RegisterObject;
  TEnvironmentTypeuserAgent.RegisterObject;
  TEnvironmentTypeversion.RegisterObject;
  TEnvironmentTypesdkPipelineOptions.RegisterObject;
  TEnvironmentTypeinternalExperiments.RegisterObject;
  TEnvironment.RegisterObject;
  TWorkerPoolTypemetadata.RegisterObject;
  TWorkerPoolTypepoolArgs.RegisterObject;
  TWorkerPool.RegisterObject;
  TPackage.RegisterObject;
  TTaskRunnerSettings.RegisterObject;
  TWorkerSettings.RegisterObject;
  TDisk.RegisterObject;
  TAutoscalingSettings.RegisterObject;
  TStepTypeproperties.RegisterObject;
  TStep.RegisterObject;
  TJobExecutionInfoTypestages.RegisterObject;
  TJobExecutionInfo.RegisterObject;
  TJobExecutionStageInfo.RegisterObject;
  TListJobsResponse.RegisterObject;
  TListJobMessagesResponse.RegisterObject;
  TJobMessage.RegisterObject;
  TJobMetrics.RegisterObject;
  TMetricUpdate.RegisterObject;
  TMetricStructuredNameTypecontext.RegisterObject;
  TMetricStructuredName.RegisterObject;
  TReportWorkItemStatusRequest.RegisterObject;
  TWorkItemStatus.RegisterObject;
  TStatusTypedetailsItem.RegisterObject;
  TStatus.RegisterObject;
  TApproximateReportedProgress.RegisterObject;
  TPosition.RegisterObject;
  TConcatPosition.RegisterObject;
  TReportedParallelism.RegisterObject;
  TDynamicSourceSplit.RegisterObject;
  TDerivedSource.RegisterObject;
  TSourceTypespec.RegisterObject;
  TSourceTypecodec.RegisterObject;
  TSourceTypebaseSpecsItem.RegisterObject;
  TSource.RegisterObject;
  TSourceMetadata.RegisterObject;
  TSourceOperationResponse.RegisterObject;
  TSourceSplitResponse.RegisterObject;
  TSourceSplitShard.RegisterObject;
  TSourceGetMetadataResponse.RegisterObject;
  TSourceFork.RegisterObject;
  TApproximateProgress.RegisterObject;
  TReportWorkItemStatusResponse.RegisterObject;
  TWorkItemServiceStateTypeharnessData.RegisterObject;
  TWorkItemServiceState.RegisterObject;
  TApproximateSplitRequest.RegisterObject;
  TLeaseWorkItemRequest.RegisterObject;
  TLeaseWorkItemResponse.RegisterObject;
  TWorkItem.RegisterObject;
  TMapTask.RegisterObject;
  TParallelInstruction.RegisterObject;
  TReadInstruction.RegisterObject;
  TWriteInstruction.RegisterObject;
  TInstructionInput.RegisterObject;
  TSinkTypespec.RegisterObject;
  TSinkTypecodec.RegisterObject;
  TSink.RegisterObject;
  TParDoInstructionTypeuserFn.RegisterObject;
  TParDoInstruction.RegisterObject;
  TSideInputInfoTypekind.RegisterObject;
  TSideInputInfo.RegisterObject;
  TMultiOutputInfo.RegisterObject;
  TPartialGroupByKeyInstructionTypeinputElementCodec.RegisterObject;
  TPartialGroupByKeyInstructionTypevalueCombiningFn.RegisterObject;
  TPartialGroupByKeyInstruction.RegisterObject;
  TFlattenInstruction.RegisterObject;
  TInstructionOutputTypecodec.RegisterObject;
  TInstructionOutput.RegisterObject;
  TSeqMapTaskTypeuserFn.RegisterObject;
  TSeqMapTask.RegisterObject;
  TSeqMapTaskOutputInfo.RegisterObject;
  TShellTask.RegisterObject;
  TStreamingSetupTask.RegisterObject;
  TTopologyConfigTypeuserStageToComputationNameMap.RegisterObject;
  TTopologyConfig.RegisterObject;
  TComputationTopology.RegisterObject;
  TKeyRangeLocation.RegisterObject;
  TStreamLocation.RegisterObject;
  TStreamingStageLocation.RegisterObject;
  TPubsubLocation.RegisterObject;
  TStreamingSideInputLocation.RegisterObject;
  TCustomSourceLocation.RegisterObject;
  TStateFamilyConfig.RegisterObject;
  TDataDiskAssignment.RegisterObject;
  TSourceOperationRequest.RegisterObject;
  TSourceSplitRequest.RegisterObject;
  TSourceSplitOptions.RegisterObject;
  TSourceGetMetadataRequest.RegisterObject;
  TStreamingComputationTask.RegisterObject;
  TMountedDataDisk.RegisterObject;
  TStreamingComputationRanges.RegisterObject;
  TKeyRangeDataDiskAssignment.RegisterObject;
  TSendWorkerMessagesRequest.RegisterObject;
  TWorkerMessageTypelabels.RegisterObject;
  TWorkerMessage.RegisterObject;
  TWorkerHealthReportTypepodsItem.RegisterObject;
  TWorkerHealthReport.RegisterObject;
  TWorkerMessageCodeTypeparameters.RegisterObject;
  TWorkerMessageCode.RegisterObject;
  TSendWorkerMessagesResponse.RegisterObject;
  TWorkerMessageResponse.RegisterObject;
  TWorkerHealthReportResponse.RegisterObject;
end;


Function TDataflowAPI.GetProjectsJobsMessagesInstance : TProjectsJobsMessagesResource;

begin
  if (FProjectsJobsMessagesInstance=Nil) then
    FProjectsJobsMessagesInstance:=CreateProjectsJobsMessagesResource;
  Result:=FProjectsJobsMessagesInstance;
end;

Function TDataflowAPI.CreateProjectsJobsMessagesResource : TProjectsJobsMessagesResource;

begin
  Result:=CreateProjectsJobsMessagesResource(Self);
end;


Function TDataflowAPI.CreateProjectsJobsMessagesResource(AOwner : TComponent) : TProjectsJobsMessagesResource;

begin
  Result:=TProjectsJobsMessagesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDataflowAPI.GetProjectsJobsWorkItemsInstance : TProjectsJobsWorkItemsResource;

begin
  if (FProjectsJobsWorkItemsInstance=Nil) then
    FProjectsJobsWorkItemsInstance:=CreateProjectsJobsWorkItemsResource;
  Result:=FProjectsJobsWorkItemsInstance;
end;

Function TDataflowAPI.CreateProjectsJobsWorkItemsResource : TProjectsJobsWorkItemsResource;

begin
  Result:=CreateProjectsJobsWorkItemsResource(Self);
end;


Function TDataflowAPI.CreateProjectsJobsWorkItemsResource(AOwner : TComponent) : TProjectsJobsWorkItemsResource;

begin
  Result:=TProjectsJobsWorkItemsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDataflowAPI.GetProjectsJobsInstance : TProjectsJobsResource;

begin
  if (FProjectsJobsInstance=Nil) then
    FProjectsJobsInstance:=CreateProjectsJobsResource;
  Result:=FProjectsJobsInstance;
end;

Function TDataflowAPI.CreateProjectsJobsResource : TProjectsJobsResource;

begin
  Result:=CreateProjectsJobsResource(Self);
end;


Function TDataflowAPI.CreateProjectsJobsResource(AOwner : TComponent) : TProjectsJobsResource;

begin
  Result:=TProjectsJobsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TDataflowAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TDataflowAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TDataflowAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TDataflowAPI.RegisterAPI;
end.
