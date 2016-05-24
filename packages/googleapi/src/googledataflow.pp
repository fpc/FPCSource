unit googledataflow;
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
  TApproximateProgress = class;
  TApproximateProgressArray = Array of TApproximateProgress;
  TAutoscalingSettings = class;
  TAutoscalingSettingsArray = Array of TAutoscalingSettings;
  TComputationTopology = class;
  TComputationTopologyArray = Array of TComputationTopology;
  TComputationTopologyinputs = class;
  TComputationTopologyinputsArray = Array of TComputationTopologyinputs;
  TComputationTopologykeyRanges = class;
  TComputationTopologykeyRangesArray = Array of TComputationTopologykeyRanges;
  TComputationTopologyoutputs = class;
  TComputationTopologyoutputsArray = Array of TComputationTopologyoutputs;
  TDataDiskAssignment = class;
  TDataDiskAssignmentArray = Array of TDataDiskAssignment;
  TDataDiskAssignmentdataDisks = class;
  TDataDiskAssignmentdataDisksArray = Array of TDataDiskAssignmentdataDisks;
  TDerivedSource = class;
  TDerivedSourceArray = Array of TDerivedSource;
  TDisk = class;
  TDiskArray = Array of TDisk;
  TDynamicSourceSplit = class;
  TDynamicSourceSplitArray = Array of TDynamicSourceSplit;
  TEnvironment = class;
  TEnvironmentArray = Array of TEnvironment;
  TEnvironmentexperiments = class;
  TEnvironmentexperimentsArray = Array of TEnvironmentexperiments;
  TEnvironmentsdkPipelineOptions = class;
  TEnvironmentsdkPipelineOptionsArray = Array of TEnvironmentsdkPipelineOptions;
  TEnvironmentuserAgent = class;
  TEnvironmentuserAgentArray = Array of TEnvironmentuserAgent;
  TEnvironmentversion = class;
  TEnvironmentversionArray = Array of TEnvironmentversion;
  TEnvironmentworkerPools = class;
  TEnvironmentworkerPoolsArray = Array of TEnvironmentworkerPools;
  TFlattenInstruction = class;
  TFlattenInstructionArray = Array of TFlattenInstruction;
  TFlattenInstructioninputs = class;
  TFlattenInstructioninputsArray = Array of TFlattenInstructioninputs;
  TGoogleprotobufValue = class;
  TGoogleprotobufValueArray = Array of TGoogleprotobufValue;
  TInstructionInput = class;
  TInstructionInputArray = Array of TInstructionInput;
  TInstructionOutput = class;
  TInstructionOutputArray = Array of TInstructionOutput;
  TInstructionOutputcodec = class;
  TInstructionOutputcodecArray = Array of TInstructionOutputcodec;
  TJob = class;
  TJobArray = Array of TJob;
  TJobsteps = class;
  TJobstepsArray = Array of TJobsteps;
  TJobExecutionInfo = class;
  TJobExecutionInfoArray = Array of TJobExecutionInfo;
  TJobExecutionInfostages = class;
  TJobExecutionInfostagesArray = Array of TJobExecutionInfostages;
  TJobExecutionStageInfo = class;
  TJobExecutionStageInfoArray = Array of TJobExecutionStageInfo;
  TJobExecutionStageInfostepName = class;
  TJobExecutionStageInfostepNameArray = Array of TJobExecutionStageInfostepName;
  TJobMessage = class;
  TJobMessageArray = Array of TJobMessage;
  TJobMetrics = class;
  TJobMetricsArray = Array of TJobMetrics;
  TJobMetricsmetrics = class;
  TJobMetricsmetricsArray = Array of TJobMetricsmetrics;
  TKeyRangeDataDiskAssignment = class;
  TKeyRangeDataDiskAssignmentArray = Array of TKeyRangeDataDiskAssignment;
  TKeyRangeLocation = class;
  TKeyRangeLocationArray = Array of TKeyRangeLocation;
  TLeaseWorkItemRequest = class;
  TLeaseWorkItemRequestArray = Array of TLeaseWorkItemRequest;
  TLeaseWorkItemRequestworkItemTypes = class;
  TLeaseWorkItemRequestworkItemTypesArray = Array of TLeaseWorkItemRequestworkItemTypes;
  TLeaseWorkItemRequestworkerCapabilities = class;
  TLeaseWorkItemRequestworkerCapabilitiesArray = Array of TLeaseWorkItemRequestworkerCapabilities;
  TLeaseWorkItemResponse = class;
  TLeaseWorkItemResponseArray = Array of TLeaseWorkItemResponse;
  TLeaseWorkItemResponseworkItems = class;
  TLeaseWorkItemResponseworkItemsArray = Array of TLeaseWorkItemResponseworkItems;
  TListJobMessagesResponse = class;
  TListJobMessagesResponseArray = Array of TListJobMessagesResponse;
  TListJobMessagesResponsejobMessages = class;
  TListJobMessagesResponsejobMessagesArray = Array of TListJobMessagesResponsejobMessages;
  TListJobsResponse = class;
  TListJobsResponseArray = Array of TListJobsResponse;
  TListJobsResponsejobs = class;
  TListJobsResponsejobsArray = Array of TListJobsResponsejobs;
  TMapTask = class;
  TMapTaskArray = Array of TMapTask;
  TMapTaskinstructions = class;
  TMapTaskinstructionsArray = Array of TMapTaskinstructions;
  TMetricStructuredName = class;
  TMetricStructuredNameArray = Array of TMetricStructuredName;
  TMetricStructuredNamecontext = class;
  TMetricStructuredNamecontextArray = Array of TMetricStructuredNamecontext;
  TMetricUpdate = class;
  TMetricUpdateArray = Array of TMetricUpdate;
  TMountedDataDisk = class;
  TMountedDataDiskArray = Array of TMountedDataDisk;
  TMultiOutputInfo = class;
  TMultiOutputInfoArray = Array of TMultiOutputInfo;
  TPackage = class;
  TPackageArray = Array of TPackage;
  TParDoInstruction = class;
  TParDoInstructionArray = Array of TParDoInstruction;
  TParDoInstructionmultiOutputInfos = class;
  TParDoInstructionmultiOutputInfosArray = Array of TParDoInstructionmultiOutputInfos;
  TParDoInstructionsideInputs = class;
  TParDoInstructionsideInputsArray = Array of TParDoInstructionsideInputs;
  TParDoInstructionuserFn = class;
  TParDoInstructionuserFnArray = Array of TParDoInstructionuserFn;
  TParallelInstruction = class;
  TParallelInstructionArray = Array of TParallelInstruction;
  TParallelInstructionoutputs = class;
  TParallelInstructionoutputsArray = Array of TParallelInstructionoutputs;
  TPartialGroupByKeyInstruction = class;
  TPartialGroupByKeyInstructionArray = Array of TPartialGroupByKeyInstruction;
  TPartialGroupByKeyInstructioninputElementCodec = class;
  TPartialGroupByKeyInstructioninputElementCodecArray = Array of TPartialGroupByKeyInstructioninputElementCodec;
  TPartialGroupByKeyInstructionvalueCombiningFn = class;
  TPartialGroupByKeyInstructionvalueCombiningFnArray = Array of TPartialGroupByKeyInstructionvalueCombiningFn;
  TPosition = class;
  TPositionArray = Array of TPosition;
  TPubsubLocation = class;
  TPubsubLocationArray = Array of TPubsubLocation;
  TReadInstruction = class;
  TReadInstructionArray = Array of TReadInstruction;
  TReportWorkItemStatusRequest = class;
  TReportWorkItemStatusRequestArray = Array of TReportWorkItemStatusRequest;
  TReportWorkItemStatusRequestworkItemStatuses = class;
  TReportWorkItemStatusRequestworkItemStatusesArray = Array of TReportWorkItemStatusRequestworkItemStatuses;
  TReportWorkItemStatusResponse = class;
  TReportWorkItemStatusResponseArray = Array of TReportWorkItemStatusResponse;
  TReportWorkItemStatusResponseworkItemServiceStates = class;
  TReportWorkItemStatusResponseworkItemServiceStatesArray = Array of TReportWorkItemStatusResponseworkItemServiceStates;
  TSeqMapTask = class;
  TSeqMapTaskArray = Array of TSeqMapTask;
  TSeqMapTaskinputs = class;
  TSeqMapTaskinputsArray = Array of TSeqMapTaskinputs;
  TSeqMapTaskoutputInfos = class;
  TSeqMapTaskoutputInfosArray = Array of TSeqMapTaskoutputInfos;
  TSeqMapTaskuserFn = class;
  TSeqMapTaskuserFnArray = Array of TSeqMapTaskuserFn;
  TSeqMapTaskOutputInfo = class;
  TSeqMapTaskOutputInfoArray = Array of TSeqMapTaskOutputInfo;
  TShellTask = class;
  TShellTaskArray = Array of TShellTask;
  TSideInputInfo = class;
  TSideInputInfoArray = Array of TSideInputInfo;
  TSideInputInfokind = class;
  TSideInputInfokindArray = Array of TSideInputInfokind;
  TSideInputInfosources = class;
  TSideInputInfosourcesArray = Array of TSideInputInfosources;
  TSink = class;
  TSinkArray = Array of TSink;
  TSinkcodec = class;
  TSinkcodecArray = Array of TSinkcodec;
  TSinkspec = class;
  TSinkspecArray = Array of TSinkspec;
  TSource = class;
  TSourceArray = Array of TSource;
  TSourcebaseSpecs = class;
  TSourcebaseSpecsArray = Array of TSourcebaseSpecs;
  TSourcecodec = class;
  TSourcecodecArray = Array of TSourcecodec;
  TSourcespec = class;
  TSourcespecArray = Array of TSourcespec;
  TSourceFork = class;
  TSourceForkArray = Array of TSourceFork;
  TSourceGetMetadataRequest = class;
  TSourceGetMetadataRequestArray = Array of TSourceGetMetadataRequest;
  TSourceGetMetadataResponse = class;
  TSourceGetMetadataResponseArray = Array of TSourceGetMetadataResponse;
  TSourceMetadata = class;
  TSourceMetadataArray = Array of TSourceMetadata;
  TSourceOperationRequest = class;
  TSourceOperationRequestArray = Array of TSourceOperationRequest;
  TSourceOperationResponse = class;
  TSourceOperationResponseArray = Array of TSourceOperationResponse;
  TSourceSplitOptions = class;
  TSourceSplitOptionsArray = Array of TSourceSplitOptions;
  TSourceSplitRequest = class;
  TSourceSplitRequestArray = Array of TSourceSplitRequest;
  TSourceSplitResponse = class;
  TSourceSplitResponseArray = Array of TSourceSplitResponse;
  TSourceSplitResponsebundles = class;
  TSourceSplitResponsebundlesArray = Array of TSourceSplitResponsebundles;
  TSourceSplitResponseshards = class;
  TSourceSplitResponseshardsArray = Array of TSourceSplitResponseshards;
  TSourceSplitShard = class;
  TSourceSplitShardArray = Array of TSourceSplitShard;
  TStatus = class;
  TStatusArray = Array of TStatus;
  TStatusdetails = class;
  TStatusdetailsArray = Array of TStatusdetails;
  TStep = class;
  TStepArray = Array of TStep;
  TStepproperties = class;
  TSteppropertiesArray = Array of TStepproperties;
  TStreamLocation = class;
  TStreamLocationArray = Array of TStreamLocation;
  TStreamingComputationRanges = class;
  TStreamingComputationRangesArray = Array of TStreamingComputationRanges;
  TStreamingComputationRangesrangeAssignments = class;
  TStreamingComputationRangesrangeAssignmentsArray = Array of TStreamingComputationRangesrangeAssignments;
  TStreamingComputationTask = class;
  TStreamingComputationTaskArray = Array of TStreamingComputationTask;
  TStreamingComputationTaskcomputationRanges = class;
  TStreamingComputationTaskcomputationRangesArray = Array of TStreamingComputationTaskcomputationRanges;
  TStreamingComputationTaskdataDisks = class;
  TStreamingComputationTaskdataDisksArray = Array of TStreamingComputationTaskdataDisks;
  TStreamingSetupTask = class;
  TStreamingSetupTaskArray = Array of TStreamingSetupTask;
  TStreamingSideInputLocation = class;
  TStreamingSideInputLocationArray = Array of TStreamingSideInputLocation;
  TStreamingStageLocation = class;
  TStreamingStageLocationArray = Array of TStreamingStageLocation;
  TTaskRunnerSettings = class;
  TTaskRunnerSettingsArray = Array of TTaskRunnerSettings;
  TTaskRunnerSettingsoauthScopes = class;
  TTaskRunnerSettingsoauthScopesArray = Array of TTaskRunnerSettingsoauthScopes;
  TTopologyConfig = class;
  TTopologyConfigArray = Array of TTopologyConfig;
  TTopologyConfigcomputations = class;
  TTopologyConfigcomputationsArray = Array of TTopologyConfigcomputations;
  TTopologyConfigdataDiskAssignments = class;
  TTopologyConfigdataDiskAssignmentsArray = Array of TTopologyConfigdataDiskAssignments;
  TWorkItem = class;
  TWorkItemArray = Array of TWorkItem;
  TWorkItempackages = class;
  TWorkItempackagesArray = Array of TWorkItempackages;
  TWorkItemServiceState = class;
  TWorkItemServiceStateArray = Array of TWorkItemServiceState;
  TWorkItemServiceStateharnessData = class;
  TWorkItemServiceStateharnessDataArray = Array of TWorkItemServiceStateharnessData;
  TWorkItemStatus = class;
  TWorkItemStatusArray = Array of TWorkItemStatus;
  TWorkItemStatuserrors = class;
  TWorkItemStatuserrorsArray = Array of TWorkItemStatuserrors;
  TWorkItemStatusmetricUpdates = class;
  TWorkItemStatusmetricUpdatesArray = Array of TWorkItemStatusmetricUpdates;
  TWorkerPool = class;
  TWorkerPoolArray = Array of TWorkerPool;
  TWorkerPooldataDisks = class;
  TWorkerPooldataDisksArray = Array of TWorkerPooldataDisks;
  TWorkerPoolmetadata = class;
  TWorkerPoolmetadataArray = Array of TWorkerPoolmetadata;
  TWorkerPoolpackages = class;
  TWorkerPoolpackagesArray = Array of TWorkerPoolpackages;
  TWorkerPoolpoolArgs = class;
  TWorkerPoolpoolArgsArray = Array of TWorkerPoolpoolArgs;
  TWorkerSettings = class;
  TWorkerSettingsArray = Array of TWorkerSettings;
  TWriteInstruction = class;
  TWriteInstructionArray = Array of TWriteInstruction;
  
  { --------------------------------------------------------------------
    TApproximateProgress
    --------------------------------------------------------------------}
  
  TApproximateProgress = Class(TGoogleBaseObject)
  Private
    FpercentComplete : integer;
    Fposition : TPosition;
    FremainingTime : string;
  Protected
    //Property setters
    Procedure SetpercentComplete(AIndex : Integer; AValue : integer); virtual;
    Procedure Setposition(AIndex : Integer; AValue : TPosition); virtual;
    Procedure SetremainingTime(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property percentComplete : integer Index 0 Read FpercentComplete Write SetpercentComplete;
    Property position : TPosition Index 8 Read Fposition Write Setposition;
    Property remainingTime : string Index 16 Read FremainingTime Write SetremainingTime;
  end;
  TApproximateProgressClass = Class of TApproximateProgress;
  
  { --------------------------------------------------------------------
    TAutoscalingSettings
    --------------------------------------------------------------------}
  
  TAutoscalingSettings = Class(TGoogleBaseObject)
  Private
    Falgorithm : string;
    FmaxNumWorkers : integer;
  Protected
    //Property setters
    Procedure Setalgorithm(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaxNumWorkers(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property algorithm : string Index 0 Read Falgorithm Write Setalgorithm;
    Property maxNumWorkers : integer Index 8 Read FmaxNumWorkers Write SetmaxNumWorkers;
  end;
  TAutoscalingSettingsClass = Class of TAutoscalingSettings;
  
  { --------------------------------------------------------------------
    TComputationTopology
    --------------------------------------------------------------------}
  
  TComputationTopology = Class(TGoogleBaseObject)
  Private
    FcomputationId : string;
    Finputs : TComputationTopologyinputs;
    FkeyRanges : TComputationTopologykeyRanges;
    Foutputs : TComputationTopologyoutputs;
  Protected
    //Property setters
    Procedure SetcomputationId(AIndex : Integer; AValue : string); virtual;
    Procedure Setinputs(AIndex : Integer; AValue : TComputationTopologyinputs); virtual;
    Procedure SetkeyRanges(AIndex : Integer; AValue : TComputationTopologykeyRanges); virtual;
    Procedure Setoutputs(AIndex : Integer; AValue : TComputationTopologyoutputs); virtual;
  Public
  Published
    Property computationId : string Index 0 Read FcomputationId Write SetcomputationId;
    Property inputs : TComputationTopologyinputs Index 8 Read Finputs Write Setinputs;
    Property keyRanges : TComputationTopologykeyRanges Index 16 Read FkeyRanges Write SetkeyRanges;
    Property outputs : TComputationTopologyoutputs Index 24 Read Foutputs Write Setoutputs;
  end;
  TComputationTopologyClass = Class of TComputationTopology;
  
  { --------------------------------------------------------------------
    TComputationTopologyinputs
    --------------------------------------------------------------------}
  
  TComputationTopologyinputs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TComputationTopologyinputsClass = Class of TComputationTopologyinputs;
  
  { --------------------------------------------------------------------
    TComputationTopologykeyRanges
    --------------------------------------------------------------------}
  
  TComputationTopologykeyRanges = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TComputationTopologykeyRangesClass = Class of TComputationTopologykeyRanges;
  
  { --------------------------------------------------------------------
    TComputationTopologyoutputs
    --------------------------------------------------------------------}
  
  TComputationTopologyoutputs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TComputationTopologyoutputsClass = Class of TComputationTopologyoutputs;
  
  { --------------------------------------------------------------------
    TDataDiskAssignment
    --------------------------------------------------------------------}
  
  TDataDiskAssignment = Class(TGoogleBaseObject)
  Private
    FdataDisks : TDataDiskAssignmentdataDisks;
    FvmInstance : string;
  Protected
    //Property setters
    Procedure SetdataDisks(AIndex : Integer; AValue : TDataDiskAssignmentdataDisks); virtual;
    Procedure SetvmInstance(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property dataDisks : TDataDiskAssignmentdataDisks Index 0 Read FdataDisks Write SetdataDisks;
    Property vmInstance : string Index 8 Read FvmInstance Write SetvmInstance;
  end;
  TDataDiskAssignmentClass = Class of TDataDiskAssignment;
  
  { --------------------------------------------------------------------
    TDataDiskAssignmentdataDisks
    --------------------------------------------------------------------}
  
  TDataDiskAssignmentdataDisks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDataDiskAssignmentdataDisksClass = Class of TDataDiskAssignmentdataDisks;
  
  { --------------------------------------------------------------------
    TDerivedSource
    --------------------------------------------------------------------}
  
  TDerivedSource = Class(TGoogleBaseObject)
  Private
    FderivationMode : string;
    Fsource : TSource;
  Protected
    //Property setters
    Procedure SetderivationMode(AIndex : Integer; AValue : string); virtual;
    Procedure Setsource(AIndex : Integer; AValue : TSource); virtual;
  Public
  Published
    Property derivationMode : string Index 0 Read FderivationMode Write SetderivationMode;
    Property source : TSource Index 8 Read Fsource Write Setsource;
  end;
  TDerivedSourceClass = Class of TDerivedSource;
  
  { --------------------------------------------------------------------
    TDisk
    --------------------------------------------------------------------}
  
  TDisk = Class(TGoogleBaseObject)
  Private
    FdiskType : string;
    FmountPoint : string;
    FsizeGb : integer;
  Protected
    //Property setters
    Procedure SetdiskType(AIndex : Integer; AValue : string); virtual;
    Procedure SetmountPoint(AIndex : Integer; AValue : string); virtual;
    Procedure SetsizeGb(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property diskType : string Index 0 Read FdiskType Write SetdiskType;
    Property mountPoint : string Index 8 Read FmountPoint Write SetmountPoint;
    Property sizeGb : integer Index 16 Read FsizeGb Write SetsizeGb;
  end;
  TDiskClass = Class of TDisk;
  
  { --------------------------------------------------------------------
    TDynamicSourceSplit
    --------------------------------------------------------------------}
  
  TDynamicSourceSplit = Class(TGoogleBaseObject)
  Private
    Fprimary : TDerivedSource;
    Fresidual : TDerivedSource;
  Protected
    //Property setters
    Procedure Setprimary(AIndex : Integer; AValue : TDerivedSource); virtual;
    Procedure Setresidual(AIndex : Integer; AValue : TDerivedSource); virtual;
  Public
  Published
    Property primary : TDerivedSource Index 0 Read Fprimary Write Setprimary;
    Property residual : TDerivedSource Index 8 Read Fresidual Write Setresidual;
  end;
  TDynamicSourceSplitClass = Class of TDynamicSourceSplit;
  
  { --------------------------------------------------------------------
    TEnvironment
    --------------------------------------------------------------------}
  
  TEnvironment = Class(TGoogleBaseObject)
  Private
    FclusterManagerApiService : string;
    Fdataset : string;
    Fexperiments : TEnvironmentexperiments;
    FsdkPipelineOptions : TEnvironmentsdkPipelineOptions;
    FtempStoragePrefix : string;
    FuserAgent : TEnvironmentuserAgent;
    Fversion : TEnvironmentversion;
    FworkerPools : TEnvironmentworkerPools;
  Protected
    //Property setters
    Procedure SetclusterManagerApiService(AIndex : Integer; AValue : string); virtual;
    Procedure Setdataset(AIndex : Integer; AValue : string); virtual;
    Procedure Setexperiments(AIndex : Integer; AValue : TEnvironmentexperiments); virtual;
    Procedure SetsdkPipelineOptions(AIndex : Integer; AValue : TEnvironmentsdkPipelineOptions); virtual;
    Procedure SettempStoragePrefix(AIndex : Integer; AValue : string); virtual;
    Procedure SetuserAgent(AIndex : Integer; AValue : TEnvironmentuserAgent); virtual;
    Procedure Setversion(AIndex : Integer; AValue : TEnvironmentversion); virtual;
    Procedure SetworkerPools(AIndex : Integer; AValue : TEnvironmentworkerPools); virtual;
  Public
  Published
    Property clusterManagerApiService : string Index 0 Read FclusterManagerApiService Write SetclusterManagerApiService;
    Property dataset : string Index 8 Read Fdataset Write Setdataset;
    Property experiments : TEnvironmentexperiments Index 16 Read Fexperiments Write Setexperiments;
    Property sdkPipelineOptions : TEnvironmentsdkPipelineOptions Index 24 Read FsdkPipelineOptions Write SetsdkPipelineOptions;
    Property tempStoragePrefix : string Index 32 Read FtempStoragePrefix Write SettempStoragePrefix;
    Property userAgent : TEnvironmentuserAgent Index 40 Read FuserAgent Write SetuserAgent;
    Property version : TEnvironmentversion Index 48 Read Fversion Write Setversion;
    Property workerPools : TEnvironmentworkerPools Index 56 Read FworkerPools Write SetworkerPools;
  end;
  TEnvironmentClass = Class of TEnvironment;
  
  { --------------------------------------------------------------------
    TEnvironmentexperiments
    --------------------------------------------------------------------}
  
  TEnvironmentexperiments = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEnvironmentexperimentsClass = Class of TEnvironmentexperiments;
  
  { --------------------------------------------------------------------
    TEnvironmentsdkPipelineOptions
    --------------------------------------------------------------------}
  
  TEnvironmentsdkPipelineOptions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TEnvironmentsdkPipelineOptionsClass = Class of TEnvironmentsdkPipelineOptions;
  
  { --------------------------------------------------------------------
    TEnvironmentuserAgent
    --------------------------------------------------------------------}
  
  TEnvironmentuserAgent = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TEnvironmentuserAgentClass = Class of TEnvironmentuserAgent;
  
  { --------------------------------------------------------------------
    TEnvironmentversion
    --------------------------------------------------------------------}
  
  TEnvironmentversion = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TEnvironmentversionClass = Class of TEnvironmentversion;
  
  { --------------------------------------------------------------------
    TEnvironmentworkerPools
    --------------------------------------------------------------------}
  
  TEnvironmentworkerPools = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEnvironmentworkerPoolsClass = Class of TEnvironmentworkerPools;
  
  { --------------------------------------------------------------------
    TFlattenInstruction
    --------------------------------------------------------------------}
  
  TFlattenInstruction = Class(TGoogleBaseObject)
  Private
    Finputs : TFlattenInstructioninputs;
  Protected
    //Property setters
    Procedure Setinputs(AIndex : Integer; AValue : TFlattenInstructioninputs); virtual;
  Public
  Published
    Property inputs : TFlattenInstructioninputs Index 0 Read Finputs Write Setinputs;
  end;
  TFlattenInstructionClass = Class of TFlattenInstruction;
  
  { --------------------------------------------------------------------
    TFlattenInstructioninputs
    --------------------------------------------------------------------}
  
  TFlattenInstructioninputs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFlattenInstructioninputsClass = Class of TFlattenInstructioninputs;
  
  { --------------------------------------------------------------------
    TGoogleprotobufValue
    --------------------------------------------------------------------}
  
  TGoogleprotobufValue = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGoogleprotobufValueClass = Class of TGoogleprotobufValue;
  
  { --------------------------------------------------------------------
    TInstructionInput
    --------------------------------------------------------------------}
  
  TInstructionInput = Class(TGoogleBaseObject)
  Private
    FoutputNum : integer;
    FproducerInstructionIndex : integer;
  Protected
    //Property setters
    Procedure SetoutputNum(AIndex : Integer; AValue : integer); virtual;
    Procedure SetproducerInstructionIndex(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property outputNum : integer Index 0 Read FoutputNum Write SetoutputNum;
    Property producerInstructionIndex : integer Index 8 Read FproducerInstructionIndex Write SetproducerInstructionIndex;
  end;
  TInstructionInputClass = Class of TInstructionInput;
  
  { --------------------------------------------------------------------
    TInstructionOutput
    --------------------------------------------------------------------}
  
  TInstructionOutput = Class(TGoogleBaseObject)
  Private
    Fcodec : TInstructionOutputcodec;
    Fname : string;
  Protected
    //Property setters
    Procedure Setcodec(AIndex : Integer; AValue : TInstructionOutputcodec); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property codec : TInstructionOutputcodec Index 0 Read Fcodec Write Setcodec;
    Property name : string Index 8 Read Fname Write Setname;
  end;
  TInstructionOutputClass = Class of TInstructionOutput;
  
  { --------------------------------------------------------------------
    TInstructionOutputcodec
    --------------------------------------------------------------------}
  
  TInstructionOutputcodec = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TInstructionOutputcodecClass = Class of TInstructionOutputcodec;
  
  { --------------------------------------------------------------------
    TJob
    --------------------------------------------------------------------}
  
  TJob = Class(TGoogleBaseObject)
  Private
    FcreateTime : string;
    FcurrentState : string;
    FcurrentStateTime : string;
    Fenvironment : TEnvironment;
    FexecutionInfo : TJobExecutionInfo;
    Fid : string;
    Fname : string;
    FprojectId : string;
    FrequestedState : string;
    Fsteps : TJobsteps;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetcreateTime(AIndex : Integer; AValue : string); virtual;
    Procedure SetcurrentState(AIndex : Integer; AValue : string); virtual;
    Procedure SetcurrentStateTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setenvironment(AIndex : Integer; AValue : TEnvironment); virtual;
    Procedure SetexecutionInfo(AIndex : Integer; AValue : TJobExecutionInfo); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : string); virtual;
    Procedure SetrequestedState(AIndex : Integer; AValue : string); virtual;
    Procedure Setsteps(AIndex : Integer; AValue : TJobsteps); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property createTime : string Index 0 Read FcreateTime Write SetcreateTime;
    Property currentState : string Index 8 Read FcurrentState Write SetcurrentState;
    Property currentStateTime : string Index 16 Read FcurrentStateTime Write SetcurrentStateTime;
    Property environment : TEnvironment Index 24 Read Fenvironment Write Setenvironment;
    Property executionInfo : TJobExecutionInfo Index 32 Read FexecutionInfo Write SetexecutionInfo;
    Property id : string Index 40 Read Fid Write Setid;
    Property name : string Index 48 Read Fname Write Setname;
    Property projectId : string Index 56 Read FprojectId Write SetprojectId;
    Property requestedState : string Index 64 Read FrequestedState Write SetrequestedState;
    Property steps : TJobsteps Index 72 Read Fsteps Write Setsteps;
    Property _type : string Index 80 Read F_type Write Set_type;
  end;
  TJobClass = Class of TJob;
  
  { --------------------------------------------------------------------
    TJobsteps
    --------------------------------------------------------------------}
  
  TJobsteps = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TJobstepsClass = Class of TJobsteps;
  
  { --------------------------------------------------------------------
    TJobExecutionInfo
    --------------------------------------------------------------------}
  
  TJobExecutionInfo = Class(TGoogleBaseObject)
  Private
    Fstages : TJobExecutionInfostages;
  Protected
    //Property setters
    Procedure Setstages(AIndex : Integer; AValue : TJobExecutionInfostages); virtual;
  Public
  Published
    Property stages : TJobExecutionInfostages Index 0 Read Fstages Write Setstages;
  end;
  TJobExecutionInfoClass = Class of TJobExecutionInfo;
  
  { --------------------------------------------------------------------
    TJobExecutionInfostages
    --------------------------------------------------------------------}
  
  TJobExecutionInfostages = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TJobExecutionInfostagesClass = Class of TJobExecutionInfostages;
  
  { --------------------------------------------------------------------
    TJobExecutionStageInfo
    --------------------------------------------------------------------}
  
  TJobExecutionStageInfo = Class(TGoogleBaseObject)
  Private
    FstepName : TJobExecutionStageInfostepName;
  Protected
    //Property setters
    Procedure SetstepName(AIndex : Integer; AValue : TJobExecutionStageInfostepName); virtual;
  Public
  Published
    Property stepName : TJobExecutionStageInfostepName Index 0 Read FstepName Write SetstepName;
  end;
  TJobExecutionStageInfoClass = Class of TJobExecutionStageInfo;
  
  { --------------------------------------------------------------------
    TJobExecutionStageInfostepName
    --------------------------------------------------------------------}
  
  TJobExecutionStageInfostepName = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TJobExecutionStageInfostepNameClass = Class of TJobExecutionStageInfostepName;
  
  { --------------------------------------------------------------------
    TJobMessage
    --------------------------------------------------------------------}
  
  TJobMessage = Class(TGoogleBaseObject)
  Private
    Fid : string;
    FmessageImportance : string;
    FmessageText : string;
    Ftime : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetmessageImportance(AIndex : Integer; AValue : string); virtual;
    Procedure SetmessageText(AIndex : Integer; AValue : string); virtual;
    Procedure Settime(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property messageImportance : string Index 8 Read FmessageImportance Write SetmessageImportance;
    Property messageText : string Index 16 Read FmessageText Write SetmessageText;
    Property time : string Index 24 Read Ftime Write Settime;
  end;
  TJobMessageClass = Class of TJobMessage;
  
  { --------------------------------------------------------------------
    TJobMetrics
    --------------------------------------------------------------------}
  
  TJobMetrics = Class(TGoogleBaseObject)
  Private
    FmetricTime : string;
    Fmetrics : TJobMetricsmetrics;
  Protected
    //Property setters
    Procedure SetmetricTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TJobMetricsmetrics); virtual;
  Public
  Published
    Property metricTime : string Index 0 Read FmetricTime Write SetmetricTime;
    Property metrics : TJobMetricsmetrics Index 8 Read Fmetrics Write Setmetrics;
  end;
  TJobMetricsClass = Class of TJobMetrics;
  
  { --------------------------------------------------------------------
    TJobMetricsmetrics
    --------------------------------------------------------------------}
  
  TJobMetricsmetrics = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TJobMetricsmetricsClass = Class of TJobMetricsmetrics;
  
  { --------------------------------------------------------------------
    TKeyRangeDataDiskAssignment
    --------------------------------------------------------------------}
  
  TKeyRangeDataDiskAssignment = Class(TGoogleBaseObject)
  Private
    FdataDisk : string;
    F_end : string;
    Fstart : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdataDisk(AIndex : Integer; AValue : string); virtual;
    Procedure Set_end(AIndex : Integer; AValue : string); virtual;
    Procedure Setstart(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property dataDisk : string Index 0 Read FdataDisk Write SetdataDisk;
    Property _end : string Index 8 Read F_end Write Set_end;
    Property start : string Index 16 Read Fstart Write Setstart;
  end;
  TKeyRangeDataDiskAssignmentClass = Class of TKeyRangeDataDiskAssignment;
  
  { --------------------------------------------------------------------
    TKeyRangeLocation
    --------------------------------------------------------------------}
  
  TKeyRangeLocation = Class(TGoogleBaseObject)
  Private
    FdataDisk : string;
    FdeliveryEndpoint : string;
    F_end : string;
    FpersistentDirectory : string;
    Fstart : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdataDisk(AIndex : Integer; AValue : string); virtual;
    Procedure SetdeliveryEndpoint(AIndex : Integer; AValue : string); virtual;
    Procedure Set_end(AIndex : Integer; AValue : string); virtual;
    Procedure SetpersistentDirectory(AIndex : Integer; AValue : string); virtual;
    Procedure Setstart(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property dataDisk : string Index 0 Read FdataDisk Write SetdataDisk;
    Property deliveryEndpoint : string Index 8 Read FdeliveryEndpoint Write SetdeliveryEndpoint;
    Property _end : string Index 16 Read F_end Write Set_end;
    Property persistentDirectory : string Index 24 Read FpersistentDirectory Write SetpersistentDirectory;
    Property start : string Index 32 Read Fstart Write Setstart;
  end;
  TKeyRangeLocationClass = Class of TKeyRangeLocation;
  
  { --------------------------------------------------------------------
    TLeaseWorkItemRequest
    --------------------------------------------------------------------}
  
  TLeaseWorkItemRequest = Class(TGoogleBaseObject)
  Private
    FcurrentWorkerTime : string;
    FrequestedLeaseDuration : string;
    FworkItemTypes : TLeaseWorkItemRequestworkItemTypes;
    FworkerCapabilities : TLeaseWorkItemRequestworkerCapabilities;
    FworkerId : string;
  Protected
    //Property setters
    Procedure SetcurrentWorkerTime(AIndex : Integer; AValue : string); virtual;
    Procedure SetrequestedLeaseDuration(AIndex : Integer; AValue : string); virtual;
    Procedure SetworkItemTypes(AIndex : Integer; AValue : TLeaseWorkItemRequestworkItemTypes); virtual;
    Procedure SetworkerCapabilities(AIndex : Integer; AValue : TLeaseWorkItemRequestworkerCapabilities); virtual;
    Procedure SetworkerId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property currentWorkerTime : string Index 0 Read FcurrentWorkerTime Write SetcurrentWorkerTime;
    Property requestedLeaseDuration : string Index 8 Read FrequestedLeaseDuration Write SetrequestedLeaseDuration;
    Property workItemTypes : TLeaseWorkItemRequestworkItemTypes Index 16 Read FworkItemTypes Write SetworkItemTypes;
    Property workerCapabilities : TLeaseWorkItemRequestworkerCapabilities Index 24 Read FworkerCapabilities Write SetworkerCapabilities;
    Property workerId : string Index 32 Read FworkerId Write SetworkerId;
  end;
  TLeaseWorkItemRequestClass = Class of TLeaseWorkItemRequest;
  
  { --------------------------------------------------------------------
    TLeaseWorkItemRequestworkItemTypes
    --------------------------------------------------------------------}
  
  TLeaseWorkItemRequestworkItemTypes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLeaseWorkItemRequestworkItemTypesClass = Class of TLeaseWorkItemRequestworkItemTypes;
  
  { --------------------------------------------------------------------
    TLeaseWorkItemRequestworkerCapabilities
    --------------------------------------------------------------------}
  
  TLeaseWorkItemRequestworkerCapabilities = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLeaseWorkItemRequestworkerCapabilitiesClass = Class of TLeaseWorkItemRequestworkerCapabilities;
  
  { --------------------------------------------------------------------
    TLeaseWorkItemResponse
    --------------------------------------------------------------------}
  
  TLeaseWorkItemResponse = Class(TGoogleBaseObject)
  Private
    FworkItems : TLeaseWorkItemResponseworkItems;
  Protected
    //Property setters
    Procedure SetworkItems(AIndex : Integer; AValue : TLeaseWorkItemResponseworkItems); virtual;
  Public
  Published
    Property workItems : TLeaseWorkItemResponseworkItems Index 0 Read FworkItems Write SetworkItems;
  end;
  TLeaseWorkItemResponseClass = Class of TLeaseWorkItemResponse;
  
  { --------------------------------------------------------------------
    TLeaseWorkItemResponseworkItems
    --------------------------------------------------------------------}
  
  TLeaseWorkItemResponseworkItems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLeaseWorkItemResponseworkItemsClass = Class of TLeaseWorkItemResponseworkItems;
  
  { --------------------------------------------------------------------
    TListJobMessagesResponse
    --------------------------------------------------------------------}
  
  TListJobMessagesResponse = Class(TGoogleBaseObject)
  Private
    FjobMessages : TListJobMessagesResponsejobMessages;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure SetjobMessages(AIndex : Integer; AValue : TListJobMessagesResponsejobMessages); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property jobMessages : TListJobMessagesResponsejobMessages Index 0 Read FjobMessages Write SetjobMessages;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListJobMessagesResponseClass = Class of TListJobMessagesResponse;
  
  { --------------------------------------------------------------------
    TListJobMessagesResponsejobMessages
    --------------------------------------------------------------------}
  
  TListJobMessagesResponsejobMessages = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListJobMessagesResponsejobMessagesClass = Class of TListJobMessagesResponsejobMessages;
  
  { --------------------------------------------------------------------
    TListJobsResponse
    --------------------------------------------------------------------}
  
  TListJobsResponse = Class(TGoogleBaseObject)
  Private
    Fjobs : TListJobsResponsejobs;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setjobs(AIndex : Integer; AValue : TListJobsResponsejobs); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property jobs : TListJobsResponsejobs Index 0 Read Fjobs Write Setjobs;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListJobsResponseClass = Class of TListJobsResponse;
  
  { --------------------------------------------------------------------
    TListJobsResponsejobs
    --------------------------------------------------------------------}
  
  TListJobsResponsejobs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListJobsResponsejobsClass = Class of TListJobsResponsejobs;
  
  { --------------------------------------------------------------------
    TMapTask
    --------------------------------------------------------------------}
  
  TMapTask = Class(TGoogleBaseObject)
  Private
    Finstructions : TMapTaskinstructions;
    FstageName : string;
    FsystemName : string;
  Protected
    //Property setters
    Procedure Setinstructions(AIndex : Integer; AValue : TMapTaskinstructions); virtual;
    Procedure SetstageName(AIndex : Integer; AValue : string); virtual;
    Procedure SetsystemName(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property instructions : TMapTaskinstructions Index 0 Read Finstructions Write Setinstructions;
    Property stageName : string Index 8 Read FstageName Write SetstageName;
    Property systemName : string Index 16 Read FsystemName Write SetsystemName;
  end;
  TMapTaskClass = Class of TMapTask;
  
  { --------------------------------------------------------------------
    TMapTaskinstructions
    --------------------------------------------------------------------}
  
  TMapTaskinstructions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMapTaskinstructionsClass = Class of TMapTaskinstructions;
  
  { --------------------------------------------------------------------
    TMetricStructuredName
    --------------------------------------------------------------------}
  
  TMetricStructuredName = Class(TGoogleBaseObject)
  Private
    Fcontext : TMetricStructuredNamecontext;
    Fname : string;
    Forigin : string;
  Protected
    //Property setters
    Procedure Setcontext(AIndex : Integer; AValue : TMetricStructuredNamecontext); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setorigin(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property context : TMetricStructuredNamecontext Index 0 Read Fcontext Write Setcontext;
    Property name : string Index 8 Read Fname Write Setname;
    Property origin : string Index 16 Read Forigin Write Setorigin;
  end;
  TMetricStructuredNameClass = Class of TMetricStructuredName;
  
  { --------------------------------------------------------------------
    TMetricStructuredNamecontext
    --------------------------------------------------------------------}
  
  TMetricStructuredNamecontext = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TMetricStructuredNamecontextClass = Class of TMetricStructuredNamecontext;
  
  { --------------------------------------------------------------------
    TMetricUpdate
    --------------------------------------------------------------------}
  
  TMetricUpdate = Class(TGoogleBaseObject)
  Private
    Fcumulative : boolean;
    Finternal : TGoogleprotobufValue;
    Fkind : string;
    FmeanCount : TGoogleprotobufValue;
    FmeanSum : TGoogleprotobufValue;
    Fname : TMetricStructuredName;
    Fscalar : TGoogleprotobufValue;
    F_set : TGoogleprotobufValue;
    FupdateTime : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcumulative(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setinternal(AIndex : Integer; AValue : TGoogleprotobufValue); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmeanCount(AIndex : Integer; AValue : TGoogleprotobufValue); virtual;
    Procedure SetmeanSum(AIndex : Integer; AValue : TGoogleprotobufValue); virtual;
    Procedure Setname(AIndex : Integer; AValue : TMetricStructuredName); virtual;
    Procedure Setscalar(AIndex : Integer; AValue : TGoogleprotobufValue); virtual;
    Procedure Set_set(AIndex : Integer; AValue : TGoogleprotobufValue); virtual;
    Procedure SetupdateTime(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property cumulative : boolean Index 0 Read Fcumulative Write Setcumulative;
    Property internal : TGoogleprotobufValue Index 8 Read Finternal Write Setinternal;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property meanCount : TGoogleprotobufValue Index 24 Read FmeanCount Write SetmeanCount;
    Property meanSum : TGoogleprotobufValue Index 32 Read FmeanSum Write SetmeanSum;
    Property name : TMetricStructuredName Index 40 Read Fname Write Setname;
    Property scalar : TGoogleprotobufValue Index 48 Read Fscalar Write Setscalar;
    Property _set : TGoogleprotobufValue Index 56 Read F_set Write Set_set;
    Property updateTime : string Index 64 Read FupdateTime Write SetupdateTime;
  end;
  TMetricUpdateClass = Class of TMetricUpdate;
  
  { --------------------------------------------------------------------
    TMountedDataDisk
    --------------------------------------------------------------------}
  
  TMountedDataDisk = Class(TGoogleBaseObject)
  Private
    FdataDisk : string;
  Protected
    //Property setters
    Procedure SetdataDisk(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property dataDisk : string Index 0 Read FdataDisk Write SetdataDisk;
  end;
  TMountedDataDiskClass = Class of TMountedDataDisk;
  
  { --------------------------------------------------------------------
    TMultiOutputInfo
    --------------------------------------------------------------------}
  
  TMultiOutputInfo = Class(TGoogleBaseObject)
  Private
    Ftag : string;
  Protected
    //Property setters
    Procedure Settag(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property tag : string Index 0 Read Ftag Write Settag;
  end;
  TMultiOutputInfoClass = Class of TMultiOutputInfo;
  
  { --------------------------------------------------------------------
    TPackage
    --------------------------------------------------------------------}
  
  TPackage = Class(TGoogleBaseObject)
  Private
    Flocation : string;
    Fname : string;
  Protected
    //Property setters
    Procedure Setlocation(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property location : string Index 0 Read Flocation Write Setlocation;
    Property name : string Index 8 Read Fname Write Setname;
  end;
  TPackageClass = Class of TPackage;
  
  { --------------------------------------------------------------------
    TParDoInstruction
    --------------------------------------------------------------------}
  
  TParDoInstruction = Class(TGoogleBaseObject)
  Private
    Finput : TInstructionInput;
    FmultiOutputInfos : TParDoInstructionmultiOutputInfos;
    FnumOutputs : integer;
    FsideInputs : TParDoInstructionsideInputs;
    FuserFn : TParDoInstructionuserFn;
  Protected
    //Property setters
    Procedure Setinput(AIndex : Integer; AValue : TInstructionInput); virtual;
    Procedure SetmultiOutputInfos(AIndex : Integer; AValue : TParDoInstructionmultiOutputInfos); virtual;
    Procedure SetnumOutputs(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsideInputs(AIndex : Integer; AValue : TParDoInstructionsideInputs); virtual;
    Procedure SetuserFn(AIndex : Integer; AValue : TParDoInstructionuserFn); virtual;
  Public
  Published
    Property input : TInstructionInput Index 0 Read Finput Write Setinput;
    Property multiOutputInfos : TParDoInstructionmultiOutputInfos Index 8 Read FmultiOutputInfos Write SetmultiOutputInfos;
    Property numOutputs : integer Index 16 Read FnumOutputs Write SetnumOutputs;
    Property sideInputs : TParDoInstructionsideInputs Index 24 Read FsideInputs Write SetsideInputs;
    Property userFn : TParDoInstructionuserFn Index 32 Read FuserFn Write SetuserFn;
  end;
  TParDoInstructionClass = Class of TParDoInstruction;
  
  { --------------------------------------------------------------------
    TParDoInstructionmultiOutputInfos
    --------------------------------------------------------------------}
  
  TParDoInstructionmultiOutputInfos = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TParDoInstructionmultiOutputInfosClass = Class of TParDoInstructionmultiOutputInfos;
  
  { --------------------------------------------------------------------
    TParDoInstructionsideInputs
    --------------------------------------------------------------------}
  
  TParDoInstructionsideInputs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TParDoInstructionsideInputsClass = Class of TParDoInstructionsideInputs;
  
  { --------------------------------------------------------------------
    TParDoInstructionuserFn
    --------------------------------------------------------------------}
  
  TParDoInstructionuserFn = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TParDoInstructionuserFnClass = Class of TParDoInstructionuserFn;
  
  { --------------------------------------------------------------------
    TParallelInstruction
    --------------------------------------------------------------------}
  
  TParallelInstruction = Class(TGoogleBaseObject)
  Private
    Fflatten : TFlattenInstruction;
    Fname : string;
    Foutputs : TParallelInstructionoutputs;
    FparDo : TParDoInstruction;
    FpartialGroupByKey : TPartialGroupByKeyInstruction;
    Fread : TReadInstruction;
    FsystemName : string;
    Fwrite : TWriteInstruction;
  Protected
    //Property setters
    Procedure Setflatten(AIndex : Integer; AValue : TFlattenInstruction); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setoutputs(AIndex : Integer; AValue : TParallelInstructionoutputs); virtual;
    Procedure SetparDo(AIndex : Integer; AValue : TParDoInstruction); virtual;
    Procedure SetpartialGroupByKey(AIndex : Integer; AValue : TPartialGroupByKeyInstruction); virtual;
    Procedure Setread(AIndex : Integer; AValue : TReadInstruction); virtual;
    Procedure SetsystemName(AIndex : Integer; AValue : string); virtual;
    Procedure Setwrite(AIndex : Integer; AValue : TWriteInstruction); virtual;
  Public
  Published
    Property flatten : TFlattenInstruction Index 0 Read Fflatten Write Setflatten;
    Property name : string Index 8 Read Fname Write Setname;
    Property outputs : TParallelInstructionoutputs Index 16 Read Foutputs Write Setoutputs;
    Property parDo : TParDoInstruction Index 24 Read FparDo Write SetparDo;
    Property partialGroupByKey : TPartialGroupByKeyInstruction Index 32 Read FpartialGroupByKey Write SetpartialGroupByKey;
    Property read : TReadInstruction Index 40 Read Fread Write Setread;
    Property systemName : string Index 48 Read FsystemName Write SetsystemName;
    Property write : TWriteInstruction Index 56 Read Fwrite Write Setwrite;
  end;
  TParallelInstructionClass = Class of TParallelInstruction;
  
  { --------------------------------------------------------------------
    TParallelInstructionoutputs
    --------------------------------------------------------------------}
  
  TParallelInstructionoutputs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TParallelInstructionoutputsClass = Class of TParallelInstructionoutputs;
  
  { --------------------------------------------------------------------
    TPartialGroupByKeyInstruction
    --------------------------------------------------------------------}
  
  TPartialGroupByKeyInstruction = Class(TGoogleBaseObject)
  Private
    Finput : TInstructionInput;
    FinputElementCodec : TPartialGroupByKeyInstructioninputElementCodec;
    FvalueCombiningFn : TPartialGroupByKeyInstructionvalueCombiningFn;
  Protected
    //Property setters
    Procedure Setinput(AIndex : Integer; AValue : TInstructionInput); virtual;
    Procedure SetinputElementCodec(AIndex : Integer; AValue : TPartialGroupByKeyInstructioninputElementCodec); virtual;
    Procedure SetvalueCombiningFn(AIndex : Integer; AValue : TPartialGroupByKeyInstructionvalueCombiningFn); virtual;
  Public
  Published
    Property input : TInstructionInput Index 0 Read Finput Write Setinput;
    Property inputElementCodec : TPartialGroupByKeyInstructioninputElementCodec Index 8 Read FinputElementCodec Write SetinputElementCodec;
    Property valueCombiningFn : TPartialGroupByKeyInstructionvalueCombiningFn Index 16 Read FvalueCombiningFn Write SetvalueCombiningFn;
  end;
  TPartialGroupByKeyInstructionClass = Class of TPartialGroupByKeyInstruction;
  
  { --------------------------------------------------------------------
    TPartialGroupByKeyInstructioninputElementCodec
    --------------------------------------------------------------------}
  
  TPartialGroupByKeyInstructioninputElementCodec = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TPartialGroupByKeyInstructioninputElementCodecClass = Class of TPartialGroupByKeyInstructioninputElementCodec;
  
  { --------------------------------------------------------------------
    TPartialGroupByKeyInstructionvalueCombiningFn
    --------------------------------------------------------------------}
  
  TPartialGroupByKeyInstructionvalueCombiningFn = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TPartialGroupByKeyInstructionvalueCombiningFnClass = Class of TPartialGroupByKeyInstructionvalueCombiningFn;
  
  { --------------------------------------------------------------------
    TPosition
    --------------------------------------------------------------------}
  
  TPosition = Class(TGoogleBaseObject)
  Private
    FbyteOffset : string;
    F_end : boolean;
    Fkey : string;
    FrecordIndex : string;
    FshufflePosition : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetbyteOffset(AIndex : Integer; AValue : string); virtual;
    Procedure Set_end(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure SetrecordIndex(AIndex : Integer; AValue : string); virtual;
    Procedure SetshufflePosition(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property byteOffset : string Index 0 Read FbyteOffset Write SetbyteOffset;
    Property _end : boolean Index 8 Read F_end Write Set_end;
    Property key : string Index 16 Read Fkey Write Setkey;
    Property recordIndex : string Index 24 Read FrecordIndex Write SetrecordIndex;
    Property shufflePosition : string Index 32 Read FshufflePosition Write SetshufflePosition;
  end;
  TPositionClass = Class of TPosition;
  
  { --------------------------------------------------------------------
    TPubsubLocation
    --------------------------------------------------------------------}
  
  TPubsubLocation = Class(TGoogleBaseObject)
  Private
    FdropLateData : boolean;
    FidLabel : string;
    Fsubscription : string;
    FtimestampLabel : string;
    Ftopic : string;
    FtrackingSubscription : string;
  Protected
    //Property setters
    Procedure SetdropLateData(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetidLabel(AIndex : Integer; AValue : string); virtual;
    Procedure Setsubscription(AIndex : Integer; AValue : string); virtual;
    Procedure SettimestampLabel(AIndex : Integer; AValue : string); virtual;
    Procedure Settopic(AIndex : Integer; AValue : string); virtual;
    Procedure SettrackingSubscription(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property dropLateData : boolean Index 0 Read FdropLateData Write SetdropLateData;
    Property idLabel : string Index 8 Read FidLabel Write SetidLabel;
    Property subscription : string Index 16 Read Fsubscription Write Setsubscription;
    Property timestampLabel : string Index 24 Read FtimestampLabel Write SettimestampLabel;
    Property topic : string Index 32 Read Ftopic Write Settopic;
    Property trackingSubscription : string Index 40 Read FtrackingSubscription Write SettrackingSubscription;
  end;
  TPubsubLocationClass = Class of TPubsubLocation;
  
  { --------------------------------------------------------------------
    TReadInstruction
    --------------------------------------------------------------------}
  
  TReadInstruction = Class(TGoogleBaseObject)
  Private
    Fsource : TSource;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; AValue : TSource); virtual;
  Public
  Published
    Property source : TSource Index 0 Read Fsource Write Setsource;
  end;
  TReadInstructionClass = Class of TReadInstruction;
  
  { --------------------------------------------------------------------
    TReportWorkItemStatusRequest
    --------------------------------------------------------------------}
  
  TReportWorkItemStatusRequest = Class(TGoogleBaseObject)
  Private
    FcurrentWorkerTime : string;
    FworkItemStatuses : TReportWorkItemStatusRequestworkItemStatuses;
    FworkerId : string;
  Protected
    //Property setters
    Procedure SetcurrentWorkerTime(AIndex : Integer; AValue : string); virtual;
    Procedure SetworkItemStatuses(AIndex : Integer; AValue : TReportWorkItemStatusRequestworkItemStatuses); virtual;
    Procedure SetworkerId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property currentWorkerTime : string Index 0 Read FcurrentWorkerTime Write SetcurrentWorkerTime;
    Property workItemStatuses : TReportWorkItemStatusRequestworkItemStatuses Index 8 Read FworkItemStatuses Write SetworkItemStatuses;
    Property workerId : string Index 16 Read FworkerId Write SetworkerId;
  end;
  TReportWorkItemStatusRequestClass = Class of TReportWorkItemStatusRequest;
  
  { --------------------------------------------------------------------
    TReportWorkItemStatusRequestworkItemStatuses
    --------------------------------------------------------------------}
  
  TReportWorkItemStatusRequestworkItemStatuses = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReportWorkItemStatusRequestworkItemStatusesClass = Class of TReportWorkItemStatusRequestworkItemStatuses;
  
  { --------------------------------------------------------------------
    TReportWorkItemStatusResponse
    --------------------------------------------------------------------}
  
  TReportWorkItemStatusResponse = Class(TGoogleBaseObject)
  Private
    FworkItemServiceStates : TReportWorkItemStatusResponseworkItemServiceStates;
  Protected
    //Property setters
    Procedure SetworkItemServiceStates(AIndex : Integer; AValue : TReportWorkItemStatusResponseworkItemServiceStates); virtual;
  Public
  Published
    Property workItemServiceStates : TReportWorkItemStatusResponseworkItemServiceStates Index 0 Read FworkItemServiceStates Write SetworkItemServiceStates;
  end;
  TReportWorkItemStatusResponseClass = Class of TReportWorkItemStatusResponse;
  
  { --------------------------------------------------------------------
    TReportWorkItemStatusResponseworkItemServiceStates
    --------------------------------------------------------------------}
  
  TReportWorkItemStatusResponseworkItemServiceStates = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReportWorkItemStatusResponseworkItemServiceStatesClass = Class of TReportWorkItemStatusResponseworkItemServiceStates;
  
  { --------------------------------------------------------------------
    TSeqMapTask
    --------------------------------------------------------------------}
  
  TSeqMapTask = Class(TGoogleBaseObject)
  Private
    Finputs : TSeqMapTaskinputs;
    Fname : string;
    FoutputInfos : TSeqMapTaskoutputInfos;
    FstageName : string;
    FsystemName : string;
    FuserFn : TSeqMapTaskuserFn;
  Protected
    //Property setters
    Procedure Setinputs(AIndex : Integer; AValue : TSeqMapTaskinputs); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetoutputInfos(AIndex : Integer; AValue : TSeqMapTaskoutputInfos); virtual;
    Procedure SetstageName(AIndex : Integer; AValue : string); virtual;
    Procedure SetsystemName(AIndex : Integer; AValue : string); virtual;
    Procedure SetuserFn(AIndex : Integer; AValue : TSeqMapTaskuserFn); virtual;
  Public
  Published
    Property inputs : TSeqMapTaskinputs Index 0 Read Finputs Write Setinputs;
    Property name : string Index 8 Read Fname Write Setname;
    Property outputInfos : TSeqMapTaskoutputInfos Index 16 Read FoutputInfos Write SetoutputInfos;
    Property stageName : string Index 24 Read FstageName Write SetstageName;
    Property systemName : string Index 32 Read FsystemName Write SetsystemName;
    Property userFn : TSeqMapTaskuserFn Index 40 Read FuserFn Write SetuserFn;
  end;
  TSeqMapTaskClass = Class of TSeqMapTask;
  
  { --------------------------------------------------------------------
    TSeqMapTaskinputs
    --------------------------------------------------------------------}
  
  TSeqMapTaskinputs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSeqMapTaskinputsClass = Class of TSeqMapTaskinputs;
  
  { --------------------------------------------------------------------
    TSeqMapTaskoutputInfos
    --------------------------------------------------------------------}
  
  TSeqMapTaskoutputInfos = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSeqMapTaskoutputInfosClass = Class of TSeqMapTaskoutputInfos;
  
  { --------------------------------------------------------------------
    TSeqMapTaskuserFn
    --------------------------------------------------------------------}
  
  TSeqMapTaskuserFn = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSeqMapTaskuserFnClass = Class of TSeqMapTaskuserFn;
  
  { --------------------------------------------------------------------
    TSeqMapTaskOutputInfo
    --------------------------------------------------------------------}
  
  TSeqMapTaskOutputInfo = Class(TGoogleBaseObject)
  Private
    Fsink : TSink;
    Ftag : string;
  Protected
    //Property setters
    Procedure Setsink(AIndex : Integer; AValue : TSink); virtual;
    Procedure Settag(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property sink : TSink Index 0 Read Fsink Write Setsink;
    Property tag : string Index 8 Read Ftag Write Settag;
  end;
  TSeqMapTaskOutputInfoClass = Class of TSeqMapTaskOutputInfo;
  
  { --------------------------------------------------------------------
    TShellTask
    --------------------------------------------------------------------}
  
  TShellTask = Class(TGoogleBaseObject)
  Private
    Fcommand : string;
    FexitCode : integer;
  Protected
    //Property setters
    Procedure Setcommand(AIndex : Integer; AValue : string); virtual;
    Procedure SetexitCode(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property command : string Index 0 Read Fcommand Write Setcommand;
    Property exitCode : integer Index 8 Read FexitCode Write SetexitCode;
  end;
  TShellTaskClass = Class of TShellTask;
  
  { --------------------------------------------------------------------
    TSideInputInfo
    --------------------------------------------------------------------}
  
  TSideInputInfo = Class(TGoogleBaseObject)
  Private
    Fkind : TSideInputInfokind;
    Fsources : TSideInputInfosources;
    Ftag : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : TSideInputInfokind); virtual;
    Procedure Setsources(AIndex : Integer; AValue : TSideInputInfosources); virtual;
    Procedure Settag(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : TSideInputInfokind Index 0 Read Fkind Write Setkind;
    Property sources : TSideInputInfosources Index 8 Read Fsources Write Setsources;
    Property tag : string Index 16 Read Ftag Write Settag;
  end;
  TSideInputInfoClass = Class of TSideInputInfo;
  
  { --------------------------------------------------------------------
    TSideInputInfokind
    --------------------------------------------------------------------}
  
  TSideInputInfokind = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSideInputInfokindClass = Class of TSideInputInfokind;
  
  { --------------------------------------------------------------------
    TSideInputInfosources
    --------------------------------------------------------------------}
  
  TSideInputInfosources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSideInputInfosourcesClass = Class of TSideInputInfosources;
  
  { --------------------------------------------------------------------
    TSink
    --------------------------------------------------------------------}
  
  TSink = Class(TGoogleBaseObject)
  Private
    Fcodec : TSinkcodec;
    Fspec : TSinkspec;
  Protected
    //Property setters
    Procedure Setcodec(AIndex : Integer; AValue : TSinkcodec); virtual;
    Procedure Setspec(AIndex : Integer; AValue : TSinkspec); virtual;
  Public
  Published
    Property codec : TSinkcodec Index 0 Read Fcodec Write Setcodec;
    Property spec : TSinkspec Index 8 Read Fspec Write Setspec;
  end;
  TSinkClass = Class of TSink;
  
  { --------------------------------------------------------------------
    TSinkcodec
    --------------------------------------------------------------------}
  
  TSinkcodec = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSinkcodecClass = Class of TSinkcodec;
  
  { --------------------------------------------------------------------
    TSinkspec
    --------------------------------------------------------------------}
  
  TSinkspec = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSinkspecClass = Class of TSinkspec;
  
  { --------------------------------------------------------------------
    TSource
    --------------------------------------------------------------------}
  
  TSource = Class(TGoogleBaseObject)
  Private
    FbaseSpecs : TSourcebaseSpecs;
    Fcodec : TSourcecodec;
    FdoesNotNeedSplitting : boolean;
    Fmetadata : TSourceMetadata;
    Fspec : TSourcespec;
  Protected
    //Property setters
    Procedure SetbaseSpecs(AIndex : Integer; AValue : TSourcebaseSpecs); virtual;
    Procedure Setcodec(AIndex : Integer; AValue : TSourcecodec); virtual;
    Procedure SetdoesNotNeedSplitting(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TSourceMetadata); virtual;
    Procedure Setspec(AIndex : Integer; AValue : TSourcespec); virtual;
  Public
  Published
    Property baseSpecs : TSourcebaseSpecs Index 0 Read FbaseSpecs Write SetbaseSpecs;
    Property codec : TSourcecodec Index 8 Read Fcodec Write Setcodec;
    Property doesNotNeedSplitting : boolean Index 16 Read FdoesNotNeedSplitting Write SetdoesNotNeedSplitting;
    Property metadata : TSourceMetadata Index 24 Read Fmetadata Write Setmetadata;
    Property spec : TSourcespec Index 32 Read Fspec Write Setspec;
  end;
  TSourceClass = Class of TSource;
  
  { --------------------------------------------------------------------
    TSourcebaseSpecs
    --------------------------------------------------------------------}
  
  TSourcebaseSpecs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSourcebaseSpecsClass = Class of TSourcebaseSpecs;
  
  { --------------------------------------------------------------------
    TSourcecodec
    --------------------------------------------------------------------}
  
  TSourcecodec = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSourcecodecClass = Class of TSourcecodec;
  
  { --------------------------------------------------------------------
    TSourcespec
    --------------------------------------------------------------------}
  
  TSourcespec = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSourcespecClass = Class of TSourcespec;
  
  { --------------------------------------------------------------------
    TSourceFork
    --------------------------------------------------------------------}
  
  TSourceFork = Class(TGoogleBaseObject)
  Private
    Fprimary : TSourceSplitShard;
    FprimarySource : TDerivedSource;
    Fresidual : TSourceSplitShard;
    FresidualSource : TDerivedSource;
  Protected
    //Property setters
    Procedure Setprimary(AIndex : Integer; AValue : TSourceSplitShard); virtual;
    Procedure SetprimarySource(AIndex : Integer; AValue : TDerivedSource); virtual;
    Procedure Setresidual(AIndex : Integer; AValue : TSourceSplitShard); virtual;
    Procedure SetresidualSource(AIndex : Integer; AValue : TDerivedSource); virtual;
  Public
  Published
    Property primary : TSourceSplitShard Index 0 Read Fprimary Write Setprimary;
    Property primarySource : TDerivedSource Index 8 Read FprimarySource Write SetprimarySource;
    Property residual : TSourceSplitShard Index 16 Read Fresidual Write Setresidual;
    Property residualSource : TDerivedSource Index 24 Read FresidualSource Write SetresidualSource;
  end;
  TSourceForkClass = Class of TSourceFork;
  
  { --------------------------------------------------------------------
    TSourceGetMetadataRequest
    --------------------------------------------------------------------}
  
  TSourceGetMetadataRequest = Class(TGoogleBaseObject)
  Private
    Fsource : TSource;
  Protected
    //Property setters
    Procedure Setsource(AIndex : Integer; AValue : TSource); virtual;
  Public
  Published
    Property source : TSource Index 0 Read Fsource Write Setsource;
  end;
  TSourceGetMetadataRequestClass = Class of TSourceGetMetadataRequest;
  
  { --------------------------------------------------------------------
    TSourceGetMetadataResponse
    --------------------------------------------------------------------}
  
  TSourceGetMetadataResponse = Class(TGoogleBaseObject)
  Private
    Fmetadata : TSourceMetadata;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; AValue : TSourceMetadata); virtual;
  Public
  Published
    Property metadata : TSourceMetadata Index 0 Read Fmetadata Write Setmetadata;
  end;
  TSourceGetMetadataResponseClass = Class of TSourceGetMetadataResponse;
  
  { --------------------------------------------------------------------
    TSourceMetadata
    --------------------------------------------------------------------}
  
  TSourceMetadata = Class(TGoogleBaseObject)
  Private
    FestimatedSizeBytes : string;
    Finfinite : boolean;
    FproducesSortedKeys : boolean;
  Protected
    //Property setters
    Procedure SetestimatedSizeBytes(AIndex : Integer; AValue : string); virtual;
    Procedure Setinfinite(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetproducesSortedKeys(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property estimatedSizeBytes : string Index 0 Read FestimatedSizeBytes Write SetestimatedSizeBytes;
    Property infinite : boolean Index 8 Read Finfinite Write Setinfinite;
    Property producesSortedKeys : boolean Index 16 Read FproducesSortedKeys Write SetproducesSortedKeys;
  end;
  TSourceMetadataClass = Class of TSourceMetadata;
  
  { --------------------------------------------------------------------
    TSourceOperationRequest
    --------------------------------------------------------------------}
  
  TSourceOperationRequest = Class(TGoogleBaseObject)
  Private
    FgetMetadata : TSourceGetMetadataRequest;
    Fsplit : TSourceSplitRequest;
  Protected
    //Property setters
    Procedure SetgetMetadata(AIndex : Integer; AValue : TSourceGetMetadataRequest); virtual;
    Procedure Setsplit(AIndex : Integer; AValue : TSourceSplitRequest); virtual;
  Public
  Published
    Property getMetadata : TSourceGetMetadataRequest Index 0 Read FgetMetadata Write SetgetMetadata;
    Property split : TSourceSplitRequest Index 8 Read Fsplit Write Setsplit;
  end;
  TSourceOperationRequestClass = Class of TSourceOperationRequest;
  
  { --------------------------------------------------------------------
    TSourceOperationResponse
    --------------------------------------------------------------------}
  
  TSourceOperationResponse = Class(TGoogleBaseObject)
  Private
    FgetMetadata : TSourceGetMetadataResponse;
    Fsplit : TSourceSplitResponse;
  Protected
    //Property setters
    Procedure SetgetMetadata(AIndex : Integer; AValue : TSourceGetMetadataResponse); virtual;
    Procedure Setsplit(AIndex : Integer; AValue : TSourceSplitResponse); virtual;
  Public
  Published
    Property getMetadata : TSourceGetMetadataResponse Index 0 Read FgetMetadata Write SetgetMetadata;
    Property split : TSourceSplitResponse Index 8 Read Fsplit Write Setsplit;
  end;
  TSourceOperationResponseClass = Class of TSourceOperationResponse;
  
  { --------------------------------------------------------------------
    TSourceSplitOptions
    --------------------------------------------------------------------}
  
  TSourceSplitOptions = Class(TGoogleBaseObject)
  Private
    FdesiredBundleSizeBytes : string;
    FdesiredShardSizeBytes : string;
  Protected
    //Property setters
    Procedure SetdesiredBundleSizeBytes(AIndex : Integer; AValue : string); virtual;
    Procedure SetdesiredShardSizeBytes(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property desiredBundleSizeBytes : string Index 0 Read FdesiredBundleSizeBytes Write SetdesiredBundleSizeBytes;
    Property desiredShardSizeBytes : string Index 8 Read FdesiredShardSizeBytes Write SetdesiredShardSizeBytes;
  end;
  TSourceSplitOptionsClass = Class of TSourceSplitOptions;
  
  { --------------------------------------------------------------------
    TSourceSplitRequest
    --------------------------------------------------------------------}
  
  TSourceSplitRequest = Class(TGoogleBaseObject)
  Private
    Foptions : TSourceSplitOptions;
    Fsource : TSource;
  Protected
    //Property setters
    Procedure Setoptions(AIndex : Integer; AValue : TSourceSplitOptions); virtual;
    Procedure Setsource(AIndex : Integer; AValue : TSource); virtual;
  Public
  Published
    Property options : TSourceSplitOptions Index 0 Read Foptions Write Setoptions;
    Property source : TSource Index 8 Read Fsource Write Setsource;
  end;
  TSourceSplitRequestClass = Class of TSourceSplitRequest;
  
  { --------------------------------------------------------------------
    TSourceSplitResponse
    --------------------------------------------------------------------}
  
  TSourceSplitResponse = Class(TGoogleBaseObject)
  Private
    Fbundles : TSourceSplitResponsebundles;
    Foutcome : string;
    Fshards : TSourceSplitResponseshards;
  Protected
    //Property setters
    Procedure Setbundles(AIndex : Integer; AValue : TSourceSplitResponsebundles); virtual;
    Procedure Setoutcome(AIndex : Integer; AValue : string); virtual;
    Procedure Setshards(AIndex : Integer; AValue : TSourceSplitResponseshards); virtual;
  Public
  Published
    Property bundles : TSourceSplitResponsebundles Index 0 Read Fbundles Write Setbundles;
    Property outcome : string Index 8 Read Foutcome Write Setoutcome;
    Property shards : TSourceSplitResponseshards Index 16 Read Fshards Write Setshards;
  end;
  TSourceSplitResponseClass = Class of TSourceSplitResponse;
  
  { --------------------------------------------------------------------
    TSourceSplitResponsebundles
    --------------------------------------------------------------------}
  
  TSourceSplitResponsebundles = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSourceSplitResponsebundlesClass = Class of TSourceSplitResponsebundles;
  
  { --------------------------------------------------------------------
    TSourceSplitResponseshards
    --------------------------------------------------------------------}
  
  TSourceSplitResponseshards = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSourceSplitResponseshardsClass = Class of TSourceSplitResponseshards;
  
  { --------------------------------------------------------------------
    TSourceSplitShard
    --------------------------------------------------------------------}
  
  TSourceSplitShard = Class(TGoogleBaseObject)
  Private
    FderivationMode : string;
    Fsource : TSource;
  Protected
    //Property setters
    Procedure SetderivationMode(AIndex : Integer; AValue : string); virtual;
    Procedure Setsource(AIndex : Integer; AValue : TSource); virtual;
  Public
  Published
    Property derivationMode : string Index 0 Read FderivationMode Write SetderivationMode;
    Property source : TSource Index 8 Read Fsource Write Setsource;
  end;
  TSourceSplitShardClass = Class of TSourceSplitShard;
  
  { --------------------------------------------------------------------
    TStatus
    --------------------------------------------------------------------}
  
  TStatus = Class(TGoogleBaseObject)
  Private
    Fcode : integer;
    Fdetails : TStatusdetails;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : integer); virtual;
    Procedure Setdetails(AIndex : Integer; AValue : TStatusdetails); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : integer Index 0 Read Fcode Write Setcode;
    Property details : TStatusdetails Index 8 Read Fdetails Write Setdetails;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TStatusClass = Class of TStatus;
  
  { --------------------------------------------------------------------
    TStatusdetails
    --------------------------------------------------------------------}
  
  TStatusdetails = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TStatusdetailsClass = Class of TStatusdetails;
  
  { --------------------------------------------------------------------
    TStep
    --------------------------------------------------------------------}
  
  TStep = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Fname : string;
    Fproperties : TStepproperties;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setproperties(AIndex : Integer; AValue : TStepproperties); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property name : string Index 8 Read Fname Write Setname;
    Property properties : TStepproperties Index 16 Read Fproperties Write Setproperties;
  end;
  TStepClass = Class of TStep;
  
  { --------------------------------------------------------------------
    TStepproperties
    --------------------------------------------------------------------}
  
  TStepproperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSteppropertiesClass = Class of TStepproperties;
  
  { --------------------------------------------------------------------
    TStreamLocation
    --------------------------------------------------------------------}
  
  TStreamLocation = Class(TGoogleBaseObject)
  Private
    FpubsubLocation : TPubsubLocation;
    FsideInputLocation : TStreamingSideInputLocation;
    FstreamingStageLocation : TStreamingStageLocation;
  Protected
    //Property setters
    Procedure SetpubsubLocation(AIndex : Integer; AValue : TPubsubLocation); virtual;
    Procedure SetsideInputLocation(AIndex : Integer; AValue : TStreamingSideInputLocation); virtual;
    Procedure SetstreamingStageLocation(AIndex : Integer; AValue : TStreamingStageLocation); virtual;
  Public
  Published
    Property pubsubLocation : TPubsubLocation Index 0 Read FpubsubLocation Write SetpubsubLocation;
    Property sideInputLocation : TStreamingSideInputLocation Index 8 Read FsideInputLocation Write SetsideInputLocation;
    Property streamingStageLocation : TStreamingStageLocation Index 16 Read FstreamingStageLocation Write SetstreamingStageLocation;
  end;
  TStreamLocationClass = Class of TStreamLocation;
  
  { --------------------------------------------------------------------
    TStreamingComputationRanges
    --------------------------------------------------------------------}
  
  TStreamingComputationRanges = Class(TGoogleBaseObject)
  Private
    FcomputationId : string;
    FrangeAssignments : TStreamingComputationRangesrangeAssignments;
  Protected
    //Property setters
    Procedure SetcomputationId(AIndex : Integer; AValue : string); virtual;
    Procedure SetrangeAssignments(AIndex : Integer; AValue : TStreamingComputationRangesrangeAssignments); virtual;
  Public
  Published
    Property computationId : string Index 0 Read FcomputationId Write SetcomputationId;
    Property rangeAssignments : TStreamingComputationRangesrangeAssignments Index 8 Read FrangeAssignments Write SetrangeAssignments;
  end;
  TStreamingComputationRangesClass = Class of TStreamingComputationRanges;
  
  { --------------------------------------------------------------------
    TStreamingComputationRangesrangeAssignments
    --------------------------------------------------------------------}
  
  TStreamingComputationRangesrangeAssignments = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TStreamingComputationRangesrangeAssignmentsClass = Class of TStreamingComputationRangesrangeAssignments;
  
  { --------------------------------------------------------------------
    TStreamingComputationTask
    --------------------------------------------------------------------}
  
  TStreamingComputationTask = Class(TGoogleBaseObject)
  Private
    FcomputationRanges : TStreamingComputationTaskcomputationRanges;
    FdataDisks : TStreamingComputationTaskdataDisks;
    FtaskType : string;
  Protected
    //Property setters
    Procedure SetcomputationRanges(AIndex : Integer; AValue : TStreamingComputationTaskcomputationRanges); virtual;
    Procedure SetdataDisks(AIndex : Integer; AValue : TStreamingComputationTaskdataDisks); virtual;
    Procedure SettaskType(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property computationRanges : TStreamingComputationTaskcomputationRanges Index 0 Read FcomputationRanges Write SetcomputationRanges;
    Property dataDisks : TStreamingComputationTaskdataDisks Index 8 Read FdataDisks Write SetdataDisks;
    Property taskType : string Index 16 Read FtaskType Write SettaskType;
  end;
  TStreamingComputationTaskClass = Class of TStreamingComputationTask;
  
  { --------------------------------------------------------------------
    TStreamingComputationTaskcomputationRanges
    --------------------------------------------------------------------}
  
  TStreamingComputationTaskcomputationRanges = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TStreamingComputationTaskcomputationRangesClass = Class of TStreamingComputationTaskcomputationRanges;
  
  { --------------------------------------------------------------------
    TStreamingComputationTaskdataDisks
    --------------------------------------------------------------------}
  
  TStreamingComputationTaskdataDisks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TStreamingComputationTaskdataDisksClass = Class of TStreamingComputationTaskdataDisks;
  
  { --------------------------------------------------------------------
    TStreamingSetupTask
    --------------------------------------------------------------------}
  
  TStreamingSetupTask = Class(TGoogleBaseObject)
  Private
    FreceiveWorkPort : integer;
    FstreamingComputationTopology : TTopologyConfig;
    FworkerHarnessPort : integer;
  Protected
    //Property setters
    Procedure SetreceiveWorkPort(AIndex : Integer; AValue : integer); virtual;
    Procedure SetstreamingComputationTopology(AIndex : Integer; AValue : TTopologyConfig); virtual;
    Procedure SetworkerHarnessPort(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property receiveWorkPort : integer Index 0 Read FreceiveWorkPort Write SetreceiveWorkPort;
    Property streamingComputationTopology : TTopologyConfig Index 8 Read FstreamingComputationTopology Write SetstreamingComputationTopology;
    Property workerHarnessPort : integer Index 16 Read FworkerHarnessPort Write SetworkerHarnessPort;
  end;
  TStreamingSetupTaskClass = Class of TStreamingSetupTask;
  
  { --------------------------------------------------------------------
    TStreamingSideInputLocation
    --------------------------------------------------------------------}
  
  TStreamingSideInputLocation = Class(TGoogleBaseObject)
  Private
    Ftag : string;
  Protected
    //Property setters
    Procedure Settag(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property tag : string Index 0 Read Ftag Write Settag;
  end;
  TStreamingSideInputLocationClass = Class of TStreamingSideInputLocation;
  
  { --------------------------------------------------------------------
    TStreamingStageLocation
    --------------------------------------------------------------------}
  
  TStreamingStageLocation = Class(TGoogleBaseObject)
  Private
    FstreamId : string;
  Protected
    //Property setters
    Procedure SetstreamId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property streamId : string Index 0 Read FstreamId Write SetstreamId;
  end;
  TStreamingStageLocationClass = Class of TStreamingStageLocation;
  
  { --------------------------------------------------------------------
    TTaskRunnerSettings
    --------------------------------------------------------------------}
  
  TTaskRunnerSettings = Class(TGoogleBaseObject)
  Private
    Falsologtostderr : boolean;
    FbaseTaskDir : string;
    FbaseUrl : string;
    FcommandlinesFileName : string;
    FcontinueOnException : boolean;
    FdataflowApiVersion : string;
    FharnessCommand : string;
    FlanguageHint : string;
    FlogDir : string;
    FlogToSerialconsole : boolean;
    FlogUploadLocation : string;
    FoauthScopes : TTaskRunnerSettingsoauthScopes;
    FparallelWorkerSettings : TWorkerSettings;
    FstreamingWorkerMainClass : string;
    FtaskGroup : string;
    FtaskUser : string;
    FtempStoragePrefix : string;
    FvmId : string;
    FworkflowFileName : string;
  Protected
    //Property setters
    Procedure Setalsologtostderr(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetbaseTaskDir(AIndex : Integer; AValue : string); virtual;
    Procedure SetbaseUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetcommandlinesFileName(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontinueOnException(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdataflowApiVersion(AIndex : Integer; AValue : string); virtual;
    Procedure SetharnessCommand(AIndex : Integer; AValue : string); virtual;
    Procedure SetlanguageHint(AIndex : Integer; AValue : string); virtual;
    Procedure SetlogDir(AIndex : Integer; AValue : string); virtual;
    Procedure SetlogToSerialconsole(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetlogUploadLocation(AIndex : Integer; AValue : string); virtual;
    Procedure SetoauthScopes(AIndex : Integer; AValue : TTaskRunnerSettingsoauthScopes); virtual;
    Procedure SetparallelWorkerSettings(AIndex : Integer; AValue : TWorkerSettings); virtual;
    Procedure SetstreamingWorkerMainClass(AIndex : Integer; AValue : string); virtual;
    Procedure SettaskGroup(AIndex : Integer; AValue : string); virtual;
    Procedure SettaskUser(AIndex : Integer; AValue : string); virtual;
    Procedure SettempStoragePrefix(AIndex : Integer; AValue : string); virtual;
    Procedure SetvmId(AIndex : Integer; AValue : string); virtual;
    Procedure SetworkflowFileName(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property alsologtostderr : boolean Index 0 Read Falsologtostderr Write Setalsologtostderr;
    Property baseTaskDir : string Index 8 Read FbaseTaskDir Write SetbaseTaskDir;
    Property baseUrl : string Index 16 Read FbaseUrl Write SetbaseUrl;
    Property commandlinesFileName : string Index 24 Read FcommandlinesFileName Write SetcommandlinesFileName;
    Property continueOnException : boolean Index 32 Read FcontinueOnException Write SetcontinueOnException;
    Property dataflowApiVersion : string Index 40 Read FdataflowApiVersion Write SetdataflowApiVersion;
    Property harnessCommand : string Index 48 Read FharnessCommand Write SetharnessCommand;
    Property languageHint : string Index 56 Read FlanguageHint Write SetlanguageHint;
    Property logDir : string Index 64 Read FlogDir Write SetlogDir;
    Property logToSerialconsole : boolean Index 72 Read FlogToSerialconsole Write SetlogToSerialconsole;
    Property logUploadLocation : string Index 80 Read FlogUploadLocation Write SetlogUploadLocation;
    Property oauthScopes : TTaskRunnerSettingsoauthScopes Index 88 Read FoauthScopes Write SetoauthScopes;
    Property parallelWorkerSettings : TWorkerSettings Index 96 Read FparallelWorkerSettings Write SetparallelWorkerSettings;
    Property streamingWorkerMainClass : string Index 104 Read FstreamingWorkerMainClass Write SetstreamingWorkerMainClass;
    Property taskGroup : string Index 112 Read FtaskGroup Write SettaskGroup;
    Property taskUser : string Index 120 Read FtaskUser Write SettaskUser;
    Property tempStoragePrefix : string Index 128 Read FtempStoragePrefix Write SettempStoragePrefix;
    Property vmId : string Index 136 Read FvmId Write SetvmId;
    Property workflowFileName : string Index 144 Read FworkflowFileName Write SetworkflowFileName;
  end;
  TTaskRunnerSettingsClass = Class of TTaskRunnerSettings;
  
  { --------------------------------------------------------------------
    TTaskRunnerSettingsoauthScopes
    --------------------------------------------------------------------}
  
  TTaskRunnerSettingsoauthScopes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTaskRunnerSettingsoauthScopesClass = Class of TTaskRunnerSettingsoauthScopes;
  
  { --------------------------------------------------------------------
    TTopologyConfig
    --------------------------------------------------------------------}
  
  TTopologyConfig = Class(TGoogleBaseObject)
  Private
    Fcomputations : TTopologyConfigcomputations;
    FdataDiskAssignments : TTopologyConfigdataDiskAssignments;
  Protected
    //Property setters
    Procedure Setcomputations(AIndex : Integer; AValue : TTopologyConfigcomputations); virtual;
    Procedure SetdataDiskAssignments(AIndex : Integer; AValue : TTopologyConfigdataDiskAssignments); virtual;
  Public
  Published
    Property computations : TTopologyConfigcomputations Index 0 Read Fcomputations Write Setcomputations;
    Property dataDiskAssignments : TTopologyConfigdataDiskAssignments Index 8 Read FdataDiskAssignments Write SetdataDiskAssignments;
  end;
  TTopologyConfigClass = Class of TTopologyConfig;
  
  { --------------------------------------------------------------------
    TTopologyConfigcomputations
    --------------------------------------------------------------------}
  
  TTopologyConfigcomputations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTopologyConfigcomputationsClass = Class of TTopologyConfigcomputations;
  
  { --------------------------------------------------------------------
    TTopologyConfigdataDiskAssignments
    --------------------------------------------------------------------}
  
  TTopologyConfigdataDiskAssignments = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTopologyConfigdataDiskAssignmentsClass = Class of TTopologyConfigdataDiskAssignments;
  
  { --------------------------------------------------------------------
    TWorkItem
    --------------------------------------------------------------------}
  
  TWorkItem = Class(TGoogleBaseObject)
  Private
    Fconfiguration : string;
    Fid : string;
    FinitialReportIndex : string;
    FjobId : string;
    FleaseExpireTime : string;
    FmapTask : TMapTask;
    Fpackages : TWorkItempackages;
    FprojectId : string;
    FreportStatusInterval : string;
    FseqMapTask : TSeqMapTask;
    FshellTask : TShellTask;
    FsourceOperationTask : TSourceOperationRequest;
    FstreamingComputationTask : TStreamingComputationTask;
    FstreamingSetupTask : TStreamingSetupTask;
  Protected
    //Property setters
    Procedure Setconfiguration(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinitialReportIndex(AIndex : Integer; AValue : string); virtual;
    Procedure SetjobId(AIndex : Integer; AValue : string); virtual;
    Procedure SetleaseExpireTime(AIndex : Integer; AValue : string); virtual;
    Procedure SetmapTask(AIndex : Integer; AValue : TMapTask); virtual;
    Procedure Setpackages(AIndex : Integer; AValue : TWorkItempackages); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : string); virtual;
    Procedure SetreportStatusInterval(AIndex : Integer; AValue : string); virtual;
    Procedure SetseqMapTask(AIndex : Integer; AValue : TSeqMapTask); virtual;
    Procedure SetshellTask(AIndex : Integer; AValue : TShellTask); virtual;
    Procedure SetsourceOperationTask(AIndex : Integer; AValue : TSourceOperationRequest); virtual;
    Procedure SetstreamingComputationTask(AIndex : Integer; AValue : TStreamingComputationTask); virtual;
    Procedure SetstreamingSetupTask(AIndex : Integer; AValue : TStreamingSetupTask); virtual;
  Public
  Published
    Property configuration : string Index 0 Read Fconfiguration Write Setconfiguration;
    Property id : string Index 8 Read Fid Write Setid;
    Property initialReportIndex : string Index 16 Read FinitialReportIndex Write SetinitialReportIndex;
    Property jobId : string Index 24 Read FjobId Write SetjobId;
    Property leaseExpireTime : string Index 32 Read FleaseExpireTime Write SetleaseExpireTime;
    Property mapTask : TMapTask Index 40 Read FmapTask Write SetmapTask;
    Property packages : TWorkItempackages Index 48 Read Fpackages Write Setpackages;
    Property projectId : string Index 56 Read FprojectId Write SetprojectId;
    Property reportStatusInterval : string Index 64 Read FreportStatusInterval Write SetreportStatusInterval;
    Property seqMapTask : TSeqMapTask Index 72 Read FseqMapTask Write SetseqMapTask;
    Property shellTask : TShellTask Index 80 Read FshellTask Write SetshellTask;
    Property sourceOperationTask : TSourceOperationRequest Index 88 Read FsourceOperationTask Write SetsourceOperationTask;
    Property streamingComputationTask : TStreamingComputationTask Index 96 Read FstreamingComputationTask Write SetstreamingComputationTask;
    Property streamingSetupTask : TStreamingSetupTask Index 104 Read FstreamingSetupTask Write SetstreamingSetupTask;
  end;
  TWorkItemClass = Class of TWorkItem;
  
  { --------------------------------------------------------------------
    TWorkItempackages
    --------------------------------------------------------------------}
  
  TWorkItempackages = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TWorkItempackagesClass = Class of TWorkItempackages;
  
  { --------------------------------------------------------------------
    TWorkItemServiceState
    --------------------------------------------------------------------}
  
  TWorkItemServiceState = Class(TGoogleBaseObject)
  Private
    FharnessData : TWorkItemServiceStateharnessData;
    FleaseExpireTime : string;
    FnextReportIndex : string;
    FreportStatusInterval : string;
    FsuggestedStopPoint : TApproximateProgress;
    FsuggestedStopPosition : TPosition;
  Protected
    //Property setters
    Procedure SetharnessData(AIndex : Integer; AValue : TWorkItemServiceStateharnessData); virtual;
    Procedure SetleaseExpireTime(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextReportIndex(AIndex : Integer; AValue : string); virtual;
    Procedure SetreportStatusInterval(AIndex : Integer; AValue : string); virtual;
    Procedure SetsuggestedStopPoint(AIndex : Integer; AValue : TApproximateProgress); virtual;
    Procedure SetsuggestedStopPosition(AIndex : Integer; AValue : TPosition); virtual;
  Public
  Published
    Property harnessData : TWorkItemServiceStateharnessData Index 0 Read FharnessData Write SetharnessData;
    Property leaseExpireTime : string Index 8 Read FleaseExpireTime Write SetleaseExpireTime;
    Property nextReportIndex : string Index 16 Read FnextReportIndex Write SetnextReportIndex;
    Property reportStatusInterval : string Index 24 Read FreportStatusInterval Write SetreportStatusInterval;
    Property suggestedStopPoint : TApproximateProgress Index 32 Read FsuggestedStopPoint Write SetsuggestedStopPoint;
    Property suggestedStopPosition : TPosition Index 40 Read FsuggestedStopPosition Write SetsuggestedStopPosition;
  end;
  TWorkItemServiceStateClass = Class of TWorkItemServiceState;
  
  { --------------------------------------------------------------------
    TWorkItemServiceStateharnessData
    --------------------------------------------------------------------}
  
  TWorkItemServiceStateharnessData = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TWorkItemServiceStateharnessDataClass = Class of TWorkItemServiceStateharnessData;
  
  { --------------------------------------------------------------------
    TWorkItemStatus
    --------------------------------------------------------------------}
  
  TWorkItemStatus = Class(TGoogleBaseObject)
  Private
    Fcompleted : boolean;
    FdynamicSourceSplit : TDynamicSourceSplit;
    Ferrors : TWorkItemStatuserrors;
    FmetricUpdates : TWorkItemStatusmetricUpdates;
    Fprogress : TApproximateProgress;
    FreportIndex : string;
    FrequestedLeaseDuration : string;
    FsourceFork : TSourceFork;
    FsourceOperationResponse : TSourceOperationResponse;
    FstopPosition : TPosition;
    FworkItemId : string;
  Protected
    //Property setters
    Procedure Setcompleted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdynamicSourceSplit(AIndex : Integer; AValue : TDynamicSourceSplit); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TWorkItemStatuserrors); virtual;
    Procedure SetmetricUpdates(AIndex : Integer; AValue : TWorkItemStatusmetricUpdates); virtual;
    Procedure Setprogress(AIndex : Integer; AValue : TApproximateProgress); virtual;
    Procedure SetreportIndex(AIndex : Integer; AValue : string); virtual;
    Procedure SetrequestedLeaseDuration(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceFork(AIndex : Integer; AValue : TSourceFork); virtual;
    Procedure SetsourceOperationResponse(AIndex : Integer; AValue : TSourceOperationResponse); virtual;
    Procedure SetstopPosition(AIndex : Integer; AValue : TPosition); virtual;
    Procedure SetworkItemId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property completed : boolean Index 0 Read Fcompleted Write Setcompleted;
    Property dynamicSourceSplit : TDynamicSourceSplit Index 8 Read FdynamicSourceSplit Write SetdynamicSourceSplit;
    Property errors : TWorkItemStatuserrors Index 16 Read Ferrors Write Seterrors;
    Property metricUpdates : TWorkItemStatusmetricUpdates Index 24 Read FmetricUpdates Write SetmetricUpdates;
    Property progress : TApproximateProgress Index 32 Read Fprogress Write Setprogress;
    Property reportIndex : string Index 40 Read FreportIndex Write SetreportIndex;
    Property requestedLeaseDuration : string Index 48 Read FrequestedLeaseDuration Write SetrequestedLeaseDuration;
    Property sourceFork : TSourceFork Index 56 Read FsourceFork Write SetsourceFork;
    Property sourceOperationResponse : TSourceOperationResponse Index 64 Read FsourceOperationResponse Write SetsourceOperationResponse;
    Property stopPosition : TPosition Index 72 Read FstopPosition Write SetstopPosition;
    Property workItemId : string Index 80 Read FworkItemId Write SetworkItemId;
  end;
  TWorkItemStatusClass = Class of TWorkItemStatus;
  
  { --------------------------------------------------------------------
    TWorkItemStatuserrors
    --------------------------------------------------------------------}
  
  TWorkItemStatuserrors = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TWorkItemStatuserrorsClass = Class of TWorkItemStatuserrors;
  
  { --------------------------------------------------------------------
    TWorkItemStatusmetricUpdates
    --------------------------------------------------------------------}
  
  TWorkItemStatusmetricUpdates = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TWorkItemStatusmetricUpdatesClass = Class of TWorkItemStatusmetricUpdates;
  
  { --------------------------------------------------------------------
    TWorkerPool
    --------------------------------------------------------------------}
  
  TWorkerPool = Class(TGoogleBaseObject)
  Private
    FautoscalingSettings : TAutoscalingSettings;
    FdataDisks : TWorkerPooldataDisks;
    FdefaultPackageSet : string;
    FdiskSizeGb : integer;
    FdiskSourceImage : string;
    FdiskType : string;
    Fkind : string;
    FmachineType : string;
    Fmetadata : TWorkerPoolmetadata;
    FnumWorkers : integer;
    FonHostMaintenance : string;
    Fpackages : TWorkerPoolpackages;
    FpoolArgs : TWorkerPoolpoolArgs;
    FtaskrunnerSettings : TTaskRunnerSettings;
    FteardownPolicy : string;
    Fzone : string;
  Protected
    //Property setters
    Procedure SetautoscalingSettings(AIndex : Integer; AValue : TAutoscalingSettings); virtual;
    Procedure SetdataDisks(AIndex : Integer; AValue : TWorkerPooldataDisks); virtual;
    Procedure SetdefaultPackageSet(AIndex : Integer; AValue : string); virtual;
    Procedure SetdiskSizeGb(AIndex : Integer; AValue : integer); virtual;
    Procedure SetdiskSourceImage(AIndex : Integer; AValue : string); virtual;
    Procedure SetdiskType(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmachineType(AIndex : Integer; AValue : string); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TWorkerPoolmetadata); virtual;
    Procedure SetnumWorkers(AIndex : Integer; AValue : integer); virtual;
    Procedure SetonHostMaintenance(AIndex : Integer; AValue : string); virtual;
    Procedure Setpackages(AIndex : Integer; AValue : TWorkerPoolpackages); virtual;
    Procedure SetpoolArgs(AIndex : Integer; AValue : TWorkerPoolpoolArgs); virtual;
    Procedure SettaskrunnerSettings(AIndex : Integer; AValue : TTaskRunnerSettings); virtual;
    Procedure SetteardownPolicy(AIndex : Integer; AValue : string); virtual;
    Procedure Setzone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property autoscalingSettings : TAutoscalingSettings Index 0 Read FautoscalingSettings Write SetautoscalingSettings;
    Property dataDisks : TWorkerPooldataDisks Index 8 Read FdataDisks Write SetdataDisks;
    Property defaultPackageSet : string Index 16 Read FdefaultPackageSet Write SetdefaultPackageSet;
    Property diskSizeGb : integer Index 24 Read FdiskSizeGb Write SetdiskSizeGb;
    Property diskSourceImage : string Index 32 Read FdiskSourceImage Write SetdiskSourceImage;
    Property diskType : string Index 40 Read FdiskType Write SetdiskType;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property machineType : string Index 56 Read FmachineType Write SetmachineType;
    Property metadata : TWorkerPoolmetadata Index 64 Read Fmetadata Write Setmetadata;
    Property numWorkers : integer Index 72 Read FnumWorkers Write SetnumWorkers;
    Property onHostMaintenance : string Index 80 Read FonHostMaintenance Write SetonHostMaintenance;
    Property packages : TWorkerPoolpackages Index 88 Read Fpackages Write Setpackages;
    Property poolArgs : TWorkerPoolpoolArgs Index 96 Read FpoolArgs Write SetpoolArgs;
    Property taskrunnerSettings : TTaskRunnerSettings Index 104 Read FtaskrunnerSettings Write SettaskrunnerSettings;
    Property teardownPolicy : string Index 112 Read FteardownPolicy Write SetteardownPolicy;
    Property zone : string Index 120 Read Fzone Write Setzone;
  end;
  TWorkerPoolClass = Class of TWorkerPool;
  
  { --------------------------------------------------------------------
    TWorkerPooldataDisks
    --------------------------------------------------------------------}
  
  TWorkerPooldataDisks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TWorkerPooldataDisksClass = Class of TWorkerPooldataDisks;
  
  { --------------------------------------------------------------------
    TWorkerPoolmetadata
    --------------------------------------------------------------------}
  
  TWorkerPoolmetadata = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TWorkerPoolmetadataClass = Class of TWorkerPoolmetadata;
  
  { --------------------------------------------------------------------
    TWorkerPoolpackages
    --------------------------------------------------------------------}
  
  TWorkerPoolpackages = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TWorkerPoolpackagesClass = Class of TWorkerPoolpackages;
  
  { --------------------------------------------------------------------
    TWorkerPoolpoolArgs
    --------------------------------------------------------------------}
  
  TWorkerPoolpoolArgs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TWorkerPoolpoolArgsClass = Class of TWorkerPoolpoolArgs;
  
  { --------------------------------------------------------------------
    TWorkerSettings
    --------------------------------------------------------------------}
  
  TWorkerSettings = Class(TGoogleBaseObject)
  Private
    FbaseUrl : string;
    FreportingEnabled : boolean;
    FservicePath : string;
    FshuffleServicePath : string;
    FtempStoragePrefix : string;
    FworkerId : string;
  Protected
    //Property setters
    Procedure SetbaseUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetreportingEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetservicePath(AIndex : Integer; AValue : string); virtual;
    Procedure SetshuffleServicePath(AIndex : Integer; AValue : string); virtual;
    Procedure SettempStoragePrefix(AIndex : Integer; AValue : string); virtual;
    Procedure SetworkerId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property baseUrl : string Index 0 Read FbaseUrl Write SetbaseUrl;
    Property reportingEnabled : boolean Index 8 Read FreportingEnabled Write SetreportingEnabled;
    Property servicePath : string Index 16 Read FservicePath Write SetservicePath;
    Property shuffleServicePath : string Index 24 Read FshuffleServicePath Write SetshuffleServicePath;
    Property tempStoragePrefix : string Index 32 Read FtempStoragePrefix Write SettempStoragePrefix;
    Property workerId : string Index 40 Read FworkerId Write SetworkerId;
  end;
  TWorkerSettingsClass = Class of TWorkerSettings;
  
  { --------------------------------------------------------------------
    TWriteInstruction
    --------------------------------------------------------------------}
  
  TWriteInstruction = Class(TGoogleBaseObject)
  Private
    Finput : TInstructionInput;
    Fsink : TSink;
  Protected
    //Property setters
    Procedure Setinput(AIndex : Integer; AValue : TInstructionInput); virtual;
    Procedure Setsink(AIndex : Integer; AValue : TSink); virtual;
  Public
  Published
    Property input : TInstructionInput Index 0 Read Finput Write Setinput;
    Property sink : TSink Index 8 Read Fsink Write Setsink;
  end;
  TWriteInstructionClass = Class of TWriteInstruction;
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
  end;
  
  
  { --------------------------------------------------------------------
    TDataflowAPI
    --------------------------------------------------------------------}
  
  TDataflowAPI = Class(TGoogleAPI)
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
  TApproximateProgress
  --------------------------------------------------------------------}


Procedure TApproximateProgress.SetpercentComplete(AIndex : Integer; AValue : integer); 

begin
  If (FpercentComplete=AValue) then exit;
  FpercentComplete:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApproximateProgress.Setposition(AIndex : Integer; AValue : TPosition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApproximateProgress.SetremainingTime(AIndex : Integer; AValue : string); 

begin
  If (FremainingTime=AValue) then exit;
  FremainingTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAutoscalingSettings
  --------------------------------------------------------------------}


Procedure TAutoscalingSettings.Setalgorithm(AIndex : Integer; AValue : string); 

begin
  If (Falgorithm=AValue) then exit;
  Falgorithm:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingSettings.SetmaxNumWorkers(AIndex : Integer; AValue : integer); 

begin
  If (FmaxNumWorkers=AValue) then exit;
  FmaxNumWorkers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TComputationTopology
  --------------------------------------------------------------------}


Procedure TComputationTopology.SetcomputationId(AIndex : Integer; AValue : string); 

begin
  If (FcomputationId=AValue) then exit;
  FcomputationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComputationTopology.Setinputs(AIndex : Integer; AValue : TComputationTopologyinputs); 

begin
  If (Finputs=AValue) then exit;
  Finputs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComputationTopology.SetkeyRanges(AIndex : Integer; AValue : TComputationTopologykeyRanges); 

begin
  If (FkeyRanges=AValue) then exit;
  FkeyRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComputationTopology.Setoutputs(AIndex : Integer; AValue : TComputationTopologyoutputs); 

begin
  If (Foutputs=AValue) then exit;
  Foutputs:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TComputationTopologyinputs
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TComputationTopologykeyRanges
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TComputationTopologyoutputs
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDataDiskAssignment
  --------------------------------------------------------------------}


Procedure TDataDiskAssignment.SetdataDisks(AIndex : Integer; AValue : TDataDiskAssignmentdataDisks); 

begin
  If (FdataDisks=AValue) then exit;
  FdataDisks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataDiskAssignment.SetvmInstance(AIndex : Integer; AValue : string); 

begin
  If (FvmInstance=AValue) then exit;
  FvmInstance:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDataDiskAssignmentdataDisks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDerivedSource
  --------------------------------------------------------------------}


Procedure TDerivedSource.SetderivationMode(AIndex : Integer; AValue : string); 

begin
  If (FderivationMode=AValue) then exit;
  FderivationMode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDerivedSource.Setsource(AIndex : Integer; AValue : TSource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDisk
  --------------------------------------------------------------------}


Procedure TDisk.SetdiskType(AIndex : Integer; AValue : string); 

begin
  If (FdiskType=AValue) then exit;
  FdiskType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.SetmountPoint(AIndex : Integer; AValue : string); 

begin
  If (FmountPoint=AValue) then exit;
  FmountPoint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.SetsizeGb(AIndex : Integer; AValue : integer); 

begin
  If (FsizeGb=AValue) then exit;
  FsizeGb:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDynamicSourceSplit
  --------------------------------------------------------------------}


Procedure TDynamicSourceSplit.Setprimary(AIndex : Integer; AValue : TDerivedSource); 

begin
  If (Fprimary=AValue) then exit;
  Fprimary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDynamicSourceSplit.Setresidual(AIndex : Integer; AValue : TDerivedSource); 

begin
  If (Fresidual=AValue) then exit;
  Fresidual:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEnvironment
  --------------------------------------------------------------------}


Procedure TEnvironment.SetclusterManagerApiService(AIndex : Integer; AValue : string); 

begin
  If (FclusterManagerApiService=AValue) then exit;
  FclusterManagerApiService:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.Setdataset(AIndex : Integer; AValue : string); 

begin
  If (Fdataset=AValue) then exit;
  Fdataset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.Setexperiments(AIndex : Integer; AValue : TEnvironmentexperiments); 

begin
  If (Fexperiments=AValue) then exit;
  Fexperiments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetsdkPipelineOptions(AIndex : Integer; AValue : TEnvironmentsdkPipelineOptions); 

begin
  If (FsdkPipelineOptions=AValue) then exit;
  FsdkPipelineOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SettempStoragePrefix(AIndex : Integer; AValue : string); 

begin
  If (FtempStoragePrefix=AValue) then exit;
  FtempStoragePrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetuserAgent(AIndex : Integer; AValue : TEnvironmentuserAgent); 

begin
  If (FuserAgent=AValue) then exit;
  FuserAgent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.Setversion(AIndex : Integer; AValue : TEnvironmentversion); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetworkerPools(AIndex : Integer; AValue : TEnvironmentworkerPools); 

begin
  If (FworkerPools=AValue) then exit;
  FworkerPools:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEnvironmentexperiments
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEnvironmentsdkPipelineOptions
  --------------------------------------------------------------------}


Class Function TEnvironmentsdkPipelineOptions.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TEnvironmentuserAgent
  --------------------------------------------------------------------}


Class Function TEnvironmentuserAgent.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TEnvironmentversion
  --------------------------------------------------------------------}


Class Function TEnvironmentversion.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TEnvironmentworkerPools
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFlattenInstruction
  --------------------------------------------------------------------}


Procedure TFlattenInstruction.Setinputs(AIndex : Integer; AValue : TFlattenInstructioninputs); 

begin
  If (Finputs=AValue) then exit;
  Finputs:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFlattenInstructioninputs
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGoogleprotobufValue
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TInstructionInput
  --------------------------------------------------------------------}


Procedure TInstructionInput.SetoutputNum(AIndex : Integer; AValue : integer); 

begin
  If (FoutputNum=AValue) then exit;
  FoutputNum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstructionInput.SetproducerInstructionIndex(AIndex : Integer; AValue : integer); 

begin
  If (FproducerInstructionIndex=AValue) then exit;
  FproducerInstructionIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstructionOutput
  --------------------------------------------------------------------}


Procedure TInstructionOutput.Setcodec(AIndex : Integer; AValue : TInstructionOutputcodec); 

begin
  If (Fcodec=AValue) then exit;
  Fcodec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstructionOutput.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstructionOutputcodec
  --------------------------------------------------------------------}


Class Function TInstructionOutputcodec.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TJob
  --------------------------------------------------------------------}


Procedure TJob.SetcreateTime(AIndex : Integer; AValue : string); 

begin
  If (FcreateTime=AValue) then exit;
  FcreateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetcurrentState(AIndex : Integer; AValue : string); 

begin
  If (FcurrentState=AValue) then exit;
  FcurrentState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetcurrentStateTime(AIndex : Integer; AValue : string); 

begin
  If (FcurrentStateTime=AValue) then exit;
  FcurrentStateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setenvironment(AIndex : Integer; AValue : TEnvironment); 

begin
  If (Fenvironment=AValue) then exit;
  Fenvironment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetexecutionInfo(AIndex : Integer; AValue : TJobExecutionInfo); 

begin
  If (FexecutionInfo=AValue) then exit;
  FexecutionInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetprojectId(AIndex : Integer; AValue : string); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetrequestedState(AIndex : Integer; AValue : string); 

begin
  If (FrequestedState=AValue) then exit;
  FrequestedState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setsteps(AIndex : Integer; AValue : TJobsteps); 

begin
  If (Fsteps=AValue) then exit;
  Fsteps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
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




{ --------------------------------------------------------------------
  TJobsteps
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TJobExecutionInfo
  --------------------------------------------------------------------}


Procedure TJobExecutionInfo.Setstages(AIndex : Integer; AValue : TJobExecutionInfostages); 

begin
  If (Fstages=AValue) then exit;
  Fstages:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobExecutionInfostages
  --------------------------------------------------------------------}


Class Function TJobExecutionInfostages.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TJobExecutionStageInfo
  --------------------------------------------------------------------}


Procedure TJobExecutionStageInfo.SetstepName(AIndex : Integer; AValue : TJobExecutionStageInfostepName); 

begin
  If (FstepName=AValue) then exit;
  FstepName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobExecutionStageInfostepName
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TJobMessage
  --------------------------------------------------------------------}


Procedure TJobMessage.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobMessage.SetmessageImportance(AIndex : Integer; AValue : string); 

begin
  If (FmessageImportance=AValue) then exit;
  FmessageImportance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobMessage.SetmessageText(AIndex : Integer; AValue : string); 

begin
  If (FmessageText=AValue) then exit;
  FmessageText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobMessage.Settime(AIndex : Integer; AValue : string); 

begin
  If (Ftime=AValue) then exit;
  Ftime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobMetrics
  --------------------------------------------------------------------}


Procedure TJobMetrics.SetmetricTime(AIndex : Integer; AValue : string); 

begin
  If (FmetricTime=AValue) then exit;
  FmetricTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobMetrics.Setmetrics(AIndex : Integer; AValue : TJobMetricsmetrics); 

begin
  If (Fmetrics=AValue) then exit;
  Fmetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobMetricsmetrics
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TKeyRangeDataDiskAssignment
  --------------------------------------------------------------------}


Procedure TKeyRangeDataDiskAssignment.SetdataDisk(AIndex : Integer; AValue : string); 

begin
  If (FdataDisk=AValue) then exit;
  FdataDisk:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeDataDiskAssignment.Set_end(AIndex : Integer; AValue : string); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeDataDiskAssignment.Setstart(AIndex : Integer; AValue : string); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
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
  TKeyRangeLocation
  --------------------------------------------------------------------}


Procedure TKeyRangeLocation.SetdataDisk(AIndex : Integer; AValue : string); 

begin
  If (FdataDisk=AValue) then exit;
  FdataDisk:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeLocation.SetdeliveryEndpoint(AIndex : Integer; AValue : string); 

begin
  If (FdeliveryEndpoint=AValue) then exit;
  FdeliveryEndpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeLocation.Set_end(AIndex : Integer; AValue : string); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeLocation.SetpersistentDirectory(AIndex : Integer; AValue : string); 

begin
  If (FpersistentDirectory=AValue) then exit;
  FpersistentDirectory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeLocation.Setstart(AIndex : Integer; AValue : string); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
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
  TLeaseWorkItemRequest
  --------------------------------------------------------------------}


Procedure TLeaseWorkItemRequest.SetcurrentWorkerTime(AIndex : Integer; AValue : string); 

begin
  If (FcurrentWorkerTime=AValue) then exit;
  FcurrentWorkerTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaseWorkItemRequest.SetrequestedLeaseDuration(AIndex : Integer; AValue : string); 

begin
  If (FrequestedLeaseDuration=AValue) then exit;
  FrequestedLeaseDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaseWorkItemRequest.SetworkItemTypes(AIndex : Integer; AValue : TLeaseWorkItemRequestworkItemTypes); 

begin
  If (FworkItemTypes=AValue) then exit;
  FworkItemTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaseWorkItemRequest.SetworkerCapabilities(AIndex : Integer; AValue : TLeaseWorkItemRequestworkerCapabilities); 

begin
  If (FworkerCapabilities=AValue) then exit;
  FworkerCapabilities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaseWorkItemRequest.SetworkerId(AIndex : Integer; AValue : string); 

begin
  If (FworkerId=AValue) then exit;
  FworkerId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLeaseWorkItemRequestworkItemTypes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLeaseWorkItemRequestworkerCapabilities
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLeaseWorkItemResponse
  --------------------------------------------------------------------}


Procedure TLeaseWorkItemResponse.SetworkItems(AIndex : Integer; AValue : TLeaseWorkItemResponseworkItems); 

begin
  If (FworkItems=AValue) then exit;
  FworkItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLeaseWorkItemResponseworkItems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListJobMessagesResponse
  --------------------------------------------------------------------}


Procedure TListJobMessagesResponse.SetjobMessages(AIndex : Integer; AValue : TListJobMessagesResponsejobMessages); 

begin
  If (FjobMessages=AValue) then exit;
  FjobMessages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListJobMessagesResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListJobMessagesResponsejobMessages
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListJobsResponse
  --------------------------------------------------------------------}


Procedure TListJobsResponse.Setjobs(AIndex : Integer; AValue : TListJobsResponsejobs); 

begin
  If (Fjobs=AValue) then exit;
  Fjobs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListJobsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListJobsResponsejobs
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMapTask
  --------------------------------------------------------------------}


Procedure TMapTask.Setinstructions(AIndex : Integer; AValue : TMapTaskinstructions); 

begin
  If (Finstructions=AValue) then exit;
  Finstructions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapTask.SetstageName(AIndex : Integer; AValue : string); 

begin
  If (FstageName=AValue) then exit;
  FstageName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapTask.SetsystemName(AIndex : Integer; AValue : string); 

begin
  If (FsystemName=AValue) then exit;
  FsystemName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMapTaskinstructions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMetricStructuredName
  --------------------------------------------------------------------}


Procedure TMetricStructuredName.Setcontext(AIndex : Integer; AValue : TMetricStructuredNamecontext); 

begin
  If (Fcontext=AValue) then exit;
  Fcontext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricStructuredName.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricStructuredName.Setorigin(AIndex : Integer; AValue : string); 

begin
  If (Forigin=AValue) then exit;
  Forigin:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetricStructuredNamecontext
  --------------------------------------------------------------------}


Class Function TMetricStructuredNamecontext.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TMetricUpdate
  --------------------------------------------------------------------}


Procedure TMetricUpdate.Setcumulative(AIndex : Integer; AValue : boolean); 

begin
  If (Fcumulative=AValue) then exit;
  Fcumulative:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricUpdate.Setinternal(AIndex : Integer; AValue : TGoogleprotobufValue); 

begin
  If (Finternal=AValue) then exit;
  Finternal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricUpdate.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricUpdate.SetmeanCount(AIndex : Integer; AValue : TGoogleprotobufValue); 

begin
  If (FmeanCount=AValue) then exit;
  FmeanCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricUpdate.SetmeanSum(AIndex : Integer; AValue : TGoogleprotobufValue); 

begin
  If (FmeanSum=AValue) then exit;
  FmeanSum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricUpdate.Setname(AIndex : Integer; AValue : TMetricStructuredName); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricUpdate.Setscalar(AIndex : Integer; AValue : TGoogleprotobufValue); 

begin
  If (Fscalar=AValue) then exit;
  Fscalar:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricUpdate.Set_set(AIndex : Integer; AValue : TGoogleprotobufValue); 

begin
  If (F_set=AValue) then exit;
  F_set:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricUpdate.SetupdateTime(AIndex : Integer; AValue : string); 

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
  TMountedDataDisk
  --------------------------------------------------------------------}


Procedure TMountedDataDisk.SetdataDisk(AIndex : Integer; AValue : string); 

begin
  If (FdataDisk=AValue) then exit;
  FdataDisk:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMultiOutputInfo
  --------------------------------------------------------------------}


Procedure TMultiOutputInfo.Settag(AIndex : Integer; AValue : string); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPackage
  --------------------------------------------------------------------}


Procedure TPackage.Setlocation(AIndex : Integer; AValue : string); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPackage.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TParDoInstruction
  --------------------------------------------------------------------}


Procedure TParDoInstruction.Setinput(AIndex : Integer; AValue : TInstructionInput); 

begin
  If (Finput=AValue) then exit;
  Finput:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParDoInstruction.SetmultiOutputInfos(AIndex : Integer; AValue : TParDoInstructionmultiOutputInfos); 

begin
  If (FmultiOutputInfos=AValue) then exit;
  FmultiOutputInfos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParDoInstruction.SetnumOutputs(AIndex : Integer; AValue : integer); 

begin
  If (FnumOutputs=AValue) then exit;
  FnumOutputs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParDoInstruction.SetsideInputs(AIndex : Integer; AValue : TParDoInstructionsideInputs); 

begin
  If (FsideInputs=AValue) then exit;
  FsideInputs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParDoInstruction.SetuserFn(AIndex : Integer; AValue : TParDoInstructionuserFn); 

begin
  If (FuserFn=AValue) then exit;
  FuserFn:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TParDoInstructionmultiOutputInfos
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TParDoInstructionsideInputs
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TParDoInstructionuserFn
  --------------------------------------------------------------------}


Class Function TParDoInstructionuserFn.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TParallelInstruction
  --------------------------------------------------------------------}


Procedure TParallelInstruction.Setflatten(AIndex : Integer; AValue : TFlattenInstruction); 

begin
  If (Fflatten=AValue) then exit;
  Fflatten:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParallelInstruction.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParallelInstruction.Setoutputs(AIndex : Integer; AValue : TParallelInstructionoutputs); 

begin
  If (Foutputs=AValue) then exit;
  Foutputs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParallelInstruction.SetparDo(AIndex : Integer; AValue : TParDoInstruction); 

begin
  If (FparDo=AValue) then exit;
  FparDo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParallelInstruction.SetpartialGroupByKey(AIndex : Integer; AValue : TPartialGroupByKeyInstruction); 

begin
  If (FpartialGroupByKey=AValue) then exit;
  FpartialGroupByKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParallelInstruction.Setread(AIndex : Integer; AValue : TReadInstruction); 

begin
  If (Fread=AValue) then exit;
  Fread:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParallelInstruction.SetsystemName(AIndex : Integer; AValue : string); 

begin
  If (FsystemName=AValue) then exit;
  FsystemName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParallelInstruction.Setwrite(AIndex : Integer; AValue : TWriteInstruction); 

begin
  If (Fwrite=AValue) then exit;
  Fwrite:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TParallelInstructionoutputs
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPartialGroupByKeyInstruction
  --------------------------------------------------------------------}


Procedure TPartialGroupByKeyInstruction.Setinput(AIndex : Integer; AValue : TInstructionInput); 

begin
  If (Finput=AValue) then exit;
  Finput:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPartialGroupByKeyInstruction.SetinputElementCodec(AIndex : Integer; AValue : TPartialGroupByKeyInstructioninputElementCodec); 

begin
  If (FinputElementCodec=AValue) then exit;
  FinputElementCodec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPartialGroupByKeyInstruction.SetvalueCombiningFn(AIndex : Integer; AValue : TPartialGroupByKeyInstructionvalueCombiningFn); 

begin
  If (FvalueCombiningFn=AValue) then exit;
  FvalueCombiningFn:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPartialGroupByKeyInstructioninputElementCodec
  --------------------------------------------------------------------}


Class Function TPartialGroupByKeyInstructioninputElementCodec.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TPartialGroupByKeyInstructionvalueCombiningFn
  --------------------------------------------------------------------}


Class Function TPartialGroupByKeyInstructionvalueCombiningFn.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TPosition
  --------------------------------------------------------------------}


Procedure TPosition.SetbyteOffset(AIndex : Integer; AValue : string); 

begin
  If (FbyteOffset=AValue) then exit;
  FbyteOffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.Set_end(AIndex : Integer; AValue : boolean); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.SetrecordIndex(AIndex : Integer; AValue : string); 

begin
  If (FrecordIndex=AValue) then exit;
  FrecordIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.SetshufflePosition(AIndex : Integer; AValue : string); 

begin
  If (FshufflePosition=AValue) then exit;
  FshufflePosition:=AValue;
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
  TPubsubLocation
  --------------------------------------------------------------------}


Procedure TPubsubLocation.SetdropLateData(AIndex : Integer; AValue : boolean); 

begin
  If (FdropLateData=AValue) then exit;
  FdropLateData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubLocation.SetidLabel(AIndex : Integer; AValue : string); 

begin
  If (FidLabel=AValue) then exit;
  FidLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubLocation.Setsubscription(AIndex : Integer; AValue : string); 

begin
  If (Fsubscription=AValue) then exit;
  Fsubscription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubLocation.SettimestampLabel(AIndex : Integer; AValue : string); 

begin
  If (FtimestampLabel=AValue) then exit;
  FtimestampLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubLocation.Settopic(AIndex : Integer; AValue : string); 

begin
  If (Ftopic=AValue) then exit;
  Ftopic:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubLocation.SettrackingSubscription(AIndex : Integer; AValue : string); 

begin
  If (FtrackingSubscription=AValue) then exit;
  FtrackingSubscription:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReadInstruction
  --------------------------------------------------------------------}


Procedure TReadInstruction.Setsource(AIndex : Integer; AValue : TSource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportWorkItemStatusRequest
  --------------------------------------------------------------------}


Procedure TReportWorkItemStatusRequest.SetcurrentWorkerTime(AIndex : Integer; AValue : string); 

begin
  If (FcurrentWorkerTime=AValue) then exit;
  FcurrentWorkerTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportWorkItemStatusRequest.SetworkItemStatuses(AIndex : Integer; AValue : TReportWorkItemStatusRequestworkItemStatuses); 

begin
  If (FworkItemStatuses=AValue) then exit;
  FworkItemStatuses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportWorkItemStatusRequest.SetworkerId(AIndex : Integer; AValue : string); 

begin
  If (FworkerId=AValue) then exit;
  FworkerId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportWorkItemStatusRequestworkItemStatuses
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReportWorkItemStatusResponse
  --------------------------------------------------------------------}


Procedure TReportWorkItemStatusResponse.SetworkItemServiceStates(AIndex : Integer; AValue : TReportWorkItemStatusResponseworkItemServiceStates); 

begin
  If (FworkItemServiceStates=AValue) then exit;
  FworkItemServiceStates:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportWorkItemStatusResponseworkItemServiceStates
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSeqMapTask
  --------------------------------------------------------------------}


Procedure TSeqMapTask.Setinputs(AIndex : Integer; AValue : TSeqMapTaskinputs); 

begin
  If (Finputs=AValue) then exit;
  Finputs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTask.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTask.SetoutputInfos(AIndex : Integer; AValue : TSeqMapTaskoutputInfos); 

begin
  If (FoutputInfos=AValue) then exit;
  FoutputInfos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTask.SetstageName(AIndex : Integer; AValue : string); 

begin
  If (FstageName=AValue) then exit;
  FstageName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTask.SetsystemName(AIndex : Integer; AValue : string); 

begin
  If (FsystemName=AValue) then exit;
  FsystemName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTask.SetuserFn(AIndex : Integer; AValue : TSeqMapTaskuserFn); 

begin
  If (FuserFn=AValue) then exit;
  FuserFn:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSeqMapTaskinputs
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSeqMapTaskoutputInfos
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSeqMapTaskuserFn
  --------------------------------------------------------------------}


Class Function TSeqMapTaskuserFn.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSeqMapTaskOutputInfo
  --------------------------------------------------------------------}


Procedure TSeqMapTaskOutputInfo.Setsink(AIndex : Integer; AValue : TSink); 

begin
  If (Fsink=AValue) then exit;
  Fsink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTaskOutputInfo.Settag(AIndex : Integer; AValue : string); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TShellTask
  --------------------------------------------------------------------}


Procedure TShellTask.Setcommand(AIndex : Integer; AValue : string); 

begin
  If (Fcommand=AValue) then exit;
  Fcommand:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TShellTask.SetexitCode(AIndex : Integer; AValue : integer); 

begin
  If (FexitCode=AValue) then exit;
  FexitCode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSideInputInfo
  --------------------------------------------------------------------}


Procedure TSideInputInfo.Setkind(AIndex : Integer; AValue : TSideInputInfokind); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSideInputInfo.Setsources(AIndex : Integer; AValue : TSideInputInfosources); 

begin
  If (Fsources=AValue) then exit;
  Fsources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSideInputInfo.Settag(AIndex : Integer; AValue : string); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSideInputInfokind
  --------------------------------------------------------------------}


Class Function TSideInputInfokind.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSideInputInfosources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSink
  --------------------------------------------------------------------}


Procedure TSink.Setcodec(AIndex : Integer; AValue : TSinkcodec); 

begin
  If (Fcodec=AValue) then exit;
  Fcodec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSink.Setspec(AIndex : Integer; AValue : TSinkspec); 

begin
  If (Fspec=AValue) then exit;
  Fspec:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSinkcodec
  --------------------------------------------------------------------}


Class Function TSinkcodec.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSinkspec
  --------------------------------------------------------------------}


Class Function TSinkspec.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSource
  --------------------------------------------------------------------}


Procedure TSource.SetbaseSpecs(AIndex : Integer; AValue : TSourcebaseSpecs); 

begin
  If (FbaseSpecs=AValue) then exit;
  FbaseSpecs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSource.Setcodec(AIndex : Integer; AValue : TSourcecodec); 

begin
  If (Fcodec=AValue) then exit;
  Fcodec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSource.SetdoesNotNeedSplitting(AIndex : Integer; AValue : boolean); 

begin
  If (FdoesNotNeedSplitting=AValue) then exit;
  FdoesNotNeedSplitting:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSource.Setmetadata(AIndex : Integer; AValue : TSourceMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSource.Setspec(AIndex : Integer; AValue : TSourcespec); 

begin
  If (Fspec=AValue) then exit;
  Fspec:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourcebaseSpecs
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSourcecodec
  --------------------------------------------------------------------}


Class Function TSourcecodec.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSourcespec
  --------------------------------------------------------------------}


Class Function TSourcespec.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSourceFork
  --------------------------------------------------------------------}


Procedure TSourceFork.Setprimary(AIndex : Integer; AValue : TSourceSplitShard); 

begin
  If (Fprimary=AValue) then exit;
  Fprimary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceFork.SetprimarySource(AIndex : Integer; AValue : TDerivedSource); 

begin
  If (FprimarySource=AValue) then exit;
  FprimarySource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceFork.Setresidual(AIndex : Integer; AValue : TSourceSplitShard); 

begin
  If (Fresidual=AValue) then exit;
  Fresidual:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceFork.SetresidualSource(AIndex : Integer; AValue : TDerivedSource); 

begin
  If (FresidualSource=AValue) then exit;
  FresidualSource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceGetMetadataRequest
  --------------------------------------------------------------------}


Procedure TSourceGetMetadataRequest.Setsource(AIndex : Integer; AValue : TSource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceGetMetadataResponse
  --------------------------------------------------------------------}


Procedure TSourceGetMetadataResponse.Setmetadata(AIndex : Integer; AValue : TSourceMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceMetadata
  --------------------------------------------------------------------}


Procedure TSourceMetadata.SetestimatedSizeBytes(AIndex : Integer; AValue : string); 

begin
  If (FestimatedSizeBytes=AValue) then exit;
  FestimatedSizeBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceMetadata.Setinfinite(AIndex : Integer; AValue : boolean); 

begin
  If (Finfinite=AValue) then exit;
  Finfinite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceMetadata.SetproducesSortedKeys(AIndex : Integer; AValue : boolean); 

begin
  If (FproducesSortedKeys=AValue) then exit;
  FproducesSortedKeys:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceOperationRequest
  --------------------------------------------------------------------}


Procedure TSourceOperationRequest.SetgetMetadata(AIndex : Integer; AValue : TSourceGetMetadataRequest); 

begin
  If (FgetMetadata=AValue) then exit;
  FgetMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceOperationRequest.Setsplit(AIndex : Integer; AValue : TSourceSplitRequest); 

begin
  If (Fsplit=AValue) then exit;
  Fsplit:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceOperationResponse
  --------------------------------------------------------------------}


Procedure TSourceOperationResponse.SetgetMetadata(AIndex : Integer; AValue : TSourceGetMetadataResponse); 

begin
  If (FgetMetadata=AValue) then exit;
  FgetMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceOperationResponse.Setsplit(AIndex : Integer; AValue : TSourceSplitResponse); 

begin
  If (Fsplit=AValue) then exit;
  Fsplit:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceSplitOptions
  --------------------------------------------------------------------}


Procedure TSourceSplitOptions.SetdesiredBundleSizeBytes(AIndex : Integer; AValue : string); 

begin
  If (FdesiredBundleSizeBytes=AValue) then exit;
  FdesiredBundleSizeBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceSplitOptions.SetdesiredShardSizeBytes(AIndex : Integer; AValue : string); 

begin
  If (FdesiredShardSizeBytes=AValue) then exit;
  FdesiredShardSizeBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceSplitRequest
  --------------------------------------------------------------------}


Procedure TSourceSplitRequest.Setoptions(AIndex : Integer; AValue : TSourceSplitOptions); 

begin
  If (Foptions=AValue) then exit;
  Foptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceSplitRequest.Setsource(AIndex : Integer; AValue : TSource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceSplitResponse
  --------------------------------------------------------------------}


Procedure TSourceSplitResponse.Setbundles(AIndex : Integer; AValue : TSourceSplitResponsebundles); 

begin
  If (Fbundles=AValue) then exit;
  Fbundles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceSplitResponse.Setoutcome(AIndex : Integer; AValue : string); 

begin
  If (Foutcome=AValue) then exit;
  Foutcome:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceSplitResponse.Setshards(AIndex : Integer; AValue : TSourceSplitResponseshards); 

begin
  If (Fshards=AValue) then exit;
  Fshards:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceSplitResponsebundles
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSourceSplitResponseshards
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSourceSplitShard
  --------------------------------------------------------------------}


Procedure TSourceSplitShard.SetderivationMode(AIndex : Integer; AValue : string); 

begin
  If (FderivationMode=AValue) then exit;
  FderivationMode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceSplitShard.Setsource(AIndex : Integer; AValue : TSource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStatus
  --------------------------------------------------------------------}


Procedure TStatus.Setcode(AIndex : Integer; AValue : integer); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setdetails(AIndex : Integer; AValue : TStatusdetails); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStatusdetails
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TStep
  --------------------------------------------------------------------}


Procedure TStep.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.Setproperties(AIndex : Integer; AValue : TStepproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStepproperties
  --------------------------------------------------------------------}


Class Function TStepproperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TStreamLocation
  --------------------------------------------------------------------}


Procedure TStreamLocation.SetpubsubLocation(AIndex : Integer; AValue : TPubsubLocation); 

begin
  If (FpubsubLocation=AValue) then exit;
  FpubsubLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamLocation.SetsideInputLocation(AIndex : Integer; AValue : TStreamingSideInputLocation); 

begin
  If (FsideInputLocation=AValue) then exit;
  FsideInputLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamLocation.SetstreamingStageLocation(AIndex : Integer; AValue : TStreamingStageLocation); 

begin
  If (FstreamingStageLocation=AValue) then exit;
  FstreamingStageLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStreamingComputationRanges
  --------------------------------------------------------------------}


Procedure TStreamingComputationRanges.SetcomputationId(AIndex : Integer; AValue : string); 

begin
  If (FcomputationId=AValue) then exit;
  FcomputationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingComputationRanges.SetrangeAssignments(AIndex : Integer; AValue : TStreamingComputationRangesrangeAssignments); 

begin
  If (FrangeAssignments=AValue) then exit;
  FrangeAssignments:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStreamingComputationRangesrangeAssignments
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TStreamingComputationTask
  --------------------------------------------------------------------}


Procedure TStreamingComputationTask.SetcomputationRanges(AIndex : Integer; AValue : TStreamingComputationTaskcomputationRanges); 

begin
  If (FcomputationRanges=AValue) then exit;
  FcomputationRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingComputationTask.SetdataDisks(AIndex : Integer; AValue : TStreamingComputationTaskdataDisks); 

begin
  If (FdataDisks=AValue) then exit;
  FdataDisks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingComputationTask.SettaskType(AIndex : Integer; AValue : string); 

begin
  If (FtaskType=AValue) then exit;
  FtaskType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStreamingComputationTaskcomputationRanges
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TStreamingComputationTaskdataDisks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TStreamingSetupTask
  --------------------------------------------------------------------}


Procedure TStreamingSetupTask.SetreceiveWorkPort(AIndex : Integer; AValue : integer); 

begin
  If (FreceiveWorkPort=AValue) then exit;
  FreceiveWorkPort:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingSetupTask.SetstreamingComputationTopology(AIndex : Integer; AValue : TTopologyConfig); 

begin
  If (FstreamingComputationTopology=AValue) then exit;
  FstreamingComputationTopology:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingSetupTask.SetworkerHarnessPort(AIndex : Integer; AValue : integer); 

begin
  If (FworkerHarnessPort=AValue) then exit;
  FworkerHarnessPort:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStreamingSideInputLocation
  --------------------------------------------------------------------}


Procedure TStreamingSideInputLocation.Settag(AIndex : Integer; AValue : string); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStreamingStageLocation
  --------------------------------------------------------------------}


Procedure TStreamingStageLocation.SetstreamId(AIndex : Integer; AValue : string); 

begin
  If (FstreamId=AValue) then exit;
  FstreamId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTaskRunnerSettings
  --------------------------------------------------------------------}


Procedure TTaskRunnerSettings.Setalsologtostderr(AIndex : Integer; AValue : boolean); 

begin
  If (Falsologtostderr=AValue) then exit;
  Falsologtostderr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetbaseTaskDir(AIndex : Integer; AValue : string); 

begin
  If (FbaseTaskDir=AValue) then exit;
  FbaseTaskDir:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetbaseUrl(AIndex : Integer; AValue : string); 

begin
  If (FbaseUrl=AValue) then exit;
  FbaseUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetcommandlinesFileName(AIndex : Integer; AValue : string); 

begin
  If (FcommandlinesFileName=AValue) then exit;
  FcommandlinesFileName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetcontinueOnException(AIndex : Integer; AValue : boolean); 

begin
  If (FcontinueOnException=AValue) then exit;
  FcontinueOnException:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetdataflowApiVersion(AIndex : Integer; AValue : string); 

begin
  If (FdataflowApiVersion=AValue) then exit;
  FdataflowApiVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetharnessCommand(AIndex : Integer; AValue : string); 

begin
  If (FharnessCommand=AValue) then exit;
  FharnessCommand:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetlanguageHint(AIndex : Integer; AValue : string); 

begin
  If (FlanguageHint=AValue) then exit;
  FlanguageHint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetlogDir(AIndex : Integer; AValue : string); 

begin
  If (FlogDir=AValue) then exit;
  FlogDir:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetlogToSerialconsole(AIndex : Integer; AValue : boolean); 

begin
  If (FlogToSerialconsole=AValue) then exit;
  FlogToSerialconsole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetlogUploadLocation(AIndex : Integer; AValue : string); 

begin
  If (FlogUploadLocation=AValue) then exit;
  FlogUploadLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetoauthScopes(AIndex : Integer; AValue : TTaskRunnerSettingsoauthScopes); 

begin
  If (FoauthScopes=AValue) then exit;
  FoauthScopes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetparallelWorkerSettings(AIndex : Integer; AValue : TWorkerSettings); 

begin
  If (FparallelWorkerSettings=AValue) then exit;
  FparallelWorkerSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetstreamingWorkerMainClass(AIndex : Integer; AValue : string); 

begin
  If (FstreamingWorkerMainClass=AValue) then exit;
  FstreamingWorkerMainClass:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SettaskGroup(AIndex : Integer; AValue : string); 

begin
  If (FtaskGroup=AValue) then exit;
  FtaskGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SettaskUser(AIndex : Integer; AValue : string); 

begin
  If (FtaskUser=AValue) then exit;
  FtaskUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SettempStoragePrefix(AIndex : Integer; AValue : string); 

begin
  If (FtempStoragePrefix=AValue) then exit;
  FtempStoragePrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetvmId(AIndex : Integer; AValue : string); 

begin
  If (FvmId=AValue) then exit;
  FvmId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetworkflowFileName(AIndex : Integer; AValue : string); 

begin
  If (FworkflowFileName=AValue) then exit;
  FworkflowFileName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTaskRunnerSettingsoauthScopes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTopologyConfig
  --------------------------------------------------------------------}


Procedure TTopologyConfig.Setcomputations(AIndex : Integer; AValue : TTopologyConfigcomputations); 

begin
  If (Fcomputations=AValue) then exit;
  Fcomputations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTopologyConfig.SetdataDiskAssignments(AIndex : Integer; AValue : TTopologyConfigdataDiskAssignments); 

begin
  If (FdataDiskAssignments=AValue) then exit;
  FdataDiskAssignments:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTopologyConfigcomputations
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTopologyConfigdataDiskAssignments
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWorkItem
  --------------------------------------------------------------------}


Procedure TWorkItem.Setconfiguration(AIndex : Integer; AValue : string); 

begin
  If (Fconfiguration=AValue) then exit;
  Fconfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetinitialReportIndex(AIndex : Integer; AValue : string); 

begin
  If (FinitialReportIndex=AValue) then exit;
  FinitialReportIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetjobId(AIndex : Integer; AValue : string); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetleaseExpireTime(AIndex : Integer; AValue : string); 

begin
  If (FleaseExpireTime=AValue) then exit;
  FleaseExpireTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetmapTask(AIndex : Integer; AValue : TMapTask); 

begin
  If (FmapTask=AValue) then exit;
  FmapTask:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.Setpackages(AIndex : Integer; AValue : TWorkItempackages); 

begin
  If (Fpackages=AValue) then exit;
  Fpackages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetprojectId(AIndex : Integer; AValue : string); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetreportStatusInterval(AIndex : Integer; AValue : string); 

begin
  If (FreportStatusInterval=AValue) then exit;
  FreportStatusInterval:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetseqMapTask(AIndex : Integer; AValue : TSeqMapTask); 

begin
  If (FseqMapTask=AValue) then exit;
  FseqMapTask:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetshellTask(AIndex : Integer; AValue : TShellTask); 

begin
  If (FshellTask=AValue) then exit;
  FshellTask:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetsourceOperationTask(AIndex : Integer; AValue : TSourceOperationRequest); 

begin
  If (FsourceOperationTask=AValue) then exit;
  FsourceOperationTask:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetstreamingComputationTask(AIndex : Integer; AValue : TStreamingComputationTask); 

begin
  If (FstreamingComputationTask=AValue) then exit;
  FstreamingComputationTask:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetstreamingSetupTask(AIndex : Integer; AValue : TStreamingSetupTask); 

begin
  If (FstreamingSetupTask=AValue) then exit;
  FstreamingSetupTask:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWorkItempackages
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWorkItemServiceState
  --------------------------------------------------------------------}


Procedure TWorkItemServiceState.SetharnessData(AIndex : Integer; AValue : TWorkItemServiceStateharnessData); 

begin
  If (FharnessData=AValue) then exit;
  FharnessData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemServiceState.SetleaseExpireTime(AIndex : Integer; AValue : string); 

begin
  If (FleaseExpireTime=AValue) then exit;
  FleaseExpireTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemServiceState.SetnextReportIndex(AIndex : Integer; AValue : string); 

begin
  If (FnextReportIndex=AValue) then exit;
  FnextReportIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemServiceState.SetreportStatusInterval(AIndex : Integer; AValue : string); 

begin
  If (FreportStatusInterval=AValue) then exit;
  FreportStatusInterval:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemServiceState.SetsuggestedStopPoint(AIndex : Integer; AValue : TApproximateProgress); 

begin
  If (FsuggestedStopPoint=AValue) then exit;
  FsuggestedStopPoint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemServiceState.SetsuggestedStopPosition(AIndex : Integer; AValue : TPosition); 

begin
  If (FsuggestedStopPosition=AValue) then exit;
  FsuggestedStopPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWorkItemServiceStateharnessData
  --------------------------------------------------------------------}


Class Function TWorkItemServiceStateharnessData.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWorkItemStatus
  --------------------------------------------------------------------}


Procedure TWorkItemStatus.Setcompleted(AIndex : Integer; AValue : boolean); 

begin
  If (Fcompleted=AValue) then exit;
  Fcompleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetdynamicSourceSplit(AIndex : Integer; AValue : TDynamicSourceSplit); 

begin
  If (FdynamicSourceSplit=AValue) then exit;
  FdynamicSourceSplit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.Seterrors(AIndex : Integer; AValue : TWorkItemStatuserrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetmetricUpdates(AIndex : Integer; AValue : TWorkItemStatusmetricUpdates); 

begin
  If (FmetricUpdates=AValue) then exit;
  FmetricUpdates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.Setprogress(AIndex : Integer; AValue : TApproximateProgress); 

begin
  If (Fprogress=AValue) then exit;
  Fprogress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetreportIndex(AIndex : Integer; AValue : string); 

begin
  If (FreportIndex=AValue) then exit;
  FreportIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetrequestedLeaseDuration(AIndex : Integer; AValue : string); 

begin
  If (FrequestedLeaseDuration=AValue) then exit;
  FrequestedLeaseDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetsourceFork(AIndex : Integer; AValue : TSourceFork); 

begin
  If (FsourceFork=AValue) then exit;
  FsourceFork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetsourceOperationResponse(AIndex : Integer; AValue : TSourceOperationResponse); 

begin
  If (FsourceOperationResponse=AValue) then exit;
  FsourceOperationResponse:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetstopPosition(AIndex : Integer; AValue : TPosition); 

begin
  If (FstopPosition=AValue) then exit;
  FstopPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetworkItemId(AIndex : Integer; AValue : string); 

begin
  If (FworkItemId=AValue) then exit;
  FworkItemId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWorkItemStatuserrors
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWorkItemStatusmetricUpdates
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWorkerPool
  --------------------------------------------------------------------}


Procedure TWorkerPool.SetautoscalingSettings(AIndex : Integer; AValue : TAutoscalingSettings); 

begin
  If (FautoscalingSettings=AValue) then exit;
  FautoscalingSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetdataDisks(AIndex : Integer; AValue : TWorkerPooldataDisks); 

begin
  If (FdataDisks=AValue) then exit;
  FdataDisks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetdefaultPackageSet(AIndex : Integer; AValue : string); 

begin
  If (FdefaultPackageSet=AValue) then exit;
  FdefaultPackageSet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetdiskSizeGb(AIndex : Integer; AValue : integer); 

begin
  If (FdiskSizeGb=AValue) then exit;
  FdiskSizeGb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetdiskSourceImage(AIndex : Integer; AValue : string); 

begin
  If (FdiskSourceImage=AValue) then exit;
  FdiskSourceImage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetdiskType(AIndex : Integer; AValue : string); 

begin
  If (FdiskType=AValue) then exit;
  FdiskType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetmachineType(AIndex : Integer; AValue : string); 

begin
  If (FmachineType=AValue) then exit;
  FmachineType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.Setmetadata(AIndex : Integer; AValue : TWorkerPoolmetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetnumWorkers(AIndex : Integer; AValue : integer); 

begin
  If (FnumWorkers=AValue) then exit;
  FnumWorkers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetonHostMaintenance(AIndex : Integer; AValue : string); 

begin
  If (FonHostMaintenance=AValue) then exit;
  FonHostMaintenance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.Setpackages(AIndex : Integer; AValue : TWorkerPoolpackages); 

begin
  If (Fpackages=AValue) then exit;
  Fpackages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetpoolArgs(AIndex : Integer; AValue : TWorkerPoolpoolArgs); 

begin
  If (FpoolArgs=AValue) then exit;
  FpoolArgs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SettaskrunnerSettings(AIndex : Integer; AValue : TTaskRunnerSettings); 

begin
  If (FtaskrunnerSettings=AValue) then exit;
  FtaskrunnerSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetteardownPolicy(AIndex : Integer; AValue : string); 

begin
  If (FteardownPolicy=AValue) then exit;
  FteardownPolicy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.Setzone(AIndex : Integer; AValue : string); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWorkerPooldataDisks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWorkerPoolmetadata
  --------------------------------------------------------------------}


Class Function TWorkerPoolmetadata.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWorkerPoolpackages
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TWorkerPoolpoolArgs
  --------------------------------------------------------------------}


Class Function TWorkerPoolpoolArgs.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWorkerSettings
  --------------------------------------------------------------------}


Procedure TWorkerSettings.SetbaseUrl(AIndex : Integer; AValue : string); 

begin
  If (FbaseUrl=AValue) then exit;
  FbaseUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerSettings.SetreportingEnabled(AIndex : Integer; AValue : boolean); 

begin
  If (FreportingEnabled=AValue) then exit;
  FreportingEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerSettings.SetservicePath(AIndex : Integer; AValue : string); 

begin
  If (FservicePath=AValue) then exit;
  FservicePath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerSettings.SetshuffleServicePath(AIndex : Integer; AValue : string); 

begin
  If (FshuffleServicePath=AValue) then exit;
  FshuffleServicePath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerSettings.SettempStoragePrefix(AIndex : Integer; AValue : string); 

begin
  If (FtempStoragePrefix=AValue) then exit;
  FtempStoragePrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerSettings.SetworkerId(AIndex : Integer; AValue : string); 

begin
  If (FworkerId=AValue) then exit;
  FworkerId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWriteInstruction
  --------------------------------------------------------------------}


Procedure TWriteInstruction.Setinput(AIndex : Integer; AValue : TInstructionInput); 

begin
  If (Finput=AValue) then exit;
  Finput:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWriteInstruction.Setsink(AIndex : Integer; AValue : TSink); 

begin
  If (Fsink=AValue) then exit;
  Fsink:=AValue;
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
  Result:=TdataflowAPI;
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
  Result:='20150417';
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
  Result:='Google Dataflow API.';
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
  Result:='';
end;

Class Function TDataflowAPI.APIrootUrl : string;

begin
  Result:='https://dataflow.googleapis.com/';
end;

Class Function TDataflowAPI.APIbasePath : string;

begin
  Result:='/v1b3/projects/';
end;

Class Function TDataflowAPI.APIbaseURL : String;

begin
  Result:='https://dataflow.googleapis.com/v1b3/projects/';
end;

Class Function TDataflowAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TDataflowAPI.APIservicePath : string;

begin
  Result:='v1b3/projects/';
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
  TApproximateProgress.RegisterObject;
  TAutoscalingSettings.RegisterObject;
  TComputationTopology.RegisterObject;
  TComputationTopologyinputs.RegisterObject;
  TComputationTopologykeyRanges.RegisterObject;
  TComputationTopologyoutputs.RegisterObject;
  TDataDiskAssignment.RegisterObject;
  TDataDiskAssignmentdataDisks.RegisterObject;
  TDerivedSource.RegisterObject;
  TDisk.RegisterObject;
  TDynamicSourceSplit.RegisterObject;
  TEnvironment.RegisterObject;
  TEnvironmentexperiments.RegisterObject;
  TEnvironmentsdkPipelineOptions.RegisterObject;
  TEnvironmentuserAgent.RegisterObject;
  TEnvironmentversion.RegisterObject;
  TEnvironmentworkerPools.RegisterObject;
  TFlattenInstruction.RegisterObject;
  TFlattenInstructioninputs.RegisterObject;
  TGoogleprotobufValue.RegisterObject;
  TInstructionInput.RegisterObject;
  TInstructionOutput.RegisterObject;
  TInstructionOutputcodec.RegisterObject;
  TJob.RegisterObject;
  TJobsteps.RegisterObject;
  TJobExecutionInfo.RegisterObject;
  TJobExecutionInfostages.RegisterObject;
  TJobExecutionStageInfo.RegisterObject;
  TJobExecutionStageInfostepName.RegisterObject;
  TJobMessage.RegisterObject;
  TJobMetrics.RegisterObject;
  TJobMetricsmetrics.RegisterObject;
  TKeyRangeDataDiskAssignment.RegisterObject;
  TKeyRangeLocation.RegisterObject;
  TLeaseWorkItemRequest.RegisterObject;
  TLeaseWorkItemRequestworkItemTypes.RegisterObject;
  TLeaseWorkItemRequestworkerCapabilities.RegisterObject;
  TLeaseWorkItemResponse.RegisterObject;
  TLeaseWorkItemResponseworkItems.RegisterObject;
  TListJobMessagesResponse.RegisterObject;
  TListJobMessagesResponsejobMessages.RegisterObject;
  TListJobsResponse.RegisterObject;
  TListJobsResponsejobs.RegisterObject;
  TMapTask.RegisterObject;
  TMapTaskinstructions.RegisterObject;
  TMetricStructuredName.RegisterObject;
  TMetricStructuredNamecontext.RegisterObject;
  TMetricUpdate.RegisterObject;
  TMountedDataDisk.RegisterObject;
  TMultiOutputInfo.RegisterObject;
  TPackage.RegisterObject;
  TParDoInstruction.RegisterObject;
  TParDoInstructionmultiOutputInfos.RegisterObject;
  TParDoInstructionsideInputs.RegisterObject;
  TParDoInstructionuserFn.RegisterObject;
  TParallelInstruction.RegisterObject;
  TParallelInstructionoutputs.RegisterObject;
  TPartialGroupByKeyInstruction.RegisterObject;
  TPartialGroupByKeyInstructioninputElementCodec.RegisterObject;
  TPartialGroupByKeyInstructionvalueCombiningFn.RegisterObject;
  TPosition.RegisterObject;
  TPubsubLocation.RegisterObject;
  TReadInstruction.RegisterObject;
  TReportWorkItemStatusRequest.RegisterObject;
  TReportWorkItemStatusRequestworkItemStatuses.RegisterObject;
  TReportWorkItemStatusResponse.RegisterObject;
  TReportWorkItemStatusResponseworkItemServiceStates.RegisterObject;
  TSeqMapTask.RegisterObject;
  TSeqMapTaskinputs.RegisterObject;
  TSeqMapTaskoutputInfos.RegisterObject;
  TSeqMapTaskuserFn.RegisterObject;
  TSeqMapTaskOutputInfo.RegisterObject;
  TShellTask.RegisterObject;
  TSideInputInfo.RegisterObject;
  TSideInputInfokind.RegisterObject;
  TSideInputInfosources.RegisterObject;
  TSink.RegisterObject;
  TSinkcodec.RegisterObject;
  TSinkspec.RegisterObject;
  TSource.RegisterObject;
  TSourcebaseSpecs.RegisterObject;
  TSourcecodec.RegisterObject;
  TSourcespec.RegisterObject;
  TSourceFork.RegisterObject;
  TSourceGetMetadataRequest.RegisterObject;
  TSourceGetMetadataResponse.RegisterObject;
  TSourceMetadata.RegisterObject;
  TSourceOperationRequest.RegisterObject;
  TSourceOperationResponse.RegisterObject;
  TSourceSplitOptions.RegisterObject;
  TSourceSplitRequest.RegisterObject;
  TSourceSplitResponse.RegisterObject;
  TSourceSplitResponsebundles.RegisterObject;
  TSourceSplitResponseshards.RegisterObject;
  TSourceSplitShard.RegisterObject;
  TStatus.RegisterObject;
  TStatusdetails.RegisterObject;
  TStep.RegisterObject;
  TStepproperties.RegisterObject;
  TStreamLocation.RegisterObject;
  TStreamingComputationRanges.RegisterObject;
  TStreamingComputationRangesrangeAssignments.RegisterObject;
  TStreamingComputationTask.RegisterObject;
  TStreamingComputationTaskcomputationRanges.RegisterObject;
  TStreamingComputationTaskdataDisks.RegisterObject;
  TStreamingSetupTask.RegisterObject;
  TStreamingSideInputLocation.RegisterObject;
  TStreamingStageLocation.RegisterObject;
  TTaskRunnerSettings.RegisterObject;
  TTaskRunnerSettingsoauthScopes.RegisterObject;
  TTopologyConfig.RegisterObject;
  TTopologyConfigcomputations.RegisterObject;
  TTopologyConfigdataDiskAssignments.RegisterObject;
  TWorkItem.RegisterObject;
  TWorkItempackages.RegisterObject;
  TWorkItemServiceState.RegisterObject;
  TWorkItemServiceStateharnessData.RegisterObject;
  TWorkItemStatus.RegisterObject;
  TWorkItemStatuserrors.RegisterObject;
  TWorkItemStatusmetricUpdates.RegisterObject;
  TWorkerPool.RegisterObject;
  TWorkerPooldataDisks.RegisterObject;
  TWorkerPoolmetadata.RegisterObject;
  TWorkerPoolpackages.RegisterObject;
  TWorkerPoolpoolArgs.RegisterObject;
  TWorkerSettings.RegisterObject;
  TWriteInstruction.RegisterObject;
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
  Result.API:=Self;
end;



initialization
  TDataflowAPI.RegisterAPI;
end.
