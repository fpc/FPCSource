unit googledataflow;
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
  TGoogleprotobufValue = TJSONSchema;
  TApproximateProgress = Class;
  TAutoscalingSettings = Class;
  TComputationTopology = Class;
  TDataDiskAssignment = Class;
  TDerivedSource = Class;
  TDisk = Class;
  TDynamicSourceSplit = Class;
  TEnvironment = Class;
  TFlattenInstruction = Class;
  TInstructionInput = Class;
  TInstructionOutput = Class;
  TJob = Class;
  TJobExecutionInfo = Class;
  TJobExecutionStageInfo = Class;
  TJobMessage = Class;
  TJobMetrics = Class;
  TKeyRangeDataDiskAssignment = Class;
  TKeyRangeLocation = Class;
  TLeaseWorkItemRequest = Class;
  TLeaseWorkItemResponse = Class;
  TListJobMessagesResponse = Class;
  TListJobsResponse = Class;
  TMapTask = Class;
  TMetricStructuredName = Class;
  TMetricUpdate = Class;
  TMountedDataDisk = Class;
  TMultiOutputInfo = Class;
  TPackage = Class;
  TParDoInstruction = Class;
  TParallelInstruction = Class;
  TPartialGroupByKeyInstruction = Class;
  TPosition = Class;
  TPubsubLocation = Class;
  TReadInstruction = Class;
  TReportWorkItemStatusRequest = Class;
  TReportWorkItemStatusResponse = Class;
  TSeqMapTask = Class;
  TSeqMapTaskOutputInfo = Class;
  TShellTask = Class;
  TSideInputInfo = Class;
  TSink = Class;
  TSource = Class;
  TSourceFork = Class;
  TSourceGetMetadataRequest = Class;
  TSourceGetMetadataResponse = Class;
  TSourceMetadata = Class;
  TSourceOperationRequest = Class;
  TSourceOperationResponse = Class;
  TSourceSplitOptions = Class;
  TSourceSplitRequest = Class;
  TSourceSplitResponse = Class;
  TSourceSplitShard = Class;
  TStatus = Class;
  TStep = Class;
  TStreamLocation = Class;
  TStreamingComputationRanges = Class;
  TStreamingComputationTask = Class;
  TStreamingSetupTask = Class;
  TStreamingSideInputLocation = Class;
  TStreamingStageLocation = Class;
  TTaskRunnerSettings = Class;
  TTopologyConfig = Class;
  TWorkItem = Class;
  TWorkItemServiceState = Class;
  TWorkItemStatus = Class;
  TWorkerPool = Class;
  TWorkerSettings = Class;
  TWriteInstruction = Class;
  TApproximateProgressArray = Array of TApproximateProgress;
  TAutoscalingSettingsArray = Array of TAutoscalingSettings;
  TComputationTopologyArray = Array of TComputationTopology;
  TDataDiskAssignmentArray = Array of TDataDiskAssignment;
  TDerivedSourceArray = Array of TDerivedSource;
  TDiskArray = Array of TDisk;
  TDynamicSourceSplitArray = Array of TDynamicSourceSplit;
  TEnvironmentArray = Array of TEnvironment;
  TFlattenInstructionArray = Array of TFlattenInstruction;
  TInstructionInputArray = Array of TInstructionInput;
  TInstructionOutputArray = Array of TInstructionOutput;
  TJobArray = Array of TJob;
  TJobExecutionInfoArray = Array of TJobExecutionInfo;
  TJobExecutionStageInfoArray = Array of TJobExecutionStageInfo;
  TJobMessageArray = Array of TJobMessage;
  TJobMetricsArray = Array of TJobMetrics;
  TKeyRangeDataDiskAssignmentArray = Array of TKeyRangeDataDiskAssignment;
  TKeyRangeLocationArray = Array of TKeyRangeLocation;
  TLeaseWorkItemRequestArray = Array of TLeaseWorkItemRequest;
  TLeaseWorkItemResponseArray = Array of TLeaseWorkItemResponse;
  TListJobMessagesResponseArray = Array of TListJobMessagesResponse;
  TListJobsResponseArray = Array of TListJobsResponse;
  TMapTaskArray = Array of TMapTask;
  TMetricStructuredNameArray = Array of TMetricStructuredName;
  TMetricUpdateArray = Array of TMetricUpdate;
  TMountedDataDiskArray = Array of TMountedDataDisk;
  TMultiOutputInfoArray = Array of TMultiOutputInfo;
  TPackageArray = Array of TPackage;
  TParDoInstructionArray = Array of TParDoInstruction;
  TParallelInstructionArray = Array of TParallelInstruction;
  TPartialGroupByKeyInstructionArray = Array of TPartialGroupByKeyInstruction;
  TPositionArray = Array of TPosition;
  TPubsubLocationArray = Array of TPubsubLocation;
  TReadInstructionArray = Array of TReadInstruction;
  TReportWorkItemStatusRequestArray = Array of TReportWorkItemStatusRequest;
  TReportWorkItemStatusResponseArray = Array of TReportWorkItemStatusResponse;
  TSeqMapTaskArray = Array of TSeqMapTask;
  TSeqMapTaskOutputInfoArray = Array of TSeqMapTaskOutputInfo;
  TShellTaskArray = Array of TShellTask;
  TSideInputInfoArray = Array of TSideInputInfo;
  TSinkArray = Array of TSink;
  TSourceArray = Array of TSource;
  TSourceForkArray = Array of TSourceFork;
  TSourceGetMetadataRequestArray = Array of TSourceGetMetadataRequest;
  TSourceGetMetadataResponseArray = Array of TSourceGetMetadataResponse;
  TSourceMetadataArray = Array of TSourceMetadata;
  TSourceOperationRequestArray = Array of TSourceOperationRequest;
  TSourceOperationResponseArray = Array of TSourceOperationResponse;
  TSourceSplitOptionsArray = Array of TSourceSplitOptions;
  TSourceSplitRequestArray = Array of TSourceSplitRequest;
  TSourceSplitResponseArray = Array of TSourceSplitResponse;
  TSourceSplitShardArray = Array of TSourceSplitShard;
  TStatusArray = Array of TStatus;
  TStepArray = Array of TStep;
  TStreamLocationArray = Array of TStreamLocation;
  TStreamingComputationRangesArray = Array of TStreamingComputationRanges;
  TStreamingComputationTaskArray = Array of TStreamingComputationTask;
  TStreamingSetupTaskArray = Array of TStreamingSetupTask;
  TStreamingSideInputLocationArray = Array of TStreamingSideInputLocation;
  TStreamingStageLocationArray = Array of TStreamingStageLocation;
  TTaskRunnerSettingsArray = Array of TTaskRunnerSettings;
  TTopologyConfigArray = Array of TTopologyConfig;
  TWorkItemArray = Array of TWorkItem;
  TWorkItemServiceStateArray = Array of TWorkItemServiceState;
  TWorkItemStatusArray = Array of TWorkItemStatus;
  TWorkerPoolArray = Array of TWorkerPool;
  TWorkerSettingsArray = Array of TWorkerSettings;
  TWriteInstructionArray = Array of TWriteInstruction;
  //Anonymous types, using auto-generated names
  TEnvironmentTypesdkPipelineOptions = Class;
  TEnvironmentTypeuserAgent = Class;
  TEnvironmentTypeversion = Class;
  TInstructionOutputTypecodec = Class;
  TJobExecutionInfoTypestages = Class;
  TMetricStructuredNameTypecontext = Class;
  TParDoInstructionTypeuserFn = Class;
  TPartialGroupByKeyInstructionTypeinputElementCodec = Class;
  TPartialGroupByKeyInstructionTypevalueCombiningFn = Class;
  TSeqMapTaskTypeuserFn = Class;
  TSideInputInfoTypekind = Class;
  TSinkTypecodec = Class;
  TSinkTypespec = Class;
  TSourceTypebaseSpecsItem = Class;
  TSourceTypecodec = Class;
  TSourceTypespec = Class;
  TStatusTypedetailsItem = Class;
  TStepTypeproperties = Class;
  TWorkItemServiceStateTypeharnessData = Class;
  TWorkerPoolTypemetadata = Class;
  TWorkerPoolTypepoolArgs = Class;
  TComputationTopologyTypeinputsArray = Array of TStreamLocation;
  TComputationTopologyTypekeyRangesArray = Array of TKeyRangeLocation;
  TComputationTopologyTypeoutputsArray = Array of TStreamLocation;
  TEnvironmentTypeworkerPoolsArray = Array of TWorkerPool;
  TFlattenInstructionTypeinputsArray = Array of TInstructionInput;
  TJobTypestepsArray = Array of TStep;
  TJobMetricsTypemetricsArray = Array of TMetricUpdate;
  TLeaseWorkItemResponseTypeworkItemsArray = Array of TWorkItem;
  TListJobMessagesResponseTypejobMessagesArray = Array of TJobMessage;
  TListJobsResponseTypejobsArray = Array of TJob;
  TMapTaskTypeinstructionsArray = Array of TParallelInstruction;
  TParDoInstructionTypemultiOutputInfosArray = Array of TMultiOutputInfo;
  TParDoInstructionTypesideInputsArray = Array of TSideInputInfo;
  TParallelInstructionTypeoutputsArray = Array of TInstructionOutput;
  TReportWorkItemStatusRequestTypeworkItemStatusesArray = Array of TWorkItemStatus;
  TReportWorkItemStatusResponseTypeworkItemServiceStatesArray = Array of TWorkItemServiceState;
  TSeqMapTaskTypeinputsArray = Array of TSideInputInfo;
  TSeqMapTaskTypeoutputInfosArray = Array of TSeqMapTaskOutputInfo;
  TSideInputInfoTypesourcesArray = Array of TSource;
  TSourceTypebaseSpecsArray = Array of TSourceTypebaseSpecsItem;
  TSourceSplitResponseTypebundlesArray = Array of TDerivedSource;
  TSourceSplitResponseTypeshardsArray = Array of TSourceSplitShard;
  TStatusTypedetailsArray = Array of TStatusTypedetailsItem;
  TStreamingComputationRangesTyperangeAssignmentsArray = Array of TKeyRangeDataDiskAssignment;
  TStreamingComputationTaskTypecomputationRangesArray = Array of TStreamingComputationRanges;
  TStreamingComputationTaskTypedataDisksArray = Array of TMountedDataDisk;
  TTopologyConfigTypecomputationsArray = Array of TComputationTopology;
  TTopologyConfigTypedataDiskAssignmentsArray = Array of TDataDiskAssignment;
  TWorkItemTypepackagesArray = Array of TPackage;
  TWorkItemStatusTypeerrorsArray = Array of TStatus;
  TWorkItemStatusTypemetricUpdatesArray = Array of TMetricUpdate;
  TWorkerPoolTypedataDisksArray = Array of TDisk;
  TWorkerPoolTypepackagesArray = Array of TPackage;
  
  { --------------------------------------------------------------------
    TApproximateProgress
    --------------------------------------------------------------------}
  
  TApproximateProgress = Class(TGoogleBaseObject)
  Private
    FpercentComplete : integer;
    Fposition : TPosition;
    FremainingTime : String;
  Protected
    //Property setters
    Procedure SetpercentComplete(AIndex : Integer; AValue : integer); virtual;
    Procedure Setposition(AIndex : Integer; AValue : TPosition); virtual;
    Procedure SetremainingTime(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property percentComplete : integer Index 0 Read FpercentComplete Write SetpercentComplete;
    Property position : TPosition Index 8 Read Fposition Write Setposition;
    Property remainingTime : String Index 16 Read FremainingTime Write SetremainingTime;
  end;
  TApproximateProgressClass = Class of TApproximateProgress;
  
  { --------------------------------------------------------------------
    TAutoscalingSettings
    --------------------------------------------------------------------}
  
  TAutoscalingSettings = Class(TGoogleBaseObject)
  Private
    Falgorithm : String;
    FmaxNumWorkers : integer;
  Protected
    //Property setters
    Procedure Setalgorithm(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaxNumWorkers(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property algorithm : String Index 0 Read Falgorithm Write Setalgorithm;
    Property maxNumWorkers : integer Index 8 Read FmaxNumWorkers Write SetmaxNumWorkers;
  end;
  TAutoscalingSettingsClass = Class of TAutoscalingSettings;
  
  { --------------------------------------------------------------------
    TComputationTopology
    --------------------------------------------------------------------}
  
  TComputationTopology = Class(TGoogleBaseObject)
  Private
    FcomputationId : String;
    Finputs : TComputationTopologyTypeinputsArray;
    FkeyRanges : TComputationTopologyTypekeyRangesArray;
    Foutputs : TComputationTopologyTypeoutputsArray;
  Protected
    //Property setters
    Procedure SetcomputationId(AIndex : Integer; AValue : String); virtual;
    Procedure Setinputs(AIndex : Integer; AValue : TComputationTopologyTypeinputsArray); virtual;
    Procedure SetkeyRanges(AIndex : Integer; AValue : TComputationTopologyTypekeyRangesArray); virtual;
    Procedure Setoutputs(AIndex : Integer; AValue : TComputationTopologyTypeoutputsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property computationId : String Index 0 Read FcomputationId Write SetcomputationId;
    Property inputs : TComputationTopologyTypeinputsArray Index 8 Read Finputs Write Setinputs;
    Property keyRanges : TComputationTopologyTypekeyRangesArray Index 16 Read FkeyRanges Write SetkeyRanges;
    Property outputs : TComputationTopologyTypeoutputsArray Index 24 Read Foutputs Write Setoutputs;
  end;
  TComputationTopologyClass = Class of TComputationTopology;
  
  { --------------------------------------------------------------------
    TDataDiskAssignment
    --------------------------------------------------------------------}
  
  TDataDiskAssignment = Class(TGoogleBaseObject)
  Private
    FdataDisks : TStringArray;
    FvmInstance : String;
  Protected
    //Property setters
    Procedure SetdataDisks(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetvmInstance(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property dataDisks : TStringArray Index 0 Read FdataDisks Write SetdataDisks;
    Property vmInstance : String Index 8 Read FvmInstance Write SetvmInstance;
  end;
  TDataDiskAssignmentClass = Class of TDataDiskAssignment;
  
  { --------------------------------------------------------------------
    TDerivedSource
    --------------------------------------------------------------------}
  
  TDerivedSource = Class(TGoogleBaseObject)
  Private
    FderivationMode : String;
    Fsource : TSource;
  Protected
    //Property setters
    Procedure SetderivationMode(AIndex : Integer; AValue : String); virtual;
    Procedure Setsource(AIndex : Integer; AValue : TSource); virtual;
  Public
  Published
    Property derivationMode : String Index 0 Read FderivationMode Write SetderivationMode;
    Property source : TSource Index 8 Read Fsource Write Setsource;
  end;
  TDerivedSourceClass = Class of TDerivedSource;
  
  { --------------------------------------------------------------------
    TDisk
    --------------------------------------------------------------------}
  
  TDisk = Class(TGoogleBaseObject)
  Private
    FdiskType : String;
    FmountPoint : String;
    FsizeGb : integer;
  Protected
    //Property setters
    Procedure SetdiskType(AIndex : Integer; AValue : String); virtual;
    Procedure SetmountPoint(AIndex : Integer; AValue : String); virtual;
    Procedure SetsizeGb(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property diskType : String Index 0 Read FdiskType Write SetdiskType;
    Property mountPoint : String Index 8 Read FmountPoint Write SetmountPoint;
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
    TEnvironment
    --------------------------------------------------------------------}
  
  TEnvironment = Class(TGoogleBaseObject)
  Private
    FclusterManagerApiService : String;
    Fdataset : String;
    Fexperiments : TStringArray;
    FsdkPipelineOptions : TEnvironmentTypesdkPipelineOptions;
    FtempStoragePrefix : String;
    FuserAgent : TEnvironmentTypeuserAgent;
    Fversion : TEnvironmentTypeversion;
    FworkerPools : TEnvironmentTypeworkerPoolsArray;
  Protected
    //Property setters
    Procedure SetclusterManagerApiService(AIndex : Integer; AValue : String); virtual;
    Procedure Setdataset(AIndex : Integer; AValue : String); virtual;
    Procedure Setexperiments(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetsdkPipelineOptions(AIndex : Integer; AValue : TEnvironmentTypesdkPipelineOptions); virtual;
    Procedure SettempStoragePrefix(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserAgent(AIndex : Integer; AValue : TEnvironmentTypeuserAgent); virtual;
    Procedure Setversion(AIndex : Integer; AValue : TEnvironmentTypeversion); virtual;
    Procedure SetworkerPools(AIndex : Integer; AValue : TEnvironmentTypeworkerPoolsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property clusterManagerApiService : String Index 0 Read FclusterManagerApiService Write SetclusterManagerApiService;
    Property dataset : String Index 8 Read Fdataset Write Setdataset;
    Property experiments : TStringArray Index 16 Read Fexperiments Write Setexperiments;
    Property sdkPipelineOptions : TEnvironmentTypesdkPipelineOptions Index 24 Read FsdkPipelineOptions Write SetsdkPipelineOptions;
    Property tempStoragePrefix : String Index 32 Read FtempStoragePrefix Write SettempStoragePrefix;
    Property userAgent : TEnvironmentTypeuserAgent Index 40 Read FuserAgent Write SetuserAgent;
    Property version : TEnvironmentTypeversion Index 48 Read Fversion Write Setversion;
    Property workerPools : TEnvironmentTypeworkerPoolsArray Index 56 Read FworkerPools Write SetworkerPools;
  end;
  TEnvironmentClass = Class of TEnvironment;
  
  { --------------------------------------------------------------------
    TFlattenInstruction
    --------------------------------------------------------------------}
  
  TFlattenInstruction = Class(TGoogleBaseObject)
  Private
    Finputs : TFlattenInstructionTypeinputsArray;
  Protected
    //Property setters
    Procedure Setinputs(AIndex : Integer; AValue : TFlattenInstructionTypeinputsArray); virtual;
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
    Fcodec : TInstructionOutputTypecodec;
    Fname : String;
  Protected
    //Property setters
    Procedure Setcodec(AIndex : Integer; AValue : TInstructionOutputTypecodec); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property codec : TInstructionOutputTypecodec Index 0 Read Fcodec Write Setcodec;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TInstructionOutputClass = Class of TInstructionOutput;
  
  { --------------------------------------------------------------------
    TJob
    --------------------------------------------------------------------}
  
  TJob = Class(TGoogleBaseObject)
  Private
    FcreateTime : String;
    FcurrentState : String;
    FcurrentStateTime : String;
    Fenvironment : TEnvironment;
    FexecutionInfo : TJobExecutionInfo;
    Fid : String;
    Fname : String;
    FprojectId : String;
    FrequestedState : String;
    Fsteps : TJobTypestepsArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetcreateTime(AIndex : Integer; AValue : String); virtual;
    Procedure SetcurrentState(AIndex : Integer; AValue : String); virtual;
    Procedure SetcurrentStateTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setenvironment(AIndex : Integer; AValue : TEnvironment); virtual;
    Procedure SetexecutionInfo(AIndex : Integer; AValue : TJobExecutionInfo); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : String); virtual;
    Procedure SetrequestedState(AIndex : Integer; AValue : String); virtual;
    Procedure Setsteps(AIndex : Integer; AValue : TJobTypestepsArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property createTime : String Index 0 Read FcreateTime Write SetcreateTime;
    Property currentState : String Index 8 Read FcurrentState Write SetcurrentState;
    Property currentStateTime : String Index 16 Read FcurrentStateTime Write SetcurrentStateTime;
    Property environment : TEnvironment Index 24 Read Fenvironment Write Setenvironment;
    Property executionInfo : TJobExecutionInfo Index 32 Read FexecutionInfo Write SetexecutionInfo;
    Property id : String Index 40 Read Fid Write Setid;
    Property name : String Index 48 Read Fname Write Setname;
    Property projectId : String Index 56 Read FprojectId Write SetprojectId;
    Property requestedState : String Index 64 Read FrequestedState Write SetrequestedState;
    Property steps : TJobTypestepsArray Index 72 Read Fsteps Write Setsteps;
    Property _type : String Index 80 Read F_type Write Set_type;
  end;
  TJobClass = Class of TJob;
  
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
    Procedure Setstages(AIndex : Integer; AValue : TJobExecutionInfoTypestages); virtual;
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
    Procedure SetstepName(AIndex : Integer; AValue : TStringArray); virtual;
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
    TJobMessage
    --------------------------------------------------------------------}
  
  TJobMessage = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FmessageImportance : String;
    FmessageText : String;
    Ftime : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetmessageImportance(AIndex : Integer; AValue : String); virtual;
    Procedure SetmessageText(AIndex : Integer; AValue : String); virtual;
    Procedure Settime(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property messageImportance : String Index 8 Read FmessageImportance Write SetmessageImportance;
    Property messageText : String Index 16 Read FmessageText Write SetmessageText;
    Property time : String Index 24 Read Ftime Write Settime;
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
    Procedure SetmetricTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setmetrics(AIndex : Integer; AValue : TJobMetricsTypemetricsArray); virtual;
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
    TKeyRangeDataDiskAssignment
    --------------------------------------------------------------------}
  
  TKeyRangeDataDiskAssignment = Class(TGoogleBaseObject)
  Private
    FdataDisk : String;
    F_end : String;
    Fstart : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdataDisk(AIndex : Integer; AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property dataDisk : String Index 0 Read FdataDisk Write SetdataDisk;
    Property _end : String Index 8 Read F_end Write Set_end;
    Property start : String Index 16 Read Fstart Write Setstart;
  end;
  TKeyRangeDataDiskAssignmentClass = Class of TKeyRangeDataDiskAssignment;
  
  { --------------------------------------------------------------------
    TKeyRangeLocation
    --------------------------------------------------------------------}
  
  TKeyRangeLocation = Class(TGoogleBaseObject)
  Private
    FdataDisk : String;
    FdeliveryEndpoint : String;
    F_end : String;
    FpersistentDirectory : String;
    Fstart : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdataDisk(AIndex : Integer; AValue : String); virtual;
    Procedure SetdeliveryEndpoint(AIndex : Integer; AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; AValue : String); virtual;
    Procedure SetpersistentDirectory(AIndex : Integer; AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property dataDisk : String Index 0 Read FdataDisk Write SetdataDisk;
    Property deliveryEndpoint : String Index 8 Read FdeliveryEndpoint Write SetdeliveryEndpoint;
    Property _end : String Index 16 Read F_end Write Set_end;
    Property persistentDirectory : String Index 24 Read FpersistentDirectory Write SetpersistentDirectory;
    Property start : String Index 32 Read Fstart Write Setstart;
  end;
  TKeyRangeLocationClass = Class of TKeyRangeLocation;
  
  { --------------------------------------------------------------------
    TLeaseWorkItemRequest
    --------------------------------------------------------------------}
  
  TLeaseWorkItemRequest = Class(TGoogleBaseObject)
  Private
    FcurrentWorkerTime : String;
    FrequestedLeaseDuration : String;
    FworkItemTypes : TStringArray;
    FworkerCapabilities : TStringArray;
    FworkerId : String;
  Protected
    //Property setters
    Procedure SetcurrentWorkerTime(AIndex : Integer; AValue : String); virtual;
    Procedure SetrequestedLeaseDuration(AIndex : Integer; AValue : String); virtual;
    Procedure SetworkItemTypes(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetworkerCapabilities(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetworkerId(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property currentWorkerTime : String Index 0 Read FcurrentWorkerTime Write SetcurrentWorkerTime;
    Property requestedLeaseDuration : String Index 8 Read FrequestedLeaseDuration Write SetrequestedLeaseDuration;
    Property workItemTypes : TStringArray Index 16 Read FworkItemTypes Write SetworkItemTypes;
    Property workerCapabilities : TStringArray Index 24 Read FworkerCapabilities Write SetworkerCapabilities;
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
    Procedure SetworkItems(AIndex : Integer; AValue : TLeaseWorkItemResponseTypeworkItemsArray); virtual;
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
    TListJobMessagesResponse
    --------------------------------------------------------------------}
  
  TListJobMessagesResponse = Class(TGoogleBaseObject)
  Private
    FjobMessages : TListJobMessagesResponseTypejobMessagesArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetjobMessages(AIndex : Integer; AValue : TListJobMessagesResponseTypejobMessagesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
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
    TListJobsResponse
    --------------------------------------------------------------------}
  
  TListJobsResponse = Class(TGoogleBaseObject)
  Private
    Fjobs : TListJobsResponseTypejobsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setjobs(AIndex : Integer; AValue : TListJobsResponseTypejobsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
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
    TMapTask
    --------------------------------------------------------------------}
  
  TMapTask = Class(TGoogleBaseObject)
  Private
    Finstructions : TMapTaskTypeinstructionsArray;
    FstageName : String;
    FsystemName : String;
  Protected
    //Property setters
    Procedure Setinstructions(AIndex : Integer; AValue : TMapTaskTypeinstructionsArray); virtual;
    Procedure SetstageName(AIndex : Integer; AValue : String); virtual;
    Procedure SetsystemName(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property instructions : TMapTaskTypeinstructionsArray Index 0 Read Finstructions Write Setinstructions;
    Property stageName : String Index 8 Read FstageName Write SetstageName;
    Property systemName : String Index 16 Read FsystemName Write SetsystemName;
  end;
  TMapTaskClass = Class of TMapTask;
  
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
    Fcontext : TMetricStructuredNameTypecontext;
    Fname : String;
    Forigin : String;
  Protected
    //Property setters
    Procedure Setcontext(AIndex : Integer; AValue : TMetricStructuredNameTypecontext); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setorigin(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property context : TMetricStructuredNameTypecontext Index 0 Read Fcontext Write Setcontext;
    Property name : String Index 8 Read Fname Write Setname;
    Property origin : String Index 16 Read Forigin Write Setorigin;
  end;
  TMetricStructuredNameClass = Class of TMetricStructuredName;
  
  { --------------------------------------------------------------------
    TMetricUpdate
    --------------------------------------------------------------------}
  
  TMetricUpdate = Class(TGoogleBaseObject)
  Private
    Fcumulative : boolean;
    Finternal : TGoogleprotobufValue;
    Fkind : String;
    FmeanCount : TGoogleprotobufValue;
    FmeanSum : TGoogleprotobufValue;
    Fname : TMetricStructuredName;
    Fscalar : TGoogleprotobufValue;
    F_set : TGoogleprotobufValue;
    FupdateTime : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcumulative(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setinternal(AIndex : Integer; AValue : TGoogleprotobufValue); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmeanCount(AIndex : Integer; AValue : TGoogleprotobufValue); virtual;
    Procedure SetmeanSum(AIndex : Integer; AValue : TGoogleprotobufValue); virtual;
    Procedure Setname(AIndex : Integer; AValue : TMetricStructuredName); virtual;
    Procedure Setscalar(AIndex : Integer; AValue : TGoogleprotobufValue); virtual;
    Procedure Set_set(AIndex : Integer; AValue : TGoogleprotobufValue); virtual;
    Procedure SetupdateTime(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property cumulative : boolean Index 0 Read Fcumulative Write Setcumulative;
    Property internal : TGoogleprotobufValue Index 8 Read Finternal Write Setinternal;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property meanCount : TGoogleprotobufValue Index 24 Read FmeanCount Write SetmeanCount;
    Property meanSum : TGoogleprotobufValue Index 32 Read FmeanSum Write SetmeanSum;
    Property name : TMetricStructuredName Index 40 Read Fname Write Setname;
    Property scalar : TGoogleprotobufValue Index 48 Read Fscalar Write Setscalar;
    Property _set : TGoogleprotobufValue Index 56 Read F_set Write Set_set;
    Property updateTime : String Index 64 Read FupdateTime Write SetupdateTime;
  end;
  TMetricUpdateClass = Class of TMetricUpdate;
  
  { --------------------------------------------------------------------
    TMountedDataDisk
    --------------------------------------------------------------------}
  
  TMountedDataDisk = Class(TGoogleBaseObject)
  Private
    FdataDisk : String;
  Protected
    //Property setters
    Procedure SetdataDisk(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property dataDisk : String Index 0 Read FdataDisk Write SetdataDisk;
  end;
  TMountedDataDiskClass = Class of TMountedDataDisk;
  
  { --------------------------------------------------------------------
    TMultiOutputInfo
    --------------------------------------------------------------------}
  
  TMultiOutputInfo = Class(TGoogleBaseObject)
  Private
    Ftag : String;
  Protected
    //Property setters
    Procedure Settag(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property tag : String Index 0 Read Ftag Write Settag;
  end;
  TMultiOutputInfoClass = Class of TMultiOutputInfo;
  
  { --------------------------------------------------------------------
    TPackage
    --------------------------------------------------------------------}
  
  TPackage = Class(TGoogleBaseObject)
  Private
    Flocation : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setlocation(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property location : String Index 0 Read Flocation Write Setlocation;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TPackageClass = Class of TPackage;
  
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
    FmultiOutputInfos : TParDoInstructionTypemultiOutputInfosArray;
    FnumOutputs : integer;
    FsideInputs : TParDoInstructionTypesideInputsArray;
    FuserFn : TParDoInstructionTypeuserFn;
  Protected
    //Property setters
    Procedure Setinput(AIndex : Integer; AValue : TInstructionInput); virtual;
    Procedure SetmultiOutputInfos(AIndex : Integer; AValue : TParDoInstructionTypemultiOutputInfosArray); virtual;
    Procedure SetnumOutputs(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsideInputs(AIndex : Integer; AValue : TParDoInstructionTypesideInputsArray); virtual;
    Procedure SetuserFn(AIndex : Integer; AValue : TParDoInstructionTypeuserFn); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property input : TInstructionInput Index 0 Read Finput Write Setinput;
    Property multiOutputInfos : TParDoInstructionTypemultiOutputInfosArray Index 8 Read FmultiOutputInfos Write SetmultiOutputInfos;
    Property numOutputs : integer Index 16 Read FnumOutputs Write SetnumOutputs;
    Property sideInputs : TParDoInstructionTypesideInputsArray Index 24 Read FsideInputs Write SetsideInputs;
    Property userFn : TParDoInstructionTypeuserFn Index 32 Read FuserFn Write SetuserFn;
  end;
  TParDoInstructionClass = Class of TParDoInstruction;
  
  { --------------------------------------------------------------------
    TParallelInstruction
    --------------------------------------------------------------------}
  
  TParallelInstruction = Class(TGoogleBaseObject)
  Private
    Fflatten : TFlattenInstruction;
    Fname : String;
    Foutputs : TParallelInstructionTypeoutputsArray;
    FparDo : TParDoInstruction;
    FpartialGroupByKey : TPartialGroupByKeyInstruction;
    Fread : TReadInstruction;
    FsystemName : String;
    Fwrite : TWriteInstruction;
  Protected
    //Property setters
    Procedure Setflatten(AIndex : Integer; AValue : TFlattenInstruction); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setoutputs(AIndex : Integer; AValue : TParallelInstructionTypeoutputsArray); virtual;
    Procedure SetparDo(AIndex : Integer; AValue : TParDoInstruction); virtual;
    Procedure SetpartialGroupByKey(AIndex : Integer; AValue : TPartialGroupByKeyInstruction); virtual;
    Procedure Setread(AIndex : Integer; AValue : TReadInstruction); virtual;
    Procedure SetsystemName(AIndex : Integer; AValue : String); virtual;
    Procedure Setwrite(AIndex : Integer; AValue : TWriteInstruction); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property flatten : TFlattenInstruction Index 0 Read Fflatten Write Setflatten;
    Property name : String Index 8 Read Fname Write Setname;
    Property outputs : TParallelInstructionTypeoutputsArray Index 16 Read Foutputs Write Setoutputs;
    Property parDo : TParDoInstruction Index 24 Read FparDo Write SetparDo;
    Property partialGroupByKey : TPartialGroupByKeyInstruction Index 32 Read FpartialGroupByKey Write SetpartialGroupByKey;
    Property read : TReadInstruction Index 40 Read Fread Write Setread;
    Property systemName : String Index 48 Read FsystemName Write SetsystemName;
    Property write : TWriteInstruction Index 56 Read Fwrite Write Setwrite;
  end;
  TParallelInstructionClass = Class of TParallelInstruction;
  
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
  Protected
    //Property setters
    Procedure Setinput(AIndex : Integer; AValue : TInstructionInput); virtual;
    Procedure SetinputElementCodec(AIndex : Integer; AValue : TPartialGroupByKeyInstructionTypeinputElementCodec); virtual;
    Procedure SetvalueCombiningFn(AIndex : Integer; AValue : TPartialGroupByKeyInstructionTypevalueCombiningFn); virtual;
  Public
  Published
    Property input : TInstructionInput Index 0 Read Finput Write Setinput;
    Property inputElementCodec : TPartialGroupByKeyInstructionTypeinputElementCodec Index 8 Read FinputElementCodec Write SetinputElementCodec;
    Property valueCombiningFn : TPartialGroupByKeyInstructionTypevalueCombiningFn Index 16 Read FvalueCombiningFn Write SetvalueCombiningFn;
  end;
  TPartialGroupByKeyInstructionClass = Class of TPartialGroupByKeyInstruction;
  
  { --------------------------------------------------------------------
    TPosition
    --------------------------------------------------------------------}
  
  TPosition = Class(TGoogleBaseObject)
  Private
    FbyteOffset : String;
    F_end : boolean;
    Fkey : String;
    FrecordIndex : String;
    FshufflePosition : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetbyteOffset(AIndex : Integer; AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkey(AIndex : Integer; AValue : String); virtual;
    Procedure SetrecordIndex(AIndex : Integer; AValue : String); virtual;
    Procedure SetshufflePosition(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property byteOffset : String Index 0 Read FbyteOffset Write SetbyteOffset;
    Property _end : boolean Index 8 Read F_end Write Set_end;
    Property key : String Index 16 Read Fkey Write Setkey;
    Property recordIndex : String Index 24 Read FrecordIndex Write SetrecordIndex;
    Property shufflePosition : String Index 32 Read FshufflePosition Write SetshufflePosition;
  end;
  TPositionClass = Class of TPosition;
  
  { --------------------------------------------------------------------
    TPubsubLocation
    --------------------------------------------------------------------}
  
  TPubsubLocation = Class(TGoogleBaseObject)
  Private
    FdropLateData : boolean;
    FidLabel : String;
    Fsubscription : String;
    FtimestampLabel : String;
    Ftopic : String;
    FtrackingSubscription : String;
  Protected
    //Property setters
    Procedure SetdropLateData(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetidLabel(AIndex : Integer; AValue : String); virtual;
    Procedure Setsubscription(AIndex : Integer; AValue : String); virtual;
    Procedure SettimestampLabel(AIndex : Integer; AValue : String); virtual;
    Procedure Settopic(AIndex : Integer; AValue : String); virtual;
    Procedure SettrackingSubscription(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property dropLateData : boolean Index 0 Read FdropLateData Write SetdropLateData;
    Property idLabel : String Index 8 Read FidLabel Write SetidLabel;
    Property subscription : String Index 16 Read Fsubscription Write Setsubscription;
    Property timestampLabel : String Index 24 Read FtimestampLabel Write SettimestampLabel;
    Property topic : String Index 32 Read Ftopic Write Settopic;
    Property trackingSubscription : String Index 40 Read FtrackingSubscription Write SettrackingSubscription;
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
    FcurrentWorkerTime : String;
    FworkItemStatuses : TReportWorkItemStatusRequestTypeworkItemStatusesArray;
    FworkerId : String;
  Protected
    //Property setters
    Procedure SetcurrentWorkerTime(AIndex : Integer; AValue : String); virtual;
    Procedure SetworkItemStatuses(AIndex : Integer; AValue : TReportWorkItemStatusRequestTypeworkItemStatusesArray); virtual;
    Procedure SetworkerId(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property currentWorkerTime : String Index 0 Read FcurrentWorkerTime Write SetcurrentWorkerTime;
    Property workItemStatuses : TReportWorkItemStatusRequestTypeworkItemStatusesArray Index 8 Read FworkItemStatuses Write SetworkItemStatuses;
    Property workerId : String Index 16 Read FworkerId Write SetworkerId;
  end;
  TReportWorkItemStatusRequestClass = Class of TReportWorkItemStatusRequest;
  
  { --------------------------------------------------------------------
    TReportWorkItemStatusResponse
    --------------------------------------------------------------------}
  
  TReportWorkItemStatusResponse = Class(TGoogleBaseObject)
  Private
    FworkItemServiceStates : TReportWorkItemStatusResponseTypeworkItemServiceStatesArray;
  Protected
    //Property setters
    Procedure SetworkItemServiceStates(AIndex : Integer; AValue : TReportWorkItemStatusResponseTypeworkItemServiceStatesArray); virtual;
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
    Fname : String;
    FoutputInfos : TSeqMapTaskTypeoutputInfosArray;
    FstageName : String;
    FsystemName : String;
    FuserFn : TSeqMapTaskTypeuserFn;
  Protected
    //Property setters
    Procedure Setinputs(AIndex : Integer; AValue : TSeqMapTaskTypeinputsArray); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetoutputInfos(AIndex : Integer; AValue : TSeqMapTaskTypeoutputInfosArray); virtual;
    Procedure SetstageName(AIndex : Integer; AValue : String); virtual;
    Procedure SetsystemName(AIndex : Integer; AValue : String); virtual;
    Procedure SetuserFn(AIndex : Integer; AValue : TSeqMapTaskTypeuserFn); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property inputs : TSeqMapTaskTypeinputsArray Index 0 Read Finputs Write Setinputs;
    Property name : String Index 8 Read Fname Write Setname;
    Property outputInfos : TSeqMapTaskTypeoutputInfosArray Index 16 Read FoutputInfos Write SetoutputInfos;
    Property stageName : String Index 24 Read FstageName Write SetstageName;
    Property systemName : String Index 32 Read FsystemName Write SetsystemName;
    Property userFn : TSeqMapTaskTypeuserFn Index 40 Read FuserFn Write SetuserFn;
  end;
  TSeqMapTaskClass = Class of TSeqMapTask;
  
  { --------------------------------------------------------------------
    TSeqMapTaskOutputInfo
    --------------------------------------------------------------------}
  
  TSeqMapTaskOutputInfo = Class(TGoogleBaseObject)
  Private
    Fsink : TSink;
    Ftag : String;
  Protected
    //Property setters
    Procedure Setsink(AIndex : Integer; AValue : TSink); virtual;
    Procedure Settag(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property sink : TSink Index 0 Read Fsink Write Setsink;
    Property tag : String Index 8 Read Ftag Write Settag;
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
    Procedure Setcommand(AIndex : Integer; AValue : String); virtual;
    Procedure SetexitCode(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property command : String Index 0 Read Fcommand Write Setcommand;
    Property exitCode : integer Index 8 Read FexitCode Write SetexitCode;
  end;
  TShellTaskClass = Class of TShellTask;
  
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
    Fkind : TSideInputInfoTypekind;
    Fsources : TSideInputInfoTypesourcesArray;
    Ftag : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : TSideInputInfoTypekind); virtual;
    Procedure Setsources(AIndex : Integer; AValue : TSideInputInfoTypesourcesArray); virtual;
    Procedure Settag(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property kind : TSideInputInfoTypekind Index 0 Read Fkind Write Setkind;
    Property sources : TSideInputInfoTypesourcesArray Index 8 Read Fsources Write Setsources;
    Property tag : String Index 16 Read Ftag Write Settag;
  end;
  TSideInputInfoClass = Class of TSideInputInfo;
  
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
    TSink
    --------------------------------------------------------------------}
  
  TSink = Class(TGoogleBaseObject)
  Private
    Fcodec : TSinkTypecodec;
    Fspec : TSinkTypespec;
  Protected
    //Property setters
    Procedure Setcodec(AIndex : Integer; AValue : TSinkTypecodec); virtual;
    Procedure Setspec(AIndex : Integer; AValue : TSinkTypespec); virtual;
  Public
  Published
    Property codec : TSinkTypecodec Index 0 Read Fcodec Write Setcodec;
    Property spec : TSinkTypespec Index 8 Read Fspec Write Setspec;
  end;
  TSinkClass = Class of TSink;
  
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
    TSource
    --------------------------------------------------------------------}
  
  TSource = Class(TGoogleBaseObject)
  Private
    FbaseSpecs : TSourceTypebaseSpecsArray;
    Fcodec : TSourceTypecodec;
    FdoesNotNeedSplitting : boolean;
    Fmetadata : TSourceMetadata;
    Fspec : TSourceTypespec;
  Protected
    //Property setters
    Procedure SetbaseSpecs(AIndex : Integer; AValue : TSourceTypebaseSpecsArray); virtual;
    Procedure Setcodec(AIndex : Integer; AValue : TSourceTypecodec); virtual;
    Procedure SetdoesNotNeedSplitting(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TSourceMetadata); virtual;
    Procedure Setspec(AIndex : Integer; AValue : TSourceTypespec); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property baseSpecs : TSourceTypebaseSpecsArray Index 0 Read FbaseSpecs Write SetbaseSpecs;
    Property codec : TSourceTypecodec Index 8 Read Fcodec Write Setcodec;
    Property doesNotNeedSplitting : boolean Index 16 Read FdoesNotNeedSplitting Write SetdoesNotNeedSplitting;
    Property metadata : TSourceMetadata Index 24 Read Fmetadata Write Setmetadata;
    Property spec : TSourceTypespec Index 32 Read Fspec Write Setspec;
  end;
  TSourceClass = Class of TSource;
  
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
    FestimatedSizeBytes : String;
    Finfinite : boolean;
    FproducesSortedKeys : boolean;
  Protected
    //Property setters
    Procedure SetestimatedSizeBytes(AIndex : Integer; AValue : String); virtual;
    Procedure Setinfinite(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetproducesSortedKeys(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property estimatedSizeBytes : String Index 0 Read FestimatedSizeBytes Write SetestimatedSizeBytes;
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
    FdesiredBundleSizeBytes : String;
    FdesiredShardSizeBytes : String;
  Protected
    //Property setters
    Procedure SetdesiredBundleSizeBytes(AIndex : Integer; AValue : String); virtual;
    Procedure SetdesiredShardSizeBytes(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property desiredBundleSizeBytes : String Index 0 Read FdesiredBundleSizeBytes Write SetdesiredBundleSizeBytes;
    Property desiredShardSizeBytes : String Index 8 Read FdesiredShardSizeBytes Write SetdesiredShardSizeBytes;
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
    Fbundles : TSourceSplitResponseTypebundlesArray;
    Foutcome : String;
    Fshards : TSourceSplitResponseTypeshardsArray;
  Protected
    //Property setters
    Procedure Setbundles(AIndex : Integer; AValue : TSourceSplitResponseTypebundlesArray); virtual;
    Procedure Setoutcome(AIndex : Integer; AValue : String); virtual;
    Procedure Setshards(AIndex : Integer; AValue : TSourceSplitResponseTypeshardsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property bundles : TSourceSplitResponseTypebundlesArray Index 0 Read Fbundles Write Setbundles;
    Property outcome : String Index 8 Read Foutcome Write Setoutcome;
    Property shards : TSourceSplitResponseTypeshardsArray Index 16 Read Fshards Write Setshards;
  end;
  TSourceSplitResponseClass = Class of TSourceSplitResponse;
  
  { --------------------------------------------------------------------
    TSourceSplitShard
    --------------------------------------------------------------------}
  
  TSourceSplitShard = Class(TGoogleBaseObject)
  Private
    FderivationMode : String;
    Fsource : TSource;
  Protected
    //Property setters
    Procedure SetderivationMode(AIndex : Integer; AValue : String); virtual;
    Procedure Setsource(AIndex : Integer; AValue : TSource); virtual;
  Public
  Published
    Property derivationMode : String Index 0 Read FderivationMode Write SetderivationMode;
    Property source : TSource Index 8 Read Fsource Write Setsource;
  end;
  TSourceSplitShardClass = Class of TSourceSplitShard;
  
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
    Fdetails : TStatusTypedetailsArray;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : integer); virtual;
    Procedure Setdetails(AIndex : Integer; AValue : TStatusTypedetailsArray); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property code : integer Index 0 Read Fcode Write Setcode;
    Property details : TStatusTypedetailsArray Index 8 Read Fdetails Write Setdetails;
    Property message : String Index 16 Read Fmessage Write Setmessage;
  end;
  TStatusClass = Class of TStatus;
  
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
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setproperties(AIndex : Integer; AValue : TStepTypeproperties); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property name : String Index 8 Read Fname Write Setname;
    Property properties : TStepTypeproperties Index 16 Read Fproperties Write Setproperties;
  end;
  TStepClass = Class of TStep;
  
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
    FcomputationId : String;
    FrangeAssignments : TStreamingComputationRangesTyperangeAssignmentsArray;
  Protected
    //Property setters
    Procedure SetcomputationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetrangeAssignments(AIndex : Integer; AValue : TStreamingComputationRangesTyperangeAssignmentsArray); virtual;
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
    TStreamingComputationTask
    --------------------------------------------------------------------}
  
  TStreamingComputationTask = Class(TGoogleBaseObject)
  Private
    FcomputationRanges : TStreamingComputationTaskTypecomputationRangesArray;
    FdataDisks : TStreamingComputationTaskTypedataDisksArray;
    FtaskType : String;
  Protected
    //Property setters
    Procedure SetcomputationRanges(AIndex : Integer; AValue : TStreamingComputationTaskTypecomputationRangesArray); virtual;
    Procedure SetdataDisks(AIndex : Integer; AValue : TStreamingComputationTaskTypedataDisksArray); virtual;
    Procedure SettaskType(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property computationRanges : TStreamingComputationTaskTypecomputationRangesArray Index 0 Read FcomputationRanges Write SetcomputationRanges;
    Property dataDisks : TStreamingComputationTaskTypedataDisksArray Index 8 Read FdataDisks Write SetdataDisks;
    Property taskType : String Index 16 Read FtaskType Write SettaskType;
  end;
  TStreamingComputationTaskClass = Class of TStreamingComputationTask;
  
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
    Ftag : String;
  Protected
    //Property setters
    Procedure Settag(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property tag : String Index 0 Read Ftag Write Settag;
  end;
  TStreamingSideInputLocationClass = Class of TStreamingSideInputLocation;
  
  { --------------------------------------------------------------------
    TStreamingStageLocation
    --------------------------------------------------------------------}
  
  TStreamingStageLocation = Class(TGoogleBaseObject)
  Private
    FstreamId : String;
  Protected
    //Property setters
    Procedure SetstreamId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property streamId : String Index 0 Read FstreamId Write SetstreamId;
  end;
  TStreamingStageLocationClass = Class of TStreamingStageLocation;
  
  { --------------------------------------------------------------------
    TTaskRunnerSettings
    --------------------------------------------------------------------}
  
  TTaskRunnerSettings = Class(TGoogleBaseObject)
  Private
    Falsologtostderr : boolean;
    FbaseTaskDir : String;
    FbaseUrl : String;
    FcommandlinesFileName : String;
    FcontinueOnException : boolean;
    FdataflowApiVersion : String;
    FharnessCommand : String;
    FlanguageHint : String;
    FlogDir : String;
    FlogToSerialconsole : boolean;
    FlogUploadLocation : String;
    FoauthScopes : TStringArray;
    FparallelWorkerSettings : TWorkerSettings;
    FstreamingWorkerMainClass : String;
    FtaskGroup : String;
    FtaskUser : String;
    FtempStoragePrefix : String;
    FvmId : String;
    FworkflowFileName : String;
  Protected
    //Property setters
    Procedure Setalsologtostderr(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetbaseTaskDir(AIndex : Integer; AValue : String); virtual;
    Procedure SetbaseUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetcommandlinesFileName(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontinueOnException(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdataflowApiVersion(AIndex : Integer; AValue : String); virtual;
    Procedure SetharnessCommand(AIndex : Integer; AValue : String); virtual;
    Procedure SetlanguageHint(AIndex : Integer; AValue : String); virtual;
    Procedure SetlogDir(AIndex : Integer; AValue : String); virtual;
    Procedure SetlogToSerialconsole(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetlogUploadLocation(AIndex : Integer; AValue : String); virtual;
    Procedure SetoauthScopes(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetparallelWorkerSettings(AIndex : Integer; AValue : TWorkerSettings); virtual;
    Procedure SetstreamingWorkerMainClass(AIndex : Integer; AValue : String); virtual;
    Procedure SettaskGroup(AIndex : Integer; AValue : String); virtual;
    Procedure SettaskUser(AIndex : Integer; AValue : String); virtual;
    Procedure SettempStoragePrefix(AIndex : Integer; AValue : String); virtual;
    Procedure SetvmId(AIndex : Integer; AValue : String); virtual;
    Procedure SetworkflowFileName(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property alsologtostderr : boolean Index 0 Read Falsologtostderr Write Setalsologtostderr;
    Property baseTaskDir : String Index 8 Read FbaseTaskDir Write SetbaseTaskDir;
    Property baseUrl : String Index 16 Read FbaseUrl Write SetbaseUrl;
    Property commandlinesFileName : String Index 24 Read FcommandlinesFileName Write SetcommandlinesFileName;
    Property continueOnException : boolean Index 32 Read FcontinueOnException Write SetcontinueOnException;
    Property dataflowApiVersion : String Index 40 Read FdataflowApiVersion Write SetdataflowApiVersion;
    Property harnessCommand : String Index 48 Read FharnessCommand Write SetharnessCommand;
    Property languageHint : String Index 56 Read FlanguageHint Write SetlanguageHint;
    Property logDir : String Index 64 Read FlogDir Write SetlogDir;
    Property logToSerialconsole : boolean Index 72 Read FlogToSerialconsole Write SetlogToSerialconsole;
    Property logUploadLocation : String Index 80 Read FlogUploadLocation Write SetlogUploadLocation;
    Property oauthScopes : TStringArray Index 88 Read FoauthScopes Write SetoauthScopes;
    Property parallelWorkerSettings : TWorkerSettings Index 96 Read FparallelWorkerSettings Write SetparallelWorkerSettings;
    Property streamingWorkerMainClass : String Index 104 Read FstreamingWorkerMainClass Write SetstreamingWorkerMainClass;
    Property taskGroup : String Index 112 Read FtaskGroup Write SettaskGroup;
    Property taskUser : String Index 120 Read FtaskUser Write SettaskUser;
    Property tempStoragePrefix : String Index 128 Read FtempStoragePrefix Write SettempStoragePrefix;
    Property vmId : String Index 136 Read FvmId Write SetvmId;
    Property workflowFileName : String Index 144 Read FworkflowFileName Write SetworkflowFileName;
  end;
  TTaskRunnerSettingsClass = Class of TTaskRunnerSettings;
  
  { --------------------------------------------------------------------
    TTopologyConfig
    --------------------------------------------------------------------}
  
  TTopologyConfig = Class(TGoogleBaseObject)
  Private
    Fcomputations : TTopologyConfigTypecomputationsArray;
    FdataDiskAssignments : TTopologyConfigTypedataDiskAssignmentsArray;
  Protected
    //Property setters
    Procedure Setcomputations(AIndex : Integer; AValue : TTopologyConfigTypecomputationsArray); virtual;
    Procedure SetdataDiskAssignments(AIndex : Integer; AValue : TTopologyConfigTypedataDiskAssignmentsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property computations : TTopologyConfigTypecomputationsArray Index 0 Read Fcomputations Write Setcomputations;
    Property dataDiskAssignments : TTopologyConfigTypedataDiskAssignmentsArray Index 8 Read FdataDiskAssignments Write SetdataDiskAssignments;
  end;
  TTopologyConfigClass = Class of TTopologyConfig;
  
  { --------------------------------------------------------------------
    TWorkItem
    --------------------------------------------------------------------}
  
  TWorkItem = Class(TGoogleBaseObject)
  Private
    Fconfiguration : String;
    Fid : String;
    FinitialReportIndex : String;
    FjobId : String;
    FleaseExpireTime : String;
    FmapTask : TMapTask;
    Fpackages : TWorkItemTypepackagesArray;
    FprojectId : String;
    FreportStatusInterval : String;
    FseqMapTask : TSeqMapTask;
    FshellTask : TShellTask;
    FsourceOperationTask : TSourceOperationRequest;
    FstreamingComputationTask : TStreamingComputationTask;
    FstreamingSetupTask : TStreamingSetupTask;
  Protected
    //Property setters
    Procedure Setconfiguration(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinitialReportIndex(AIndex : Integer; AValue : String); virtual;
    Procedure SetjobId(AIndex : Integer; AValue : String); virtual;
    Procedure SetleaseExpireTime(AIndex : Integer; AValue : String); virtual;
    Procedure SetmapTask(AIndex : Integer; AValue : TMapTask); virtual;
    Procedure Setpackages(AIndex : Integer; AValue : TWorkItemTypepackagesArray); virtual;
    Procedure SetprojectId(AIndex : Integer; AValue : String); virtual;
    Procedure SetreportStatusInterval(AIndex : Integer; AValue : String); virtual;
    Procedure SetseqMapTask(AIndex : Integer; AValue : TSeqMapTask); virtual;
    Procedure SetshellTask(AIndex : Integer; AValue : TShellTask); virtual;
    Procedure SetsourceOperationTask(AIndex : Integer; AValue : TSourceOperationRequest); virtual;
    Procedure SetstreamingComputationTask(AIndex : Integer; AValue : TStreamingComputationTask); virtual;
    Procedure SetstreamingSetupTask(AIndex : Integer; AValue : TStreamingSetupTask); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property configuration : String Index 0 Read Fconfiguration Write Setconfiguration;
    Property id : String Index 8 Read Fid Write Setid;
    Property initialReportIndex : String Index 16 Read FinitialReportIndex Write SetinitialReportIndex;
    Property jobId : String Index 24 Read FjobId Write SetjobId;
    Property leaseExpireTime : String Index 32 Read FleaseExpireTime Write SetleaseExpireTime;
    Property mapTask : TMapTask Index 40 Read FmapTask Write SetmapTask;
    Property packages : TWorkItemTypepackagesArray Index 48 Read Fpackages Write Setpackages;
    Property projectId : String Index 56 Read FprojectId Write SetprojectId;
    Property reportStatusInterval : String Index 64 Read FreportStatusInterval Write SetreportStatusInterval;
    Property seqMapTask : TSeqMapTask Index 72 Read FseqMapTask Write SetseqMapTask;
    Property shellTask : TShellTask Index 80 Read FshellTask Write SetshellTask;
    Property sourceOperationTask : TSourceOperationRequest Index 88 Read FsourceOperationTask Write SetsourceOperationTask;
    Property streamingComputationTask : TStreamingComputationTask Index 96 Read FstreamingComputationTask Write SetstreamingComputationTask;
    Property streamingSetupTask : TStreamingSetupTask Index 104 Read FstreamingSetupTask Write SetstreamingSetupTask;
  end;
  TWorkItemClass = Class of TWorkItem;
  
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
    FharnessData : TWorkItemServiceStateTypeharnessData;
    FleaseExpireTime : String;
    FnextReportIndex : String;
    FreportStatusInterval : String;
    FsuggestedStopPoint : TApproximateProgress;
    FsuggestedStopPosition : TPosition;
  Protected
    //Property setters
    Procedure SetharnessData(AIndex : Integer; AValue : TWorkItemServiceStateTypeharnessData); virtual;
    Procedure SetleaseExpireTime(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextReportIndex(AIndex : Integer; AValue : String); virtual;
    Procedure SetreportStatusInterval(AIndex : Integer; AValue : String); virtual;
    Procedure SetsuggestedStopPoint(AIndex : Integer; AValue : TApproximateProgress); virtual;
    Procedure SetsuggestedStopPosition(AIndex : Integer; AValue : TPosition); virtual;
  Public
  Published
    Property harnessData : TWorkItemServiceStateTypeharnessData Index 0 Read FharnessData Write SetharnessData;
    Property leaseExpireTime : String Index 8 Read FleaseExpireTime Write SetleaseExpireTime;
    Property nextReportIndex : String Index 16 Read FnextReportIndex Write SetnextReportIndex;
    Property reportStatusInterval : String Index 24 Read FreportStatusInterval Write SetreportStatusInterval;
    Property suggestedStopPoint : TApproximateProgress Index 32 Read FsuggestedStopPoint Write SetsuggestedStopPoint;
    Property suggestedStopPosition : TPosition Index 40 Read FsuggestedStopPosition Write SetsuggestedStopPosition;
  end;
  TWorkItemServiceStateClass = Class of TWorkItemServiceState;
  
  { --------------------------------------------------------------------
    TWorkItemStatus
    --------------------------------------------------------------------}
  
  TWorkItemStatus = Class(TGoogleBaseObject)
  Private
    Fcompleted : boolean;
    FdynamicSourceSplit : TDynamicSourceSplit;
    Ferrors : TWorkItemStatusTypeerrorsArray;
    FmetricUpdates : TWorkItemStatusTypemetricUpdatesArray;
    Fprogress : TApproximateProgress;
    FreportIndex : String;
    FrequestedLeaseDuration : String;
    FsourceFork : TSourceFork;
    FsourceOperationResponse : TSourceOperationResponse;
    FstopPosition : TPosition;
    FworkItemId : String;
  Protected
    //Property setters
    Procedure Setcompleted(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdynamicSourceSplit(AIndex : Integer; AValue : TDynamicSourceSplit); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TWorkItemStatusTypeerrorsArray); virtual;
    Procedure SetmetricUpdates(AIndex : Integer; AValue : TWorkItemStatusTypemetricUpdatesArray); virtual;
    Procedure Setprogress(AIndex : Integer; AValue : TApproximateProgress); virtual;
    Procedure SetreportIndex(AIndex : Integer; AValue : String); virtual;
    Procedure SetrequestedLeaseDuration(AIndex : Integer; AValue : String); virtual;
    Procedure SetsourceFork(AIndex : Integer; AValue : TSourceFork); virtual;
    Procedure SetsourceOperationResponse(AIndex : Integer; AValue : TSourceOperationResponse); virtual;
    Procedure SetstopPosition(AIndex : Integer; AValue : TPosition); virtual;
    Procedure SetworkItemId(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property completed : boolean Index 0 Read Fcompleted Write Setcompleted;
    Property dynamicSourceSplit : TDynamicSourceSplit Index 8 Read FdynamicSourceSplit Write SetdynamicSourceSplit;
    Property errors : TWorkItemStatusTypeerrorsArray Index 16 Read Ferrors Write Seterrors;
    Property metricUpdates : TWorkItemStatusTypemetricUpdatesArray Index 24 Read FmetricUpdates Write SetmetricUpdates;
    Property progress : TApproximateProgress Index 32 Read Fprogress Write Setprogress;
    Property reportIndex : String Index 40 Read FreportIndex Write SetreportIndex;
    Property requestedLeaseDuration : String Index 48 Read FrequestedLeaseDuration Write SetrequestedLeaseDuration;
    Property sourceFork : TSourceFork Index 56 Read FsourceFork Write SetsourceFork;
    Property sourceOperationResponse : TSourceOperationResponse Index 64 Read FsourceOperationResponse Write SetsourceOperationResponse;
    Property stopPosition : TPosition Index 72 Read FstopPosition Write SetstopPosition;
    Property workItemId : String Index 80 Read FworkItemId Write SetworkItemId;
  end;
  TWorkItemStatusClass = Class of TWorkItemStatus;
  
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
    FautoscalingSettings : TAutoscalingSettings;
    FdataDisks : TWorkerPoolTypedataDisksArray;
    FdefaultPackageSet : String;
    FdiskSizeGb : integer;
    FdiskSourceImage : String;
    FdiskType : String;
    Fkind : String;
    FmachineType : String;
    Fmetadata : TWorkerPoolTypemetadata;
    FnumWorkers : integer;
    FonHostMaintenance : String;
    Fpackages : TWorkerPoolTypepackagesArray;
    FpoolArgs : TWorkerPoolTypepoolArgs;
    FtaskrunnerSettings : TTaskRunnerSettings;
    FteardownPolicy : String;
    Fzone : String;
  Protected
    //Property setters
    Procedure SetautoscalingSettings(AIndex : Integer; AValue : TAutoscalingSettings); virtual;
    Procedure SetdataDisks(AIndex : Integer; AValue : TWorkerPoolTypedataDisksArray); virtual;
    Procedure SetdefaultPackageSet(AIndex : Integer; AValue : String); virtual;
    Procedure SetdiskSizeGb(AIndex : Integer; AValue : integer); virtual;
    Procedure SetdiskSourceImage(AIndex : Integer; AValue : String); virtual;
    Procedure SetdiskType(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmachineType(AIndex : Integer; AValue : String); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TWorkerPoolTypemetadata); virtual;
    Procedure SetnumWorkers(AIndex : Integer; AValue : integer); virtual;
    Procedure SetonHostMaintenance(AIndex : Integer; AValue : String); virtual;
    Procedure Setpackages(AIndex : Integer; AValue : TWorkerPoolTypepackagesArray); virtual;
    Procedure SetpoolArgs(AIndex : Integer; AValue : TWorkerPoolTypepoolArgs); virtual;
    Procedure SettaskrunnerSettings(AIndex : Integer; AValue : TTaskRunnerSettings); virtual;
    Procedure SetteardownPolicy(AIndex : Integer; AValue : String); virtual;
    Procedure Setzone(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property autoscalingSettings : TAutoscalingSettings Index 0 Read FautoscalingSettings Write SetautoscalingSettings;
    Property dataDisks : TWorkerPoolTypedataDisksArray Index 8 Read FdataDisks Write SetdataDisks;
    Property defaultPackageSet : String Index 16 Read FdefaultPackageSet Write SetdefaultPackageSet;
    Property diskSizeGb : integer Index 24 Read FdiskSizeGb Write SetdiskSizeGb;
    Property diskSourceImage : String Index 32 Read FdiskSourceImage Write SetdiskSourceImage;
    Property diskType : String Index 40 Read FdiskType Write SetdiskType;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property machineType : String Index 56 Read FmachineType Write SetmachineType;
    Property metadata : TWorkerPoolTypemetadata Index 64 Read Fmetadata Write Setmetadata;
    Property numWorkers : integer Index 72 Read FnumWorkers Write SetnumWorkers;
    Property onHostMaintenance : String Index 80 Read FonHostMaintenance Write SetonHostMaintenance;
    Property packages : TWorkerPoolTypepackagesArray Index 88 Read Fpackages Write Setpackages;
    Property poolArgs : TWorkerPoolTypepoolArgs Index 96 Read FpoolArgs Write SetpoolArgs;
    Property taskrunnerSettings : TTaskRunnerSettings Index 104 Read FtaskrunnerSettings Write SettaskrunnerSettings;
    Property teardownPolicy : String Index 112 Read FteardownPolicy Write SetteardownPolicy;
    Property zone : String Index 120 Read Fzone Write Setzone;
  end;
  TWorkerPoolClass = Class of TWorkerPool;
  
  { --------------------------------------------------------------------
    TWorkerSettings
    --------------------------------------------------------------------}
  
  TWorkerSettings = Class(TGoogleBaseObject)
  Private
    FbaseUrl : String;
    FreportingEnabled : boolean;
    FservicePath : String;
    FshuffleServicePath : String;
    FtempStoragePrefix : String;
    FworkerId : String;
  Protected
    //Property setters
    Procedure SetbaseUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetreportingEnabled(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetservicePath(AIndex : Integer; AValue : String); virtual;
    Procedure SetshuffleServicePath(AIndex : Integer; AValue : String); virtual;
    Procedure SettempStoragePrefix(AIndex : Integer; AValue : String); virtual;
    Procedure SetworkerId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property baseUrl : String Index 0 Read FbaseUrl Write SetbaseUrl;
    Property reportingEnabled : boolean Index 8 Read FreportingEnabled Write SetreportingEnabled;
    Property servicePath : String Index 16 Read FservicePath Write SetservicePath;
    Property shuffleServicePath : String Index 24 Read FshuffleServicePath Write SetshuffleServicePath;
    Property tempStoragePrefix : String Index 32 Read FtempStoragePrefix Write SettempStoragePrefix;
    Property workerId : String Index 40 Read FworkerId Write SetworkerId;
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
    TProjectsJobsMessagesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsJobsMessagesResource, method List
  
  TProjectsJobsMessagesListOptions = Record
    endTime : String;
    minimumImportance : String;
    pageSize : integer;
    pageToken : String;
    startTime : String;
  end;
  
  TProjectsJobsMessagesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(jobId: string; projectId: string; AQuery : string  = '') : TListJobMessagesResponse;
    Function List(jobId: string; projectId: string; AQuery : TProjectsJobsMessageslistOptions) : TListJobMessagesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsJobsWorkItemsResource
    --------------------------------------------------------------------}
  
  TProjectsJobsWorkItemsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Lease(jobId: string; projectId: string; aLeaseWorkItemRequest : TLeaseWorkItemRequest) : TLeaseWorkItemResponse;
    Function ReportStatus(jobId: string; projectId: string; aReportWorkItemStatusRequest : TReportWorkItemStatusRequest) : TReportWorkItemStatusResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsJobsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TProjectsJobsResource, method Create
  
  TProjectsJobsCreateOptions = Record
    replaceJobId : String;
    view : String;
  end;
  
  
  //Optional query Options for TProjectsJobsResource, method Get
  
  TProjectsJobsGetOptions = Record
    view : String;
  end;
  
  
  //Optional query Options for TProjectsJobsResource, method GetMetrics
  
  TProjectsJobsGetMetricsOptions = Record
    startTime : String;
  end;
  
  
  //Optional query Options for TProjectsJobsResource, method List
  
  TProjectsJobsListOptions = Record
    pageSize : integer;
    pageToken : String;
    view : String;
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
    Function Get(jobId: string; projectId: string; AQuery : string  = '') : TJob;
    Function Get(jobId: string; projectId: string; AQuery : TProjectsJobsgetOptions) : TJob;
    Function GetMetrics(jobId: string; projectId: string; AQuery : string  = '') : TJobMetrics;
    Function GetMetrics(jobId: string; projectId: string; AQuery : TProjectsJobsgetMetricsOptions) : TJobMetrics;
    Function List(projectId: string; AQuery : string  = '') : TListJobsResponse;
    Function List(projectId: string; AQuery : TProjectsJobslistOptions) : TListJobsResponse;
    Function Patch(jobId: string; projectId: string; aJob : TJob) : TJob;
    Function Update(jobId: string; projectId: string; aJob : TJob) : TJob;
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



Procedure TApproximateProgress.SetremainingTime(AIndex : Integer; AValue : String); 

begin
  If (FremainingTime=AValue) then exit;
  FremainingTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAutoscalingSettings
  --------------------------------------------------------------------}


Procedure TAutoscalingSettings.Setalgorithm(AIndex : Integer; AValue : String); 

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


Procedure TComputationTopology.SetcomputationId(AIndex : Integer; AValue : String); 

begin
  If (FcomputationId=AValue) then exit;
  FcomputationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComputationTopology.Setinputs(AIndex : Integer; AValue : TComputationTopologyTypeinputsArray); 

begin
  If (Finputs=AValue) then exit;
  Finputs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComputationTopology.SetkeyRanges(AIndex : Integer; AValue : TComputationTopologyTypekeyRangesArray); 

begin
  If (FkeyRanges=AValue) then exit;
  FkeyRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TComputationTopology.Setoutputs(AIndex : Integer; AValue : TComputationTopologyTypeoutputsArray); 

begin
  If (Foutputs=AValue) then exit;
  Foutputs:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TComputationTopology.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'inputs' : SetLength(Finputs,ALength);
  'keyranges' : SetLength(FkeyRanges,ALength);
  'outputs' : SetLength(Foutputs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDataDiskAssignment
  --------------------------------------------------------------------}


Procedure TDataDiskAssignment.SetdataDisks(AIndex : Integer; AValue : TStringArray); 

begin
  If (FdataDisks=AValue) then exit;
  FdataDisks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataDiskAssignment.SetvmInstance(AIndex : Integer; AValue : String); 

begin
  If (FvmInstance=AValue) then exit;
  FvmInstance:=AValue;
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
  TDerivedSource
  --------------------------------------------------------------------}


Procedure TDerivedSource.SetderivationMode(AIndex : Integer; AValue : String); 

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


Procedure TDisk.SetdiskType(AIndex : Integer; AValue : String); 

begin
  If (FdiskType=AValue) then exit;
  FdiskType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDisk.SetmountPoint(AIndex : Integer; AValue : String); 

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
  TEnvironmentTypesdkPipelineOptions
  --------------------------------------------------------------------}


Class Function TEnvironmentTypesdkPipelineOptions.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



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
  TEnvironment
  --------------------------------------------------------------------}


Procedure TEnvironment.SetclusterManagerApiService(AIndex : Integer; AValue : String); 

begin
  If (FclusterManagerApiService=AValue) then exit;
  FclusterManagerApiService:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.Setdataset(AIndex : Integer; AValue : String); 

begin
  If (Fdataset=AValue) then exit;
  Fdataset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.Setexperiments(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fexperiments=AValue) then exit;
  Fexperiments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetsdkPipelineOptions(AIndex : Integer; AValue : TEnvironmentTypesdkPipelineOptions); 

begin
  If (FsdkPipelineOptions=AValue) then exit;
  FsdkPipelineOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SettempStoragePrefix(AIndex : Integer; AValue : String); 

begin
  If (FtempStoragePrefix=AValue) then exit;
  FtempStoragePrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetuserAgent(AIndex : Integer; AValue : TEnvironmentTypeuserAgent); 

begin
  If (FuserAgent=AValue) then exit;
  FuserAgent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.Setversion(AIndex : Integer; AValue : TEnvironmentTypeversion); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvironment.SetworkerPools(AIndex : Integer; AValue : TEnvironmentTypeworkerPoolsArray); 

begin
  If (FworkerPools=AValue) then exit;
  FworkerPools:=AValue;
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
  TFlattenInstruction
  --------------------------------------------------------------------}


Procedure TFlattenInstruction.Setinputs(AIndex : Integer; AValue : TFlattenInstructionTypeinputsArray); 

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
  TInstructionOutputTypecodec
  --------------------------------------------------------------------}


Class Function TInstructionOutputTypecodec.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TInstructionOutput
  --------------------------------------------------------------------}


Procedure TInstructionOutput.Setcodec(AIndex : Integer; AValue : TInstructionOutputTypecodec); 

begin
  If (Fcodec=AValue) then exit;
  Fcodec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstructionOutput.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJob
  --------------------------------------------------------------------}


Procedure TJob.SetcreateTime(AIndex : Integer; AValue : String); 

begin
  If (FcreateTime=AValue) then exit;
  FcreateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetcurrentState(AIndex : Integer; AValue : String); 

begin
  If (FcurrentState=AValue) then exit;
  FcurrentState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetcurrentStateTime(AIndex : Integer; AValue : String); 

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



Procedure TJob.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetprojectId(AIndex : Integer; AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetrequestedState(AIndex : Integer; AValue : String); 

begin
  If (FrequestedState=AValue) then exit;
  FrequestedState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setsteps(AIndex : Integer; AValue : TJobTypestepsArray); 

begin
  If (Fsteps=AValue) then exit;
  Fsteps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Set_type(AIndex : Integer; AValue : String); 

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

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TJob.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'steps' : SetLength(Fsteps,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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


Procedure TJobExecutionInfo.Setstages(AIndex : Integer; AValue : TJobExecutionInfoTypestages); 

begin
  If (Fstages=AValue) then exit;
  Fstages:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobExecutionStageInfo
  --------------------------------------------------------------------}


Procedure TJobExecutionStageInfo.SetstepName(AIndex : Integer; AValue : TStringArray); 

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
  TJobMessage
  --------------------------------------------------------------------}


Procedure TJobMessage.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobMessage.SetmessageImportance(AIndex : Integer; AValue : String); 

begin
  If (FmessageImportance=AValue) then exit;
  FmessageImportance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobMessage.SetmessageText(AIndex : Integer; AValue : String); 

begin
  If (FmessageText=AValue) then exit;
  FmessageText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobMessage.Settime(AIndex : Integer; AValue : String); 

begin
  If (Ftime=AValue) then exit;
  Ftime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJobMetrics
  --------------------------------------------------------------------}


Procedure TJobMetrics.SetmetricTime(AIndex : Integer; AValue : String); 

begin
  If (FmetricTime=AValue) then exit;
  FmetricTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobMetrics.Setmetrics(AIndex : Integer; AValue : TJobMetricsTypemetricsArray); 

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
  TKeyRangeDataDiskAssignment
  --------------------------------------------------------------------}


Procedure TKeyRangeDataDiskAssignment.SetdataDisk(AIndex : Integer; AValue : String); 

begin
  If (FdataDisk=AValue) then exit;
  FdataDisk:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeDataDiskAssignment.Set_end(AIndex : Integer; AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeDataDiskAssignment.Setstart(AIndex : Integer; AValue : String); 

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


Procedure TKeyRangeLocation.SetdataDisk(AIndex : Integer; AValue : String); 

begin
  If (FdataDisk=AValue) then exit;
  FdataDisk:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeLocation.SetdeliveryEndpoint(AIndex : Integer; AValue : String); 

begin
  If (FdeliveryEndpoint=AValue) then exit;
  FdeliveryEndpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeLocation.Set_end(AIndex : Integer; AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeLocation.SetpersistentDirectory(AIndex : Integer; AValue : String); 

begin
  If (FpersistentDirectory=AValue) then exit;
  FpersistentDirectory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyRangeLocation.Setstart(AIndex : Integer; AValue : String); 

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


Procedure TLeaseWorkItemRequest.SetcurrentWorkerTime(AIndex : Integer; AValue : String); 

begin
  If (FcurrentWorkerTime=AValue) then exit;
  FcurrentWorkerTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaseWorkItemRequest.SetrequestedLeaseDuration(AIndex : Integer; AValue : String); 

begin
  If (FrequestedLeaseDuration=AValue) then exit;
  FrequestedLeaseDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaseWorkItemRequest.SetworkItemTypes(AIndex : Integer; AValue : TStringArray); 

begin
  If (FworkItemTypes=AValue) then exit;
  FworkItemTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaseWorkItemRequest.SetworkerCapabilities(AIndex : Integer; AValue : TStringArray); 

begin
  If (FworkerCapabilities=AValue) then exit;
  FworkerCapabilities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaseWorkItemRequest.SetworkerId(AIndex : Integer; AValue : String); 

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


Procedure TLeaseWorkItemResponse.SetworkItems(AIndex : Integer; AValue : TLeaseWorkItemResponseTypeworkItemsArray); 

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
  TListJobMessagesResponse
  --------------------------------------------------------------------}


Procedure TListJobMessagesResponse.SetjobMessages(AIndex : Integer; AValue : TListJobMessagesResponseTypejobMessagesArray); 

begin
  If (FjobMessages=AValue) then exit;
  FjobMessages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListJobMessagesResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

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
  TListJobsResponse
  --------------------------------------------------------------------}


Procedure TListJobsResponse.Setjobs(AIndex : Integer; AValue : TListJobsResponseTypejobsArray); 

begin
  If (Fjobs=AValue) then exit;
  Fjobs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListJobsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

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
  TMapTask
  --------------------------------------------------------------------}


Procedure TMapTask.Setinstructions(AIndex : Integer; AValue : TMapTaskTypeinstructionsArray); 

begin
  If (Finstructions=AValue) then exit;
  Finstructions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapTask.SetstageName(AIndex : Integer; AValue : String); 

begin
  If (FstageName=AValue) then exit;
  FstageName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMapTask.SetsystemName(AIndex : Integer; AValue : String); 

begin
  If (FsystemName=AValue) then exit;
  FsystemName:=AValue;
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
  TMetricStructuredNameTypecontext
  --------------------------------------------------------------------}


Class Function TMetricStructuredNameTypecontext.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TMetricStructuredName
  --------------------------------------------------------------------}


Procedure TMetricStructuredName.Setcontext(AIndex : Integer; AValue : TMetricStructuredNameTypecontext); 

begin
  If (Fcontext=AValue) then exit;
  Fcontext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricStructuredName.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetricStructuredName.Setorigin(AIndex : Integer; AValue : String); 

begin
  If (Forigin=AValue) then exit;
  Forigin:=AValue;
  MarkPropertyChanged(AIndex);
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



Procedure TMetricUpdate.Setkind(AIndex : Integer; AValue : String); 

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



Procedure TMetricUpdate.SetupdateTime(AIndex : Integer; AValue : String); 

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


Procedure TMountedDataDisk.SetdataDisk(AIndex : Integer; AValue : String); 

begin
  If (FdataDisk=AValue) then exit;
  FdataDisk:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMultiOutputInfo
  --------------------------------------------------------------------}


Procedure TMultiOutputInfo.Settag(AIndex : Integer; AValue : String); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPackage
  --------------------------------------------------------------------}


Procedure TPackage.Setlocation(AIndex : Integer; AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPackage.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
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


Procedure TParDoInstruction.Setinput(AIndex : Integer; AValue : TInstructionInput); 

begin
  If (Finput=AValue) then exit;
  Finput:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParDoInstruction.SetmultiOutputInfos(AIndex : Integer; AValue : TParDoInstructionTypemultiOutputInfosArray); 

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



Procedure TParDoInstruction.SetsideInputs(AIndex : Integer; AValue : TParDoInstructionTypesideInputsArray); 

begin
  If (FsideInputs=AValue) then exit;
  FsideInputs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParDoInstruction.SetuserFn(AIndex : Integer; AValue : TParDoInstructionTypeuserFn); 

begin
  If (FuserFn=AValue) then exit;
  FuserFn:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TParDoInstruction.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'multioutputinfos' : SetLength(FmultiOutputInfos,ALength);
  'sideinputs' : SetLength(FsideInputs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TParallelInstruction
  --------------------------------------------------------------------}


Procedure TParallelInstruction.Setflatten(AIndex : Integer; AValue : TFlattenInstruction); 

begin
  If (Fflatten=AValue) then exit;
  Fflatten:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParallelInstruction.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParallelInstruction.Setoutputs(AIndex : Integer; AValue : TParallelInstructionTypeoutputsArray); 

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



Procedure TParallelInstruction.SetsystemName(AIndex : Integer; AValue : String); 

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


Procedure TPartialGroupByKeyInstruction.Setinput(AIndex : Integer; AValue : TInstructionInput); 

begin
  If (Finput=AValue) then exit;
  Finput:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPartialGroupByKeyInstruction.SetinputElementCodec(AIndex : Integer; AValue : TPartialGroupByKeyInstructionTypeinputElementCodec); 

begin
  If (FinputElementCodec=AValue) then exit;
  FinputElementCodec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPartialGroupByKeyInstruction.SetvalueCombiningFn(AIndex : Integer; AValue : TPartialGroupByKeyInstructionTypevalueCombiningFn); 

begin
  If (FvalueCombiningFn=AValue) then exit;
  FvalueCombiningFn:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPosition
  --------------------------------------------------------------------}


Procedure TPosition.SetbyteOffset(AIndex : Integer; AValue : String); 

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



Procedure TPosition.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.SetrecordIndex(AIndex : Integer; AValue : String); 

begin
  If (FrecordIndex=AValue) then exit;
  FrecordIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.SetshufflePosition(AIndex : Integer; AValue : String); 

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



Procedure TPubsubLocation.SetidLabel(AIndex : Integer; AValue : String); 

begin
  If (FidLabel=AValue) then exit;
  FidLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubLocation.Setsubscription(AIndex : Integer; AValue : String); 

begin
  If (Fsubscription=AValue) then exit;
  Fsubscription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubLocation.SettimestampLabel(AIndex : Integer; AValue : String); 

begin
  If (FtimestampLabel=AValue) then exit;
  FtimestampLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubLocation.Settopic(AIndex : Integer; AValue : String); 

begin
  If (Ftopic=AValue) then exit;
  Ftopic:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPubsubLocation.SettrackingSubscription(AIndex : Integer; AValue : String); 

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


Procedure TReportWorkItemStatusRequest.SetcurrentWorkerTime(AIndex : Integer; AValue : String); 

begin
  If (FcurrentWorkerTime=AValue) then exit;
  FcurrentWorkerTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportWorkItemStatusRequest.SetworkItemStatuses(AIndex : Integer; AValue : TReportWorkItemStatusRequestTypeworkItemStatusesArray); 

begin
  If (FworkItemStatuses=AValue) then exit;
  FworkItemStatuses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportWorkItemStatusRequest.SetworkerId(AIndex : Integer; AValue : String); 

begin
  If (FworkerId=AValue) then exit;
  FworkerId:=AValue;
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
  TReportWorkItemStatusResponse
  --------------------------------------------------------------------}


Procedure TReportWorkItemStatusResponse.SetworkItemServiceStates(AIndex : Integer; AValue : TReportWorkItemStatusResponseTypeworkItemServiceStatesArray); 

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
  TSeqMapTaskTypeuserFn
  --------------------------------------------------------------------}


Class Function TSeqMapTaskTypeuserFn.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSeqMapTask
  --------------------------------------------------------------------}


Procedure TSeqMapTask.Setinputs(AIndex : Integer; AValue : TSeqMapTaskTypeinputsArray); 

begin
  If (Finputs=AValue) then exit;
  Finputs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTask.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTask.SetoutputInfos(AIndex : Integer; AValue : TSeqMapTaskTypeoutputInfosArray); 

begin
  If (FoutputInfos=AValue) then exit;
  FoutputInfos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTask.SetstageName(AIndex : Integer; AValue : String); 

begin
  If (FstageName=AValue) then exit;
  FstageName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTask.SetsystemName(AIndex : Integer; AValue : String); 

begin
  If (FsystemName=AValue) then exit;
  FsystemName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTask.SetuserFn(AIndex : Integer; AValue : TSeqMapTaskTypeuserFn); 

begin
  If (FuserFn=AValue) then exit;
  FuserFn:=AValue;
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


Procedure TSeqMapTaskOutputInfo.Setsink(AIndex : Integer; AValue : TSink); 

begin
  If (Fsink=AValue) then exit;
  Fsink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSeqMapTaskOutputInfo.Settag(AIndex : Integer; AValue : String); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TShellTask
  --------------------------------------------------------------------}


Procedure TShellTask.Setcommand(AIndex : Integer; AValue : String); 

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
  TSideInputInfoTypekind
  --------------------------------------------------------------------}


Class Function TSideInputInfoTypekind.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSideInputInfo
  --------------------------------------------------------------------}


Procedure TSideInputInfo.Setkind(AIndex : Integer; AValue : TSideInputInfoTypekind); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSideInputInfo.Setsources(AIndex : Integer; AValue : TSideInputInfoTypesourcesArray); 

begin
  If (Fsources=AValue) then exit;
  Fsources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSideInputInfo.Settag(AIndex : Integer; AValue : String); 

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
  TSinkTypecodec
  --------------------------------------------------------------------}


Class Function TSinkTypecodec.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSinkTypespec
  --------------------------------------------------------------------}


Class Function TSinkTypespec.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSink
  --------------------------------------------------------------------}


Procedure TSink.Setcodec(AIndex : Integer; AValue : TSinkTypecodec); 

begin
  If (Fcodec=AValue) then exit;
  Fcodec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSink.Setspec(AIndex : Integer; AValue : TSinkTypespec); 

begin
  If (Fspec=AValue) then exit;
  Fspec:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSourceTypebaseSpecsItem
  --------------------------------------------------------------------}


Class Function TSourceTypebaseSpecsItem.AllowAdditionalProperties : Boolean;

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
  TSourceTypespec
  --------------------------------------------------------------------}


Class Function TSourceTypespec.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSource
  --------------------------------------------------------------------}


Procedure TSource.SetbaseSpecs(AIndex : Integer; AValue : TSourceTypebaseSpecsArray); 

begin
  If (FbaseSpecs=AValue) then exit;
  FbaseSpecs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSource.Setcodec(AIndex : Integer; AValue : TSourceTypecodec); 

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



Procedure TSource.Setspec(AIndex : Integer; AValue : TSourceTypespec); 

begin
  If (Fspec=AValue) then exit;
  Fspec:=AValue;
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


Procedure TSourceMetadata.SetestimatedSizeBytes(AIndex : Integer; AValue : String); 

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


Procedure TSourceSplitOptions.SetdesiredBundleSizeBytes(AIndex : Integer; AValue : String); 

begin
  If (FdesiredBundleSizeBytes=AValue) then exit;
  FdesiredBundleSizeBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceSplitOptions.SetdesiredShardSizeBytes(AIndex : Integer; AValue : String); 

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


Procedure TSourceSplitResponse.Setbundles(AIndex : Integer; AValue : TSourceSplitResponseTypebundlesArray); 

begin
  If (Fbundles=AValue) then exit;
  Fbundles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceSplitResponse.Setoutcome(AIndex : Integer; AValue : String); 

begin
  If (Foutcome=AValue) then exit;
  Foutcome:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSourceSplitResponse.Setshards(AIndex : Integer; AValue : TSourceSplitResponseTypeshardsArray); 

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


Procedure TSourceSplitShard.SetderivationMode(AIndex : Integer; AValue : String); 

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
  TStatusTypedetailsItem
  --------------------------------------------------------------------}


Class Function TStatusTypedetailsItem.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
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



Procedure TStatus.Setdetails(AIndex : Integer; AValue : TStatusTypedetailsArray); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
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
  TStepTypeproperties
  --------------------------------------------------------------------}


Class Function TStepTypeproperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TStep
  --------------------------------------------------------------------}


Procedure TStep.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStep.Setproperties(AIndex : Integer; AValue : TStepTypeproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
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


Procedure TStreamingComputationRanges.SetcomputationId(AIndex : Integer; AValue : String); 

begin
  If (FcomputationId=AValue) then exit;
  FcomputationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingComputationRanges.SetrangeAssignments(AIndex : Integer; AValue : TStreamingComputationRangesTyperangeAssignmentsArray); 

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
  TStreamingComputationTask
  --------------------------------------------------------------------}


Procedure TStreamingComputationTask.SetcomputationRanges(AIndex : Integer; AValue : TStreamingComputationTaskTypecomputationRangesArray); 

begin
  If (FcomputationRanges=AValue) then exit;
  FcomputationRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingComputationTask.SetdataDisks(AIndex : Integer; AValue : TStreamingComputationTaskTypedataDisksArray); 

begin
  If (FdataDisks=AValue) then exit;
  FdataDisks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamingComputationTask.SettaskType(AIndex : Integer; AValue : String); 

begin
  If (FtaskType=AValue) then exit;
  FtaskType:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TStreamingComputationTask.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'computationranges' : SetLength(FcomputationRanges,ALength);
  'datadisks' : SetLength(FdataDisks,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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


Procedure TStreamingSideInputLocation.Settag(AIndex : Integer; AValue : String); 

begin
  If (Ftag=AValue) then exit;
  Ftag:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStreamingStageLocation
  --------------------------------------------------------------------}


Procedure TStreamingStageLocation.SetstreamId(AIndex : Integer; AValue : String); 

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



Procedure TTaskRunnerSettings.SetbaseTaskDir(AIndex : Integer; AValue : String); 

begin
  If (FbaseTaskDir=AValue) then exit;
  FbaseTaskDir:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetbaseUrl(AIndex : Integer; AValue : String); 

begin
  If (FbaseUrl=AValue) then exit;
  FbaseUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetcommandlinesFileName(AIndex : Integer; AValue : String); 

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



Procedure TTaskRunnerSettings.SetdataflowApiVersion(AIndex : Integer; AValue : String); 

begin
  If (FdataflowApiVersion=AValue) then exit;
  FdataflowApiVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetharnessCommand(AIndex : Integer; AValue : String); 

begin
  If (FharnessCommand=AValue) then exit;
  FharnessCommand:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetlanguageHint(AIndex : Integer; AValue : String); 

begin
  If (FlanguageHint=AValue) then exit;
  FlanguageHint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetlogDir(AIndex : Integer; AValue : String); 

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



Procedure TTaskRunnerSettings.SetlogUploadLocation(AIndex : Integer; AValue : String); 

begin
  If (FlogUploadLocation=AValue) then exit;
  FlogUploadLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetoauthScopes(AIndex : Integer; AValue : TStringArray); 

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



Procedure TTaskRunnerSettings.SetstreamingWorkerMainClass(AIndex : Integer; AValue : String); 

begin
  If (FstreamingWorkerMainClass=AValue) then exit;
  FstreamingWorkerMainClass:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SettaskGroup(AIndex : Integer; AValue : String); 

begin
  If (FtaskGroup=AValue) then exit;
  FtaskGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SettaskUser(AIndex : Integer; AValue : String); 

begin
  If (FtaskUser=AValue) then exit;
  FtaskUser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SettempStoragePrefix(AIndex : Integer; AValue : String); 

begin
  If (FtempStoragePrefix=AValue) then exit;
  FtempStoragePrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetvmId(AIndex : Integer; AValue : String); 

begin
  If (FvmId=AValue) then exit;
  FvmId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaskRunnerSettings.SetworkflowFileName(AIndex : Integer; AValue : String); 

begin
  If (FworkflowFileName=AValue) then exit;
  FworkflowFileName:=AValue;
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
  TTopologyConfig
  --------------------------------------------------------------------}


Procedure TTopologyConfig.Setcomputations(AIndex : Integer; AValue : TTopologyConfigTypecomputationsArray); 

begin
  If (Fcomputations=AValue) then exit;
  Fcomputations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTopologyConfig.SetdataDiskAssignments(AIndex : Integer; AValue : TTopologyConfigTypedataDiskAssignmentsArray); 

begin
  If (FdataDiskAssignments=AValue) then exit;
  FdataDiskAssignments:=AValue;
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
  TWorkItem
  --------------------------------------------------------------------}


Procedure TWorkItem.Setconfiguration(AIndex : Integer; AValue : String); 

begin
  If (Fconfiguration=AValue) then exit;
  Fconfiguration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetinitialReportIndex(AIndex : Integer; AValue : String); 

begin
  If (FinitialReportIndex=AValue) then exit;
  FinitialReportIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetjobId(AIndex : Integer; AValue : String); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetleaseExpireTime(AIndex : Integer; AValue : String); 

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



Procedure TWorkItem.Setpackages(AIndex : Integer; AValue : TWorkItemTypepackagesArray); 

begin
  If (Fpackages=AValue) then exit;
  Fpackages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetprojectId(AIndex : Integer; AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItem.SetreportStatusInterval(AIndex : Integer; AValue : String); 

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
  TWorkItemServiceStateTypeharnessData
  --------------------------------------------------------------------}


Class Function TWorkItemServiceStateTypeharnessData.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWorkItemServiceState
  --------------------------------------------------------------------}


Procedure TWorkItemServiceState.SetharnessData(AIndex : Integer; AValue : TWorkItemServiceStateTypeharnessData); 

begin
  If (FharnessData=AValue) then exit;
  FharnessData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemServiceState.SetleaseExpireTime(AIndex : Integer; AValue : String); 

begin
  If (FleaseExpireTime=AValue) then exit;
  FleaseExpireTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemServiceState.SetnextReportIndex(AIndex : Integer; AValue : String); 

begin
  If (FnextReportIndex=AValue) then exit;
  FnextReportIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemServiceState.SetreportStatusInterval(AIndex : Integer; AValue : String); 

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



Procedure TWorkItemStatus.Seterrors(AIndex : Integer; AValue : TWorkItemStatusTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetmetricUpdates(AIndex : Integer; AValue : TWorkItemStatusTypemetricUpdatesArray); 

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



Procedure TWorkItemStatus.SetreportIndex(AIndex : Integer; AValue : String); 

begin
  If (FreportIndex=AValue) then exit;
  FreportIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkItemStatus.SetrequestedLeaseDuration(AIndex : Integer; AValue : String); 

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



Procedure TWorkItemStatus.SetworkItemId(AIndex : Integer; AValue : String); 

begin
  If (FworkItemId=AValue) then exit;
  FworkItemId:=AValue;
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


Procedure TWorkerPool.SetautoscalingSettings(AIndex : Integer; AValue : TAutoscalingSettings); 

begin
  If (FautoscalingSettings=AValue) then exit;
  FautoscalingSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetdataDisks(AIndex : Integer; AValue : TWorkerPoolTypedataDisksArray); 

begin
  If (FdataDisks=AValue) then exit;
  FdataDisks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetdefaultPackageSet(AIndex : Integer; AValue : String); 

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



Procedure TWorkerPool.SetdiskSourceImage(AIndex : Integer; AValue : String); 

begin
  If (FdiskSourceImage=AValue) then exit;
  FdiskSourceImage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetdiskType(AIndex : Integer; AValue : String); 

begin
  If (FdiskType=AValue) then exit;
  FdiskType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetmachineType(AIndex : Integer; AValue : String); 

begin
  If (FmachineType=AValue) then exit;
  FmachineType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.Setmetadata(AIndex : Integer; AValue : TWorkerPoolTypemetadata); 

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



Procedure TWorkerPool.SetonHostMaintenance(AIndex : Integer; AValue : String); 

begin
  If (FonHostMaintenance=AValue) then exit;
  FonHostMaintenance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.Setpackages(AIndex : Integer; AValue : TWorkerPoolTypepackagesArray); 

begin
  If (Fpackages=AValue) then exit;
  Fpackages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.SetpoolArgs(AIndex : Integer; AValue : TWorkerPoolTypepoolArgs); 

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



Procedure TWorkerPool.SetteardownPolicy(AIndex : Integer; AValue : String); 

begin
  If (FteardownPolicy=AValue) then exit;
  FteardownPolicy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerPool.Setzone(AIndex : Integer; AValue : String); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TWorkerPool.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'datadisks' : SetLength(FdataDisks,ALength);
  'packages' : SetLength(Fpackages,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWorkerSettings
  --------------------------------------------------------------------}


Procedure TWorkerSettings.SetbaseUrl(AIndex : Integer; AValue : String); 

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



Procedure TWorkerSettings.SetservicePath(AIndex : Integer; AValue : String); 

begin
  If (FservicePath=AValue) then exit;
  FservicePath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerSettings.SetshuffleServicePath(AIndex : Integer; AValue : String); 

begin
  If (FshuffleServicePath=AValue) then exit;
  FshuffleServicePath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerSettings.SettempStoragePrefix(AIndex : Integer; AValue : String); 

begin
  If (FtempStoragePrefix=AValue) then exit;
  FtempStoragePrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWorkerSettings.SetworkerId(AIndex : Integer; AValue : String); 

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

Function TProjectsJobsMessagesResource.List(jobId: string; projectId: string; AQuery : string = '') : TListJobMessagesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/jobs/{jobId}/messages';
  _Methodid   = 'dataflow.projects.jobs.messages.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListJobMessagesResponse) as TListJobMessagesResponse;
end;


Function TProjectsJobsMessagesResource.List(jobId: string; projectId: string; AQuery : TProjectsJobsMessageslistOptions) : TListJobMessagesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'endTime',AQuery.endTime);
  AddToQuery(_Q,'minimumImportance',AQuery.minimumImportance);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'startTime',AQuery.startTime);
  Result:=List(jobId,projectId,_Q);
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

Function TProjectsJobsWorkItemsResource.Lease(jobId: string; projectId: string; aLeaseWorkItemRequest : TLeaseWorkItemRequest) : TLeaseWorkItemResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{projectId}/jobs/{jobId}/workItems:lease';
  _Methodid   = 'dataflow.projects.jobs.workItems.lease';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLeaseWorkItemRequest,TLeaseWorkItemResponse) as TLeaseWorkItemResponse;
end;

Function TProjectsJobsWorkItemsResource.ReportStatus(jobId: string; projectId: string; aReportWorkItemStatusRequest : TReportWorkItemStatusRequest) : TReportWorkItemStatusResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{projectId}/jobs/{jobId}/workItems:reportStatus';
  _Methodid   = 'dataflow.projects.jobs.workItems.reportStatus';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aReportWorkItemStatusRequest,TReportWorkItemStatusResponse) as TReportWorkItemStatusResponse;
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
  _Path       = '{projectId}/jobs';
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
  AddToQuery(_Q,'replaceJobId',AQuery.replaceJobId);
  AddToQuery(_Q,'view',AQuery.view);
  Result:=Create(projectId,aJob,_Q);
end;

Function TProjectsJobsResource.Get(jobId: string; projectId: string; AQuery : string = '') : TJob;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/jobs/{jobId}';
  _Methodid   = 'dataflow.projects.jobs.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TJob) as TJob;
end;


Function TProjectsJobsResource.Get(jobId: string; projectId: string; AQuery : TProjectsJobsgetOptions) : TJob;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'view',AQuery.view);
  Result:=Get(jobId,projectId,_Q);
end;

Function TProjectsJobsResource.GetMetrics(jobId: string; projectId: string; AQuery : string = '') : TJobMetrics;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/jobs/{jobId}/metrics';
  _Methodid   = 'dataflow.projects.jobs.getMetrics';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TJobMetrics) as TJobMetrics;
end;


Function TProjectsJobsResource.GetMetrics(jobId: string; projectId: string; AQuery : TProjectsJobsgetMetricsOptions) : TJobMetrics;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'startTime',AQuery.startTime);
  Result:=GetMetrics(jobId,projectId,_Q);
end;

Function TProjectsJobsResource.List(projectId: string; AQuery : string = '') : TListJobsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/jobs';
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
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'view',AQuery.view);
  Result:=List(projectId,_Q);
end;

Function TProjectsJobsResource.Patch(jobId: string; projectId: string; aJob : TJob) : TJob;

Const
  _HTTPMethod = 'PATCH';
  _Path       = '{projectId}/jobs/{jobId}';
  _Methodid   = 'dataflow.projects.jobs.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aJob,TJob) as TJob;
end;

Function TProjectsJobsResource.Update(jobId: string; projectId: string; aJob : TJob) : TJob;

Const
  _HTTPMethod = 'PUT';
  _Path       = '{projectId}/jobs/{jobId}';
  _Methodid   = 'dataflow.projects.jobs.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId,'projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aJob,TJob) as TJob;
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
  TDataDiskAssignment.RegisterObject;
  TDerivedSource.RegisterObject;
  TDisk.RegisterObject;
  TDynamicSourceSplit.RegisterObject;
  TEnvironmentTypesdkPipelineOptions.RegisterObject;
  TEnvironmentTypeuserAgent.RegisterObject;
  TEnvironmentTypeversion.RegisterObject;
  TEnvironment.RegisterObject;
  TFlattenInstruction.RegisterObject;
  TInstructionInput.RegisterObject;
  TInstructionOutputTypecodec.RegisterObject;
  TInstructionOutput.RegisterObject;
  TJob.RegisterObject;
  TJobExecutionInfoTypestages.RegisterObject;
  TJobExecutionInfo.RegisterObject;
  TJobExecutionStageInfo.RegisterObject;
  TJobMessage.RegisterObject;
  TJobMetrics.RegisterObject;
  TKeyRangeDataDiskAssignment.RegisterObject;
  TKeyRangeLocation.RegisterObject;
  TLeaseWorkItemRequest.RegisterObject;
  TLeaseWorkItemResponse.RegisterObject;
  TListJobMessagesResponse.RegisterObject;
  TListJobsResponse.RegisterObject;
  TMapTask.RegisterObject;
  TMetricStructuredNameTypecontext.RegisterObject;
  TMetricStructuredName.RegisterObject;
  TMetricUpdate.RegisterObject;
  TMountedDataDisk.RegisterObject;
  TMultiOutputInfo.RegisterObject;
  TPackage.RegisterObject;
  TParDoInstructionTypeuserFn.RegisterObject;
  TParDoInstruction.RegisterObject;
  TParallelInstruction.RegisterObject;
  TPartialGroupByKeyInstructionTypeinputElementCodec.RegisterObject;
  TPartialGroupByKeyInstructionTypevalueCombiningFn.RegisterObject;
  TPartialGroupByKeyInstruction.RegisterObject;
  TPosition.RegisterObject;
  TPubsubLocation.RegisterObject;
  TReadInstruction.RegisterObject;
  TReportWorkItemStatusRequest.RegisterObject;
  TReportWorkItemStatusResponse.RegisterObject;
  TSeqMapTaskTypeuserFn.RegisterObject;
  TSeqMapTask.RegisterObject;
  TSeqMapTaskOutputInfo.RegisterObject;
  TShellTask.RegisterObject;
  TSideInputInfoTypekind.RegisterObject;
  TSideInputInfo.RegisterObject;
  TSinkTypecodec.RegisterObject;
  TSinkTypespec.RegisterObject;
  TSink.RegisterObject;
  TSourceTypebaseSpecsItem.RegisterObject;
  TSourceTypecodec.RegisterObject;
  TSourceTypespec.RegisterObject;
  TSource.RegisterObject;
  TSourceFork.RegisterObject;
  TSourceGetMetadataRequest.RegisterObject;
  TSourceGetMetadataResponse.RegisterObject;
  TSourceMetadata.RegisterObject;
  TSourceOperationRequest.RegisterObject;
  TSourceOperationResponse.RegisterObject;
  TSourceSplitOptions.RegisterObject;
  TSourceSplitRequest.RegisterObject;
  TSourceSplitResponse.RegisterObject;
  TSourceSplitShard.RegisterObject;
  TStatusTypedetailsItem.RegisterObject;
  TStatus.RegisterObject;
  TStepTypeproperties.RegisterObject;
  TStep.RegisterObject;
  TStreamLocation.RegisterObject;
  TStreamingComputationRanges.RegisterObject;
  TStreamingComputationTask.RegisterObject;
  TStreamingSetupTask.RegisterObject;
  TStreamingSideInputLocation.RegisterObject;
  TStreamingStageLocation.RegisterObject;
  TTaskRunnerSettings.RegisterObject;
  TTopologyConfig.RegisterObject;
  TWorkItem.RegisterObject;
  TWorkItemServiceStateTypeharnessData.RegisterObject;
  TWorkItemServiceState.RegisterObject;
  TWorkItemStatus.RegisterObject;
  TWorkerPoolTypemetadata.RegisterObject;
  TWorkerPoolTypepoolArgs.RegisterObject;
  TWorkerPool.RegisterObject;
  TWorkerSettings.RegisterObject;
  TWriteInstruction.RegisterObject;
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
