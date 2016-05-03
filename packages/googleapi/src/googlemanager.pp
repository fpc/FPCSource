unit googlemanager;
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
//Generated on: 16-5-15 08:53:05
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAccessConfig = Class;
  TAction = Class;
  TAllowedRule = Class;
  TAutoscalingModule = Class;
  TAutoscalingModuleStatus = Class;
  TDeployState = Class;
  TDeployment = Class;
  TDeploymentsListResponse = Class;
  TDiskAttachment = Class;
  TEnvVariable = Class;
  TExistingDisk = Class;
  TFirewallModule = Class;
  TFirewallModuleStatus = Class;
  THealthCheckModule = Class;
  THealthCheckModuleStatus = Class;
  TLbModule = Class;
  TLbModuleStatus = Class;
  TMetadata = Class;
  TMetadataItem = Class;
  TModule = Class;
  TModuleStatus = Class;
  TNetworkInterface = Class;
  TNetworkModule = Class;
  TNetworkModuleStatus = Class;
  TNewDisk = Class;
  TNewDiskInitializeParams = Class;
  TParamOverride = Class;
  TReplicaPoolModule = Class;
  TReplicaPoolModuleStatus = Class;
  TReplicaPoolParams = Class;
  TReplicaPoolParamsV1Beta1 = Class;
  TServiceAccount = Class;
  TTag = Class;
  TTemplate = Class;
  TTemplatesListResponse = Class;
  TAccessConfigArray = Array of TAccessConfig;
  TActionArray = Array of TAction;
  TAllowedRuleArray = Array of TAllowedRule;
  TAutoscalingModuleArray = Array of TAutoscalingModule;
  TAutoscalingModuleStatusArray = Array of TAutoscalingModuleStatus;
  TDeployStateArray = Array of TDeployState;
  TDeploymentArray = Array of TDeployment;
  TDeploymentsListResponseArray = Array of TDeploymentsListResponse;
  TDiskAttachmentArray = Array of TDiskAttachment;
  TEnvVariableArray = Array of TEnvVariable;
  TExistingDiskArray = Array of TExistingDisk;
  TFirewallModuleArray = Array of TFirewallModule;
  TFirewallModuleStatusArray = Array of TFirewallModuleStatus;
  THealthCheckModuleArray = Array of THealthCheckModule;
  THealthCheckModuleStatusArray = Array of THealthCheckModuleStatus;
  TLbModuleArray = Array of TLbModule;
  TLbModuleStatusArray = Array of TLbModuleStatus;
  TMetadataArray = Array of TMetadata;
  TMetadataItemArray = Array of TMetadataItem;
  TModuleArray = Array of TModule;
  TModuleStatusArray = Array of TModuleStatus;
  TNetworkInterfaceArray = Array of TNetworkInterface;
  TNetworkModuleArray = Array of TNetworkModule;
  TNetworkModuleStatusArray = Array of TNetworkModuleStatus;
  TNewDiskArray = Array of TNewDisk;
  TNewDiskInitializeParamsArray = Array of TNewDiskInitializeParams;
  TParamOverrideArray = Array of TParamOverride;
  TReplicaPoolModuleArray = Array of TReplicaPoolModule;
  TReplicaPoolModuleStatusArray = Array of TReplicaPoolModuleStatus;
  TReplicaPoolParamsArray = Array of TReplicaPoolParams;
  TReplicaPoolParamsV1Beta1Array = Array of TReplicaPoolParamsV1Beta1;
  TServiceAccountArray = Array of TServiceAccount;
  TTagArray = Array of TTag;
  TTemplateArray = Array of TTemplate;
  TTemplatesListResponseArray = Array of TTemplatesListResponse;
  //Anonymous types, using auto-generated names
  TDeploymentTypemodules = Class;
  TReplicaPoolModuleTypeenvVariables = Class;
  TTemplateTypeactions = Class;
  TTemplateTypemodules = Class;
  TDeploymentTypeoverridesArray = Array of TParamOverride;
  TDeploymentsListResponseTyperesourcesArray = Array of TDeployment;
  TFirewallModuleTypeallowedArray = Array of TAllowedRule;
  TMetadataTypeitemsArray = Array of TMetadataItem;
  TNetworkInterfaceTypeaccessConfigsArray = Array of TAccessConfig;
  TReplicaPoolParamsV1Beta1TypedisksToAttachArray = Array of TExistingDisk;
  TReplicaPoolParamsV1Beta1TypedisksToCreateArray = Array of TNewDisk;
  TReplicaPoolParamsV1Beta1TypenetworkInterfacesArray = Array of TNetworkInterface;
  TReplicaPoolParamsV1Beta1TypeserviceAccountsArray = Array of TServiceAccount;
  TTemplatesListResponseTyperesourcesArray = Array of TTemplate;
  
  { --------------------------------------------------------------------
    TAccessConfig
    --------------------------------------------------------------------}
  
  TAccessConfig = Class(TGoogleBaseObject)
  Private
    Fname : String;
    FnatIp : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetnatIp(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property natIp : String Index 8 Read FnatIp Write SetnatIp;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TAccessConfigClass = Class of TAccessConfig;
  
  { --------------------------------------------------------------------
    TAction
    --------------------------------------------------------------------}
  
  TAction = Class(TGoogleBaseObject)
  Private
    Fcommands : TStringArray;
    FtimeoutMs : integer;
  Protected
    //Property setters
    Procedure Setcommands(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SettimeoutMs(AIndex : Integer; AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property commands : TStringArray Index 0 Read Fcommands Write Setcommands;
    Property timeoutMs : integer Index 8 Read FtimeoutMs Write SettimeoutMs;
  end;
  TActionClass = Class of TAction;
  
  { --------------------------------------------------------------------
    TAllowedRule
    --------------------------------------------------------------------}
  
  TAllowedRule = Class(TGoogleBaseObject)
  Private
    FIPProtocol : String;
    Fports : TStringArray;
  Protected
    //Property setters
    Procedure SetIPProtocol(AIndex : Integer; AValue : String); virtual;
    Procedure Setports(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property IPProtocol : String Index 0 Read FIPProtocol Write SetIPProtocol;
    Property ports : TStringArray Index 8 Read Fports Write Setports;
  end;
  TAllowedRuleClass = Class of TAllowedRule;
  
  { --------------------------------------------------------------------
    TAutoscalingModule
    --------------------------------------------------------------------}
  
  TAutoscalingModule = Class(TGoogleBaseObject)
  Private
    FcoolDownPeriodSec : integer;
    Fdescription : String;
    FmaxNumReplicas : integer;
    FminNumReplicas : integer;
    FsignalType : String;
    FtargetModule : String;
    FtargetUtilization : double;
  Protected
    //Property setters
    Procedure SetcoolDownPeriodSec(AIndex : Integer; AValue : integer); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaxNumReplicas(AIndex : Integer; AValue : integer); virtual;
    Procedure SetminNumReplicas(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsignalType(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetModule(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetUtilization(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property coolDownPeriodSec : integer Index 0 Read FcoolDownPeriodSec Write SetcoolDownPeriodSec;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property maxNumReplicas : integer Index 16 Read FmaxNumReplicas Write SetmaxNumReplicas;
    Property minNumReplicas : integer Index 24 Read FminNumReplicas Write SetminNumReplicas;
    Property signalType : String Index 32 Read FsignalType Write SetsignalType;
    Property targetModule : String Index 40 Read FtargetModule Write SettargetModule;
    Property targetUtilization : double Index 48 Read FtargetUtilization Write SettargetUtilization;
  end;
  TAutoscalingModuleClass = Class of TAutoscalingModule;
  
  { --------------------------------------------------------------------
    TAutoscalingModuleStatus
    --------------------------------------------------------------------}
  
  TAutoscalingModuleStatus = Class(TGoogleBaseObject)
  Private
    FautoscalingConfigUrl : String;
  Protected
    //Property setters
    Procedure SetautoscalingConfigUrl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property autoscalingConfigUrl : String Index 0 Read FautoscalingConfigUrl Write SetautoscalingConfigUrl;
  end;
  TAutoscalingModuleStatusClass = Class of TAutoscalingModuleStatus;
  
  { --------------------------------------------------------------------
    TDeployState
    --------------------------------------------------------------------}
  
  TDeployState = Class(TGoogleBaseObject)
  Private
    Fdetails : String;
    Fstatus : String;
  Protected
    //Property setters
    Procedure Setdetails(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property details : String Index 0 Read Fdetails Write Setdetails;
    Property status : String Index 8 Read Fstatus Write Setstatus;
  end;
  TDeployStateClass = Class of TDeployState;
  
  { --------------------------------------------------------------------
    TDeploymentTypemodules
    --------------------------------------------------------------------}
  
  TDeploymentTypemodules = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TDeploymentTypemodulesClass = Class of TDeploymentTypemodules;
  
  { --------------------------------------------------------------------
    TDeployment
    --------------------------------------------------------------------}
  
  TDeployment = Class(TGoogleBaseObject)
  Private
    FcreationDate : String;
    Fdescription : String;
    Fmodules : TDeploymentTypemodules;
    Fname : String;
    Foverrides : TDeploymentTypeoverridesArray;
    Fstate : TDeployState;
    FtemplateName : String;
  Protected
    //Property setters
    Procedure SetcreationDate(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setmodules(AIndex : Integer; AValue : TDeploymentTypemodules); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setoverrides(AIndex : Integer; AValue : TDeploymentTypeoverridesArray); virtual;
    Procedure Setstate(AIndex : Integer; AValue : TDeployState); virtual;
    Procedure SettemplateName(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property creationDate : String Index 0 Read FcreationDate Write SetcreationDate;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property modules : TDeploymentTypemodules Index 16 Read Fmodules Write Setmodules;
    Property name : String Index 24 Read Fname Write Setname;
    Property overrides : TDeploymentTypeoverridesArray Index 32 Read Foverrides Write Setoverrides;
    Property state : TDeployState Index 40 Read Fstate Write Setstate;
    Property templateName : String Index 48 Read FtemplateName Write SettemplateName;
  end;
  TDeploymentClass = Class of TDeployment;
  
  { --------------------------------------------------------------------
    TDeploymentsListResponse
    --------------------------------------------------------------------}
  
  TDeploymentsListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Fresources : TDeploymentsListResponseTyperesourcesArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TDeploymentsListResponseTyperesourcesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property resources : TDeploymentsListResponseTyperesourcesArray Index 8 Read Fresources Write Setresources;
  end;
  TDeploymentsListResponseClass = Class of TDeploymentsListResponse;
  
  { --------------------------------------------------------------------
    TDiskAttachment
    --------------------------------------------------------------------}
  
  TDiskAttachment = Class(TGoogleBaseObject)
  Private
    FdeviceName : String;
    Findex : integer;
  Protected
    //Property setters
    Procedure SetdeviceName(AIndex : Integer; AValue : String); virtual;
    Procedure Setindex(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property deviceName : String Index 0 Read FdeviceName Write SetdeviceName;
    Property index : integer Index 8 Read Findex Write Setindex;
  end;
  TDiskAttachmentClass = Class of TDiskAttachment;
  
  { --------------------------------------------------------------------
    TEnvVariable
    --------------------------------------------------------------------}
  
  TEnvVariable = Class(TGoogleBaseObject)
  Private
    Fhidden : boolean;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Sethidden(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property hidden : boolean Index 0 Read Fhidden Write Sethidden;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TEnvVariableClass = Class of TEnvVariable;
  
  { --------------------------------------------------------------------
    TExistingDisk
    --------------------------------------------------------------------}
  
  TExistingDisk = Class(TGoogleBaseObject)
  Private
    Fattachment : TDiskAttachment;
    Fsource : String;
  Protected
    //Property setters
    Procedure Setattachment(AIndex : Integer; AValue : TDiskAttachment); virtual;
    Procedure Setsource(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property attachment : TDiskAttachment Index 0 Read Fattachment Write Setattachment;
    Property source : String Index 8 Read Fsource Write Setsource;
  end;
  TExistingDiskClass = Class of TExistingDisk;
  
  { --------------------------------------------------------------------
    TFirewallModule
    --------------------------------------------------------------------}
  
  TFirewallModule = Class(TGoogleBaseObject)
  Private
    Fallowed : TFirewallModuleTypeallowedArray;
    Fdescription : String;
    Fnetwork : String;
    FsourceRanges : TStringArray;
    FsourceTags : TStringArray;
    FtargetTags : TStringArray;
  Protected
    //Property setters
    Procedure Setallowed(AIndex : Integer; AValue : TFirewallModuleTypeallowedArray); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setnetwork(AIndex : Integer; AValue : String); virtual;
    Procedure SetsourceRanges(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetsourceTags(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SettargetTags(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property allowed : TFirewallModuleTypeallowedArray Index 0 Read Fallowed Write Setallowed;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property network : String Index 16 Read Fnetwork Write Setnetwork;
    Property sourceRanges : TStringArray Index 24 Read FsourceRanges Write SetsourceRanges;
    Property sourceTags : TStringArray Index 32 Read FsourceTags Write SetsourceTags;
    Property targetTags : TStringArray Index 40 Read FtargetTags Write SettargetTags;
  end;
  TFirewallModuleClass = Class of TFirewallModule;
  
  { --------------------------------------------------------------------
    TFirewallModuleStatus
    --------------------------------------------------------------------}
  
  TFirewallModuleStatus = Class(TGoogleBaseObject)
  Private
    FfirewallUrl : String;
  Protected
    //Property setters
    Procedure SetfirewallUrl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property firewallUrl : String Index 0 Read FfirewallUrl Write SetfirewallUrl;
  end;
  TFirewallModuleStatusClass = Class of TFirewallModuleStatus;
  
  { --------------------------------------------------------------------
    THealthCheckModule
    --------------------------------------------------------------------}
  
  THealthCheckModule = Class(TGoogleBaseObject)
  Private
    FcheckIntervalSec : integer;
    Fdescription : String;
    FhealthyThreshold : integer;
    Fhost : String;
    Fpath : String;
    Fport : integer;
    FtimeoutSec : integer;
    FunhealthyThreshold : integer;
  Protected
    //Property setters
    Procedure SetcheckIntervalSec(AIndex : Integer; AValue : integer); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SethealthyThreshold(AIndex : Integer; AValue : integer); virtual;
    Procedure Sethost(AIndex : Integer; AValue : String); virtual;
    Procedure Setpath(AIndex : Integer; AValue : String); virtual;
    Procedure Setport(AIndex : Integer; AValue : integer); virtual;
    Procedure SettimeoutSec(AIndex : Integer; AValue : integer); virtual;
    Procedure SetunhealthyThreshold(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property checkIntervalSec : integer Index 0 Read FcheckIntervalSec Write SetcheckIntervalSec;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property healthyThreshold : integer Index 16 Read FhealthyThreshold Write SethealthyThreshold;
    Property host : String Index 24 Read Fhost Write Sethost;
    Property path : String Index 32 Read Fpath Write Setpath;
    Property port : integer Index 40 Read Fport Write Setport;
    Property timeoutSec : integer Index 48 Read FtimeoutSec Write SettimeoutSec;
    Property unhealthyThreshold : integer Index 56 Read FunhealthyThreshold Write SetunhealthyThreshold;
  end;
  THealthCheckModuleClass = Class of THealthCheckModule;
  
  { --------------------------------------------------------------------
    THealthCheckModuleStatus
    --------------------------------------------------------------------}
  
  THealthCheckModuleStatus = Class(TGoogleBaseObject)
  Private
    FhealthCheckUrl : String;
  Protected
    //Property setters
    Procedure SethealthCheckUrl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property healthCheckUrl : String Index 0 Read FhealthCheckUrl Write SethealthCheckUrl;
  end;
  THealthCheckModuleStatusClass = Class of THealthCheckModuleStatus;
  
  { --------------------------------------------------------------------
    TLbModule
    --------------------------------------------------------------------}
  
  TLbModule = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    FhealthChecks : TStringArray;
    FipAddress : String;
    FipProtocol : String;
    FportRange : String;
    FsessionAffinity : String;
    FtargetModules : TStringArray;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SethealthChecks(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetipAddress(AIndex : Integer; AValue : String); virtual;
    Procedure SetipProtocol(AIndex : Integer; AValue : String); virtual;
    Procedure SetportRange(AIndex : Integer; AValue : String); virtual;
    Procedure SetsessionAffinity(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetModules(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property healthChecks : TStringArray Index 8 Read FhealthChecks Write SethealthChecks;
    Property ipAddress : String Index 16 Read FipAddress Write SetipAddress;
    Property ipProtocol : String Index 24 Read FipProtocol Write SetipProtocol;
    Property portRange : String Index 32 Read FportRange Write SetportRange;
    Property sessionAffinity : String Index 40 Read FsessionAffinity Write SetsessionAffinity;
    Property targetModules : TStringArray Index 48 Read FtargetModules Write SettargetModules;
  end;
  TLbModuleClass = Class of TLbModule;
  
  { --------------------------------------------------------------------
    TLbModuleStatus
    --------------------------------------------------------------------}
  
  TLbModuleStatus = Class(TGoogleBaseObject)
  Private
    FforwardingRuleUrl : String;
    FtargetPoolUrl : String;
  Protected
    //Property setters
    Procedure SetforwardingRuleUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetPoolUrl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property forwardingRuleUrl : String Index 0 Read FforwardingRuleUrl Write SetforwardingRuleUrl;
    Property targetPoolUrl : String Index 8 Read FtargetPoolUrl Write SettargetPoolUrl;
  end;
  TLbModuleStatusClass = Class of TLbModuleStatus;
  
  { --------------------------------------------------------------------
    TMetadata
    --------------------------------------------------------------------}
  
  TMetadata = Class(TGoogleBaseObject)
  Private
    FfingerPrint : String;
    Fitems : TMetadataTypeitemsArray;
  Protected
    //Property setters
    Procedure SetfingerPrint(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TMetadataTypeitemsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property fingerPrint : String Index 0 Read FfingerPrint Write SetfingerPrint;
    Property items : TMetadataTypeitemsArray Index 8 Read Fitems Write Setitems;
  end;
  TMetadataClass = Class of TMetadata;
  
  { --------------------------------------------------------------------
    TMetadataItem
    --------------------------------------------------------------------}
  
  TMetadataItem = Class(TGoogleBaseObject)
  Private
    Fkey : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property key : String Index 0 Read Fkey Write Setkey;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TMetadataItemClass = Class of TMetadataItem;
  
  { --------------------------------------------------------------------
    TModule
    --------------------------------------------------------------------}
  
  TModule = Class(TGoogleBaseObject)
  Private
    FautoscalingModule : TAutoscalingModule;
    FfirewallModule : TFirewallModule;
    FhealthCheckModule : THealthCheckModule;
    FlbModule : TLbModule;
    FnetworkModule : TNetworkModule;
    FreplicaPoolModule : TReplicaPoolModule;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetautoscalingModule(AIndex : Integer; AValue : TAutoscalingModule); virtual;
    Procedure SetfirewallModule(AIndex : Integer; AValue : TFirewallModule); virtual;
    Procedure SethealthCheckModule(AIndex : Integer; AValue : THealthCheckModule); virtual;
    Procedure SetlbModule(AIndex : Integer; AValue : TLbModule); virtual;
    Procedure SetnetworkModule(AIndex : Integer; AValue : TNetworkModule); virtual;
    Procedure SetreplicaPoolModule(AIndex : Integer; AValue : TReplicaPoolModule); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property autoscalingModule : TAutoscalingModule Index 0 Read FautoscalingModule Write SetautoscalingModule;
    Property firewallModule : TFirewallModule Index 8 Read FfirewallModule Write SetfirewallModule;
    Property healthCheckModule : THealthCheckModule Index 16 Read FhealthCheckModule Write SethealthCheckModule;
    Property lbModule : TLbModule Index 24 Read FlbModule Write SetlbModule;
    Property networkModule : TNetworkModule Index 32 Read FnetworkModule Write SetnetworkModule;
    Property replicaPoolModule : TReplicaPoolModule Index 40 Read FreplicaPoolModule Write SetreplicaPoolModule;
    Property _type : String Index 48 Read F_type Write Set_type;
  end;
  TModuleClass = Class of TModule;
  
  { --------------------------------------------------------------------
    TModuleStatus
    --------------------------------------------------------------------}
  
  TModuleStatus = Class(TGoogleBaseObject)
  Private
    FautoscalingModuleStatus : TAutoscalingModuleStatus;
    FfirewallModuleStatus : TFirewallModuleStatus;
    FhealthCheckModuleStatus : THealthCheckModuleStatus;
    FlbModuleStatus : TLbModuleStatus;
    FnetworkModuleStatus : TNetworkModuleStatus;
    FreplicaPoolModuleStatus : TReplicaPoolModuleStatus;
    Fstate : TDeployState;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetautoscalingModuleStatus(AIndex : Integer; AValue : TAutoscalingModuleStatus); virtual;
    Procedure SetfirewallModuleStatus(AIndex : Integer; AValue : TFirewallModuleStatus); virtual;
    Procedure SethealthCheckModuleStatus(AIndex : Integer; AValue : THealthCheckModuleStatus); virtual;
    Procedure SetlbModuleStatus(AIndex : Integer; AValue : TLbModuleStatus); virtual;
    Procedure SetnetworkModuleStatus(AIndex : Integer; AValue : TNetworkModuleStatus); virtual;
    Procedure SetreplicaPoolModuleStatus(AIndex : Integer; AValue : TReplicaPoolModuleStatus); virtual;
    Procedure Setstate(AIndex : Integer; AValue : TDeployState); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property autoscalingModuleStatus : TAutoscalingModuleStatus Index 0 Read FautoscalingModuleStatus Write SetautoscalingModuleStatus;
    Property firewallModuleStatus : TFirewallModuleStatus Index 8 Read FfirewallModuleStatus Write SetfirewallModuleStatus;
    Property healthCheckModuleStatus : THealthCheckModuleStatus Index 16 Read FhealthCheckModuleStatus Write SethealthCheckModuleStatus;
    Property lbModuleStatus : TLbModuleStatus Index 24 Read FlbModuleStatus Write SetlbModuleStatus;
    Property networkModuleStatus : TNetworkModuleStatus Index 32 Read FnetworkModuleStatus Write SetnetworkModuleStatus;
    Property replicaPoolModuleStatus : TReplicaPoolModuleStatus Index 40 Read FreplicaPoolModuleStatus Write SetreplicaPoolModuleStatus;
    Property state : TDeployState Index 48 Read Fstate Write Setstate;
    Property _type : String Index 56 Read F_type Write Set_type;
  end;
  TModuleStatusClass = Class of TModuleStatus;
  
  { --------------------------------------------------------------------
    TNetworkInterface
    --------------------------------------------------------------------}
  
  TNetworkInterface = Class(TGoogleBaseObject)
  Private
    FaccessConfigs : TNetworkInterfaceTypeaccessConfigsArray;
    Fname : String;
    Fnetwork : String;
    FnetworkIp : String;
  Protected
    //Property setters
    Procedure SetaccessConfigs(AIndex : Integer; AValue : TNetworkInterfaceTypeaccessConfigsArray); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setnetwork(AIndex : Integer; AValue : String); virtual;
    Procedure SetnetworkIp(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accessConfigs : TNetworkInterfaceTypeaccessConfigsArray Index 0 Read FaccessConfigs Write SetaccessConfigs;
    Property name : String Index 8 Read Fname Write Setname;
    Property network : String Index 16 Read Fnetwork Write Setnetwork;
    Property networkIp : String Index 24 Read FnetworkIp Write SetnetworkIp;
  end;
  TNetworkInterfaceClass = Class of TNetworkInterface;
  
  { --------------------------------------------------------------------
    TNetworkModule
    --------------------------------------------------------------------}
  
  TNetworkModule = Class(TGoogleBaseObject)
  Private
    FIPv4Range : String;
    Fdescription : String;
    FgatewayIPv4 : String;
  Protected
    //Property setters
    Procedure SetIPv4Range(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetgatewayIPv4(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property IPv4Range : String Index 0 Read FIPv4Range Write SetIPv4Range;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property gatewayIPv4 : String Index 16 Read FgatewayIPv4 Write SetgatewayIPv4;
  end;
  TNetworkModuleClass = Class of TNetworkModule;
  
  { --------------------------------------------------------------------
    TNetworkModuleStatus
    --------------------------------------------------------------------}
  
  TNetworkModuleStatus = Class(TGoogleBaseObject)
  Private
    FnetworkUrl : String;
  Protected
    //Property setters
    Procedure SetnetworkUrl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property networkUrl : String Index 0 Read FnetworkUrl Write SetnetworkUrl;
  end;
  TNetworkModuleStatusClass = Class of TNetworkModuleStatus;
  
  { --------------------------------------------------------------------
    TNewDisk
    --------------------------------------------------------------------}
  
  TNewDisk = Class(TGoogleBaseObject)
  Private
    Fattachment : TDiskAttachment;
    FautoDelete : boolean;
    Fboot : boolean;
    FinitializeParams : TNewDiskInitializeParams;
  Protected
    //Property setters
    Procedure Setattachment(AIndex : Integer; AValue : TDiskAttachment); virtual;
    Procedure SetautoDelete(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setboot(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetinitializeParams(AIndex : Integer; AValue : TNewDiskInitializeParams); virtual;
  Public
  Published
    Property attachment : TDiskAttachment Index 0 Read Fattachment Write Setattachment;
    Property autoDelete : boolean Index 8 Read FautoDelete Write SetautoDelete;
    Property boot : boolean Index 16 Read Fboot Write Setboot;
    Property initializeParams : TNewDiskInitializeParams Index 24 Read FinitializeParams Write SetinitializeParams;
  end;
  TNewDiskClass = Class of TNewDisk;
  
  { --------------------------------------------------------------------
    TNewDiskInitializeParams
    --------------------------------------------------------------------}
  
  TNewDiskInitializeParams = Class(TGoogleBaseObject)
  Private
    FdiskSizeGb : String;
    FdiskType : String;
    FsourceImage : String;
  Protected
    //Property setters
    Procedure SetdiskSizeGb(AIndex : Integer; AValue : String); virtual;
    Procedure SetdiskType(AIndex : Integer; AValue : String); virtual;
    Procedure SetsourceImage(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property diskSizeGb : String Index 0 Read FdiskSizeGb Write SetdiskSizeGb;
    Property diskType : String Index 8 Read FdiskType Write SetdiskType;
    Property sourceImage : String Index 16 Read FsourceImage Write SetsourceImage;
  end;
  TNewDiskInitializeParamsClass = Class of TNewDiskInitializeParams;
  
  { --------------------------------------------------------------------
    TParamOverride
    --------------------------------------------------------------------}
  
  TParamOverride = Class(TGoogleBaseObject)
  Private
    Fpath : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setpath(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property path : String Index 0 Read Fpath Write Setpath;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TParamOverrideClass = Class of TParamOverride;
  
  { --------------------------------------------------------------------
    TReplicaPoolModuleTypeenvVariables
    --------------------------------------------------------------------}
  
  TReplicaPoolModuleTypeenvVariables = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TReplicaPoolModuleTypeenvVariablesClass = Class of TReplicaPoolModuleTypeenvVariables;
  
  { --------------------------------------------------------------------
    TReplicaPoolModule
    --------------------------------------------------------------------}
  
  TReplicaPoolModule = Class(TGoogleBaseObject)
  Private
    FenvVariables : TReplicaPoolModuleTypeenvVariables;
    FhealthChecks : TStringArray;
    FnumReplicas : integer;
    FreplicaPoolParams : TReplicaPoolParams;
    FresourceView : String;
  Protected
    //Property setters
    Procedure SetenvVariables(AIndex : Integer; AValue : TReplicaPoolModuleTypeenvVariables); virtual;
    Procedure SethealthChecks(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetnumReplicas(AIndex : Integer; AValue : integer); virtual;
    Procedure SetreplicaPoolParams(AIndex : Integer; AValue : TReplicaPoolParams); virtual;
    Procedure SetresourceView(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property envVariables : TReplicaPoolModuleTypeenvVariables Index 0 Read FenvVariables Write SetenvVariables;
    Property healthChecks : TStringArray Index 8 Read FhealthChecks Write SethealthChecks;
    Property numReplicas : integer Index 16 Read FnumReplicas Write SetnumReplicas;
    Property replicaPoolParams : TReplicaPoolParams Index 24 Read FreplicaPoolParams Write SetreplicaPoolParams;
    Property resourceView : String Index 32 Read FresourceView Write SetresourceView;
  end;
  TReplicaPoolModuleClass = Class of TReplicaPoolModule;
  
  { --------------------------------------------------------------------
    TReplicaPoolModuleStatus
    --------------------------------------------------------------------}
  
  TReplicaPoolModuleStatus = Class(TGoogleBaseObject)
  Private
    FreplicaPoolUrl : String;
    FresourceViewUrl : String;
  Protected
    //Property setters
    Procedure SetreplicaPoolUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetresourceViewUrl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property replicaPoolUrl : String Index 0 Read FreplicaPoolUrl Write SetreplicaPoolUrl;
    Property resourceViewUrl : String Index 8 Read FresourceViewUrl Write SetresourceViewUrl;
  end;
  TReplicaPoolModuleStatusClass = Class of TReplicaPoolModuleStatus;
  
  { --------------------------------------------------------------------
    TReplicaPoolParams
    --------------------------------------------------------------------}
  
  TReplicaPoolParams = Class(TGoogleBaseObject)
  Private
    Fv1beta1 : TReplicaPoolParamsV1Beta1;
  Protected
    //Property setters
    Procedure Setv1beta1(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1); virtual;
  Public
  Published
    Property v1beta1 : TReplicaPoolParamsV1Beta1 Index 0 Read Fv1beta1 Write Setv1beta1;
  end;
  TReplicaPoolParamsClass = Class of TReplicaPoolParams;
  
  { --------------------------------------------------------------------
    TReplicaPoolParamsV1Beta1
    --------------------------------------------------------------------}
  
  TReplicaPoolParamsV1Beta1 = Class(TGoogleBaseObject)
  Private
    FautoRestart : boolean;
    FbaseInstanceName : String;
    FcanIpForward : boolean;
    Fdescription : String;
    FdisksToAttach : TReplicaPoolParamsV1Beta1TypedisksToAttachArray;
    FdisksToCreate : TReplicaPoolParamsV1Beta1TypedisksToCreateArray;
    FinitAction : String;
    FmachineType : String;
    Fmetadata : TMetadata;
    FnetworkInterfaces : TReplicaPoolParamsV1Beta1TypenetworkInterfacesArray;
    FonHostMaintenance : String;
    FserviceAccounts : TReplicaPoolParamsV1Beta1TypeserviceAccountsArray;
    Ftags : TTag;
    Fzone : String;
  Protected
    //Property setters
    Procedure SetautoRestart(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetbaseInstanceName(AIndex : Integer; AValue : String); virtual;
    Procedure SetcanIpForward(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisksToAttach(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1TypedisksToAttachArray); virtual;
    Procedure SetdisksToCreate(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1TypedisksToCreateArray); virtual;
    Procedure SetinitAction(AIndex : Integer; AValue : String); virtual;
    Procedure SetmachineType(AIndex : Integer; AValue : String); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TMetadata); virtual;
    Procedure SetnetworkInterfaces(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1TypenetworkInterfacesArray); virtual;
    Procedure SetonHostMaintenance(AIndex : Integer; AValue : String); virtual;
    Procedure SetserviceAccounts(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1TypeserviceAccountsArray); virtual;
    Procedure Settags(AIndex : Integer; AValue : TTag); virtual;
    Procedure Setzone(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property autoRestart : boolean Index 0 Read FautoRestart Write SetautoRestart;
    Property baseInstanceName : String Index 8 Read FbaseInstanceName Write SetbaseInstanceName;
    Property canIpForward : boolean Index 16 Read FcanIpForward Write SetcanIpForward;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property disksToAttach : TReplicaPoolParamsV1Beta1TypedisksToAttachArray Index 32 Read FdisksToAttach Write SetdisksToAttach;
    Property disksToCreate : TReplicaPoolParamsV1Beta1TypedisksToCreateArray Index 40 Read FdisksToCreate Write SetdisksToCreate;
    Property initAction : String Index 48 Read FinitAction Write SetinitAction;
    Property machineType : String Index 56 Read FmachineType Write SetmachineType;
    Property metadata : TMetadata Index 64 Read Fmetadata Write Setmetadata;
    Property networkInterfaces : TReplicaPoolParamsV1Beta1TypenetworkInterfacesArray Index 72 Read FnetworkInterfaces Write SetnetworkInterfaces;
    Property onHostMaintenance : String Index 80 Read FonHostMaintenance Write SetonHostMaintenance;
    Property serviceAccounts : TReplicaPoolParamsV1Beta1TypeserviceAccountsArray Index 88 Read FserviceAccounts Write SetserviceAccounts;
    Property tags : TTag Index 96 Read Ftags Write Settags;
    Property zone : String Index 104 Read Fzone Write Setzone;
  end;
  TReplicaPoolParamsV1Beta1Class = Class of TReplicaPoolParamsV1Beta1;
  
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
    TTag
    --------------------------------------------------------------------}
  
  TTag = Class(TGoogleBaseObject)
  Private
    FfingerPrint : String;
    Fitems : TStringArray;
  Protected
    //Property setters
    Procedure SetfingerPrint(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property fingerPrint : String Index 0 Read FfingerPrint Write SetfingerPrint;
    Property items : TStringArray Index 8 Read Fitems Write Setitems;
  end;
  TTagClass = Class of TTag;
  
  { --------------------------------------------------------------------
    TTemplateTypeactions
    --------------------------------------------------------------------}
  
  TTemplateTypeactions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TTemplateTypeactionsClass = Class of TTemplateTypeactions;
  
  { --------------------------------------------------------------------
    TTemplateTypemodules
    --------------------------------------------------------------------}
  
  TTemplateTypemodules = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TTemplateTypemodulesClass = Class of TTemplateTypemodules;
  
  { --------------------------------------------------------------------
    TTemplate
    --------------------------------------------------------------------}
  
  TTemplate = Class(TGoogleBaseObject)
  Private
    Factions : TTemplateTypeactions;
    Fdescription : String;
    Fmodules : TTemplateTypemodules;
    Fname : String;
  Protected
    //Property setters
    Procedure Setactions(AIndex : Integer; AValue : TTemplateTypeactions); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setmodules(AIndex : Integer; AValue : TTemplateTypemodules); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property actions : TTemplateTypeactions Index 0 Read Factions Write Setactions;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property modules : TTemplateTypemodules Index 16 Read Fmodules Write Setmodules;
    Property name : String Index 24 Read Fname Write Setname;
  end;
  TTemplateClass = Class of TTemplate;
  
  { --------------------------------------------------------------------
    TTemplatesListResponse
    --------------------------------------------------------------------}
  
  TTemplatesListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Fresources : TTemplatesListResponseTyperesourcesArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TTemplatesListResponseTyperesourcesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property resources : TTemplatesListResponseTyperesourcesArray Index 8 Read Fresources Write Setresources;
  end;
  TTemplatesListResponseClass = Class of TTemplatesListResponse;
  
  { --------------------------------------------------------------------
    TDeploymentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDeploymentsResource, method List
  
  TDeploymentsListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TDeploymentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(deploymentName: string; projectId: string; region: string);
    Function Get(deploymentName: string; projectId: string; region: string) : TDeployment;
    Function Insert(projectId: string; region: string; aDeployment : TDeployment) : TDeployment;
    Function List(projectId: string; region: string; AQuery : string  = '') : TDeploymentsListResponse;
    Function List(projectId: string; region: string; AQuery : TDeploymentslistOptions) : TDeploymentsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TTemplatesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTemplatesResource, method List
  
  TTemplatesListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TTemplatesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(projectId: string; templateName: string);
    Function Get(projectId: string; templateName: string) : TTemplate;
    Function Insert(projectId: string; aTemplate : TTemplate) : TTemplate;
    Function List(projectId: string; AQuery : string  = '') : TTemplatesListResponse;
    Function List(projectId: string; AQuery : TTemplateslistOptions) : TTemplatesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TManagerAPI
    --------------------------------------------------------------------}
  
  TManagerAPI = Class(TGoogleAPI)
  Private
    FDeploymentsInstance : TDeploymentsResource;
    FTemplatesInstance : TTemplatesResource;
    Function GetDeploymentsInstance : TDeploymentsResource;virtual;
    Function GetTemplatesInstance : TTemplatesResource;virtual;
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
    Function CreateDeploymentsResource(AOwner : TComponent) : TDeploymentsResource;virtual;overload;
    Function CreateDeploymentsResource : TDeploymentsResource;virtual;overload;
    Function CreateTemplatesResource(AOwner : TComponent) : TTemplatesResource;virtual;overload;
    Function CreateTemplatesResource : TTemplatesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property DeploymentsResource : TDeploymentsResource Read GetDeploymentsInstance;
    Property TemplatesResource : TTemplatesResource Read GetTemplatesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAccessConfig
  --------------------------------------------------------------------}


Procedure TAccessConfig.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccessConfig.SetnatIp(AIndex : Integer; AValue : String); 

begin
  If (FnatIp=AValue) then exit;
  FnatIp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccessConfig.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAccessConfig.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAction
  --------------------------------------------------------------------}


Procedure TAction.Setcommands(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fcommands=AValue) then exit;
  Fcommands:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAction.SettimeoutMs(AIndex : Integer; AValue : integer); 

begin
  If (FtimeoutMs=AValue) then exit;
  FtimeoutMs:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAction.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'commands' : SetLength(Fcommands,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAllowedRule
  --------------------------------------------------------------------}


Procedure TAllowedRule.SetIPProtocol(AIndex : Integer; AValue : String); 

begin
  If (FIPProtocol=AValue) then exit;
  FIPProtocol:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAllowedRule.Setports(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fports=AValue) then exit;
  Fports:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAllowedRule.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'ports' : SetLength(Fports,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAutoscalingModule
  --------------------------------------------------------------------}


Procedure TAutoscalingModule.SetcoolDownPeriodSec(AIndex : Integer; AValue : integer); 

begin
  If (FcoolDownPeriodSec=AValue) then exit;
  FcoolDownPeriodSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingModule.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingModule.SetmaxNumReplicas(AIndex : Integer; AValue : integer); 

begin
  If (FmaxNumReplicas=AValue) then exit;
  FmaxNumReplicas:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingModule.SetminNumReplicas(AIndex : Integer; AValue : integer); 

begin
  If (FminNumReplicas=AValue) then exit;
  FminNumReplicas:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingModule.SetsignalType(AIndex : Integer; AValue : String); 

begin
  If (FsignalType=AValue) then exit;
  FsignalType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingModule.SettargetModule(AIndex : Integer; AValue : String); 

begin
  If (FtargetModule=AValue) then exit;
  FtargetModule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingModule.SettargetUtilization(AIndex : Integer; AValue : double); 

begin
  If (FtargetUtilization=AValue) then exit;
  FtargetUtilization:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAutoscalingModuleStatus
  --------------------------------------------------------------------}


Procedure TAutoscalingModuleStatus.SetautoscalingConfigUrl(AIndex : Integer; AValue : String); 

begin
  If (FautoscalingConfigUrl=AValue) then exit;
  FautoscalingConfigUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeployState
  --------------------------------------------------------------------}


Procedure TDeployState.Setdetails(AIndex : Integer; AValue : String); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployState.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeploymentTypemodules
  --------------------------------------------------------------------}


Class Function TDeploymentTypemodules.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TDeployment
  --------------------------------------------------------------------}


Procedure TDeployment.SetcreationDate(AIndex : Integer; AValue : String); 

begin
  If (FcreationDate=AValue) then exit;
  FcreationDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setmodules(AIndex : Integer; AValue : TDeploymentTypemodules); 

begin
  If (Fmodules=AValue) then exit;
  Fmodules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setoverrides(AIndex : Integer; AValue : TDeploymentTypeoverridesArray); 

begin
  If (Foverrides=AValue) then exit;
  Foverrides:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setstate(AIndex : Integer; AValue : TDeployState); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.SettemplateName(AIndex : Integer; AValue : String); 

begin
  If (FtemplateName=AValue) then exit;
  FtemplateName:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDeployment.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'overrides' : SetLength(Foverrides,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDeploymentsListResponse
  --------------------------------------------------------------------}


Procedure TDeploymentsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeploymentsListResponse.Setresources(AIndex : Integer; AValue : TDeploymentsListResponseTyperesourcesArray); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDeploymentsListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'resources' : SetLength(Fresources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDiskAttachment
  --------------------------------------------------------------------}


Procedure TDiskAttachment.SetdeviceName(AIndex : Integer; AValue : String); 

begin
  If (FdeviceName=AValue) then exit;
  FdeviceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiskAttachment.Setindex(AIndex : Integer; AValue : integer); 

begin
  If (Findex=AValue) then exit;
  Findex:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEnvVariable
  --------------------------------------------------------------------}


Procedure TEnvVariable.Sethidden(AIndex : Integer; AValue : boolean); 

begin
  If (Fhidden=AValue) then exit;
  Fhidden:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEnvVariable.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExistingDisk
  --------------------------------------------------------------------}


Procedure TExistingDisk.Setattachment(AIndex : Integer; AValue : TDiskAttachment); 

begin
  If (Fattachment=AValue) then exit;
  Fattachment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExistingDisk.Setsource(AIndex : Integer; AValue : String); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFirewallModule
  --------------------------------------------------------------------}


Procedure TFirewallModule.Setallowed(AIndex : Integer; AValue : TFirewallModuleTypeallowedArray); 

begin
  If (Fallowed=AValue) then exit;
  Fallowed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewallModule.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewallModule.Setnetwork(AIndex : Integer; AValue : String); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewallModule.SetsourceRanges(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsourceRanges=AValue) then exit;
  FsourceRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewallModule.SetsourceTags(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsourceTags=AValue) then exit;
  FsourceTags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewallModule.SettargetTags(AIndex : Integer; AValue : TStringArray); 

begin
  If (FtargetTags=AValue) then exit;
  FtargetTags:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFirewallModule.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'allowed' : SetLength(Fallowed,ALength);
  'sourceranges' : SetLength(FsourceRanges,ALength);
  'sourcetags' : SetLength(FsourceTags,ALength);
  'targettags' : SetLength(FtargetTags,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TFirewallModuleStatus
  --------------------------------------------------------------------}


Procedure TFirewallModuleStatus.SetfirewallUrl(AIndex : Integer; AValue : String); 

begin
  If (FfirewallUrl=AValue) then exit;
  FfirewallUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THealthCheckModule
  --------------------------------------------------------------------}


Procedure THealthCheckModule.SetcheckIntervalSec(AIndex : Integer; AValue : integer); 

begin
  If (FcheckIntervalSec=AValue) then exit;
  FcheckIntervalSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthCheckModule.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthCheckModule.SethealthyThreshold(AIndex : Integer; AValue : integer); 

begin
  If (FhealthyThreshold=AValue) then exit;
  FhealthyThreshold:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthCheckModule.Sethost(AIndex : Integer; AValue : String); 

begin
  If (Fhost=AValue) then exit;
  Fhost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthCheckModule.Setpath(AIndex : Integer; AValue : String); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthCheckModule.Setport(AIndex : Integer; AValue : integer); 

begin
  If (Fport=AValue) then exit;
  Fport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthCheckModule.SettimeoutSec(AIndex : Integer; AValue : integer); 

begin
  If (FtimeoutSec=AValue) then exit;
  FtimeoutSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthCheckModule.SetunhealthyThreshold(AIndex : Integer; AValue : integer); 

begin
  If (FunhealthyThreshold=AValue) then exit;
  FunhealthyThreshold:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  THealthCheckModuleStatus
  --------------------------------------------------------------------}


Procedure THealthCheckModuleStatus.SethealthCheckUrl(AIndex : Integer; AValue : String); 

begin
  If (FhealthCheckUrl=AValue) then exit;
  FhealthCheckUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLbModule
  --------------------------------------------------------------------}


Procedure TLbModule.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLbModule.SethealthChecks(AIndex : Integer; AValue : TStringArray); 

begin
  If (FhealthChecks=AValue) then exit;
  FhealthChecks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLbModule.SetipAddress(AIndex : Integer; AValue : String); 

begin
  If (FipAddress=AValue) then exit;
  FipAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLbModule.SetipProtocol(AIndex : Integer; AValue : String); 

begin
  If (FipProtocol=AValue) then exit;
  FipProtocol:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLbModule.SetportRange(AIndex : Integer; AValue : String); 

begin
  If (FportRange=AValue) then exit;
  FportRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLbModule.SetsessionAffinity(AIndex : Integer; AValue : String); 

begin
  If (FsessionAffinity=AValue) then exit;
  FsessionAffinity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLbModule.SettargetModules(AIndex : Integer; AValue : TStringArray); 

begin
  If (FtargetModules=AValue) then exit;
  FtargetModules:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TLbModule.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'healthchecks' : SetLength(FhealthChecks,ALength);
  'targetmodules' : SetLength(FtargetModules,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLbModuleStatus
  --------------------------------------------------------------------}


Procedure TLbModuleStatus.SetforwardingRuleUrl(AIndex : Integer; AValue : String); 

begin
  If (FforwardingRuleUrl=AValue) then exit;
  FforwardingRuleUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLbModuleStatus.SettargetPoolUrl(AIndex : Integer; AValue : String); 

begin
  If (FtargetPoolUrl=AValue) then exit;
  FtargetPoolUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetadata
  --------------------------------------------------------------------}


Procedure TMetadata.SetfingerPrint(AIndex : Integer; AValue : String); 

begin
  If (FfingerPrint=AValue) then exit;
  FfingerPrint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Setitems(AIndex : Integer; AValue : TMetadataTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMetadata.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMetadataItem
  --------------------------------------------------------------------}


Procedure TMetadataItem.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadataItem.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TModule
  --------------------------------------------------------------------}


Procedure TModule.SetautoscalingModule(AIndex : Integer; AValue : TAutoscalingModule); 

begin
  If (FautoscalingModule=AValue) then exit;
  FautoscalingModule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModule.SetfirewallModule(AIndex : Integer; AValue : TFirewallModule); 

begin
  If (FfirewallModule=AValue) then exit;
  FfirewallModule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModule.SethealthCheckModule(AIndex : Integer; AValue : THealthCheckModule); 

begin
  If (FhealthCheckModule=AValue) then exit;
  FhealthCheckModule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModule.SetlbModule(AIndex : Integer; AValue : TLbModule); 

begin
  If (FlbModule=AValue) then exit;
  FlbModule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModule.SetnetworkModule(AIndex : Integer; AValue : TNetworkModule); 

begin
  If (FnetworkModule=AValue) then exit;
  FnetworkModule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModule.SetreplicaPoolModule(AIndex : Integer; AValue : TReplicaPoolModule); 

begin
  If (FreplicaPoolModule=AValue) then exit;
  FreplicaPoolModule:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModule.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TModule.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TModuleStatus
  --------------------------------------------------------------------}


Procedure TModuleStatus.SetautoscalingModuleStatus(AIndex : Integer; AValue : TAutoscalingModuleStatus); 

begin
  If (FautoscalingModuleStatus=AValue) then exit;
  FautoscalingModuleStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModuleStatus.SetfirewallModuleStatus(AIndex : Integer; AValue : TFirewallModuleStatus); 

begin
  If (FfirewallModuleStatus=AValue) then exit;
  FfirewallModuleStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModuleStatus.SethealthCheckModuleStatus(AIndex : Integer; AValue : THealthCheckModuleStatus); 

begin
  If (FhealthCheckModuleStatus=AValue) then exit;
  FhealthCheckModuleStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModuleStatus.SetlbModuleStatus(AIndex : Integer; AValue : TLbModuleStatus); 

begin
  If (FlbModuleStatus=AValue) then exit;
  FlbModuleStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModuleStatus.SetnetworkModuleStatus(AIndex : Integer; AValue : TNetworkModuleStatus); 

begin
  If (FnetworkModuleStatus=AValue) then exit;
  FnetworkModuleStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModuleStatus.SetreplicaPoolModuleStatus(AIndex : Integer; AValue : TReplicaPoolModuleStatus); 

begin
  If (FreplicaPoolModuleStatus=AValue) then exit;
  FreplicaPoolModuleStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModuleStatus.Setstate(AIndex : Integer; AValue : TDeployState); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TModuleStatus.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TModuleStatus.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TNetworkInterface
  --------------------------------------------------------------------}


Procedure TNetworkInterface.SetaccessConfigs(AIndex : Integer; AValue : TNetworkInterfaceTypeaccessConfigsArray); 

begin
  If (FaccessConfigs=AValue) then exit;
  FaccessConfigs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkInterface.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkInterface.Setnetwork(AIndex : Integer; AValue : String); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkInterface.SetnetworkIp(AIndex : Integer; AValue : String); 

begin
  If (FnetworkIp=AValue) then exit;
  FnetworkIp:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TNetworkInterface.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'accessconfigs' : SetLength(FaccessConfigs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TNetworkModule
  --------------------------------------------------------------------}


Procedure TNetworkModule.SetIPv4Range(AIndex : Integer; AValue : String); 

begin
  If (FIPv4Range=AValue) then exit;
  FIPv4Range:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkModule.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkModule.SetgatewayIPv4(AIndex : Integer; AValue : String); 

begin
  If (FgatewayIPv4=AValue) then exit;
  FgatewayIPv4:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNetworkModuleStatus
  --------------------------------------------------------------------}


Procedure TNetworkModuleStatus.SetnetworkUrl(AIndex : Integer; AValue : String); 

begin
  If (FnetworkUrl=AValue) then exit;
  FnetworkUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNewDisk
  --------------------------------------------------------------------}


Procedure TNewDisk.Setattachment(AIndex : Integer; AValue : TDiskAttachment); 

begin
  If (Fattachment=AValue) then exit;
  Fattachment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNewDisk.SetautoDelete(AIndex : Integer; AValue : boolean); 

begin
  If (FautoDelete=AValue) then exit;
  FautoDelete:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNewDisk.Setboot(AIndex : Integer; AValue : boolean); 

begin
  If (Fboot=AValue) then exit;
  Fboot:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNewDisk.SetinitializeParams(AIndex : Integer; AValue : TNewDiskInitializeParams); 

begin
  If (FinitializeParams=AValue) then exit;
  FinitializeParams:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNewDiskInitializeParams
  --------------------------------------------------------------------}


Procedure TNewDiskInitializeParams.SetdiskSizeGb(AIndex : Integer; AValue : String); 

begin
  If (FdiskSizeGb=AValue) then exit;
  FdiskSizeGb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNewDiskInitializeParams.SetdiskType(AIndex : Integer; AValue : String); 

begin
  If (FdiskType=AValue) then exit;
  FdiskType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNewDiskInitializeParams.SetsourceImage(AIndex : Integer; AValue : String); 

begin
  If (FsourceImage=AValue) then exit;
  FsourceImage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TParamOverride
  --------------------------------------------------------------------}


Procedure TParamOverride.Setpath(AIndex : Integer; AValue : String); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParamOverride.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReplicaPoolModuleTypeenvVariables
  --------------------------------------------------------------------}


Class Function TReplicaPoolModuleTypeenvVariables.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TReplicaPoolModule
  --------------------------------------------------------------------}


Procedure TReplicaPoolModule.SetenvVariables(AIndex : Integer; AValue : TReplicaPoolModuleTypeenvVariables); 

begin
  If (FenvVariables=AValue) then exit;
  FenvVariables:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolModule.SethealthChecks(AIndex : Integer; AValue : TStringArray); 

begin
  If (FhealthChecks=AValue) then exit;
  FhealthChecks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolModule.SetnumReplicas(AIndex : Integer; AValue : integer); 

begin
  If (FnumReplicas=AValue) then exit;
  FnumReplicas:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolModule.SetreplicaPoolParams(AIndex : Integer; AValue : TReplicaPoolParams); 

begin
  If (FreplicaPoolParams=AValue) then exit;
  FreplicaPoolParams:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolModule.SetresourceView(AIndex : Integer; AValue : String); 

begin
  If (FresourceView=AValue) then exit;
  FresourceView:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TReplicaPoolModule.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'healthchecks' : SetLength(FhealthChecks,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReplicaPoolModuleStatus
  --------------------------------------------------------------------}


Procedure TReplicaPoolModuleStatus.SetreplicaPoolUrl(AIndex : Integer; AValue : String); 

begin
  If (FreplicaPoolUrl=AValue) then exit;
  FreplicaPoolUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolModuleStatus.SetresourceViewUrl(AIndex : Integer; AValue : String); 

begin
  If (FresourceViewUrl=AValue) then exit;
  FresourceViewUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReplicaPoolParams
  --------------------------------------------------------------------}


Procedure TReplicaPoolParams.Setv1beta1(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1); 

begin
  If (Fv1beta1=AValue) then exit;
  Fv1beta1:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReplicaPoolParamsV1Beta1
  --------------------------------------------------------------------}


Procedure TReplicaPoolParamsV1Beta1.SetautoRestart(AIndex : Integer; AValue : boolean); 

begin
  If (FautoRestart=AValue) then exit;
  FautoRestart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.SetbaseInstanceName(AIndex : Integer; AValue : String); 

begin
  If (FbaseInstanceName=AValue) then exit;
  FbaseInstanceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.SetcanIpForward(AIndex : Integer; AValue : boolean); 

begin
  If (FcanIpForward=AValue) then exit;
  FcanIpForward:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.SetdisksToAttach(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1TypedisksToAttachArray); 

begin
  If (FdisksToAttach=AValue) then exit;
  FdisksToAttach:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.SetdisksToCreate(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1TypedisksToCreateArray); 

begin
  If (FdisksToCreate=AValue) then exit;
  FdisksToCreate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.SetinitAction(AIndex : Integer; AValue : String); 

begin
  If (FinitAction=AValue) then exit;
  FinitAction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.SetmachineType(AIndex : Integer; AValue : String); 

begin
  If (FmachineType=AValue) then exit;
  FmachineType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.Setmetadata(AIndex : Integer; AValue : TMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.SetnetworkInterfaces(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1TypenetworkInterfacesArray); 

begin
  If (FnetworkInterfaces=AValue) then exit;
  FnetworkInterfaces:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.SetonHostMaintenance(AIndex : Integer; AValue : String); 

begin
  If (FonHostMaintenance=AValue) then exit;
  FonHostMaintenance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.SetserviceAccounts(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1TypeserviceAccountsArray); 

begin
  If (FserviceAccounts=AValue) then exit;
  FserviceAccounts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.Settags(AIndex : Integer; AValue : TTag); 

begin
  If (Ftags=AValue) then exit;
  Ftags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.Setzone(AIndex : Integer; AValue : String); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TReplicaPoolParamsV1Beta1.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'diskstoattach' : SetLength(FdisksToAttach,ALength);
  'diskstocreate' : SetLength(FdisksToCreate,ALength);
  'networkinterfaces' : SetLength(FnetworkInterfaces,ALength);
  'serviceaccounts' : SetLength(FserviceAccounts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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
  TTag
  --------------------------------------------------------------------}


Procedure TTag.SetfingerPrint(AIndex : Integer; AValue : String); 

begin
  If (FfingerPrint=AValue) then exit;
  FfingerPrint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Setitems(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTag.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTemplateTypeactions
  --------------------------------------------------------------------}


Class Function TTemplateTypeactions.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TTemplateTypemodules
  --------------------------------------------------------------------}


Class Function TTemplateTypemodules.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TTemplate
  --------------------------------------------------------------------}


Procedure TTemplate.Setactions(AIndex : Integer; AValue : TTemplateTypeactions); 

begin
  If (Factions=AValue) then exit;
  Factions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplate.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplate.Setmodules(AIndex : Integer; AValue : TTemplateTypemodules); 

begin
  If (Fmodules=AValue) then exit;
  Fmodules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplate.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTemplatesListResponse
  --------------------------------------------------------------------}


Procedure TTemplatesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplatesListResponse.Setresources(AIndex : Integer; AValue : TTemplatesListResponseTyperesourcesArray); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTemplatesListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'resources' : SetLength(Fresources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDeploymentsResource
  --------------------------------------------------------------------}


Class Function TDeploymentsResource.ResourceName : String;

begin
  Result:='deployments';
end;

Class Function TDeploymentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmanagerAPI;
end;

Procedure TDeploymentsResource.Delete(deploymentName: string; projectId: string; region: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{projectId}/regions/{region}/deployments/{deploymentName}';
  _Methodid   = 'manager.deployments.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deploymentName',deploymentName,'projectId',projectId,'region',region]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TDeploymentsResource.Get(deploymentName: string; projectId: string; region: string) : TDeployment;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/regions/{region}/deployments/{deploymentName}';
  _Methodid   = 'manager.deployments.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['deploymentName',deploymentName,'projectId',projectId,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDeployment) as TDeployment;
end;

Function TDeploymentsResource.Insert(projectId: string; region: string; aDeployment : TDeployment) : TDeployment;

Const
  _HTTPMethod = 'POST';
  _Path       = '{projectId}/regions/{region}/deployments';
  _Methodid   = 'manager.deployments.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDeployment,TDeployment) as TDeployment;
end;

Function TDeploymentsResource.List(projectId: string; region: string; AQuery : string = '') : TDeploymentsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/regions/{region}/deployments';
  _Methodid   = 'manager.deployments.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'region',region]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TDeploymentsListResponse) as TDeploymentsListResponse;
end;


Function TDeploymentsResource.List(projectId: string; region: string; AQuery : TDeploymentslistOptions) : TDeploymentsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(projectId,region,_Q);
end;



{ --------------------------------------------------------------------
  TTemplatesResource
  --------------------------------------------------------------------}


Class Function TTemplatesResource.ResourceName : String;

begin
  Result:='templates';
end;

Class Function TTemplatesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TmanagerAPI;
end;

Procedure TTemplatesResource.Delete(projectId: string; templateName: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{projectId}/templates/{templateName}';
  _Methodid   = 'manager.templates.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'templateName',templateName]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TTemplatesResource.Get(projectId: string; templateName: string) : TTemplate;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/templates/{templateName}';
  _Methodid   = 'manager.templates.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId,'templateName',templateName]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TTemplate) as TTemplate;
end;

Function TTemplatesResource.Insert(projectId: string; aTemplate : TTemplate) : TTemplate;

Const
  _HTTPMethod = 'POST';
  _Path       = '{projectId}/templates';
  _Methodid   = 'manager.templates.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTemplate,TTemplate) as TTemplate;
end;

Function TTemplatesResource.List(projectId: string; AQuery : string = '') : TTemplatesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{projectId}/templates';
  _Methodid   = 'manager.templates.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TTemplatesListResponse) as TTemplatesListResponse;
end;


Function TTemplatesResource.List(projectId: string; AQuery : TTemplateslistOptions) : TTemplatesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(projectId,_Q);
end;



{ --------------------------------------------------------------------
  TManagerAPI
  --------------------------------------------------------------------}

Class Function TManagerAPI.APIName : String;

begin
  Result:='manager';
end;

Class Function TManagerAPI.APIVersion : String;

begin
  Result:='v1beta2';
end;

Class Function TManagerAPI.APIRevision : String;

begin
  Result:='20140915';
end;

Class Function TManagerAPI.APIID : String;

begin
  Result:='manager:v1beta2';
end;

Class Function TManagerAPI.APITitle : String;

begin
  Result:='Deployment Manager API';
end;

Class Function TManagerAPI.APIDescription : String;

begin
  Result:='The Deployment Manager API allows users to declaratively configure, deploy and run complex solutions on the Google Cloud Platform.';
end;

Class Function TManagerAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TManagerAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TManagerAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TManagerAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TManagerAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/deployment-manager/';
end;

Class Function TManagerAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TManagerAPI.APIbasePath : string;

begin
  Result:='/manager/v1beta2/projects/';
end;

Class Function TManagerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/manager/v1beta2/projects/';
end;

Class Function TManagerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TManagerAPI.APIservicePath : string;

begin
  Result:='manager/v1beta2/projects/';
end;

Class Function TManagerAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TManagerAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,6);
  Result[0].Name:='https://www.googleapis.com/auth/appengine.admin';
  Result[0].Description:='View and manage your applications deployed on Google App Engine';
  Result[1].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[1].Description:='View and manage your data across Google Cloud Platform services';
  Result[2].Name:='https://www.googleapis.com/auth/compute';
  Result[2].Description:='View and manage your Google Compute Engine resources';
  Result[3].Name:='https://www.googleapis.com/auth/devstorage.read_write';
  Result[3].Description:='Manage your data in Google Cloud Storage';
  Result[4].Name:='https://www.googleapis.com/auth/ndev.cloudman';
  Result[4].Description:='View and manage your Google Cloud Platform management resources and deployment status information';
  Result[5].Name:='https://www.googleapis.com/auth/ndev.cloudman.readonly';
  Result[5].Description:='View your Google Cloud Platform management resources and deployment status information';
  
end;

Class Function TManagerAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TManagerAPI.RegisterAPIResources;

begin
  TAccessConfig.RegisterObject;
  TAction.RegisterObject;
  TAllowedRule.RegisterObject;
  TAutoscalingModule.RegisterObject;
  TAutoscalingModuleStatus.RegisterObject;
  TDeployState.RegisterObject;
  TDeploymentTypemodules.RegisterObject;
  TDeployment.RegisterObject;
  TDeploymentsListResponse.RegisterObject;
  TDiskAttachment.RegisterObject;
  TEnvVariable.RegisterObject;
  TExistingDisk.RegisterObject;
  TFirewallModule.RegisterObject;
  TFirewallModuleStatus.RegisterObject;
  THealthCheckModule.RegisterObject;
  THealthCheckModuleStatus.RegisterObject;
  TLbModule.RegisterObject;
  TLbModuleStatus.RegisterObject;
  TMetadata.RegisterObject;
  TMetadataItem.RegisterObject;
  TModule.RegisterObject;
  TModuleStatus.RegisterObject;
  TNetworkInterface.RegisterObject;
  TNetworkModule.RegisterObject;
  TNetworkModuleStatus.RegisterObject;
  TNewDisk.RegisterObject;
  TNewDiskInitializeParams.RegisterObject;
  TParamOverride.RegisterObject;
  TReplicaPoolModuleTypeenvVariables.RegisterObject;
  TReplicaPoolModule.RegisterObject;
  TReplicaPoolModuleStatus.RegisterObject;
  TReplicaPoolParams.RegisterObject;
  TReplicaPoolParamsV1Beta1.RegisterObject;
  TServiceAccount.RegisterObject;
  TTag.RegisterObject;
  TTemplateTypeactions.RegisterObject;
  TTemplateTypemodules.RegisterObject;
  TTemplate.RegisterObject;
  TTemplatesListResponse.RegisterObject;
end;


Function TManagerAPI.GetDeploymentsInstance : TDeploymentsResource;

begin
  if (FDeploymentsInstance=Nil) then
    FDeploymentsInstance:=CreateDeploymentsResource;
  Result:=FDeploymentsInstance;
end;

Function TManagerAPI.CreateDeploymentsResource : TDeploymentsResource;

begin
  Result:=CreateDeploymentsResource(Self);
end;


Function TManagerAPI.CreateDeploymentsResource(AOwner : TComponent) : TDeploymentsResource;

begin
  Result:=TDeploymentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TManagerAPI.GetTemplatesInstance : TTemplatesResource;

begin
  if (FTemplatesInstance=Nil) then
    FTemplatesInstance:=CreateTemplatesResource;
  Result:=FTemplatesInstance;
end;

Function TManagerAPI.CreateTemplatesResource : TTemplatesResource;

begin
  Result:=CreateTemplatesResource(Self);
end;


Function TManagerAPI.CreateTemplatesResource(AOwner : TComponent) : TTemplatesResource;

begin
  Result:=TTemplatesResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TManagerAPI.RegisterAPI;
end.
