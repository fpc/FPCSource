unit googlemanager;
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
  TAccessConfig = class;
  TAccessConfigArray = Array of TAccessConfig;
  TAction = class;
  TActionArray = Array of TAction;
  TActioncommands = class;
  TActioncommandsArray = Array of TActioncommands;
  TAllowedRule = class;
  TAllowedRuleArray = Array of TAllowedRule;
  TAllowedRuleports = class;
  TAllowedRuleportsArray = Array of TAllowedRuleports;
  TAutoscalingModule = class;
  TAutoscalingModuleArray = Array of TAutoscalingModule;
  TAutoscalingModuleStatus = class;
  TAutoscalingModuleStatusArray = Array of TAutoscalingModuleStatus;
  TDeployState = class;
  TDeployStateArray = Array of TDeployState;
  TDeployment = class;
  TDeploymentArray = Array of TDeployment;
  TDeploymentmodules = class;
  TDeploymentmodulesArray = Array of TDeploymentmodules;
  TDeploymentoverrides = class;
  TDeploymentoverridesArray = Array of TDeploymentoverrides;
  TDeploymentsListResponse = class;
  TDeploymentsListResponseArray = Array of TDeploymentsListResponse;
  TDeploymentsListResponseresources = class;
  TDeploymentsListResponseresourcesArray = Array of TDeploymentsListResponseresources;
  TDiskAttachment = class;
  TDiskAttachmentArray = Array of TDiskAttachment;
  TEnvVariable = class;
  TEnvVariableArray = Array of TEnvVariable;
  TExistingDisk = class;
  TExistingDiskArray = Array of TExistingDisk;
  TFirewallModule = class;
  TFirewallModuleArray = Array of TFirewallModule;
  TFirewallModuleallowed = class;
  TFirewallModuleallowedArray = Array of TFirewallModuleallowed;
  TFirewallModulesourceRanges = class;
  TFirewallModulesourceRangesArray = Array of TFirewallModulesourceRanges;
  TFirewallModulesourceTags = class;
  TFirewallModulesourceTagsArray = Array of TFirewallModulesourceTags;
  TFirewallModuletargetTags = class;
  TFirewallModuletargetTagsArray = Array of TFirewallModuletargetTags;
  TFirewallModuleStatus = class;
  TFirewallModuleStatusArray = Array of TFirewallModuleStatus;
  THealthCheckModule = class;
  THealthCheckModuleArray = Array of THealthCheckModule;
  THealthCheckModuleStatus = class;
  THealthCheckModuleStatusArray = Array of THealthCheckModuleStatus;
  TLbModule = class;
  TLbModuleArray = Array of TLbModule;
  TLbModulehealthChecks = class;
  TLbModulehealthChecksArray = Array of TLbModulehealthChecks;
  TLbModuletargetModules = class;
  TLbModuletargetModulesArray = Array of TLbModuletargetModules;
  TLbModuleStatus = class;
  TLbModuleStatusArray = Array of TLbModuleStatus;
  TMetadata = class;
  TMetadataArray = Array of TMetadata;
  TMetadataitems = class;
  TMetadataitemsArray = Array of TMetadataitems;
  TMetadataItem = class;
  TMetadataItemArray = Array of TMetadataItem;
  TModule = class;
  TModuleArray = Array of TModule;
  TModuleStatus = class;
  TModuleStatusArray = Array of TModuleStatus;
  TNetworkInterface = class;
  TNetworkInterfaceArray = Array of TNetworkInterface;
  TNetworkInterfaceaccessConfigs = class;
  TNetworkInterfaceaccessConfigsArray = Array of TNetworkInterfaceaccessConfigs;
  TNetworkModule = class;
  TNetworkModuleArray = Array of TNetworkModule;
  TNetworkModuleStatus = class;
  TNetworkModuleStatusArray = Array of TNetworkModuleStatus;
  TNewDisk = class;
  TNewDiskArray = Array of TNewDisk;
  TNewDiskInitializeParams = class;
  TNewDiskInitializeParamsArray = Array of TNewDiskInitializeParams;
  TParamOverride = class;
  TParamOverrideArray = Array of TParamOverride;
  TReplicaPoolModule = class;
  TReplicaPoolModuleArray = Array of TReplicaPoolModule;
  TReplicaPoolModuleenvVariables = class;
  TReplicaPoolModuleenvVariablesArray = Array of TReplicaPoolModuleenvVariables;
  TReplicaPoolModulehealthChecks = class;
  TReplicaPoolModulehealthChecksArray = Array of TReplicaPoolModulehealthChecks;
  TReplicaPoolModuleStatus = class;
  TReplicaPoolModuleStatusArray = Array of TReplicaPoolModuleStatus;
  TReplicaPoolParams = class;
  TReplicaPoolParamsArray = Array of TReplicaPoolParams;
  TReplicaPoolParamsV1Beta1 = class;
  TReplicaPoolParamsV1Beta1Array = Array of TReplicaPoolParamsV1Beta1;
  TReplicaPoolParamsV1Beta1disksToAttach = class;
  TReplicaPoolParamsV1Beta1disksToAttachArray = Array of TReplicaPoolParamsV1Beta1disksToAttach;
  TReplicaPoolParamsV1Beta1disksToCreate = class;
  TReplicaPoolParamsV1Beta1disksToCreateArray = Array of TReplicaPoolParamsV1Beta1disksToCreate;
  TReplicaPoolParamsV1Beta1networkInterfaces = class;
  TReplicaPoolParamsV1Beta1networkInterfacesArray = Array of TReplicaPoolParamsV1Beta1networkInterfaces;
  TReplicaPoolParamsV1Beta1serviceAccounts = class;
  TReplicaPoolParamsV1Beta1serviceAccountsArray = Array of TReplicaPoolParamsV1Beta1serviceAccounts;
  TServiceAccount = class;
  TServiceAccountArray = Array of TServiceAccount;
  TServiceAccountscopes = class;
  TServiceAccountscopesArray = Array of TServiceAccountscopes;
  TTag = class;
  TTagArray = Array of TTag;
  TTagitems = class;
  TTagitemsArray = Array of TTagitems;
  TTemplate = class;
  TTemplateArray = Array of TTemplate;
  TTemplateactions = class;
  TTemplateactionsArray = Array of TTemplateactions;
  TTemplatemodules = class;
  TTemplatemodulesArray = Array of TTemplatemodules;
  TTemplatesListResponse = class;
  TTemplatesListResponseArray = Array of TTemplatesListResponse;
  TTemplatesListResponseresources = class;
  TTemplatesListResponseresourcesArray = Array of TTemplatesListResponseresources;
  
  { --------------------------------------------------------------------
    TAccessConfig
    --------------------------------------------------------------------}
  
  TAccessConfig = Class(TGoogleBaseObject)
  Private
    Fname : string;
    FnatIp : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetnatIp(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property natIp : string Index 8 Read FnatIp Write SetnatIp;
    Property _type : string Index 16 Read F_type Write Set_type;
  end;
  TAccessConfigClass = Class of TAccessConfig;
  
  { --------------------------------------------------------------------
    TAction
    --------------------------------------------------------------------}
  
  TAction = Class(TGoogleBaseObject)
  Private
    Fcommands : TActioncommands;
    FtimeoutMs : integer;
  Protected
    //Property setters
    Procedure Setcommands(AIndex : Integer; AValue : TActioncommands); virtual;
    Procedure SettimeoutMs(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property commands : TActioncommands Index 0 Read Fcommands Write Setcommands;
    Property timeoutMs : integer Index 8 Read FtimeoutMs Write SettimeoutMs;
  end;
  TActionClass = Class of TAction;
  
  { --------------------------------------------------------------------
    TActioncommands
    --------------------------------------------------------------------}
  
  TActioncommands = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TActioncommandsClass = Class of TActioncommands;
  
  { --------------------------------------------------------------------
    TAllowedRule
    --------------------------------------------------------------------}
  
  TAllowedRule = Class(TGoogleBaseObject)
  Private
    FIPProtocol : string;
    Fports : TAllowedRuleports;
  Protected
    //Property setters
    Procedure SetIPProtocol(AIndex : Integer; AValue : string); virtual;
    Procedure Setports(AIndex : Integer; AValue : TAllowedRuleports); virtual;
  Public
  Published
    Property IPProtocol : string Index 0 Read FIPProtocol Write SetIPProtocol;
    Property ports : TAllowedRuleports Index 8 Read Fports Write Setports;
  end;
  TAllowedRuleClass = Class of TAllowedRule;
  
  { --------------------------------------------------------------------
    TAllowedRuleports
    --------------------------------------------------------------------}
  
  TAllowedRuleports = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAllowedRuleportsClass = Class of TAllowedRuleports;
  
  { --------------------------------------------------------------------
    TAutoscalingModule
    --------------------------------------------------------------------}
  
  TAutoscalingModule = Class(TGoogleBaseObject)
  Private
    FcoolDownPeriodSec : integer;
    Fdescription : string;
    FmaxNumReplicas : integer;
    FminNumReplicas : integer;
    FsignalType : string;
    FtargetModule : string;
    FtargetUtilization : double;
  Protected
    //Property setters
    Procedure SetcoolDownPeriodSec(AIndex : Integer; AValue : integer); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaxNumReplicas(AIndex : Integer; AValue : integer); virtual;
    Procedure SetminNumReplicas(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsignalType(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetModule(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetUtilization(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property coolDownPeriodSec : integer Index 0 Read FcoolDownPeriodSec Write SetcoolDownPeriodSec;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property maxNumReplicas : integer Index 16 Read FmaxNumReplicas Write SetmaxNumReplicas;
    Property minNumReplicas : integer Index 24 Read FminNumReplicas Write SetminNumReplicas;
    Property signalType : string Index 32 Read FsignalType Write SetsignalType;
    Property targetModule : string Index 40 Read FtargetModule Write SettargetModule;
    Property targetUtilization : double Index 48 Read FtargetUtilization Write SettargetUtilization;
  end;
  TAutoscalingModuleClass = Class of TAutoscalingModule;
  
  { --------------------------------------------------------------------
    TAutoscalingModuleStatus
    --------------------------------------------------------------------}
  
  TAutoscalingModuleStatus = Class(TGoogleBaseObject)
  Private
    FautoscalingConfigUrl : string;
  Protected
    //Property setters
    Procedure SetautoscalingConfigUrl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property autoscalingConfigUrl : string Index 0 Read FautoscalingConfigUrl Write SetautoscalingConfigUrl;
  end;
  TAutoscalingModuleStatusClass = Class of TAutoscalingModuleStatus;
  
  { --------------------------------------------------------------------
    TDeployState
    --------------------------------------------------------------------}
  
  TDeployState = Class(TGoogleBaseObject)
  Private
    Fdetails : string;
    Fstatus : string;
  Protected
    //Property setters
    Procedure Setdetails(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property details : string Index 0 Read Fdetails Write Setdetails;
    Property status : string Index 8 Read Fstatus Write Setstatus;
  end;
  TDeployStateClass = Class of TDeployState;
  
  { --------------------------------------------------------------------
    TDeployment
    --------------------------------------------------------------------}
  
  TDeployment = Class(TGoogleBaseObject)
  Private
    FcreationDate : string;
    Fdescription : string;
    Fmodules : TDeploymentmodules;
    Fname : string;
    Foverrides : TDeploymentoverrides;
    Fstate : TDeployState;
    FtemplateName : string;
  Protected
    //Property setters
    Procedure SetcreationDate(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setmodules(AIndex : Integer; AValue : TDeploymentmodules); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setoverrides(AIndex : Integer; AValue : TDeploymentoverrides); virtual;
    Procedure Setstate(AIndex : Integer; AValue : TDeployState); virtual;
    Procedure SettemplateName(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property creationDate : string Index 0 Read FcreationDate Write SetcreationDate;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property modules : TDeploymentmodules Index 16 Read Fmodules Write Setmodules;
    Property name : string Index 24 Read Fname Write Setname;
    Property overrides : TDeploymentoverrides Index 32 Read Foverrides Write Setoverrides;
    Property state : TDeployState Index 40 Read Fstate Write Setstate;
    Property templateName : string Index 48 Read FtemplateName Write SettemplateName;
  end;
  TDeploymentClass = Class of TDeployment;
  
  { --------------------------------------------------------------------
    TDeploymentmodules
    --------------------------------------------------------------------}
  
  TDeploymentmodules = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TDeploymentmodulesClass = Class of TDeploymentmodules;
  
  { --------------------------------------------------------------------
    TDeploymentoverrides
    --------------------------------------------------------------------}
  
  TDeploymentoverrides = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDeploymentoverridesClass = Class of TDeploymentoverrides;
  
  { --------------------------------------------------------------------
    TDeploymentsListResponse
    --------------------------------------------------------------------}
  
  TDeploymentsListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    Fresources : TDeploymentsListResponseresources;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TDeploymentsListResponseresources); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property resources : TDeploymentsListResponseresources Index 8 Read Fresources Write Setresources;
  end;
  TDeploymentsListResponseClass = Class of TDeploymentsListResponse;
  
  { --------------------------------------------------------------------
    TDeploymentsListResponseresources
    --------------------------------------------------------------------}
  
  TDeploymentsListResponseresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDeploymentsListResponseresourcesClass = Class of TDeploymentsListResponseresources;
  
  { --------------------------------------------------------------------
    TDiskAttachment
    --------------------------------------------------------------------}
  
  TDiskAttachment = Class(TGoogleBaseObject)
  Private
    FdeviceName : string;
    Findex : integer;
  Protected
    //Property setters
    Procedure SetdeviceName(AIndex : Integer; AValue : string); virtual;
    Procedure Setindex(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property deviceName : string Index 0 Read FdeviceName Write SetdeviceName;
    Property index : integer Index 8 Read Findex Write Setindex;
  end;
  TDiskAttachmentClass = Class of TDiskAttachment;
  
  { --------------------------------------------------------------------
    TEnvVariable
    --------------------------------------------------------------------}
  
  TEnvVariable = Class(TGoogleBaseObject)
  Private
    Fhidden : boolean;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Sethidden(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property hidden : boolean Index 0 Read Fhidden Write Sethidden;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TEnvVariableClass = Class of TEnvVariable;
  
  { --------------------------------------------------------------------
    TExistingDisk
    --------------------------------------------------------------------}
  
  TExistingDisk = Class(TGoogleBaseObject)
  Private
    Fattachment : TDiskAttachment;
    Fsource : string;
  Protected
    //Property setters
    Procedure Setattachment(AIndex : Integer; AValue : TDiskAttachment); virtual;
    Procedure Setsource(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property attachment : TDiskAttachment Index 0 Read Fattachment Write Setattachment;
    Property source : string Index 8 Read Fsource Write Setsource;
  end;
  TExistingDiskClass = Class of TExistingDisk;
  
  { --------------------------------------------------------------------
    TFirewallModule
    --------------------------------------------------------------------}
  
  TFirewallModule = Class(TGoogleBaseObject)
  Private
    Fallowed : TFirewallModuleallowed;
    Fdescription : string;
    Fnetwork : string;
    FsourceRanges : TFirewallModulesourceRanges;
    FsourceTags : TFirewallModulesourceTags;
    FtargetTags : TFirewallModuletargetTags;
  Protected
    //Property setters
    Procedure Setallowed(AIndex : Integer; AValue : TFirewallModuleallowed); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setnetwork(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceRanges(AIndex : Integer; AValue : TFirewallModulesourceRanges); virtual;
    Procedure SetsourceTags(AIndex : Integer; AValue : TFirewallModulesourceTags); virtual;
    Procedure SettargetTags(AIndex : Integer; AValue : TFirewallModuletargetTags); virtual;
  Public
  Published
    Property allowed : TFirewallModuleallowed Index 0 Read Fallowed Write Setallowed;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property network : string Index 16 Read Fnetwork Write Setnetwork;
    Property sourceRanges : TFirewallModulesourceRanges Index 24 Read FsourceRanges Write SetsourceRanges;
    Property sourceTags : TFirewallModulesourceTags Index 32 Read FsourceTags Write SetsourceTags;
    Property targetTags : TFirewallModuletargetTags Index 40 Read FtargetTags Write SettargetTags;
  end;
  TFirewallModuleClass = Class of TFirewallModule;
  
  { --------------------------------------------------------------------
    TFirewallModuleallowed
    --------------------------------------------------------------------}
  
  TFirewallModuleallowed = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFirewallModuleallowedClass = Class of TFirewallModuleallowed;
  
  { --------------------------------------------------------------------
    TFirewallModulesourceRanges
    --------------------------------------------------------------------}
  
  TFirewallModulesourceRanges = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFirewallModulesourceRangesClass = Class of TFirewallModulesourceRanges;
  
  { --------------------------------------------------------------------
    TFirewallModulesourceTags
    --------------------------------------------------------------------}
  
  TFirewallModulesourceTags = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFirewallModulesourceTagsClass = Class of TFirewallModulesourceTags;
  
  { --------------------------------------------------------------------
    TFirewallModuletargetTags
    --------------------------------------------------------------------}
  
  TFirewallModuletargetTags = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFirewallModuletargetTagsClass = Class of TFirewallModuletargetTags;
  
  { --------------------------------------------------------------------
    TFirewallModuleStatus
    --------------------------------------------------------------------}
  
  TFirewallModuleStatus = Class(TGoogleBaseObject)
  Private
    FfirewallUrl : string;
  Protected
    //Property setters
    Procedure SetfirewallUrl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property firewallUrl : string Index 0 Read FfirewallUrl Write SetfirewallUrl;
  end;
  TFirewallModuleStatusClass = Class of TFirewallModuleStatus;
  
  { --------------------------------------------------------------------
    THealthCheckModule
    --------------------------------------------------------------------}
  
  THealthCheckModule = Class(TGoogleBaseObject)
  Private
    FcheckIntervalSec : integer;
    Fdescription : string;
    FhealthyThreshold : integer;
    Fhost : string;
    Fpath : string;
    Fport : integer;
    FtimeoutSec : integer;
    FunhealthyThreshold : integer;
  Protected
    //Property setters
    Procedure SetcheckIntervalSec(AIndex : Integer; AValue : integer); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SethealthyThreshold(AIndex : Integer; AValue : integer); virtual;
    Procedure Sethost(AIndex : Integer; AValue : string); virtual;
    Procedure Setpath(AIndex : Integer; AValue : string); virtual;
    Procedure Setport(AIndex : Integer; AValue : integer); virtual;
    Procedure SettimeoutSec(AIndex : Integer; AValue : integer); virtual;
    Procedure SetunhealthyThreshold(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property checkIntervalSec : integer Index 0 Read FcheckIntervalSec Write SetcheckIntervalSec;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property healthyThreshold : integer Index 16 Read FhealthyThreshold Write SethealthyThreshold;
    Property host : string Index 24 Read Fhost Write Sethost;
    Property path : string Index 32 Read Fpath Write Setpath;
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
    FhealthCheckUrl : string;
  Protected
    //Property setters
    Procedure SethealthCheckUrl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property healthCheckUrl : string Index 0 Read FhealthCheckUrl Write SethealthCheckUrl;
  end;
  THealthCheckModuleStatusClass = Class of THealthCheckModuleStatus;
  
  { --------------------------------------------------------------------
    TLbModule
    --------------------------------------------------------------------}
  
  TLbModule = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    FhealthChecks : TLbModulehealthChecks;
    FipAddress : string;
    FipProtocol : string;
    FportRange : string;
    FsessionAffinity : string;
    FtargetModules : TLbModuletargetModules;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SethealthChecks(AIndex : Integer; AValue : TLbModulehealthChecks); virtual;
    Procedure SetipAddress(AIndex : Integer; AValue : string); virtual;
    Procedure SetipProtocol(AIndex : Integer; AValue : string); virtual;
    Procedure SetportRange(AIndex : Integer; AValue : string); virtual;
    Procedure SetsessionAffinity(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetModules(AIndex : Integer; AValue : TLbModuletargetModules); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property healthChecks : TLbModulehealthChecks Index 8 Read FhealthChecks Write SethealthChecks;
    Property ipAddress : string Index 16 Read FipAddress Write SetipAddress;
    Property ipProtocol : string Index 24 Read FipProtocol Write SetipProtocol;
    Property portRange : string Index 32 Read FportRange Write SetportRange;
    Property sessionAffinity : string Index 40 Read FsessionAffinity Write SetsessionAffinity;
    Property targetModules : TLbModuletargetModules Index 48 Read FtargetModules Write SettargetModules;
  end;
  TLbModuleClass = Class of TLbModule;
  
  { --------------------------------------------------------------------
    TLbModulehealthChecks
    --------------------------------------------------------------------}
  
  TLbModulehealthChecks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLbModulehealthChecksClass = Class of TLbModulehealthChecks;
  
  { --------------------------------------------------------------------
    TLbModuletargetModules
    --------------------------------------------------------------------}
  
  TLbModuletargetModules = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLbModuletargetModulesClass = Class of TLbModuletargetModules;
  
  { --------------------------------------------------------------------
    TLbModuleStatus
    --------------------------------------------------------------------}
  
  TLbModuleStatus = Class(TGoogleBaseObject)
  Private
    FforwardingRuleUrl : string;
    FtargetPoolUrl : string;
  Protected
    //Property setters
    Procedure SetforwardingRuleUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetPoolUrl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property forwardingRuleUrl : string Index 0 Read FforwardingRuleUrl Write SetforwardingRuleUrl;
    Property targetPoolUrl : string Index 8 Read FtargetPoolUrl Write SettargetPoolUrl;
  end;
  TLbModuleStatusClass = Class of TLbModuleStatus;
  
  { --------------------------------------------------------------------
    TMetadata
    --------------------------------------------------------------------}
  
  TMetadata = Class(TGoogleBaseObject)
  Private
    FfingerPrint : string;
    Fitems : TMetadataitems;
  Protected
    //Property setters
    Procedure SetfingerPrint(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TMetadataitems); virtual;
  Public
  Published
    Property fingerPrint : string Index 0 Read FfingerPrint Write SetfingerPrint;
    Property items : TMetadataitems Index 8 Read Fitems Write Setitems;
  end;
  TMetadataClass = Class of TMetadata;
  
  { --------------------------------------------------------------------
    TMetadataitems
    --------------------------------------------------------------------}
  
  TMetadataitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMetadataitemsClass = Class of TMetadataitems;
  
  { --------------------------------------------------------------------
    TMetadataItem
    --------------------------------------------------------------------}
  
  TMetadataItem = Class(TGoogleBaseObject)
  Private
    Fkey : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property key : string Index 0 Read Fkey Write Setkey;
    Property value : string Index 8 Read Fvalue Write Setvalue;
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
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetautoscalingModule(AIndex : Integer; AValue : TAutoscalingModule); virtual;
    Procedure SetfirewallModule(AIndex : Integer; AValue : TFirewallModule); virtual;
    Procedure SethealthCheckModule(AIndex : Integer; AValue : THealthCheckModule); virtual;
    Procedure SetlbModule(AIndex : Integer; AValue : TLbModule); virtual;
    Procedure SetnetworkModule(AIndex : Integer; AValue : TNetworkModule); virtual;
    Procedure SetreplicaPoolModule(AIndex : Integer; AValue : TReplicaPoolModule); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property autoscalingModule : TAutoscalingModule Index 0 Read FautoscalingModule Write SetautoscalingModule;
    Property firewallModule : TFirewallModule Index 8 Read FfirewallModule Write SetfirewallModule;
    Property healthCheckModule : THealthCheckModule Index 16 Read FhealthCheckModule Write SethealthCheckModule;
    Property lbModule : TLbModule Index 24 Read FlbModule Write SetlbModule;
    Property networkModule : TNetworkModule Index 32 Read FnetworkModule Write SetnetworkModule;
    Property replicaPoolModule : TReplicaPoolModule Index 40 Read FreplicaPoolModule Write SetreplicaPoolModule;
    Property _type : string Index 48 Read F_type Write Set_type;
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
    F_type : string;
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
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property autoscalingModuleStatus : TAutoscalingModuleStatus Index 0 Read FautoscalingModuleStatus Write SetautoscalingModuleStatus;
    Property firewallModuleStatus : TFirewallModuleStatus Index 8 Read FfirewallModuleStatus Write SetfirewallModuleStatus;
    Property healthCheckModuleStatus : THealthCheckModuleStatus Index 16 Read FhealthCheckModuleStatus Write SethealthCheckModuleStatus;
    Property lbModuleStatus : TLbModuleStatus Index 24 Read FlbModuleStatus Write SetlbModuleStatus;
    Property networkModuleStatus : TNetworkModuleStatus Index 32 Read FnetworkModuleStatus Write SetnetworkModuleStatus;
    Property replicaPoolModuleStatus : TReplicaPoolModuleStatus Index 40 Read FreplicaPoolModuleStatus Write SetreplicaPoolModuleStatus;
    Property state : TDeployState Index 48 Read Fstate Write Setstate;
    Property _type : string Index 56 Read F_type Write Set_type;
  end;
  TModuleStatusClass = Class of TModuleStatus;
  
  { --------------------------------------------------------------------
    TNetworkInterface
    --------------------------------------------------------------------}
  
  TNetworkInterface = Class(TGoogleBaseObject)
  Private
    FaccessConfigs : TNetworkInterfaceaccessConfigs;
    Fname : string;
    Fnetwork : string;
    FnetworkIp : string;
  Protected
    //Property setters
    Procedure SetaccessConfigs(AIndex : Integer; AValue : TNetworkInterfaceaccessConfigs); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setnetwork(AIndex : Integer; AValue : string); virtual;
    Procedure SetnetworkIp(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accessConfigs : TNetworkInterfaceaccessConfigs Index 0 Read FaccessConfigs Write SetaccessConfigs;
    Property name : string Index 8 Read Fname Write Setname;
    Property network : string Index 16 Read Fnetwork Write Setnetwork;
    Property networkIp : string Index 24 Read FnetworkIp Write SetnetworkIp;
  end;
  TNetworkInterfaceClass = Class of TNetworkInterface;
  
  { --------------------------------------------------------------------
    TNetworkInterfaceaccessConfigs
    --------------------------------------------------------------------}
  
  TNetworkInterfaceaccessConfigs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TNetworkInterfaceaccessConfigsClass = Class of TNetworkInterfaceaccessConfigs;
  
  { --------------------------------------------------------------------
    TNetworkModule
    --------------------------------------------------------------------}
  
  TNetworkModule = Class(TGoogleBaseObject)
  Private
    FIPv4Range : string;
    Fdescription : string;
    FgatewayIPv4 : string;
  Protected
    //Property setters
    Procedure SetIPv4Range(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetgatewayIPv4(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property IPv4Range : string Index 0 Read FIPv4Range Write SetIPv4Range;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property gatewayIPv4 : string Index 16 Read FgatewayIPv4 Write SetgatewayIPv4;
  end;
  TNetworkModuleClass = Class of TNetworkModule;
  
  { --------------------------------------------------------------------
    TNetworkModuleStatus
    --------------------------------------------------------------------}
  
  TNetworkModuleStatus = Class(TGoogleBaseObject)
  Private
    FnetworkUrl : string;
  Protected
    //Property setters
    Procedure SetnetworkUrl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property networkUrl : string Index 0 Read FnetworkUrl Write SetnetworkUrl;
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
    FdiskSizeGb : string;
    FdiskType : string;
    FsourceImage : string;
  Protected
    //Property setters
    Procedure SetdiskSizeGb(AIndex : Integer; AValue : string); virtual;
    Procedure SetdiskType(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceImage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property diskSizeGb : string Index 0 Read FdiskSizeGb Write SetdiskSizeGb;
    Property diskType : string Index 8 Read FdiskType Write SetdiskType;
    Property sourceImage : string Index 16 Read FsourceImage Write SetsourceImage;
  end;
  TNewDiskInitializeParamsClass = Class of TNewDiskInitializeParams;
  
  { --------------------------------------------------------------------
    TParamOverride
    --------------------------------------------------------------------}
  
  TParamOverride = Class(TGoogleBaseObject)
  Private
    Fpath : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setpath(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property path : string Index 0 Read Fpath Write Setpath;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TParamOverrideClass = Class of TParamOverride;
  
  { --------------------------------------------------------------------
    TReplicaPoolModule
    --------------------------------------------------------------------}
  
  TReplicaPoolModule = Class(TGoogleBaseObject)
  Private
    FenvVariables : TReplicaPoolModuleenvVariables;
    FhealthChecks : TReplicaPoolModulehealthChecks;
    FnumReplicas : integer;
    FreplicaPoolParams : TReplicaPoolParams;
    FresourceView : string;
  Protected
    //Property setters
    Procedure SetenvVariables(AIndex : Integer; AValue : TReplicaPoolModuleenvVariables); virtual;
    Procedure SethealthChecks(AIndex : Integer; AValue : TReplicaPoolModulehealthChecks); virtual;
    Procedure SetnumReplicas(AIndex : Integer; AValue : integer); virtual;
    Procedure SetreplicaPoolParams(AIndex : Integer; AValue : TReplicaPoolParams); virtual;
    Procedure SetresourceView(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property envVariables : TReplicaPoolModuleenvVariables Index 0 Read FenvVariables Write SetenvVariables;
    Property healthChecks : TReplicaPoolModulehealthChecks Index 8 Read FhealthChecks Write SethealthChecks;
    Property numReplicas : integer Index 16 Read FnumReplicas Write SetnumReplicas;
    Property replicaPoolParams : TReplicaPoolParams Index 24 Read FreplicaPoolParams Write SetreplicaPoolParams;
    Property resourceView : string Index 32 Read FresourceView Write SetresourceView;
  end;
  TReplicaPoolModuleClass = Class of TReplicaPoolModule;
  
  { --------------------------------------------------------------------
    TReplicaPoolModuleenvVariables
    --------------------------------------------------------------------}
  
  TReplicaPoolModuleenvVariables = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TReplicaPoolModuleenvVariablesClass = Class of TReplicaPoolModuleenvVariables;
  
  { --------------------------------------------------------------------
    TReplicaPoolModulehealthChecks
    --------------------------------------------------------------------}
  
  TReplicaPoolModulehealthChecks = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReplicaPoolModulehealthChecksClass = Class of TReplicaPoolModulehealthChecks;
  
  { --------------------------------------------------------------------
    TReplicaPoolModuleStatus
    --------------------------------------------------------------------}
  
  TReplicaPoolModuleStatus = Class(TGoogleBaseObject)
  Private
    FreplicaPoolUrl : string;
    FresourceViewUrl : string;
  Protected
    //Property setters
    Procedure SetreplicaPoolUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SetresourceViewUrl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property replicaPoolUrl : string Index 0 Read FreplicaPoolUrl Write SetreplicaPoolUrl;
    Property resourceViewUrl : string Index 8 Read FresourceViewUrl Write SetresourceViewUrl;
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
    FbaseInstanceName : string;
    FcanIpForward : boolean;
    Fdescription : string;
    FdisksToAttach : TReplicaPoolParamsV1Beta1disksToAttach;
    FdisksToCreate : TReplicaPoolParamsV1Beta1disksToCreate;
    FinitAction : string;
    FmachineType : string;
    Fmetadata : TMetadata;
    FnetworkInterfaces : TReplicaPoolParamsV1Beta1networkInterfaces;
    FonHostMaintenance : string;
    FserviceAccounts : TReplicaPoolParamsV1Beta1serviceAccounts;
    Ftags : TTag;
    Fzone : string;
  Protected
    //Property setters
    Procedure SetautoRestart(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetbaseInstanceName(AIndex : Integer; AValue : string); virtual;
    Procedure SetcanIpForward(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisksToAttach(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1disksToAttach); virtual;
    Procedure SetdisksToCreate(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1disksToCreate); virtual;
    Procedure SetinitAction(AIndex : Integer; AValue : string); virtual;
    Procedure SetmachineType(AIndex : Integer; AValue : string); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TMetadata); virtual;
    Procedure SetnetworkInterfaces(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1networkInterfaces); virtual;
    Procedure SetonHostMaintenance(AIndex : Integer; AValue : string); virtual;
    Procedure SetserviceAccounts(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1serviceAccounts); virtual;
    Procedure Settags(AIndex : Integer; AValue : TTag); virtual;
    Procedure Setzone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property autoRestart : boolean Index 0 Read FautoRestart Write SetautoRestart;
    Property baseInstanceName : string Index 8 Read FbaseInstanceName Write SetbaseInstanceName;
    Property canIpForward : boolean Index 16 Read FcanIpForward Write SetcanIpForward;
    Property description : string Index 24 Read Fdescription Write Setdescription;
    Property disksToAttach : TReplicaPoolParamsV1Beta1disksToAttach Index 32 Read FdisksToAttach Write SetdisksToAttach;
    Property disksToCreate : TReplicaPoolParamsV1Beta1disksToCreate Index 40 Read FdisksToCreate Write SetdisksToCreate;
    Property initAction : string Index 48 Read FinitAction Write SetinitAction;
    Property machineType : string Index 56 Read FmachineType Write SetmachineType;
    Property metadata : TMetadata Index 64 Read Fmetadata Write Setmetadata;
    Property networkInterfaces : TReplicaPoolParamsV1Beta1networkInterfaces Index 72 Read FnetworkInterfaces Write SetnetworkInterfaces;
    Property onHostMaintenance : string Index 80 Read FonHostMaintenance Write SetonHostMaintenance;
    Property serviceAccounts : TReplicaPoolParamsV1Beta1serviceAccounts Index 88 Read FserviceAccounts Write SetserviceAccounts;
    Property tags : TTag Index 96 Read Ftags Write Settags;
    Property zone : string Index 104 Read Fzone Write Setzone;
  end;
  TReplicaPoolParamsV1Beta1Class = Class of TReplicaPoolParamsV1Beta1;
  
  { --------------------------------------------------------------------
    TReplicaPoolParamsV1Beta1disksToAttach
    --------------------------------------------------------------------}
  
  TReplicaPoolParamsV1Beta1disksToAttach = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReplicaPoolParamsV1Beta1disksToAttachClass = Class of TReplicaPoolParamsV1Beta1disksToAttach;
  
  { --------------------------------------------------------------------
    TReplicaPoolParamsV1Beta1disksToCreate
    --------------------------------------------------------------------}
  
  TReplicaPoolParamsV1Beta1disksToCreate = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReplicaPoolParamsV1Beta1disksToCreateClass = Class of TReplicaPoolParamsV1Beta1disksToCreate;
  
  { --------------------------------------------------------------------
    TReplicaPoolParamsV1Beta1networkInterfaces
    --------------------------------------------------------------------}
  
  TReplicaPoolParamsV1Beta1networkInterfaces = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReplicaPoolParamsV1Beta1networkInterfacesClass = Class of TReplicaPoolParamsV1Beta1networkInterfaces;
  
  { --------------------------------------------------------------------
    TReplicaPoolParamsV1Beta1serviceAccounts
    --------------------------------------------------------------------}
  
  TReplicaPoolParamsV1Beta1serviceAccounts = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReplicaPoolParamsV1Beta1serviceAccountsClass = Class of TReplicaPoolParamsV1Beta1serviceAccounts;
  
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
    TTag
    --------------------------------------------------------------------}
  
  TTag = Class(TGoogleBaseObject)
  Private
    FfingerPrint : string;
    Fitems : TTagitems;
  Protected
    //Property setters
    Procedure SetfingerPrint(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TTagitems); virtual;
  Public
  Published
    Property fingerPrint : string Index 0 Read FfingerPrint Write SetfingerPrint;
    Property items : TTagitems Index 8 Read Fitems Write Setitems;
  end;
  TTagClass = Class of TTag;
  
  { --------------------------------------------------------------------
    TTagitems
    --------------------------------------------------------------------}
  
  TTagitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTagitemsClass = Class of TTagitems;
  
  { --------------------------------------------------------------------
    TTemplate
    --------------------------------------------------------------------}
  
  TTemplate = Class(TGoogleBaseObject)
  Private
    Factions : TTemplateactions;
    Fdescription : string;
    Fmodules : TTemplatemodules;
    Fname : string;
  Protected
    //Property setters
    Procedure Setactions(AIndex : Integer; AValue : TTemplateactions); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setmodules(AIndex : Integer; AValue : TTemplatemodules); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property actions : TTemplateactions Index 0 Read Factions Write Setactions;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property modules : TTemplatemodules Index 16 Read Fmodules Write Setmodules;
    Property name : string Index 24 Read Fname Write Setname;
  end;
  TTemplateClass = Class of TTemplate;
  
  { --------------------------------------------------------------------
    TTemplateactions
    --------------------------------------------------------------------}
  
  TTemplateactions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TTemplateactionsClass = Class of TTemplateactions;
  
  { --------------------------------------------------------------------
    TTemplatemodules
    --------------------------------------------------------------------}
  
  TTemplatemodules = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TTemplatemodulesClass = Class of TTemplatemodules;
  
  { --------------------------------------------------------------------
    TTemplatesListResponse
    --------------------------------------------------------------------}
  
  TTemplatesListResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    Fresources : TTemplatesListResponseresources;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setresources(AIndex : Integer; AValue : TTemplatesListResponseresources); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property resources : TTemplatesListResponseresources Index 8 Read Fresources Write Setresources;
  end;
  TTemplatesListResponseClass = Class of TTemplatesListResponse;
  
  { --------------------------------------------------------------------
    TTemplatesListResponseresources
    --------------------------------------------------------------------}
  
  TTemplatesListResponseresources = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTemplatesListResponseresourcesClass = Class of TTemplatesListResponseresources;
  
  { --------------------------------------------------------------------
    TDeploymentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDeploymentsResource, method List
  
  TDeploymentsListOptions = Record
    maxResults : integer;
    pageToken : string;
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
    pageToken : string;
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


Procedure TAccessConfig.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccessConfig.SetnatIp(AIndex : Integer; AValue : string); 

begin
  If (FnatIp=AValue) then exit;
  FnatIp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccessConfig.Set_type(AIndex : Integer; AValue : string); 

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


Procedure TAction.Setcommands(AIndex : Integer; AValue : TActioncommands); 

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





{ --------------------------------------------------------------------
  TActioncommands
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAllowedRule
  --------------------------------------------------------------------}


Procedure TAllowedRule.SetIPProtocol(AIndex : Integer; AValue : string); 

begin
  If (FIPProtocol=AValue) then exit;
  FIPProtocol:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAllowedRule.Setports(AIndex : Integer; AValue : TAllowedRuleports); 

begin
  If (Fports=AValue) then exit;
  Fports:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAllowedRuleports
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAutoscalingModule
  --------------------------------------------------------------------}


Procedure TAutoscalingModule.SetcoolDownPeriodSec(AIndex : Integer; AValue : integer); 

begin
  If (FcoolDownPeriodSec=AValue) then exit;
  FcoolDownPeriodSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingModule.Setdescription(AIndex : Integer; AValue : string); 

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



Procedure TAutoscalingModule.SetsignalType(AIndex : Integer; AValue : string); 

begin
  If (FsignalType=AValue) then exit;
  FsignalType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingModule.SettargetModule(AIndex : Integer; AValue : string); 

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


Procedure TAutoscalingModuleStatus.SetautoscalingConfigUrl(AIndex : Integer; AValue : string); 

begin
  If (FautoscalingConfigUrl=AValue) then exit;
  FautoscalingConfigUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeployState
  --------------------------------------------------------------------}


Procedure TDeployState.Setdetails(AIndex : Integer; AValue : string); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployState.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeployment
  --------------------------------------------------------------------}


Procedure TDeployment.SetcreationDate(AIndex : Integer; AValue : string); 

begin
  If (FcreationDate=AValue) then exit;
  FcreationDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setmodules(AIndex : Integer; AValue : TDeploymentmodules); 

begin
  If (Fmodules=AValue) then exit;
  Fmodules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeployment.Setoverrides(AIndex : Integer; AValue : TDeploymentoverrides); 

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



Procedure TDeployment.SettemplateName(AIndex : Integer; AValue : string); 

begin
  If (FtemplateName=AValue) then exit;
  FtemplateName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeploymentmodules
  --------------------------------------------------------------------}


Class Function TDeploymentmodules.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TDeploymentoverrides
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDeploymentsListResponse
  --------------------------------------------------------------------}


Procedure TDeploymentsListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeploymentsListResponse.Setresources(AIndex : Integer; AValue : TDeploymentsListResponseresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeploymentsListResponseresources
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDiskAttachment
  --------------------------------------------------------------------}


Procedure TDiskAttachment.SetdeviceName(AIndex : Integer; AValue : string); 

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



Procedure TEnvVariable.Setvalue(AIndex : Integer; AValue : string); 

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



Procedure TExistingDisk.Setsource(AIndex : Integer; AValue : string); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFirewallModule
  --------------------------------------------------------------------}


Procedure TFirewallModule.Setallowed(AIndex : Integer; AValue : TFirewallModuleallowed); 

begin
  If (Fallowed=AValue) then exit;
  Fallowed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewallModule.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewallModule.Setnetwork(AIndex : Integer; AValue : string); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewallModule.SetsourceRanges(AIndex : Integer; AValue : TFirewallModulesourceRanges); 

begin
  If (FsourceRanges=AValue) then exit;
  FsourceRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewallModule.SetsourceTags(AIndex : Integer; AValue : TFirewallModulesourceTags); 

begin
  If (FsourceTags=AValue) then exit;
  FsourceTags:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFirewallModule.SettargetTags(AIndex : Integer; AValue : TFirewallModuletargetTags); 

begin
  If (FtargetTags=AValue) then exit;
  FtargetTags:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFirewallModuleallowed
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFirewallModulesourceRanges
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFirewallModulesourceTags
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFirewallModuletargetTags
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFirewallModuleStatus
  --------------------------------------------------------------------}


Procedure TFirewallModuleStatus.SetfirewallUrl(AIndex : Integer; AValue : string); 

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



Procedure THealthCheckModule.Setdescription(AIndex : Integer; AValue : string); 

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



Procedure THealthCheckModule.Sethost(AIndex : Integer; AValue : string); 

begin
  If (Fhost=AValue) then exit;
  Fhost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure THealthCheckModule.Setpath(AIndex : Integer; AValue : string); 

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


Procedure THealthCheckModuleStatus.SethealthCheckUrl(AIndex : Integer; AValue : string); 

begin
  If (FhealthCheckUrl=AValue) then exit;
  FhealthCheckUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLbModule
  --------------------------------------------------------------------}


Procedure TLbModule.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLbModule.SethealthChecks(AIndex : Integer; AValue : TLbModulehealthChecks); 

begin
  If (FhealthChecks=AValue) then exit;
  FhealthChecks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLbModule.SetipAddress(AIndex : Integer; AValue : string); 

begin
  If (FipAddress=AValue) then exit;
  FipAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLbModule.SetipProtocol(AIndex : Integer; AValue : string); 

begin
  If (FipProtocol=AValue) then exit;
  FipProtocol:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLbModule.SetportRange(AIndex : Integer; AValue : string); 

begin
  If (FportRange=AValue) then exit;
  FportRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLbModule.SetsessionAffinity(AIndex : Integer; AValue : string); 

begin
  If (FsessionAffinity=AValue) then exit;
  FsessionAffinity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLbModule.SettargetModules(AIndex : Integer; AValue : TLbModuletargetModules); 

begin
  If (FtargetModules=AValue) then exit;
  FtargetModules:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLbModulehealthChecks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLbModuletargetModules
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLbModuleStatus
  --------------------------------------------------------------------}


Procedure TLbModuleStatus.SetforwardingRuleUrl(AIndex : Integer; AValue : string); 

begin
  If (FforwardingRuleUrl=AValue) then exit;
  FforwardingRuleUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLbModuleStatus.SettargetPoolUrl(AIndex : Integer; AValue : string); 

begin
  If (FtargetPoolUrl=AValue) then exit;
  FtargetPoolUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetadata
  --------------------------------------------------------------------}


Procedure TMetadata.SetfingerPrint(AIndex : Integer; AValue : string); 

begin
  If (FfingerPrint=AValue) then exit;
  FfingerPrint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Setitems(AIndex : Integer; AValue : TMetadataitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetadataitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMetadataItem
  --------------------------------------------------------------------}


Procedure TMetadataItem.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadataItem.Setvalue(AIndex : Integer; AValue : string); 

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



Procedure TModule.Set_type(AIndex : Integer; AValue : string); 

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



Procedure TModuleStatus.Set_type(AIndex : Integer; AValue : string); 

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


Procedure TNetworkInterface.SetaccessConfigs(AIndex : Integer; AValue : TNetworkInterfaceaccessConfigs); 

begin
  If (FaccessConfigs=AValue) then exit;
  FaccessConfigs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkInterface.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkInterface.Setnetwork(AIndex : Integer; AValue : string); 

begin
  If (Fnetwork=AValue) then exit;
  Fnetwork:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkInterface.SetnetworkIp(AIndex : Integer; AValue : string); 

begin
  If (FnetworkIp=AValue) then exit;
  FnetworkIp:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNetworkInterfaceaccessConfigs
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TNetworkModule
  --------------------------------------------------------------------}


Procedure TNetworkModule.SetIPv4Range(AIndex : Integer; AValue : string); 

begin
  If (FIPv4Range=AValue) then exit;
  FIPv4Range:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkModule.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNetworkModule.SetgatewayIPv4(AIndex : Integer; AValue : string); 

begin
  If (FgatewayIPv4=AValue) then exit;
  FgatewayIPv4:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNetworkModuleStatus
  --------------------------------------------------------------------}


Procedure TNetworkModuleStatus.SetnetworkUrl(AIndex : Integer; AValue : string); 

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


Procedure TNewDiskInitializeParams.SetdiskSizeGb(AIndex : Integer; AValue : string); 

begin
  If (FdiskSizeGb=AValue) then exit;
  FdiskSizeGb:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNewDiskInitializeParams.SetdiskType(AIndex : Integer; AValue : string); 

begin
  If (FdiskType=AValue) then exit;
  FdiskType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNewDiskInitializeParams.SetsourceImage(AIndex : Integer; AValue : string); 

begin
  If (FsourceImage=AValue) then exit;
  FsourceImage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TParamOverride
  --------------------------------------------------------------------}


Procedure TParamOverride.Setpath(AIndex : Integer; AValue : string); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TParamOverride.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReplicaPoolModule
  --------------------------------------------------------------------}


Procedure TReplicaPoolModule.SetenvVariables(AIndex : Integer; AValue : TReplicaPoolModuleenvVariables); 

begin
  If (FenvVariables=AValue) then exit;
  FenvVariables:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolModule.SethealthChecks(AIndex : Integer; AValue : TReplicaPoolModulehealthChecks); 

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



Procedure TReplicaPoolModule.SetresourceView(AIndex : Integer; AValue : string); 

begin
  If (FresourceView=AValue) then exit;
  FresourceView:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReplicaPoolModuleenvVariables
  --------------------------------------------------------------------}


Class Function TReplicaPoolModuleenvVariables.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TReplicaPoolModulehealthChecks
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReplicaPoolModuleStatus
  --------------------------------------------------------------------}


Procedure TReplicaPoolModuleStatus.SetreplicaPoolUrl(AIndex : Integer; AValue : string); 

begin
  If (FreplicaPoolUrl=AValue) then exit;
  FreplicaPoolUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolModuleStatus.SetresourceViewUrl(AIndex : Integer; AValue : string); 

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



Procedure TReplicaPoolParamsV1Beta1.SetbaseInstanceName(AIndex : Integer; AValue : string); 

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



Procedure TReplicaPoolParamsV1Beta1.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.SetdisksToAttach(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1disksToAttach); 

begin
  If (FdisksToAttach=AValue) then exit;
  FdisksToAttach:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.SetdisksToCreate(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1disksToCreate); 

begin
  If (FdisksToCreate=AValue) then exit;
  FdisksToCreate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.SetinitAction(AIndex : Integer; AValue : string); 

begin
  If (FinitAction=AValue) then exit;
  FinitAction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.SetmachineType(AIndex : Integer; AValue : string); 

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



Procedure TReplicaPoolParamsV1Beta1.SetnetworkInterfaces(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1networkInterfaces); 

begin
  If (FnetworkInterfaces=AValue) then exit;
  FnetworkInterfaces:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.SetonHostMaintenance(AIndex : Integer; AValue : string); 

begin
  If (FonHostMaintenance=AValue) then exit;
  FonHostMaintenance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReplicaPoolParamsV1Beta1.SetserviceAccounts(AIndex : Integer; AValue : TReplicaPoolParamsV1Beta1serviceAccounts); 

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



Procedure TReplicaPoolParamsV1Beta1.Setzone(AIndex : Integer; AValue : string); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReplicaPoolParamsV1Beta1disksToAttach
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReplicaPoolParamsV1Beta1disksToCreate
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReplicaPoolParamsV1Beta1networkInterfaces
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReplicaPoolParamsV1Beta1serviceAccounts
  --------------------------------------------------------------------}




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
  TTag
  --------------------------------------------------------------------}


Procedure TTag.SetfingerPrint(AIndex : Integer; AValue : string); 

begin
  If (FfingerPrint=AValue) then exit;
  FfingerPrint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTag.Setitems(AIndex : Integer; AValue : TTagitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTagitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTemplate
  --------------------------------------------------------------------}


Procedure TTemplate.Setactions(AIndex : Integer; AValue : TTemplateactions); 

begin
  If (Factions=AValue) then exit;
  Factions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplate.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplate.Setmodules(AIndex : Integer; AValue : TTemplatemodules); 

begin
  If (Fmodules=AValue) then exit;
  Fmodules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplate.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTemplateactions
  --------------------------------------------------------------------}


Class Function TTemplateactions.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TTemplatemodules
  --------------------------------------------------------------------}


Class Function TTemplatemodules.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TTemplatesListResponse
  --------------------------------------------------------------------}


Procedure TTemplatesListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTemplatesListResponse.Setresources(AIndex : Integer; AValue : TTemplatesListResponseresources); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTemplatesListResponseresources
  --------------------------------------------------------------------}




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
  Result:='https://www.googleapis.com/';
end;

Class Function TManagerAPI.APIbasePath : string;

begin
  Result:='/manager/v1beta2/projects/';
end;

Class Function TManagerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/manager/v1beta2/projects/';
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
  TActioncommands.RegisterObject;
  TAllowedRule.RegisterObject;
  TAllowedRuleports.RegisterObject;
  TAutoscalingModule.RegisterObject;
  TAutoscalingModuleStatus.RegisterObject;
  TDeployState.RegisterObject;
  TDeployment.RegisterObject;
  TDeploymentmodules.RegisterObject;
  TDeploymentoverrides.RegisterObject;
  TDeploymentsListResponse.RegisterObject;
  TDeploymentsListResponseresources.RegisterObject;
  TDiskAttachment.RegisterObject;
  TEnvVariable.RegisterObject;
  TExistingDisk.RegisterObject;
  TFirewallModule.RegisterObject;
  TFirewallModuleallowed.RegisterObject;
  TFirewallModulesourceRanges.RegisterObject;
  TFirewallModulesourceTags.RegisterObject;
  TFirewallModuletargetTags.RegisterObject;
  TFirewallModuleStatus.RegisterObject;
  THealthCheckModule.RegisterObject;
  THealthCheckModuleStatus.RegisterObject;
  TLbModule.RegisterObject;
  TLbModulehealthChecks.RegisterObject;
  TLbModuletargetModules.RegisterObject;
  TLbModuleStatus.RegisterObject;
  TMetadata.RegisterObject;
  TMetadataitems.RegisterObject;
  TMetadataItem.RegisterObject;
  TModule.RegisterObject;
  TModuleStatus.RegisterObject;
  TNetworkInterface.RegisterObject;
  TNetworkInterfaceaccessConfigs.RegisterObject;
  TNetworkModule.RegisterObject;
  TNetworkModuleStatus.RegisterObject;
  TNewDisk.RegisterObject;
  TNewDiskInitializeParams.RegisterObject;
  TParamOverride.RegisterObject;
  TReplicaPoolModule.RegisterObject;
  TReplicaPoolModuleenvVariables.RegisterObject;
  TReplicaPoolModulehealthChecks.RegisterObject;
  TReplicaPoolModuleStatus.RegisterObject;
  TReplicaPoolParams.RegisterObject;
  TReplicaPoolParamsV1Beta1.RegisterObject;
  TReplicaPoolParamsV1Beta1disksToAttach.RegisterObject;
  TReplicaPoolParamsV1Beta1disksToCreate.RegisterObject;
  TReplicaPoolParamsV1Beta1networkInterfaces.RegisterObject;
  TReplicaPoolParamsV1Beta1serviceAccounts.RegisterObject;
  TServiceAccount.RegisterObject;
  TServiceAccountscopes.RegisterObject;
  TTag.RegisterObject;
  TTagitems.RegisterObject;
  TTemplate.RegisterObject;
  TTemplateactions.RegisterObject;
  TTemplatemodules.RegisterObject;
  TTemplatesListResponse.RegisterObject;
  TTemplatesListResponseresources.RegisterObject;
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
  Result.API:=Self;
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
  Result.API:=Self;
end;



initialization
  TManagerAPI.RegisterAPI;
end.
