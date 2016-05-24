unit googleautoscaler;
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
  TAutoscaler = class;
  TAutoscalerArray = Array of TAutoscaler;
  TAutoscalerListResponse = class;
  TAutoscalerListResponseArray = Array of TAutoscalerListResponse;
  TAutoscalerListResponseitems = class;
  TAutoscalerListResponseitemsArray = Array of TAutoscalerListResponseitems;
  TAutoscalingPolicy = class;
  TAutoscalingPolicyArray = Array of TAutoscalingPolicy;
  TAutoscalingPolicycustomMetricUtilizations = class;
  TAutoscalingPolicycustomMetricUtilizationsArray = Array of TAutoscalingPolicycustomMetricUtilizations;
  TAutoscalingPolicyCpuUtilization = class;
  TAutoscalingPolicyCpuUtilizationArray = Array of TAutoscalingPolicyCpuUtilization;
  TAutoscalingPolicyCustomMetricUtilization = class;
  TAutoscalingPolicyCustomMetricUtilizationArray = Array of TAutoscalingPolicyCustomMetricUtilization;
  TAutoscalingPolicyLoadBalancingUtilization = class;
  TAutoscalingPolicyLoadBalancingUtilizationArray = Array of TAutoscalingPolicyLoadBalancingUtilization;
  TDeprecationStatus = class;
  TDeprecationStatusArray = Array of TDeprecationStatus;
  TOperation = class;
  TOperationArray = Array of TOperation;
  TOperationerror = class;
  TOperationerrorArray = Array of TOperationerror;
  TOperationerrorerrors = class;
  TOperationerrorerrorsArray = Array of TOperationerrorerrors;
  TOperationwarnings = class;
  TOperationwarningsArray = Array of TOperationwarnings;
  TOperationwarningsdata = class;
  TOperationwarningsdataArray = Array of TOperationwarningsdata;
  TOperationList = class;
  TOperationListArray = Array of TOperationList;
  TOperationListitems = class;
  TOperationListitemsArray = Array of TOperationListitems;
  TZone = class;
  TZoneArray = Array of TZone;
  TZonemaintenanceWindows = class;
  TZonemaintenanceWindowsArray = Array of TZonemaintenanceWindows;
  TZoneList = class;
  TZoneListArray = Array of TZoneList;
  TZoneListitems = class;
  TZoneListitemsArray = Array of TZoneListitems;
  
  { --------------------------------------------------------------------
    TAutoscaler
    --------------------------------------------------------------------}
  
  TAutoscaler = Class(TGoogleBaseObject)
  Private
    FautoscalingPolicy : TAutoscalingPolicy;
    FcreationTimestamp : string;
    Fdescription : string;
    Fid : string;
    Fkind : string;
    Fname : string;
    FselfLink : string;
    Ftarget : string;
  Protected
    //Property setters
    Procedure SetautoscalingPolicy(AIndex : Integer; AValue : TAutoscalingPolicy); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Settarget(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property autoscalingPolicy : TAutoscalingPolicy Index 0 Read FautoscalingPolicy Write SetautoscalingPolicy;
    Property creationTimestamp : string Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property name : string Index 40 Read Fname Write Setname;
    Property selfLink : string Index 48 Read FselfLink Write SetselfLink;
    Property target : string Index 56 Read Ftarget Write Settarget;
  end;
  TAutoscalerClass = Class of TAutoscaler;
  
  { --------------------------------------------------------------------
    TAutoscalerListResponse
    --------------------------------------------------------------------}
  
  TAutoscalerListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TAutoscalerListResponseitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAutoscalerListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TAutoscalerListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TAutoscalerListResponseClass = Class of TAutoscalerListResponse;
  
  { --------------------------------------------------------------------
    TAutoscalerListResponseitems
    --------------------------------------------------------------------}
  
  TAutoscalerListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAutoscalerListResponseitemsClass = Class of TAutoscalerListResponseitems;
  
  { --------------------------------------------------------------------
    TAutoscalingPolicy
    --------------------------------------------------------------------}
  
  TAutoscalingPolicy = Class(TGoogleBaseObject)
  Private
    FcoolDownPeriodSec : integer;
    FcpuUtilization : TAutoscalingPolicyCpuUtilization;
    FcustomMetricUtilizations : TAutoscalingPolicycustomMetricUtilizations;
    FloadBalancingUtilization : TAutoscalingPolicyLoadBalancingUtilization;
    FmaxNumReplicas : integer;
    FminNumReplicas : integer;
  Protected
    //Property setters
    Procedure SetcoolDownPeriodSec(AIndex : Integer; AValue : integer); virtual;
    Procedure SetcpuUtilization(AIndex : Integer; AValue : TAutoscalingPolicyCpuUtilization); virtual;
    Procedure SetcustomMetricUtilizations(AIndex : Integer; AValue : TAutoscalingPolicycustomMetricUtilizations); virtual;
    Procedure SetloadBalancingUtilization(AIndex : Integer; AValue : TAutoscalingPolicyLoadBalancingUtilization); virtual;
    Procedure SetmaxNumReplicas(AIndex : Integer; AValue : integer); virtual;
    Procedure SetminNumReplicas(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property coolDownPeriodSec : integer Index 0 Read FcoolDownPeriodSec Write SetcoolDownPeriodSec;
    Property cpuUtilization : TAutoscalingPolicyCpuUtilization Index 8 Read FcpuUtilization Write SetcpuUtilization;
    Property customMetricUtilizations : TAutoscalingPolicycustomMetricUtilizations Index 16 Read FcustomMetricUtilizations Write SetcustomMetricUtilizations;
    Property loadBalancingUtilization : TAutoscalingPolicyLoadBalancingUtilization Index 24 Read FloadBalancingUtilization Write SetloadBalancingUtilization;
    Property maxNumReplicas : integer Index 32 Read FmaxNumReplicas Write SetmaxNumReplicas;
    Property minNumReplicas : integer Index 40 Read FminNumReplicas Write SetminNumReplicas;
  end;
  TAutoscalingPolicyClass = Class of TAutoscalingPolicy;
  
  { --------------------------------------------------------------------
    TAutoscalingPolicycustomMetricUtilizations
    --------------------------------------------------------------------}
  
  TAutoscalingPolicycustomMetricUtilizations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAutoscalingPolicycustomMetricUtilizationsClass = Class of TAutoscalingPolicycustomMetricUtilizations;
  
  { --------------------------------------------------------------------
    TAutoscalingPolicyCpuUtilization
    --------------------------------------------------------------------}
  
  TAutoscalingPolicyCpuUtilization = Class(TGoogleBaseObject)
  Private
    FutilizationTarget : double;
  Protected
    //Property setters
    Procedure SetutilizationTarget(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property utilizationTarget : double Index 0 Read FutilizationTarget Write SetutilizationTarget;
  end;
  TAutoscalingPolicyCpuUtilizationClass = Class of TAutoscalingPolicyCpuUtilization;
  
  { --------------------------------------------------------------------
    TAutoscalingPolicyCustomMetricUtilization
    --------------------------------------------------------------------}
  
  TAutoscalingPolicyCustomMetricUtilization = Class(TGoogleBaseObject)
  Private
    Fmetric : string;
    FutilizationTarget : double;
    FutilizationTargetType : string;
  Protected
    //Property setters
    Procedure Setmetric(AIndex : Integer; AValue : string); virtual;
    Procedure SetutilizationTarget(AIndex : Integer; AValue : double); virtual;
    Procedure SetutilizationTargetType(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property metric : string Index 0 Read Fmetric Write Setmetric;
    Property utilizationTarget : double Index 8 Read FutilizationTarget Write SetutilizationTarget;
    Property utilizationTargetType : string Index 16 Read FutilizationTargetType Write SetutilizationTargetType;
  end;
  TAutoscalingPolicyCustomMetricUtilizationClass = Class of TAutoscalingPolicyCustomMetricUtilization;
  
  { --------------------------------------------------------------------
    TAutoscalingPolicyLoadBalancingUtilization
    --------------------------------------------------------------------}
  
  TAutoscalingPolicyLoadBalancingUtilization = Class(TGoogleBaseObject)
  Private
    FutilizationTarget : double;
  Protected
    //Property setters
    Procedure SetutilizationTarget(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property utilizationTarget : double Index 0 Read FutilizationTarget Write SetutilizationTarget;
  end;
  TAutoscalingPolicyLoadBalancingUtilizationClass = Class of TAutoscalingPolicyLoadBalancingUtilization;
  
  { --------------------------------------------------------------------
    TDeprecationStatus
    --------------------------------------------------------------------}
  
  TDeprecationStatus = Class(TGoogleBaseObject)
  Private
    Fdeleted : string;
    Fdeprecated : string;
    Fobsolete : string;
    Freplacement : string;
    Fstate : string;
  Protected
    //Property setters
    Procedure Setdeleted(AIndex : Integer; AValue : string); virtual;
    Procedure Setdeprecated(AIndex : Integer; AValue : string); virtual;
    Procedure Setobsolete(AIndex : Integer; AValue : string); virtual;
    Procedure Setreplacement(AIndex : Integer; AValue : string); virtual;
    Procedure Setstate(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property deleted : string Index 0 Read Fdeleted Write Setdeleted;
    Property deprecated : string Index 8 Read Fdeprecated Write Setdeprecated;
    Property obsolete : string Index 16 Read Fobsolete Write Setobsolete;
    Property replacement : string Index 24 Read Freplacement Write Setreplacement;
    Property state : string Index 32 Read Fstate Write Setstate;
  end;
  TDeprecationStatusClass = Class of TDeprecationStatus;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    FclientOperationId : string;
    FcreationTimestamp : string;
    FendTime : string;
    Ferror : TOperationerror;
    FhttpErrorMessage : string;
    FhttpErrorStatusCode : integer;
    Fid : string;
    FinsertTime : string;
    Fkind : string;
    Fname : string;
    FoperationType : string;
    Fprogress : integer;
    Fregion : string;
    FselfLink : string;
    FstartTime : string;
    Fstatus : string;
    FstatusMessage : string;
    FtargetId : string;
    FtargetLink : string;
    Fuser : string;
    Fwarnings : TOperationwarnings;
    Fzone : string;
  Protected
    //Property setters
    Procedure SetclientOperationId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : string); virtual;
    Procedure Seterror(AIndex : Integer; AValue : TOperationerror); virtual;
    Procedure SethttpErrorMessage(AIndex : Integer; AValue : string); virtual;
    Procedure SethttpErrorStatusCode(AIndex : Integer; AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinsertTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetoperationType(AIndex : Integer; AValue : string); virtual;
    Procedure Setprogress(AIndex : Integer; AValue : integer); virtual;
    Procedure Setregion(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetstatusMessage(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetId(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setuser(AIndex : Integer; AValue : string); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TOperationwarnings); virtual;
    Procedure Setzone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property clientOperationId : string Index 0 Read FclientOperationId Write SetclientOperationId;
    Property creationTimestamp : string Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property endTime : string Index 16 Read FendTime Write SetendTime;
    Property error : TOperationerror Index 24 Read Ferror Write Seterror;
    Property httpErrorMessage : string Index 32 Read FhttpErrorMessage Write SethttpErrorMessage;
    Property httpErrorStatusCode : integer Index 40 Read FhttpErrorStatusCode Write SethttpErrorStatusCode;
    Property id : string Index 48 Read Fid Write Setid;
    Property insertTime : string Index 56 Read FinsertTime Write SetinsertTime;
    Property kind : string Index 64 Read Fkind Write Setkind;
    Property name : string Index 72 Read Fname Write Setname;
    Property operationType : string Index 80 Read FoperationType Write SetoperationType;
    Property progress : integer Index 88 Read Fprogress Write Setprogress;
    Property region : string Index 96 Read Fregion Write Setregion;
    Property selfLink : string Index 104 Read FselfLink Write SetselfLink;
    Property startTime : string Index 112 Read FstartTime Write SetstartTime;
    Property status : string Index 120 Read Fstatus Write Setstatus;
    Property statusMessage : string Index 128 Read FstatusMessage Write SetstatusMessage;
    Property targetId : string Index 136 Read FtargetId Write SettargetId;
    Property targetLink : string Index 144 Read FtargetLink Write SettargetLink;
    Property user : string Index 152 Read Fuser Write Setuser;
    Property warnings : TOperationwarnings Index 160 Read Fwarnings Write Setwarnings;
    Property zone : string Index 168 Read Fzone Write Setzone;
  end;
  TOperationClass = Class of TOperation;
  
  { --------------------------------------------------------------------
    TOperationerror
    --------------------------------------------------------------------}
  
  TOperationerror = Class(TGoogleBaseObject)
  Private
    Ferrors : TOperationerrorerrors;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; AValue : TOperationerrorerrors); virtual;
  Public
  Published
    Property errors : TOperationerrorerrors Index 0 Read Ferrors Write Seterrors;
  end;
  TOperationerrorClass = Class of TOperationerror;
  
  { --------------------------------------------------------------------
    TOperationerrorerrors
    --------------------------------------------------------------------}
  
  TOperationerrorerrors = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Flocation : string;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : string); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property location : string Index 8 Read Flocation Write Setlocation;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationerrorerrorsClass = Class of TOperationerrorerrors;
  
  { --------------------------------------------------------------------
    TOperationwarnings
    --------------------------------------------------------------------}
  
  TOperationwarnings = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fdata : TOperationwarningsdata;
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TOperationwarningsdata); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property data : TOperationwarningsdata Index 8 Read Fdata Write Setdata;
    Property message : string Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationwarningsClass = Class of TOperationwarnings;
  
  { --------------------------------------------------------------------
    TOperationwarningsdata
    --------------------------------------------------------------------}
  
  TOperationwarningsdata = Class(TGoogleBaseObject)
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
  TOperationwarningsdataClass = Class of TOperationwarningsdata;
  
  { --------------------------------------------------------------------
    TOperationList
    --------------------------------------------------------------------}
  
  TOperationList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TOperationListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TOperationListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TOperationListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TOperationListClass = Class of TOperationList;
  
  { --------------------------------------------------------------------
    TOperationListitems
    --------------------------------------------------------------------}
  
  TOperationListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TOperationListitemsClass = Class of TOperationListitems;
  
  { --------------------------------------------------------------------
    TZone
    --------------------------------------------------------------------}
  
  TZone = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : string;
    Fdeprecated : TDeprecationStatus;
    Fdescription : string;
    Fid : string;
    Fkind : string;
    FmaintenanceWindows : TZonemaintenanceWindows;
    Fname : string;
    Fregion : string;
    FselfLink : string;
    Fstatus : string;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : string); virtual;
    Procedure Setdeprecated(AIndex : Integer; AValue : TDeprecationStatus); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaintenanceWindows(AIndex : Integer; AValue : TZonemaintenanceWindows); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setregion(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property creationTimestamp : string Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property deprecated : TDeprecationStatus Index 8 Read Fdeprecated Write Setdeprecated;
    Property description : string Index 16 Read Fdescription Write Setdescription;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property maintenanceWindows : TZonemaintenanceWindows Index 40 Read FmaintenanceWindows Write SetmaintenanceWindows;
    Property name : string Index 48 Read Fname Write Setname;
    Property region : string Index 56 Read Fregion Write Setregion;
    Property selfLink : string Index 64 Read FselfLink Write SetselfLink;
    Property status : string Index 72 Read Fstatus Write Setstatus;
  end;
  TZoneClass = Class of TZone;
  
  { --------------------------------------------------------------------
    TZonemaintenanceWindows
    --------------------------------------------------------------------}
  
  TZonemaintenanceWindows = Class(TGoogleBaseObject)
  Private
    FbeginTime : string;
    Fdescription : string;
    FendTime : string;
    Fname : string;
  Protected
    //Property setters
    Procedure SetbeginTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property beginTime : string Index 0 Read FbeginTime Write SetbeginTime;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property endTime : string Index 16 Read FendTime Write SetendTime;
    Property name : string Index 24 Read Fname Write Setname;
  end;
  TZonemaintenanceWindowsClass = Class of TZonemaintenanceWindows;
  
  { --------------------------------------------------------------------
    TZoneList
    --------------------------------------------------------------------}
  
  TZoneList = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fitems : TZoneListitems;
    Fkind : string;
    FnextPageToken : string;
    FselfLink : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TZoneListitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property items : TZoneListitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : string Index 32 Read FselfLink Write SetselfLink;
  end;
  TZoneListClass = Class of TZoneList;
  
  { --------------------------------------------------------------------
    TZoneListitems
    --------------------------------------------------------------------}
  
  TZoneListitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TZoneListitemsClass = Class of TZoneListitems;
  
  { --------------------------------------------------------------------
    TAutoscalersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAutoscalersResource, method List
  
  TAutoscalersListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TAutoscalersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(autoscaler: string; project: string; zone: string) : TOperation;
    Function Get(autoscaler: string; project: string; zone: string) : TAutoscaler;
    Function Insert(project: string; zone: string; aAutoscaler : TAutoscaler) : TOperation;
    Function List(project: string; zone: string; AQuery : string  = '') : TAutoscalerListResponse;
    Function List(project: string; zone: string; AQuery : TAutoscalerslistOptions) : TAutoscalerListResponse;
    Function Patch(autoscaler: string; project: string; zone: string; aAutoscaler : TAutoscaler) : TOperation;
    Function Update(autoscaler: string; project: string; zone: string; aAutoscaler : TAutoscaler) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TZoneOperationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TZoneOperationsResource, method List
  
  TZoneOperationsListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TZoneOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(operation: string; project: string; zone: string);
    Function Get(operation: string; project: string; zone: string) : TOperation;
    Function List(project: string; zone: string; AQuery : string  = '') : TOperationList;
    Function List(project: string; zone: string; AQuery : TZoneOperationslistOptions) : TOperationList;
  end;
  
  
  { --------------------------------------------------------------------
    TZonesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TZonesResource, method List
  
  TZonesListOptions = Record
    filter : string;
    maxResults : integer;
    pageToken : string;
  end;
  
  TZonesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(project: string; AQuery : string  = '') : TZoneList;
    Function List(project: string; AQuery : TZoneslistOptions) : TZoneList;
  end;
  
  
  { --------------------------------------------------------------------
    TAutoscalerAPI
    --------------------------------------------------------------------}
  
  TAutoscalerAPI = Class(TGoogleAPI)
  Private
    FAutoscalersInstance : TAutoscalersResource;
    FZoneOperationsInstance : TZoneOperationsResource;
    FZonesInstance : TZonesResource;
    Function GetAutoscalersInstance : TAutoscalersResource;virtual;
    Function GetZoneOperationsInstance : TZoneOperationsResource;virtual;
    Function GetZonesInstance : TZonesResource;virtual;
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
    Function CreateAutoscalersResource(AOwner : TComponent) : TAutoscalersResource;virtual;overload;
    Function CreateAutoscalersResource : TAutoscalersResource;virtual;overload;
    Function CreateZoneOperationsResource(AOwner : TComponent) : TZoneOperationsResource;virtual;overload;
    Function CreateZoneOperationsResource : TZoneOperationsResource;virtual;overload;
    Function CreateZonesResource(AOwner : TComponent) : TZonesResource;virtual;overload;
    Function CreateZonesResource : TZonesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AutoscalersResource : TAutoscalersResource Read GetAutoscalersInstance;
    Property ZoneOperationsResource : TZoneOperationsResource Read GetZoneOperationsInstance;
    Property ZonesResource : TZonesResource Read GetZonesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAutoscaler
  --------------------------------------------------------------------}


Procedure TAutoscaler.SetautoscalingPolicy(AIndex : Integer; AValue : TAutoscalingPolicy); 

begin
  If (FautoscalingPolicy=AValue) then exit;
  FautoscalingPolicy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscaler.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscaler.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscaler.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscaler.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscaler.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscaler.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscaler.Settarget(AIndex : Integer; AValue : string); 

begin
  If (Ftarget=AValue) then exit;
  Ftarget:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAutoscalerListResponse
  --------------------------------------------------------------------}


Procedure TAutoscalerListResponse.Setitems(AIndex : Integer; AValue : TAutoscalerListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalerListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalerListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAutoscalerListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAutoscalingPolicy
  --------------------------------------------------------------------}


Procedure TAutoscalingPolicy.SetcoolDownPeriodSec(AIndex : Integer; AValue : integer); 

begin
  If (FcoolDownPeriodSec=AValue) then exit;
  FcoolDownPeriodSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingPolicy.SetcpuUtilization(AIndex : Integer; AValue : TAutoscalingPolicyCpuUtilization); 

begin
  If (FcpuUtilization=AValue) then exit;
  FcpuUtilization:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingPolicy.SetcustomMetricUtilizations(AIndex : Integer; AValue : TAutoscalingPolicycustomMetricUtilizations); 

begin
  If (FcustomMetricUtilizations=AValue) then exit;
  FcustomMetricUtilizations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingPolicy.SetloadBalancingUtilization(AIndex : Integer; AValue : TAutoscalingPolicyLoadBalancingUtilization); 

begin
  If (FloadBalancingUtilization=AValue) then exit;
  FloadBalancingUtilization:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingPolicy.SetmaxNumReplicas(AIndex : Integer; AValue : integer); 

begin
  If (FmaxNumReplicas=AValue) then exit;
  FmaxNumReplicas:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingPolicy.SetminNumReplicas(AIndex : Integer; AValue : integer); 

begin
  If (FminNumReplicas=AValue) then exit;
  FminNumReplicas:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAutoscalingPolicycustomMetricUtilizations
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAutoscalingPolicyCpuUtilization
  --------------------------------------------------------------------}


Procedure TAutoscalingPolicyCpuUtilization.SetutilizationTarget(AIndex : Integer; AValue : double); 

begin
  If (FutilizationTarget=AValue) then exit;
  FutilizationTarget:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAutoscalingPolicyCustomMetricUtilization
  --------------------------------------------------------------------}


Procedure TAutoscalingPolicyCustomMetricUtilization.Setmetric(AIndex : Integer; AValue : string); 

begin
  If (Fmetric=AValue) then exit;
  Fmetric:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingPolicyCustomMetricUtilization.SetutilizationTarget(AIndex : Integer; AValue : double); 

begin
  If (FutilizationTarget=AValue) then exit;
  FutilizationTarget:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalingPolicyCustomMetricUtilization.SetutilizationTargetType(AIndex : Integer; AValue : string); 

begin
  If (FutilizationTargetType=AValue) then exit;
  FutilizationTargetType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAutoscalingPolicyLoadBalancingUtilization
  --------------------------------------------------------------------}


Procedure TAutoscalingPolicyLoadBalancingUtilization.SetutilizationTarget(AIndex : Integer; AValue : double); 

begin
  If (FutilizationTarget=AValue) then exit;
  FutilizationTarget:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDeprecationStatus
  --------------------------------------------------------------------}


Procedure TDeprecationStatus.Setdeleted(AIndex : Integer; AValue : string); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeprecationStatus.Setdeprecated(AIndex : Integer; AValue : string); 

begin
  If (Fdeprecated=AValue) then exit;
  Fdeprecated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeprecationStatus.Setobsolete(AIndex : Integer; AValue : string); 

begin
  If (Fobsolete=AValue) then exit;
  Fobsolete:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeprecationStatus.Setreplacement(AIndex : Integer; AValue : string); 

begin
  If (Freplacement=AValue) then exit;
  Freplacement:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeprecationStatus.Setstate(AIndex : Integer; AValue : string); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.SetclientOperationId(AIndex : Integer; AValue : string); 

begin
  If (FclientOperationId=AValue) then exit;
  FclientOperationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetendTime(AIndex : Integer; AValue : string); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Seterror(AIndex : Integer; AValue : TOperationerror); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SethttpErrorMessage(AIndex : Integer; AValue : string); 

begin
  If (FhttpErrorMessage=AValue) then exit;
  FhttpErrorMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SethttpErrorStatusCode(AIndex : Integer; AValue : integer); 

begin
  If (FhttpErrorStatusCode=AValue) then exit;
  FhttpErrorStatusCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetinsertTime(AIndex : Integer; AValue : string); 

begin
  If (FinsertTime=AValue) then exit;
  FinsertTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
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



Procedure TOperation.Setprogress(AIndex : Integer; AValue : integer); 

begin
  If (Fprogress=AValue) then exit;
  Fprogress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setregion(AIndex : Integer; AValue : string); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstartTime(AIndex : Integer; AValue : string); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstatusMessage(AIndex : Integer; AValue : string); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetId(AIndex : Integer; AValue : string); 

begin
  If (FtargetId=AValue) then exit;
  FtargetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetLink(AIndex : Integer; AValue : string); 

begin
  If (FtargetLink=AValue) then exit;
  FtargetLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setuser(AIndex : Integer; AValue : string); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setwarnings(AIndex : Integer; AValue : TOperationwarnings); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setzone(AIndex : Integer; AValue : string); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationerror
  --------------------------------------------------------------------}


Procedure TOperationerror.Seterrors(AIndex : Integer; AValue : TOperationerrorerrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationerrorerrors
  --------------------------------------------------------------------}


Procedure TOperationerrorerrors.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationerrorerrors.Setlocation(AIndex : Integer; AValue : string); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationerrorerrors.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationwarnings
  --------------------------------------------------------------------}


Procedure TOperationwarnings.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationwarnings.Setdata(AIndex : Integer; AValue : TOperationwarningsdata); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationwarnings.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationwarningsdata
  --------------------------------------------------------------------}


Procedure TOperationwarningsdata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationwarningsdata.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationList
  --------------------------------------------------------------------}


Procedure TOperationList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.Setitems(AIndex : Integer; AValue : TOperationListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TZone
  --------------------------------------------------------------------}


Procedure TZone.SetcreationTimestamp(AIndex : Integer; AValue : string); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setdeprecated(AIndex : Integer; AValue : TDeprecationStatus); 

begin
  If (Fdeprecated=AValue) then exit;
  Fdeprecated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.SetmaintenanceWindows(AIndex : Integer; AValue : TZonemaintenanceWindows); 

begin
  If (FmaintenanceWindows=AValue) then exit;
  FmaintenanceWindows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setregion(AIndex : Integer; AValue : string); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZonemaintenanceWindows
  --------------------------------------------------------------------}


Procedure TZonemaintenanceWindows.SetbeginTime(AIndex : Integer; AValue : string); 

begin
  If (FbeginTime=AValue) then exit;
  FbeginTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZonemaintenanceWindows.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZonemaintenanceWindows.SetendTime(AIndex : Integer; AValue : string); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZonemaintenanceWindows.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZoneList
  --------------------------------------------------------------------}


Procedure TZoneList.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneList.Setitems(AIndex : Integer; AValue : TZoneListitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneList.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneList.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneList.SetselfLink(AIndex : Integer; AValue : string); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZoneListitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAutoscalersResource
  --------------------------------------------------------------------}


Class Function TAutoscalersResource.ResourceName : String;

begin
  Result:='autoscalers';
end;

Class Function TAutoscalersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TautoscalerAPI;
end;

Function TAutoscalersResource.Delete(autoscaler: string; project: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'projects/{project}/zones/{zone}/autoscalers/{autoscaler}';
  _Methodid   = 'autoscaler.autoscalers.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['autoscaler',autoscaler,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TAutoscalersResource.Get(autoscaler: string; project: string; zone: string) : TAutoscaler;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{project}/zones/{zone}/autoscalers/{autoscaler}';
  _Methodid   = 'autoscaler.autoscalers.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['autoscaler',autoscaler,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAutoscaler) as TAutoscaler;
end;

Function TAutoscalersResource.Insert(project: string; zone: string; aAutoscaler : TAutoscaler) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'projects/{project}/zones/{zone}/autoscalers';
  _Methodid   = 'autoscaler.autoscalers.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAutoscaler,TOperation) as TOperation;
end;

Function TAutoscalersResource.List(project: string; zone: string; AQuery : string = '') : TAutoscalerListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'projects/{project}/zones/{zone}/autoscalers';
  _Methodid   = 'autoscaler.autoscalers.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAutoscalerListResponse) as TAutoscalerListResponse;
end;


Function TAutoscalersResource.List(project: string; zone: string; AQuery : TAutoscalerslistOptions) : TAutoscalerListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,zone,_Q);
end;

Function TAutoscalersResource.Patch(autoscaler: string; project: string; zone: string; aAutoscaler : TAutoscaler) : TOperation;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'projects/{project}/zones/{zone}/autoscalers/{autoscaler}';
  _Methodid   = 'autoscaler.autoscalers.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['autoscaler',autoscaler,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAutoscaler,TOperation) as TOperation;
end;

Function TAutoscalersResource.Update(autoscaler: string; project: string; zone: string; aAutoscaler : TAutoscaler) : TOperation;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'projects/{project}/zones/{zone}/autoscalers/{autoscaler}';
  _Methodid   = 'autoscaler.autoscalers.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['autoscaler',autoscaler,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAutoscaler,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TZoneOperationsResource
  --------------------------------------------------------------------}


Class Function TZoneOperationsResource.ResourceName : String;

begin
  Result:='zoneOperations';
end;

Class Function TZoneOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TautoscalerAPI;
end;

Procedure TZoneOperationsResource.Delete(operation: string; project: string; zone: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/zones/{zone}/operations/{operation}';
  _Methodid   = 'autoscaler.zoneOperations.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operation',operation,'project',project,'zone',zone]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TZoneOperationsResource.Get(operation: string; project: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/operations/{operation}';
  _Methodid   = 'autoscaler.zoneOperations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operation',operation,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TZoneOperationsResource.List(project: string; zone: string; AQuery : string = '') : TOperationList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/operations';
  _Methodid   = 'autoscaler.zoneOperations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperationList) as TOperationList;
end;


Function TZoneOperationsResource.List(project: string; zone: string; AQuery : TZoneOperationslistOptions) : TOperationList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,zone,_Q);
end;



{ --------------------------------------------------------------------
  TZonesResource
  --------------------------------------------------------------------}


Class Function TZonesResource.ResourceName : String;

begin
  Result:='zones';
end;

Class Function TZonesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TautoscalerAPI;
end;

Function TZonesResource.List(project: string; AQuery : string = '') : TZoneList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones';
  _Methodid   = 'autoscaler.zones.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TZoneList) as TZoneList;
end;


Function TZonesResource.List(project: string; AQuery : TZoneslistOptions) : TZoneList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
end;



{ --------------------------------------------------------------------
  TAutoscalerAPI
  --------------------------------------------------------------------}

Class Function TAutoscalerAPI.APIName : String;

begin
  Result:='autoscaler';
end;

Class Function TAutoscalerAPI.APIVersion : String;

begin
  Result:='v1beta2';
end;

Class Function TAutoscalerAPI.APIRevision : String;

begin
  Result:='20141112';
end;

Class Function TAutoscalerAPI.APIID : String;

begin
  Result:='autoscaler:v1beta2';
end;

Class Function TAutoscalerAPI.APITitle : String;

begin
  Result:='Google Compute Engine Autoscaler API';
end;

Class Function TAutoscalerAPI.APIDescription : String;

begin
  Result:='The Google Compute Engine Autoscaler API provides autoscaling for groups of Cloud VMs.';
end;

Class Function TAutoscalerAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TAutoscalerAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TAutoscalerAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TAutoscalerAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TAutoscalerAPI.APIdocumentationLink : String;

begin
  Result:='http://developers.google.com/compute/docs/autoscaler';
end;

Class Function TAutoscalerAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TAutoscalerAPI.APIbasePath : string;

begin
  Result:='/autoscaler/v1beta2/';
end;

Class Function TAutoscalerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/autoscaler/v1beta2/';
end;

Class Function TAutoscalerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TAutoscalerAPI.APIservicePath : string;

begin
  Result:='autoscaler/v1beta2/';
end;

Class Function TAutoscalerAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TAutoscalerAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/compute';
  Result[0].Description:='View and manage your Google Compute Engine resources';
  Result[1].Name:='https://www.googleapis.com/auth/compute.readonly';
  Result[1].Description:='View your Google Compute Engine resources';
  
end;

Class Function TAutoscalerAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TAutoscalerAPI.RegisterAPIResources;

begin
  TAutoscaler.RegisterObject;
  TAutoscalerListResponse.RegisterObject;
  TAutoscalerListResponseitems.RegisterObject;
  TAutoscalingPolicy.RegisterObject;
  TAutoscalingPolicycustomMetricUtilizations.RegisterObject;
  TAutoscalingPolicyCpuUtilization.RegisterObject;
  TAutoscalingPolicyCustomMetricUtilization.RegisterObject;
  TAutoscalingPolicyLoadBalancingUtilization.RegisterObject;
  TDeprecationStatus.RegisterObject;
  TOperation.RegisterObject;
  TOperationerror.RegisterObject;
  TOperationerrorerrors.RegisterObject;
  TOperationwarnings.RegisterObject;
  TOperationwarningsdata.RegisterObject;
  TOperationList.RegisterObject;
  TOperationListitems.RegisterObject;
  TZone.RegisterObject;
  TZonemaintenanceWindows.RegisterObject;
  TZoneList.RegisterObject;
  TZoneListitems.RegisterObject;
end;


Function TAutoscalerAPI.GetAutoscalersInstance : TAutoscalersResource;

begin
  if (FAutoscalersInstance=Nil) then
    FAutoscalersInstance:=CreateAutoscalersResource;
  Result:=FAutoscalersInstance;
end;

Function TAutoscalerAPI.CreateAutoscalersResource : TAutoscalersResource;

begin
  Result:=CreateAutoscalersResource(Self);
end;


Function TAutoscalerAPI.CreateAutoscalersResource(AOwner : TComponent) : TAutoscalersResource;

begin
  Result:=TAutoscalersResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAutoscalerAPI.GetZoneOperationsInstance : TZoneOperationsResource;

begin
  if (FZoneOperationsInstance=Nil) then
    FZoneOperationsInstance:=CreateZoneOperationsResource;
  Result:=FZoneOperationsInstance;
end;

Function TAutoscalerAPI.CreateZoneOperationsResource : TZoneOperationsResource;

begin
  Result:=CreateZoneOperationsResource(Self);
end;


Function TAutoscalerAPI.CreateZoneOperationsResource(AOwner : TComponent) : TZoneOperationsResource;

begin
  Result:=TZoneOperationsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAutoscalerAPI.GetZonesInstance : TZonesResource;

begin
  if (FZonesInstance=Nil) then
    FZonesInstance:=CreateZonesResource;
  Result:=FZonesInstance;
end;

Function TAutoscalerAPI.CreateZonesResource : TZonesResource;

begin
  Result:=CreateZonesResource(Self);
end;


Function TAutoscalerAPI.CreateZonesResource(AOwner : TComponent) : TZonesResource;

begin
  Result:=TZonesResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TAutoscalerAPI.RegisterAPI;
end.
