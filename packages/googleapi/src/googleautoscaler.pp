unit googleautoscaler;
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
//Generated on: 9-5-15 13:22:49
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAutoscaler = class;
  TAutoscalerListResponse = class;
  TAutoscalingPolicy = class;
  TAutoscalingPolicyCpuUtilization = class;
  TAutoscalingPolicyCustomMetricUtilization = class;
  TAutoscalingPolicyLoadBalancingUtilization = class;
  TDeprecationStatus = class;
  TOperation = class;
  TOperationList = class;
  TZone = class;
  TZoneList = class;
  TAutoscalerArray = Array of TAutoscaler;
  TAutoscalerListResponseArray = Array of TAutoscalerListResponse;
  TAutoscalingPolicyArray = Array of TAutoscalingPolicy;
  TAutoscalingPolicyCpuUtilizationArray = Array of TAutoscalingPolicyCpuUtilization;
  TAutoscalingPolicyCustomMetricUtilizationArray = Array of TAutoscalingPolicyCustomMetricUtilization;
  TAutoscalingPolicyLoadBalancingUtilizationArray = Array of TAutoscalingPolicyLoadBalancingUtilization;
  TDeprecationStatusArray = Array of TDeprecationStatus;
  TOperationArray = Array of TOperation;
  TOperationListArray = Array of TOperationList;
  TZoneArray = Array of TZone;
  TZoneListArray = Array of TZoneList;
  //Anonymous types, using auto-generated names
  TOperationTypeerrorTypeerrorsItem = class;
  TOperationTypeerror = class;
  TOperationTypewarningsItemTypedataItem = class;
  TOperationTypewarningsItem = class;
  TZoneTypemaintenanceWindowsItem = class;
  TAutoscalerListResponseTypeitemsArray = Array of TAutoscaler;
  TAutoscalingPolicyTypecustomMetricUtilizationsArray = Array of TAutoscalingPolicyCustomMetricUtilization;
  TOperationTypeerrorTypeerrorsArray = Array of TOperationTypeerrorTypeerrorsItem;
  TOperationTypewarningsItemTypedataArray = Array of TOperationTypewarningsItemTypedataItem;
  TOperationTypewarningsArray = Array of TOperationTypewarningsItem;
  TOperationListTypeitemsArray = Array of TOperation;
  TZoneTypemaintenanceWindowsArray = Array of TZoneTypemaintenanceWindowsItem;
  TZoneListTypeitemsArray = Array of TZone;
  
  { --------------------------------------------------------------------
    TAutoscaler
    --------------------------------------------------------------------}
  
  TAutoscaler = Class(TGoogleBaseObject)
  Private
    FautoscalingPolicy : TAutoscalingPolicy;
    FcreationTimestamp : String;
    Fdescription : String;
    Fid : String;
    Fkind : String;
    Fname : String;
    FselfLink : String;
    Ftarget : String;
  Protected
    //Property setters
    Procedure SetautoscalingPolicy(AIndex : Integer; AValue : TAutoscalingPolicy); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Settarget(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property autoscalingPolicy : TAutoscalingPolicy Index 0 Read FautoscalingPolicy Write SetautoscalingPolicy;
    Property creationTimestamp : String Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property id : String Index 24 Read Fid Write Setid;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property name : String Index 40 Read Fname Write Setname;
    Property selfLink : String Index 48 Read FselfLink Write SetselfLink;
    Property target : String Index 56 Read Ftarget Write Settarget;
  end;
  TAutoscalerClass = Class of TAutoscaler;
  
  { --------------------------------------------------------------------
    TAutoscalerListResponse
    --------------------------------------------------------------------}
  
  TAutoscalerListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TAutoscalerListResponseTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAutoscalerListResponseTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TAutoscalerListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TAutoscalerListResponseClass = Class of TAutoscalerListResponse;
  
  { --------------------------------------------------------------------
    TAutoscalingPolicy
    --------------------------------------------------------------------}
  
  TAutoscalingPolicy = Class(TGoogleBaseObject)
  Private
    FcoolDownPeriodSec : integer;
    FcpuUtilization : TAutoscalingPolicyCpuUtilization;
    FcustomMetricUtilizations : TAutoscalingPolicyTypecustomMetricUtilizationsArray;
    FloadBalancingUtilization : TAutoscalingPolicyLoadBalancingUtilization;
    FmaxNumReplicas : integer;
    FminNumReplicas : integer;
  Protected
    //Property setters
    Procedure SetcoolDownPeriodSec(AIndex : Integer; AValue : integer); virtual;
    Procedure SetcpuUtilization(AIndex : Integer; AValue : TAutoscalingPolicyCpuUtilization); virtual;
    Procedure SetcustomMetricUtilizations(AIndex : Integer; AValue : TAutoscalingPolicyTypecustomMetricUtilizationsArray); virtual;
    Procedure SetloadBalancingUtilization(AIndex : Integer; AValue : TAutoscalingPolicyLoadBalancingUtilization); virtual;
    Procedure SetmaxNumReplicas(AIndex : Integer; AValue : integer); virtual;
    Procedure SetminNumReplicas(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property coolDownPeriodSec : integer Index 0 Read FcoolDownPeriodSec Write SetcoolDownPeriodSec;
    Property cpuUtilization : TAutoscalingPolicyCpuUtilization Index 8 Read FcpuUtilization Write SetcpuUtilization;
    Property customMetricUtilizations : TAutoscalingPolicyTypecustomMetricUtilizationsArray Index 16 Read FcustomMetricUtilizations Write SetcustomMetricUtilizations;
    Property loadBalancingUtilization : TAutoscalingPolicyLoadBalancingUtilization Index 24 Read FloadBalancingUtilization Write SetloadBalancingUtilization;
    Property maxNumReplicas : integer Index 32 Read FmaxNumReplicas Write SetmaxNumReplicas;
    Property minNumReplicas : integer Index 40 Read FminNumReplicas Write SetminNumReplicas;
  end;
  TAutoscalingPolicyClass = Class of TAutoscalingPolicy;
  
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
    Fmetric : String;
    FutilizationTarget : double;
    FutilizationTargetType : String;
  Protected
    //Property setters
    Procedure Setmetric(AIndex : Integer; AValue : String); virtual;
    Procedure SetutilizationTarget(AIndex : Integer; AValue : double); virtual;
    Procedure SetutilizationTargetType(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property metric : String Index 0 Read Fmetric Write Setmetric;
    Property utilizationTarget : double Index 8 Read FutilizationTarget Write SetutilizationTarget;
    Property utilizationTargetType : String Index 16 Read FutilizationTargetType Write SetutilizationTargetType;
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
    Fdeleted : String;
    Fdeprecated : String;
    Fobsolete : String;
    Freplacement : String;
    Fstate : String;
  Protected
    //Property setters
    Procedure Setdeleted(AIndex : Integer; AValue : String); virtual;
    Procedure Setdeprecated(AIndex : Integer; AValue : String); virtual;
    Procedure Setobsolete(AIndex : Integer; AValue : String); virtual;
    Procedure Setreplacement(AIndex : Integer; AValue : String); virtual;
    Procedure Setstate(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property deleted : String Index 0 Read Fdeleted Write Setdeleted;
    Property deprecated : String Index 8 Read Fdeprecated Write Setdeprecated;
    Property obsolete : String Index 16 Read Fobsolete Write Setobsolete;
    Property replacement : String Index 24 Read Freplacement Write Setreplacement;
    Property state : String Index 32 Read Fstate Write Setstate;
  end;
  TDeprecationStatusClass = Class of TDeprecationStatus;
  
  { --------------------------------------------------------------------
    TOperationTypeerrorTypeerrorsItem
    --------------------------------------------------------------------}
  
  TOperationTypeerrorTypeerrorsItem = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    Flocation : String;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : String); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property location : String Index 8 Read Flocation Write Setlocation;
    Property message : String Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationTypeerrorTypeerrorsItemClass = Class of TOperationTypeerrorTypeerrorsItem;
  
  { --------------------------------------------------------------------
    TOperationTypeerror
    --------------------------------------------------------------------}
  
  TOperationTypeerror = Class(TGoogleBaseObject)
  Private
    Ferrors : TOperationTypeerrorTypeerrorsArray;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; AValue : TOperationTypeerrorTypeerrorsArray); virtual;
  Public
  Published
    Property errors : TOperationTypeerrorTypeerrorsArray Index 0 Read Ferrors Write Seterrors;
  end;
  TOperationTypeerrorClass = Class of TOperationTypeerror;
  
  { --------------------------------------------------------------------
    TOperationTypewarningsItemTypedataItem
    --------------------------------------------------------------------}
  
  TOperationTypewarningsItemTypedataItem = Class(TGoogleBaseObject)
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
  TOperationTypewarningsItemTypedataItemClass = Class of TOperationTypewarningsItemTypedataItem;
  
  { --------------------------------------------------------------------
    TOperationTypewarningsItem
    --------------------------------------------------------------------}
  
  TOperationTypewarningsItem = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    Fdata : TOperationTypewarningsItemTypedataArray;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : String); virtual;
    Procedure Setdata(AIndex : Integer; AValue : TOperationTypewarningsItemTypedataArray); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property data : TOperationTypewarningsItemTypedataArray Index 8 Read Fdata Write Setdata;
    Property message : String Index 16 Read Fmessage Write Setmessage;
  end;
  TOperationTypewarningsItemClass = Class of TOperationTypewarningsItem;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    FclientOperationId : String;
    FcreationTimestamp : String;
    FendTime : String;
    Ferror : TOperationTypeerror;
    FhttpErrorMessage : String;
    FhttpErrorStatusCode : integer;
    Fid : String;
    FinsertTime : String;
    Fkind : String;
    Fname : String;
    FoperationType : String;
    Fprogress : integer;
    Fregion : String;
    FselfLink : String;
    FstartTime : String;
    Fstatus : String;
    FstatusMessage : String;
    FtargetId : String;
    FtargetLink : String;
    Fuser : String;
    Fwarnings : TOperationTypewarningsArray;
    Fzone : String;
  Protected
    //Property setters
    Procedure SetclientOperationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : String); virtual;
    Procedure Seterror(AIndex : Integer; AValue : TOperationTypeerror); virtual;
    Procedure SethttpErrorMessage(AIndex : Integer; AValue : String); virtual;
    Procedure SethttpErrorStatusCode(AIndex : Integer; AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinsertTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetoperationType(AIndex : Integer; AValue : String); virtual;
    Procedure Setprogress(AIndex : Integer; AValue : integer); virtual;
    Procedure Setregion(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetstatusMessage(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetId(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setuser(AIndex : Integer; AValue : String); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TOperationTypewarningsArray); virtual;
    Procedure Setzone(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property clientOperationId : String Index 0 Read FclientOperationId Write SetclientOperationId;
    Property creationTimestamp : String Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property endTime : String Index 16 Read FendTime Write SetendTime;
    Property error : TOperationTypeerror Index 24 Read Ferror Write Seterror;
    Property httpErrorMessage : String Index 32 Read FhttpErrorMessage Write SethttpErrorMessage;
    Property httpErrorStatusCode : integer Index 40 Read FhttpErrorStatusCode Write SethttpErrorStatusCode;
    Property id : String Index 48 Read Fid Write Setid;
    Property insertTime : String Index 56 Read FinsertTime Write SetinsertTime;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property name : String Index 72 Read Fname Write Setname;
    Property operationType : String Index 80 Read FoperationType Write SetoperationType;
    Property progress : integer Index 88 Read Fprogress Write Setprogress;
    Property region : String Index 96 Read Fregion Write Setregion;
    Property selfLink : String Index 104 Read FselfLink Write SetselfLink;
    Property startTime : String Index 112 Read FstartTime Write SetstartTime;
    Property status : String Index 120 Read Fstatus Write Setstatus;
    Property statusMessage : String Index 128 Read FstatusMessage Write SetstatusMessage;
    Property targetId : String Index 136 Read FtargetId Write SettargetId;
    Property targetLink : String Index 144 Read FtargetLink Write SettargetLink;
    Property user : String Index 152 Read Fuser Write Setuser;
    Property warnings : TOperationTypewarningsArray Index 160 Read Fwarnings Write Setwarnings;
    Property zone : String Index 168 Read Fzone Write Setzone;
  end;
  TOperationClass = Class of TOperation;
  
  { --------------------------------------------------------------------
    TOperationList
    --------------------------------------------------------------------}
  
  TOperationList = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fitems : TOperationListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FselfLink : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TOperationListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property items : TOperationListTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : String Index 32 Read FselfLink Write SetselfLink;
  end;
  TOperationListClass = Class of TOperationList;
  
  { --------------------------------------------------------------------
    TZoneTypemaintenanceWindowsItem
    --------------------------------------------------------------------}
  
  TZoneTypemaintenanceWindowsItem = Class(TGoogleBaseObject)
  Private
    FbeginTime : String;
    Fdescription : String;
    FendTime : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetbeginTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property beginTime : String Index 0 Read FbeginTime Write SetbeginTime;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property endTime : String Index 16 Read FendTime Write SetendTime;
    Property name : String Index 24 Read Fname Write Setname;
  end;
  TZoneTypemaintenanceWindowsItemClass = Class of TZoneTypemaintenanceWindowsItem;
  
  { --------------------------------------------------------------------
    TZone
    --------------------------------------------------------------------}
  
  TZone = Class(TGoogleBaseObject)
  Private
    FcreationTimestamp : String;
    Fdeprecated : TDeprecationStatus;
    Fdescription : String;
    Fid : String;
    Fkind : String;
    FmaintenanceWindows : TZoneTypemaintenanceWindowsArray;
    Fname : String;
    Fregion : String;
    FselfLink : String;
    Fstatus : String;
  Protected
    //Property setters
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure Setdeprecated(AIndex : Integer; AValue : TDeprecationStatus); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaintenanceWindows(AIndex : Integer; AValue : TZoneTypemaintenanceWindowsArray); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setregion(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property creationTimestamp : String Index 0 Read FcreationTimestamp Write SetcreationTimestamp;
    Property deprecated : TDeprecationStatus Index 8 Read Fdeprecated Write Setdeprecated;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property id : String Index 24 Read Fid Write Setid;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property maintenanceWindows : TZoneTypemaintenanceWindowsArray Index 40 Read FmaintenanceWindows Write SetmaintenanceWindows;
    Property name : String Index 48 Read Fname Write Setname;
    Property region : String Index 56 Read Fregion Write Setregion;
    Property selfLink : String Index 64 Read FselfLink Write SetselfLink;
    Property status : String Index 72 Read Fstatus Write Setstatus;
  end;
  TZoneClass = Class of TZone;
  
  { --------------------------------------------------------------------
    TZoneList
    --------------------------------------------------------------------}
  
  TZoneList = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fitems : TZoneListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FselfLink : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TZoneListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property items : TZoneListTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : String Index 32 Read FselfLink Write SetselfLink;
  end;
  TZoneListClass = Class of TZoneList;
  
  { --------------------------------------------------------------------
    TAutoscalersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAutoscalersResource, method List
  
  TAutoscalersListOptions = Record
    filter : String;
    maxResults : integer;
    pageToken : String;
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
    filter : String;
    maxResults : integer;
    pageToken : String;
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
    filter : String;
    maxResults : integer;
    pageToken : String;
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



Procedure TAutoscaler.SetcreationTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscaler.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscaler.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscaler.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscaler.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscaler.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscaler.Settarget(AIndex : Integer; AValue : String); 

begin
  If (Ftarget=AValue) then exit;
  Ftarget:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAutoscalerListResponse
  --------------------------------------------------------------------}


Procedure TAutoscalerListResponse.Setitems(AIndex : Integer; AValue : TAutoscalerListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalerListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAutoscalerListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





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



Procedure TAutoscalingPolicy.SetcustomMetricUtilizations(AIndex : Integer; AValue : TAutoscalingPolicyTypecustomMetricUtilizationsArray); 

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


Procedure TAutoscalingPolicyCustomMetricUtilization.Setmetric(AIndex : Integer; AValue : String); 

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



Procedure TAutoscalingPolicyCustomMetricUtilization.SetutilizationTargetType(AIndex : Integer; AValue : String); 

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


Procedure TDeprecationStatus.Setdeleted(AIndex : Integer; AValue : String); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeprecationStatus.Setdeprecated(AIndex : Integer; AValue : String); 

begin
  If (Fdeprecated=AValue) then exit;
  Fdeprecated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeprecationStatus.Setobsolete(AIndex : Integer; AValue : String); 

begin
  If (Fobsolete=AValue) then exit;
  Fobsolete:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeprecationStatus.Setreplacement(AIndex : Integer; AValue : String); 

begin
  If (Freplacement=AValue) then exit;
  Freplacement:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeprecationStatus.Setstate(AIndex : Integer; AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypeerrorTypeerrorsItem
  --------------------------------------------------------------------}


Procedure TOperationTypeerrorTypeerrorsItem.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypeerrorTypeerrorsItem.Setlocation(AIndex : Integer; AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypeerrorTypeerrorsItem.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypeerror
  --------------------------------------------------------------------}


Procedure TOperationTypeerror.Seterrors(AIndex : Integer; AValue : TOperationTypeerrorTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypewarningsItemTypedataItem
  --------------------------------------------------------------------}


Procedure TOperationTypewarningsItemTypedataItem.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypewarningsItemTypedataItem.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationTypewarningsItem
  --------------------------------------------------------------------}


Procedure TOperationTypewarningsItem.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypewarningsItem.Setdata(AIndex : Integer; AValue : TOperationTypewarningsItemTypedataArray); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationTypewarningsItem.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.SetclientOperationId(AIndex : Integer; AValue : String); 

begin
  If (FclientOperationId=AValue) then exit;
  FclientOperationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetcreationTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetendTime(AIndex : Integer; AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Seterror(AIndex : Integer; AValue : TOperationTypeerror); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SethttpErrorMessage(AIndex : Integer; AValue : String); 

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



Procedure TOperation.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetinsertTime(AIndex : Integer; AValue : String); 

begin
  If (FinsertTime=AValue) then exit;
  FinsertTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetoperationType(AIndex : Integer; AValue : String); 

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



Procedure TOperation.Setregion(AIndex : Integer; AValue : String); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstartTime(AIndex : Integer; AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SetstatusMessage(AIndex : Integer; AValue : String); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetId(AIndex : Integer; AValue : String); 

begin
  If (FtargetId=AValue) then exit;
  FtargetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.SettargetLink(AIndex : Integer; AValue : String); 

begin
  If (FtargetLink=AValue) then exit;
  FtargetLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setuser(AIndex : Integer; AValue : String); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setwarnings(AIndex : Integer; AValue : TOperationTypewarningsArray); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setzone(AIndex : Integer; AValue : String); 

begin
  If (Fzone=AValue) then exit;
  Fzone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TOperationList
  --------------------------------------------------------------------}


Procedure TOperationList.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.Setitems(AIndex : Integer; AValue : TOperationListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationList.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZoneTypemaintenanceWindowsItem
  --------------------------------------------------------------------}


Procedure TZoneTypemaintenanceWindowsItem.SetbeginTime(AIndex : Integer; AValue : String); 

begin
  If (FbeginTime=AValue) then exit;
  FbeginTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneTypemaintenanceWindowsItem.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneTypemaintenanceWindowsItem.SetendTime(AIndex : Integer; AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneTypemaintenanceWindowsItem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZone
  --------------------------------------------------------------------}


Procedure TZone.SetcreationTimestamp(AIndex : Integer; AValue : String); 

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



Procedure TZone.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.SetmaintenanceWindows(AIndex : Integer; AValue : TZoneTypemaintenanceWindowsArray); 

begin
  If (FmaintenanceWindows=AValue) then exit;
  FmaintenanceWindows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setregion(AIndex : Integer; AValue : String); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZone.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TZoneList
  --------------------------------------------------------------------}


Procedure TZoneList.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneList.Setitems(AIndex : Integer; AValue : TZoneListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TZoneList.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





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
  TAutoscalingPolicy.RegisterObject;
  TAutoscalingPolicyCpuUtilization.RegisterObject;
  TAutoscalingPolicyCustomMetricUtilization.RegisterObject;
  TAutoscalingPolicyLoadBalancingUtilization.RegisterObject;
  TDeprecationStatus.RegisterObject;
  TOperationTypeerrorTypeerrorsItem.RegisterObject;
  TOperationTypeerror.RegisterObject;
  TOperationTypewarningsItemTypedataItem.RegisterObject;
  TOperationTypewarningsItem.RegisterObject;
  TOperation.RegisterObject;
  TOperationList.RegisterObject;
  TZoneTypemaintenanceWindowsItem.RegisterObject;
  TZone.RegisterObject;
  TZoneList.RegisterObject;
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
