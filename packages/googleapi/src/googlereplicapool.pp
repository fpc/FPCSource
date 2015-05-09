unit googlereplicapool;
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
//Generated on: 9-5-15 13:22:57
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TInstanceGroupManager = class;
  TInstanceGroupManagerList = class;
  TInstanceGroupManagersAbandonInstancesRequest = class;
  TInstanceGroupManagersDeleteInstancesRequest = class;
  TInstanceGroupManagersRecreateInstancesRequest = class;
  TInstanceGroupManagersSetInstanceTemplateRequest = class;
  TInstanceGroupManagersSetTargetPoolsRequest = class;
  TOperation = class;
  TOperationList = class;
  TInstanceGroupManagerArray = Array of TInstanceGroupManager;
  TInstanceGroupManagerListArray = Array of TInstanceGroupManagerList;
  TInstanceGroupManagersAbandonInstancesRequestArray = Array of TInstanceGroupManagersAbandonInstancesRequest;
  TInstanceGroupManagersDeleteInstancesRequestArray = Array of TInstanceGroupManagersDeleteInstancesRequest;
  TInstanceGroupManagersRecreateInstancesRequestArray = Array of TInstanceGroupManagersRecreateInstancesRequest;
  TInstanceGroupManagersSetInstanceTemplateRequestArray = Array of TInstanceGroupManagersSetInstanceTemplateRequest;
  TInstanceGroupManagersSetTargetPoolsRequestArray = Array of TInstanceGroupManagersSetTargetPoolsRequest;
  TOperationArray = Array of TOperation;
  TOperationListArray = Array of TOperationList;
  //Anonymous types, using auto-generated names
  TOperationTypeerrorTypeerrorsItem = class;
  TOperationTypeerror = class;
  TOperationTypewarningsItemTypedataItem = class;
  TOperationTypewarningsItem = class;
  TInstanceGroupManagerListTypeitemsArray = Array of TInstanceGroupManager;
  TOperationTypeerrorTypeerrorsArray = Array of TOperationTypeerrorTypeerrorsItem;
  TOperationTypewarningsItemTypedataArray = Array of TOperationTypewarningsItemTypedataItem;
  TOperationTypewarningsArray = Array of TOperationTypewarningsItem;
  TOperationListTypeitemsArray = Array of TOperation;
  
  { --------------------------------------------------------------------
    TInstanceGroupManager
    --------------------------------------------------------------------}
  
  TInstanceGroupManager = Class(TGoogleBaseObject)
  Private
    FbaseInstanceName : String;
    FcreationTimestamp : String;
    FcurrentSize : integer;
    Fdescription : String;
    Ffingerprint : String;
    Fgroup : String;
    Fid : String;
    FinstanceTemplate : String;
    Fkind : String;
    Fname : String;
    FselfLink : String;
    FtargetPools : TStringArray;
    FtargetSize : integer;
  Protected
    //Property setters
    Procedure SetbaseInstanceName(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure SetcurrentSize(AIndex : Integer; AValue : integer); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setfingerprint(AIndex : Integer; AValue : String); virtual;
    Procedure Setgroup(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinstanceTemplate(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetPools(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SettargetSize(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property baseInstanceName : String Index 0 Read FbaseInstanceName Write SetbaseInstanceName;
    Property creationTimestamp : String Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property currentSize : integer Index 16 Read FcurrentSize Write SetcurrentSize;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property fingerprint : String Index 32 Read Ffingerprint Write Setfingerprint;
    Property group : String Index 40 Read Fgroup Write Setgroup;
    Property id : String Index 48 Read Fid Write Setid;
    Property instanceTemplate : String Index 56 Read FinstanceTemplate Write SetinstanceTemplate;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property name : String Index 72 Read Fname Write Setname;
    Property selfLink : String Index 80 Read FselfLink Write SetselfLink;
    Property targetPools : TStringArray Index 88 Read FtargetPools Write SettargetPools;
    Property targetSize : integer Index 96 Read FtargetSize Write SettargetSize;
  end;
  TInstanceGroupManagerClass = Class of TInstanceGroupManager;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagerList
    --------------------------------------------------------------------}
  
  TInstanceGroupManagerList = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fitems : TInstanceGroupManagerListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FselfLink : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TInstanceGroupManagerListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property items : TInstanceGroupManagerListTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : String Index 32 Read FselfLink Write SetselfLink;
  end;
  TInstanceGroupManagerListClass = Class of TInstanceGroupManagerList;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagersAbandonInstancesRequest
    --------------------------------------------------------------------}
  
  TInstanceGroupManagersAbandonInstancesRequest = Class(TGoogleBaseObject)
  Private
    Finstances : TStringArray;
  Protected
    //Property setters
    Procedure Setinstances(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property instances : TStringArray Index 0 Read Finstances Write Setinstances;
  end;
  TInstanceGroupManagersAbandonInstancesRequestClass = Class of TInstanceGroupManagersAbandonInstancesRequest;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagersDeleteInstancesRequest
    --------------------------------------------------------------------}
  
  TInstanceGroupManagersDeleteInstancesRequest = Class(TGoogleBaseObject)
  Private
    Finstances : TStringArray;
  Protected
    //Property setters
    Procedure Setinstances(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property instances : TStringArray Index 0 Read Finstances Write Setinstances;
  end;
  TInstanceGroupManagersDeleteInstancesRequestClass = Class of TInstanceGroupManagersDeleteInstancesRequest;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagersRecreateInstancesRequest
    --------------------------------------------------------------------}
  
  TInstanceGroupManagersRecreateInstancesRequest = Class(TGoogleBaseObject)
  Private
    Finstances : TStringArray;
  Protected
    //Property setters
    Procedure Setinstances(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property instances : TStringArray Index 0 Read Finstances Write Setinstances;
  end;
  TInstanceGroupManagersRecreateInstancesRequestClass = Class of TInstanceGroupManagersRecreateInstancesRequest;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagersSetInstanceTemplateRequest
    --------------------------------------------------------------------}
  
  TInstanceGroupManagersSetInstanceTemplateRequest = Class(TGoogleBaseObject)
  Private
    FinstanceTemplate : String;
  Protected
    //Property setters
    Procedure SetinstanceTemplate(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property instanceTemplate : String Index 0 Read FinstanceTemplate Write SetinstanceTemplate;
  end;
  TInstanceGroupManagersSetInstanceTemplateRequestClass = Class of TInstanceGroupManagersSetInstanceTemplateRequest;
  
  { --------------------------------------------------------------------
    TInstanceGroupManagersSetTargetPoolsRequest
    --------------------------------------------------------------------}
  
  TInstanceGroupManagersSetTargetPoolsRequest = Class(TGoogleBaseObject)
  Private
    Ffingerprint : String;
    FtargetPools : TStringArray;
  Protected
    //Property setters
    Procedure Setfingerprint(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetPools(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property fingerprint : String Index 0 Read Ffingerprint Write Setfingerprint;
    Property targetPools : TStringArray Index 8 Read FtargetPools Write SettargetPools;
  end;
  TInstanceGroupManagersSetTargetPoolsRequestClass = Class of TInstanceGroupManagersSetTargetPoolsRequest;
  
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
    TInstanceGroupManagersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TInstanceGroupManagersResource, method Insert
  
  TInstanceGroupManagersInsertOptions = Record
    size : integer;
  end;
  
  
  //Optional query Options for TInstanceGroupManagersResource, method List
  
  TInstanceGroupManagersListOptions = Record
    filter : String;
    maxResults : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TInstanceGroupManagersResource, method Resize
  
  TInstanceGroupManagersResizeOptions = Record
    size : integer;
  end;
  
  TInstanceGroupManagersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AbandonInstances(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersAbandonInstancesRequest : TInstanceGroupManagersAbandonInstancesRequest) : TOperation;
    Function Delete(instanceGroupManager: string; project: string; zone: string) : TOperation;
    Function DeleteInstances(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersDeleteInstancesRequest : TInstanceGroupManagersDeleteInstancesRequest) : TOperation;
    Function Get(instanceGroupManager: string; project: string; zone: string) : TInstanceGroupManager;
    Function Insert(project: string; zone: string; aInstanceGroupManager : TInstanceGroupManager; AQuery : string  = '') : TOperation;
    Function Insert(project: string; zone: string; aInstanceGroupManager : TInstanceGroupManager; AQuery : TInstanceGroupManagersinsertOptions) : TOperation;
    Function List(project: string; zone: string; AQuery : string  = '') : TInstanceGroupManagerList;
    Function List(project: string; zone: string; AQuery : TInstanceGroupManagerslistOptions) : TInstanceGroupManagerList;
    Function RecreateInstances(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersRecreateInstancesRequest : TInstanceGroupManagersRecreateInstancesRequest) : TOperation;
    Function Resize(instanceGroupManager: string; project: string; zone: string; AQuery : string  = '') : TOperation;
    Function Resize(instanceGroupManager: string; project: string; zone: string; AQuery : TInstanceGroupManagersresizeOptions) : TOperation;
    Function SetInstanceTemplate(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersSetInstanceTemplateRequest : TInstanceGroupManagersSetInstanceTemplateRequest) : TOperation;
    Function SetTargetPools(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersSetTargetPoolsRequest : TInstanceGroupManagersSetTargetPoolsRequest) : TOperation;
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
    Function Get(operation: string; project: string; zone: string) : TOperation;
    Function List(project: string; zone: string; AQuery : string  = '') : TOperationList;
    Function List(project: string; zone: string; AQuery : TZoneOperationslistOptions) : TOperationList;
  end;
  
  
  { --------------------------------------------------------------------
    TReplicapoolAPI
    --------------------------------------------------------------------}
  
  TReplicapoolAPI = Class(TGoogleAPI)
  Private
    FInstanceGroupManagersInstance : TInstanceGroupManagersResource;
    FZoneOperationsInstance : TZoneOperationsResource;
    Function GetInstanceGroupManagersInstance : TInstanceGroupManagersResource;virtual;
    Function GetZoneOperationsInstance : TZoneOperationsResource;virtual;
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
    Function CreateInstanceGroupManagersResource(AOwner : TComponent) : TInstanceGroupManagersResource;virtual;overload;
    Function CreateInstanceGroupManagersResource : TInstanceGroupManagersResource;virtual;overload;
    Function CreateZoneOperationsResource(AOwner : TComponent) : TZoneOperationsResource;virtual;overload;
    Function CreateZoneOperationsResource : TZoneOperationsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property InstanceGroupManagersResource : TInstanceGroupManagersResource Read GetInstanceGroupManagersInstance;
    Property ZoneOperationsResource : TZoneOperationsResource Read GetZoneOperationsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TInstanceGroupManager
  --------------------------------------------------------------------}


Procedure TInstanceGroupManager.SetbaseInstanceName(AIndex : Integer; AValue : String); 

begin
  If (FbaseInstanceName=AValue) then exit;
  FbaseInstanceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.SetcreationTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.SetcurrentSize(AIndex : Integer; AValue : integer); 

begin
  If (FcurrentSize=AValue) then exit;
  FcurrentSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.Setfingerprint(AIndex : Integer; AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.Setgroup(AIndex : Integer; AValue : String); 

begin
  If (Fgroup=AValue) then exit;
  Fgroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.SetinstanceTemplate(AIndex : Integer; AValue : String); 

begin
  If (FinstanceTemplate=AValue) then exit;
  FinstanceTemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.SettargetPools(AIndex : Integer; AValue : TStringArray); 

begin
  If (FtargetPools=AValue) then exit;
  FtargetPools:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManager.SettargetSize(AIndex : Integer; AValue : integer); 

begin
  If (FtargetSize=AValue) then exit;
  FtargetSize:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceGroupManagerList
  --------------------------------------------------------------------}


Procedure TInstanceGroupManagerList.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManagerList.Setitems(AIndex : Integer; AValue : TInstanceGroupManagerListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManagerList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManagerList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManagerList.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceGroupManagersAbandonInstancesRequest
  --------------------------------------------------------------------}


Procedure TInstanceGroupManagersAbandonInstancesRequest.Setinstances(AIndex : Integer; AValue : TStringArray); 

begin
  If (Finstances=AValue) then exit;
  Finstances:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceGroupManagersDeleteInstancesRequest
  --------------------------------------------------------------------}


Procedure TInstanceGroupManagersDeleteInstancesRequest.Setinstances(AIndex : Integer; AValue : TStringArray); 

begin
  If (Finstances=AValue) then exit;
  Finstances:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceGroupManagersRecreateInstancesRequest
  --------------------------------------------------------------------}


Procedure TInstanceGroupManagersRecreateInstancesRequest.Setinstances(AIndex : Integer; AValue : TStringArray); 

begin
  If (Finstances=AValue) then exit;
  Finstances:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceGroupManagersSetInstanceTemplateRequest
  --------------------------------------------------------------------}


Procedure TInstanceGroupManagersSetInstanceTemplateRequest.SetinstanceTemplate(AIndex : Integer; AValue : String); 

begin
  If (FinstanceTemplate=AValue) then exit;
  FinstanceTemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceGroupManagersSetTargetPoolsRequest
  --------------------------------------------------------------------}


Procedure TInstanceGroupManagersSetTargetPoolsRequest.Setfingerprint(AIndex : Integer; AValue : String); 

begin
  If (Ffingerprint=AValue) then exit;
  Ffingerprint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceGroupManagersSetTargetPoolsRequest.SettargetPools(AIndex : Integer; AValue : TStringArray); 

begin
  If (FtargetPools=AValue) then exit;
  FtargetPools:=AValue;
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
  TInstanceGroupManagersResource
  --------------------------------------------------------------------}


Class Function TInstanceGroupManagersResource.ResourceName : String;

begin
  Result:='instanceGroupManagers';
end;

Class Function TInstanceGroupManagersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TreplicapoolAPI;
end;

Function TInstanceGroupManagersResource.AbandonInstances(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersAbandonInstancesRequest : TInstanceGroupManagersAbandonInstancesRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/abandonInstances';
  _Methodid   = 'replicapool.instanceGroupManagers.abandonInstances';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceGroupManager',instanceGroupManager,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstanceGroupManagersAbandonInstancesRequest,TOperation) as TOperation;
end;

Function TInstanceGroupManagersResource.Delete(instanceGroupManager: string; project: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}';
  _Methodid   = 'replicapool.instanceGroupManagers.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceGroupManager',instanceGroupManager,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TInstanceGroupManagersResource.DeleteInstances(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersDeleteInstancesRequest : TInstanceGroupManagersDeleteInstancesRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/deleteInstances';
  _Methodid   = 'replicapool.instanceGroupManagers.deleteInstances';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceGroupManager',instanceGroupManager,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstanceGroupManagersDeleteInstancesRequest,TOperation) as TOperation;
end;

Function TInstanceGroupManagersResource.Get(instanceGroupManager: string; project: string; zone: string) : TInstanceGroupManager;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}';
  _Methodid   = 'replicapool.instanceGroupManagers.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceGroupManager',instanceGroupManager,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TInstanceGroupManager) as TInstanceGroupManager;
end;

Function TInstanceGroupManagersResource.Insert(project: string; zone: string; aInstanceGroupManager : TInstanceGroupManager; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers';
  _Methodid   = 'replicapool.instanceGroupManagers.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aInstanceGroupManager,TOperation) as TOperation;
end;


Function TInstanceGroupManagersResource.Insert(project: string; zone: string; aInstanceGroupManager : TInstanceGroupManager; AQuery : TInstanceGroupManagersinsertOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'size',AQuery.size);
  Result:=Insert(project,zone,aInstanceGroupManager,_Q);
end;

Function TInstanceGroupManagersResource.List(project: string; zone: string; AQuery : string = '') : TInstanceGroupManagerList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers';
  _Methodid   = 'replicapool.instanceGroupManagers.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TInstanceGroupManagerList) as TInstanceGroupManagerList;
end;


Function TInstanceGroupManagersResource.List(project: string; zone: string; AQuery : TInstanceGroupManagerslistOptions) : TInstanceGroupManagerList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,zone,_Q);
end;

Function TInstanceGroupManagersResource.RecreateInstances(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersRecreateInstancesRequest : TInstanceGroupManagersRecreateInstancesRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/recreateInstances';
  _Methodid   = 'replicapool.instanceGroupManagers.recreateInstances';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceGroupManager',instanceGroupManager,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstanceGroupManagersRecreateInstancesRequest,TOperation) as TOperation;
end;

Function TInstanceGroupManagersResource.Resize(instanceGroupManager: string; project: string; zone: string; AQuery : string = '') : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/resize';
  _Methodid   = 'replicapool.instanceGroupManagers.resize';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceGroupManager',instanceGroupManager,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TOperation) as TOperation;
end;


Function TInstanceGroupManagersResource.Resize(instanceGroupManager: string; project: string; zone: string; AQuery : TInstanceGroupManagersresizeOptions) : TOperation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'size',AQuery.size);
  Result:=Resize(instanceGroupManager,project,zone,_Q);
end;

Function TInstanceGroupManagersResource.SetInstanceTemplate(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersSetInstanceTemplateRequest : TInstanceGroupManagersSetInstanceTemplateRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/setInstanceTemplate';
  _Methodid   = 'replicapool.instanceGroupManagers.setInstanceTemplate';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceGroupManager',instanceGroupManager,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstanceGroupManagersSetInstanceTemplateRequest,TOperation) as TOperation;
end;

Function TInstanceGroupManagersResource.SetTargetPools(instanceGroupManager: string; project: string; zone: string; aInstanceGroupManagersSetTargetPoolsRequest : TInstanceGroupManagersSetTargetPoolsRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/instanceGroupManagers/{instanceGroupManager}/setTargetPools';
  _Methodid   = 'replicapool.instanceGroupManagers.setTargetPools';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['instanceGroupManager',instanceGroupManager,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aInstanceGroupManagersSetTargetPoolsRequest,TOperation) as TOperation;
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
  Result:=TreplicapoolAPI;
end;

Function TZoneOperationsResource.Get(operation: string; project: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/operations/{operation}';
  _Methodid   = 'replicapool.zoneOperations.get';

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
  _Methodid   = 'replicapool.zoneOperations.list';

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
  TReplicapoolAPI
  --------------------------------------------------------------------}

Class Function TReplicapoolAPI.APIName : String;

begin
  Result:='replicapool';
end;

Class Function TReplicapoolAPI.APIVersion : String;

begin
  Result:='v1beta2';
end;

Class Function TReplicapoolAPI.APIRevision : String;

begin
  Result:='20150223';
end;

Class Function TReplicapoolAPI.APIID : String;

begin
  Result:='replicapool:v1beta2';
end;

Class Function TReplicapoolAPI.APITitle : String;

begin
  Result:='Google Compute Engine Instance Group Manager API';
end;

Class Function TReplicapoolAPI.APIDescription : String;

begin
  Result:='The Google Compute Engine Instance Group Manager API provides groups of homogenous Compute Engine Instances.';
end;

Class Function TReplicapoolAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TReplicapoolAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TReplicapoolAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TReplicapoolAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TReplicapoolAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/compute/docs/instance-groups/manager/v1beta2';
end;

Class Function TReplicapoolAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TReplicapoolAPI.APIbasePath : string;

begin
  Result:='/replicapool/v1beta2/projects/';
end;

Class Function TReplicapoolAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/replicapool/v1beta2/projects/';
end;

Class Function TReplicapoolAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TReplicapoolAPI.APIservicePath : string;

begin
  Result:='replicapool/v1beta2/projects/';
end;

Class Function TReplicapoolAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TReplicapoolAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,3);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/compute';
  Result[1].Description:='View and manage your Google Compute Engine resources';
  Result[2].Name:='https://www.googleapis.com/auth/compute.readonly';
  Result[2].Description:='View your Google Compute Engine resources';
  
end;

Class Function TReplicapoolAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TReplicapoolAPI.RegisterAPIResources;

begin
  TInstanceGroupManager.RegisterObject;
  TInstanceGroupManagerList.RegisterObject;
  TInstanceGroupManagersAbandonInstancesRequest.RegisterObject;
  TInstanceGroupManagersDeleteInstancesRequest.RegisterObject;
  TInstanceGroupManagersRecreateInstancesRequest.RegisterObject;
  TInstanceGroupManagersSetInstanceTemplateRequest.RegisterObject;
  TInstanceGroupManagersSetTargetPoolsRequest.RegisterObject;
  TOperationTypeerrorTypeerrorsItem.RegisterObject;
  TOperationTypeerror.RegisterObject;
  TOperationTypewarningsItemTypedataItem.RegisterObject;
  TOperationTypewarningsItem.RegisterObject;
  TOperation.RegisterObject;
  TOperationList.RegisterObject;
end;


Function TReplicapoolAPI.GetInstanceGroupManagersInstance : TInstanceGroupManagersResource;

begin
  if (FInstanceGroupManagersInstance=Nil) then
    FInstanceGroupManagersInstance:=CreateInstanceGroupManagersResource;
  Result:=FInstanceGroupManagersInstance;
end;

Function TReplicapoolAPI.CreateInstanceGroupManagersResource : TInstanceGroupManagersResource;

begin
  Result:=CreateInstanceGroupManagersResource(Self);
end;


Function TReplicapoolAPI.CreateInstanceGroupManagersResource(AOwner : TComponent) : TInstanceGroupManagersResource;

begin
  Result:=TInstanceGroupManagersResource.Create(AOwner);
  Result.API:=Self;
end;



Function TReplicapoolAPI.GetZoneOperationsInstance : TZoneOperationsResource;

begin
  if (FZoneOperationsInstance=Nil) then
    FZoneOperationsInstance:=CreateZoneOperationsResource;
  Result:=FZoneOperationsInstance;
end;

Function TReplicapoolAPI.CreateZoneOperationsResource : TZoneOperationsResource;

begin
  Result:=CreateZoneOperationsResource(Self);
end;


Function TReplicapoolAPI.CreateZoneOperationsResource(AOwner : TComponent) : TZoneOperationsResource;

begin
  Result:=TZoneOperationsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TReplicapoolAPI.RegisterAPI;
end.
