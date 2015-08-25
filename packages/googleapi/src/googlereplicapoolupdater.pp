unit googlereplicapoolupdater;
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
//Generated on: 16-5-15 08:53:07
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TInstanceUpdate = Class;
  TInstanceUpdateList = Class;
  TOperation = Class;
  TRollingUpdate = Class;
  TRollingUpdateList = Class;
  TInstanceUpdateArray = Array of TInstanceUpdate;
  TInstanceUpdateListArray = Array of TInstanceUpdateList;
  TOperationArray = Array of TOperation;
  TRollingUpdateArray = Array of TRollingUpdate;
  TRollingUpdateListArray = Array of TRollingUpdateList;
  //Anonymous types, using auto-generated names
  TInstanceUpdateTypeerrorTypeerrorsItem = Class;
  TInstanceUpdateTypeerror = Class;
  TOperationTypeerrorTypeerrorsItem = Class;
  TOperationTypeerror = Class;
  TOperationTypewarningsItemTypedataItem = Class;
  TOperationTypewarningsItem = Class;
  TRollingUpdateTypeerrorTypeerrorsItem = Class;
  TRollingUpdateTypeerror = Class;
  TRollingUpdateTypepolicy = Class;
  TInstanceUpdateTypeerrorTypeerrorsArray = Array of TInstanceUpdateTypeerrorTypeerrorsItem;
  TInstanceUpdateListTypeitemsArray = Array of TInstanceUpdate;
  TOperationTypeerrorTypeerrorsArray = Array of TOperationTypeerrorTypeerrorsItem;
  TOperationTypewarningsItemTypedataArray = Array of TOperationTypewarningsItemTypedataItem;
  TOperationTypewarningsArray = Array of TOperationTypewarningsItem;
  TRollingUpdateTypeerrorTypeerrorsArray = Array of TRollingUpdateTypeerrorTypeerrorsItem;
  TRollingUpdateListTypeitemsArray = Array of TRollingUpdate;
  
  { --------------------------------------------------------------------
    TInstanceUpdateTypeerrorTypeerrorsItem
    --------------------------------------------------------------------}
  
  TInstanceUpdateTypeerrorTypeerrorsItem = Class(TGoogleBaseObject)
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
  TInstanceUpdateTypeerrorTypeerrorsItemClass = Class of TInstanceUpdateTypeerrorTypeerrorsItem;
  
  { --------------------------------------------------------------------
    TInstanceUpdateTypeerror
    --------------------------------------------------------------------}
  
  TInstanceUpdateTypeerror = Class(TGoogleBaseObject)
  Private
    Ferrors : TInstanceUpdateTypeerrorTypeerrorsArray;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; AValue : TInstanceUpdateTypeerrorTypeerrorsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property errors : TInstanceUpdateTypeerrorTypeerrorsArray Index 0 Read Ferrors Write Seterrors;
  end;
  TInstanceUpdateTypeerrorClass = Class of TInstanceUpdateTypeerror;
  
  { --------------------------------------------------------------------
    TInstanceUpdate
    --------------------------------------------------------------------}
  
  TInstanceUpdate = Class(TGoogleBaseObject)
  Private
    Ferror : TInstanceUpdateTypeerror;
    Finstance : String;
    Fstatus : String;
  Protected
    //Property setters
    Procedure Seterror(AIndex : Integer; AValue : TInstanceUpdateTypeerror); virtual;
    Procedure Setinstance(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property error : TInstanceUpdateTypeerror Index 0 Read Ferror Write Seterror;
    Property instance : String Index 8 Read Finstance Write Setinstance;
    Property status : String Index 16 Read Fstatus Write Setstatus;
  end;
  TInstanceUpdateClass = Class of TInstanceUpdate;
  
  { --------------------------------------------------------------------
    TInstanceUpdateList
    --------------------------------------------------------------------}
  
  TInstanceUpdateList = Class(TGoogleBaseObject)
  Private
    Fitems : TInstanceUpdateListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FselfLink : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TInstanceUpdateListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TInstanceUpdateListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : String Index 24 Read FselfLink Write SetselfLink;
  end;
  TInstanceUpdateListClass = Class of TInstanceUpdateList;
  
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
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
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
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
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
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
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
    TRollingUpdateTypeerrorTypeerrorsItem
    --------------------------------------------------------------------}
  
  TRollingUpdateTypeerrorTypeerrorsItem = Class(TGoogleBaseObject)
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
  TRollingUpdateTypeerrorTypeerrorsItemClass = Class of TRollingUpdateTypeerrorTypeerrorsItem;
  
  { --------------------------------------------------------------------
    TRollingUpdateTypeerror
    --------------------------------------------------------------------}
  
  TRollingUpdateTypeerror = Class(TGoogleBaseObject)
  Private
    Ferrors : TRollingUpdateTypeerrorTypeerrorsArray;
  Protected
    //Property setters
    Procedure Seterrors(AIndex : Integer; AValue : TRollingUpdateTypeerrorTypeerrorsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property errors : TRollingUpdateTypeerrorTypeerrorsArray Index 0 Read Ferrors Write Seterrors;
  end;
  TRollingUpdateTypeerrorClass = Class of TRollingUpdateTypeerror;
  
  { --------------------------------------------------------------------
    TRollingUpdateTypepolicy
    --------------------------------------------------------------------}
  
  TRollingUpdateTypepolicy = Class(TGoogleBaseObject)
  Private
    FautoPauseAfterInstances : integer;
    FinstanceStartupTimeoutSec : integer;
    FmaxNumConcurrentInstances : integer;
    FmaxNumFailedInstances : integer;
    FminInstanceUpdateTimeSec : integer;
  Protected
    //Property setters
    Procedure SetautoPauseAfterInstances(AIndex : Integer; AValue : integer); virtual;
    Procedure SetinstanceStartupTimeoutSec(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmaxNumConcurrentInstances(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmaxNumFailedInstances(AIndex : Integer; AValue : integer); virtual;
    Procedure SetminInstanceUpdateTimeSec(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property autoPauseAfterInstances : integer Index 0 Read FautoPauseAfterInstances Write SetautoPauseAfterInstances;
    Property instanceStartupTimeoutSec : integer Index 8 Read FinstanceStartupTimeoutSec Write SetinstanceStartupTimeoutSec;
    Property maxNumConcurrentInstances : integer Index 16 Read FmaxNumConcurrentInstances Write SetmaxNumConcurrentInstances;
    Property maxNumFailedInstances : integer Index 24 Read FmaxNumFailedInstances Write SetmaxNumFailedInstances;
    Property minInstanceUpdateTimeSec : integer Index 32 Read FminInstanceUpdateTimeSec Write SetminInstanceUpdateTimeSec;
  end;
  TRollingUpdateTypepolicyClass = Class of TRollingUpdateTypepolicy;
  
  { --------------------------------------------------------------------
    TRollingUpdate
    --------------------------------------------------------------------}
  
  TRollingUpdate = Class(TGoogleBaseObject)
  Private
    FactionType : String;
    FcreationTimestamp : String;
    Fdescription : String;
    Ferror : TRollingUpdateTypeerror;
    Fid : String;
    FinstanceGroup : String;
    FinstanceGroupManager : String;
    FinstanceTemplate : String;
    Fkind : String;
    Fpolicy : TRollingUpdateTypepolicy;
    Fprogress : integer;
    FselfLink : String;
    Fstatus : String;
    FstatusMessage : String;
    Fuser : String;
  Protected
    //Property setters
    Procedure SetactionType(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreationTimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Seterror(AIndex : Integer; AValue : TRollingUpdateTypeerror); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetinstanceGroup(AIndex : Integer; AValue : String); virtual;
    Procedure SetinstanceGroupManager(AIndex : Integer; AValue : String); virtual;
    Procedure SetinstanceTemplate(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setpolicy(AIndex : Integer; AValue : TRollingUpdateTypepolicy); virtual;
    Procedure Setprogress(AIndex : Integer; AValue : integer); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure SetstatusMessage(AIndex : Integer; AValue : String); virtual;
    Procedure Setuser(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property actionType : String Index 0 Read FactionType Write SetactionType;
    Property creationTimestamp : String Index 8 Read FcreationTimestamp Write SetcreationTimestamp;
    Property description : String Index 16 Read Fdescription Write Setdescription;
    Property error : TRollingUpdateTypeerror Index 24 Read Ferror Write Seterror;
    Property id : String Index 32 Read Fid Write Setid;
    Property instanceGroup : String Index 40 Read FinstanceGroup Write SetinstanceGroup;
    Property instanceGroupManager : String Index 48 Read FinstanceGroupManager Write SetinstanceGroupManager;
    Property instanceTemplate : String Index 56 Read FinstanceTemplate Write SetinstanceTemplate;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property policy : TRollingUpdateTypepolicy Index 72 Read Fpolicy Write Setpolicy;
    Property progress : integer Index 80 Read Fprogress Write Setprogress;
    Property selfLink : String Index 88 Read FselfLink Write SetselfLink;
    Property status : String Index 96 Read Fstatus Write Setstatus;
    Property statusMessage : String Index 104 Read FstatusMessage Write SetstatusMessage;
    Property user : String Index 112 Read Fuser Write Setuser;
  end;
  TRollingUpdateClass = Class of TRollingUpdate;
  
  { --------------------------------------------------------------------
    TRollingUpdateList
    --------------------------------------------------------------------}
  
  TRollingUpdateList = Class(TGoogleBaseObject)
  Private
    Fitems : TRollingUpdateListTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
    FselfLink : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TRollingUpdateListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetselfLink(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TRollingUpdateListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
    Property selfLink : String Index 24 Read FselfLink Write SetselfLink;
  end;
  TRollingUpdateListClass = Class of TRollingUpdateList;
  
  { --------------------------------------------------------------------
    TRollingUpdatesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TRollingUpdatesResource, method List
  
  TRollingUpdatesListOptions = Record
    filter : String;
    instanceGroupManager : String;
    maxResults : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TRollingUpdatesResource, method ListInstanceUpdates
  
  TRollingUpdatesListInstanceUpdatesOptions = Record
    filter : String;
    maxResults : integer;
    pageToken : String;
  end;
  
  TRollingUpdatesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Cancel(project: string; rollingUpdate: string; zone: string) : TOperation;
    Function Get(project: string; rollingUpdate: string; zone: string) : TRollingUpdate;
    Function Insert(project: string; zone: string; aRollingUpdate : TRollingUpdate) : TOperation;
    Function List(project: string; zone: string; AQuery : string  = '') : TRollingUpdateList;
    Function List(project: string; zone: string; AQuery : TRollingUpdateslistOptions) : TRollingUpdateList;
    Function ListInstanceUpdates(project: string; rollingUpdate: string; zone: string; AQuery : string  = '') : TInstanceUpdateList;
    Function ListInstanceUpdates(project: string; rollingUpdate: string; zone: string; AQuery : TRollingUpdateslistInstanceUpdatesOptions) : TInstanceUpdateList;
    Function Pause(project: string; rollingUpdate: string; zone: string) : TOperation;
    Function Resume(project: string; rollingUpdate: string; zone: string) : TOperation;
    Function Rollback(project: string; rollingUpdate: string; zone: string) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TZoneOperationsResource
    --------------------------------------------------------------------}
  
  TZoneOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(operation: string; project: string; zone: string) : TOperation;
  end;
  
  
  { --------------------------------------------------------------------
    TReplicapoolupdaterAPI
    --------------------------------------------------------------------}
  
  TReplicapoolupdaterAPI = Class(TGoogleAPI)
  Private
    FRollingUpdatesInstance : TRollingUpdatesResource;
    FZoneOperationsInstance : TZoneOperationsResource;
    Function GetRollingUpdatesInstance : TRollingUpdatesResource;virtual;
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
    Function CreateRollingUpdatesResource(AOwner : TComponent) : TRollingUpdatesResource;virtual;overload;
    Function CreateRollingUpdatesResource : TRollingUpdatesResource;virtual;overload;
    Function CreateZoneOperationsResource(AOwner : TComponent) : TZoneOperationsResource;virtual;overload;
    Function CreateZoneOperationsResource : TZoneOperationsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property RollingUpdatesResource : TRollingUpdatesResource Read GetRollingUpdatesInstance;
    Property ZoneOperationsResource : TZoneOperationsResource Read GetZoneOperationsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TInstanceUpdateTypeerrorTypeerrorsItem
  --------------------------------------------------------------------}


Procedure TInstanceUpdateTypeerrorTypeerrorsItem.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceUpdateTypeerrorTypeerrorsItem.Setlocation(AIndex : Integer; AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceUpdateTypeerrorTypeerrorsItem.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceUpdateTypeerror
  --------------------------------------------------------------------}


Procedure TInstanceUpdateTypeerror.Seterrors(AIndex : Integer; AValue : TInstanceUpdateTypeerrorTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TInstanceUpdateTypeerror.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TInstanceUpdate
  --------------------------------------------------------------------}


Procedure TInstanceUpdate.Seterror(AIndex : Integer; AValue : TInstanceUpdateTypeerror); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceUpdate.Setinstance(AIndex : Integer; AValue : String); 

begin
  If (Finstance=AValue) then exit;
  Finstance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceUpdate.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInstanceUpdateList
  --------------------------------------------------------------------}


Procedure TInstanceUpdateList.Setitems(AIndex : Integer; AValue : TInstanceUpdateListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceUpdateList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceUpdateList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInstanceUpdateList.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TInstanceUpdateList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperationTypeerror.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperationTypewarningsItem.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'data' : SetLength(Fdata,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperation.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'warnings' : SetLength(Fwarnings,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TRollingUpdateTypeerrorTypeerrorsItem
  --------------------------------------------------------------------}


Procedure TRollingUpdateTypeerrorTypeerrorsItem.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdateTypeerrorTypeerrorsItem.Setlocation(AIndex : Integer; AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdateTypeerrorTypeerrorsItem.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRollingUpdateTypeerror
  --------------------------------------------------------------------}


Procedure TRollingUpdateTypeerror.Seterrors(AIndex : Integer; AValue : TRollingUpdateTypeerrorTypeerrorsArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TRollingUpdateTypeerror.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TRollingUpdateTypepolicy
  --------------------------------------------------------------------}


Procedure TRollingUpdateTypepolicy.SetautoPauseAfterInstances(AIndex : Integer; AValue : integer); 

begin
  If (FautoPauseAfterInstances=AValue) then exit;
  FautoPauseAfterInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdateTypepolicy.SetinstanceStartupTimeoutSec(AIndex : Integer; AValue : integer); 

begin
  If (FinstanceStartupTimeoutSec=AValue) then exit;
  FinstanceStartupTimeoutSec:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdateTypepolicy.SetmaxNumConcurrentInstances(AIndex : Integer; AValue : integer); 

begin
  If (FmaxNumConcurrentInstances=AValue) then exit;
  FmaxNumConcurrentInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdateTypepolicy.SetmaxNumFailedInstances(AIndex : Integer; AValue : integer); 

begin
  If (FmaxNumFailedInstances=AValue) then exit;
  FmaxNumFailedInstances:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdateTypepolicy.SetminInstanceUpdateTimeSec(AIndex : Integer; AValue : integer); 

begin
  If (FminInstanceUpdateTimeSec=AValue) then exit;
  FminInstanceUpdateTimeSec:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRollingUpdate
  --------------------------------------------------------------------}


Procedure TRollingUpdate.SetactionType(AIndex : Integer; AValue : String); 

begin
  If (FactionType=AValue) then exit;
  FactionType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.SetcreationTimestamp(AIndex : Integer; AValue : String); 

begin
  If (FcreationTimestamp=AValue) then exit;
  FcreationTimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.Seterror(AIndex : Integer; AValue : TRollingUpdateTypeerror); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.SetinstanceGroup(AIndex : Integer; AValue : String); 

begin
  If (FinstanceGroup=AValue) then exit;
  FinstanceGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.SetinstanceGroupManager(AIndex : Integer; AValue : String); 

begin
  If (FinstanceGroupManager=AValue) then exit;
  FinstanceGroupManager:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.SetinstanceTemplate(AIndex : Integer; AValue : String); 

begin
  If (FinstanceTemplate=AValue) then exit;
  FinstanceTemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.Setpolicy(AIndex : Integer; AValue : TRollingUpdateTypepolicy); 

begin
  If (Fpolicy=AValue) then exit;
  Fpolicy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.Setprogress(AIndex : Integer; AValue : integer); 

begin
  If (Fprogress=AValue) then exit;
  Fprogress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.SetstatusMessage(AIndex : Integer; AValue : String); 

begin
  If (FstatusMessage=AValue) then exit;
  FstatusMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdate.Setuser(AIndex : Integer; AValue : String); 

begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRollingUpdateList
  --------------------------------------------------------------------}


Procedure TRollingUpdateList.Setitems(AIndex : Integer; AValue : TRollingUpdateListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdateList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdateList.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRollingUpdateList.SetselfLink(AIndex : Integer; AValue : String); 

begin
  If (FselfLink=AValue) then exit;
  FselfLink:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TRollingUpdateList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TRollingUpdatesResource
  --------------------------------------------------------------------}


Class Function TRollingUpdatesResource.ResourceName : String;

begin
  Result:='rollingUpdates';
end;

Class Function TRollingUpdatesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TreplicapoolupdaterAPI;
end;

Function TRollingUpdatesResource.Cancel(project: string; rollingUpdate: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/rollingUpdates/{rollingUpdate}/cancel';
  _Methodid   = 'replicapoolupdater.rollingUpdates.cancel';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'rollingUpdate',rollingUpdate,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TRollingUpdatesResource.Get(project: string; rollingUpdate: string; zone: string) : TRollingUpdate;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/rollingUpdates/{rollingUpdate}';
  _Methodid   = 'replicapoolupdater.rollingUpdates.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'rollingUpdate',rollingUpdate,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TRollingUpdate) as TRollingUpdate;
end;

Function TRollingUpdatesResource.Insert(project: string; zone: string; aRollingUpdate : TRollingUpdate) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/rollingUpdates';
  _Methodid   = 'replicapoolupdater.rollingUpdates.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRollingUpdate,TOperation) as TOperation;
end;

Function TRollingUpdatesResource.List(project: string; zone: string; AQuery : string = '') : TRollingUpdateList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/rollingUpdates';
  _Methodid   = 'replicapoolupdater.rollingUpdates.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TRollingUpdateList) as TRollingUpdateList;
end;


Function TRollingUpdatesResource.List(project: string; zone: string; AQuery : TRollingUpdateslistOptions) : TRollingUpdateList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'instanceGroupManager',AQuery.instanceGroupManager);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,zone,_Q);
end;

Function TRollingUpdatesResource.ListInstanceUpdates(project: string; rollingUpdate: string; zone: string; AQuery : string = '') : TInstanceUpdateList;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/rollingUpdates/{rollingUpdate}/instanceUpdates';
  _Methodid   = 'replicapoolupdater.rollingUpdates.listInstanceUpdates';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'rollingUpdate',rollingUpdate,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TInstanceUpdateList) as TInstanceUpdateList;
end;


Function TRollingUpdatesResource.ListInstanceUpdates(project: string; rollingUpdate: string; zone: string; AQuery : TRollingUpdateslistInstanceUpdatesOptions) : TInstanceUpdateList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=ListInstanceUpdates(project,rollingUpdate,zone,_Q);
end;

Function TRollingUpdatesResource.Pause(project: string; rollingUpdate: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/rollingUpdates/{rollingUpdate}/pause';
  _Methodid   = 'replicapoolupdater.rollingUpdates.pause';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'rollingUpdate',rollingUpdate,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TRollingUpdatesResource.Resume(project: string; rollingUpdate: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/rollingUpdates/{rollingUpdate}/resume';
  _Methodid   = 'replicapoolupdater.rollingUpdates.resume';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'rollingUpdate',rollingUpdate,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TRollingUpdatesResource.Rollback(project: string; rollingUpdate: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/zones/{zone}/rollingUpdates/{rollingUpdate}/rollback';
  _Methodid   = 'replicapoolupdater.rollingUpdates.rollback';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project,'rollingUpdate',rollingUpdate,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
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
  Result:=TreplicapoolupdaterAPI;
end;

Function TZoneOperationsResource.Get(operation: string; project: string; zone: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/zones/{zone}/operations/{operation}';
  _Methodid   = 'replicapoolupdater.zoneOperations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['operation',operation,'project',project,'zone',zone]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;



{ --------------------------------------------------------------------
  TReplicapoolupdaterAPI
  --------------------------------------------------------------------}

Class Function TReplicapoolupdaterAPI.APIName : String;

begin
  Result:='replicapoolupdater';
end;

Class Function TReplicapoolupdaterAPI.APIVersion : String;

begin
  Result:='v1beta1';
end;

Class Function TReplicapoolupdaterAPI.APIRevision : String;

begin
  Result:='20150326';
end;

Class Function TReplicapoolupdaterAPI.APIID : String;

begin
  Result:='replicapoolupdater:v1beta1';
end;

Class Function TReplicapoolupdaterAPI.APITitle : String;

begin
  Result:='Google Compute Engine Instance Group Updater API';
end;

Class Function TReplicapoolupdaterAPI.APIDescription : String;

begin
  Result:='The Google Compute Engine Instance Group Updater API provides services for updating groups of Compute Engine Instances.';
end;

Class Function TReplicapoolupdaterAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TReplicapoolupdaterAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TReplicapoolupdaterAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TReplicapoolupdaterAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TReplicapoolupdaterAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/compute/docs/instance-groups/manager/#applying_rolling_updates_using_the_updater_service';
end;

Class Function TReplicapoolupdaterAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TReplicapoolupdaterAPI.APIbasePath : string;

begin
  Result:='/replicapoolupdater/v1beta1/projects/';
end;

Class Function TReplicapoolupdaterAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/replicapoolupdater/v1beta1/projects/';
end;

Class Function TReplicapoolupdaterAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TReplicapoolupdaterAPI.APIservicePath : string;

begin
  Result:='replicapoolupdater/v1beta1/projects/';
end;

Class Function TReplicapoolupdaterAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TReplicapoolupdaterAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,3);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/replicapool';
  Result[1].Description:='View and manage replica pools';
  Result[2].Name:='https://www.googleapis.com/auth/replicapool.readonly';
  Result[2].Description:='View replica pools';
  
end;

Class Function TReplicapoolupdaterAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TReplicapoolupdaterAPI.RegisterAPIResources;

begin
  TInstanceUpdateTypeerrorTypeerrorsItem.RegisterObject;
  TInstanceUpdateTypeerror.RegisterObject;
  TInstanceUpdate.RegisterObject;
  TInstanceUpdateList.RegisterObject;
  TOperationTypeerrorTypeerrorsItem.RegisterObject;
  TOperationTypeerror.RegisterObject;
  TOperationTypewarningsItemTypedataItem.RegisterObject;
  TOperationTypewarningsItem.RegisterObject;
  TOperation.RegisterObject;
  TRollingUpdateTypeerrorTypeerrorsItem.RegisterObject;
  TRollingUpdateTypeerror.RegisterObject;
  TRollingUpdateTypepolicy.RegisterObject;
  TRollingUpdate.RegisterObject;
  TRollingUpdateList.RegisterObject;
end;


Function TReplicapoolupdaterAPI.GetRollingUpdatesInstance : TRollingUpdatesResource;

begin
  if (FRollingUpdatesInstance=Nil) then
    FRollingUpdatesInstance:=CreateRollingUpdatesResource;
  Result:=FRollingUpdatesInstance;
end;

Function TReplicapoolupdaterAPI.CreateRollingUpdatesResource : TRollingUpdatesResource;

begin
  Result:=CreateRollingUpdatesResource(Self);
end;


Function TReplicapoolupdaterAPI.CreateRollingUpdatesResource(AOwner : TComponent) : TRollingUpdatesResource;

begin
  Result:=TRollingUpdatesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TReplicapoolupdaterAPI.GetZoneOperationsInstance : TZoneOperationsResource;

begin
  if (FZoneOperationsInstance=Nil) then
    FZoneOperationsInstance:=CreateZoneOperationsResource;
  Result:=FZoneOperationsInstance;
end;

Function TReplicapoolupdaterAPI.CreateZoneOperationsResource : TZoneOperationsResource;

begin
  Result:=CreateZoneOperationsResource(Self);
end;


Function TReplicapoolupdaterAPI.CreateZoneOperationsResource(AOwner : TComponent) : TZoneOperationsResource;

begin
  Result:=TZoneOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TReplicapoolupdaterAPI.RegisterAPI;
end.
