unit googledns;
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
//Generated on: 9-5-15 13:22:53
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TChange = class;
  TChangesListResponse = class;
  TManagedZone = class;
  TManagedZonesListResponse = class;
  TProject = class;
  TQuota = class;
  TResourceRecordSet = class;
  TResourceRecordSetsListResponse = class;
  TChangeArray = Array of TChange;
  TChangesListResponseArray = Array of TChangesListResponse;
  TManagedZoneArray = Array of TManagedZone;
  TManagedZonesListResponseArray = Array of TManagedZonesListResponse;
  TProjectArray = Array of TProject;
  TQuotaArray = Array of TQuota;
  TResourceRecordSetArray = Array of TResourceRecordSet;
  TResourceRecordSetsListResponseArray = Array of TResourceRecordSetsListResponse;
  //Anonymous types, using auto-generated names
  TChangeTypeadditionsArray = Array of TResourceRecordSet;
  TChangeTypedeletionsArray = Array of TResourceRecordSet;
  TChangesListResponseTypechangesArray = Array of TChange;
  TManagedZonesListResponseTypemanagedZonesArray = Array of TManagedZone;
  TResourceRecordSetsListResponseTyperrsetsArray = Array of TResourceRecordSet;
  
  { --------------------------------------------------------------------
    TChange
    --------------------------------------------------------------------}
  
  TChange = Class(TGoogleBaseObject)
  Private
    Fadditions : TChangeTypeadditionsArray;
    Fdeletions : TChangeTypedeletionsArray;
    Fid : String;
    Fkind : String;
    FstartTime : String;
    Fstatus : String;
  Protected
    //Property setters
    Procedure Setadditions(AIndex : Integer; AValue : TChangeTypeadditionsArray); virtual;
    Procedure Setdeletions(AIndex : Integer; AValue : TChangeTypedeletionsArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property additions : TChangeTypeadditionsArray Index 0 Read Fadditions Write Setadditions;
    Property deletions : TChangeTypedeletionsArray Index 8 Read Fdeletions Write Setdeletions;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property startTime : String Index 32 Read FstartTime Write SetstartTime;
    Property status : String Index 40 Read Fstatus Write Setstatus;
  end;
  TChangeClass = Class of TChange;
  
  { --------------------------------------------------------------------
    TChangesListResponse
    --------------------------------------------------------------------}
  
  TChangesListResponse = Class(TGoogleBaseObject)
  Private
    Fchanges : TChangesListResponseTypechangesArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setchanges(AIndex : Integer; AValue : TChangesListResponseTypechangesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property changes : TChangesListResponseTypechangesArray Index 0 Read Fchanges Write Setchanges;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TChangesListResponseClass = Class of TChangesListResponse;
  
  { --------------------------------------------------------------------
    TManagedZone
    --------------------------------------------------------------------}
  
  TManagedZone = Class(TGoogleBaseObject)
  Private
    FcreationTime : String;
    Fdescription : String;
    FdnsName : String;
    Fid : String;
    Fkind : String;
    Fname : String;
    FnameServerSet : String;
    FnameServers : TStringArray;
  Protected
    //Property setters
    Procedure SetcreationTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure SetdnsName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetnameServerSet(AIndex : Integer; AValue : String); virtual;
    Procedure SetnameServers(AIndex : Integer; AValue : TStringArray); virtual;
  Public
  Published
    Property creationTime : String Index 0 Read FcreationTime Write SetcreationTime;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property dnsName : String Index 16 Read FdnsName Write SetdnsName;
    Property id : String Index 24 Read Fid Write Setid;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property name : String Index 40 Read Fname Write Setname;
    Property nameServerSet : String Index 48 Read FnameServerSet Write SetnameServerSet;
    Property nameServers : TStringArray Index 56 Read FnameServers Write SetnameServers;
  end;
  TManagedZoneClass = Class of TManagedZone;
  
  { --------------------------------------------------------------------
    TManagedZonesListResponse
    --------------------------------------------------------------------}
  
  TManagedZonesListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FmanagedZones : TManagedZonesListResponseTypemanagedZonesArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmanagedZones(AIndex : Integer; AValue : TManagedZonesListResponseTypemanagedZonesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property managedZones : TManagedZonesListResponseTypemanagedZonesArray Index 8 Read FmanagedZones Write SetmanagedZones;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TManagedZonesListResponseClass = Class of TManagedZonesListResponse;
  
  { --------------------------------------------------------------------
    TProject
    --------------------------------------------------------------------}
  
  TProject = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    Fnumber : String;
    Fquota : TQuota;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setnumber(AIndex : Integer; AValue : String); virtual;
    Procedure Setquota(AIndex : Integer; AValue : TQuota); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property number : String Index 16 Read Fnumber Write Setnumber;
    Property quota : TQuota Index 24 Read Fquota Write Setquota;
  end;
  TProjectClass = Class of TProject;
  
  { --------------------------------------------------------------------
    TQuota
    --------------------------------------------------------------------}
  
  TQuota = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FmanagedZones : integer;
    FresourceRecordsPerRrset : integer;
    FrrsetAdditionsPerChange : integer;
    FrrsetDeletionsPerChange : integer;
    FrrsetsPerManagedZone : integer;
    FtotalRrdataSizePerChange : integer;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmanagedZones(AIndex : Integer; AValue : integer); virtual;
    Procedure SetresourceRecordsPerRrset(AIndex : Integer; AValue : integer); virtual;
    Procedure SetrrsetAdditionsPerChange(AIndex : Integer; AValue : integer); virtual;
    Procedure SetrrsetDeletionsPerChange(AIndex : Integer; AValue : integer); virtual;
    Procedure SetrrsetsPerManagedZone(AIndex : Integer; AValue : integer); virtual;
    Procedure SettotalRrdataSizePerChange(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property managedZones : integer Index 8 Read FmanagedZones Write SetmanagedZones;
    Property resourceRecordsPerRrset : integer Index 16 Read FresourceRecordsPerRrset Write SetresourceRecordsPerRrset;
    Property rrsetAdditionsPerChange : integer Index 24 Read FrrsetAdditionsPerChange Write SetrrsetAdditionsPerChange;
    Property rrsetDeletionsPerChange : integer Index 32 Read FrrsetDeletionsPerChange Write SetrrsetDeletionsPerChange;
    Property rrsetsPerManagedZone : integer Index 40 Read FrrsetsPerManagedZone Write SetrrsetsPerManagedZone;
    Property totalRrdataSizePerChange : integer Index 48 Read FtotalRrdataSizePerChange Write SettotalRrdataSizePerChange;
  end;
  TQuotaClass = Class of TQuota;
  
  { --------------------------------------------------------------------
    TResourceRecordSet
    --------------------------------------------------------------------}
  
  TResourceRecordSet = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fname : String;
    Frrdatas : TStringArray;
    Fttl : integer;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setrrdatas(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setttl(AIndex : Integer; AValue : integer); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property name : String Index 8 Read Fname Write Setname;
    Property rrdatas : TStringArray Index 16 Read Frrdatas Write Setrrdatas;
    Property ttl : integer Index 24 Read Fttl Write Setttl;
    Property _type : String Index 32 Read F_type Write Set_type;
  end;
  TResourceRecordSetClass = Class of TResourceRecordSet;
  
  { --------------------------------------------------------------------
    TResourceRecordSetsListResponse
    --------------------------------------------------------------------}
  
  TResourceRecordSetsListResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    FnextPageToken : String;
    Frrsets : TResourceRecordSetsListResponseTyperrsetsArray;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setrrsets(AIndex : Integer; AValue : TResourceRecordSetsListResponseTyperrsetsArray); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property rrsets : TResourceRecordSetsListResponseTyperrsetsArray Index 16 Read Frrsets Write Setrrsets;
  end;
  TResourceRecordSetsListResponseClass = Class of TResourceRecordSetsListResponse;
  
  { --------------------------------------------------------------------
    TChangesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TChangesResource, method List
  
  TChangesListOptions = Record
    maxResults : integer;
    pageToken : String;
    sortBy : String;
    sortOrder : String;
  end;
  
  TChangesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(managedZone: string; project: string; aChange : TChange) : TChange;overload;
    Function Get(changeId: string; managedZone: string; project: string) : TChange;
    Function List(managedZone: string; project: string; AQuery : string  = '') : TChangesListResponse;
    Function List(managedZone: string; project: string; AQuery : TChangeslistOptions) : TChangesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TManagedZonesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TManagedZonesResource, method List
  
  TManagedZonesListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TManagedZonesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(project: string; aManagedZone : TManagedZone) : TManagedZone;overload;
    Procedure Delete(managedZone: string; project: string);
    Function Get(managedZone: string; project: string) : TManagedZone;
    Function List(project: string; AQuery : string  = '') : TManagedZonesListResponse;
    Function List(project: string; AQuery : TManagedZoneslistOptions) : TManagedZonesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(project: string) : TProject;
  end;
  
  
  { --------------------------------------------------------------------
    TResourceRecordSetsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TResourceRecordSetsResource, method List
  
  TResourceRecordSetsListOptions = Record
    maxResults : integer;
    _name : String;
    pageToken : String;
    _type : String;
  end;
  
  TResourceRecordSetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(managedZone: string; project: string; AQuery : string  = '') : TResourceRecordSetsListResponse;
    Function List(managedZone: string; project: string; AQuery : TResourceRecordSetslistOptions) : TResourceRecordSetsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TDnsAPI
    --------------------------------------------------------------------}
  
  TDnsAPI = Class(TGoogleAPI)
  Private
    FChangesInstance : TChangesResource;
    FManagedZonesInstance : TManagedZonesResource;
    FProjectsInstance : TProjectsResource;
    FResourceRecordSetsInstance : TResourceRecordSetsResource;
    Function GetChangesInstance : TChangesResource;virtual;
    Function GetManagedZonesInstance : TManagedZonesResource;virtual;
    Function GetProjectsInstance : TProjectsResource;virtual;
    Function GetResourceRecordSetsInstance : TResourceRecordSetsResource;virtual;
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
    Function CreateChangesResource(AOwner : TComponent) : TChangesResource;virtual;overload;
    Function CreateChangesResource : TChangesResource;virtual;overload;
    Function CreateManagedZonesResource(AOwner : TComponent) : TManagedZonesResource;virtual;overload;
    Function CreateManagedZonesResource : TManagedZonesResource;virtual;overload;
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    Function CreateResourceRecordSetsResource(AOwner : TComponent) : TResourceRecordSetsResource;virtual;overload;
    Function CreateResourceRecordSetsResource : TResourceRecordSetsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ChangesResource : TChangesResource Read GetChangesInstance;
    Property ManagedZonesResource : TManagedZonesResource Read GetManagedZonesInstance;
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
    Property ResourceRecordSetsResource : TResourceRecordSetsResource Read GetResourceRecordSetsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TChange
  --------------------------------------------------------------------}


Procedure TChange.Setadditions(AIndex : Integer; AValue : TChangeTypeadditionsArray); 

begin
  If (Fadditions=AValue) then exit;
  Fadditions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChange.Setdeletions(AIndex : Integer; AValue : TChangeTypedeletionsArray); 

begin
  If (Fdeletions=AValue) then exit;
  Fdeletions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChange.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChange.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChange.SetstartTime(AIndex : Integer; AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChange.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChangesListResponse
  --------------------------------------------------------------------}


Procedure TChangesListResponse.Setchanges(AIndex : Integer; AValue : TChangesListResponseTypechangesArray); 

begin
  If (Fchanges=AValue) then exit;
  Fchanges:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TChangesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TManagedZone
  --------------------------------------------------------------------}


Procedure TManagedZone.SetcreationTime(AIndex : Integer; AValue : String); 

begin
  If (FcreationTime=AValue) then exit;
  FcreationTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManagedZone.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManagedZone.SetdnsName(AIndex : Integer; AValue : String); 

begin
  If (FdnsName=AValue) then exit;
  FdnsName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManagedZone.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManagedZone.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManagedZone.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManagedZone.SetnameServerSet(AIndex : Integer; AValue : String); 

begin
  If (FnameServerSet=AValue) then exit;
  FnameServerSet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManagedZone.SetnameServers(AIndex : Integer; AValue : TStringArray); 

begin
  If (FnameServers=AValue) then exit;
  FnameServers:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TManagedZonesListResponse
  --------------------------------------------------------------------}


Procedure TManagedZonesListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManagedZonesListResponse.SetmanagedZones(AIndex : Integer; AValue : TManagedZonesListResponseTypemanagedZonesArray); 

begin
  If (FmanagedZones=AValue) then exit;
  FmanagedZones:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TManagedZonesListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProject
  --------------------------------------------------------------------}


Procedure TProject.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setnumber(AIndex : Integer; AValue : String); 

begin
  If (Fnumber=AValue) then exit;
  Fnumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProject.Setquota(AIndex : Integer; AValue : TQuota); 

begin
  If (Fquota=AValue) then exit;
  Fquota:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQuota
  --------------------------------------------------------------------}


Procedure TQuota.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuota.SetmanagedZones(AIndex : Integer; AValue : integer); 

begin
  If (FmanagedZones=AValue) then exit;
  FmanagedZones:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuota.SetresourceRecordsPerRrset(AIndex : Integer; AValue : integer); 

begin
  If (FresourceRecordsPerRrset=AValue) then exit;
  FresourceRecordsPerRrset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuota.SetrrsetAdditionsPerChange(AIndex : Integer; AValue : integer); 

begin
  If (FrrsetAdditionsPerChange=AValue) then exit;
  FrrsetAdditionsPerChange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuota.SetrrsetDeletionsPerChange(AIndex : Integer; AValue : integer); 

begin
  If (FrrsetDeletionsPerChange=AValue) then exit;
  FrrsetDeletionsPerChange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuota.SetrrsetsPerManagedZone(AIndex : Integer; AValue : integer); 

begin
  If (FrrsetsPerManagedZone=AValue) then exit;
  FrrsetsPerManagedZone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuota.SettotalRrdataSizePerChange(AIndex : Integer; AValue : integer); 

begin
  If (FtotalRrdataSizePerChange=AValue) then exit;
  FtotalRrdataSizePerChange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResourceRecordSet
  --------------------------------------------------------------------}


Procedure TResourceRecordSet.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceRecordSet.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceRecordSet.Setrrdatas(AIndex : Integer; AValue : TStringArray); 

begin
  If (Frrdatas=AValue) then exit;
  Frrdatas:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceRecordSet.Setttl(AIndex : Integer; AValue : integer); 

begin
  If (Fttl=AValue) then exit;
  Fttl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceRecordSet.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TResourceRecordSet.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TResourceRecordSetsListResponse
  --------------------------------------------------------------------}


Procedure TResourceRecordSetsListResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceRecordSetsListResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResourceRecordSetsListResponse.Setrrsets(AIndex : Integer; AValue : TResourceRecordSetsListResponseTyperrsetsArray); 

begin
  If (Frrsets=AValue) then exit;
  Frrsets:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChangesResource
  --------------------------------------------------------------------}


Class Function TChangesResource.ResourceName : String;

begin
  Result:='changes';
end;

Class Function TChangesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdnsAPI;
end;

Function TChangesResource.Create(managedZone: string; project: string; aChange : TChange) : TChange;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/managedZones/{managedZone}/changes';
  _Methodid   = 'dns.changes.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['managedZone',managedZone,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aChange,TChange) as TChange;
end;

Function TChangesResource.Get(changeId: string; managedZone: string; project: string) : TChange;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/managedZones/{managedZone}/changes/{changeId}';
  _Methodid   = 'dns.changes.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['changeId',changeId,'managedZone',managedZone,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TChange) as TChange;
end;

Function TChangesResource.List(managedZone: string; project: string; AQuery : string = '') : TChangesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/managedZones/{managedZone}/changes';
  _Methodid   = 'dns.changes.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['managedZone',managedZone,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TChangesListResponse) as TChangesListResponse;
end;


Function TChangesResource.List(managedZone: string; project: string; AQuery : TChangeslistOptions) : TChangesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'sortBy',AQuery.sortBy);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  Result:=List(managedZone,project,_Q);
end;



{ --------------------------------------------------------------------
  TManagedZonesResource
  --------------------------------------------------------------------}


Class Function TManagedZonesResource.ResourceName : String;

begin
  Result:='managedZones';
end;

Class Function TManagedZonesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdnsAPI;
end;

Function TManagedZonesResource.Create(project: string; aManagedZone : TManagedZone) : TManagedZone;

Const
  _HTTPMethod = 'POST';
  _Path       = '{project}/managedZones';
  _Methodid   = 'dns.managedZones.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aManagedZone,TManagedZone) as TManagedZone;
end;

Procedure TManagedZonesResource.Delete(managedZone: string; project: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = '{project}/managedZones/{managedZone}';
  _Methodid   = 'dns.managedZones.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['managedZone',managedZone,'project',project]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TManagedZonesResource.Get(managedZone: string; project: string) : TManagedZone;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/managedZones/{managedZone}';
  _Methodid   = 'dns.managedZones.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['managedZone',managedZone,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TManagedZone) as TManagedZone;
end;

Function TManagedZonesResource.List(project: string; AQuery : string = '') : TManagedZonesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/managedZones';
  _Methodid   = 'dns.managedZones.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TManagedZonesListResponse) as TManagedZonesListResponse;
end;


Function TManagedZonesResource.List(project: string; AQuery : TManagedZoneslistOptions) : TManagedZonesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(project,_Q);
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
  Result:=TdnsAPI;
end;

Function TProjectsResource.Get(project: string) : TProject;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}';
  _Methodid   = 'dns.projects.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TProject) as TProject;
end;



{ --------------------------------------------------------------------
  TResourceRecordSetsResource
  --------------------------------------------------------------------}


Class Function TResourceRecordSetsResource.ResourceName : String;

begin
  Result:='resourceRecordSets';
end;

Class Function TResourceRecordSetsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdnsAPI;
end;

Function TResourceRecordSetsResource.List(managedZone: string; project: string; AQuery : string = '') : TResourceRecordSetsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = '{project}/managedZones/{managedZone}/rrsets';
  _Methodid   = 'dns.resourceRecordSets.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['managedZone',managedZone,'project',project]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TResourceRecordSetsListResponse) as TResourceRecordSetsListResponse;
end;


Function TResourceRecordSetsResource.List(managedZone: string; project: string; AQuery : TResourceRecordSetslistOptions) : TResourceRecordSetsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'name',AQuery._name);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'type',AQuery._type);
  Result:=List(managedZone,project,_Q);
end;



{ --------------------------------------------------------------------
  TDnsAPI
  --------------------------------------------------------------------}

Class Function TDnsAPI.APIName : String;

begin
  Result:='dns';
end;

Class Function TDnsAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TDnsAPI.APIRevision : String;

begin
  Result:='20150218';
end;

Class Function TDnsAPI.APIID : String;

begin
  Result:='dns:v1';
end;

Class Function TDnsAPI.APITitle : String;

begin
  Result:='Google Cloud DNS API';
end;

Class Function TDnsAPI.APIDescription : String;

begin
  Result:='The Google Cloud DNS API provides services for configuring and serving authoritative DNS records.';
end;

Class Function TDnsAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TDnsAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TDnsAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TDnsAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TDnsAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/cloud-dns';
end;

Class Function TDnsAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TDnsAPI.APIbasePath : string;

begin
  Result:='/dns/v1/projects/';
end;

Class Function TDnsAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/dns/v1/projects/';
end;

Class Function TDnsAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TDnsAPI.APIservicePath : string;

begin
  Result:='dns/v1/projects/';
end;

Class Function TDnsAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TDnsAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,3);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/ndev.clouddns.readonly';
  Result[1].Description:='View your DNS records hosted by Google Cloud DNS';
  Result[2].Name:='https://www.googleapis.com/auth/ndev.clouddns.readwrite';
  Result[2].Description:='View and manage your DNS records hosted by Google Cloud DNS';
  
end;

Class Function TDnsAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TDnsAPI.RegisterAPIResources;

begin
  TChange.RegisterObject;
  TChangesListResponse.RegisterObject;
  TManagedZone.RegisterObject;
  TManagedZonesListResponse.RegisterObject;
  TProject.RegisterObject;
  TQuota.RegisterObject;
  TResourceRecordSet.RegisterObject;
  TResourceRecordSetsListResponse.RegisterObject;
end;


Function TDnsAPI.GetChangesInstance : TChangesResource;

begin
  if (FChangesInstance=Nil) then
    FChangesInstance:=CreateChangesResource;
  Result:=FChangesInstance;
end;

Function TDnsAPI.CreateChangesResource : TChangesResource;

begin
  Result:=CreateChangesResource(Self);
end;


Function TDnsAPI.CreateChangesResource(AOwner : TComponent) : TChangesResource;

begin
  Result:=TChangesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDnsAPI.GetManagedZonesInstance : TManagedZonesResource;

begin
  if (FManagedZonesInstance=Nil) then
    FManagedZonesInstance:=CreateManagedZonesResource;
  Result:=FManagedZonesInstance;
end;

Function TDnsAPI.CreateManagedZonesResource : TManagedZonesResource;

begin
  Result:=CreateManagedZonesResource(Self);
end;


Function TDnsAPI.CreateManagedZonesResource(AOwner : TComponent) : TManagedZonesResource;

begin
  Result:=TManagedZonesResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDnsAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TDnsAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TDnsAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TDnsAPI.GetResourceRecordSetsInstance : TResourceRecordSetsResource;

begin
  if (FResourceRecordSetsInstance=Nil) then
    FResourceRecordSetsInstance:=CreateResourceRecordSetsResource;
  Result:=FResourceRecordSetsInstance;
end;

Function TDnsAPI.CreateResourceRecordSetsResource : TResourceRecordSetsResource;

begin
  Result:=CreateResourceRecordSetsResource(Self);
end;


Function TDnsAPI.CreateResourceRecordSetsResource(AOwner : TComponent) : TResourceRecordSetsResource;

begin
  Result:=TResourceRecordSetsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TDnsAPI.RegisterAPI;
end.
