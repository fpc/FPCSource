unit googleaudit;
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
  TActivities = class;
  TActivity = class;
  TActivitiesArray = Array of TActivities;
  TActivityArray = Array of TActivity;
  //Anonymous types, using auto-generated names
  TActivityTypeactor = class;
  TActivityTypeeventsItemTypeparametersItem = class;
  TActivityTypeeventsItem = class;
  TActivityTypeid = class;
  TActivitiesTypeitemsArray = Array of TActivity;
  TActivityTypeeventsItemTypeparametersArray = Array of TActivityTypeeventsItemTypeparametersItem;
  TActivityTypeeventsArray = Array of TActivityTypeeventsItem;
  
  { --------------------------------------------------------------------
    TActivities
    --------------------------------------------------------------------}
  
  TActivities = Class(TGoogleBaseObject)
  Private
    Fitems : TActivitiesTypeitemsArray;
    Fkind : String;
    Fnext : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TActivitiesTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setnext(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property items : TActivitiesTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property next : String Index 16 Read Fnext Write Setnext;
  end;
  TActivitiesClass = Class of TActivities;
  
  { --------------------------------------------------------------------
    TActivityTypeactor
    --------------------------------------------------------------------}
  
  TActivityTypeactor = Class(TGoogleBaseObject)
  Private
    FapplicationId : String;
    FcallerType : String;
    Femail : String;
    Fkey : String;
  Protected
    //Property setters
    Procedure SetapplicationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcallerType(AIndex : Integer; AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure Setkey(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property applicationId : String Index 0 Read FapplicationId Write SetapplicationId;
    Property callerType : String Index 8 Read FcallerType Write SetcallerType;
    Property email : String Index 16 Read Femail Write Setemail;
    Property key : String Index 24 Read Fkey Write Setkey;
  end;
  TActivityTypeactorClass = Class of TActivityTypeactor;
  
  { --------------------------------------------------------------------
    TActivityTypeeventsItemTypeparametersItem
    --------------------------------------------------------------------}
  
  TActivityTypeeventsItemTypeparametersItem = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TActivityTypeeventsItemTypeparametersItemClass = Class of TActivityTypeeventsItemTypeparametersItem;
  
  { --------------------------------------------------------------------
    TActivityTypeeventsItem
    --------------------------------------------------------------------}
  
  TActivityTypeeventsItem = Class(TGoogleBaseObject)
  Private
    FeventType : String;
    Fname : String;
    Fparameters : TActivityTypeeventsItemTypeparametersArray;
  Protected
    //Property setters
    Procedure SeteventType(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setparameters(AIndex : Integer; AValue : TActivityTypeeventsItemTypeparametersArray); virtual;
  Public
  Published
    Property eventType : String Index 0 Read FeventType Write SeteventType;
    Property name : String Index 8 Read Fname Write Setname;
    Property parameters : TActivityTypeeventsItemTypeparametersArray Index 16 Read Fparameters Write Setparameters;
  end;
  TActivityTypeeventsItemClass = Class of TActivityTypeeventsItem;
  
  { --------------------------------------------------------------------
    TActivityTypeid
    --------------------------------------------------------------------}
  
  TActivityTypeid = Class(TGoogleBaseObject)
  Private
    FapplicationId : String;
    FcustomerId : String;
    Ftime : TDatetime;
    FuniqQualifier : String;
  Protected
    //Property setters
    Procedure SetapplicationId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcustomerId(AIndex : Integer; AValue : String); virtual;
    Procedure Settime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetuniqQualifier(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property applicationId : String Index 0 Read FapplicationId Write SetapplicationId;
    Property customerId : String Index 8 Read FcustomerId Write SetcustomerId;
    Property time : TDatetime Index 16 Read Ftime Write Settime;
    Property uniqQualifier : String Index 24 Read FuniqQualifier Write SetuniqQualifier;
  end;
  TActivityTypeidClass = Class of TActivityTypeid;
  
  { --------------------------------------------------------------------
    TActivity
    --------------------------------------------------------------------}
  
  TActivity = Class(TGoogleBaseObject)
  Private
    Factor : TActivityTypeactor;
    Fevents : TActivityTypeeventsArray;
    Fid : TActivityTypeid;
    FipAddress : String;
    Fkind : String;
    FownerDomain : String;
  Protected
    //Property setters
    Procedure Setactor(AIndex : Integer; AValue : TActivityTypeactor); virtual;
    Procedure Setevents(AIndex : Integer; AValue : TActivityTypeeventsArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : TActivityTypeid); virtual;
    Procedure SetipAddress(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetownerDomain(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property actor : TActivityTypeactor Index 0 Read Factor Write Setactor;
    Property events : TActivityTypeeventsArray Index 8 Read Fevents Write Setevents;
    Property id : TActivityTypeid Index 16 Read Fid Write Setid;
    Property ipAddress : String Index 24 Read FipAddress Write SetipAddress;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property ownerDomain : String Index 40 Read FownerDomain Write SetownerDomain;
  end;
  TActivityClass = Class of TActivity;
  
  { --------------------------------------------------------------------
    TActivitiesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TActivitiesResource, method List
  
  TActivitiesListOptions = Record
    actorApplicationId : int64;
    actorEmail : String;
    actorIpAddress : String;
    caller : String;
    continuationToken : String;
    endTime : String;
    eventName : String;
    maxResults : integer;
    startTime : String;
  end;
  
  TActivitiesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(applicationId: string; customerId: string; AQuery : string  = '') : TActivities;
    Function List(applicationId: string; customerId: string; AQuery : TActivitieslistOptions) : TActivities;
  end;
  
  
  { --------------------------------------------------------------------
    TAuditAPI
    --------------------------------------------------------------------}
  
  TAuditAPI = Class(TGoogleAPI)
  Private
    FActivitiesInstance : TActivitiesResource;
    Function GetActivitiesInstance : TActivitiesResource;virtual;
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
    Function CreateActivitiesResource(AOwner : TComponent) : TActivitiesResource;virtual;overload;
    Function CreateActivitiesResource : TActivitiesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ActivitiesResource : TActivitiesResource Read GetActivitiesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TActivities
  --------------------------------------------------------------------}


Procedure TActivities.Setitems(AIndex : Integer; AValue : TActivitiesTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivities.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivities.Setnext(AIndex : Integer; AValue : String); 

begin
  If (Fnext=AValue) then exit;
  Fnext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeactor
  --------------------------------------------------------------------}


Procedure TActivityTypeactor.SetapplicationId(AIndex : Integer; AValue : String); 

begin
  If (FapplicationId=AValue) then exit;
  FapplicationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeactor.SetcallerType(AIndex : Integer; AValue : String); 

begin
  If (FcallerType=AValue) then exit;
  FcallerType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeactor.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeactor.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeeventsItemTypeparametersItem
  --------------------------------------------------------------------}


Procedure TActivityTypeeventsItemTypeparametersItem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeeventsItemTypeparametersItem.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeeventsItem
  --------------------------------------------------------------------}


Procedure TActivityTypeeventsItem.SeteventType(AIndex : Integer; AValue : String); 

begin
  If (FeventType=AValue) then exit;
  FeventType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeeventsItem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeeventsItem.Setparameters(AIndex : Integer; AValue : TActivityTypeeventsItemTypeparametersArray); 

begin
  If (Fparameters=AValue) then exit;
  Fparameters:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityTypeid
  --------------------------------------------------------------------}


Procedure TActivityTypeid.SetapplicationId(AIndex : Integer; AValue : String); 

begin
  If (FapplicationId=AValue) then exit;
  FapplicationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeid.SetcustomerId(AIndex : Integer; AValue : String); 

begin
  If (FcustomerId=AValue) then exit;
  FcustomerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeid.Settime(AIndex : Integer; AValue : TDatetime); 

begin
  If (Ftime=AValue) then exit;
  Ftime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityTypeid.SetuniqQualifier(AIndex : Integer; AValue : String); 

begin
  If (FuniqQualifier=AValue) then exit;
  FuniqQualifier:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivity
  --------------------------------------------------------------------}


Procedure TActivity.Setactor(AIndex : Integer; AValue : TActivityTypeactor); 

begin
  If (Factor=AValue) then exit;
  Factor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setevents(AIndex : Integer; AValue : TActivityTypeeventsArray); 

begin
  If (Fevents=AValue) then exit;
  Fevents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setid(AIndex : Integer; AValue : TActivityTypeid); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.SetipAddress(AIndex : Integer; AValue : String); 

begin
  If (FipAddress=AValue) then exit;
  FipAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.SetownerDomain(AIndex : Integer; AValue : String); 

begin
  If (FownerDomain=AValue) then exit;
  FownerDomain:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivitiesResource
  --------------------------------------------------------------------}


Class Function TActivitiesResource.ResourceName : String;

begin
  Result:='activities';
end;

Class Function TActivitiesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TauditAPI;
end;

Function TActivitiesResource.List(applicationId: string; customerId: string; AQuery : string = '') : TActivities;

Const
  _HTTPMethod = 'GET';
  _Path       = '{customerId}/{applicationId}';
  _Methodid   = 'audit.activities.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['applicationId',applicationId,'customerId',customerId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TActivities) as TActivities;
end;


Function TActivitiesResource.List(applicationId: string; customerId: string; AQuery : TActivitieslistOptions) : TActivities;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'actorApplicationId',AQuery.actorApplicationId);
  AddToQuery(_Q,'actorEmail',AQuery.actorEmail);
  AddToQuery(_Q,'actorIpAddress',AQuery.actorIpAddress);
  AddToQuery(_Q,'caller',AQuery.caller);
  AddToQuery(_Q,'continuationToken',AQuery.continuationToken);
  AddToQuery(_Q,'endTime',AQuery.endTime);
  AddToQuery(_Q,'eventName',AQuery.eventName);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'startTime',AQuery.startTime);
  Result:=List(applicationId,customerId,_Q);
end;



{ --------------------------------------------------------------------
  TAuditAPI
  --------------------------------------------------------------------}

Class Function TAuditAPI.APIName : String;

begin
  Result:='audit';
end;

Class Function TAuditAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TAuditAPI.APIRevision : String;

begin
  Result:='20150419';
end;

Class Function TAuditAPI.APIID : String;

begin
  Result:='audit:v1';
end;

Class Function TAuditAPI.APITitle : String;

begin
  Result:='Enterprise Audit API';
end;

Class Function TAuditAPI.APIDescription : String;

begin
  Result:='Lets you access user activities in your enterprise made through various applications.';
end;

Class Function TAuditAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TAuditAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TAuditAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TAuditAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TAuditAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/google-apps/admin-audit/get_started';
end;

Class Function TAuditAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TAuditAPI.APIbasePath : string;

begin
  Result:='/apps/reporting/audit/v1/';
end;

Class Function TAuditAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/apps/reporting/audit/v1/';
end;

Class Function TAuditAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TAuditAPI.APIservicePath : string;

begin
  Result:='apps/reporting/audit/v1/';
end;

Class Function TAuditAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TAuditAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TAuditAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TAuditAPI.RegisterAPIResources;

begin
  TActivities.RegisterObject;
  TActivityTypeactor.RegisterObject;
  TActivityTypeeventsItemTypeparametersItem.RegisterObject;
  TActivityTypeeventsItem.RegisterObject;
  TActivityTypeid.RegisterObject;
  TActivity.RegisterObject;
end;


Function TAuditAPI.GetActivitiesInstance : TActivitiesResource;

begin
  if (FActivitiesInstance=Nil) then
    FActivitiesInstance:=CreateActivitiesResource;
  Result:=FActivitiesInstance;
end;

Function TAuditAPI.CreateActivitiesResource : TActivitiesResource;

begin
  Result:=CreateActivitiesResource(Self);
end;


Function TAuditAPI.CreateActivitiesResource(AOwner : TComponent) : TActivitiesResource;

begin
  Result:=TActivitiesResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TAuditAPI.RegisterAPI;
end.
