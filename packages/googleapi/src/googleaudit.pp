unit googleaudit;
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
  TActivities = class;
  TActivitiesArray = Array of TActivities;
  TActivitiesitems = class;
  TActivitiesitemsArray = Array of TActivitiesitems;
  TActivity = class;
  TActivityArray = Array of TActivity;
  TActivityactor = class;
  TActivityactorArray = Array of TActivityactor;
  TActivityevents = class;
  TActivityeventsArray = Array of TActivityevents;
  TActivityeventsparameters = class;
  TActivityeventsparametersArray = Array of TActivityeventsparameters;
  TActivityid = class;
  TActivityidArray = Array of TActivityid;
  
  { --------------------------------------------------------------------
    TActivities
    --------------------------------------------------------------------}
  
  TActivities = Class(TGoogleBaseObject)
  Private
    Fitems : TActivitiesitems;
    Fkind : string;
    Fnext : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TActivitiesitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setnext(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TActivitiesitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property next : string Index 16 Read Fnext Write Setnext;
  end;
  TActivitiesClass = Class of TActivities;
  
  { --------------------------------------------------------------------
    TActivitiesitems
    --------------------------------------------------------------------}
  
  TActivitiesitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TActivitiesitemsClass = Class of TActivitiesitems;
  
  { --------------------------------------------------------------------
    TActivity
    --------------------------------------------------------------------}
  
  TActivity = Class(TGoogleBaseObject)
  Private
    Factor : TActivityactor;
    Fevents : TActivityevents;
    Fid : TActivityid;
    FipAddress : string;
    Fkind : string;
    FownerDomain : string;
  Protected
    //Property setters
    Procedure Setactor(AIndex : Integer; AValue : TActivityactor); virtual;
    Procedure Setevents(AIndex : Integer; AValue : TActivityevents); virtual;
    Procedure Setid(AIndex : Integer; AValue : TActivityid); virtual;
    Procedure SetipAddress(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetownerDomain(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property actor : TActivityactor Index 0 Read Factor Write Setactor;
    Property events : TActivityevents Index 8 Read Fevents Write Setevents;
    Property id : TActivityid Index 16 Read Fid Write Setid;
    Property ipAddress : string Index 24 Read FipAddress Write SetipAddress;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property ownerDomain : string Index 40 Read FownerDomain Write SetownerDomain;
  end;
  TActivityClass = Class of TActivity;
  
  { --------------------------------------------------------------------
    TActivityactor
    --------------------------------------------------------------------}
  
  TActivityactor = Class(TGoogleBaseObject)
  Private
    FapplicationId : string;
    FcallerType : string;
    Femail : string;
    Fkey : string;
  Protected
    //Property setters
    Procedure SetapplicationId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcallerType(AIndex : Integer; AValue : string); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property applicationId : string Index 0 Read FapplicationId Write SetapplicationId;
    Property callerType : string Index 8 Read FcallerType Write SetcallerType;
    Property email : string Index 16 Read Femail Write Setemail;
    Property key : string Index 24 Read Fkey Write Setkey;
  end;
  TActivityactorClass = Class of TActivityactor;
  
  { --------------------------------------------------------------------
    TActivityevents
    --------------------------------------------------------------------}
  
  TActivityevents = Class(TGoogleBaseObject)
  Private
    FeventType : string;
    Fname : string;
    Fparameters : TActivityeventsparameters;
  Protected
    //Property setters
    Procedure SeteventType(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setparameters(AIndex : Integer; AValue : TActivityeventsparameters); virtual;
  Public
  Published
    Property eventType : string Index 0 Read FeventType Write SeteventType;
    Property name : string Index 8 Read Fname Write Setname;
    Property parameters : TActivityeventsparameters Index 16 Read Fparameters Write Setparameters;
  end;
  TActivityeventsClass = Class of TActivityevents;
  
  { --------------------------------------------------------------------
    TActivityeventsparameters
    --------------------------------------------------------------------}
  
  TActivityeventsparameters = Class(TGoogleBaseObject)
  Private
    Fname : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property value : string Index 8 Read Fvalue Write Setvalue;
  end;
  TActivityeventsparametersClass = Class of TActivityeventsparameters;
  
  { --------------------------------------------------------------------
    TActivityid
    --------------------------------------------------------------------}
  
  TActivityid = Class(TGoogleBaseObject)
  Private
    FapplicationId : string;
    FcustomerId : string;
    Ftime : TDatetime;
    FuniqQualifier : string;
  Protected
    //Property setters
    Procedure SetapplicationId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcustomerId(AIndex : Integer; AValue : string); virtual;
    Procedure Settime(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetuniqQualifier(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property applicationId : string Index 0 Read FapplicationId Write SetapplicationId;
    Property customerId : string Index 8 Read FcustomerId Write SetcustomerId;
    Property time : TDatetime Index 16 Read Ftime Write Settime;
    Property uniqQualifier : string Index 24 Read FuniqQualifier Write SetuniqQualifier;
  end;
  TActivityidClass = Class of TActivityid;
  
  { --------------------------------------------------------------------
    TActivitiesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TActivitiesResource, method List
  
  TActivitiesListOptions = Record
    actorApplicationId : int64;
    actorEmail : string;
    actorIpAddress : string;
    caller : string;
    continuationToken : string;
    endTime : string;
    eventName : string;
    maxResults : integer;
    startTime : string;
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


Procedure TActivities.Setitems(AIndex : Integer; AValue : TActivitiesitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivities.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivities.Setnext(AIndex : Integer; AValue : string); 

begin
  If (Fnext=AValue) then exit;
  Fnext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivitiesitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TActivity
  --------------------------------------------------------------------}


Procedure TActivity.Setactor(AIndex : Integer; AValue : TActivityactor); 

begin
  If (Factor=AValue) then exit;
  Factor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setevents(AIndex : Integer; AValue : TActivityevents); 

begin
  If (Fevents=AValue) then exit;
  Fevents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setid(AIndex : Integer; AValue : TActivityid); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.SetipAddress(AIndex : Integer; AValue : string); 

begin
  If (FipAddress=AValue) then exit;
  FipAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivity.SetownerDomain(AIndex : Integer; AValue : string); 

begin
  If (FownerDomain=AValue) then exit;
  FownerDomain:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityactor
  --------------------------------------------------------------------}


Procedure TActivityactor.SetapplicationId(AIndex : Integer; AValue : string); 

begin
  If (FapplicationId=AValue) then exit;
  FapplicationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityactor.SetcallerType(AIndex : Integer; AValue : string); 

begin
  If (FcallerType=AValue) then exit;
  FcallerType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityactor.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityactor.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityevents
  --------------------------------------------------------------------}


Procedure TActivityevents.SeteventType(AIndex : Integer; AValue : string); 

begin
  If (FeventType=AValue) then exit;
  FeventType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityevents.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityevents.Setparameters(AIndex : Integer; AValue : TActivityeventsparameters); 

begin
  If (Fparameters=AValue) then exit;
  Fparameters:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityeventsparameters
  --------------------------------------------------------------------}


Procedure TActivityeventsparameters.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityeventsparameters.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TActivityid
  --------------------------------------------------------------------}


Procedure TActivityid.SetapplicationId(AIndex : Integer; AValue : string); 

begin
  If (FapplicationId=AValue) then exit;
  FapplicationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityid.SetcustomerId(AIndex : Integer; AValue : string); 

begin
  If (FcustomerId=AValue) then exit;
  FcustomerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityid.Settime(AIndex : Integer; AValue : TDatetime); 

begin
  If (Ftime=AValue) then exit;
  Ftime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TActivityid.SetuniqQualifier(AIndex : Integer; AValue : string); 

begin
  If (FuniqQualifier=AValue) then exit;
  FuniqQualifier:=AValue;
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
  TActivitiesitems.RegisterObject;
  TActivity.RegisterObject;
  TActivityactor.RegisterObject;
  TActivityevents.RegisterObject;
  TActivityeventsparameters.RegisterObject;
  TActivityid.RegisterObject;
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
