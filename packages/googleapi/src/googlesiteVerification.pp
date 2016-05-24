unit googlesiteVerification;
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
  TSiteVerificationWebResourceGettokenRequest = class;
  TSiteVerificationWebResourceGettokenRequestArray = Array of TSiteVerificationWebResourceGettokenRequest;
  TSiteVerificationWebResourceGettokenRequestsite = class;
  TSiteVerificationWebResourceGettokenRequestsiteArray = Array of TSiteVerificationWebResourceGettokenRequestsite;
  TSiteVerificationWebResourceGettokenResponse = class;
  TSiteVerificationWebResourceGettokenResponseArray = Array of TSiteVerificationWebResourceGettokenResponse;
  TSiteVerificationWebResourceListResponse = class;
  TSiteVerificationWebResourceListResponseArray = Array of TSiteVerificationWebResourceListResponse;
  TSiteVerificationWebResourceListResponseitems = class;
  TSiteVerificationWebResourceListResponseitemsArray = Array of TSiteVerificationWebResourceListResponseitems;
  TSiteVerificationWebResourceResource = class;
  TSiteVerificationWebResourceResourceArray = Array of TSiteVerificationWebResourceResource;
  TSiteVerificationWebResourceResourceowners = class;
  TSiteVerificationWebResourceResourceownersArray = Array of TSiteVerificationWebResourceResourceowners;
  TSiteVerificationWebResourceResourcesite = class;
  TSiteVerificationWebResourceResourcesiteArray = Array of TSiteVerificationWebResourceResourcesite;
  
  { --------------------------------------------------------------------
    TSiteVerificationWebResourceGettokenRequest
    --------------------------------------------------------------------}
  
  TSiteVerificationWebResourceGettokenRequest = Class(TGoogleBaseObject)
  Private
    Fsite : TSiteVerificationWebResourceGettokenRequestsite;
    FverificationMethod : string;
  Protected
    //Property setters
    Procedure Setsite(AIndex : Integer; AValue : TSiteVerificationWebResourceGettokenRequestsite); virtual;
    Procedure SetverificationMethod(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property site : TSiteVerificationWebResourceGettokenRequestsite Index 0 Read Fsite Write Setsite;
    Property verificationMethod : string Index 8 Read FverificationMethod Write SetverificationMethod;
  end;
  TSiteVerificationWebResourceGettokenRequestClass = Class of TSiteVerificationWebResourceGettokenRequest;
  
  { --------------------------------------------------------------------
    TSiteVerificationWebResourceGettokenRequestsite
    --------------------------------------------------------------------}
  
  TSiteVerificationWebResourceGettokenRequestsite = Class(TGoogleBaseObject)
  Private
    Fidentifier : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setidentifier(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property identifier : string Index 0 Read Fidentifier Write Setidentifier;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TSiteVerificationWebResourceGettokenRequestsiteClass = Class of TSiteVerificationWebResourceGettokenRequestsite;
  
  { --------------------------------------------------------------------
    TSiteVerificationWebResourceGettokenResponse
    --------------------------------------------------------------------}
  
  TSiteVerificationWebResourceGettokenResponse = Class(TGoogleBaseObject)
  Private
    Fmethod : string;
    Ftoken : string;
  Protected
    //Property setters
    Procedure Setmethod(AIndex : Integer; AValue : string); virtual;
    Procedure Settoken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property method : string Index 0 Read Fmethod Write Setmethod;
    Property token : string Index 8 Read Ftoken Write Settoken;
  end;
  TSiteVerificationWebResourceGettokenResponseClass = Class of TSiteVerificationWebResourceGettokenResponse;
  
  { --------------------------------------------------------------------
    TSiteVerificationWebResourceListResponse
    --------------------------------------------------------------------}
  
  TSiteVerificationWebResourceListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TSiteVerificationWebResourceListResponseitems;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TSiteVerificationWebResourceListResponseitems); virtual;
  Public
  Published
    Property items : TSiteVerificationWebResourceListResponseitems Index 0 Read Fitems Write Setitems;
  end;
  TSiteVerificationWebResourceListResponseClass = Class of TSiteVerificationWebResourceListResponse;
  
  { --------------------------------------------------------------------
    TSiteVerificationWebResourceListResponseitems
    --------------------------------------------------------------------}
  
  TSiteVerificationWebResourceListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSiteVerificationWebResourceListResponseitemsClass = Class of TSiteVerificationWebResourceListResponseitems;
  
  { --------------------------------------------------------------------
    TSiteVerificationWebResourceResource
    --------------------------------------------------------------------}
  
  TSiteVerificationWebResourceResource = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fowners : TSiteVerificationWebResourceResourceowners;
    Fsite : TSiteVerificationWebResourceResourcesite;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setowners(AIndex : Integer; AValue : TSiteVerificationWebResourceResourceowners); virtual;
    Procedure Setsite(AIndex : Integer; AValue : TSiteVerificationWebResourceResourcesite); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property owners : TSiteVerificationWebResourceResourceowners Index 8 Read Fowners Write Setowners;
    Property site : TSiteVerificationWebResourceResourcesite Index 16 Read Fsite Write Setsite;
  end;
  TSiteVerificationWebResourceResourceClass = Class of TSiteVerificationWebResourceResource;
  
  { --------------------------------------------------------------------
    TSiteVerificationWebResourceResourceowners
    --------------------------------------------------------------------}
  
  TSiteVerificationWebResourceResourceowners = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSiteVerificationWebResourceResourceownersClass = Class of TSiteVerificationWebResourceResourceowners;
  
  { --------------------------------------------------------------------
    TSiteVerificationWebResourceResourcesite
    --------------------------------------------------------------------}
  
  TSiteVerificationWebResourceResourcesite = Class(TGoogleBaseObject)
  Private
    Fidentifier : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setidentifier(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property identifier : string Index 0 Read Fidentifier Write Setidentifier;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TSiteVerificationWebResourceResourcesiteClass = Class of TSiteVerificationWebResourceResourcesite;
  
  { --------------------------------------------------------------------
    TWebResourceResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TWebResourceResource, method Insert
  
  TWebResourceInsertOptions = Record
    verificationMethod : string;
  end;
  
  TWebResourceResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(id: string);
    Function Get(id: string) : TSiteVerificationWebResourceResource;
    Function GetToken(aSiteVerificationWebResourceGettokenRequest : TSiteVerificationWebResourceGettokenRequest) : TSiteVerificationWebResourceGettokenResponse;
    Function Insert(aSiteVerificationWebResourceResource : TSiteVerificationWebResourceResource; AQuery : string  = '') : TSiteVerificationWebResourceResource;
    Function Insert(aSiteVerificationWebResourceResource : TSiteVerificationWebResourceResource; AQuery : TWebResourceinsertOptions) : TSiteVerificationWebResourceResource;
    Function List : TSiteVerificationWebResourceListResponse;
    Function Patch(id: string; aSiteVerificationWebResourceResource : TSiteVerificationWebResourceResource) : TSiteVerificationWebResourceResource;
    Function Update(id: string; aSiteVerificationWebResourceResource : TSiteVerificationWebResourceResource) : TSiteVerificationWebResourceResource;
  end;
  
  
  { --------------------------------------------------------------------
    TSiteVerificationAPI
    --------------------------------------------------------------------}
  
  TSiteVerificationAPI = Class(TGoogleAPI)
  Private
    FWebResourceInstance : TWebResourceResource;
    Function GetWebResourceInstance : TWebResourceResource;virtual;
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
    Function CreateWebResourceResource(AOwner : TComponent) : TWebResourceResource;virtual;overload;
    Function CreateWebResourceResource : TWebResourceResource;virtual;overload;
    //Add default on-demand instances for resources
    Property WebResourceResource : TWebResourceResource Read GetWebResourceInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TSiteVerificationWebResourceGettokenRequest
  --------------------------------------------------------------------}


Procedure TSiteVerificationWebResourceGettokenRequest.Setsite(AIndex : Integer; AValue : TSiteVerificationWebResourceGettokenRequestsite); 

begin
  If (Fsite=AValue) then exit;
  Fsite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteVerificationWebResourceGettokenRequest.SetverificationMethod(AIndex : Integer; AValue : string); 

begin
  If (FverificationMethod=AValue) then exit;
  FverificationMethod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSiteVerificationWebResourceGettokenRequestsite
  --------------------------------------------------------------------}


Procedure TSiteVerificationWebResourceGettokenRequestsite.Setidentifier(AIndex : Integer; AValue : string); 

begin
  If (Fidentifier=AValue) then exit;
  Fidentifier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteVerificationWebResourceGettokenRequestsite.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSiteVerificationWebResourceGettokenRequestsite.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TSiteVerificationWebResourceGettokenResponse
  --------------------------------------------------------------------}


Procedure TSiteVerificationWebResourceGettokenResponse.Setmethod(AIndex : Integer; AValue : string); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteVerificationWebResourceGettokenResponse.Settoken(AIndex : Integer; AValue : string); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSiteVerificationWebResourceListResponse
  --------------------------------------------------------------------}


Procedure TSiteVerificationWebResourceListResponse.Setitems(AIndex : Integer; AValue : TSiteVerificationWebResourceListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSiteVerificationWebResourceListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSiteVerificationWebResourceResource
  --------------------------------------------------------------------}


Procedure TSiteVerificationWebResourceResource.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteVerificationWebResourceResource.Setowners(AIndex : Integer; AValue : TSiteVerificationWebResourceResourceowners); 

begin
  If (Fowners=AValue) then exit;
  Fowners:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteVerificationWebResourceResource.Setsite(AIndex : Integer; AValue : TSiteVerificationWebResourceResourcesite); 

begin
  If (Fsite=AValue) then exit;
  Fsite:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSiteVerificationWebResourceResourceowners
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSiteVerificationWebResourceResourcesite
  --------------------------------------------------------------------}


Procedure TSiteVerificationWebResourceResourcesite.Setidentifier(AIndex : Integer; AValue : string); 

begin
  If (Fidentifier=AValue) then exit;
  Fidentifier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteVerificationWebResourceResourcesite.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSiteVerificationWebResourceResourcesite.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TWebResourceResource
  --------------------------------------------------------------------}


Class Function TWebResourceResource.ResourceName : String;

begin
  Result:='webResource';
end;

Class Function TWebResourceResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TsiteVerificationAPI;
end;

Procedure TWebResourceResource.Delete(id: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'webResource/{id}';
  _Methodid   = 'siteVerification.webResource.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TWebResourceResource.Get(id: string) : TSiteVerificationWebResourceResource;

Const
  _HTTPMethod = 'GET';
  _Path       = 'webResource/{id}';
  _Methodid   = 'siteVerification.webResource.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSiteVerificationWebResourceResource) as TSiteVerificationWebResourceResource;
end;

Function TWebResourceResource.GetToken(aSiteVerificationWebResourceGettokenRequest : TSiteVerificationWebResourceGettokenRequest) : TSiteVerificationWebResourceGettokenResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'token';
  _Methodid   = 'siteVerification.webResource.getToken';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSiteVerificationWebResourceGettokenRequest,TSiteVerificationWebResourceGettokenResponse) as TSiteVerificationWebResourceGettokenResponse;
end;

Function TWebResourceResource.Insert(aSiteVerificationWebResourceResource : TSiteVerificationWebResourceResource; AQuery : string = '') : TSiteVerificationWebResourceResource;

Const
  _HTTPMethod = 'POST';
  _Path       = 'webResource';
  _Methodid   = 'siteVerification.webResource.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aSiteVerificationWebResourceResource,TSiteVerificationWebResourceResource) as TSiteVerificationWebResourceResource;
end;


Function TWebResourceResource.Insert(aSiteVerificationWebResourceResource : TSiteVerificationWebResourceResource; AQuery : TWebResourceinsertOptions) : TSiteVerificationWebResourceResource;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'verificationMethod',AQuery.verificationMethod);
  Result:=Insert(aSiteVerificationWebResourceResource,_Q);
end;

Function TWebResourceResource.List : TSiteVerificationWebResourceListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'webResource';
  _Methodid   = 'siteVerification.webResource.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TSiteVerificationWebResourceListResponse) as TSiteVerificationWebResourceListResponse;
end;

Function TWebResourceResource.Patch(id: string; aSiteVerificationWebResourceResource : TSiteVerificationWebResourceResource) : TSiteVerificationWebResourceResource;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'webResource/{id}';
  _Methodid   = 'siteVerification.webResource.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSiteVerificationWebResourceResource,TSiteVerificationWebResourceResource) as TSiteVerificationWebResourceResource;
end;

Function TWebResourceResource.Update(id: string; aSiteVerificationWebResourceResource : TSiteVerificationWebResourceResource) : TSiteVerificationWebResourceResource;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'webResource/{id}';
  _Methodid   = 'siteVerification.webResource.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['id',id]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSiteVerificationWebResourceResource,TSiteVerificationWebResourceResource) as TSiteVerificationWebResourceResource;
end;



{ --------------------------------------------------------------------
  TSiteVerificationAPI
  --------------------------------------------------------------------}

Class Function TSiteVerificationAPI.APIName : String;

begin
  Result:='siteVerification';
end;

Class Function TSiteVerificationAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TSiteVerificationAPI.APIRevision : String;

begin
  Result:='20131007';
end;

Class Function TSiteVerificationAPI.APIID : String;

begin
  Result:='siteVerification:v1';
end;

Class Function TSiteVerificationAPI.APITitle : String;

begin
  Result:='Google Site Verification API';
end;

Class Function TSiteVerificationAPI.APIDescription : String;

begin
  Result:='Lets you programatically verify ownership of websites or domains with Google.';
end;

Class Function TSiteVerificationAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TSiteVerificationAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TSiteVerificationAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TSiteVerificationAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TSiteVerificationAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/site-verification/';
end;

Class Function TSiteVerificationAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TSiteVerificationAPI.APIbasePath : string;

begin
  Result:='/siteVerification/v1/';
end;

Class Function TSiteVerificationAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/siteVerification/v1/';
end;

Class Function TSiteVerificationAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TSiteVerificationAPI.APIservicePath : string;

begin
  Result:='siteVerification/v1/';
end;

Class Function TSiteVerificationAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TSiteVerificationAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/siteverification';
  Result[0].Description:='Manage the list of sites and domains you control';
  Result[1].Name:='https://www.googleapis.com/auth/siteverification.verify_only';
  Result[1].Description:='Manage your new site verifications with Google';
  
end;

Class Function TSiteVerificationAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TSiteVerificationAPI.RegisterAPIResources;

begin
  TSiteVerificationWebResourceGettokenRequest.RegisterObject;
  TSiteVerificationWebResourceGettokenRequestsite.RegisterObject;
  TSiteVerificationWebResourceGettokenResponse.RegisterObject;
  TSiteVerificationWebResourceListResponse.RegisterObject;
  TSiteVerificationWebResourceListResponseitems.RegisterObject;
  TSiteVerificationWebResourceResource.RegisterObject;
  TSiteVerificationWebResourceResourceowners.RegisterObject;
  TSiteVerificationWebResourceResourcesite.RegisterObject;
end;


Function TSiteVerificationAPI.GetWebResourceInstance : TWebResourceResource;

begin
  if (FWebResourceInstance=Nil) then
    FWebResourceInstance:=CreateWebResourceResource;
  Result:=FWebResourceInstance;
end;

Function TSiteVerificationAPI.CreateWebResourceResource : TWebResourceResource;

begin
  Result:=CreateWebResourceResource(Self);
end;


Function TSiteVerificationAPI.CreateWebResourceResource(AOwner : TComponent) : TWebResourceResource;

begin
  Result:=TWebResourceResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TSiteVerificationAPI.RegisterAPI;
end.
