unit googlesiteVerification;
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
  TSiteVerificationWebResourceGettokenRequest = Class;
  TSiteVerificationWebResourceGettokenResponse = Class;
  TSiteVerificationWebResourceListResponse = Class;
  TSiteVerificationWebResourceResource = Class;
  TSiteVerificationWebResourceGettokenRequestArray = Array of TSiteVerificationWebResourceGettokenRequest;
  TSiteVerificationWebResourceGettokenResponseArray = Array of TSiteVerificationWebResourceGettokenResponse;
  TSiteVerificationWebResourceListResponseArray = Array of TSiteVerificationWebResourceListResponse;
  TSiteVerificationWebResourceResourceArray = Array of TSiteVerificationWebResourceResource;
  //Anonymous types, using auto-generated names
  TSiteVerificationWebResourceGettokenRequestTypesite = Class;
  TSiteVerificationWebResourceResourceTypesite = Class;
  TSiteVerificationWebResourceListResponseTypeitemsArray = Array of TSiteVerificationWebResourceResource;
  
  { --------------------------------------------------------------------
    TSiteVerificationWebResourceGettokenRequestTypesite
    --------------------------------------------------------------------}
  
  TSiteVerificationWebResourceGettokenRequestTypesite = Class(TGoogleBaseObject)
  Private
    Fidentifier : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setidentifier(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property identifier : String Index 0 Read Fidentifier Write Setidentifier;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TSiteVerificationWebResourceGettokenRequestTypesiteClass = Class of TSiteVerificationWebResourceGettokenRequestTypesite;
  
  { --------------------------------------------------------------------
    TSiteVerificationWebResourceGettokenRequest
    --------------------------------------------------------------------}
  
  TSiteVerificationWebResourceGettokenRequest = Class(TGoogleBaseObject)
  Private
    Fsite : TSiteVerificationWebResourceGettokenRequestTypesite;
    FverificationMethod : String;
  Protected
    //Property setters
    Procedure Setsite(AIndex : Integer; AValue : TSiteVerificationWebResourceGettokenRequestTypesite); virtual;
    Procedure SetverificationMethod(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property site : TSiteVerificationWebResourceGettokenRequestTypesite Index 0 Read Fsite Write Setsite;
    Property verificationMethod : String Index 8 Read FverificationMethod Write SetverificationMethod;
  end;
  TSiteVerificationWebResourceGettokenRequestClass = Class of TSiteVerificationWebResourceGettokenRequest;
  
  { --------------------------------------------------------------------
    TSiteVerificationWebResourceGettokenResponse
    --------------------------------------------------------------------}
  
  TSiteVerificationWebResourceGettokenResponse = Class(TGoogleBaseObject)
  Private
    Fmethod : String;
    Ftoken : String;
  Protected
    //Property setters
    Procedure Setmethod(AIndex : Integer; AValue : String); virtual;
    Procedure Settoken(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property method : String Index 0 Read Fmethod Write Setmethod;
    Property token : String Index 8 Read Ftoken Write Settoken;
  end;
  TSiteVerificationWebResourceGettokenResponseClass = Class of TSiteVerificationWebResourceGettokenResponse;
  
  { --------------------------------------------------------------------
    TSiteVerificationWebResourceListResponse
    --------------------------------------------------------------------}
  
  TSiteVerificationWebResourceListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TSiteVerificationWebResourceListResponseTypeitemsArray;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TSiteVerificationWebResourceListResponseTypeitemsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TSiteVerificationWebResourceListResponseTypeitemsArray Index 0 Read Fitems Write Setitems;
  end;
  TSiteVerificationWebResourceListResponseClass = Class of TSiteVerificationWebResourceListResponse;
  
  { --------------------------------------------------------------------
    TSiteVerificationWebResourceResourceTypesite
    --------------------------------------------------------------------}
  
  TSiteVerificationWebResourceResourceTypesite = Class(TGoogleBaseObject)
  Private
    Fidentifier : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setidentifier(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property identifier : String Index 0 Read Fidentifier Write Setidentifier;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TSiteVerificationWebResourceResourceTypesiteClass = Class of TSiteVerificationWebResourceResourceTypesite;
  
  { --------------------------------------------------------------------
    TSiteVerificationWebResourceResource
    --------------------------------------------------------------------}
  
  TSiteVerificationWebResourceResource = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fowners : TStringArray;
    Fsite : TSiteVerificationWebResourceResourceTypesite;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setowners(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setsite(AIndex : Integer; AValue : TSiteVerificationWebResourceResourceTypesite); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property owners : TStringArray Index 8 Read Fowners Write Setowners;
    Property site : TSiteVerificationWebResourceResourceTypesite Index 16 Read Fsite Write Setsite;
  end;
  TSiteVerificationWebResourceResourceClass = Class of TSiteVerificationWebResourceResource;
  
  { --------------------------------------------------------------------
    TWebResourceResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TWebResourceResource, method Insert
  
  TWebResourceInsertOptions = Record
    verificationMethod : String;
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
  TSiteVerificationWebResourceGettokenRequestTypesite
  --------------------------------------------------------------------}


Procedure TSiteVerificationWebResourceGettokenRequestTypesite.Setidentifier(AIndex : Integer; AValue : String); 

begin
  If (Fidentifier=AValue) then exit;
  Fidentifier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteVerificationWebResourceGettokenRequestTypesite.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSiteVerificationWebResourceGettokenRequestTypesite.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TSiteVerificationWebResourceGettokenRequest
  --------------------------------------------------------------------}


Procedure TSiteVerificationWebResourceGettokenRequest.Setsite(AIndex : Integer; AValue : TSiteVerificationWebResourceGettokenRequestTypesite); 

begin
  If (Fsite=AValue) then exit;
  Fsite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteVerificationWebResourceGettokenRequest.SetverificationMethod(AIndex : Integer; AValue : String); 

begin
  If (FverificationMethod=AValue) then exit;
  FverificationMethod:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSiteVerificationWebResourceGettokenResponse
  --------------------------------------------------------------------}


Procedure TSiteVerificationWebResourceGettokenResponse.Setmethod(AIndex : Integer; AValue : String); 

begin
  If (Fmethod=AValue) then exit;
  Fmethod:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteVerificationWebResourceGettokenResponse.Settoken(AIndex : Integer; AValue : String); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSiteVerificationWebResourceListResponse
  --------------------------------------------------------------------}


Procedure TSiteVerificationWebResourceListResponse.Setitems(AIndex : Integer; AValue : TSiteVerificationWebResourceListResponseTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSiteVerificationWebResourceListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSiteVerificationWebResourceResourceTypesite
  --------------------------------------------------------------------}


Procedure TSiteVerificationWebResourceResourceTypesite.Setidentifier(AIndex : Integer; AValue : String); 

begin
  If (Fidentifier=AValue) then exit;
  Fidentifier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteVerificationWebResourceResourceTypesite.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSiteVerificationWebResourceResourceTypesite.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TSiteVerificationWebResourceResource
  --------------------------------------------------------------------}


Procedure TSiteVerificationWebResourceResource.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteVerificationWebResourceResource.Setowners(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fowners=AValue) then exit;
  Fowners:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSiteVerificationWebResourceResource.Setsite(AIndex : Integer; AValue : TSiteVerificationWebResourceResourceTypesite); 

begin
  If (Fsite=AValue) then exit;
  Fsite:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSiteVerificationWebResourceResource.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'owners' : SetLength(Fowners,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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
  Result:='https://www.googleapis.com:443/';
end;

Class Function TSiteVerificationAPI.APIbasePath : string;

begin
  Result:='/siteVerification/v1/';
end;

Class Function TSiteVerificationAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/siteVerification/v1/';
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
  TSiteVerificationWebResourceGettokenRequestTypesite.RegisterObject;
  TSiteVerificationWebResourceGettokenRequest.RegisterObject;
  TSiteVerificationWebResourceGettokenResponse.RegisterObject;
  TSiteVerificationWebResourceListResponse.RegisterObject;
  TSiteVerificationWebResourceResourceTypesite.RegisterObject;
  TSiteVerificationWebResourceResource.RegisterObject;
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
  Result.API:=Self.API;
end;



initialization
  TSiteVerificationAPI.RegisterAPI;
end.
