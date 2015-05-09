unit googleoauth2;
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
//Generated on: 9-5-15 13:22:56
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TJwk = class;
  TTokeninfo = class;
  TUserinfoplus = class;
  TJwkArray = Array of TJwk;
  TTokeninfoArray = Array of TTokeninfo;
  TUserinfoplusArray = Array of TUserinfoplus;
  //Anonymous types, using auto-generated names
  TJwkTypekeysItem = class;
  TJwkTypekeysArray = Array of TJwkTypekeysItem;
  
  { --------------------------------------------------------------------
    TJwkTypekeysItem
    --------------------------------------------------------------------}
  
  TJwkTypekeysItem = Class(TGoogleBaseObject)
  Private
    Falg : String;
    Fe : String;
    Fkid : String;
    Fkty : String;
    Fn : String;
    Fuse : String;
  Protected
    //Property setters
    Procedure Setalg(AIndex : Integer; AValue : String); virtual;
    Procedure Sete(AIndex : Integer; AValue : String); virtual;
    Procedure Setkid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkty(AIndex : Integer; AValue : String); virtual;
    Procedure Setn(AIndex : Integer; AValue : String); virtual;
    Procedure Setuse(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property alg : String Index 0 Read Falg Write Setalg;
    Property e : String Index 8 Read Fe Write Sete;
    Property kid : String Index 16 Read Fkid Write Setkid;
    Property kty : String Index 24 Read Fkty Write Setkty;
    Property n : String Index 32 Read Fn Write Setn;
    Property use : String Index 40 Read Fuse Write Setuse;
  end;
  TJwkTypekeysItemClass = Class of TJwkTypekeysItem;
  
  { --------------------------------------------------------------------
    TJwk
    --------------------------------------------------------------------}
  
  TJwk = Class(TGoogleBaseObject)
  Private
    Fkeys : TJwkTypekeysArray;
  Protected
    //Property setters
    Procedure Setkeys(AIndex : Integer; AValue : TJwkTypekeysArray); virtual;
  Public
  Published
    Property keys : TJwkTypekeysArray Index 0 Read Fkeys Write Setkeys;
  end;
  TJwkClass = Class of TJwk;
  
  { --------------------------------------------------------------------
    TTokeninfo
    --------------------------------------------------------------------}
  
  TTokeninfo = Class(TGoogleBaseObject)
  Private
    Faccess_type : String;
    Faudience : String;
    Femail : String;
    Fexpires_in : integer;
    Fissued_to : String;
    Fscope : String;
    Ftoken_handle : String;
    Fuser_id : String;
    Fverified_email : boolean;
  Protected
    //Property setters
    Procedure Setaccess_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setaudience(AIndex : Integer; AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure Setexpires_in(AIndex : Integer; AValue : integer); virtual;
    Procedure Setissued_to(AIndex : Integer; AValue : String); virtual;
    Procedure Setscope(AIndex : Integer; AValue : String); virtual;
    Procedure Settoken_handle(AIndex : Integer; AValue : String); virtual;
    Procedure Setuser_id(AIndex : Integer; AValue : String); virtual;
    Procedure Setverified_email(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property access_type : String Index 0 Read Faccess_type Write Setaccess_type;
    Property audience : String Index 8 Read Faudience Write Setaudience;
    Property email : String Index 16 Read Femail Write Setemail;
    Property expires_in : integer Index 24 Read Fexpires_in Write Setexpires_in;
    Property issued_to : String Index 32 Read Fissued_to Write Setissued_to;
    Property scope : String Index 40 Read Fscope Write Setscope;
    Property token_handle : String Index 48 Read Ftoken_handle Write Settoken_handle;
    Property user_id : String Index 56 Read Fuser_id Write Setuser_id;
    Property verified_email : boolean Index 64 Read Fverified_email Write Setverified_email;
  end;
  TTokeninfoClass = Class of TTokeninfo;
  
  { --------------------------------------------------------------------
    TUserinfoplus
    --------------------------------------------------------------------}
  
  TUserinfoplus = Class(TGoogleBaseObject)
  Private
    Femail : String;
    Ffamily_name : String;
    Fgender : String;
    Fgiven_name : String;
    Fhd : String;
    Fid : String;
    Flink : String;
    Flocale : String;
    Fname : String;
    Fpicture : String;
    Fverified_email : boolean;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; AValue : String); virtual;
    Procedure Setfamily_name(AIndex : Integer; AValue : String); virtual;
    Procedure Setgender(AIndex : Integer; AValue : String); virtual;
    Procedure Setgiven_name(AIndex : Integer; AValue : String); virtual;
    Procedure Sethd(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setlink(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocale(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setpicture(AIndex : Integer; AValue : String); virtual;
    Procedure Setverified_email(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property email : String Index 0 Read Femail Write Setemail;
    Property family_name : String Index 8 Read Ffamily_name Write Setfamily_name;
    Property gender : String Index 16 Read Fgender Write Setgender;
    Property given_name : String Index 24 Read Fgiven_name Write Setgiven_name;
    Property hd : String Index 32 Read Fhd Write Sethd;
    Property id : String Index 40 Read Fid Write Setid;
    Property link : String Index 48 Read Flink Write Setlink;
    Property locale : String Index 56 Read Flocale Write Setlocale;
    Property name : String Index 64 Read Fname Write Setname;
    Property picture : String Index 72 Read Fpicture Write Setpicture;
    Property verified_email : boolean Index 80 Read Fverified_email Write Setverified_email;
  end;
  TUserinfoplusClass = Class of TUserinfoplus;
  
  { --------------------------------------------------------------------
    TUserinfoResource
    --------------------------------------------------------------------}
  
  TUserinfoResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get : TUserinfoplus;
  end;
  
  
  { --------------------------------------------------------------------
    TOauth2API
    --------------------------------------------------------------------}
  
  TOauth2API = Class(TGoogleAPI)
  Private
    FUserinfoInstance : TUserinfoResource;
    Function GetUserinfoInstance : TUserinfoResource;virtual;
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
    Function CreateUserinfoResource(AOwner : TComponent) : TUserinfoResource;virtual;overload;
    Function CreateUserinfoResource : TUserinfoResource;virtual;overload;
    //Add default on-demand instances for resources
    Property UserinfoResource : TUserinfoResource Read GetUserinfoInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TJwkTypekeysItem
  --------------------------------------------------------------------}


Procedure TJwkTypekeysItem.Setalg(AIndex : Integer; AValue : String); 

begin
  If (Falg=AValue) then exit;
  Falg:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJwkTypekeysItem.Sete(AIndex : Integer; AValue : String); 

begin
  If (Fe=AValue) then exit;
  Fe:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJwkTypekeysItem.Setkid(AIndex : Integer; AValue : String); 

begin
  If (Fkid=AValue) then exit;
  Fkid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJwkTypekeysItem.Setkty(AIndex : Integer; AValue : String); 

begin
  If (Fkty=AValue) then exit;
  Fkty:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJwkTypekeysItem.Setn(AIndex : Integer; AValue : String); 

begin
  If (Fn=AValue) then exit;
  Fn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJwkTypekeysItem.Setuse(AIndex : Integer; AValue : String); 

begin
  If (Fuse=AValue) then exit;
  Fuse:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJwk
  --------------------------------------------------------------------}


Procedure TJwk.Setkeys(AIndex : Integer; AValue : TJwkTypekeysArray); 

begin
  If (Fkeys=AValue) then exit;
  Fkeys:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTokeninfo
  --------------------------------------------------------------------}


Procedure TTokeninfo.Setaccess_type(AIndex : Integer; AValue : String); 

begin
  If (Faccess_type=AValue) then exit;
  Faccess_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokeninfo.Setaudience(AIndex : Integer; AValue : String); 

begin
  If (Faudience=AValue) then exit;
  Faudience:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokeninfo.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokeninfo.Setexpires_in(AIndex : Integer; AValue : integer); 

begin
  If (Fexpires_in=AValue) then exit;
  Fexpires_in:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokeninfo.Setissued_to(AIndex : Integer; AValue : String); 

begin
  If (Fissued_to=AValue) then exit;
  Fissued_to:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokeninfo.Setscope(AIndex : Integer; AValue : String); 

begin
  If (Fscope=AValue) then exit;
  Fscope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokeninfo.Settoken_handle(AIndex : Integer; AValue : String); 

begin
  If (Ftoken_handle=AValue) then exit;
  Ftoken_handle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokeninfo.Setuser_id(AIndex : Integer; AValue : String); 

begin
  If (Fuser_id=AValue) then exit;
  Fuser_id:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokeninfo.Setverified_email(AIndex : Integer; AValue : boolean); 

begin
  If (Fverified_email=AValue) then exit;
  Fverified_email:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserinfoplus
  --------------------------------------------------------------------}


Procedure TUserinfoplus.Setemail(AIndex : Integer; AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setfamily_name(AIndex : Integer; AValue : String); 

begin
  If (Ffamily_name=AValue) then exit;
  Ffamily_name:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setgender(AIndex : Integer; AValue : String); 

begin
  If (Fgender=AValue) then exit;
  Fgender:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setgiven_name(AIndex : Integer; AValue : String); 

begin
  If (Fgiven_name=AValue) then exit;
  Fgiven_name:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Sethd(AIndex : Integer; AValue : String); 

begin
  If (Fhd=AValue) then exit;
  Fhd:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setlink(AIndex : Integer; AValue : String); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setlocale(AIndex : Integer; AValue : String); 

begin
  If (Flocale=AValue) then exit;
  Flocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setpicture(AIndex : Integer; AValue : String); 

begin
  If (Fpicture=AValue) then exit;
  Fpicture:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setverified_email(AIndex : Integer; AValue : boolean); 

begin
  If (Fverified_email=AValue) then exit;
  Fverified_email:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserinfoResource
  --------------------------------------------------------------------}


Class Function TUserinfoResource.ResourceName : String;

begin
  Result:='userinfo';
end;

Class Function TUserinfoResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=Toauth2API;
end;

Function TUserinfoResource.Get : TUserinfoplus;

Const
  _HTTPMethod = 'GET';
  _Path       = 'oauth2/v2/userinfo';
  _Methodid   = 'oauth2.userinfo.get';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TUserinfoplus) as TUserinfoplus;
end;



{ --------------------------------------------------------------------
  TOauth2API
  --------------------------------------------------------------------}

Class Function TOauth2API.APIName : String;

begin
  Result:='oauth2';
end;

Class Function TOauth2API.APIVersion : String;

begin
  Result:='v2';
end;

Class Function TOauth2API.APIRevision : String;

begin
  Result:='20150416';
end;

Class Function TOauth2API.APIID : String;

begin
  Result:='oauth2:v2';
end;

Class Function TOauth2API.APITitle : String;

begin
  Result:='Google OAuth2 API';
end;

Class Function TOauth2API.APIDescription : String;

begin
  Result:='Lets you access OAuth2 protocol related APIs.';
end;

Class Function TOauth2API.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TOauth2API.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TOauth2API.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TOauth2API.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TOauth2API.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/accounts/docs/OAuth2';
end;

Class Function TOauth2API.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TOauth2API.APIbasePath : string;

begin
  Result:='/';
end;

Class Function TOauth2API.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TOauth2API.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TOauth2API.APIservicePath : string;

begin
  Result:='';
end;

Class Function TOauth2API.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TOauth2API.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,4);
  Result[0].Name:='https://www.googleapis.com/auth/plus.login';
  Result[0].Description:='Know your basic profile info and list of people in your circles.';
  Result[1].Name:='https://www.googleapis.com/auth/plus.me';
  Result[1].Description:='Know who you are on Google';
  Result[2].Name:='https://www.googleapis.com/auth/userinfo.email';
  Result[2].Description:='View your email address';
  Result[3].Name:='https://www.googleapis.com/auth/userinfo.profile';
  Result[3].Description:='View your basic profile info';
  
end;

Class Function TOauth2API.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TOauth2API.RegisterAPIResources;

begin
  TJwkTypekeysItem.RegisterObject;
  TJwk.RegisterObject;
  TTokeninfo.RegisterObject;
  TUserinfoplus.RegisterObject;
end;


Function TOauth2API.GetUserinfoInstance : TUserinfoResource;

begin
  if (FUserinfoInstance=Nil) then
    FUserinfoInstance:=CreateUserinfoResource;
  Result:=FUserinfoInstance;
end;

Function TOauth2API.CreateUserinfoResource : TUserinfoResource;

begin
  Result:=CreateUserinfoResource(Self);
end;


Function TOauth2API.CreateUserinfoResource(AOwner : TComponent) : TUserinfoResource;

begin
  Result:=TUserinfoResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TOauth2API.RegisterAPI;
end.
