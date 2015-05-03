unit googleoauth2;
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
  TJwk = class;
  TJwkArray = Array of TJwk;
  TJwkkeys = class;
  TJwkkeysArray = Array of TJwkkeys;
  TTokeninfo = class;
  TTokeninfoArray = Array of TTokeninfo;
  TUserinfoplus = class;
  TUserinfoplusArray = Array of TUserinfoplus;
  
  { --------------------------------------------------------------------
    TJwk
    --------------------------------------------------------------------}
  
  TJwk = Class(TGoogleBaseObject)
  Private
    Fkeys : TJwkkeys;
  Protected
    //Property setters
    Procedure Setkeys(AIndex : Integer; AValue : TJwkkeys); virtual;
  Public
  Published
    Property keys : TJwkkeys Index 0 Read Fkeys Write Setkeys;
  end;
  TJwkClass = Class of TJwk;
  
  { --------------------------------------------------------------------
    TJwkkeys
    --------------------------------------------------------------------}
  
  TJwkkeys = Class(TGoogleBaseObject)
  Private
    Falg : string;
    Fe : string;
    Fkid : string;
    Fkty : string;
    Fn : string;
    Fuse : string;
  Protected
    //Property setters
    Procedure Setalg(AIndex : Integer; AValue : string); virtual;
    Procedure Sete(AIndex : Integer; AValue : string); virtual;
    Procedure Setkid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkty(AIndex : Integer; AValue : string); virtual;
    Procedure Setn(AIndex : Integer; AValue : string); virtual;
    Procedure Setuse(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property alg : string Index 0 Read Falg Write Setalg;
    Property e : string Index 8 Read Fe Write Sete;
    Property kid : string Index 16 Read Fkid Write Setkid;
    Property kty : string Index 24 Read Fkty Write Setkty;
    Property n : string Index 32 Read Fn Write Setn;
    Property use : string Index 40 Read Fuse Write Setuse;
  end;
  TJwkkeysClass = Class of TJwkkeys;
  
  { --------------------------------------------------------------------
    TTokeninfo
    --------------------------------------------------------------------}
  
  TTokeninfo = Class(TGoogleBaseObject)
  Private
    Faccess_type : string;
    Faudience : string;
    Femail : string;
    Fexpires_in : integer;
    Fissued_to : string;
    Fscope : string;
    Ftoken_handle : string;
    Fuser_id : string;
    Fverified_email : boolean;
  Protected
    //Property setters
    Procedure Setaccess_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setaudience(AIndex : Integer; AValue : string); virtual;
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure Setexpires_in(AIndex : Integer; AValue : integer); virtual;
    Procedure Setissued_to(AIndex : Integer; AValue : string); virtual;
    Procedure Setscope(AIndex : Integer; AValue : string); virtual;
    Procedure Settoken_handle(AIndex : Integer; AValue : string); virtual;
    Procedure Setuser_id(AIndex : Integer; AValue : string); virtual;
    Procedure Setverified_email(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property access_type : string Index 0 Read Faccess_type Write Setaccess_type;
    Property audience : string Index 8 Read Faudience Write Setaudience;
    Property email : string Index 16 Read Femail Write Setemail;
    Property expires_in : integer Index 24 Read Fexpires_in Write Setexpires_in;
    Property issued_to : string Index 32 Read Fissued_to Write Setissued_to;
    Property scope : string Index 40 Read Fscope Write Setscope;
    Property token_handle : string Index 48 Read Ftoken_handle Write Settoken_handle;
    Property user_id : string Index 56 Read Fuser_id Write Setuser_id;
    Property verified_email : boolean Index 64 Read Fverified_email Write Setverified_email;
  end;
  TTokeninfoClass = Class of TTokeninfo;
  
  { --------------------------------------------------------------------
    TUserinfoplus
    --------------------------------------------------------------------}
  
  TUserinfoplus = Class(TGoogleBaseObject)
  Private
    Femail : string;
    Ffamily_name : string;
    Fgender : string;
    Fgiven_name : string;
    Fhd : string;
    Fid : string;
    Flink : string;
    Flocale : string;
    Fname : string;
    Fpicture : string;
    Fverified_email : boolean;
  Protected
    //Property setters
    Procedure Setemail(AIndex : Integer; AValue : string); virtual;
    Procedure Setfamily_name(AIndex : Integer; AValue : string); virtual;
    Procedure Setgender(AIndex : Integer; AValue : string); virtual;
    Procedure Setgiven_name(AIndex : Integer; AValue : string); virtual;
    Procedure Sethd(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setlink(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocale(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setpicture(AIndex : Integer; AValue : string); virtual;
    Procedure Setverified_email(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property email : string Index 0 Read Femail Write Setemail;
    Property family_name : string Index 8 Read Ffamily_name Write Setfamily_name;
    Property gender : string Index 16 Read Fgender Write Setgender;
    Property given_name : string Index 24 Read Fgiven_name Write Setgiven_name;
    Property hd : string Index 32 Read Fhd Write Sethd;
    Property id : string Index 40 Read Fid Write Setid;
    Property link : string Index 48 Read Flink Write Setlink;
    Property locale : string Index 56 Read Flocale Write Setlocale;
    Property name : string Index 64 Read Fname Write Setname;
    Property picture : string Index 72 Read Fpicture Write Setpicture;
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
  TJwk
  --------------------------------------------------------------------}


Procedure TJwk.Setkeys(AIndex : Integer; AValue : TJwkkeys); 

begin
  If (Fkeys=AValue) then exit;
  Fkeys:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJwkkeys
  --------------------------------------------------------------------}


Procedure TJwkkeys.Setalg(AIndex : Integer; AValue : string); 

begin
  If (Falg=AValue) then exit;
  Falg:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJwkkeys.Sete(AIndex : Integer; AValue : string); 

begin
  If (Fe=AValue) then exit;
  Fe:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJwkkeys.Setkid(AIndex : Integer; AValue : string); 

begin
  If (Fkid=AValue) then exit;
  Fkid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJwkkeys.Setkty(AIndex : Integer; AValue : string); 

begin
  If (Fkty=AValue) then exit;
  Fkty:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJwkkeys.Setn(AIndex : Integer; AValue : string); 

begin
  If (Fn=AValue) then exit;
  Fn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJwkkeys.Setuse(AIndex : Integer; AValue : string); 

begin
  If (Fuse=AValue) then exit;
  Fuse:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTokeninfo
  --------------------------------------------------------------------}


Procedure TTokeninfo.Setaccess_type(AIndex : Integer; AValue : string); 

begin
  If (Faccess_type=AValue) then exit;
  Faccess_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokeninfo.Setaudience(AIndex : Integer; AValue : string); 

begin
  If (Faudience=AValue) then exit;
  Faudience:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokeninfo.Setemail(AIndex : Integer; AValue : string); 

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



Procedure TTokeninfo.Setissued_to(AIndex : Integer; AValue : string); 

begin
  If (Fissued_to=AValue) then exit;
  Fissued_to:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokeninfo.Setscope(AIndex : Integer; AValue : string); 

begin
  If (Fscope=AValue) then exit;
  Fscope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokeninfo.Settoken_handle(AIndex : Integer; AValue : string); 

begin
  If (Ftoken_handle=AValue) then exit;
  Ftoken_handle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokeninfo.Setuser_id(AIndex : Integer; AValue : string); 

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


Procedure TUserinfoplus.Setemail(AIndex : Integer; AValue : string); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setfamily_name(AIndex : Integer; AValue : string); 

begin
  If (Ffamily_name=AValue) then exit;
  Ffamily_name:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setgender(AIndex : Integer; AValue : string); 

begin
  If (Fgender=AValue) then exit;
  Fgender:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setgiven_name(AIndex : Integer; AValue : string); 

begin
  If (Fgiven_name=AValue) then exit;
  Fgiven_name:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Sethd(AIndex : Integer; AValue : string); 

begin
  If (Fhd=AValue) then exit;
  Fhd:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setlink(AIndex : Integer; AValue : string); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setlocale(AIndex : Integer; AValue : string); 

begin
  If (Flocale=AValue) then exit;
  Flocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserinfoplus.Setpicture(AIndex : Integer; AValue : string); 

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
  TJwk.RegisterObject;
  TJwkkeys.RegisterObject;
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
