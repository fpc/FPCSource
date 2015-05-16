unit googlewebfonts;
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
//Generated on: 16-5-15 08:53:09
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TWebfont = Class;
  TWebfontList = Class;
  TWebfontArray = Array of TWebfont;
  TWebfontListArray = Array of TWebfontList;
  //Anonymous types, using auto-generated names
  TWebfontTypefiles = Class;
  TWebfontListTypeitemsArray = Array of TWebfont;
  
  { --------------------------------------------------------------------
    TWebfontTypefiles
    --------------------------------------------------------------------}
  
  TWebfontTypefiles = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TWebfontTypefilesClass = Class of TWebfontTypefiles;
  
  { --------------------------------------------------------------------
    TWebfont
    --------------------------------------------------------------------}
  
  TWebfont = Class(TGoogleBaseObject)
  Private
    Fcategory : String;
    Ffamily : String;
    Ffiles : TWebfontTypefiles;
    Fkind : String;
    FlastModified : TDate;
    Fsubsets : TStringArray;
    Fvariants : TStringArray;
    Fversion : String;
  Protected
    //Property setters
    Procedure Setcategory(AIndex : Integer; AValue : String); virtual;
    Procedure Setfamily(AIndex : Integer; AValue : String); virtual;
    Procedure Setfiles(AIndex : Integer; AValue : TWebfontTypefiles); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlastModified(AIndex : Integer; AValue : TDate); virtual;
    Procedure Setsubsets(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setvariants(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setversion(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property category : String Index 0 Read Fcategory Write Setcategory;
    Property family : String Index 8 Read Ffamily Write Setfamily;
    Property files : TWebfontTypefiles Index 16 Read Ffiles Write Setfiles;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property lastModified : TDate Index 32 Read FlastModified Write SetlastModified;
    Property subsets : TStringArray Index 40 Read Fsubsets Write Setsubsets;
    Property variants : TStringArray Index 48 Read Fvariants Write Setvariants;
    Property version : String Index 56 Read Fversion Write Setversion;
  end;
  TWebfontClass = Class of TWebfont;
  
  { --------------------------------------------------------------------
    TWebfontList
    --------------------------------------------------------------------}
  
  TWebfontList = Class(TGoogleBaseObject)
  Private
    Fitems : TWebfontListTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TWebfontListTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TWebfontListTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TWebfontListClass = Class of TWebfontList;
  
  { --------------------------------------------------------------------
    TWebfontsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TWebfontsResource, method List
  
  TWebfontsListOptions = Record
    sort : String;
  end;
  
  TWebfontsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TWebfontList;
    Function List(AQuery : TWebfontslistOptions) : TWebfontList;
  end;
  
  
  { --------------------------------------------------------------------
    TWebfontsAPI
    --------------------------------------------------------------------}
  
  TWebfontsAPI = Class(TGoogleAPI)
  Private
    FWebfontsInstance : TWebfontsResource;
    Function GetWebfontsInstance : TWebfontsResource;virtual;
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
    Function CreateWebfontsResource(AOwner : TComponent) : TWebfontsResource;virtual;overload;
    Function CreateWebfontsResource : TWebfontsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property WebfontsResource : TWebfontsResource Read GetWebfontsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TWebfontTypefiles
  --------------------------------------------------------------------}


Class Function TWebfontTypefiles.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TWebfont
  --------------------------------------------------------------------}


Procedure TWebfont.Setcategory(AIndex : Integer; AValue : String); 

begin
  If (Fcategory=AValue) then exit;
  Fcategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebfont.Setfamily(AIndex : Integer; AValue : String); 

begin
  If (Ffamily=AValue) then exit;
  Ffamily:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebfont.Setfiles(AIndex : Integer; AValue : TWebfontTypefiles); 

begin
  If (Ffiles=AValue) then exit;
  Ffiles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebfont.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebfont.SetlastModified(AIndex : Integer; AValue : TDate); 

begin
  If (FlastModified=AValue) then exit;
  FlastModified:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebfont.Setsubsets(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fsubsets=AValue) then exit;
  Fsubsets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebfont.Setvariants(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fvariants=AValue) then exit;
  Fvariants:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebfont.Setversion(AIndex : Integer; AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TWebfont.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'subsets' : SetLength(Fsubsets,ALength);
  'variants' : SetLength(Fvariants,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWebfontList
  --------------------------------------------------------------------}


Procedure TWebfontList.Setitems(AIndex : Integer; AValue : TWebfontListTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWebfontList.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TWebfontList.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWebfontsResource
  --------------------------------------------------------------------}


Class Function TWebfontsResource.ResourceName : String;

begin
  Result:='webfonts';
end;

Class Function TWebfontsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TwebfontsAPI;
end;

Function TWebfontsResource.List(AQuery : string = '') : TWebfontList;

Const
  _HTTPMethod = 'GET';
  _Path       = 'webfonts';
  _Methodid   = 'webfonts.webfonts.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TWebfontList) as TWebfontList;
end;


Function TWebfontsResource.List(AQuery : TWebfontslistOptions) : TWebfontList;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'sort',AQuery.sort);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TWebfontsAPI
  --------------------------------------------------------------------}

Class Function TWebfontsAPI.APIName : String;

begin
  Result:='webfonts';
end;

Class Function TWebfontsAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TWebfontsAPI.APIRevision : String;

begin
  Result:='20140210';
end;

Class Function TWebfontsAPI.APIID : String;

begin
  Result:='webfonts:v1';
end;

Class Function TWebfontsAPI.APITitle : String;

begin
  Result:='Google Fonts Developer API';
end;

Class Function TWebfontsAPI.APIDescription : String;

begin
  Result:='The Google Fonts Developer API.';
end;

Class Function TWebfontsAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TWebfontsAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TWebfontsAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/feature/font_api-16.png';
end;

Class Function TWebfontsAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/feature/font_api-32.gif';
end;

Class Function TWebfontsAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/fonts/docs/developer_api';
end;

Class Function TWebfontsAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TWebfontsAPI.APIbasePath : string;

begin
  Result:='/webfonts/v1/';
end;

Class Function TWebfontsAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/webfonts/v1/';
end;

Class Function TWebfontsAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TWebfontsAPI.APIservicePath : string;

begin
  Result:='webfonts/v1/';
end;

Class Function TWebfontsAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TWebfontsAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TWebfontsAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TWebfontsAPI.RegisterAPIResources;

begin
  TWebfontTypefiles.RegisterObject;
  TWebfont.RegisterObject;
  TWebfontList.RegisterObject;
end;


Function TWebfontsAPI.GetWebfontsInstance : TWebfontsResource;

begin
  if (FWebfontsInstance=Nil) then
    FWebfontsInstance:=CreateWebfontsResource;
  Result:=FWebfontsInstance;
end;

Function TWebfontsAPI.CreateWebfontsResource : TWebfontsResource;

begin
  Result:=CreateWebfontsResource(Self);
end;


Function TWebfontsAPI.CreateWebfontsResource(AOwner : TComponent) : TWebfontsResource;

begin
  Result:=TWebfontsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TWebfontsAPI.RegisterAPI;
end.
