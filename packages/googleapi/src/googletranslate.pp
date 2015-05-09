unit googletranslate;
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
//Generated on: 9-5-15 13:22:59
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TDetectionsListResponse = class;
  TDetectionsResourceItem = class;
  TLanguagesListResponse = class;
  TLanguagesResource = class;
  TTranslationsListResponse = class;
  TTranslationsResource = class;
  TDetectionsListResponseArray = Array of TDetectionsListResponse;
  TDetectionsResourceItemArray = Array of TDetectionsResourceItem;
  TDetectionsResource = Array of TDetectionsResourceItem;
  TLanguagesListResponseArray = Array of TLanguagesListResponse;
  TLanguagesResourceArray = Array of TLanguagesResource;
  TTranslationsListResponseArray = Array of TTranslationsListResponse;
  TTranslationsResourceArray = Array of TTranslationsResource;
  //Anonymous types, using auto-generated names
  TDetectionsListResponseTypedetectionsArray = Array of TDetectionsResource;
  TLanguagesListResponseTypelanguagesArray = Array of TLanguagesResource;
  TTranslationsListResponseTypetranslationsArray = Array of TTranslationsResource;
  
  { --------------------------------------------------------------------
    TDetectionsListResponse
    --------------------------------------------------------------------}
  
  TDetectionsListResponse = Class(TGoogleBaseObject)
  Private
    Fdetections : TDetectionsListResponseTypedetectionsArray;
  Protected
    //Property setters
    Procedure Setdetections(AIndex : Integer; AValue : TDetectionsListResponseTypedetectionsArray); virtual;
  Public
  Published
    Property detections : TDetectionsListResponseTypedetectionsArray Index 0 Read Fdetections Write Setdetections;
  end;
  TDetectionsListResponseClass = Class of TDetectionsListResponse;
  
  { --------------------------------------------------------------------
    TDetectionsResourceItem
    --------------------------------------------------------------------}
  
  TDetectionsResourceItem = Class(TGoogleBaseObject)
  Private
    Fconfidence : integer;
    FisReliable : boolean;
    Flanguage : String;
  Protected
    //Property setters
    Procedure Setconfidence(AIndex : Integer; AValue : integer); virtual;
    Procedure SetisReliable(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property confidence : integer Index 0 Read Fconfidence Write Setconfidence;
    Property isReliable : boolean Index 8 Read FisReliable Write SetisReliable;
    Property language : String Index 16 Read Flanguage Write Setlanguage;
  end;
  TDetectionsResourceItemClass = Class of TDetectionsResourceItem;
  
  { --------------------------------------------------------------------
    TLanguagesListResponse
    --------------------------------------------------------------------}
  
  TLanguagesListResponse = Class(TGoogleBaseObject)
  Private
    Flanguages : TLanguagesListResponseTypelanguagesArray;
  Protected
    //Property setters
    Procedure Setlanguages(AIndex : Integer; AValue : TLanguagesListResponseTypelanguagesArray); virtual;
  Public
  Published
    Property languages : TLanguagesListResponseTypelanguagesArray Index 0 Read Flanguages Write Setlanguages;
  end;
  TLanguagesListResponseClass = Class of TLanguagesListResponse;
  
  { --------------------------------------------------------------------
    TLanguagesResource
    --------------------------------------------------------------------}
  
  TLanguagesResource = Class(TGoogleBaseObject)
  Private
    Flanguage : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setlanguage(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property language : String Index 0 Read Flanguage Write Setlanguage;
    Property name : String Index 8 Read Fname Write Setname;
  end;
  TLanguagesResourceClass = Class of TLanguagesResource;
  
  { --------------------------------------------------------------------
    TTranslationsListResponse
    --------------------------------------------------------------------}
  
  TTranslationsListResponse = Class(TGoogleBaseObject)
  Private
    Ftranslations : TTranslationsListResponseTypetranslationsArray;
  Protected
    //Property setters
    Procedure Settranslations(AIndex : Integer; AValue : TTranslationsListResponseTypetranslationsArray); virtual;
  Public
  Published
    Property translations : TTranslationsListResponseTypetranslationsArray Index 0 Read Ftranslations Write Settranslations;
  end;
  TTranslationsListResponseClass = Class of TTranslationsListResponse;
  
  { --------------------------------------------------------------------
    TTranslationsResource
    --------------------------------------------------------------------}
  
  TTranslationsResource = Class(TGoogleBaseObject)
  Private
    FdetectedSourceLanguage : String;
    FtranslatedText : String;
  Protected
    //Property setters
    Procedure SetdetectedSourceLanguage(AIndex : Integer; AValue : String); virtual;
    Procedure SettranslatedText(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property detectedSourceLanguage : String Index 0 Read FdetectedSourceLanguage Write SetdetectedSourceLanguage;
    Property translatedText : String Index 8 Read FtranslatedText Write SettranslatedText;
  end;
  TTranslationsResourceClass = Class of TTranslationsResource;
  
  { --------------------------------------------------------------------
    TDetections_Resource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDetections_Resource, method List
  
  TDetectionsListOptions = Record
    q : String;
  end;
  
  TDetections_Resource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TDetectionsListResponse;
    Function List(AQuery : TDetectionslistOptions) : TDetectionsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TLanguages_Resource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TLanguages_Resource, method List
  
  TLanguagesListOptions = Record
    target : String;
  end;
  
  TLanguages_Resource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TLanguagesListResponse;
    Function List(AQuery : TLanguageslistOptions) : TLanguagesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TTranslations_Resource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TTranslations_Resource, method List
  
  TTranslationsListOptions = Record
    cid : String;
    format : String;
    q : String;
    source : String;
    target : String;
  end;
  
  TTranslations_Resource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TTranslationsListResponse;
    Function List(AQuery : TTranslationslistOptions) : TTranslationsListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TTranslateAPI
    --------------------------------------------------------------------}
  
  TTranslateAPI = Class(TGoogleAPI)
  Private
    FDetectionsInstance : TDetections_Resource;
    FLanguagesInstance : TLanguages_Resource;
    FTranslationsInstance : TTranslations_Resource;
    Function GetDetectionsInstance : TDetections_Resource;virtual;
    Function GetLanguagesInstance : TLanguages_Resource;virtual;
    Function GetTranslationsInstance : TTranslations_Resource;virtual;
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
    Function CreateDetectionsResource(AOwner : TComponent) : TDetections_Resource;virtual;overload;
    Function CreateDetectionsResource : TDetections_Resource;virtual;overload;
    Function CreateLanguagesResource(AOwner : TComponent) : TLanguages_Resource;virtual;overload;
    Function CreateLanguagesResource : TLanguages_Resource;virtual;overload;
    Function CreateTranslationsResource(AOwner : TComponent) : TTranslations_Resource;virtual;overload;
    Function CreateTranslationsResource : TTranslations_Resource;virtual;overload;
    //Add default on-demand instances for resources
    Property DetectionsResource : TDetections_Resource Read GetDetectionsInstance;
    Property LanguagesResource : TLanguages_Resource Read GetLanguagesInstance;
    Property TranslationsResource : TTranslations_Resource Read GetTranslationsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TDetectionsListResponse
  --------------------------------------------------------------------}


Procedure TDetectionsListResponse.Setdetections(AIndex : Integer; AValue : TDetectionsListResponseTypedetectionsArray); 

begin
  If (Fdetections=AValue) then exit;
  Fdetections:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDetectionsResourceItem
  --------------------------------------------------------------------}


Procedure TDetectionsResourceItem.Setconfidence(AIndex : Integer; AValue : integer); 

begin
  If (Fconfidence=AValue) then exit;
  Fconfidence:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDetectionsResourceItem.SetisReliable(AIndex : Integer; AValue : boolean); 

begin
  If (FisReliable=AValue) then exit;
  FisReliable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDetectionsResourceItem.Setlanguage(AIndex : Integer; AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLanguagesListResponse
  --------------------------------------------------------------------}


Procedure TLanguagesListResponse.Setlanguages(AIndex : Integer; AValue : TLanguagesListResponseTypelanguagesArray); 

begin
  If (Flanguages=AValue) then exit;
  Flanguages:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLanguagesResource
  --------------------------------------------------------------------}


Procedure TLanguagesResource.Setlanguage(AIndex : Integer; AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLanguagesResource.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTranslationsListResponse
  --------------------------------------------------------------------}


Procedure TTranslationsListResponse.Settranslations(AIndex : Integer; AValue : TTranslationsListResponseTypetranslationsArray); 

begin
  If (Ftranslations=AValue) then exit;
  Ftranslations:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTranslationsResource
  --------------------------------------------------------------------}


Procedure TTranslationsResource.SetdetectedSourceLanguage(AIndex : Integer; AValue : String); 

begin
  If (FdetectedSourceLanguage=AValue) then exit;
  FdetectedSourceLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTranslationsResource.SettranslatedText(AIndex : Integer; AValue : String); 

begin
  If (FtranslatedText=AValue) then exit;
  FtranslatedText:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDetections_Resource
  --------------------------------------------------------------------}


Class Function TDetections_Resource.ResourceName : String;

begin
  Result:='detections';
end;

Class Function TDetections_Resource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtranslateAPI;
end;

Function TDetections_Resource.List(AQuery : string = '') : TDetectionsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2/detect';
  _Methodid   = 'language.detections.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TDetectionsListResponse) as TDetectionsListResponse;
end;


Function TDetections_Resource.List(AQuery : TDetectionslistOptions) : TDetectionsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'q',AQuery.q);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TLanguages_Resource
  --------------------------------------------------------------------}


Class Function TLanguages_Resource.ResourceName : String;

begin
  Result:='languages';
end;

Class Function TLanguages_Resource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtranslateAPI;
end;

Function TLanguages_Resource.List(AQuery : string = '') : TLanguagesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2/languages';
  _Methodid   = 'language.languages.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TLanguagesListResponse) as TLanguagesListResponse;
end;


Function TLanguages_Resource.List(AQuery : TLanguageslistOptions) : TLanguagesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'target',AQuery.target);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TTranslations_Resource
  --------------------------------------------------------------------}


Class Function TTranslations_Resource.ResourceName : String;

begin
  Result:='translations';
end;

Class Function TTranslations_Resource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TtranslateAPI;
end;

Function TTranslations_Resource.List(AQuery : string = '') : TTranslationsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2';
  _Methodid   = 'language.translations.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TTranslationsListResponse) as TTranslationsListResponse;
end;


Function TTranslations_Resource.List(AQuery : TTranslationslistOptions) : TTranslationsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'cid',AQuery.cid);
  AddToQuery(_Q,'format',AQuery.format);
  AddToQuery(_Q,'q',AQuery.q);
  AddToQuery(_Q,'source',AQuery.source);
  AddToQuery(_Q,'target',AQuery.target);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TTranslateAPI
  --------------------------------------------------------------------}

Class Function TTranslateAPI.APIName : String;

begin
  Result:='translate';
end;

Class Function TTranslateAPI.APIVersion : String;

begin
  Result:='v2';
end;

Class Function TTranslateAPI.APIRevision : String;

begin
  Result:='20141123';
end;

Class Function TTranslateAPI.APIID : String;

begin
  Result:='translate:v2';
end;

Class Function TTranslateAPI.APITitle : String;

begin
  Result:='Translate API';
end;

Class Function TTranslateAPI.APIDescription : String;

begin
  Result:='Lets you translate text from one language to another';
end;

Class Function TTranslateAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TTranslateAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TTranslateAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/translate-16.png';
end;

Class Function TTranslateAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/translate-32.png';
end;

Class Function TTranslateAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/translate/v2/using_rest';
end;

Class Function TTranslateAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TTranslateAPI.APIbasePath : string;

begin
  Result:='/language/translate/';
end;

Class Function TTranslateAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/language/translate/';
end;

Class Function TTranslateAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TTranslateAPI.APIservicePath : string;

begin
  Result:='language/translate/';
end;

Class Function TTranslateAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TTranslateAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TTranslateAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TTranslateAPI.RegisterAPIResources;

begin
  TDetectionsListResponse.RegisterObject;
  TDetectionsResourceItem.RegisterObject;
  TLanguagesListResponse.RegisterObject;
  TLanguagesResource.RegisterObject;
  TTranslationsListResponse.RegisterObject;
  TTranslationsResource.RegisterObject;
end;


Function TTranslateAPI.GetDetectionsInstance : TDetections_Resource;

begin
  if (FDetectionsInstance=Nil) then
    FDetectionsInstance:=CreateDetectionsResource;
  Result:=FDetectionsInstance;
end;

Function TTranslateAPI.CreateDetectionsResource : TDetections_Resource;

begin
  Result:=CreateDetectionsResource(Self);
end;


Function TTranslateAPI.CreateDetectionsResource(AOwner : TComponent) : TDetections_Resource;

begin
  Result:=TDetections_Resource.Create(AOwner);
  Result.API:=Self;
end;



Function TTranslateAPI.GetLanguagesInstance : TLanguages_Resource;

begin
  if (FLanguagesInstance=Nil) then
    FLanguagesInstance:=CreateLanguagesResource;
  Result:=FLanguagesInstance;
end;

Function TTranslateAPI.CreateLanguagesResource : TLanguages_Resource;

begin
  Result:=CreateLanguagesResource(Self);
end;


Function TTranslateAPI.CreateLanguagesResource(AOwner : TComponent) : TLanguages_Resource;

begin
  Result:=TLanguages_Resource.Create(AOwner);
  Result.API:=Self;
end;



Function TTranslateAPI.GetTranslationsInstance : TTranslations_Resource;

begin
  if (FTranslationsInstance=Nil) then
    FTranslationsInstance:=CreateTranslationsResource;
  Result:=FTranslationsInstance;
end;

Function TTranslateAPI.CreateTranslationsResource : TTranslations_Resource;

begin
  Result:=CreateTranslationsResource(Self);
end;


Function TTranslateAPI.CreateTranslationsResource(AOwner : TComponent) : TTranslations_Resource;

begin
  Result:=TTranslations_Resource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TTranslateAPI.RegisterAPI;
end.
