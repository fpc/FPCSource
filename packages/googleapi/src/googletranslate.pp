unit googletranslate;
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
  TDetectionsListResponse = class;
  TDetectionsListResponseArray = Array of TDetectionsListResponse;
  TDetectionsListResponsedetections = class;
  TDetectionsListResponsedetectionsArray = Array of TDetectionsListResponsedetections;
  TDetectionsResource = class;
  TDetectionsResourceArray = Array of TDetectionsResource;
  TLanguagesListResponse = class;
  TLanguagesListResponseArray = Array of TLanguagesListResponse;
  TLanguagesListResponselanguages = class;
  TLanguagesListResponselanguagesArray = Array of TLanguagesListResponselanguages;
  TLanguagesResource = class;
  TLanguagesResourceArray = Array of TLanguagesResource;
  TTranslationsListResponse = class;
  TTranslationsListResponseArray = Array of TTranslationsListResponse;
  TTranslationsListResponsetranslations = class;
  TTranslationsListResponsetranslationsArray = Array of TTranslationsListResponsetranslations;
  TTranslationsResource = class;
  TTranslationsResourceArray = Array of TTranslationsResource;
  
  { --------------------------------------------------------------------
    TDetectionsListResponse
    --------------------------------------------------------------------}
  
  TDetectionsListResponse = Class(TGoogleBaseObject)
  Private
    Fdetections : TDetectionsListResponsedetections;
  Protected
    //Property setters
    Procedure Setdetections(AIndex : Integer; AValue : TDetectionsListResponsedetections); virtual;
  Public
  Published
    Property detections : TDetectionsListResponsedetections Index 0 Read Fdetections Write Setdetections;
  end;
  TDetectionsListResponseClass = Class of TDetectionsListResponse;
  
  { --------------------------------------------------------------------
    TDetectionsListResponsedetections
    --------------------------------------------------------------------}
  
  TDetectionsListResponsedetections = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDetectionsListResponsedetectionsClass = Class of TDetectionsListResponsedetections;
  
  { --------------------------------------------------------------------
    TDetectionsResource
    --------------------------------------------------------------------}
  
  TDetectionsResource = Class(TGoogleBaseObject)
  Private
    Fconfidence : integer;
    FisReliable : boolean;
    Flanguage : string;
  Protected
    //Property setters
    Procedure Setconfidence(AIndex : Integer; AValue : integer); virtual;
    Procedure SetisReliable(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property confidence : integer Index 0 Read Fconfidence Write Setconfidence;
    Property isReliable : boolean Index 8 Read FisReliable Write SetisReliable;
    Property language : string Index 16 Read Flanguage Write Setlanguage;
  end;
  TDetectionsResourceClass = Class of TDetectionsResource;
  
  { --------------------------------------------------------------------
    TLanguagesListResponse
    --------------------------------------------------------------------}
  
  TLanguagesListResponse = Class(TGoogleBaseObject)
  Private
    Flanguages : TLanguagesListResponselanguages;
  Protected
    //Property setters
    Procedure Setlanguages(AIndex : Integer; AValue : TLanguagesListResponselanguages); virtual;
  Public
  Published
    Property languages : TLanguagesListResponselanguages Index 0 Read Flanguages Write Setlanguages;
  end;
  TLanguagesListResponseClass = Class of TLanguagesListResponse;
  
  { --------------------------------------------------------------------
    TLanguagesListResponselanguages
    --------------------------------------------------------------------}
  
  TLanguagesListResponselanguages = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLanguagesListResponselanguagesClass = Class of TLanguagesListResponselanguages;
  
  { --------------------------------------------------------------------
    TLanguagesResource
    --------------------------------------------------------------------}
  
  TLanguagesResource = Class(TGoogleBaseObject)
  Private
    Flanguage : string;
    Fname : string;
  Protected
    //Property setters
    Procedure Setlanguage(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property language : string Index 0 Read Flanguage Write Setlanguage;
    Property name : string Index 8 Read Fname Write Setname;
  end;
  TLanguagesResourceClass = Class of TLanguagesResource;
  
  { --------------------------------------------------------------------
    TTranslationsListResponse
    --------------------------------------------------------------------}
  
  TTranslationsListResponse = Class(TGoogleBaseObject)
  Private
    Ftranslations : TTranslationsListResponsetranslations;
  Protected
    //Property setters
    Procedure Settranslations(AIndex : Integer; AValue : TTranslationsListResponsetranslations); virtual;
  Public
  Published
    Property translations : TTranslationsListResponsetranslations Index 0 Read Ftranslations Write Settranslations;
  end;
  TTranslationsListResponseClass = Class of TTranslationsListResponse;
  
  { --------------------------------------------------------------------
    TTranslationsListResponsetranslations
    --------------------------------------------------------------------}
  
  TTranslationsListResponsetranslations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTranslationsListResponsetranslationsClass = Class of TTranslationsListResponsetranslations;
  
  { --------------------------------------------------------------------
    TTranslationsResource
    --------------------------------------------------------------------}
  
  TTranslationsResource = Class(TGoogleBaseObject)
  Private
    FdetectedSourceLanguage : string;
    FtranslatedText : string;
  Protected
    //Property setters
    Procedure SetdetectedSourceLanguage(AIndex : Integer; AValue : string); virtual;
    Procedure SettranslatedText(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property detectedSourceLanguage : string Index 0 Read FdetectedSourceLanguage Write SetdetectedSourceLanguage;
    Property translatedText : string Index 8 Read FtranslatedText Write SettranslatedText;
  end;
  TTranslationsResourceClass = Class of TTranslationsResource;
  
  { --------------------------------------------------------------------
    TDetections_Resource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDetections_Resource, method List
  
  TDetectionsListOptions = Record
    q : string;
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
    target : string;
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
    cid : string;
    format : string;
    q : string;
    source : string;
    target : string;
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


Procedure TDetectionsListResponse.Setdetections(AIndex : Integer; AValue : TDetectionsListResponsedetections); 

begin
  If (Fdetections=AValue) then exit;
  Fdetections:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDetectionsListResponsedetections
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDetectionsResource
  --------------------------------------------------------------------}


Procedure TDetectionsResource.Setconfidence(AIndex : Integer; AValue : integer); 

begin
  If (Fconfidence=AValue) then exit;
  Fconfidence:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDetectionsResource.SetisReliable(AIndex : Integer; AValue : boolean); 

begin
  If (FisReliable=AValue) then exit;
  FisReliable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDetectionsResource.Setlanguage(AIndex : Integer; AValue : string); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLanguagesListResponse
  --------------------------------------------------------------------}


Procedure TLanguagesListResponse.Setlanguages(AIndex : Integer; AValue : TLanguagesListResponselanguages); 

begin
  If (Flanguages=AValue) then exit;
  Flanguages:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLanguagesListResponselanguages
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLanguagesResource
  --------------------------------------------------------------------}


Procedure TLanguagesResource.Setlanguage(AIndex : Integer; AValue : string); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLanguagesResource.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTranslationsListResponse
  --------------------------------------------------------------------}


Procedure TTranslationsListResponse.Settranslations(AIndex : Integer; AValue : TTranslationsListResponsetranslations); 

begin
  If (Ftranslations=AValue) then exit;
  Ftranslations:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTranslationsListResponsetranslations
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTranslationsResource
  --------------------------------------------------------------------}


Procedure TTranslationsResource.SetdetectedSourceLanguage(AIndex : Integer; AValue : string); 

begin
  If (FdetectedSourceLanguage=AValue) then exit;
  FdetectedSourceLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTranslationsResource.SettranslatedText(AIndex : Integer; AValue : string); 

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
  TDetectionsListResponsedetections.RegisterObject;
  TDetectionsResource.RegisterObject;
  TLanguagesListResponse.RegisterObject;
  TLanguagesListResponselanguages.RegisterObject;
  TLanguagesResource.RegisterObject;
  TTranslationsListResponse.RegisterObject;
  TTranslationsListResponsetranslations.RegisterObject;
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
