unit googleacceleratedmobilepageurl;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAmpUrl = Class;
  TAmpUrlError = Class;
  TBatchGetAmpUrlsRequest = Class;
  TBatchGetAmpUrlsResponse = Class;
  TAmpUrlArray = Array of TAmpUrl;
  TAmpUrlErrorArray = Array of TAmpUrlError;
  TBatchGetAmpUrlsRequestArray = Array of TBatchGetAmpUrlsRequest;
  TBatchGetAmpUrlsResponseArray = Array of TBatchGetAmpUrlsResponse;
  //Anonymous types, using auto-generated names
  TBatchGetAmpUrlsResponseTypeurlErrorsArray = Array of TAmpUrlError;
  TBatchGetAmpUrlsResponseTypeampUrlsArray = Array of TAmpUrl;
  
  { --------------------------------------------------------------------
    TAmpUrl
    --------------------------------------------------------------------}
  
  TAmpUrl = Class(TGoogleBaseObject)
  Private
    FampUrl : String;
    ForiginalUrl : String;
    FcdnAmpUrl : String;
  Protected
    //Property setters
    Procedure SetampUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoriginalUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcdnAmpUrl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property ampUrl : String Index 0 Read FampUrl Write SetampUrl;
    Property originalUrl : String Index 8 Read ForiginalUrl Write SetoriginalUrl;
    Property cdnAmpUrl : String Index 16 Read FcdnAmpUrl Write SetcdnAmpUrl;
  end;
  TAmpUrlClass = Class of TAmpUrl;
  
  { --------------------------------------------------------------------
    TAmpUrlError
    --------------------------------------------------------------------}
  
  TAmpUrlError = Class(TGoogleBaseObject)
  Private
    ForiginalUrl : String;
    FerrorCode : String;
    FerrorMessage : String;
  Protected
    //Property setters
    Procedure SetoriginalUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SeterrorCode(AIndex : Integer; const AValue : String); virtual;
    Procedure SeterrorMessage(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property originalUrl : String Index 0 Read ForiginalUrl Write SetoriginalUrl;
    Property errorCode : String Index 8 Read FerrorCode Write SeterrorCode;
    Property errorMessage : String Index 16 Read FerrorMessage Write SeterrorMessage;
  end;
  TAmpUrlErrorClass = Class of TAmpUrlError;
  
  { --------------------------------------------------------------------
    TBatchGetAmpUrlsRequest
    --------------------------------------------------------------------}
  
  TBatchGetAmpUrlsRequest = Class(TGoogleBaseObject)
  Private
    Furls : TStringArray;
  Protected
    //Property setters
    Procedure Seturls(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property urls : TStringArray Index 0 Read Furls Write Seturls;
  end;
  TBatchGetAmpUrlsRequestClass = Class of TBatchGetAmpUrlsRequest;
  
  { --------------------------------------------------------------------
    TBatchGetAmpUrlsResponse
    --------------------------------------------------------------------}
  
  TBatchGetAmpUrlsResponse = Class(TGoogleBaseObject)
  Private
    FurlErrors : TBatchGetAmpUrlsResponseTypeurlErrorsArray;
    FampUrls : TBatchGetAmpUrlsResponseTypeampUrlsArray;
  Protected
    //Property setters
    Procedure SeturlErrors(AIndex : Integer; const AValue : TBatchGetAmpUrlsResponseTypeurlErrorsArray); virtual;
    Procedure SetampUrls(AIndex : Integer; const AValue : TBatchGetAmpUrlsResponseTypeampUrlsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property urlErrors : TBatchGetAmpUrlsResponseTypeurlErrorsArray Index 0 Read FurlErrors Write SeturlErrors;
    Property ampUrls : TBatchGetAmpUrlsResponseTypeampUrlsArray Index 8 Read FampUrls Write SetampUrls;
  end;
  TBatchGetAmpUrlsResponseClass = Class of TBatchGetAmpUrlsResponse;
  
  { --------------------------------------------------------------------
    TAmpUrlsResource
    --------------------------------------------------------------------}
  
  TAmpUrlsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function BatchGet(aBatchGetAmpUrlsRequest : TBatchGetAmpUrlsRequest) : TBatchGetAmpUrlsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TAcceleratedmobilepageurlAPI
    --------------------------------------------------------------------}
  
  TAcceleratedmobilepageurlAPI = Class(TGoogleAPI)
  Private
    FAmpUrlsInstance : TAmpUrlsResource;
    Function GetAmpUrlsInstance : TAmpUrlsResource;virtual;
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
    Function CreateAmpUrlsResource(AOwner : TComponent) : TAmpUrlsResource;virtual;overload;
    Function CreateAmpUrlsResource : TAmpUrlsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AmpUrlsResource : TAmpUrlsResource Read GetAmpUrlsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAmpUrl
  --------------------------------------------------------------------}


Procedure TAmpUrl.SetampUrl(AIndex : Integer; const AValue : String); 

begin
  If (FampUrl=AValue) then exit;
  FampUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAmpUrl.SetoriginalUrl(AIndex : Integer; const AValue : String); 

begin
  If (ForiginalUrl=AValue) then exit;
  ForiginalUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAmpUrl.SetcdnAmpUrl(AIndex : Integer; const AValue : String); 

begin
  If (FcdnAmpUrl=AValue) then exit;
  FcdnAmpUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAmpUrlError
  --------------------------------------------------------------------}


Procedure TAmpUrlError.SetoriginalUrl(AIndex : Integer; const AValue : String); 

begin
  If (ForiginalUrl=AValue) then exit;
  ForiginalUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAmpUrlError.SeterrorCode(AIndex : Integer; const AValue : String); 

begin
  If (FerrorCode=AValue) then exit;
  FerrorCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAmpUrlError.SeterrorMessage(AIndex : Integer; const AValue : String); 

begin
  If (FerrorMessage=AValue) then exit;
  FerrorMessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchGetAmpUrlsRequest
  --------------------------------------------------------------------}


Procedure TBatchGetAmpUrlsRequest.Seturls(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Furls=AValue) then exit;
  Furls:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBatchGetAmpUrlsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'urls' : SetLength(Furls,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBatchGetAmpUrlsResponse
  --------------------------------------------------------------------}


Procedure TBatchGetAmpUrlsResponse.SeturlErrors(AIndex : Integer; const AValue : TBatchGetAmpUrlsResponseTypeurlErrorsArray); 

begin
  If (FurlErrors=AValue) then exit;
  FurlErrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchGetAmpUrlsResponse.SetampUrls(AIndex : Integer; const AValue : TBatchGetAmpUrlsResponseTypeampUrlsArray); 

begin
  If (FampUrls=AValue) then exit;
  FampUrls:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBatchGetAmpUrlsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'urlerrors' : SetLength(FurlErrors,ALength);
  'ampurls' : SetLength(FampUrls,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAmpUrlsResource
  --------------------------------------------------------------------}


Class Function TAmpUrlsResource.ResourceName : String;

begin
  Result:='ampUrls';
end;

Class Function TAmpUrlsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TacceleratedmobilepageurlAPI;
end;

Function TAmpUrlsResource.BatchGet(aBatchGetAmpUrlsRequest : TBatchGetAmpUrlsRequest) : TBatchGetAmpUrlsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/ampUrls:batchGet';
  _Methodid   = 'acceleratedmobilepageurl.ampUrls.batchGet';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aBatchGetAmpUrlsRequest,TBatchGetAmpUrlsResponse) as TBatchGetAmpUrlsResponse;
end;



{ --------------------------------------------------------------------
  TAcceleratedmobilepageurlAPI
  --------------------------------------------------------------------}

Class Function TAcceleratedmobilepageurlAPI.APIName : String;

begin
  Result:='acceleratedmobilepageurl';
end;

Class Function TAcceleratedmobilepageurlAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TAcceleratedmobilepageurlAPI.APIRevision : String;

begin
  Result:='20160518';
end;

Class Function TAcceleratedmobilepageurlAPI.APIID : String;

begin
  Result:='acceleratedmobilepageurl:v1';
end;

Class Function TAcceleratedmobilepageurlAPI.APITitle : String;

begin
  Result:='Accelerated Mobile Page (AMP) URL API';
end;

Class Function TAcceleratedmobilepageurlAPI.APIDescription : String;

begin
  Result:='This API contains a single method, [batchGet](/amp/cache/reference/acceleratedmobilepageurl/rest/v1/ampUrls/batchGet). Call this method to retrieve the AMP URL (and equivalent AMP Cache URL) for given public URL(s). For more information, see [Link to AMP Content](/amp/cache/use-amp-url).';
end;

Class Function TAcceleratedmobilepageurlAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TAcceleratedmobilepageurlAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TAcceleratedmobilepageurlAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TAcceleratedmobilepageurlAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TAcceleratedmobilepageurlAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/amp/cache/';
end;

Class Function TAcceleratedmobilepageurlAPI.APIrootUrl : string;

begin
  Result:='https://acceleratedmobilepageurl.googleapis.com/';
end;

Class Function TAcceleratedmobilepageurlAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TAcceleratedmobilepageurlAPI.APIbaseURL : String;

begin
  Result:='https://acceleratedmobilepageurl.googleapis.com/';
end;

Class Function TAcceleratedmobilepageurlAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TAcceleratedmobilepageurlAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TAcceleratedmobilepageurlAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TAcceleratedmobilepageurlAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TAcceleratedmobilepageurlAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TAcceleratedmobilepageurlAPI.RegisterAPIResources;

begin
  TAmpUrl.RegisterObject;
  TAmpUrlError.RegisterObject;
  TBatchGetAmpUrlsRequest.RegisterObject;
  TBatchGetAmpUrlsResponse.RegisterObject;
end;


Function TAcceleratedmobilepageurlAPI.GetAmpUrlsInstance : TAmpUrlsResource;

begin
  if (FAmpUrlsInstance=Nil) then
    FAmpUrlsInstance:=CreateAmpUrlsResource;
  Result:=FAmpUrlsInstance;
end;

Function TAcceleratedmobilepageurlAPI.CreateAmpUrlsResource : TAmpUrlsResource;

begin
  Result:=CreateAmpUrlsResource(Self);
end;


Function TAcceleratedmobilepageurlAPI.CreateAmpUrlsResource(AOwner : TComponent) : TAmpUrlsResource;

begin
  Result:=TAmpUrlsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TAcceleratedmobilepageurlAPI.RegisterAPI;
end.
