unit googleconsumersurveys;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TFieldMask = Class;
  TPageInfo = Class;
  TResultsGetRequest = Class;
  TResultsMask = Class;
  TSurvey = Class;
  TSurveyAudience = Class;
  TSurveyCost = Class;
  TSurveyQuestion = Class;
  TSurveyQuestionImage = Class;
  TSurveyResults = Class;
  TSurveysListResponse = Class;
  TSurveysStartRequest = Class;
  TSurveysStartResponse = Class;
  TSurveysStopResponse = Class;
  TTokenPagination = Class;
  TFieldMaskArray = Array of TFieldMask;
  TPageInfoArray = Array of TPageInfo;
  TResultsGetRequestArray = Array of TResultsGetRequest;
  TResultsMaskArray = Array of TResultsMask;
  TSurveyArray = Array of TSurvey;
  TSurveyAudienceArray = Array of TSurveyAudience;
  TSurveyCostArray = Array of TSurveyCost;
  TSurveyQuestionArray = Array of TSurveyQuestion;
  TSurveyQuestionImageArray = Array of TSurveyQuestionImage;
  TSurveyResultsArray = Array of TSurveyResults;
  TSurveysListResponseArray = Array of TSurveysListResponse;
  TSurveysStartRequestArray = Array of TSurveysStartRequest;
  TSurveysStartResponseArray = Array of TSurveysStartResponse;
  TSurveysStopResponseArray = Array of TSurveysStopResponse;
  TTokenPaginationArray = Array of TTokenPagination;
  //Anonymous types, using auto-generated names
  TFieldMaskTypefieldsArray = Array of TFieldMask;
  TResultsMaskTypefieldsArray = Array of TFieldMask;
  TSurveyTypequestionsArray = Array of TSurveyQuestion;
  TSurveyQuestionTypeimagesArray = Array of TSurveyQuestionImage;
  TSurveysListResponseTyperesourcesArray = Array of TSurvey;
  
  { --------------------------------------------------------------------
    TFieldMask
    --------------------------------------------------------------------}
  
  TFieldMask = Class(TGoogleBaseObject)
  Private
    Ffields : TFieldMaskTypefieldsArray;
    Fid : integer;
  Protected
    //Property setters
    Procedure Setfields(AIndex : Integer; const AValue : TFieldMaskTypefieldsArray); virtual;
    Procedure Setid(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property fields : TFieldMaskTypefieldsArray Index 0 Read Ffields Write Setfields;
    Property id : integer Index 8 Read Fid Write Setid;
  end;
  TFieldMaskClass = Class of TFieldMask;
  
  { --------------------------------------------------------------------
    TPageInfo
    --------------------------------------------------------------------}
  
  TPageInfo = Class(TGoogleBaseObject)
  Private
    FresultPerPage : integer;
    FstartIndex : integer;
    FtotalResults : integer;
  Protected
    //Property setters
    Procedure SetresultPerPage(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetstartIndex(AIndex : Integer; const AValue : integer); virtual;
    Procedure SettotalResults(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property resultPerPage : integer Index 0 Read FresultPerPage Write SetresultPerPage;
    Property startIndex : integer Index 8 Read FstartIndex Write SetstartIndex;
    Property totalResults : integer Index 16 Read FtotalResults Write SettotalResults;
  end;
  TPageInfoClass = Class of TPageInfo;
  
  { --------------------------------------------------------------------
    TResultsGetRequest
    --------------------------------------------------------------------}
  
  TResultsGetRequest = Class(TGoogleBaseObject)
  Private
    FresultMask : TResultsMask;
  Protected
    //Property setters
    Procedure SetresultMask(AIndex : Integer; const AValue : TResultsMask); virtual;
  Public
  Published
    Property resultMask : TResultsMask Index 0 Read FresultMask Write SetresultMask;
  end;
  TResultsGetRequestClass = Class of TResultsGetRequest;
  
  { --------------------------------------------------------------------
    TResultsMask
    --------------------------------------------------------------------}
  
  TResultsMask = Class(TGoogleBaseObject)
  Private
    Ffields : TResultsMaskTypefieldsArray;
    Fprojection : String;
  Protected
    //Property setters
    Procedure Setfields(AIndex : Integer; const AValue : TResultsMaskTypefieldsArray); virtual;
    Procedure Setprojection(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property fields : TResultsMaskTypefieldsArray Index 0 Read Ffields Write Setfields;
    Property projection : String Index 8 Read Fprojection Write Setprojection;
  end;
  TResultsMaskClass = Class of TResultsMask;
  
  { --------------------------------------------------------------------
    TSurvey
    --------------------------------------------------------------------}
  
  TSurvey = Class(TGoogleBaseObject)
  Private
    Faudience : TSurveyAudience;
    Fcost : TSurveyCost;
    FcustomerData : String;
    Fdescription : String;
    Fowners : TStringArray;
    Fquestions : TSurveyTypequestionsArray;
    Fstate : String;
    FsurveyUrlId : String;
    Ftitle : String;
    FwantedResponseCount : integer;
  Protected
    //Property setters
    Procedure Setaudience(AIndex : Integer; const AValue : TSurveyAudience); virtual;
    Procedure Setcost(AIndex : Integer; const AValue : TSurveyCost); virtual;
    Procedure SetcustomerData(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setowners(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setquestions(AIndex : Integer; const AValue : TSurveyTypequestionsArray); virtual;
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsurveyUrlId(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure SetwantedResponseCount(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property audience : TSurveyAudience Index 0 Read Faudience Write Setaudience;
    Property cost : TSurveyCost Index 8 Read Fcost Write Setcost;
    Property customerData : String Index 16 Read FcustomerData Write SetcustomerData;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property owners : TStringArray Index 32 Read Fowners Write Setowners;
    Property questions : TSurveyTypequestionsArray Index 40 Read Fquestions Write Setquestions;
    Property state : String Index 48 Read Fstate Write Setstate;
    Property surveyUrlId : String Index 56 Read FsurveyUrlId Write SetsurveyUrlId;
    Property title : String Index 64 Read Ftitle Write Settitle;
    Property wantedResponseCount : integer Index 72 Read FwantedResponseCount Write SetwantedResponseCount;
  end;
  TSurveyClass = Class of TSurvey;
  
  { --------------------------------------------------------------------
    TSurveyAudience
    --------------------------------------------------------------------}
  
  TSurveyAudience = Class(TGoogleBaseObject)
  Private
    Fages : TStringArray;
    Fcountry : String;
    FcountrySubdivision : String;
    Fgender : String;
    Flanguages : TStringArray;
    FmobileAppPanelId : String;
    FpopulationSource : String;
  Protected
    //Property setters
    Procedure Setages(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setcountry(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcountrySubdivision(AIndex : Integer; const AValue : String); virtual;
    Procedure Setgender(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlanguages(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetmobileAppPanelId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpopulationSource(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property ages : TStringArray Index 0 Read Fages Write Setages;
    Property country : String Index 8 Read Fcountry Write Setcountry;
    Property countrySubdivision : String Index 16 Read FcountrySubdivision Write SetcountrySubdivision;
    Property gender : String Index 24 Read Fgender Write Setgender;
    Property languages : TStringArray Index 32 Read Flanguages Write Setlanguages;
    Property mobileAppPanelId : String Index 40 Read FmobileAppPanelId Write SetmobileAppPanelId;
    Property populationSource : String Index 48 Read FpopulationSource Write SetpopulationSource;
  end;
  TSurveyAudienceClass = Class of TSurveyAudience;
  
  { --------------------------------------------------------------------
    TSurveyCost
    --------------------------------------------------------------------}
  
  TSurveyCost = Class(TGoogleBaseObject)
  Private
    FcostPerResponseNanos : String;
    FcurrencyCode : String;
    FmaxCostPerResponseNanos : String;
    Fnanos : String;
  Protected
    //Property setters
    Procedure SetcostPerResponseNanos(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmaxCostPerResponseNanos(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnanos(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property costPerResponseNanos : String Index 0 Read FcostPerResponseNanos Write SetcostPerResponseNanos;
    Property currencyCode : String Index 8 Read FcurrencyCode Write SetcurrencyCode;
    Property maxCostPerResponseNanos : String Index 16 Read FmaxCostPerResponseNanos Write SetmaxCostPerResponseNanos;
    Property nanos : String Index 24 Read Fnanos Write Setnanos;
  end;
  TSurveyCostClass = Class of TSurveyCost;
  
  { --------------------------------------------------------------------
    TSurveyQuestion
    --------------------------------------------------------------------}
  
  TSurveyQuestion = Class(TGoogleBaseObject)
  Private
    FanswerOrder : String;
    Fanswers : TStringArray;
    FhasOther : boolean;
    FhighValueLabel : String;
    Fimages : TSurveyQuestionTypeimagesArray;
    FlastAnswerPositionPinned : boolean;
    FlowValueLabel : String;
    FmustPickSuggestion : boolean;
    FnumStars : String;
    FopenTextPlaceholder : String;
    FopenTextSuggestions : TStringArray;
    Fquestion : String;
    FsentimentText : String;
    FsingleLineResponse : boolean;
    FthresholdAnswers : TStringArray;
    F_type : String;
    FunitOfMeasurementLabel : String;
    FvideoId : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetanswerOrder(AIndex : Integer; const AValue : String); virtual;
    Procedure Setanswers(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SethasOther(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SethighValueLabel(AIndex : Integer; const AValue : String); virtual;
    Procedure Setimages(AIndex : Integer; const AValue : TSurveyQuestionTypeimagesArray); virtual;
    Procedure SetlastAnswerPositionPinned(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetlowValueLabel(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmustPickSuggestion(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetnumStars(AIndex : Integer; const AValue : String); virtual;
    Procedure SetopenTextPlaceholder(AIndex : Integer; const AValue : String); virtual;
    Procedure SetopenTextSuggestions(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setquestion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsentimentText(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsingleLineResponse(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetthresholdAnswers(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetunitOfMeasurementLabel(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvideoId(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property answerOrder : String Index 0 Read FanswerOrder Write SetanswerOrder;
    Property answers : TStringArray Index 8 Read Fanswers Write Setanswers;
    Property hasOther : boolean Index 16 Read FhasOther Write SethasOther;
    Property highValueLabel : String Index 24 Read FhighValueLabel Write SethighValueLabel;
    Property images : TSurveyQuestionTypeimagesArray Index 32 Read Fimages Write Setimages;
    Property lastAnswerPositionPinned : boolean Index 40 Read FlastAnswerPositionPinned Write SetlastAnswerPositionPinned;
    Property lowValueLabel : String Index 48 Read FlowValueLabel Write SetlowValueLabel;
    Property mustPickSuggestion : boolean Index 56 Read FmustPickSuggestion Write SetmustPickSuggestion;
    Property numStars : String Index 64 Read FnumStars Write SetnumStars;
    Property openTextPlaceholder : String Index 72 Read FopenTextPlaceholder Write SetopenTextPlaceholder;
    Property openTextSuggestions : TStringArray Index 80 Read FopenTextSuggestions Write SetopenTextSuggestions;
    Property question : String Index 88 Read Fquestion Write Setquestion;
    Property sentimentText : String Index 96 Read FsentimentText Write SetsentimentText;
    Property singleLineResponse : boolean Index 104 Read FsingleLineResponse Write SetsingleLineResponse;
    Property thresholdAnswers : TStringArray Index 112 Read FthresholdAnswers Write SetthresholdAnswers;
    Property _type : String Index 120 Read F_type Write Set_type;
    Property unitOfMeasurementLabel : String Index 128 Read FunitOfMeasurementLabel Write SetunitOfMeasurementLabel;
    Property videoId : String Index 136 Read FvideoId Write SetvideoId;
  end;
  TSurveyQuestionClass = Class of TSurveyQuestion;
  
  { --------------------------------------------------------------------
    TSurveyQuestionImage
    --------------------------------------------------------------------}
  
  TSurveyQuestionImage = Class(TGoogleBaseObject)
  Private
    FaltText : String;
    Fdata : String;
    Furl : String;
  Protected
    //Property setters
    Procedure SetaltText(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdata(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property altText : String Index 0 Read FaltText Write SetaltText;
    Property data : String Index 8 Read Fdata Write Setdata;
    Property url : String Index 16 Read Furl Write Seturl;
  end;
  TSurveyQuestionImageClass = Class of TSurveyQuestionImage;
  
  { --------------------------------------------------------------------
    TSurveyResults
    --------------------------------------------------------------------}
  
  TSurveyResults = Class(TGoogleBaseObject)
  Private
    Fstatus : String;
    FsurveyUrlId : String;
  Protected
    //Property setters
    Procedure Setstatus(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsurveyUrlId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property status : String Index 0 Read Fstatus Write Setstatus;
    Property surveyUrlId : String Index 8 Read FsurveyUrlId Write SetsurveyUrlId;
  end;
  TSurveyResultsClass = Class of TSurveyResults;
  
  { --------------------------------------------------------------------
    TSurveysListResponse
    --------------------------------------------------------------------}
  
  TSurveysListResponse = Class(TGoogleBaseObject)
  Private
    FpageInfo : TPageInfo;
    FrequestId : String;
    Fresources : TSurveysListResponseTyperesourcesArray;
    FtokenPagination : TTokenPagination;
  Protected
    //Property setters
    Procedure SetpageInfo(AIndex : Integer; const AValue : TPageInfo); virtual;
    Procedure SetrequestId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setresources(AIndex : Integer; const AValue : TSurveysListResponseTyperesourcesArray); virtual;
    Procedure SettokenPagination(AIndex : Integer; const AValue : TTokenPagination); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property pageInfo : TPageInfo Index 0 Read FpageInfo Write SetpageInfo;
    Property requestId : String Index 8 Read FrequestId Write SetrequestId;
    Property resources : TSurveysListResponseTyperesourcesArray Index 16 Read Fresources Write Setresources;
    Property tokenPagination : TTokenPagination Index 24 Read FtokenPagination Write SettokenPagination;
  end;
  TSurveysListResponseClass = Class of TSurveysListResponse;
  
  { --------------------------------------------------------------------
    TSurveysStartRequest
    --------------------------------------------------------------------}
  
  TSurveysStartRequest = Class(TGoogleBaseObject)
  Private
    FmaxCostPerResponseNanos : String;
  Protected
    //Property setters
    Procedure SetmaxCostPerResponseNanos(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property maxCostPerResponseNanos : String Index 0 Read FmaxCostPerResponseNanos Write SetmaxCostPerResponseNanos;
  end;
  TSurveysStartRequestClass = Class of TSurveysStartRequest;
  
  { --------------------------------------------------------------------
    TSurveysStartResponse
    --------------------------------------------------------------------}
  
  TSurveysStartResponse = Class(TGoogleBaseObject)
  Private
    FrequestId : String;
    Fresource : TSurvey;
  Protected
    //Property setters
    Procedure SetrequestId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setresource(AIndex : Integer; const AValue : TSurvey); virtual;
  Public
  Published
    Property requestId : String Index 0 Read FrequestId Write SetrequestId;
    Property resource : TSurvey Index 8 Read Fresource Write Setresource;
  end;
  TSurveysStartResponseClass = Class of TSurveysStartResponse;
  
  { --------------------------------------------------------------------
    TSurveysStopResponse
    --------------------------------------------------------------------}
  
  TSurveysStopResponse = Class(TGoogleBaseObject)
  Private
    FrequestId : String;
    Fresource : TSurvey;
  Protected
    //Property setters
    Procedure SetrequestId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setresource(AIndex : Integer; const AValue : TSurvey); virtual;
  Public
  Published
    Property requestId : String Index 0 Read FrequestId Write SetrequestId;
    Property resource : TSurvey Index 8 Read Fresource Write Setresource;
  end;
  TSurveysStopResponseClass = Class of TSurveysStopResponse;
  
  { --------------------------------------------------------------------
    TTokenPagination
    --------------------------------------------------------------------}
  
  TTokenPagination = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    FpreviousPageToken : String;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpreviousPageToken(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property previousPageToken : String Index 8 Read FpreviousPageToken Write SetpreviousPageToken;
  end;
  TTokenPaginationClass = Class of TTokenPagination;
  
  { --------------------------------------------------------------------
    TResultsResource
    --------------------------------------------------------------------}
  
  TResultsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(surveyUrlId: string; aResultsGetRequest : TResultsGetRequest) : TSurveyResults;
  end;
  
  
  { --------------------------------------------------------------------
    TSurveysResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSurveysResource, method List
  
  TSurveysListOptions = Record
    maxResults : integer;
    startIndex : integer;
    token : String;
  end;
  
  TSurveysResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(surveyUrlId: string) : TSurvey;
    Function Insert(aSurvey : TSurvey) : TSurvey;
    Function List(AQuery : string  = '') : TSurveysListResponse;
    Function List(AQuery : TSurveyslistOptions) : TSurveysListResponse;
    Function Start(resourceId: string; aSurveysStartRequest : TSurveysStartRequest) : TSurveysStartResponse;
    Function Stop(resourceId: string) : TSurveysStopResponse;
    Function Update(surveyUrlId: string; aSurvey : TSurvey) : TSurvey;
  end;
  
  
  { --------------------------------------------------------------------
    TConsumersurveysAPI
    --------------------------------------------------------------------}
  
  TConsumersurveysAPI = Class(TGoogleAPI)
  Private
    FResultsInstance : TResultsResource;
    FSurveysInstance : TSurveysResource;
    Function GetResultsInstance : TResultsResource;virtual;
    Function GetSurveysInstance : TSurveysResource;virtual;
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
    Function CreateResultsResource(AOwner : TComponent) : TResultsResource;virtual;overload;
    Function CreateResultsResource : TResultsResource;virtual;overload;
    Function CreateSurveysResource(AOwner : TComponent) : TSurveysResource;virtual;overload;
    Function CreateSurveysResource : TSurveysResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ResultsResource : TResultsResource Read GetResultsInstance;
    Property SurveysResource : TSurveysResource Read GetSurveysInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TFieldMask
  --------------------------------------------------------------------}


Procedure TFieldMask.Setfields(AIndex : Integer; const AValue : TFieldMaskTypefieldsArray); 

begin
  If (Ffields=AValue) then exit;
  Ffields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFieldMask.Setid(AIndex : Integer; const AValue : integer); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFieldMask.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'fields' : SetLength(Ffields,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPageInfo
  --------------------------------------------------------------------}


Procedure TPageInfo.SetresultPerPage(AIndex : Integer; const AValue : integer); 

begin
  If (FresultPerPage=AValue) then exit;
  FresultPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageInfo.SetstartIndex(AIndex : Integer; const AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPageInfo.SettotalResults(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultsGetRequest
  --------------------------------------------------------------------}


Procedure TResultsGetRequest.SetresultMask(AIndex : Integer; const AValue : TResultsMask); 

begin
  If (FresultMask=AValue) then exit;
  FresultMask:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultsMask
  --------------------------------------------------------------------}


Procedure TResultsMask.Setfields(AIndex : Integer; const AValue : TResultsMaskTypefieldsArray); 

begin
  If (Ffields=AValue) then exit;
  Ffields:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultsMask.Setprojection(AIndex : Integer; const AValue : String); 

begin
  If (Fprojection=AValue) then exit;
  Fprojection:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TResultsMask.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'fields' : SetLength(Ffields,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSurvey
  --------------------------------------------------------------------}


Procedure TSurvey.Setaudience(AIndex : Integer; const AValue : TSurveyAudience); 

begin
  If (Faudience=AValue) then exit;
  Faudience:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurvey.Setcost(AIndex : Integer; const AValue : TSurveyCost); 

begin
  If (Fcost=AValue) then exit;
  Fcost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurvey.SetcustomerData(AIndex : Integer; const AValue : String); 

begin
  If (FcustomerData=AValue) then exit;
  FcustomerData:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurvey.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurvey.Setowners(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fowners=AValue) then exit;
  Fowners:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurvey.Setquestions(AIndex : Integer; const AValue : TSurveyTypequestionsArray); 

begin
  If (Fquestions=AValue) then exit;
  Fquestions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurvey.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurvey.SetsurveyUrlId(AIndex : Integer; const AValue : String); 

begin
  If (FsurveyUrlId=AValue) then exit;
  FsurveyUrlId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurvey.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurvey.SetwantedResponseCount(AIndex : Integer; const AValue : integer); 

begin
  If (FwantedResponseCount=AValue) then exit;
  FwantedResponseCount:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSurvey.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'owners' : SetLength(Fowners,ALength);
  'questions' : SetLength(Fquestions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSurveyAudience
  --------------------------------------------------------------------}


Procedure TSurveyAudience.Setages(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fages=AValue) then exit;
  Fages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyAudience.Setcountry(AIndex : Integer; const AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyAudience.SetcountrySubdivision(AIndex : Integer; const AValue : String); 

begin
  If (FcountrySubdivision=AValue) then exit;
  FcountrySubdivision:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyAudience.Setgender(AIndex : Integer; const AValue : String); 

begin
  If (Fgender=AValue) then exit;
  Fgender:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyAudience.Setlanguages(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Flanguages=AValue) then exit;
  Flanguages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyAudience.SetmobileAppPanelId(AIndex : Integer; const AValue : String); 

begin
  If (FmobileAppPanelId=AValue) then exit;
  FmobileAppPanelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyAudience.SetpopulationSource(AIndex : Integer; const AValue : String); 

begin
  If (FpopulationSource=AValue) then exit;
  FpopulationSource:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSurveyAudience.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'ages' : SetLength(Fages,ALength);
  'languages' : SetLength(Flanguages,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSurveyCost
  --------------------------------------------------------------------}


Procedure TSurveyCost.SetcostPerResponseNanos(AIndex : Integer; const AValue : String); 

begin
  If (FcostPerResponseNanos=AValue) then exit;
  FcostPerResponseNanos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyCost.SetcurrencyCode(AIndex : Integer; const AValue : String); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyCost.SetmaxCostPerResponseNanos(AIndex : Integer; const AValue : String); 

begin
  If (FmaxCostPerResponseNanos=AValue) then exit;
  FmaxCostPerResponseNanos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyCost.Setnanos(AIndex : Integer; const AValue : String); 

begin
  If (Fnanos=AValue) then exit;
  Fnanos:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSurveyQuestion
  --------------------------------------------------------------------}


Procedure TSurveyQuestion.SetanswerOrder(AIndex : Integer; const AValue : String); 

begin
  If (FanswerOrder=AValue) then exit;
  FanswerOrder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.Setanswers(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fanswers=AValue) then exit;
  Fanswers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.SethasOther(AIndex : Integer; const AValue : boolean); 

begin
  If (FhasOther=AValue) then exit;
  FhasOther:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.SethighValueLabel(AIndex : Integer; const AValue : String); 

begin
  If (FhighValueLabel=AValue) then exit;
  FhighValueLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.Setimages(AIndex : Integer; const AValue : TSurveyQuestionTypeimagesArray); 

begin
  If (Fimages=AValue) then exit;
  Fimages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.SetlastAnswerPositionPinned(AIndex : Integer; const AValue : boolean); 

begin
  If (FlastAnswerPositionPinned=AValue) then exit;
  FlastAnswerPositionPinned:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.SetlowValueLabel(AIndex : Integer; const AValue : String); 

begin
  If (FlowValueLabel=AValue) then exit;
  FlowValueLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.SetmustPickSuggestion(AIndex : Integer; const AValue : boolean); 

begin
  If (FmustPickSuggestion=AValue) then exit;
  FmustPickSuggestion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.SetnumStars(AIndex : Integer; const AValue : String); 

begin
  If (FnumStars=AValue) then exit;
  FnumStars:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.SetopenTextPlaceholder(AIndex : Integer; const AValue : String); 

begin
  If (FopenTextPlaceholder=AValue) then exit;
  FopenTextPlaceholder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.SetopenTextSuggestions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FopenTextSuggestions=AValue) then exit;
  FopenTextSuggestions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.Setquestion(AIndex : Integer; const AValue : String); 

begin
  If (Fquestion=AValue) then exit;
  Fquestion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.SetsentimentText(AIndex : Integer; const AValue : String); 

begin
  If (FsentimentText=AValue) then exit;
  FsentimentText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.SetsingleLineResponse(AIndex : Integer; const AValue : boolean); 

begin
  If (FsingleLineResponse=AValue) then exit;
  FsingleLineResponse:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.SetthresholdAnswers(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FthresholdAnswers=AValue) then exit;
  FthresholdAnswers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.SetunitOfMeasurementLabel(AIndex : Integer; const AValue : String); 

begin
  If (FunitOfMeasurementLabel=AValue) then exit;
  FunitOfMeasurementLabel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestion.SetvideoId(AIndex : Integer; const AValue : String); 

begin
  If (FvideoId=AValue) then exit;
  FvideoId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSurveyQuestion.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSurveyQuestion.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'answers' : SetLength(Fanswers,ALength);
  'images' : SetLength(Fimages,ALength);
  'opentextsuggestions' : SetLength(FopenTextSuggestions,ALength);
  'thresholdanswers' : SetLength(FthresholdAnswers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSurveyQuestionImage
  --------------------------------------------------------------------}


Procedure TSurveyQuestionImage.SetaltText(AIndex : Integer; const AValue : String); 

begin
  If (FaltText=AValue) then exit;
  FaltText:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestionImage.Setdata(AIndex : Integer; const AValue : String); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyQuestionImage.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSurveyResults
  --------------------------------------------------------------------}


Procedure TSurveyResults.Setstatus(AIndex : Integer; const AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveyResults.SetsurveyUrlId(AIndex : Integer; const AValue : String); 

begin
  If (FsurveyUrlId=AValue) then exit;
  FsurveyUrlId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSurveysListResponse
  --------------------------------------------------------------------}


Procedure TSurveysListResponse.SetpageInfo(AIndex : Integer; const AValue : TPageInfo); 

begin
  If (FpageInfo=AValue) then exit;
  FpageInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveysListResponse.SetrequestId(AIndex : Integer; const AValue : String); 

begin
  If (FrequestId=AValue) then exit;
  FrequestId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveysListResponse.Setresources(AIndex : Integer; const AValue : TSurveysListResponseTyperesourcesArray); 

begin
  If (Fresources=AValue) then exit;
  Fresources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveysListResponse.SettokenPagination(AIndex : Integer; const AValue : TTokenPagination); 

begin
  If (FtokenPagination=AValue) then exit;
  FtokenPagination:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSurveysListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'resources' : SetLength(Fresources,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSurveysStartRequest
  --------------------------------------------------------------------}


Procedure TSurveysStartRequest.SetmaxCostPerResponseNanos(AIndex : Integer; const AValue : String); 

begin
  If (FmaxCostPerResponseNanos=AValue) then exit;
  FmaxCostPerResponseNanos:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSurveysStartResponse
  --------------------------------------------------------------------}


Procedure TSurveysStartResponse.SetrequestId(AIndex : Integer; const AValue : String); 

begin
  If (FrequestId=AValue) then exit;
  FrequestId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveysStartResponse.Setresource(AIndex : Integer; const AValue : TSurvey); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSurveysStopResponse
  --------------------------------------------------------------------}


Procedure TSurveysStopResponse.SetrequestId(AIndex : Integer; const AValue : String); 

begin
  If (FrequestId=AValue) then exit;
  FrequestId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSurveysStopResponse.Setresource(AIndex : Integer; const AValue : TSurvey); 

begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTokenPagination
  --------------------------------------------------------------------}


Procedure TTokenPagination.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTokenPagination.SetpreviousPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FpreviousPageToken=AValue) then exit;
  FpreviousPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultsResource
  --------------------------------------------------------------------}


Class Function TResultsResource.ResourceName : String;

begin
  Result:='results';
end;

Class Function TResultsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TconsumersurveysAPI;
end;

Function TResultsResource.Get(surveyUrlId: string; aResultsGetRequest : TResultsGetRequest) : TSurveyResults;

Const
  _HTTPMethod = 'GET';
  _Path       = 'surveys/{surveyUrlId}/results';
  _Methodid   = 'consumersurveys.results.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['surveyUrlId',surveyUrlId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aResultsGetRequest,TSurveyResults) as TSurveyResults;
end;



{ --------------------------------------------------------------------
  TSurveysResource
  --------------------------------------------------------------------}


Class Function TSurveysResource.ResourceName : String;

begin
  Result:='surveys';
end;

Class Function TSurveysResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TconsumersurveysAPI;
end;

Function TSurveysResource.Get(surveyUrlId: string) : TSurvey;

Const
  _HTTPMethod = 'GET';
  _Path       = 'surveys/{surveyUrlId}';
  _Methodid   = 'consumersurveys.surveys.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['surveyUrlId',surveyUrlId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSurvey) as TSurvey;
end;

Function TSurveysResource.Insert(aSurvey : TSurvey) : TSurvey;

Const
  _HTTPMethod = 'POST';
  _Path       = 'surveys';
  _Methodid   = 'consumersurveys.surveys.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSurvey,TSurvey) as TSurvey;
end;

Function TSurveysResource.List(AQuery : string = '') : TSurveysListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'surveys';
  _Methodid   = 'consumersurveys.surveys.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TSurveysListResponse) as TSurveysListResponse;
end;


Function TSurveysResource.List(AQuery : TSurveyslistOptions) : TSurveysListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  AddToQuery(_Q,'token',AQuery.token);
  Result:=List(_Q);
end;

Function TSurveysResource.Start(resourceId: string; aSurveysStartRequest : TSurveysStartRequest) : TSurveysStartResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'surveys/{resourceId}/start';
  _Methodid   = 'consumersurveys.surveys.start';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resourceId',resourceId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSurveysStartRequest,TSurveysStartResponse) as TSurveysStartResponse;
end;

Function TSurveysResource.Stop(resourceId: string) : TSurveysStopResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'surveys/{resourceId}/stop';
  _Methodid   = 'consumersurveys.surveys.stop';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resourceId',resourceId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSurveysStopResponse) as TSurveysStopResponse;
end;

Function TSurveysResource.Update(surveyUrlId: string; aSurvey : TSurvey) : TSurvey;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'surveys/{surveyUrlId}';
  _Methodid   = 'consumersurveys.surveys.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['surveyUrlId',surveyUrlId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSurvey,TSurvey) as TSurvey;
end;



{ --------------------------------------------------------------------
  TConsumersurveysAPI
  --------------------------------------------------------------------}

Class Function TConsumersurveysAPI.APIName : String;

begin
  Result:='consumersurveys';
end;

Class Function TConsumersurveysAPI.APIVersion : String;

begin
  Result:='v2';
end;

Class Function TConsumersurveysAPI.APIRevision : String;

begin
  Result:='20160519';
end;

Class Function TConsumersurveysAPI.APIID : String;

begin
  Result:='consumersurveys:v2';
end;

Class Function TConsumersurveysAPI.APITitle : String;

begin
  Result:='Consumer Surveys API';
end;

Class Function TConsumersurveysAPI.APIDescription : String;

begin
  Result:='Creates and conducts surveys, lists the surveys that an authenticated user owns, and retrieves survey results and information about specified surveys.';
end;

Class Function TConsumersurveysAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TConsumersurveysAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TConsumersurveysAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TConsumersurveysAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TConsumersurveysAPI.APIdocumentationLink : String;

begin
  Result:='';
end;

Class Function TConsumersurveysAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TConsumersurveysAPI.APIbasePath : string;

begin
  Result:='/consumersurveys/v2/';
end;

Class Function TConsumersurveysAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/consumersurveys/v2/';
end;

Class Function TConsumersurveysAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TConsumersurveysAPI.APIservicePath : string;

begin
  Result:='consumersurveys/v2/';
end;

Class Function TConsumersurveysAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TConsumersurveysAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,3);
  Result[0].Name:='https://www.googleapis.com/auth/consumersurveys';
  Result[0].Description:='View and edit your surveys and results';
  Result[1].Name:='https://www.googleapis.com/auth/consumersurveys.readonly';
  Result[1].Description:='View the results for your surveys';
  Result[2].Name:='https://www.googleapis.com/auth/userinfo.email';
  Result[2].Description:='View your email address';
  
end;

Class Function TConsumersurveysAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TConsumersurveysAPI.RegisterAPIResources;

begin
  TFieldMask.RegisterObject;
  TPageInfo.RegisterObject;
  TResultsGetRequest.RegisterObject;
  TResultsMask.RegisterObject;
  TSurvey.RegisterObject;
  TSurveyAudience.RegisterObject;
  TSurveyCost.RegisterObject;
  TSurveyQuestion.RegisterObject;
  TSurveyQuestionImage.RegisterObject;
  TSurveyResults.RegisterObject;
  TSurveysListResponse.RegisterObject;
  TSurveysStartRequest.RegisterObject;
  TSurveysStartResponse.RegisterObject;
  TSurveysStopResponse.RegisterObject;
  TTokenPagination.RegisterObject;
end;


Function TConsumersurveysAPI.GetResultsInstance : TResultsResource;

begin
  if (FResultsInstance=Nil) then
    FResultsInstance:=CreateResultsResource;
  Result:=FResultsInstance;
end;

Function TConsumersurveysAPI.CreateResultsResource : TResultsResource;

begin
  Result:=CreateResultsResource(Self);
end;


Function TConsumersurveysAPI.CreateResultsResource(AOwner : TComponent) : TResultsResource;

begin
  Result:=TResultsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TConsumersurveysAPI.GetSurveysInstance : TSurveysResource;

begin
  if (FSurveysInstance=Nil) then
    FSurveysInstance:=CreateSurveysResource;
  Result:=FSurveysInstance;
end;

Function TConsumersurveysAPI.CreateSurveysResource : TSurveysResource;

begin
  Result:=CreateSurveysResource(Self);
end;


Function TConsumersurveysAPI.CreateSurveysResource(AOwner : TComponent) : TSurveysResource;

begin
  Result:=TSurveysResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TConsumersurveysAPI.RegisterAPI;
end.
