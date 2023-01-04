unit googlepartners;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TLogUserEventRequest = Class;
  TRequestMetadata = Class;
  TUserOverrides = Class;
  TTrafficSource = Class;
  TEventData = Class;
  TLead = Class;
  TMoney = Class;
  TLogUserEventResponse = Class;
  TResponseMetadata = Class;
  TDebugInfo = Class;
  TLogMessageRequest = Class;
  TLogMessageResponse = Class;
  TListUserStatesResponse = Class;
  TGetCompanyResponse = Class;
  TCompany = Class;
  TLocalizedCompanyInfo = Class;
  TLocation = Class;
  TLatLng = Class;
  TPublicProfile = Class;
  TCertificationStatus = Class;
  TCertificationExamStatus = Class;
  TRank = Class;
  TListCompaniesResponse = Class;
  TCreateLeadRequest = Class;
  TRecaptchaChallenge = Class;
  TCreateLeadResponse = Class;
  TLogUserEventRequestArray = Array of TLogUserEventRequest;
  TRequestMetadataArray = Array of TRequestMetadata;
  TUserOverridesArray = Array of TUserOverrides;
  TTrafficSourceArray = Array of TTrafficSource;
  TEventDataArray = Array of TEventData;
  TLeadArray = Array of TLead;
  TMoneyArray = Array of TMoney;
  TLogUserEventResponseArray = Array of TLogUserEventResponse;
  TResponseMetadataArray = Array of TResponseMetadata;
  TDebugInfoArray = Array of TDebugInfo;
  TLogMessageRequestArray = Array of TLogMessageRequest;
  TLogMessageResponseArray = Array of TLogMessageResponse;
  TListUserStatesResponseArray = Array of TListUserStatesResponse;
  TGetCompanyResponseArray = Array of TGetCompanyResponse;
  TCompanyArray = Array of TCompany;
  TLocalizedCompanyInfoArray = Array of TLocalizedCompanyInfo;
  TLocationArray = Array of TLocation;
  TLatLngArray = Array of TLatLng;
  TPublicProfileArray = Array of TPublicProfile;
  TCertificationStatusArray = Array of TCertificationStatus;
  TCertificationExamStatusArray = Array of TCertificationExamStatus;
  TRankArray = Array of TRank;
  TListCompaniesResponseArray = Array of TListCompaniesResponse;
  TCreateLeadRequestArray = Array of TCreateLeadRequest;
  TRecaptchaChallengeArray = Array of TRecaptchaChallenge;
  TCreateLeadResponseArray = Array of TCreateLeadResponse;
  //Anonymous types, using auto-generated names
  TLogMessageRequestTypeclientInfo = Class;
  TLogUserEventRequestTypeeventDatasArray = Array of TEventData;
  TCompanyTypelocalizedInfosArray = Array of TLocalizedCompanyInfo;
  TCompanyTypelocationsArray = Array of TLocation;
  TCompanyTypecertificationStatusesArray = Array of TCertificationStatus;
  TCompanyTyperanksArray = Array of TRank;
  TCertificationStatusTypeexamStatusesArray = Array of TCertificationExamStatus;
  TListCompaniesResponseTypecompaniesArray = Array of TCompany;
  
  { --------------------------------------------------------------------
    TLogUserEventRequest
    --------------------------------------------------------------------}
  
  TLogUserEventRequest = Class(TGoogleBaseObject)
  Private
    FrequestMetadata : TRequestMetadata;
    FeventAction : String;
    FeventCategory : String;
    FeventScope : String;
    FeventDatas : TLogUserEventRequestTypeeventDatasArray;
    Furl : String;
    Flead : TLead;
  Protected
    //Property setters
    Procedure SetrequestMetadata(AIndex : Integer; const AValue : TRequestMetadata); virtual;
    Procedure SeteventAction(AIndex : Integer; const AValue : String); virtual;
    Procedure SeteventCategory(AIndex : Integer; const AValue : String); virtual;
    Procedure SeteventScope(AIndex : Integer; const AValue : String); virtual;
    Procedure SeteventDatas(AIndex : Integer; const AValue : TLogUserEventRequestTypeeventDatasArray); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlead(AIndex : Integer; const AValue : TLead); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property requestMetadata : TRequestMetadata Index 0 Read FrequestMetadata Write SetrequestMetadata;
    Property eventAction : String Index 8 Read FeventAction Write SeteventAction;
    Property eventCategory : String Index 16 Read FeventCategory Write SeteventCategory;
    Property eventScope : String Index 24 Read FeventScope Write SeteventScope;
    Property eventDatas : TLogUserEventRequestTypeeventDatasArray Index 32 Read FeventDatas Write SeteventDatas;
    Property url : String Index 40 Read Furl Write Seturl;
    Property lead : TLead Index 48 Read Flead Write Setlead;
  end;
  TLogUserEventRequestClass = Class of TLogUserEventRequest;
  
  { --------------------------------------------------------------------
    TRequestMetadata
    --------------------------------------------------------------------}
  
  TRequestMetadata = Class(TGoogleBaseObject)
  Private
    FuserOverrides : TUserOverrides;
    Flocale : String;
    FpartnersSessionId : String;
    FexperimentIds : TStringArray;
    FtrafficSource : TTrafficSource;
  Protected
    //Property setters
    Procedure SetuserOverrides(AIndex : Integer; const AValue : TUserOverrides); virtual;
    Procedure Setlocale(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpartnersSessionId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetexperimentIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SettrafficSource(AIndex : Integer; const AValue : TTrafficSource); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property userOverrides : TUserOverrides Index 0 Read FuserOverrides Write SetuserOverrides;
    Property locale : String Index 8 Read Flocale Write Setlocale;
    Property partnersSessionId : String Index 16 Read FpartnersSessionId Write SetpartnersSessionId;
    Property experimentIds : TStringArray Index 24 Read FexperimentIds Write SetexperimentIds;
    Property trafficSource : TTrafficSource Index 32 Read FtrafficSource Write SettrafficSource;
  end;
  TRequestMetadataClass = Class of TRequestMetadata;
  
  { --------------------------------------------------------------------
    TUserOverrides
    --------------------------------------------------------------------}
  
  TUserOverrides = Class(TGoogleBaseObject)
  Private
    FipAddress : String;
    FuserId : String;
  Protected
    //Property setters
    Procedure SetipAddress(AIndex : Integer; const AValue : String); virtual;
    Procedure SetuserId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property ipAddress : String Index 0 Read FipAddress Write SetipAddress;
    Property userId : String Index 8 Read FuserId Write SetuserId;
  end;
  TUserOverridesClass = Class of TUserOverrides;
  
  { --------------------------------------------------------------------
    TTrafficSource
    --------------------------------------------------------------------}
  
  TTrafficSource = Class(TGoogleBaseObject)
  Private
    FtrafficSourceId : String;
    FtrafficSubId : String;
  Protected
    //Property setters
    Procedure SettrafficSourceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettrafficSubId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property trafficSourceId : String Index 0 Read FtrafficSourceId Write SettrafficSourceId;
    Property trafficSubId : String Index 8 Read FtrafficSubId Write SettrafficSubId;
  end;
  TTrafficSourceClass = Class of TTrafficSource;
  
  { --------------------------------------------------------------------
    TEventData
    --------------------------------------------------------------------}
  
  TEventData = Class(TGoogleBaseObject)
  Private
    Fkey : String;
    Fvalues : TStringArray;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalues(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property key : String Index 0 Read Fkey Write Setkey;
    Property values : TStringArray Index 8 Read Fvalues Write Setvalues;
  end;
  TEventDataClass = Class of TEventData;
  
  { --------------------------------------------------------------------
    TLead
    --------------------------------------------------------------------}
  
  TLead = Class(TGoogleBaseObject)
  Private
    Fid : String;
    F_type : String;
    Femail : String;
    FgivenName : String;
    FfamilyName : String;
    FwebsiteUrl : String;
    FphoneNumber : String;
    Fcomments : String;
    FgpsMotivations : TStringArray;
    FminMonthlyBudget : TMoney;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setemail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgivenName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfamilyName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetwebsiteUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetphoneNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcomments(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgpsMotivations(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetminMonthlyBudget(AIndex : Integer; const AValue : TMoney); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property _type : String Index 8 Read F_type Write Set_type;
    Property email : String Index 16 Read Femail Write Setemail;
    Property givenName : String Index 24 Read FgivenName Write SetgivenName;
    Property familyName : String Index 32 Read FfamilyName Write SetfamilyName;
    Property websiteUrl : String Index 40 Read FwebsiteUrl Write SetwebsiteUrl;
    Property phoneNumber : String Index 48 Read FphoneNumber Write SetphoneNumber;
    Property comments : String Index 56 Read Fcomments Write Setcomments;
    Property gpsMotivations : TStringArray Index 64 Read FgpsMotivations Write SetgpsMotivations;
    Property minMonthlyBudget : TMoney Index 72 Read FminMonthlyBudget Write SetminMonthlyBudget;
  end;
  TLeadClass = Class of TLead;
  
  { --------------------------------------------------------------------
    TMoney
    --------------------------------------------------------------------}
  
  TMoney = Class(TGoogleBaseObject)
  Private
    FcurrencyCode : String;
    Funits : String;
    Fnanos : integer;
  Protected
    //Property setters
    Procedure SetcurrencyCode(AIndex : Integer; const AValue : String); virtual;
    Procedure Setunits(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnanos(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property currencyCode : String Index 0 Read FcurrencyCode Write SetcurrencyCode;
    Property units : String Index 8 Read Funits Write Setunits;
    Property nanos : integer Index 16 Read Fnanos Write Setnanos;
  end;
  TMoneyClass = Class of TMoney;
  
  { --------------------------------------------------------------------
    TLogUserEventResponse
    --------------------------------------------------------------------}
  
  TLogUserEventResponse = Class(TGoogleBaseObject)
  Private
    FresponseMetadata : TResponseMetadata;
  Protected
    //Property setters
    Procedure SetresponseMetadata(AIndex : Integer; const AValue : TResponseMetadata); virtual;
  Public
  Published
    Property responseMetadata : TResponseMetadata Index 0 Read FresponseMetadata Write SetresponseMetadata;
  end;
  TLogUserEventResponseClass = Class of TLogUserEventResponse;
  
  { --------------------------------------------------------------------
    TResponseMetadata
    --------------------------------------------------------------------}
  
  TResponseMetadata = Class(TGoogleBaseObject)
  Private
    FdebugInfo : TDebugInfo;
  Protected
    //Property setters
    Procedure SetdebugInfo(AIndex : Integer; const AValue : TDebugInfo); virtual;
  Public
  Published
    Property debugInfo : TDebugInfo Index 0 Read FdebugInfo Write SetdebugInfo;
  end;
  TResponseMetadataClass = Class of TResponseMetadata;
  
  { --------------------------------------------------------------------
    TDebugInfo
    --------------------------------------------------------------------}
  
  TDebugInfo = Class(TGoogleBaseObject)
  Private
    FserverInfo : String;
    FserviceUrl : String;
    FserverTraceInfo : String;
  Protected
    //Property setters
    Procedure SetserverInfo(AIndex : Integer; const AValue : String); virtual;
    Procedure SetserviceUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetserverTraceInfo(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property serverInfo : String Index 0 Read FserverInfo Write SetserverInfo;
    Property serviceUrl : String Index 8 Read FserviceUrl Write SetserviceUrl;
    Property serverTraceInfo : String Index 16 Read FserverTraceInfo Write SetserverTraceInfo;
  end;
  TDebugInfoClass = Class of TDebugInfo;
  
  { --------------------------------------------------------------------
    TLogMessageRequestTypeclientInfo
    --------------------------------------------------------------------}
  
  TLogMessageRequestTypeclientInfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TLogMessageRequestTypeclientInfoClass = Class of TLogMessageRequestTypeclientInfo;
  
  { --------------------------------------------------------------------
    TLogMessageRequest
    --------------------------------------------------------------------}
  
  TLogMessageRequest = Class(TGoogleBaseObject)
  Private
    FrequestMetadata : TRequestMetadata;
    Flevel : String;
    Fdetails : String;
    FclientInfo : TLogMessageRequestTypeclientInfo;
  Protected
    //Property setters
    Procedure SetrequestMetadata(AIndex : Integer; const AValue : TRequestMetadata); virtual;
    Procedure Setlevel(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdetails(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclientInfo(AIndex : Integer; const AValue : TLogMessageRequestTypeclientInfo); virtual;
  Public
  Published
    Property requestMetadata : TRequestMetadata Index 0 Read FrequestMetadata Write SetrequestMetadata;
    Property level : String Index 8 Read Flevel Write Setlevel;
    Property details : String Index 16 Read Fdetails Write Setdetails;
    Property clientInfo : TLogMessageRequestTypeclientInfo Index 24 Read FclientInfo Write SetclientInfo;
  end;
  TLogMessageRequestClass = Class of TLogMessageRequest;
  
  { --------------------------------------------------------------------
    TLogMessageResponse
    --------------------------------------------------------------------}
  
  TLogMessageResponse = Class(TGoogleBaseObject)
  Private
    FresponseMetadata : TResponseMetadata;
  Protected
    //Property setters
    Procedure SetresponseMetadata(AIndex : Integer; const AValue : TResponseMetadata); virtual;
  Public
  Published
    Property responseMetadata : TResponseMetadata Index 0 Read FresponseMetadata Write SetresponseMetadata;
  end;
  TLogMessageResponseClass = Class of TLogMessageResponse;
  
  { --------------------------------------------------------------------
    TListUserStatesResponse
    --------------------------------------------------------------------}
  
  TListUserStatesResponse = Class(TGoogleBaseObject)
  Private
    FresponseMetadata : TResponseMetadata;
    FuserStates : TStringArray;
  Protected
    //Property setters
    Procedure SetresponseMetadata(AIndex : Integer; const AValue : TResponseMetadata); virtual;
    Procedure SetuserStates(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property responseMetadata : TResponseMetadata Index 0 Read FresponseMetadata Write SetresponseMetadata;
    Property userStates : TStringArray Index 8 Read FuserStates Write SetuserStates;
  end;
  TListUserStatesResponseClass = Class of TListUserStatesResponse;
  
  { --------------------------------------------------------------------
    TGetCompanyResponse
    --------------------------------------------------------------------}
  
  TGetCompanyResponse = Class(TGoogleBaseObject)
  Private
    FresponseMetadata : TResponseMetadata;
    Fcompany : TCompany;
  Protected
    //Property setters
    Procedure SetresponseMetadata(AIndex : Integer; const AValue : TResponseMetadata); virtual;
    Procedure Setcompany(AIndex : Integer; const AValue : TCompany); virtual;
  Public
  Published
    Property responseMetadata : TResponseMetadata Index 0 Read FresponseMetadata Write SetresponseMetadata;
    Property company : TCompany Index 8 Read Fcompany Write Setcompany;
  end;
  TGetCompanyResponseClass = Class of TGetCompanyResponse;
  
  { --------------------------------------------------------------------
    TCompany
    --------------------------------------------------------------------}
  
  TCompany = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fname : String;
    FlocalizedInfos : TCompanyTypelocalizedInfosArray;
    Flocations : TCompanyTypelocationsArray;
    FconvertedMinMonthlyBudget : TMoney;
    ForiginalMinMonthlyBudget : TMoney;
    FpublicProfile : TPublicProfile;
    FcertificationStatuses : TCompanyTypecertificationStatusesArray;
    Franks : TCompanyTyperanksArray;
    FwebsiteUrl : String;
    Findustries : TStringArray;
    Fservices : TStringArray;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlocalizedInfos(AIndex : Integer; const AValue : TCompanyTypelocalizedInfosArray); virtual;
    Procedure Setlocations(AIndex : Integer; const AValue : TCompanyTypelocationsArray); virtual;
    Procedure SetconvertedMinMonthlyBudget(AIndex : Integer; const AValue : TMoney); virtual;
    Procedure SetoriginalMinMonthlyBudget(AIndex : Integer; const AValue : TMoney); virtual;
    Procedure SetpublicProfile(AIndex : Integer; const AValue : TPublicProfile); virtual;
    Procedure SetcertificationStatuses(AIndex : Integer; const AValue : TCompanyTypecertificationStatusesArray); virtual;
    Procedure Setranks(AIndex : Integer; const AValue : TCompanyTyperanksArray); virtual;
    Procedure SetwebsiteUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setindustries(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setservices(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property name : String Index 8 Read Fname Write Setname;
    Property localizedInfos : TCompanyTypelocalizedInfosArray Index 16 Read FlocalizedInfos Write SetlocalizedInfos;
    Property locations : TCompanyTypelocationsArray Index 24 Read Flocations Write Setlocations;
    Property convertedMinMonthlyBudget : TMoney Index 32 Read FconvertedMinMonthlyBudget Write SetconvertedMinMonthlyBudget;
    Property originalMinMonthlyBudget : TMoney Index 40 Read ForiginalMinMonthlyBudget Write SetoriginalMinMonthlyBudget;
    Property publicProfile : TPublicProfile Index 48 Read FpublicProfile Write SetpublicProfile;
    Property certificationStatuses : TCompanyTypecertificationStatusesArray Index 56 Read FcertificationStatuses Write SetcertificationStatuses;
    Property ranks : TCompanyTyperanksArray Index 64 Read Franks Write Setranks;
    Property websiteUrl : String Index 72 Read FwebsiteUrl Write SetwebsiteUrl;
    Property industries : TStringArray Index 80 Read Findustries Write Setindustries;
    Property services : TStringArray Index 88 Read Fservices Write Setservices;
  end;
  TCompanyClass = Class of TCompany;
  
  { --------------------------------------------------------------------
    TLocalizedCompanyInfo
    --------------------------------------------------------------------}
  
  TLocalizedCompanyInfo = Class(TGoogleBaseObject)
  Private
    FlanguageCode : String;
    FdisplayName : String;
    Foverview : String;
    FcountryCodes : TStringArray;
  Protected
    //Property setters
    Procedure SetlanguageCode(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setoverview(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcountryCodes(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property languageCode : String Index 0 Read FlanguageCode Write SetlanguageCode;
    Property displayName : String Index 8 Read FdisplayName Write SetdisplayName;
    Property overview : String Index 16 Read Foverview Write Setoverview;
    Property countryCodes : TStringArray Index 24 Read FcountryCodes Write SetcountryCodes;
  end;
  TLocalizedCompanyInfoClass = Class of TLocalizedCompanyInfo;
  
  { --------------------------------------------------------------------
    TLocation
    --------------------------------------------------------------------}
  
  TLocation = Class(TGoogleBaseObject)
  Private
    Faddress : String;
    FlatLng : TLatLng;
  Protected
    //Property setters
    Procedure Setaddress(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlatLng(AIndex : Integer; const AValue : TLatLng); virtual;
  Public
  Published
    Property address : String Index 0 Read Faddress Write Setaddress;
    Property latLng : TLatLng Index 8 Read FlatLng Write SetlatLng;
  end;
  TLocationClass = Class of TLocation;
  
  { --------------------------------------------------------------------
    TLatLng
    --------------------------------------------------------------------}
  
  TLatLng = Class(TGoogleBaseObject)
  Private
    Flatitude : double;
    Flongitude : double;
  Protected
    //Property setters
    Procedure Setlatitude(AIndex : Integer; const AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property latitude : double Index 0 Read Flatitude Write Setlatitude;
    Property longitude : double Index 8 Read Flongitude Write Setlongitude;
  end;
  TLatLngClass = Class of TLatLng;
  
  { --------------------------------------------------------------------
    TPublicProfile
    --------------------------------------------------------------------}
  
  TPublicProfile = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FdisplayName : String;
    Furl : String;
    FdisplayImageUrl : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdisplayImageUrl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property displayName : String Index 8 Read FdisplayName Write SetdisplayName;
    Property url : String Index 16 Read Furl Write Seturl;
    Property displayImageUrl : String Index 24 Read FdisplayImageUrl Write SetdisplayImageUrl;
  end;
  TPublicProfileClass = Class of TPublicProfile;
  
  { --------------------------------------------------------------------
    TCertificationStatus
    --------------------------------------------------------------------}
  
  TCertificationStatus = Class(TGoogleBaseObject)
  Private
    F_type : String;
    FexamStatuses : TCertificationStatusTypeexamStatusesArray;
    FisCertified : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetexamStatuses(AIndex : Integer; const AValue : TCertificationStatusTypeexamStatusesArray); virtual;
    Procedure SetisCertified(AIndex : Integer; const AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property examStatuses : TCertificationStatusTypeexamStatusesArray Index 8 Read FexamStatuses Write SetexamStatuses;
    Property isCertified : boolean Index 16 Read FisCertified Write SetisCertified;
  end;
  TCertificationStatusClass = Class of TCertificationStatus;
  
  { --------------------------------------------------------------------
    TCertificationExamStatus
    --------------------------------------------------------------------}
  
  TCertificationExamStatus = Class(TGoogleBaseObject)
  Private
    F_type : String;
    FnumberUsersPass : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnumberUsersPass(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property numberUsersPass : integer Index 8 Read FnumberUsersPass Write SetnumberUsersPass;
  end;
  TCertificationExamStatusClass = Class of TCertificationExamStatus;
  
  { --------------------------------------------------------------------
    TRank
    --------------------------------------------------------------------}
  
  TRank = Class(TGoogleBaseObject)
  Private
    F_type : String;
    Fvalue : double;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property value : double Index 8 Read Fvalue Write Setvalue;
  end;
  TRankClass = Class of TRank;
  
  { --------------------------------------------------------------------
    TListCompaniesResponse
    --------------------------------------------------------------------}
  
  TListCompaniesResponse = Class(TGoogleBaseObject)
  Private
    FresponseMetadata : TResponseMetadata;
    Fcompanies : TListCompaniesResponseTypecompaniesArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetresponseMetadata(AIndex : Integer; const AValue : TResponseMetadata); virtual;
    Procedure Setcompanies(AIndex : Integer; const AValue : TListCompaniesResponseTypecompaniesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property responseMetadata : TResponseMetadata Index 0 Read FresponseMetadata Write SetresponseMetadata;
    Property companies : TListCompaniesResponseTypecompaniesArray Index 8 Read Fcompanies Write Setcompanies;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TListCompaniesResponseClass = Class of TListCompaniesResponse;
  
  { --------------------------------------------------------------------
    TCreateLeadRequest
    --------------------------------------------------------------------}
  
  TCreateLeadRequest = Class(TGoogleBaseObject)
  Private
    FrequestMetadata : TRequestMetadata;
    Flead : TLead;
    FrecaptchaChallenge : TRecaptchaChallenge;
  Protected
    //Property setters
    Procedure SetrequestMetadata(AIndex : Integer; const AValue : TRequestMetadata); virtual;
    Procedure Setlead(AIndex : Integer; const AValue : TLead); virtual;
    Procedure SetrecaptchaChallenge(AIndex : Integer; const AValue : TRecaptchaChallenge); virtual;
  Public
  Published
    Property requestMetadata : TRequestMetadata Index 0 Read FrequestMetadata Write SetrequestMetadata;
    Property lead : TLead Index 8 Read Flead Write Setlead;
    Property recaptchaChallenge : TRecaptchaChallenge Index 16 Read FrecaptchaChallenge Write SetrecaptchaChallenge;
  end;
  TCreateLeadRequestClass = Class of TCreateLeadRequest;
  
  { --------------------------------------------------------------------
    TRecaptchaChallenge
    --------------------------------------------------------------------}
  
  TRecaptchaChallenge = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fresponse : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setresponse(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property response : String Index 8 Read Fresponse Write Setresponse;
  end;
  TRecaptchaChallengeClass = Class of TRecaptchaChallenge;
  
  { --------------------------------------------------------------------
    TCreateLeadResponse
    --------------------------------------------------------------------}
  
  TCreateLeadResponse = Class(TGoogleBaseObject)
  Private
    FresponseMetadata : TResponseMetadata;
    Flead : TLead;
    FrecaptchaStatus : String;
  Protected
    //Property setters
    Procedure SetresponseMetadata(AIndex : Integer; const AValue : TResponseMetadata); virtual;
    Procedure Setlead(AIndex : Integer; const AValue : TLead); virtual;
    Procedure SetrecaptchaStatus(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property responseMetadata : TResponseMetadata Index 0 Read FresponseMetadata Write SetresponseMetadata;
    Property lead : TLead Index 8 Read Flead Write Setlead;
    Property recaptchaStatus : String Index 16 Read FrecaptchaStatus Write SetrecaptchaStatus;
  end;
  TCreateLeadResponseClass = Class of TCreateLeadResponse;
  
  { --------------------------------------------------------------------
    TUserEventsResource
    --------------------------------------------------------------------}
  
  TUserEventsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Log(aLogUserEventRequest : TLogUserEventRequest) : TLogUserEventResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TClientMessagesResource
    --------------------------------------------------------------------}
  
  TClientMessagesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Log(aLogMessageRequest : TLogMessageRequest) : TLogMessageResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TUserStatesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUserStatesResource, method List
  
  TUserStatesListOptions = Record
    requestMetadatauserOverridesipAddress : String;
    requestMetadatauserOverridesuserId : String;
    requestMetadatalocale : String;
    requestMetadatapartnersSessionId : String;
    requestMetadataexperimentIds : String;
    requestMetadatatrafficSourcetrafficSourceId : String;
    requestMetadatatrafficSourcetrafficSubId : String;
  end;
  
  TUserStatesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TListUserStatesResponse;
    Function List(AQuery : TUserStateslistOptions) : TListUserStatesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TCompaniesLeadsResource
    --------------------------------------------------------------------}
  
  TCompaniesLeadsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(companyId: string; aCreateLeadRequest : TCreateLeadRequest) : TCreateLeadResponse;overload;
  end;
  
  
  { --------------------------------------------------------------------
    TCompaniesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCompaniesResource, method Get
  
  TCompaniesGetOptions = Record
    requestMetadatauserOverridesipAddress : String;
    requestMetadatauserOverridesuserId : String;
    requestMetadatalocale : String;
    requestMetadatapartnersSessionId : String;
    requestMetadataexperimentIds : String;
    requestMetadatatrafficSourcetrafficSourceId : String;
    requestMetadatatrafficSourcetrafficSubId : String;
    view : String;
    orderBy : String;
    currencyCode : String;
    address : String;
  end;
  
  
  //Optional query Options for TCompaniesResource, method List
  
  TCompaniesListOptions = Record
    requestMetadatauserOverridesipAddress : String;
    requestMetadatauserOverridesuserId : String;
    requestMetadatalocale : String;
    requestMetadatapartnersSessionId : String;
    requestMetadataexperimentIds : String;
    requestMetadatatrafficSourcetrafficSourceId : String;
    requestMetadatatrafficSourcetrafficSubId : String;
    pageSize : integer;
    pageToken : String;
    companyName : String;
    view : String;
    minMonthlyBudgetcurrencyCode : String;
    minMonthlyBudgetunits : int64;
    minMonthlyBudgetnanos : integer;
    maxMonthlyBudgetcurrencyCode : String;
    maxMonthlyBudgetunits : int64;
    maxMonthlyBudgetnanos : integer;
    industries : String;
    services : String;
    languageCodes : String;
    address : String;
    orderBy : String;
    gpsMotivations : String;
    websiteUrl : String;
  end;
  
  TCompaniesResource = Class(TGoogleResource)
  Private
    FLeadsInstance : TCompaniesLeadsResource;
    Function GetLeadsInstance : TCompaniesLeadsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(companyId: string; AQuery : string  = '') : TGetCompanyResponse;
    Function Get(companyId: string; AQuery : TCompaniesgetOptions) : TGetCompanyResponse;
    Function List(AQuery : string  = '') : TListCompaniesResponse;
    Function List(AQuery : TCompanieslistOptions) : TListCompaniesResponse;
    Function CreateLeadsResource(AOwner : TComponent) : TCompaniesLeadsResource;virtual;overload;
    Function CreateLeadsResource : TCompaniesLeadsResource;virtual;overload;
    Property LeadsResource : TCompaniesLeadsResource Read GetLeadsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TPartnersAPI
    --------------------------------------------------------------------}
  
  TPartnersAPI = Class(TGoogleAPI)
  Private
    FUserEventsInstance : TUserEventsResource;
    FClientMessagesInstance : TClientMessagesResource;
    FUserStatesInstance : TUserStatesResource;
    FCompaniesLeadsInstance : TCompaniesLeadsResource;
    FCompaniesInstance : TCompaniesResource;
    Function GetUserEventsInstance : TUserEventsResource;virtual;
    Function GetClientMessagesInstance : TClientMessagesResource;virtual;
    Function GetUserStatesInstance : TUserStatesResource;virtual;
    Function GetCompaniesLeadsInstance : TCompaniesLeadsResource;virtual;
    Function GetCompaniesInstance : TCompaniesResource;virtual;
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
    Function CreateUserEventsResource(AOwner : TComponent) : TUserEventsResource;virtual;overload;
    Function CreateUserEventsResource : TUserEventsResource;virtual;overload;
    Function CreateClientMessagesResource(AOwner : TComponent) : TClientMessagesResource;virtual;overload;
    Function CreateClientMessagesResource : TClientMessagesResource;virtual;overload;
    Function CreateUserStatesResource(AOwner : TComponent) : TUserStatesResource;virtual;overload;
    Function CreateUserStatesResource : TUserStatesResource;virtual;overload;
    Function CreateCompaniesLeadsResource(AOwner : TComponent) : TCompaniesLeadsResource;virtual;overload;
    Function CreateCompaniesLeadsResource : TCompaniesLeadsResource;virtual;overload;
    Function CreateCompaniesResource(AOwner : TComponent) : TCompaniesResource;virtual;overload;
    Function CreateCompaniesResource : TCompaniesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property UserEventsResource : TUserEventsResource Read GetUserEventsInstance;
    Property ClientMessagesResource : TClientMessagesResource Read GetClientMessagesInstance;
    Property UserStatesResource : TUserStatesResource Read GetUserStatesInstance;
    Property CompaniesLeadsResource : TCompaniesLeadsResource Read GetCompaniesLeadsInstance;
    Property CompaniesResource : TCompaniesResource Read GetCompaniesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TLogUserEventRequest
  --------------------------------------------------------------------}


Procedure TLogUserEventRequest.SetrequestMetadata(AIndex : Integer; const AValue : TRequestMetadata); 

begin
  If (FrequestMetadata=AValue) then exit;
  FrequestMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogUserEventRequest.SeteventAction(AIndex : Integer; const AValue : String); 

begin
  If (FeventAction=AValue) then exit;
  FeventAction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogUserEventRequest.SeteventCategory(AIndex : Integer; const AValue : String); 

begin
  If (FeventCategory=AValue) then exit;
  FeventCategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogUserEventRequest.SeteventScope(AIndex : Integer; const AValue : String); 

begin
  If (FeventScope=AValue) then exit;
  FeventScope:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogUserEventRequest.SeteventDatas(AIndex : Integer; const AValue : TLogUserEventRequestTypeeventDatasArray); 

begin
  If (FeventDatas=AValue) then exit;
  FeventDatas:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogUserEventRequest.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogUserEventRequest.Setlead(AIndex : Integer; const AValue : TLead); 

begin
  If (Flead=AValue) then exit;
  Flead:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TLogUserEventRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'eventdatas' : SetLength(FeventDatas,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TRequestMetadata
  --------------------------------------------------------------------}


Procedure TRequestMetadata.SetuserOverrides(AIndex : Integer; const AValue : TUserOverrides); 

begin
  If (FuserOverrides=AValue) then exit;
  FuserOverrides:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestMetadata.Setlocale(AIndex : Integer; const AValue : String); 

begin
  If (Flocale=AValue) then exit;
  Flocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestMetadata.SetpartnersSessionId(AIndex : Integer; const AValue : String); 

begin
  If (FpartnersSessionId=AValue) then exit;
  FpartnersSessionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestMetadata.SetexperimentIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FexperimentIds=AValue) then exit;
  FexperimentIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRequestMetadata.SettrafficSource(AIndex : Integer; const AValue : TTrafficSource); 

begin
  If (FtrafficSource=AValue) then exit;
  FtrafficSource:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TRequestMetadata.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'experimentids' : SetLength(FexperimentIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUserOverrides
  --------------------------------------------------------------------}


Procedure TUserOverrides.SetipAddress(AIndex : Integer; const AValue : String); 

begin
  If (FipAddress=AValue) then exit;
  FipAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUserOverrides.SetuserId(AIndex : Integer; const AValue : String); 

begin
  If (FuserId=AValue) then exit;
  FuserId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTrafficSource
  --------------------------------------------------------------------}


Procedure TTrafficSource.SettrafficSourceId(AIndex : Integer; const AValue : String); 

begin
  If (FtrafficSourceId=AValue) then exit;
  FtrafficSourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTrafficSource.SettrafficSubId(AIndex : Integer; const AValue : String); 

begin
  If (FtrafficSubId=AValue) then exit;
  FtrafficSubId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventData
  --------------------------------------------------------------------}


Procedure TEventData.Setkey(AIndex : Integer; const AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventData.Setvalues(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TEventData.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'values' : SetLength(Fvalues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLead
  --------------------------------------------------------------------}


Procedure TLead.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLead.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLead.Setemail(AIndex : Integer; const AValue : String); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLead.SetgivenName(AIndex : Integer; const AValue : String); 

begin
  If (FgivenName=AValue) then exit;
  FgivenName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLead.SetfamilyName(AIndex : Integer; const AValue : String); 

begin
  If (FfamilyName=AValue) then exit;
  FfamilyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLead.SetwebsiteUrl(AIndex : Integer; const AValue : String); 

begin
  If (FwebsiteUrl=AValue) then exit;
  FwebsiteUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLead.SetphoneNumber(AIndex : Integer; const AValue : String); 

begin
  If (FphoneNumber=AValue) then exit;
  FphoneNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLead.Setcomments(AIndex : Integer; const AValue : String); 

begin
  If (Fcomments=AValue) then exit;
  Fcomments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLead.SetgpsMotivations(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FgpsMotivations=AValue) then exit;
  FgpsMotivations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLead.SetminMonthlyBudget(AIndex : Integer; const AValue : TMoney); 

begin
  If (FminMonthlyBudget=AValue) then exit;
  FminMonthlyBudget:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TLead.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TLead.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'gpsmotivations' : SetLength(FgpsMotivations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMoney
  --------------------------------------------------------------------}


Procedure TMoney.SetcurrencyCode(AIndex : Integer; const AValue : String); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMoney.Setunits(AIndex : Integer; const AValue : String); 

begin
  If (Funits=AValue) then exit;
  Funits:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMoney.Setnanos(AIndex : Integer; const AValue : integer); 

begin
  If (Fnanos=AValue) then exit;
  Fnanos:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLogUserEventResponse
  --------------------------------------------------------------------}


Procedure TLogUserEventResponse.SetresponseMetadata(AIndex : Integer; const AValue : TResponseMetadata); 

begin
  If (FresponseMetadata=AValue) then exit;
  FresponseMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResponseMetadata
  --------------------------------------------------------------------}


Procedure TResponseMetadata.SetdebugInfo(AIndex : Integer; const AValue : TDebugInfo); 

begin
  If (FdebugInfo=AValue) then exit;
  FdebugInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDebugInfo
  --------------------------------------------------------------------}


Procedure TDebugInfo.SetserverInfo(AIndex : Integer; const AValue : String); 

begin
  If (FserverInfo=AValue) then exit;
  FserverInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDebugInfo.SetserviceUrl(AIndex : Integer; const AValue : String); 

begin
  If (FserviceUrl=AValue) then exit;
  FserviceUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDebugInfo.SetserverTraceInfo(AIndex : Integer; const AValue : String); 

begin
  If (FserverTraceInfo=AValue) then exit;
  FserverTraceInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLogMessageRequestTypeclientInfo
  --------------------------------------------------------------------}


Class Function TLogMessageRequestTypeclientInfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TLogMessageRequest
  --------------------------------------------------------------------}


Procedure TLogMessageRequest.SetrequestMetadata(AIndex : Integer; const AValue : TRequestMetadata); 

begin
  If (FrequestMetadata=AValue) then exit;
  FrequestMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogMessageRequest.Setlevel(AIndex : Integer; const AValue : String); 

begin
  If (Flevel=AValue) then exit;
  Flevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogMessageRequest.Setdetails(AIndex : Integer; const AValue : String); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLogMessageRequest.SetclientInfo(AIndex : Integer; const AValue : TLogMessageRequestTypeclientInfo); 

begin
  If (FclientInfo=AValue) then exit;
  FclientInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLogMessageResponse
  --------------------------------------------------------------------}


Procedure TLogMessageResponse.SetresponseMetadata(AIndex : Integer; const AValue : TResponseMetadata); 

begin
  If (FresponseMetadata=AValue) then exit;
  FresponseMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListUserStatesResponse
  --------------------------------------------------------------------}


Procedure TListUserStatesResponse.SetresponseMetadata(AIndex : Integer; const AValue : TResponseMetadata); 

begin
  If (FresponseMetadata=AValue) then exit;
  FresponseMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListUserStatesResponse.SetuserStates(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FuserStates=AValue) then exit;
  FuserStates:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListUserStatesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'userstates' : SetLength(FuserStates,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGetCompanyResponse
  --------------------------------------------------------------------}


Procedure TGetCompanyResponse.SetresponseMetadata(AIndex : Integer; const AValue : TResponseMetadata); 

begin
  If (FresponseMetadata=AValue) then exit;
  FresponseMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetCompanyResponse.Setcompany(AIndex : Integer; const AValue : TCompany); 

begin
  If (Fcompany=AValue) then exit;
  Fcompany:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCompany
  --------------------------------------------------------------------}


Procedure TCompany.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompany.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompany.SetlocalizedInfos(AIndex : Integer; const AValue : TCompanyTypelocalizedInfosArray); 

begin
  If (FlocalizedInfos=AValue) then exit;
  FlocalizedInfos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompany.Setlocations(AIndex : Integer; const AValue : TCompanyTypelocationsArray); 

begin
  If (Flocations=AValue) then exit;
  Flocations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompany.SetconvertedMinMonthlyBudget(AIndex : Integer; const AValue : TMoney); 

begin
  If (FconvertedMinMonthlyBudget=AValue) then exit;
  FconvertedMinMonthlyBudget:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompany.SetoriginalMinMonthlyBudget(AIndex : Integer; const AValue : TMoney); 

begin
  If (ForiginalMinMonthlyBudget=AValue) then exit;
  ForiginalMinMonthlyBudget:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompany.SetpublicProfile(AIndex : Integer; const AValue : TPublicProfile); 

begin
  If (FpublicProfile=AValue) then exit;
  FpublicProfile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompany.SetcertificationStatuses(AIndex : Integer; const AValue : TCompanyTypecertificationStatusesArray); 

begin
  If (FcertificationStatuses=AValue) then exit;
  FcertificationStatuses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompany.Setranks(AIndex : Integer; const AValue : TCompanyTyperanksArray); 

begin
  If (Franks=AValue) then exit;
  Franks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompany.SetwebsiteUrl(AIndex : Integer; const AValue : String); 

begin
  If (FwebsiteUrl=AValue) then exit;
  FwebsiteUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompany.Setindustries(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Findustries=AValue) then exit;
  Findustries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompany.Setservices(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fservices=AValue) then exit;
  Fservices:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCompany.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'localizedinfos' : SetLength(FlocalizedInfos,ALength);
  'locations' : SetLength(Flocations,ALength);
  'certificationstatuses' : SetLength(FcertificationStatuses,ALength);
  'ranks' : SetLength(Franks,ALength);
  'industries' : SetLength(Findustries,ALength);
  'services' : SetLength(Fservices,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLocalizedCompanyInfo
  --------------------------------------------------------------------}


Procedure TLocalizedCompanyInfo.SetlanguageCode(AIndex : Integer; const AValue : String); 

begin
  If (FlanguageCode=AValue) then exit;
  FlanguageCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocalizedCompanyInfo.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocalizedCompanyInfo.Setoverview(AIndex : Integer; const AValue : String); 

begin
  If (Foverview=AValue) then exit;
  Foverview:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocalizedCompanyInfo.SetcountryCodes(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FcountryCodes=AValue) then exit;
  FcountryCodes:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TLocalizedCompanyInfo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'countrycodes' : SetLength(FcountryCodes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLocation
  --------------------------------------------------------------------}


Procedure TLocation.Setaddress(AIndex : Integer; const AValue : String); 

begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocation.SetlatLng(AIndex : Integer; const AValue : TLatLng); 

begin
  If (FlatLng=AValue) then exit;
  FlatLng:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLatLng
  --------------------------------------------------------------------}


Procedure TLatLng.Setlatitude(AIndex : Integer; const AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLatLng.Setlongitude(AIndex : Integer; const AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPublicProfile
  --------------------------------------------------------------------}


Procedure TPublicProfile.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublicProfile.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublicProfile.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPublicProfile.SetdisplayImageUrl(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayImageUrl=AValue) then exit;
  FdisplayImageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCertificationStatus
  --------------------------------------------------------------------}


Procedure TCertificationStatus.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCertificationStatus.SetexamStatuses(AIndex : Integer; const AValue : TCertificationStatusTypeexamStatusesArray); 

begin
  If (FexamStatuses=AValue) then exit;
  FexamStatuses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCertificationStatus.SetisCertified(AIndex : Integer; const AValue : boolean); 

begin
  If (FisCertified=AValue) then exit;
  FisCertified:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCertificationStatus.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCertificationStatus.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'examstatuses' : SetLength(FexamStatuses,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCertificationExamStatus
  --------------------------------------------------------------------}


Procedure TCertificationExamStatus.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCertificationExamStatus.SetnumberUsersPass(AIndex : Integer; const AValue : integer); 

begin
  If (FnumberUsersPass=AValue) then exit;
  FnumberUsersPass:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCertificationExamStatus.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TRank
  --------------------------------------------------------------------}


Procedure TRank.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRank.Setvalue(AIndex : Integer; const AValue : double); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TRank.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TListCompaniesResponse
  --------------------------------------------------------------------}


Procedure TListCompaniesResponse.SetresponseMetadata(AIndex : Integer; const AValue : TResponseMetadata); 

begin
  If (FresponseMetadata=AValue) then exit;
  FresponseMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListCompaniesResponse.Setcompanies(AIndex : Integer; const AValue : TListCompaniesResponseTypecompaniesArray); 

begin
  If (Fcompanies=AValue) then exit;
  Fcompanies:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListCompaniesResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListCompaniesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'companies' : SetLength(Fcompanies,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCreateLeadRequest
  --------------------------------------------------------------------}


Procedure TCreateLeadRequest.SetrequestMetadata(AIndex : Integer; const AValue : TRequestMetadata); 

begin
  If (FrequestMetadata=AValue) then exit;
  FrequestMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateLeadRequest.Setlead(AIndex : Integer; const AValue : TLead); 

begin
  If (Flead=AValue) then exit;
  Flead:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateLeadRequest.SetrecaptchaChallenge(AIndex : Integer; const AValue : TRecaptchaChallenge); 

begin
  If (FrecaptchaChallenge=AValue) then exit;
  FrecaptchaChallenge:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRecaptchaChallenge
  --------------------------------------------------------------------}


Procedure TRecaptchaChallenge.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRecaptchaChallenge.Setresponse(AIndex : Integer; const AValue : String); 

begin
  If (Fresponse=AValue) then exit;
  Fresponse:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCreateLeadResponse
  --------------------------------------------------------------------}


Procedure TCreateLeadResponse.SetresponseMetadata(AIndex : Integer; const AValue : TResponseMetadata); 

begin
  If (FresponseMetadata=AValue) then exit;
  FresponseMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateLeadResponse.Setlead(AIndex : Integer; const AValue : TLead); 

begin
  If (Flead=AValue) then exit;
  Flead:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCreateLeadResponse.SetrecaptchaStatus(AIndex : Integer; const AValue : String); 

begin
  If (FrecaptchaStatus=AValue) then exit;
  FrecaptchaStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUserEventsResource
  --------------------------------------------------------------------}


Class Function TUserEventsResource.ResourceName : String;

begin
  Result:='userEvents';
end;

Class Function TUserEventsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TpartnersAPI;
end;

Function TUserEventsResource.Log(aLogUserEventRequest : TLogUserEventRequest) : TLogUserEventResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v2/userEvents:log';
  _Methodid   = 'partners.userEvents.log';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aLogUserEventRequest,TLogUserEventResponse) as TLogUserEventResponse;
end;



{ --------------------------------------------------------------------
  TClientMessagesResource
  --------------------------------------------------------------------}


Class Function TClientMessagesResource.ResourceName : String;

begin
  Result:='clientMessages';
end;

Class Function TClientMessagesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TpartnersAPI;
end;

Function TClientMessagesResource.Log(aLogMessageRequest : TLogMessageRequest) : TLogMessageResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v2/clientMessages:log';
  _Methodid   = 'partners.clientMessages.log';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aLogMessageRequest,TLogMessageResponse) as TLogMessageResponse;
end;



{ --------------------------------------------------------------------
  TUserStatesResource
  --------------------------------------------------------------------}


Class Function TUserStatesResource.ResourceName : String;

begin
  Result:='userStates';
end;

Class Function TUserStatesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TpartnersAPI;
end;

Function TUserStatesResource.List(AQuery : string = '') : TListUserStatesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2/userStates';
  _Methodid   = 'partners.userStates.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TListUserStatesResponse) as TListUserStatesResponse;
end;


Function TUserStatesResource.List(AQuery : TUserStateslistOptions) : TListUserStatesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'requestMetadata.userOverrides.ipAddress',AQuery.requestMetadatauserOverridesipAddress);
  AddToQuery(_Q,'requestMetadata.userOverrides.userId',AQuery.requestMetadatauserOverridesuserId);
  AddToQuery(_Q,'requestMetadata.locale',AQuery.requestMetadatalocale);
  AddToQuery(_Q,'requestMetadata.partnersSessionId',AQuery.requestMetadatapartnersSessionId);
  AddToQuery(_Q,'requestMetadata.experimentIds',AQuery.requestMetadataexperimentIds);
  AddToQuery(_Q,'requestMetadata.trafficSource.trafficSourceId',AQuery.requestMetadatatrafficSourcetrafficSourceId);
  AddToQuery(_Q,'requestMetadata.trafficSource.trafficSubId',AQuery.requestMetadatatrafficSourcetrafficSubId);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TCompaniesLeadsResource
  --------------------------------------------------------------------}


Class Function TCompaniesLeadsResource.ResourceName : String;

begin
  Result:='leads';
end;

Class Function TCompaniesLeadsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TpartnersAPI;
end;

Function TCompaniesLeadsResource.Create(companyId: string; aCreateLeadRequest : TCreateLeadRequest) : TCreateLeadResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v2/companies/{companyId}/leads';
  _Methodid   = 'partners.companies.leads.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['companyId',companyId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCreateLeadRequest,TCreateLeadResponse) as TCreateLeadResponse;
end;



{ --------------------------------------------------------------------
  TCompaniesResource
  --------------------------------------------------------------------}


Class Function TCompaniesResource.ResourceName : String;

begin
  Result:='companies';
end;

Class Function TCompaniesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TpartnersAPI;
end;

Function TCompaniesResource.Get(companyId: string; AQuery : string = '') : TGetCompanyResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2/companies/{companyId}';
  _Methodid   = 'partners.companies.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['companyId',companyId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TGetCompanyResponse) as TGetCompanyResponse;
end;


Function TCompaniesResource.Get(companyId: string; AQuery : TCompaniesgetOptions) : TGetCompanyResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'requestMetadata.userOverrides.ipAddress',AQuery.requestMetadatauserOverridesipAddress);
  AddToQuery(_Q,'requestMetadata.userOverrides.userId',AQuery.requestMetadatauserOverridesuserId);
  AddToQuery(_Q,'requestMetadata.locale',AQuery.requestMetadatalocale);
  AddToQuery(_Q,'requestMetadata.partnersSessionId',AQuery.requestMetadatapartnersSessionId);
  AddToQuery(_Q,'requestMetadata.experimentIds',AQuery.requestMetadataexperimentIds);
  AddToQuery(_Q,'requestMetadata.trafficSource.trafficSourceId',AQuery.requestMetadatatrafficSourcetrafficSourceId);
  AddToQuery(_Q,'requestMetadata.trafficSource.trafficSubId',AQuery.requestMetadatatrafficSourcetrafficSubId);
  AddToQuery(_Q,'view',AQuery.view);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'currencyCode',AQuery.currencyCode);
  AddToQuery(_Q,'address',AQuery.address);
  Result:=Get(companyId,_Q);
end;

Function TCompaniesResource.List(AQuery : string = '') : TListCompaniesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v2/companies';
  _Methodid   = 'partners.companies.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TListCompaniesResponse) as TListCompaniesResponse;
end;


Function TCompaniesResource.List(AQuery : TCompanieslistOptions) : TListCompaniesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'requestMetadata.userOverrides.ipAddress',AQuery.requestMetadatauserOverridesipAddress);
  AddToQuery(_Q,'requestMetadata.userOverrides.userId',AQuery.requestMetadatauserOverridesuserId);
  AddToQuery(_Q,'requestMetadata.locale',AQuery.requestMetadatalocale);
  AddToQuery(_Q,'requestMetadata.partnersSessionId',AQuery.requestMetadatapartnersSessionId);
  AddToQuery(_Q,'requestMetadata.experimentIds',AQuery.requestMetadataexperimentIds);
  AddToQuery(_Q,'requestMetadata.trafficSource.trafficSourceId',AQuery.requestMetadatatrafficSourcetrafficSourceId);
  AddToQuery(_Q,'requestMetadata.trafficSource.trafficSubId',AQuery.requestMetadatatrafficSourcetrafficSubId);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'companyName',AQuery.companyName);
  AddToQuery(_Q,'view',AQuery.view);
  AddToQuery(_Q,'minMonthlyBudget.currencyCode',AQuery.minMonthlyBudgetcurrencyCode);
  AddToQuery(_Q,'minMonthlyBudget.units',AQuery.minMonthlyBudgetunits);
  AddToQuery(_Q,'minMonthlyBudget.nanos',AQuery.minMonthlyBudgetnanos);
  AddToQuery(_Q,'maxMonthlyBudget.currencyCode',AQuery.maxMonthlyBudgetcurrencyCode);
  AddToQuery(_Q,'maxMonthlyBudget.units',AQuery.maxMonthlyBudgetunits);
  AddToQuery(_Q,'maxMonthlyBudget.nanos',AQuery.maxMonthlyBudgetnanos);
  AddToQuery(_Q,'industries',AQuery.industries);
  AddToQuery(_Q,'services',AQuery.services);
  AddToQuery(_Q,'languageCodes',AQuery.languageCodes);
  AddToQuery(_Q,'address',AQuery.address);
  AddToQuery(_Q,'orderBy',AQuery.orderBy);
  AddToQuery(_Q,'gpsMotivations',AQuery.gpsMotivations);
  AddToQuery(_Q,'websiteUrl',AQuery.websiteUrl);
  Result:=List(_Q);
end;



Function TCompaniesResource.GetLeadsInstance : TCompaniesLeadsResource;

begin
  if (FLeadsInstance=Nil) then
    FLeadsInstance:=CreateLeadsResource;
  Result:=FLeadsInstance;
end;

Function TCompaniesResource.CreateLeadsResource : TCompaniesLeadsResource;

begin
  Result:=CreateLeadsResource(Self);
end;


Function TCompaniesResource.CreateLeadsResource(AOwner : TComponent) : TCompaniesLeadsResource;

begin
  Result:=TCompaniesLeadsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TPartnersAPI
  --------------------------------------------------------------------}

Class Function TPartnersAPI.APIName : String;

begin
  Result:='partners';
end;

Class Function TPartnersAPI.APIVersion : String;

begin
  Result:='v2';
end;

Class Function TPartnersAPI.APIRevision : String;

begin
  Result:='20151009';
end;

Class Function TPartnersAPI.APIID : String;

begin
  Result:='partners:v2';
end;

Class Function TPartnersAPI.APITitle : String;

begin
  Result:='Google Partners API';
end;

Class Function TPartnersAPI.APIDescription : String;

begin
  Result:='Lets advertisers search certified companies and create contact leads with them, and also audits the usage of clients.';
end;

Class Function TPartnersAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TPartnersAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TPartnersAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TPartnersAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TPartnersAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/partners/';
end;

Class Function TPartnersAPI.APIrootUrl : string;

begin
  Result:='https://partners.googleapis.com/';
end;

Class Function TPartnersAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TPartnersAPI.APIbaseURL : String;

begin
  Result:='https://partners.googleapis.com/';
end;

Class Function TPartnersAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TPartnersAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TPartnersAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TPartnersAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TPartnersAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TPartnersAPI.RegisterAPIResources;

begin
  TLogUserEventRequest.RegisterObject;
  TRequestMetadata.RegisterObject;
  TUserOverrides.RegisterObject;
  TTrafficSource.RegisterObject;
  TEventData.RegisterObject;
  TLead.RegisterObject;
  TMoney.RegisterObject;
  TLogUserEventResponse.RegisterObject;
  TResponseMetadata.RegisterObject;
  TDebugInfo.RegisterObject;
  TLogMessageRequestTypeclientInfo.RegisterObject;
  TLogMessageRequest.RegisterObject;
  TLogMessageResponse.RegisterObject;
  TListUserStatesResponse.RegisterObject;
  TGetCompanyResponse.RegisterObject;
  TCompany.RegisterObject;
  TLocalizedCompanyInfo.RegisterObject;
  TLocation.RegisterObject;
  TLatLng.RegisterObject;
  TPublicProfile.RegisterObject;
  TCertificationStatus.RegisterObject;
  TCertificationExamStatus.RegisterObject;
  TRank.RegisterObject;
  TListCompaniesResponse.RegisterObject;
  TCreateLeadRequest.RegisterObject;
  TRecaptchaChallenge.RegisterObject;
  TCreateLeadResponse.RegisterObject;
end;


Function TPartnersAPI.GetUserEventsInstance : TUserEventsResource;

begin
  if (FUserEventsInstance=Nil) then
    FUserEventsInstance:=CreateUserEventsResource;
  Result:=FUserEventsInstance;
end;

Function TPartnersAPI.CreateUserEventsResource : TUserEventsResource;

begin
  Result:=CreateUserEventsResource(Self);
end;


Function TPartnersAPI.CreateUserEventsResource(AOwner : TComponent) : TUserEventsResource;

begin
  Result:=TUserEventsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPartnersAPI.GetClientMessagesInstance : TClientMessagesResource;

begin
  if (FClientMessagesInstance=Nil) then
    FClientMessagesInstance:=CreateClientMessagesResource;
  Result:=FClientMessagesInstance;
end;

Function TPartnersAPI.CreateClientMessagesResource : TClientMessagesResource;

begin
  Result:=CreateClientMessagesResource(Self);
end;


Function TPartnersAPI.CreateClientMessagesResource(AOwner : TComponent) : TClientMessagesResource;

begin
  Result:=TClientMessagesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPartnersAPI.GetUserStatesInstance : TUserStatesResource;

begin
  if (FUserStatesInstance=Nil) then
    FUserStatesInstance:=CreateUserStatesResource;
  Result:=FUserStatesInstance;
end;

Function TPartnersAPI.CreateUserStatesResource : TUserStatesResource;

begin
  Result:=CreateUserStatesResource(Self);
end;


Function TPartnersAPI.CreateUserStatesResource(AOwner : TComponent) : TUserStatesResource;

begin
  Result:=TUserStatesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPartnersAPI.GetCompaniesLeadsInstance : TCompaniesLeadsResource;

begin
  if (FCompaniesLeadsInstance=Nil) then
    FCompaniesLeadsInstance:=CreateCompaniesLeadsResource;
  Result:=FCompaniesLeadsInstance;
end;

Function TPartnersAPI.CreateCompaniesLeadsResource : TCompaniesLeadsResource;

begin
  Result:=CreateCompaniesLeadsResource(Self);
end;


Function TPartnersAPI.CreateCompaniesLeadsResource(AOwner : TComponent) : TCompaniesLeadsResource;

begin
  Result:=TCompaniesLeadsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPartnersAPI.GetCompaniesInstance : TCompaniesResource;

begin
  if (FCompaniesInstance=Nil) then
    FCompaniesInstance:=CreateCompaniesResource;
  Result:=FCompaniesInstance;
end;

Function TPartnersAPI.CreateCompaniesResource : TCompaniesResource;

begin
  Result:=CreateCompaniesResource(Self);
end;


Function TPartnersAPI.CreateCompaniesResource(AOwner : TComponent) : TCompaniesResource;

begin
  Result:=TCompaniesResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TPartnersAPI.RegisterAPI;
end.
