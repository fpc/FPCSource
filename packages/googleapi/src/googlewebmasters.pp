unit googlewebmasters;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TApiDataRow = Class;
  TApiDimensionFilter = Class;
  TApiDimensionFilterGroup = Class;
  TSearchAnalyticsQueryRequest = Class;
  TSearchAnalyticsQueryResponse = Class;
  TSitemapsListResponse = Class;
  TSitesListResponse = Class;
  TUrlCrawlErrorCount = Class;
  TUrlCrawlErrorCountsPerType = Class;
  TUrlCrawlErrorsCountsQueryResponse = Class;
  TUrlCrawlErrorsSample = Class;
  TUrlCrawlErrorsSamplesListResponse = Class;
  TUrlSampleDetails = Class;
  TWmxSite = Class;
  TWmxSitemap = Class;
  TWmxSitemapContent = Class;
  TApiDataRowArray = Array of TApiDataRow;
  TApiDimensionFilterArray = Array of TApiDimensionFilter;
  TApiDimensionFilterGroupArray = Array of TApiDimensionFilterGroup;
  TSearchAnalyticsQueryRequestArray = Array of TSearchAnalyticsQueryRequest;
  TSearchAnalyticsQueryResponseArray = Array of TSearchAnalyticsQueryResponse;
  TSitemapsListResponseArray = Array of TSitemapsListResponse;
  TSitesListResponseArray = Array of TSitesListResponse;
  TUrlCrawlErrorCountArray = Array of TUrlCrawlErrorCount;
  TUrlCrawlErrorCountsPerTypeArray = Array of TUrlCrawlErrorCountsPerType;
  TUrlCrawlErrorsCountsQueryResponseArray = Array of TUrlCrawlErrorsCountsQueryResponse;
  TUrlCrawlErrorsSampleArray = Array of TUrlCrawlErrorsSample;
  TUrlCrawlErrorsSamplesListResponseArray = Array of TUrlCrawlErrorsSamplesListResponse;
  TUrlSampleDetailsArray = Array of TUrlSampleDetails;
  TWmxSiteArray = Array of TWmxSite;
  TWmxSitemapArray = Array of TWmxSitemap;
  TWmxSitemapContentArray = Array of TWmxSitemapContent;
  //Anonymous types, using auto-generated names
  TApiDimensionFilterGroupTypefiltersArray = Array of TApiDimensionFilter;
  TSearchAnalyticsQueryRequestTypedimensionFilterGroupsArray = Array of TApiDimensionFilterGroup;
  TSearchAnalyticsQueryResponseTyperowsArray = Array of TApiDataRow;
  TSitemapsListResponseTypesitemapArray = Array of TWmxSitemap;
  TSitesListResponseTypesiteEntryArray = Array of TWmxSite;
  TUrlCrawlErrorCountsPerTypeTypeentriesArray = Array of TUrlCrawlErrorCount;
  TUrlCrawlErrorsCountsQueryResponseTypecountPerTypesArray = Array of TUrlCrawlErrorCountsPerType;
  TUrlCrawlErrorsSamplesListResponseTypeurlCrawlErrorSampleArray = Array of TUrlCrawlErrorsSample;
  TWmxSitemapTypecontentsArray = Array of TWmxSitemapContent;
  
  { --------------------------------------------------------------------
    TApiDataRow
    --------------------------------------------------------------------}
  
  TApiDataRow = Class(TGoogleBaseObject)
  Private
    Fclicks : double;
    Fctr : double;
    Fimpressions : double;
    Fkeys : TStringArray;
    Fposition : double;
  Protected
    //Property setters
    Procedure Setclicks(AIndex : Integer; const AValue : double); virtual;
    Procedure Setctr(AIndex : Integer; const AValue : double); virtual;
    Procedure Setimpressions(AIndex : Integer; const AValue : double); virtual;
    Procedure Setkeys(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setposition(AIndex : Integer; const AValue : double); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property clicks : double Index 0 Read Fclicks Write Setclicks;
    Property ctr : double Index 8 Read Fctr Write Setctr;
    Property impressions : double Index 16 Read Fimpressions Write Setimpressions;
    Property keys : TStringArray Index 24 Read Fkeys Write Setkeys;
    Property position : double Index 32 Read Fposition Write Setposition;
  end;
  TApiDataRowClass = Class of TApiDataRow;
  
  { --------------------------------------------------------------------
    TApiDimensionFilter
    --------------------------------------------------------------------}
  
  TApiDimensionFilter = Class(TGoogleBaseObject)
  Private
    Fdimension : String;
    Fexpression : String;
    F_operator : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdimension(AIndex : Integer; const AValue : String); virtual;
    Procedure Setexpression(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_operator(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property dimension : String Index 0 Read Fdimension Write Setdimension;
    Property expression : String Index 8 Read Fexpression Write Setexpression;
    Property _operator : String Index 16 Read F_operator Write Set_operator;
  end;
  TApiDimensionFilterClass = Class of TApiDimensionFilter;
  
  { --------------------------------------------------------------------
    TApiDimensionFilterGroup
    --------------------------------------------------------------------}
  
  TApiDimensionFilterGroup = Class(TGoogleBaseObject)
  Private
    Ffilters : TApiDimensionFilterGroupTypefiltersArray;
    FgroupType : String;
  Protected
    //Property setters
    Procedure Setfilters(AIndex : Integer; const AValue : TApiDimensionFilterGroupTypefiltersArray); virtual;
    Procedure SetgroupType(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property filters : TApiDimensionFilterGroupTypefiltersArray Index 0 Read Ffilters Write Setfilters;
    Property groupType : String Index 8 Read FgroupType Write SetgroupType;
  end;
  TApiDimensionFilterGroupClass = Class of TApiDimensionFilterGroup;
  
  { --------------------------------------------------------------------
    TSearchAnalyticsQueryRequest
    --------------------------------------------------------------------}
  
  TSearchAnalyticsQueryRequest = Class(TGoogleBaseObject)
  Private
    FaggregationType : String;
    FdimensionFilterGroups : TSearchAnalyticsQueryRequestTypedimensionFilterGroupsArray;
    Fdimensions : TStringArray;
    FendDate : String;
    FrowLimit : integer;
    FsearchType : String;
    FstartDate : String;
    FstartRow : integer;
  Protected
    //Property setters
    Procedure SetaggregationType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdimensionFilterGroups(AIndex : Integer; const AValue : TSearchAnalyticsQueryRequestTypedimensionFilterGroupsArray); virtual;
    Procedure Setdimensions(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetendDate(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrowLimit(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetsearchType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartDate(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartRow(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property aggregationType : String Index 0 Read FaggregationType Write SetaggregationType;
    Property dimensionFilterGroups : TSearchAnalyticsQueryRequestTypedimensionFilterGroupsArray Index 8 Read FdimensionFilterGroups Write SetdimensionFilterGroups;
    Property dimensions : TStringArray Index 16 Read Fdimensions Write Setdimensions;
    Property endDate : String Index 24 Read FendDate Write SetendDate;
    Property rowLimit : integer Index 32 Read FrowLimit Write SetrowLimit;
    Property searchType : String Index 40 Read FsearchType Write SetsearchType;
    Property startDate : String Index 48 Read FstartDate Write SetstartDate;
    Property startRow : integer Index 56 Read FstartRow Write SetstartRow;
  end;
  TSearchAnalyticsQueryRequestClass = Class of TSearchAnalyticsQueryRequest;
  
  { --------------------------------------------------------------------
    TSearchAnalyticsQueryResponse
    --------------------------------------------------------------------}
  
  TSearchAnalyticsQueryResponse = Class(TGoogleBaseObject)
  Private
    FresponseAggregationType : String;
    Frows : TSearchAnalyticsQueryResponseTyperowsArray;
  Protected
    //Property setters
    Procedure SetresponseAggregationType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrows(AIndex : Integer; const AValue : TSearchAnalyticsQueryResponseTyperowsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property responseAggregationType : String Index 0 Read FresponseAggregationType Write SetresponseAggregationType;
    Property rows : TSearchAnalyticsQueryResponseTyperowsArray Index 8 Read Frows Write Setrows;
  end;
  TSearchAnalyticsQueryResponseClass = Class of TSearchAnalyticsQueryResponse;
  
  { --------------------------------------------------------------------
    TSitemapsListResponse
    --------------------------------------------------------------------}
  
  TSitemapsListResponse = Class(TGoogleBaseObject)
  Private
    Fsitemap : TSitemapsListResponseTypesitemapArray;
  Protected
    //Property setters
    Procedure Setsitemap(AIndex : Integer; const AValue : TSitemapsListResponseTypesitemapArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property sitemap : TSitemapsListResponseTypesitemapArray Index 0 Read Fsitemap Write Setsitemap;
  end;
  TSitemapsListResponseClass = Class of TSitemapsListResponse;
  
  { --------------------------------------------------------------------
    TSitesListResponse
    --------------------------------------------------------------------}
  
  TSitesListResponse = Class(TGoogleBaseObject)
  Private
    FsiteEntry : TSitesListResponseTypesiteEntryArray;
  Protected
    //Property setters
    Procedure SetsiteEntry(AIndex : Integer; const AValue : TSitesListResponseTypesiteEntryArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property siteEntry : TSitesListResponseTypesiteEntryArray Index 0 Read FsiteEntry Write SetsiteEntry;
  end;
  TSitesListResponseClass = Class of TSitesListResponse;
  
  { --------------------------------------------------------------------
    TUrlCrawlErrorCount
    --------------------------------------------------------------------}
  
  TUrlCrawlErrorCount = Class(TGoogleBaseObject)
  Private
    Fcount : String;
    Ftimestamp : TDatetime;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; const AValue : String); virtual;
    Procedure Settimestamp(AIndex : Integer; const AValue : TDatetime); virtual;
  Public
  Published
    Property count : String Index 0 Read Fcount Write Setcount;
    Property timestamp : TDatetime Index 8 Read Ftimestamp Write Settimestamp;
  end;
  TUrlCrawlErrorCountClass = Class of TUrlCrawlErrorCount;
  
  { --------------------------------------------------------------------
    TUrlCrawlErrorCountsPerType
    --------------------------------------------------------------------}
  
  TUrlCrawlErrorCountsPerType = Class(TGoogleBaseObject)
  Private
    Fcategory : String;
    Fentries : TUrlCrawlErrorCountsPerTypeTypeentriesArray;
    Fplatform : String;
  Protected
    //Property setters
    Procedure Setcategory(AIndex : Integer; const AValue : String); virtual;
    Procedure Setentries(AIndex : Integer; const AValue : TUrlCrawlErrorCountsPerTypeTypeentriesArray); virtual;
    Procedure Setplatform(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property category : String Index 0 Read Fcategory Write Setcategory;
    Property entries : TUrlCrawlErrorCountsPerTypeTypeentriesArray Index 8 Read Fentries Write Setentries;
    Property platform : String Index 16 Read Fplatform Write Setplatform;
  end;
  TUrlCrawlErrorCountsPerTypeClass = Class of TUrlCrawlErrorCountsPerType;
  
  { --------------------------------------------------------------------
    TUrlCrawlErrorsCountsQueryResponse
    --------------------------------------------------------------------}
  
  TUrlCrawlErrorsCountsQueryResponse = Class(TGoogleBaseObject)
  Private
    FcountPerTypes : TUrlCrawlErrorsCountsQueryResponseTypecountPerTypesArray;
  Protected
    //Property setters
    Procedure SetcountPerTypes(AIndex : Integer; const AValue : TUrlCrawlErrorsCountsQueryResponseTypecountPerTypesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property countPerTypes : TUrlCrawlErrorsCountsQueryResponseTypecountPerTypesArray Index 0 Read FcountPerTypes Write SetcountPerTypes;
  end;
  TUrlCrawlErrorsCountsQueryResponseClass = Class of TUrlCrawlErrorsCountsQueryResponse;
  
  { --------------------------------------------------------------------
    TUrlCrawlErrorsSample
    --------------------------------------------------------------------}
  
  TUrlCrawlErrorsSample = Class(TGoogleBaseObject)
  Private
    Ffirst_detected : TDatetime;
    Flast_crawled : TDatetime;
    FpageUrl : String;
    FresponseCode : integer;
    FurlDetails : TUrlSampleDetails;
  Protected
    //Property setters
    Procedure Setfirst_detected(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure Setlast_crawled(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetpageUrl(AIndex : Integer; const AValue : String); virtual;
    Procedure SetresponseCode(AIndex : Integer; const AValue : integer); virtual;
    Procedure SeturlDetails(AIndex : Integer; const AValue : TUrlSampleDetails); virtual;
  Public
  Published
    Property first_detected : TDatetime Index 0 Read Ffirst_detected Write Setfirst_detected;
    Property last_crawled : TDatetime Index 8 Read Flast_crawled Write Setlast_crawled;
    Property pageUrl : String Index 16 Read FpageUrl Write SetpageUrl;
    Property responseCode : integer Index 24 Read FresponseCode Write SetresponseCode;
    Property urlDetails : TUrlSampleDetails Index 32 Read FurlDetails Write SeturlDetails;
  end;
  TUrlCrawlErrorsSampleClass = Class of TUrlCrawlErrorsSample;
  
  { --------------------------------------------------------------------
    TUrlCrawlErrorsSamplesListResponse
    --------------------------------------------------------------------}
  
  TUrlCrawlErrorsSamplesListResponse = Class(TGoogleBaseObject)
  Private
    FurlCrawlErrorSample : TUrlCrawlErrorsSamplesListResponseTypeurlCrawlErrorSampleArray;
  Protected
    //Property setters
    Procedure SeturlCrawlErrorSample(AIndex : Integer; const AValue : TUrlCrawlErrorsSamplesListResponseTypeurlCrawlErrorSampleArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property urlCrawlErrorSample : TUrlCrawlErrorsSamplesListResponseTypeurlCrawlErrorSampleArray Index 0 Read FurlCrawlErrorSample Write SeturlCrawlErrorSample;
  end;
  TUrlCrawlErrorsSamplesListResponseClass = Class of TUrlCrawlErrorsSamplesListResponse;
  
  { --------------------------------------------------------------------
    TUrlSampleDetails
    --------------------------------------------------------------------}
  
  TUrlSampleDetails = Class(TGoogleBaseObject)
  Private
    FcontainingSitemaps : TStringArray;
    FlinkedFromUrls : TStringArray;
  Protected
    //Property setters
    Procedure SetcontainingSitemaps(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetlinkedFromUrls(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property containingSitemaps : TStringArray Index 0 Read FcontainingSitemaps Write SetcontainingSitemaps;
    Property linkedFromUrls : TStringArray Index 8 Read FlinkedFromUrls Write SetlinkedFromUrls;
  end;
  TUrlSampleDetailsClass = Class of TUrlSampleDetails;
  
  { --------------------------------------------------------------------
    TWmxSite
    --------------------------------------------------------------------}
  
  TWmxSite = Class(TGoogleBaseObject)
  Private
    FpermissionLevel : String;
    FsiteUrl : String;
  Protected
    //Property setters
    Procedure SetpermissionLevel(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsiteUrl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property permissionLevel : String Index 0 Read FpermissionLevel Write SetpermissionLevel;
    Property siteUrl : String Index 8 Read FsiteUrl Write SetsiteUrl;
  end;
  TWmxSiteClass = Class of TWmxSite;
  
  { --------------------------------------------------------------------
    TWmxSitemap
    --------------------------------------------------------------------}
  
  TWmxSitemap = Class(TGoogleBaseObject)
  Private
    Fcontents : TWmxSitemapTypecontentsArray;
    Ferrors : String;
    FisPending : boolean;
    FisSitemapsIndex : boolean;
    FlastDownloaded : TDatetime;
    FlastSubmitted : TDatetime;
    Fpath : String;
    F_type : String;
    Fwarnings : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcontents(AIndex : Integer; const AValue : TWmxSitemapTypecontentsArray); virtual;
    Procedure Seterrors(AIndex : Integer; const AValue : String); virtual;
    Procedure SetisPending(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetisSitemapsIndex(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetlastDownloaded(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure SetlastSubmitted(AIndex : Integer; const AValue : TDatetime); virtual;
    Procedure Setpath(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setwarnings(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property contents : TWmxSitemapTypecontentsArray Index 0 Read Fcontents Write Setcontents;
    Property errors : String Index 8 Read Ferrors Write Seterrors;
    Property isPending : boolean Index 16 Read FisPending Write SetisPending;
    Property isSitemapsIndex : boolean Index 24 Read FisSitemapsIndex Write SetisSitemapsIndex;
    Property lastDownloaded : TDatetime Index 32 Read FlastDownloaded Write SetlastDownloaded;
    Property lastSubmitted : TDatetime Index 40 Read FlastSubmitted Write SetlastSubmitted;
    Property path : String Index 48 Read Fpath Write Setpath;
    Property _type : String Index 56 Read F_type Write Set_type;
    Property warnings : String Index 64 Read Fwarnings Write Setwarnings;
  end;
  TWmxSitemapClass = Class of TWmxSitemap;
  
  { --------------------------------------------------------------------
    TWmxSitemapContent
    --------------------------------------------------------------------}
  
  TWmxSitemapContent = Class(TGoogleBaseObject)
  Private
    Findexed : String;
    Fsubmitted : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setindexed(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsubmitted(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property indexed : String Index 0 Read Findexed Write Setindexed;
    Property submitted : String Index 8 Read Fsubmitted Write Setsubmitted;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TWmxSitemapContentClass = Class of TWmxSitemapContent;
  
  { --------------------------------------------------------------------
    TSearchanalyticsResource
    --------------------------------------------------------------------}
  
  TSearchanalyticsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Query(siteUrl: string; aSearchAnalyticsQueryRequest : TSearchAnalyticsQueryRequest) : TSearchAnalyticsQueryResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TSitemapsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSitemapsResource, method List
  
  TSitemapsListOptions = Record
    sitemapIndex : String;
  end;
  
  TSitemapsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(feedpath: string; siteUrl: string);
    Function Get(feedpath: string; siteUrl: string) : TWmxSitemap;
    Function List(siteUrl: string; AQuery : string  = '') : TSitemapsListResponse;
    Function List(siteUrl: string; AQuery : TSitemapslistOptions) : TSitemapsListResponse;
    Procedure Submit(feedpath: string; siteUrl: string);
  end;
  
  
  { --------------------------------------------------------------------
    TSitesResource
    --------------------------------------------------------------------}
  
  TSitesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Add(siteUrl: string);
    Procedure Delete(siteUrl: string);
    Function Get(siteUrl: string) : TWmxSite;
    Function List : TSitesListResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TUrlcrawlerrorscountsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUrlcrawlerrorscountsResource, method Query
  
  TUrlcrawlerrorscountsQueryOptions = Record
    category : String;
    latestCountsOnly : boolean;
    platform : String;
  end;
  
  TUrlcrawlerrorscountsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Query(siteUrl: string; AQuery : string  = '') : TUrlCrawlErrorsCountsQueryResponse;
    Function Query(siteUrl: string; AQuery : TUrlcrawlerrorscountsqueryOptions) : TUrlCrawlErrorsCountsQueryResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TUrlcrawlerrorssamplesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUrlcrawlerrorssamplesResource, method Get
  
  TUrlcrawlerrorssamplesGetOptions = Record
    category : String;
    platform : String;
  end;
  
  
  //Optional query Options for TUrlcrawlerrorssamplesResource, method List
  
  TUrlcrawlerrorssamplesListOptions = Record
    category : String;
    platform : String;
  end;
  
  
  //Optional query Options for TUrlcrawlerrorssamplesResource, method MarkAsFixed
  
  TUrlcrawlerrorssamplesMarkAsFixedOptions = Record
    category : String;
    platform : String;
  end;
  
  TUrlcrawlerrorssamplesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(siteUrl: string; url: string; AQuery : string  = '') : TUrlCrawlErrorsSample;
    Function Get(siteUrl: string; url: string; AQuery : TUrlcrawlerrorssamplesgetOptions) : TUrlCrawlErrorsSample;
    Function List(siteUrl: string; AQuery : string  = '') : TUrlCrawlErrorsSamplesListResponse;
    Function List(siteUrl: string; AQuery : TUrlcrawlerrorssampleslistOptions) : TUrlCrawlErrorsSamplesListResponse;
    Procedure MarkAsFixed(siteUrl: string; url: string; AQuery : string  = '');
    Procedure MarkAsFixed(siteUrl: string; url: string; AQuery : TUrlcrawlerrorssamplesmarkAsFixedOptions);
  end;
  
  
  { --------------------------------------------------------------------
    TWebmastersAPI
    --------------------------------------------------------------------}
  
  TWebmastersAPI = Class(TGoogleAPI)
  Private
    FSearchanalyticsInstance : TSearchanalyticsResource;
    FSitemapsInstance : TSitemapsResource;
    FSitesInstance : TSitesResource;
    FUrlcrawlerrorscountsInstance : TUrlcrawlerrorscountsResource;
    FUrlcrawlerrorssamplesInstance : TUrlcrawlerrorssamplesResource;
    Function GetSearchanalyticsInstance : TSearchanalyticsResource;virtual;
    Function GetSitemapsInstance : TSitemapsResource;virtual;
    Function GetSitesInstance : TSitesResource;virtual;
    Function GetUrlcrawlerrorscountsInstance : TUrlcrawlerrorscountsResource;virtual;
    Function GetUrlcrawlerrorssamplesInstance : TUrlcrawlerrorssamplesResource;virtual;
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
    Function CreateSearchanalyticsResource(AOwner : TComponent) : TSearchanalyticsResource;virtual;overload;
    Function CreateSearchanalyticsResource : TSearchanalyticsResource;virtual;overload;
    Function CreateSitemapsResource(AOwner : TComponent) : TSitemapsResource;virtual;overload;
    Function CreateSitemapsResource : TSitemapsResource;virtual;overload;
    Function CreateSitesResource(AOwner : TComponent) : TSitesResource;virtual;overload;
    Function CreateSitesResource : TSitesResource;virtual;overload;
    Function CreateUrlcrawlerrorscountsResource(AOwner : TComponent) : TUrlcrawlerrorscountsResource;virtual;overload;
    Function CreateUrlcrawlerrorscountsResource : TUrlcrawlerrorscountsResource;virtual;overload;
    Function CreateUrlcrawlerrorssamplesResource(AOwner : TComponent) : TUrlcrawlerrorssamplesResource;virtual;overload;
    Function CreateUrlcrawlerrorssamplesResource : TUrlcrawlerrorssamplesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property SearchanalyticsResource : TSearchanalyticsResource Read GetSearchanalyticsInstance;
    Property SitemapsResource : TSitemapsResource Read GetSitemapsInstance;
    Property SitesResource : TSitesResource Read GetSitesInstance;
    Property UrlcrawlerrorscountsResource : TUrlcrawlerrorscountsResource Read GetUrlcrawlerrorscountsInstance;
    Property UrlcrawlerrorssamplesResource : TUrlcrawlerrorssamplesResource Read GetUrlcrawlerrorssamplesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TApiDataRow
  --------------------------------------------------------------------}


Procedure TApiDataRow.Setclicks(AIndex : Integer; const AValue : double); 

begin
  If (Fclicks=AValue) then exit;
  Fclicks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApiDataRow.Setctr(AIndex : Integer; const AValue : double); 

begin
  If (Fctr=AValue) then exit;
  Fctr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApiDataRow.Setimpressions(AIndex : Integer; const AValue : double); 

begin
  If (Fimpressions=AValue) then exit;
  Fimpressions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApiDataRow.Setkeys(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fkeys=AValue) then exit;
  Fkeys:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApiDataRow.Setposition(AIndex : Integer; const AValue : double); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TApiDataRow.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'keys' : SetLength(Fkeys,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TApiDimensionFilter
  --------------------------------------------------------------------}


Procedure TApiDimensionFilter.Setdimension(AIndex : Integer; const AValue : String); 

begin
  If (Fdimension=AValue) then exit;
  Fdimension:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApiDimensionFilter.Setexpression(AIndex : Integer; const AValue : String); 

begin
  If (Fexpression=AValue) then exit;
  Fexpression:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApiDimensionFilter.Set_operator(AIndex : Integer; const AValue : String); 

begin
  If (F_operator=AValue) then exit;
  F_operator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TApiDimensionFilter.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_operator' : Result:='operator';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TApiDimensionFilterGroup
  --------------------------------------------------------------------}


Procedure TApiDimensionFilterGroup.Setfilters(AIndex : Integer; const AValue : TApiDimensionFilterGroupTypefiltersArray); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TApiDimensionFilterGroup.SetgroupType(AIndex : Integer; const AValue : String); 

begin
  If (FgroupType=AValue) then exit;
  FgroupType:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TApiDimensionFilterGroup.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'filters' : SetLength(Ffilters,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchAnalyticsQueryRequest
  --------------------------------------------------------------------}


Procedure TSearchAnalyticsQueryRequest.SetaggregationType(AIndex : Integer; const AValue : String); 

begin
  If (FaggregationType=AValue) then exit;
  FaggregationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnalyticsQueryRequest.SetdimensionFilterGroups(AIndex : Integer; const AValue : TSearchAnalyticsQueryRequestTypedimensionFilterGroupsArray); 

begin
  If (FdimensionFilterGroups=AValue) then exit;
  FdimensionFilterGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnalyticsQueryRequest.Setdimensions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fdimensions=AValue) then exit;
  Fdimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnalyticsQueryRequest.SetendDate(AIndex : Integer; const AValue : String); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnalyticsQueryRequest.SetrowLimit(AIndex : Integer; const AValue : integer); 

begin
  If (FrowLimit=AValue) then exit;
  FrowLimit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnalyticsQueryRequest.SetsearchType(AIndex : Integer; const AValue : String); 

begin
  If (FsearchType=AValue) then exit;
  FsearchType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnalyticsQueryRequest.SetstartDate(AIndex : Integer; const AValue : String); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnalyticsQueryRequest.SetstartRow(AIndex : Integer; const AValue : integer); 

begin
  If (FstartRow=AValue) then exit;
  FstartRow:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchAnalyticsQueryRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'dimensionfiltergroups' : SetLength(FdimensionFilterGroups,ALength);
  'dimensions' : SetLength(Fdimensions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchAnalyticsQueryResponse
  --------------------------------------------------------------------}


Procedure TSearchAnalyticsQueryResponse.SetresponseAggregationType(AIndex : Integer; const AValue : String); 

begin
  If (FresponseAggregationType=AValue) then exit;
  FresponseAggregationType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnalyticsQueryResponse.Setrows(AIndex : Integer; const AValue : TSearchAnalyticsQueryResponseTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchAnalyticsQueryResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'rows' : SetLength(Frows,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSitemapsListResponse
  --------------------------------------------------------------------}


Procedure TSitemapsListResponse.Setsitemap(AIndex : Integer; const AValue : TSitemapsListResponseTypesitemapArray); 

begin
  If (Fsitemap=AValue) then exit;
  Fsitemap:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSitemapsListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sitemap' : SetLength(Fsitemap,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSitesListResponse
  --------------------------------------------------------------------}


Procedure TSitesListResponse.SetsiteEntry(AIndex : Integer; const AValue : TSitesListResponseTypesiteEntryArray); 

begin
  If (FsiteEntry=AValue) then exit;
  FsiteEntry:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSitesListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'siteentry' : SetLength(FsiteEntry,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUrlCrawlErrorCount
  --------------------------------------------------------------------}


Procedure TUrlCrawlErrorCount.Setcount(AIndex : Integer; const AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlCrawlErrorCount.Settimestamp(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Ftimestamp=AValue) then exit;
  Ftimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlCrawlErrorCountsPerType
  --------------------------------------------------------------------}


Procedure TUrlCrawlErrorCountsPerType.Setcategory(AIndex : Integer; const AValue : String); 

begin
  If (Fcategory=AValue) then exit;
  Fcategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlCrawlErrorCountsPerType.Setentries(AIndex : Integer; const AValue : TUrlCrawlErrorCountsPerTypeTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlCrawlErrorCountsPerType.Setplatform(AIndex : Integer; const AValue : String); 

begin
  If (Fplatform=AValue) then exit;
  Fplatform:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUrlCrawlErrorCountsPerType.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUrlCrawlErrorsCountsQueryResponse
  --------------------------------------------------------------------}


Procedure TUrlCrawlErrorsCountsQueryResponse.SetcountPerTypes(AIndex : Integer; const AValue : TUrlCrawlErrorsCountsQueryResponseTypecountPerTypesArray); 

begin
  If (FcountPerTypes=AValue) then exit;
  FcountPerTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUrlCrawlErrorsCountsQueryResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'countpertypes' : SetLength(FcountPerTypes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUrlCrawlErrorsSample
  --------------------------------------------------------------------}


Procedure TUrlCrawlErrorsSample.Setfirst_detected(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Ffirst_detected=AValue) then exit;
  Ffirst_detected:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlCrawlErrorsSample.Setlast_crawled(AIndex : Integer; const AValue : TDatetime); 

begin
  If (Flast_crawled=AValue) then exit;
  Flast_crawled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlCrawlErrorsSample.SetpageUrl(AIndex : Integer; const AValue : String); 

begin
  If (FpageUrl=AValue) then exit;
  FpageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlCrawlErrorsSample.SetresponseCode(AIndex : Integer; const AValue : integer); 

begin
  If (FresponseCode=AValue) then exit;
  FresponseCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlCrawlErrorsSample.SeturlDetails(AIndex : Integer; const AValue : TUrlSampleDetails); 

begin
  If (FurlDetails=AValue) then exit;
  FurlDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlCrawlErrorsSamplesListResponse
  --------------------------------------------------------------------}


Procedure TUrlCrawlErrorsSamplesListResponse.SeturlCrawlErrorSample(AIndex : Integer; const AValue : TUrlCrawlErrorsSamplesListResponseTypeurlCrawlErrorSampleArray); 

begin
  If (FurlCrawlErrorSample=AValue) then exit;
  FurlCrawlErrorSample:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUrlCrawlErrorsSamplesListResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'urlcrawlerrorsample' : SetLength(FurlCrawlErrorSample,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUrlSampleDetails
  --------------------------------------------------------------------}


Procedure TUrlSampleDetails.SetcontainingSitemaps(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FcontainingSitemaps=AValue) then exit;
  FcontainingSitemaps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlSampleDetails.SetlinkedFromUrls(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FlinkedFromUrls=AValue) then exit;
  FlinkedFromUrls:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUrlSampleDetails.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'containingsitemaps' : SetLength(FcontainingSitemaps,ALength);
  'linkedfromurls' : SetLength(FlinkedFromUrls,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWmxSite
  --------------------------------------------------------------------}


Procedure TWmxSite.SetpermissionLevel(AIndex : Integer; const AValue : String); 

begin
  If (FpermissionLevel=AValue) then exit;
  FpermissionLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSite.SetsiteUrl(AIndex : Integer; const AValue : String); 

begin
  If (FsiteUrl=AValue) then exit;
  FsiteUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWmxSitemap
  --------------------------------------------------------------------}


Procedure TWmxSitemap.Setcontents(AIndex : Integer; const AValue : TWmxSitemapTypecontentsArray); 

begin
  If (Fcontents=AValue) then exit;
  Fcontents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemap.Seterrors(AIndex : Integer; const AValue : String); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemap.SetisPending(AIndex : Integer; const AValue : boolean); 

begin
  If (FisPending=AValue) then exit;
  FisPending:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemap.SetisSitemapsIndex(AIndex : Integer; const AValue : boolean); 

begin
  If (FisSitemapsIndex=AValue) then exit;
  FisSitemapsIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemap.SetlastDownloaded(AIndex : Integer; const AValue : TDatetime); 

begin
  If (FlastDownloaded=AValue) then exit;
  FlastDownloaded:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemap.SetlastSubmitted(AIndex : Integer; const AValue : TDatetime); 

begin
  If (FlastSubmitted=AValue) then exit;
  FlastSubmitted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemap.Setpath(AIndex : Integer; const AValue : String); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemap.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemap.Setwarnings(AIndex : Integer; const AValue : String); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TWmxSitemap.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TWmxSitemap.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'contents' : SetLength(Fcontents,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TWmxSitemapContent
  --------------------------------------------------------------------}


Procedure TWmxSitemapContent.Setindexed(AIndex : Integer; const AValue : String); 

begin
  If (Findexed=AValue) then exit;
  Findexed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemapContent.Setsubmitted(AIndex : Integer; const AValue : String); 

begin
  If (Fsubmitted=AValue) then exit;
  Fsubmitted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemapContent.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TWmxSitemapContent.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TSearchanalyticsResource
  --------------------------------------------------------------------}


Class Function TSearchanalyticsResource.ResourceName : String;

begin
  Result:='searchanalytics';
end;

Class Function TSearchanalyticsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TwebmastersAPI;
end;

Function TSearchanalyticsResource.Query(siteUrl: string; aSearchAnalyticsQueryRequest : TSearchAnalyticsQueryRequest) : TSearchAnalyticsQueryResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'sites/{siteUrl}/searchAnalytics/query';
  _Methodid   = 'webmasters.searchanalytics.query';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['siteUrl',siteUrl]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSearchAnalyticsQueryRequest,TSearchAnalyticsQueryResponse) as TSearchAnalyticsQueryResponse;
end;



{ --------------------------------------------------------------------
  TSitemapsResource
  --------------------------------------------------------------------}


Class Function TSitemapsResource.ResourceName : String;

begin
  Result:='sitemaps';
end;

Class Function TSitemapsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TwebmastersAPI;
end;

Procedure TSitemapsResource.Delete(feedpath: string; siteUrl: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'sites/{siteUrl}/sitemaps/{feedpath}';
  _Methodid   = 'webmasters.sitemaps.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['feedpath',feedpath,'siteUrl',siteUrl]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TSitemapsResource.Get(feedpath: string; siteUrl: string) : TWmxSitemap;

Const
  _HTTPMethod = 'GET';
  _Path       = 'sites/{siteUrl}/sitemaps/{feedpath}';
  _Methodid   = 'webmasters.sitemaps.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['feedpath',feedpath,'siteUrl',siteUrl]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TWmxSitemap) as TWmxSitemap;
end;

Function TSitemapsResource.List(siteUrl: string; AQuery : string = '') : TSitemapsListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'sites/{siteUrl}/sitemaps';
  _Methodid   = 'webmasters.sitemaps.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['siteUrl',siteUrl]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TSitemapsListResponse) as TSitemapsListResponse;
end;


Function TSitemapsResource.List(siteUrl: string; AQuery : TSitemapslistOptions) : TSitemapsListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'sitemapIndex',AQuery.sitemapIndex);
  Result:=List(siteUrl,_Q);
end;

Procedure TSitemapsResource.Submit(feedpath: string; siteUrl: string);

Const
  _HTTPMethod = 'PUT';
  _Path       = 'sites/{siteUrl}/sitemaps/{feedpath}';
  _Methodid   = 'webmasters.sitemaps.submit';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['feedpath',feedpath,'siteUrl',siteUrl]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;



{ --------------------------------------------------------------------
  TSitesResource
  --------------------------------------------------------------------}


Class Function TSitesResource.ResourceName : String;

begin
  Result:='sites';
end;

Class Function TSitesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TwebmastersAPI;
end;

Procedure TSitesResource.Add(siteUrl: string);

Const
  _HTTPMethod = 'PUT';
  _Path       = 'sites/{siteUrl}';
  _Methodid   = 'webmasters.sites.add';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['siteUrl',siteUrl]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Procedure TSitesResource.Delete(siteUrl: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'sites/{siteUrl}';
  _Methodid   = 'webmasters.sites.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['siteUrl',siteUrl]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TSitesResource.Get(siteUrl: string) : TWmxSite;

Const
  _HTTPMethod = 'GET';
  _Path       = 'sites/{siteUrl}';
  _Methodid   = 'webmasters.sites.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['siteUrl',siteUrl]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TWmxSite) as TWmxSite;
end;

Function TSitesResource.List : TSitesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'sites';
  _Methodid   = 'webmasters.sites.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TSitesListResponse) as TSitesListResponse;
end;



{ --------------------------------------------------------------------
  TUrlcrawlerrorscountsResource
  --------------------------------------------------------------------}


Class Function TUrlcrawlerrorscountsResource.ResourceName : String;

begin
  Result:='urlcrawlerrorscounts';
end;

Class Function TUrlcrawlerrorscountsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TwebmastersAPI;
end;

Function TUrlcrawlerrorscountsResource.Query(siteUrl: string; AQuery : string = '') : TUrlCrawlErrorsCountsQueryResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'sites/{siteUrl}/urlCrawlErrorsCounts/query';
  _Methodid   = 'webmasters.urlcrawlerrorscounts.query';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['siteUrl',siteUrl]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TUrlCrawlErrorsCountsQueryResponse) as TUrlCrawlErrorsCountsQueryResponse;
end;


Function TUrlcrawlerrorscountsResource.Query(siteUrl: string; AQuery : TUrlcrawlerrorscountsqueryOptions) : TUrlCrawlErrorsCountsQueryResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'category',AQuery.category);
  AddToQuery(_Q,'latestCountsOnly',AQuery.latestCountsOnly);
  AddToQuery(_Q,'platform',AQuery.platform);
  Result:=Query(siteUrl,_Q);
end;



{ --------------------------------------------------------------------
  TUrlcrawlerrorssamplesResource
  --------------------------------------------------------------------}


Class Function TUrlcrawlerrorssamplesResource.ResourceName : String;

begin
  Result:='urlcrawlerrorssamples';
end;

Class Function TUrlcrawlerrorssamplesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TwebmastersAPI;
end;

Function TUrlcrawlerrorssamplesResource.Get(siteUrl: string; url: string; AQuery : string = '') : TUrlCrawlErrorsSample;

Const
  _HTTPMethod = 'GET';
  _Path       = 'sites/{siteUrl}/urlCrawlErrorsSamples/{url}';
  _Methodid   = 'webmasters.urlcrawlerrorssamples.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['siteUrl',siteUrl,'url',url]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TUrlCrawlErrorsSample) as TUrlCrawlErrorsSample;
end;


Function TUrlcrawlerrorssamplesResource.Get(siteUrl: string; url: string; AQuery : TUrlcrawlerrorssamplesgetOptions) : TUrlCrawlErrorsSample;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'category',AQuery.category);
  AddToQuery(_Q,'platform',AQuery.platform);
  Result:=Get(siteUrl,url,_Q);
end;

Function TUrlcrawlerrorssamplesResource.List(siteUrl: string; AQuery : string = '') : TUrlCrawlErrorsSamplesListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'sites/{siteUrl}/urlCrawlErrorsSamples';
  _Methodid   = 'webmasters.urlcrawlerrorssamples.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['siteUrl',siteUrl]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TUrlCrawlErrorsSamplesListResponse) as TUrlCrawlErrorsSamplesListResponse;
end;


Function TUrlcrawlerrorssamplesResource.List(siteUrl: string; AQuery : TUrlcrawlerrorssampleslistOptions) : TUrlCrawlErrorsSamplesListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'category',AQuery.category);
  AddToQuery(_Q,'platform',AQuery.platform);
  Result:=List(siteUrl,_Q);
end;

Procedure TUrlcrawlerrorssamplesResource.MarkAsFixed(siteUrl: string; url: string; AQuery : string = '');

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'sites/{siteUrl}/urlCrawlErrorsSamples/{url}';
  _Methodid   = 'webmasters.urlcrawlerrorssamples.markAsFixed';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['siteUrl',siteUrl,'url',url]);
  ServiceCall(_HTTPMethod,_P,AQuery,Nil,Nil);
end;


Procedure TUrlcrawlerrorssamplesResource.MarkAsFixed(siteUrl: string; url: string; AQuery : TUrlcrawlerrorssamplesmarkAsFixedOptions);

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'category',AQuery.category);
  AddToQuery(_Q,'platform',AQuery.platform);
  MarkAsFixed(siteUrl,url,_Q);
end;



{ --------------------------------------------------------------------
  TWebmastersAPI
  --------------------------------------------------------------------}

Class Function TWebmastersAPI.APIName : String;

begin
  Result:='webmasters';
end;

Class Function TWebmastersAPI.APIVersion : String;

begin
  Result:='v3';
end;

Class Function TWebmastersAPI.APIRevision : String;

begin
  Result:='20160317';
end;

Class Function TWebmastersAPI.APIID : String;

begin
  Result:='webmasters:v3';
end;

Class Function TWebmastersAPI.APITitle : String;

begin
  Result:='Search Console API';
end;

Class Function TWebmastersAPI.APIDescription : String;

begin
  Result:='View Google Search Console data for your verified sites.';
end;

Class Function TWebmastersAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TWebmastersAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TWebmastersAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/webmaster_tools-16.png';
end;

Class Function TWebmastersAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/webmaster_tools-32.png';
end;

Class Function TWebmastersAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/webmaster-tools/';
end;

Class Function TWebmastersAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TWebmastersAPI.APIbasePath : string;

begin
  Result:='/webmasters/v3/';
end;

Class Function TWebmastersAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/webmasters/v3/';
end;

Class Function TWebmastersAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TWebmastersAPI.APIservicePath : string;

begin
  Result:='webmasters/v3/';
end;

Class Function TWebmastersAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TWebmastersAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/webmasters';
  Result[0].Description:='View and manage Search Console data for your verified sites';
  Result[1].Name:='https://www.googleapis.com/auth/webmasters.readonly';
  Result[1].Description:='View Search Console data for your verified sites';
  
end;

Class Function TWebmastersAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TWebmastersAPI.RegisterAPIResources;

begin
  TApiDataRow.RegisterObject;
  TApiDimensionFilter.RegisterObject;
  TApiDimensionFilterGroup.RegisterObject;
  TSearchAnalyticsQueryRequest.RegisterObject;
  TSearchAnalyticsQueryResponse.RegisterObject;
  TSitemapsListResponse.RegisterObject;
  TSitesListResponse.RegisterObject;
  TUrlCrawlErrorCount.RegisterObject;
  TUrlCrawlErrorCountsPerType.RegisterObject;
  TUrlCrawlErrorsCountsQueryResponse.RegisterObject;
  TUrlCrawlErrorsSample.RegisterObject;
  TUrlCrawlErrorsSamplesListResponse.RegisterObject;
  TUrlSampleDetails.RegisterObject;
  TWmxSite.RegisterObject;
  TWmxSitemap.RegisterObject;
  TWmxSitemapContent.RegisterObject;
end;


Function TWebmastersAPI.GetSearchanalyticsInstance : TSearchanalyticsResource;

begin
  if (FSearchanalyticsInstance=Nil) then
    FSearchanalyticsInstance:=CreateSearchanalyticsResource;
  Result:=FSearchanalyticsInstance;
end;

Function TWebmastersAPI.CreateSearchanalyticsResource : TSearchanalyticsResource;

begin
  Result:=CreateSearchanalyticsResource(Self);
end;


Function TWebmastersAPI.CreateSearchanalyticsResource(AOwner : TComponent) : TSearchanalyticsResource;

begin
  Result:=TSearchanalyticsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TWebmastersAPI.GetSitemapsInstance : TSitemapsResource;

begin
  if (FSitemapsInstance=Nil) then
    FSitemapsInstance:=CreateSitemapsResource;
  Result:=FSitemapsInstance;
end;

Function TWebmastersAPI.CreateSitemapsResource : TSitemapsResource;

begin
  Result:=CreateSitemapsResource(Self);
end;


Function TWebmastersAPI.CreateSitemapsResource(AOwner : TComponent) : TSitemapsResource;

begin
  Result:=TSitemapsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TWebmastersAPI.GetSitesInstance : TSitesResource;

begin
  if (FSitesInstance=Nil) then
    FSitesInstance:=CreateSitesResource;
  Result:=FSitesInstance;
end;

Function TWebmastersAPI.CreateSitesResource : TSitesResource;

begin
  Result:=CreateSitesResource(Self);
end;


Function TWebmastersAPI.CreateSitesResource(AOwner : TComponent) : TSitesResource;

begin
  Result:=TSitesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TWebmastersAPI.GetUrlcrawlerrorscountsInstance : TUrlcrawlerrorscountsResource;

begin
  if (FUrlcrawlerrorscountsInstance=Nil) then
    FUrlcrawlerrorscountsInstance:=CreateUrlcrawlerrorscountsResource;
  Result:=FUrlcrawlerrorscountsInstance;
end;

Function TWebmastersAPI.CreateUrlcrawlerrorscountsResource : TUrlcrawlerrorscountsResource;

begin
  Result:=CreateUrlcrawlerrorscountsResource(Self);
end;


Function TWebmastersAPI.CreateUrlcrawlerrorscountsResource(AOwner : TComponent) : TUrlcrawlerrorscountsResource;

begin
  Result:=TUrlcrawlerrorscountsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TWebmastersAPI.GetUrlcrawlerrorssamplesInstance : TUrlcrawlerrorssamplesResource;

begin
  if (FUrlcrawlerrorssamplesInstance=Nil) then
    FUrlcrawlerrorssamplesInstance:=CreateUrlcrawlerrorssamplesResource;
  Result:=FUrlcrawlerrorssamplesInstance;
end;

Function TWebmastersAPI.CreateUrlcrawlerrorssamplesResource : TUrlcrawlerrorssamplesResource;

begin
  Result:=CreateUrlcrawlerrorssamplesResource(Self);
end;


Function TWebmastersAPI.CreateUrlcrawlerrorssamplesResource(AOwner : TComponent) : TUrlcrawlerrorssamplesResource;

begin
  Result:=TUrlcrawlerrorssamplesResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TWebmastersAPI.RegisterAPI;
end.
