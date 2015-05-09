unit googlewebmasters;
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
  TSitemapsListResponse = class;
  TSitesListResponse = class;
  TUrlCrawlErrorCount = class;
  TUrlCrawlErrorCountsPerType = class;
  TUrlCrawlErrorsCountsQueryResponse = class;
  TUrlCrawlErrorsSample = class;
  TUrlCrawlErrorsSamplesListResponse = class;
  TUrlSampleDetails = class;
  TWmxSite = class;
  TWmxSitemap = class;
  TWmxSitemapContent = class;
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
  TSitemapsListResponseTypesitemapArray = Array of TWmxSitemap;
  TSitesListResponseTypesiteEntryArray = Array of TWmxSite;
  TUrlCrawlErrorCountsPerTypeTypeentriesArray = Array of TUrlCrawlErrorCount;
  TUrlCrawlErrorsCountsQueryResponseTypecountPerTypesArray = Array of TUrlCrawlErrorCountsPerType;
  TUrlCrawlErrorsSamplesListResponseTypeurlCrawlErrorSampleArray = Array of TUrlCrawlErrorsSample;
  TWmxSitemapTypecontentsArray = Array of TWmxSitemapContent;
  
  { --------------------------------------------------------------------
    TSitemapsListResponse
    --------------------------------------------------------------------}
  
  TSitemapsListResponse = Class(TGoogleBaseObject)
  Private
    Fsitemap : TSitemapsListResponseTypesitemapArray;
  Protected
    //Property setters
    Procedure Setsitemap(AIndex : Integer; AValue : TSitemapsListResponseTypesitemapArray); virtual;
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
    Procedure SetsiteEntry(AIndex : Integer; AValue : TSitesListResponseTypesiteEntryArray); virtual;
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
    Procedure Setcount(AIndex : Integer; AValue : String); virtual;
    Procedure Settimestamp(AIndex : Integer; AValue : TDatetime); virtual;
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
    Procedure Setcategory(AIndex : Integer; AValue : String); virtual;
    Procedure Setentries(AIndex : Integer; AValue : TUrlCrawlErrorCountsPerTypeTypeentriesArray); virtual;
    Procedure Setplatform(AIndex : Integer; AValue : String); virtual;
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
    Procedure SetcountPerTypes(AIndex : Integer; AValue : TUrlCrawlErrorsCountsQueryResponseTypecountPerTypesArray); virtual;
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
    Procedure Setfirst_detected(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setlast_crawled(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetpageUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SetresponseCode(AIndex : Integer; AValue : integer); virtual;
    Procedure SeturlDetails(AIndex : Integer; AValue : TUrlSampleDetails); virtual;
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
    Procedure SeturlCrawlErrorSample(AIndex : Integer; AValue : TUrlCrawlErrorsSamplesListResponseTypeurlCrawlErrorSampleArray); virtual;
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
    Procedure SetcontainingSitemaps(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetlinkedFromUrls(AIndex : Integer; AValue : TStringArray); virtual;
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
    Procedure SetpermissionLevel(AIndex : Integer; AValue : String); virtual;
    Procedure SetsiteUrl(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setcontents(AIndex : Integer; AValue : TWmxSitemapTypecontentsArray); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : String); virtual;
    Procedure SetisPending(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetisSitemapsIndex(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetlastDownloaded(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetlastSubmitted(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure Setpath(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : String); virtual;
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
    Procedure Setindexed(AIndex : Integer; AValue : String); virtual;
    Procedure Setsubmitted(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property indexed : String Index 0 Read Findexed Write Setindexed;
    Property submitted : String Index 8 Read Fsubmitted Write Setsubmitted;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TWmxSitemapContentClass = Class of TWmxSitemapContent;
  
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
    FSitemapsInstance : TSitemapsResource;
    FSitesInstance : TSitesResource;
    FUrlcrawlerrorscountsInstance : TUrlcrawlerrorscountsResource;
    FUrlcrawlerrorssamplesInstance : TUrlcrawlerrorssamplesResource;
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
    Function CreateSitemapsResource(AOwner : TComponent) : TSitemapsResource;virtual;overload;
    Function CreateSitemapsResource : TSitemapsResource;virtual;overload;
    Function CreateSitesResource(AOwner : TComponent) : TSitesResource;virtual;overload;
    Function CreateSitesResource : TSitesResource;virtual;overload;
    Function CreateUrlcrawlerrorscountsResource(AOwner : TComponent) : TUrlcrawlerrorscountsResource;virtual;overload;
    Function CreateUrlcrawlerrorscountsResource : TUrlcrawlerrorscountsResource;virtual;overload;
    Function CreateUrlcrawlerrorssamplesResource(AOwner : TComponent) : TUrlcrawlerrorssamplesResource;virtual;overload;
    Function CreateUrlcrawlerrorssamplesResource : TUrlcrawlerrorssamplesResource;virtual;overload;
    //Add default on-demand instances for resources
    Property SitemapsResource : TSitemapsResource Read GetSitemapsInstance;
    Property SitesResource : TSitesResource Read GetSitesInstance;
    Property UrlcrawlerrorscountsResource : TUrlcrawlerrorscountsResource Read GetUrlcrawlerrorscountsInstance;
    Property UrlcrawlerrorssamplesResource : TUrlcrawlerrorssamplesResource Read GetUrlcrawlerrorssamplesInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TSitemapsListResponse
  --------------------------------------------------------------------}


Procedure TSitemapsListResponse.Setsitemap(AIndex : Integer; AValue : TSitemapsListResponseTypesitemapArray); 

begin
  If (Fsitemap=AValue) then exit;
  Fsitemap:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSitesListResponse
  --------------------------------------------------------------------}


Procedure TSitesListResponse.SetsiteEntry(AIndex : Integer; AValue : TSitesListResponseTypesiteEntryArray); 

begin
  If (FsiteEntry=AValue) then exit;
  FsiteEntry:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlCrawlErrorCount
  --------------------------------------------------------------------}


Procedure TUrlCrawlErrorCount.Setcount(AIndex : Integer; AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlCrawlErrorCount.Settimestamp(AIndex : Integer; AValue : TDatetime); 

begin
  If (Ftimestamp=AValue) then exit;
  Ftimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlCrawlErrorCountsPerType
  --------------------------------------------------------------------}


Procedure TUrlCrawlErrorCountsPerType.Setcategory(AIndex : Integer; AValue : String); 

begin
  If (Fcategory=AValue) then exit;
  Fcategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlCrawlErrorCountsPerType.Setentries(AIndex : Integer; AValue : TUrlCrawlErrorCountsPerTypeTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlCrawlErrorCountsPerType.Setplatform(AIndex : Integer; AValue : String); 

begin
  If (Fplatform=AValue) then exit;
  Fplatform:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlCrawlErrorsCountsQueryResponse
  --------------------------------------------------------------------}


Procedure TUrlCrawlErrorsCountsQueryResponse.SetcountPerTypes(AIndex : Integer; AValue : TUrlCrawlErrorsCountsQueryResponseTypecountPerTypesArray); 

begin
  If (FcountPerTypes=AValue) then exit;
  FcountPerTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlCrawlErrorsSample
  --------------------------------------------------------------------}


Procedure TUrlCrawlErrorsSample.Setfirst_detected(AIndex : Integer; AValue : TDatetime); 

begin
  If (Ffirst_detected=AValue) then exit;
  Ffirst_detected:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlCrawlErrorsSample.Setlast_crawled(AIndex : Integer; AValue : TDatetime); 

begin
  If (Flast_crawled=AValue) then exit;
  Flast_crawled:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlCrawlErrorsSample.SetpageUrl(AIndex : Integer; AValue : String); 

begin
  If (FpageUrl=AValue) then exit;
  FpageUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlCrawlErrorsSample.SetresponseCode(AIndex : Integer; AValue : integer); 

begin
  If (FresponseCode=AValue) then exit;
  FresponseCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlCrawlErrorsSample.SeturlDetails(AIndex : Integer; AValue : TUrlSampleDetails); 

begin
  If (FurlDetails=AValue) then exit;
  FurlDetails:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlCrawlErrorsSamplesListResponse
  --------------------------------------------------------------------}


Procedure TUrlCrawlErrorsSamplesListResponse.SeturlCrawlErrorSample(AIndex : Integer; AValue : TUrlCrawlErrorsSamplesListResponseTypeurlCrawlErrorSampleArray); 

begin
  If (FurlCrawlErrorSample=AValue) then exit;
  FurlCrawlErrorSample:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlSampleDetails
  --------------------------------------------------------------------}


Procedure TUrlSampleDetails.SetcontainingSitemaps(AIndex : Integer; AValue : TStringArray); 

begin
  If (FcontainingSitemaps=AValue) then exit;
  FcontainingSitemaps:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlSampleDetails.SetlinkedFromUrls(AIndex : Integer; AValue : TStringArray); 

begin
  If (FlinkedFromUrls=AValue) then exit;
  FlinkedFromUrls:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWmxSite
  --------------------------------------------------------------------}


Procedure TWmxSite.SetpermissionLevel(AIndex : Integer; AValue : String); 

begin
  If (FpermissionLevel=AValue) then exit;
  FpermissionLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSite.SetsiteUrl(AIndex : Integer; AValue : String); 

begin
  If (FsiteUrl=AValue) then exit;
  FsiteUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TWmxSitemap
  --------------------------------------------------------------------}


Procedure TWmxSitemap.Setcontents(AIndex : Integer; AValue : TWmxSitemapTypecontentsArray); 

begin
  If (Fcontents=AValue) then exit;
  Fcontents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemap.Seterrors(AIndex : Integer; AValue : String); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemap.SetisPending(AIndex : Integer; AValue : boolean); 

begin
  If (FisPending=AValue) then exit;
  FisPending:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemap.SetisSitemapsIndex(AIndex : Integer; AValue : boolean); 

begin
  If (FisSitemapsIndex=AValue) then exit;
  FisSitemapsIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemap.SetlastDownloaded(AIndex : Integer; AValue : TDatetime); 

begin
  If (FlastDownloaded=AValue) then exit;
  FlastDownloaded:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemap.SetlastSubmitted(AIndex : Integer; AValue : TDatetime); 

begin
  If (FlastSubmitted=AValue) then exit;
  FlastSubmitted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemap.Setpath(AIndex : Integer; AValue : String); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemap.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemap.Setwarnings(AIndex : Integer; AValue : String); 

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




{ --------------------------------------------------------------------
  TWmxSitemapContent
  --------------------------------------------------------------------}


Procedure TWmxSitemapContent.Setindexed(AIndex : Integer; AValue : String); 

begin
  If (Findexed=AValue) then exit;
  Findexed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemapContent.Setsubmitted(AIndex : Integer; AValue : String); 

begin
  If (Fsubmitted=AValue) then exit;
  Fsubmitted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TWmxSitemapContent.Set_type(AIndex : Integer; AValue : String); 

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
  Result:='20140908';
end;

Class Function TWebmastersAPI.APIID : String;

begin
  Result:='webmasters:v3';
end;

Class Function TWebmastersAPI.APITitle : String;

begin
  Result:='Webmaster Tools API';
end;

Class Function TWebmastersAPI.APIDescription : String;

begin
  Result:='Lets you view Google Webmaster Tools data for your verified sites.';
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
  Result:='https://developers.google.com/webmaster-tools/v3/welcome';
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
  Result[0].Description:='View and modify Webmaster Tools data for your verified sites';
  Result[1].Name:='https://www.googleapis.com/auth/webmasters.readonly';
  Result[1].Description:='View Webmaster Tools data for your verified sites';
  
end;

Class Function TWebmastersAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TWebmastersAPI.RegisterAPIResources;

begin
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
end;



initialization
  TWebmastersAPI.RegisterAPI;
end.
