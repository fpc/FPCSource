unit googlecustomsearch;
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
//Generated on: 16-5-15 08:53:01
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TContext = Class;
  TPromotion = Class;
  TQuery = Class;
  TResult = Class;
  TSearch = Class;
  TContextArray = Array of TContext;
  TPromotionArray = Array of TPromotion;
  TQueryArray = Array of TQuery;
  TResultArray = Array of TResult;
  TSearchArray = Array of TSearch;
  //Anonymous types, using auto-generated names
  TContextTypefacetsItemItem = Class;
  TPromotionTypebodyLinesItem = Class;
  TPromotionTypeimage = Class;
  TResultTypeimage = Class;
  TResultTypelabelsItem = Class;
  TResultTypepagemap = Class;
  TSearchTypequeries = Class;
  TSearchTypesearchInformation = Class;
  TSearchTypespelling = Class;
  TSearchTypeurl = Class;
  TContextTypefacetsItemArray = Array of TContextTypefacetsItemItem;
  TContextTypefacetsArray = Array of TContextTypefacetsItemArray;
  TPromotionTypebodyLinesArray = Array of TPromotionTypebodyLinesItem;
  TResultTypelabelsArray = Array of TResultTypelabelsItem;
  TSearchTypeitemsArray = Array of TResult;
  TSearchTypepromotionsArray = Array of TPromotion;
  
  { --------------------------------------------------------------------
    TContextTypefacetsItemItem
    --------------------------------------------------------------------}
  
  TContextTypefacetsItemItem = Class(TGoogleBaseObject)
  Private
    Fanchor : String;
    F_label : String;
    Flabel_with_op : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setanchor(AIndex : Integer; AValue : String); virtual;
    Procedure Set_label(AIndex : Integer; AValue : String); virtual;
    Procedure Setlabel_with_op(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property anchor : String Index 0 Read Fanchor Write Setanchor;
    Property _label : String Index 8 Read F_label Write Set_label;
    Property label_with_op : String Index 16 Read Flabel_with_op Write Setlabel_with_op;
  end;
  TContextTypefacetsItemItemClass = Class of TContextTypefacetsItemItem;
  
  { --------------------------------------------------------------------
    TContext
    --------------------------------------------------------------------}
  
  TContext = Class(TGoogleBaseObject)
  Private
    Ffacets : TContextTypefacetsArray;
    Ftitle : String;
  Protected
    //Property setters
    Procedure Setfacets(AIndex : Integer; AValue : TContextTypefacetsArray); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property facets : TContextTypefacetsArray Index 0 Read Ffacets Write Setfacets;
    Property title : String Index 8 Read Ftitle Write Settitle;
  end;
  TContextClass = Class of TContext;
  
  { --------------------------------------------------------------------
    TPromotionTypebodyLinesItem
    --------------------------------------------------------------------}
  
  TPromotionTypebodyLinesItem = Class(TGoogleBaseObject)
  Private
    FhtmlTitle : String;
    Flink : String;
    Ftitle : String;
    Furl : String;
  Protected
    //Property setters
    Procedure SethtmlTitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setlink(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property htmlTitle : String Index 0 Read FhtmlTitle Write SethtmlTitle;
    Property link : String Index 8 Read Flink Write Setlink;
    Property title : String Index 16 Read Ftitle Write Settitle;
    Property url : String Index 24 Read Furl Write Seturl;
  end;
  TPromotionTypebodyLinesItemClass = Class of TPromotionTypebodyLinesItem;
  
  { --------------------------------------------------------------------
    TPromotionTypeimage
    --------------------------------------------------------------------}
  
  TPromotionTypeimage = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    Fsource : String;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Setsource(AIndex : Integer; AValue : String); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property source : String Index 8 Read Fsource Write Setsource;
    Property width : integer Index 16 Read Fwidth Write Setwidth;
  end;
  TPromotionTypeimageClass = Class of TPromotionTypeimage;
  
  { --------------------------------------------------------------------
    TPromotion
    --------------------------------------------------------------------}
  
  TPromotion = Class(TGoogleBaseObject)
  Private
    FbodyLines : TPromotionTypebodyLinesArray;
    FdisplayLink : String;
    FhtmlTitle : String;
    Fimage : TPromotionTypeimage;
    Flink : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetbodyLines(AIndex : Integer; AValue : TPromotionTypebodyLinesArray); virtual;
    Procedure SetdisplayLink(AIndex : Integer; AValue : String); virtual;
    Procedure SethtmlTitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TPromotionTypeimage); virtual;
    Procedure Setlink(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property bodyLines : TPromotionTypebodyLinesArray Index 0 Read FbodyLines Write SetbodyLines;
    Property displayLink : String Index 8 Read FdisplayLink Write SetdisplayLink;
    Property htmlTitle : String Index 16 Read FhtmlTitle Write SethtmlTitle;
    Property image : TPromotionTypeimage Index 24 Read Fimage Write Setimage;
    Property link : String Index 32 Read Flink Write Setlink;
    Property title : String Index 40 Read Ftitle Write Settitle;
  end;
  TPromotionClass = Class of TPromotion;
  
  { --------------------------------------------------------------------
    TQuery
    --------------------------------------------------------------------}
  
  TQuery = Class(TGoogleBaseObject)
  Private
    Fcount : integer;
    Fcr : String;
    Fcref : String;
    Fcx : String;
    FdateRestrict : String;
    FdisableCnTwTranslation : String;
    FexactTerms : String;
    FexcludeTerms : String;
    FfileType : String;
    Ffilter : String;
    Fgl : String;
    FgoogleHost : String;
    FhighRange : String;
    Fhl : String;
    Fhq : String;
    FimgColorType : String;
    FimgDominantColor : String;
    FimgSize : String;
    FimgType : String;
    FinputEncoding : String;
    Flanguage : String;
    FlinkSite : String;
    FlowRange : String;
    ForTerms : String;
    FoutputEncoding : String;
    FrelatedSite : String;
    Frights : String;
    Fsafe : String;
    FsearchTerms : String;
    FsearchType : String;
    FsiteSearch : String;
    FsiteSearchFilter : String;
    Fsort : String;
    FstartIndex : integer;
    FstartPage : integer;
    Ftitle : String;
    FtotalResults : String;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setcr(AIndex : Integer; AValue : String); virtual;
    Procedure Setcref(AIndex : Integer; AValue : String); virtual;
    Procedure Setcx(AIndex : Integer; AValue : String); virtual;
    Procedure SetdateRestrict(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisableCnTwTranslation(AIndex : Integer; AValue : String); virtual;
    Procedure SetexactTerms(AIndex : Integer; AValue : String); virtual;
    Procedure SetexcludeTerms(AIndex : Integer; AValue : String); virtual;
    Procedure SetfileType(AIndex : Integer; AValue : String); virtual;
    Procedure Setfilter(AIndex : Integer; AValue : String); virtual;
    Procedure Setgl(AIndex : Integer; AValue : String); virtual;
    Procedure SetgoogleHost(AIndex : Integer; AValue : String); virtual;
    Procedure SethighRange(AIndex : Integer; AValue : String); virtual;
    Procedure Sethl(AIndex : Integer; AValue : String); virtual;
    Procedure Sethq(AIndex : Integer; AValue : String); virtual;
    Procedure SetimgColorType(AIndex : Integer; AValue : String); virtual;
    Procedure SetimgDominantColor(AIndex : Integer; AValue : String); virtual;
    Procedure SetimgSize(AIndex : Integer; AValue : String); virtual;
    Procedure SetimgType(AIndex : Integer; AValue : String); virtual;
    Procedure SetinputEncoding(AIndex : Integer; AValue : String); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : String); virtual;
    Procedure SetlinkSite(AIndex : Integer; AValue : String); virtual;
    Procedure SetlowRange(AIndex : Integer; AValue : String); virtual;
    Procedure SetorTerms(AIndex : Integer; AValue : String); virtual;
    Procedure SetoutputEncoding(AIndex : Integer; AValue : String); virtual;
    Procedure SetrelatedSite(AIndex : Integer; AValue : String); virtual;
    Procedure Setrights(AIndex : Integer; AValue : String); virtual;
    Procedure Setsafe(AIndex : Integer; AValue : String); virtual;
    Procedure SetsearchTerms(AIndex : Integer; AValue : String); virtual;
    Procedure SetsearchType(AIndex : Integer; AValue : String); virtual;
    Procedure SetsiteSearch(AIndex : Integer; AValue : String); virtual;
    Procedure SetsiteSearchFilter(AIndex : Integer; AValue : String); virtual;
    Procedure Setsort(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SetstartPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property count : integer Index 0 Read Fcount Write Setcount;
    Property cr : String Index 8 Read Fcr Write Setcr;
    Property cref : String Index 16 Read Fcref Write Setcref;
    Property cx : String Index 24 Read Fcx Write Setcx;
    Property dateRestrict : String Index 32 Read FdateRestrict Write SetdateRestrict;
    Property disableCnTwTranslation : String Index 40 Read FdisableCnTwTranslation Write SetdisableCnTwTranslation;
    Property exactTerms : String Index 48 Read FexactTerms Write SetexactTerms;
    Property excludeTerms : String Index 56 Read FexcludeTerms Write SetexcludeTerms;
    Property fileType : String Index 64 Read FfileType Write SetfileType;
    Property filter : String Index 72 Read Ffilter Write Setfilter;
    Property gl : String Index 80 Read Fgl Write Setgl;
    Property googleHost : String Index 88 Read FgoogleHost Write SetgoogleHost;
    Property highRange : String Index 96 Read FhighRange Write SethighRange;
    Property hl : String Index 104 Read Fhl Write Sethl;
    Property hq : String Index 112 Read Fhq Write Sethq;
    Property imgColorType : String Index 120 Read FimgColorType Write SetimgColorType;
    Property imgDominantColor : String Index 128 Read FimgDominantColor Write SetimgDominantColor;
    Property imgSize : String Index 136 Read FimgSize Write SetimgSize;
    Property imgType : String Index 144 Read FimgType Write SetimgType;
    Property inputEncoding : String Index 152 Read FinputEncoding Write SetinputEncoding;
    Property language : String Index 160 Read Flanguage Write Setlanguage;
    Property linkSite : String Index 168 Read FlinkSite Write SetlinkSite;
    Property lowRange : String Index 176 Read FlowRange Write SetlowRange;
    Property orTerms : String Index 184 Read ForTerms Write SetorTerms;
    Property outputEncoding : String Index 192 Read FoutputEncoding Write SetoutputEncoding;
    Property relatedSite : String Index 200 Read FrelatedSite Write SetrelatedSite;
    Property rights : String Index 208 Read Frights Write Setrights;
    Property safe : String Index 216 Read Fsafe Write Setsafe;
    Property searchTerms : String Index 224 Read FsearchTerms Write SetsearchTerms;
    Property searchType : String Index 232 Read FsearchType Write SetsearchType;
    Property siteSearch : String Index 240 Read FsiteSearch Write SetsiteSearch;
    Property siteSearchFilter : String Index 248 Read FsiteSearchFilter Write SetsiteSearchFilter;
    Property sort : String Index 256 Read Fsort Write Setsort;
    Property startIndex : integer Index 264 Read FstartIndex Write SetstartIndex;
    Property startPage : integer Index 272 Read FstartPage Write SetstartPage;
    Property title : String Index 280 Read Ftitle Write Settitle;
    Property totalResults : String Index 288 Read FtotalResults Write SettotalResults;
  end;
  TQueryClass = Class of TQuery;
  
  { --------------------------------------------------------------------
    TResultTypeimage
    --------------------------------------------------------------------}
  
  TResultTypeimage = Class(TGoogleBaseObject)
  Private
    FbyteSize : integer;
    FcontextLink : String;
    Fheight : integer;
    FthumbnailHeight : integer;
    FthumbnailLink : String;
    FthumbnailWidth : integer;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure SetbyteSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetcontextLink(AIndex : Integer; AValue : String); virtual;
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure SetthumbnailHeight(AIndex : Integer; AValue : integer); virtual;
    Procedure SetthumbnailLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetthumbnailWidth(AIndex : Integer; AValue : integer); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property byteSize : integer Index 0 Read FbyteSize Write SetbyteSize;
    Property contextLink : String Index 8 Read FcontextLink Write SetcontextLink;
    Property height : integer Index 16 Read Fheight Write Setheight;
    Property thumbnailHeight : integer Index 24 Read FthumbnailHeight Write SetthumbnailHeight;
    Property thumbnailLink : String Index 32 Read FthumbnailLink Write SetthumbnailLink;
    Property thumbnailWidth : integer Index 40 Read FthumbnailWidth Write SetthumbnailWidth;
    Property width : integer Index 48 Read Fwidth Write Setwidth;
  end;
  TResultTypeimageClass = Class of TResultTypeimage;
  
  { --------------------------------------------------------------------
    TResultTypelabelsItem
    --------------------------------------------------------------------}
  
  TResultTypelabelsItem = Class(TGoogleBaseObject)
  Private
    FdisplayName : String;
    Flabel_with_op : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : String); virtual;
    Procedure Setlabel_with_op(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property displayName : String Index 0 Read FdisplayName Write SetdisplayName;
    Property label_with_op : String Index 8 Read Flabel_with_op Write Setlabel_with_op;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TResultTypelabelsItemClass = Class of TResultTypelabelsItem;
  
  { --------------------------------------------------------------------
    TResultTypepagemap
    --------------------------------------------------------------------}
  
  TResultTypepagemap = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TResultTypepagemapClass = Class of TResultTypepagemap;
  
  { --------------------------------------------------------------------
    TResult
    --------------------------------------------------------------------}
  
  TResult = Class(TGoogleBaseObject)
  Private
    FcacheId : String;
    FdisplayLink : String;
    FfileFormat : String;
    FformattedUrl : String;
    FhtmlFormattedUrl : String;
    FhtmlSnippet : String;
    FhtmlTitle : String;
    Fimage : TResultTypeimage;
    Fkind : String;
    Flabels : TResultTypelabelsArray;
    Flink : String;
    Fmime : String;
    Fpagemap : TResultTypepagemap;
    Fsnippet : String;
    Ftitle : String;
  Protected
    //Property setters
    Procedure SetcacheId(AIndex : Integer; AValue : String); virtual;
    Procedure SetdisplayLink(AIndex : Integer; AValue : String); virtual;
    Procedure SetfileFormat(AIndex : Integer; AValue : String); virtual;
    Procedure SetformattedUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SethtmlFormattedUrl(AIndex : Integer; AValue : String); virtual;
    Procedure SethtmlSnippet(AIndex : Integer; AValue : String); virtual;
    Procedure SethtmlTitle(AIndex : Integer; AValue : String); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TResultTypeimage); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setlabels(AIndex : Integer; AValue : TResultTypelabelsArray); virtual;
    Procedure Setlink(AIndex : Integer; AValue : String); virtual;
    Procedure Setmime(AIndex : Integer; AValue : String); virtual;
    Procedure Setpagemap(AIndex : Integer; AValue : TResultTypepagemap); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property cacheId : String Index 0 Read FcacheId Write SetcacheId;
    Property displayLink : String Index 8 Read FdisplayLink Write SetdisplayLink;
    Property fileFormat : String Index 16 Read FfileFormat Write SetfileFormat;
    Property formattedUrl : String Index 24 Read FformattedUrl Write SetformattedUrl;
    Property htmlFormattedUrl : String Index 32 Read FhtmlFormattedUrl Write SethtmlFormattedUrl;
    Property htmlSnippet : String Index 40 Read FhtmlSnippet Write SethtmlSnippet;
    Property htmlTitle : String Index 48 Read FhtmlTitle Write SethtmlTitle;
    Property image : TResultTypeimage Index 56 Read Fimage Write Setimage;
    Property kind : String Index 64 Read Fkind Write Setkind;
    Property labels : TResultTypelabelsArray Index 72 Read Flabels Write Setlabels;
    Property link : String Index 80 Read Flink Write Setlink;
    Property mime : String Index 88 Read Fmime Write Setmime;
    Property pagemap : TResultTypepagemap Index 96 Read Fpagemap Write Setpagemap;
    Property snippet : String Index 104 Read Fsnippet Write Setsnippet;
    Property title : String Index 112 Read Ftitle Write Settitle;
  end;
  TResultClass = Class of TResult;
  
  { --------------------------------------------------------------------
    TSearchTypequeries
    --------------------------------------------------------------------}
  
  TSearchTypequeries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSearchTypequeriesClass = Class of TSearchTypequeries;
  
  { --------------------------------------------------------------------
    TSearchTypesearchInformation
    --------------------------------------------------------------------}
  
  TSearchTypesearchInformation = Class(TGoogleBaseObject)
  Private
    FformattedSearchTime : String;
    FformattedTotalResults : String;
    FsearchTime : double;
    FtotalResults : String;
  Protected
    //Property setters
    Procedure SetformattedSearchTime(AIndex : Integer; AValue : String); virtual;
    Procedure SetformattedTotalResults(AIndex : Integer; AValue : String); virtual;
    Procedure SetsearchTime(AIndex : Integer; AValue : double); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property formattedSearchTime : String Index 0 Read FformattedSearchTime Write SetformattedSearchTime;
    Property formattedTotalResults : String Index 8 Read FformattedTotalResults Write SetformattedTotalResults;
    Property searchTime : double Index 16 Read FsearchTime Write SetsearchTime;
    Property totalResults : String Index 24 Read FtotalResults Write SettotalResults;
  end;
  TSearchTypesearchInformationClass = Class of TSearchTypesearchInformation;
  
  { --------------------------------------------------------------------
    TSearchTypespelling
    --------------------------------------------------------------------}
  
  TSearchTypespelling = Class(TGoogleBaseObject)
  Private
    FcorrectedQuery : String;
    FhtmlCorrectedQuery : String;
  Protected
    //Property setters
    Procedure SetcorrectedQuery(AIndex : Integer; AValue : String); virtual;
    Procedure SethtmlCorrectedQuery(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property correctedQuery : String Index 0 Read FcorrectedQuery Write SetcorrectedQuery;
    Property htmlCorrectedQuery : String Index 8 Read FhtmlCorrectedQuery Write SethtmlCorrectedQuery;
  end;
  TSearchTypespellingClass = Class of TSearchTypespelling;
  
  { --------------------------------------------------------------------
    TSearchTypeurl
    --------------------------------------------------------------------}
  
  TSearchTypeurl = Class(TGoogleBaseObject)
  Private
    Ftemplate : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Settemplate(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property template : String Index 0 Read Ftemplate Write Settemplate;
    Property _type : String Index 8 Read F_type Write Set_type;
  end;
  TSearchTypeurlClass = Class of TSearchTypeurl;
  
  { --------------------------------------------------------------------
    TSearch
    --------------------------------------------------------------------}
  
  TSearch = Class(TGoogleBaseObject)
  Private
    Fcontext : TContext;
    Fitems : TSearchTypeitemsArray;
    Fkind : String;
    Fpromotions : TSearchTypepromotionsArray;
    Fqueries : TSearchTypequeries;
    FsearchInformation : TSearchTypesearchInformation;
    Fspelling : TSearchTypespelling;
    Furl : TSearchTypeurl;
  Protected
    //Property setters
    Procedure Setcontext(AIndex : Integer; AValue : TContext); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TSearchTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setpromotions(AIndex : Integer; AValue : TSearchTypepromotionsArray); virtual;
    Procedure Setqueries(AIndex : Integer; AValue : TSearchTypequeries); virtual;
    Procedure SetsearchInformation(AIndex : Integer; AValue : TSearchTypesearchInformation); virtual;
    Procedure Setspelling(AIndex : Integer; AValue : TSearchTypespelling); virtual;
    Procedure Seturl(AIndex : Integer; AValue : TSearchTypeurl); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property context : TContext Index 0 Read Fcontext Write Setcontext;
    Property items : TSearchTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property promotions : TSearchTypepromotionsArray Index 24 Read Fpromotions Write Setpromotions;
    Property queries : TSearchTypequeries Index 32 Read Fqueries Write Setqueries;
    Property searchInformation : TSearchTypesearchInformation Index 40 Read FsearchInformation Write SetsearchInformation;
    Property spelling : TSearchTypespelling Index 48 Read Fspelling Write Setspelling;
    Property url : TSearchTypeurl Index 56 Read Furl Write Seturl;
  end;
  TSearchClass = Class of TSearch;
  
  { --------------------------------------------------------------------
    TCseResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCseResource, method List
  
  TCseListOptions = Record
    c2coff : String;
    cr : String;
    cref : String;
    cx : String;
    dateRestrict : String;
    exactTerms : String;
    excludeTerms : String;
    fileType : String;
    filter : String;
    gl : String;
    googlehost : String;
    highRange : String;
    hl : String;
    hq : String;
    imgColorType : String;
    imgDominantColor : String;
    imgSize : String;
    imgType : String;
    linkSite : String;
    lowRange : String;
    lr : String;
    num : integer;
    orTerms : String;
    q : String;
    relatedSite : String;
    rights : String;
    safe : String;
    searchType : String;
    siteSearch : String;
    siteSearchFilter : String;
    sort : String;
    start : integer;
  end;
  
  TCseResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TSearch;
    Function List(AQuery : TCselistOptions) : TSearch;
  end;
  
  
  { --------------------------------------------------------------------
    TCustomsearchAPI
    --------------------------------------------------------------------}
  
  TCustomsearchAPI = Class(TGoogleAPI)
  Private
    FCseInstance : TCseResource;
    Function GetCseInstance : TCseResource;virtual;
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
    Function CreateCseResource(AOwner : TComponent) : TCseResource;virtual;overload;
    Function CreateCseResource : TCseResource;virtual;overload;
    //Add default on-demand instances for resources
    Property CseResource : TCseResource Read GetCseInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TContextTypefacetsItemItem
  --------------------------------------------------------------------}


Procedure TContextTypefacetsItemItem.Setanchor(AIndex : Integer; AValue : String); 

begin
  If (Fanchor=AValue) then exit;
  Fanchor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContextTypefacetsItemItem.Set_label(AIndex : Integer; AValue : String); 

begin
  If (F_label=AValue) then exit;
  F_label:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContextTypefacetsItemItem.Setlabel_with_op(AIndex : Integer; AValue : String); 

begin
  If (Flabel_with_op=AValue) then exit;
  Flabel_with_op:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TContextTypefacetsItemItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_label' : Result:='label';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TContext
  --------------------------------------------------------------------}


Procedure TContext.Setfacets(AIndex : Integer; AValue : TContextTypefacetsArray); 

begin
  If (Ffacets=AValue) then exit;
  Ffacets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContext.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TContext.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'facets' : SetLength(Ffacets,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPromotionTypebodyLinesItem
  --------------------------------------------------------------------}


Procedure TPromotionTypebodyLinesItem.SethtmlTitle(AIndex : Integer; AValue : String); 

begin
  If (FhtmlTitle=AValue) then exit;
  FhtmlTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotionTypebodyLinesItem.Setlink(AIndex : Integer; AValue : String); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotionTypebodyLinesItem.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotionTypebodyLinesItem.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPromotionTypeimage
  --------------------------------------------------------------------}


Procedure TPromotionTypeimage.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotionTypeimage.Setsource(AIndex : Integer; AValue : String); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotionTypeimage.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPromotion
  --------------------------------------------------------------------}


Procedure TPromotion.SetbodyLines(AIndex : Integer; AValue : TPromotionTypebodyLinesArray); 

begin
  If (FbodyLines=AValue) then exit;
  FbodyLines:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotion.SetdisplayLink(AIndex : Integer; AValue : String); 

begin
  If (FdisplayLink=AValue) then exit;
  FdisplayLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotion.SethtmlTitle(AIndex : Integer; AValue : String); 

begin
  If (FhtmlTitle=AValue) then exit;
  FhtmlTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotion.Setimage(AIndex : Integer; AValue : TPromotionTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotion.Setlink(AIndex : Integer; AValue : String); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotion.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPromotion.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'bodylines' : SetLength(FbodyLines,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TQuery
  --------------------------------------------------------------------}


Procedure TQuery.Setcount(AIndex : Integer; AValue : integer); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setcr(AIndex : Integer; AValue : String); 

begin
  If (Fcr=AValue) then exit;
  Fcr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setcref(AIndex : Integer; AValue : String); 

begin
  If (Fcref=AValue) then exit;
  Fcref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setcx(AIndex : Integer; AValue : String); 

begin
  If (Fcx=AValue) then exit;
  Fcx:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetdateRestrict(AIndex : Integer; AValue : String); 

begin
  If (FdateRestrict=AValue) then exit;
  FdateRestrict:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetdisableCnTwTranslation(AIndex : Integer; AValue : String); 

begin
  If (FdisableCnTwTranslation=AValue) then exit;
  FdisableCnTwTranslation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetexactTerms(AIndex : Integer; AValue : String); 

begin
  If (FexactTerms=AValue) then exit;
  FexactTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetexcludeTerms(AIndex : Integer; AValue : String); 

begin
  If (FexcludeTerms=AValue) then exit;
  FexcludeTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetfileType(AIndex : Integer; AValue : String); 

begin
  If (FfileType=AValue) then exit;
  FfileType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setfilter(AIndex : Integer; AValue : String); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setgl(AIndex : Integer; AValue : String); 

begin
  If (Fgl=AValue) then exit;
  Fgl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetgoogleHost(AIndex : Integer; AValue : String); 

begin
  If (FgoogleHost=AValue) then exit;
  FgoogleHost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SethighRange(AIndex : Integer; AValue : String); 

begin
  If (FhighRange=AValue) then exit;
  FhighRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Sethl(AIndex : Integer; AValue : String); 

begin
  If (Fhl=AValue) then exit;
  Fhl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Sethq(AIndex : Integer; AValue : String); 

begin
  If (Fhq=AValue) then exit;
  Fhq:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetimgColorType(AIndex : Integer; AValue : String); 

begin
  If (FimgColorType=AValue) then exit;
  FimgColorType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetimgDominantColor(AIndex : Integer; AValue : String); 

begin
  If (FimgDominantColor=AValue) then exit;
  FimgDominantColor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetimgSize(AIndex : Integer; AValue : String); 

begin
  If (FimgSize=AValue) then exit;
  FimgSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetimgType(AIndex : Integer; AValue : String); 

begin
  If (FimgType=AValue) then exit;
  FimgType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetinputEncoding(AIndex : Integer; AValue : String); 

begin
  If (FinputEncoding=AValue) then exit;
  FinputEncoding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setlanguage(AIndex : Integer; AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetlinkSite(AIndex : Integer; AValue : String); 

begin
  If (FlinkSite=AValue) then exit;
  FlinkSite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetlowRange(AIndex : Integer; AValue : String); 

begin
  If (FlowRange=AValue) then exit;
  FlowRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetorTerms(AIndex : Integer; AValue : String); 

begin
  If (ForTerms=AValue) then exit;
  ForTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetoutputEncoding(AIndex : Integer; AValue : String); 

begin
  If (FoutputEncoding=AValue) then exit;
  FoutputEncoding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetrelatedSite(AIndex : Integer; AValue : String); 

begin
  If (FrelatedSite=AValue) then exit;
  FrelatedSite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setrights(AIndex : Integer; AValue : String); 

begin
  If (Frights=AValue) then exit;
  Frights:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setsafe(AIndex : Integer; AValue : String); 

begin
  If (Fsafe=AValue) then exit;
  Fsafe:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetsearchTerms(AIndex : Integer; AValue : String); 

begin
  If (FsearchTerms=AValue) then exit;
  FsearchTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetsearchType(AIndex : Integer; AValue : String); 

begin
  If (FsearchType=AValue) then exit;
  FsearchType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetsiteSearch(AIndex : Integer; AValue : String); 

begin
  If (FsiteSearch=AValue) then exit;
  FsiteSearch:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetsiteSearchFilter(AIndex : Integer; AValue : String); 

begin
  If (FsiteSearchFilter=AValue) then exit;
  FsiteSearchFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setsort(AIndex : Integer; AValue : String); 

begin
  If (Fsort=AValue) then exit;
  Fsort:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetstartIndex(AIndex : Integer; AValue : integer); 

begin
  If (FstartIndex=AValue) then exit;
  FstartIndex:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetstartPage(AIndex : Integer; AValue : integer); 

begin
  If (FstartPage=AValue) then exit;
  FstartPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SettotalResults(AIndex : Integer; AValue : String); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultTypeimage
  --------------------------------------------------------------------}


Procedure TResultTypeimage.SetbyteSize(AIndex : Integer; AValue : integer); 

begin
  If (FbyteSize=AValue) then exit;
  FbyteSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypeimage.SetcontextLink(AIndex : Integer; AValue : String); 

begin
  If (FcontextLink=AValue) then exit;
  FcontextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypeimage.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypeimage.SetthumbnailHeight(AIndex : Integer; AValue : integer); 

begin
  If (FthumbnailHeight=AValue) then exit;
  FthumbnailHeight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypeimage.SetthumbnailLink(AIndex : Integer; AValue : String); 

begin
  If (FthumbnailLink=AValue) then exit;
  FthumbnailLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypeimage.SetthumbnailWidth(AIndex : Integer; AValue : integer); 

begin
  If (FthumbnailWidth=AValue) then exit;
  FthumbnailWidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypeimage.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultTypelabelsItem
  --------------------------------------------------------------------}


Procedure TResultTypelabelsItem.SetdisplayName(AIndex : Integer; AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypelabelsItem.Setlabel_with_op(AIndex : Integer; AValue : String); 

begin
  If (Flabel_with_op=AValue) then exit;
  Flabel_with_op:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultTypelabelsItem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultTypepagemap
  --------------------------------------------------------------------}


Class Function TResultTypepagemap.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TResult
  --------------------------------------------------------------------}


Procedure TResult.SetcacheId(AIndex : Integer; AValue : String); 

begin
  If (FcacheId=AValue) then exit;
  FcacheId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SetdisplayLink(AIndex : Integer; AValue : String); 

begin
  If (FdisplayLink=AValue) then exit;
  FdisplayLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SetfileFormat(AIndex : Integer; AValue : String); 

begin
  If (FfileFormat=AValue) then exit;
  FfileFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SetformattedUrl(AIndex : Integer; AValue : String); 

begin
  If (FformattedUrl=AValue) then exit;
  FformattedUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SethtmlFormattedUrl(AIndex : Integer; AValue : String); 

begin
  If (FhtmlFormattedUrl=AValue) then exit;
  FhtmlFormattedUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SethtmlSnippet(AIndex : Integer; AValue : String); 

begin
  If (FhtmlSnippet=AValue) then exit;
  FhtmlSnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SethtmlTitle(AIndex : Integer; AValue : String); 

begin
  If (FhtmlTitle=AValue) then exit;
  FhtmlTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setimage(AIndex : Integer; AValue : TResultTypeimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setlabels(AIndex : Integer; AValue : TResultTypelabelsArray); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setlink(AIndex : Integer; AValue : String); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setmime(AIndex : Integer; AValue : String); 

begin
  If (Fmime=AValue) then exit;
  Fmime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setpagemap(AIndex : Integer; AValue : TResultTypepagemap); 

begin
  If (Fpagemap=AValue) then exit;
  Fpagemap:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setsnippet(AIndex : Integer; AValue : String); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TResult.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'labels' : SetLength(Flabels,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchTypequeries
  --------------------------------------------------------------------}


Class Function TSearchTypequeries.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSearchTypesearchInformation
  --------------------------------------------------------------------}


Procedure TSearchTypesearchInformation.SetformattedSearchTime(AIndex : Integer; AValue : String); 

begin
  If (FformattedSearchTime=AValue) then exit;
  FformattedSearchTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchTypesearchInformation.SetformattedTotalResults(AIndex : Integer; AValue : String); 

begin
  If (FformattedTotalResults=AValue) then exit;
  FformattedTotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchTypesearchInformation.SetsearchTime(AIndex : Integer; AValue : double); 

begin
  If (FsearchTime=AValue) then exit;
  FsearchTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchTypesearchInformation.SettotalResults(AIndex : Integer; AValue : String); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchTypespelling
  --------------------------------------------------------------------}


Procedure TSearchTypespelling.SetcorrectedQuery(AIndex : Integer; AValue : String); 

begin
  If (FcorrectedQuery=AValue) then exit;
  FcorrectedQuery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchTypespelling.SethtmlCorrectedQuery(AIndex : Integer; AValue : String); 

begin
  If (FhtmlCorrectedQuery=AValue) then exit;
  FhtmlCorrectedQuery:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchTypeurl
  --------------------------------------------------------------------}


Procedure TSearchTypeurl.Settemplate(AIndex : Integer; AValue : String); 

begin
  If (Ftemplate=AValue) then exit;
  Ftemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchTypeurl.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSearchTypeurl.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TSearch
  --------------------------------------------------------------------}


Procedure TSearch.Setcontext(AIndex : Integer; AValue : TContext); 

begin
  If (Fcontext=AValue) then exit;
  Fcontext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearch.Setitems(AIndex : Integer; AValue : TSearchTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearch.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearch.Setpromotions(AIndex : Integer; AValue : TSearchTypepromotionsArray); 

begin
  If (Fpromotions=AValue) then exit;
  Fpromotions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearch.Setqueries(AIndex : Integer; AValue : TSearchTypequeries); 

begin
  If (Fqueries=AValue) then exit;
  Fqueries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearch.SetsearchInformation(AIndex : Integer; AValue : TSearchTypesearchInformation); 

begin
  If (FsearchInformation=AValue) then exit;
  FsearchInformation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearch.Setspelling(AIndex : Integer; AValue : TSearchTypespelling); 

begin
  If (Fspelling=AValue) then exit;
  Fspelling:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearch.Seturl(AIndex : Integer; AValue : TSearchTypeurl); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearch.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  'promotions' : SetLength(Fpromotions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCseResource
  --------------------------------------------------------------------}


Class Function TCseResource.ResourceName : String;

begin
  Result:='cse';
end;

Class Function TCseResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TcustomsearchAPI;
end;

Function TCseResource.List(AQuery : string = '') : TSearch;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1';
  _Methodid   = 'search.cse.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TSearch) as TSearch;
end;


Function TCseResource.List(AQuery : TCselistOptions) : TSearch;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'c2coff',AQuery.c2coff);
  AddToQuery(_Q,'cr',AQuery.cr);
  AddToQuery(_Q,'cref',AQuery.cref);
  AddToQuery(_Q,'cx',AQuery.cx);
  AddToQuery(_Q,'dateRestrict',AQuery.dateRestrict);
  AddToQuery(_Q,'exactTerms',AQuery.exactTerms);
  AddToQuery(_Q,'excludeTerms',AQuery.excludeTerms);
  AddToQuery(_Q,'fileType',AQuery.fileType);
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'gl',AQuery.gl);
  AddToQuery(_Q,'googlehost',AQuery.googlehost);
  AddToQuery(_Q,'highRange',AQuery.highRange);
  AddToQuery(_Q,'hl',AQuery.hl);
  AddToQuery(_Q,'hq',AQuery.hq);
  AddToQuery(_Q,'imgColorType',AQuery.imgColorType);
  AddToQuery(_Q,'imgDominantColor',AQuery.imgDominantColor);
  AddToQuery(_Q,'imgSize',AQuery.imgSize);
  AddToQuery(_Q,'imgType',AQuery.imgType);
  AddToQuery(_Q,'linkSite',AQuery.linkSite);
  AddToQuery(_Q,'lowRange',AQuery.lowRange);
  AddToQuery(_Q,'lr',AQuery.lr);
  AddToQuery(_Q,'num',AQuery.num);
  AddToQuery(_Q,'orTerms',AQuery.orTerms);
  AddToQuery(_Q,'q',AQuery.q);
  AddToQuery(_Q,'relatedSite',AQuery.relatedSite);
  AddToQuery(_Q,'rights',AQuery.rights);
  AddToQuery(_Q,'safe',AQuery.safe);
  AddToQuery(_Q,'searchType',AQuery.searchType);
  AddToQuery(_Q,'siteSearch',AQuery.siteSearch);
  AddToQuery(_Q,'siteSearchFilter',AQuery.siteSearchFilter);
  AddToQuery(_Q,'sort',AQuery.sort);
  AddToQuery(_Q,'start',AQuery.start);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TCustomsearchAPI
  --------------------------------------------------------------------}

Class Function TCustomsearchAPI.APIName : String;

begin
  Result:='customsearch';
end;

Class Function TCustomsearchAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TCustomsearchAPI.APIRevision : String;

begin
  Result:='20131205';
end;

Class Function TCustomsearchAPI.APIID : String;

begin
  Result:='customsearch:v1';
end;

Class Function TCustomsearchAPI.APITitle : String;

begin
  Result:='CustomSearch API';
end;

Class Function TCustomsearchAPI.APIDescription : String;

begin
  Result:='Lets you search over a website or collection of websites';
end;

Class Function TCustomsearchAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TCustomsearchAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TCustomsearchAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TCustomsearchAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TCustomsearchAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/custom-search/v1/using_rest';
end;

Class Function TCustomsearchAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TCustomsearchAPI.APIbasePath : string;

begin
  Result:='/customsearch/';
end;

Class Function TCustomsearchAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/customsearch/';
end;

Class Function TCustomsearchAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TCustomsearchAPI.APIservicePath : string;

begin
  Result:='customsearch/';
end;

Class Function TCustomsearchAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TCustomsearchAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TCustomsearchAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TCustomsearchAPI.RegisterAPIResources;

begin
  TContextTypefacetsItemItem.RegisterObject;
  TContext.RegisterObject;
  TPromotionTypebodyLinesItem.RegisterObject;
  TPromotionTypeimage.RegisterObject;
  TPromotion.RegisterObject;
  TQuery.RegisterObject;
  TResultTypeimage.RegisterObject;
  TResultTypelabelsItem.RegisterObject;
  TResultTypepagemap.RegisterObject;
  TResult.RegisterObject;
  TSearchTypequeries.RegisterObject;
  TSearchTypesearchInformation.RegisterObject;
  TSearchTypespelling.RegisterObject;
  TSearchTypeurl.RegisterObject;
  TSearch.RegisterObject;
end;


Function TCustomsearchAPI.GetCseInstance : TCseResource;

begin
  if (FCseInstance=Nil) then
    FCseInstance:=CreateCseResource;
  Result:=FCseInstance;
end;

Function TCustomsearchAPI.CreateCseResource : TCseResource;

begin
  Result:=CreateCseResource(Self);
end;


Function TCustomsearchAPI.CreateCseResource(AOwner : TComponent) : TCseResource;

begin
  Result:=TCseResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TCustomsearchAPI.RegisterAPI;
end.
