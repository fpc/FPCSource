unit googlecustomsearch;
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
  TContext = class;
  TContextArray = Array of TContext;
  TContextfacets = class;
  TContextfacetsArray = Array of TContextfacets;
  TPromotion = class;
  TPromotionArray = Array of TPromotion;
  TPromotionbodyLines = class;
  TPromotionbodyLinesArray = Array of TPromotionbodyLines;
  TPromotionimage = class;
  TPromotionimageArray = Array of TPromotionimage;
  TQuery = class;
  TQueryArray = Array of TQuery;
  TResult = class;
  TResultArray = Array of TResult;
  TResultimage = class;
  TResultimageArray = Array of TResultimage;
  TResultlabels = class;
  TResultlabelsArray = Array of TResultlabels;
  TResultpagemap = class;
  TResultpagemapArray = Array of TResultpagemap;
  TSearch = class;
  TSearchArray = Array of TSearch;
  TSearchitems = class;
  TSearchitemsArray = Array of TSearchitems;
  TSearchpromotions = class;
  TSearchpromotionsArray = Array of TSearchpromotions;
  TSearchqueries = class;
  TSearchqueriesArray = Array of TSearchqueries;
  TSearchsearchInformation = class;
  TSearchsearchInformationArray = Array of TSearchsearchInformation;
  TSearchspelling = class;
  TSearchspellingArray = Array of TSearchspelling;
  TSearchurl = class;
  TSearchurlArray = Array of TSearchurl;
  
  { --------------------------------------------------------------------
    TContext
    --------------------------------------------------------------------}
  
  TContext = Class(TGoogleBaseObject)
  Private
    Ffacets : TContextfacets;
    Ftitle : string;
  Protected
    //Property setters
    Procedure Setfacets(AIndex : Integer; AValue : TContextfacets); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property facets : TContextfacets Index 0 Read Ffacets Write Setfacets;
    Property title : string Index 8 Read Ftitle Write Settitle;
  end;
  TContextClass = Class of TContext;
  
  { --------------------------------------------------------------------
    TContextfacets
    --------------------------------------------------------------------}
  
  TContextfacets = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TContextfacetsClass = Class of TContextfacets;
  
  { --------------------------------------------------------------------
    TPromotion
    --------------------------------------------------------------------}
  
  TPromotion = Class(TGoogleBaseObject)
  Private
    FbodyLines : TPromotionbodyLines;
    FdisplayLink : string;
    FhtmlTitle : string;
    Fimage : TPromotionimage;
    Flink : string;
    Ftitle : string;
  Protected
    //Property setters
    Procedure SetbodyLines(AIndex : Integer; AValue : TPromotionbodyLines); virtual;
    Procedure SetdisplayLink(AIndex : Integer; AValue : string); virtual;
    Procedure SethtmlTitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TPromotionimage); virtual;
    Procedure Setlink(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property bodyLines : TPromotionbodyLines Index 0 Read FbodyLines Write SetbodyLines;
    Property displayLink : string Index 8 Read FdisplayLink Write SetdisplayLink;
    Property htmlTitle : string Index 16 Read FhtmlTitle Write SethtmlTitle;
    Property image : TPromotionimage Index 24 Read Fimage Write Setimage;
    Property link : string Index 32 Read Flink Write Setlink;
    Property title : string Index 40 Read Ftitle Write Settitle;
  end;
  TPromotionClass = Class of TPromotion;
  
  { --------------------------------------------------------------------
    TPromotionbodyLines
    --------------------------------------------------------------------}
  
  TPromotionbodyLines = Class(TGoogleBaseObject)
  Private
    FhtmlTitle : string;
    Flink : string;
    Ftitle : string;
    Furl : string;
  Protected
    //Property setters
    Procedure SethtmlTitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setlink(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property htmlTitle : string Index 0 Read FhtmlTitle Write SethtmlTitle;
    Property link : string Index 8 Read Flink Write Setlink;
    Property title : string Index 16 Read Ftitle Write Settitle;
    Property url : string Index 24 Read Furl Write Seturl;
  end;
  TPromotionbodyLinesClass = Class of TPromotionbodyLines;
  
  { --------------------------------------------------------------------
    TPromotionimage
    --------------------------------------------------------------------}
  
  TPromotionimage = Class(TGoogleBaseObject)
  Private
    Fheight : integer;
    Fsource : string;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure Setsource(AIndex : Integer; AValue : string); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property height : integer Index 0 Read Fheight Write Setheight;
    Property source : string Index 8 Read Fsource Write Setsource;
    Property width : integer Index 16 Read Fwidth Write Setwidth;
  end;
  TPromotionimageClass = Class of TPromotionimage;
  
  { --------------------------------------------------------------------
    TQuery
    --------------------------------------------------------------------}
  
  TQuery = Class(TGoogleBaseObject)
  Private
    Fcount : integer;
    Fcr : string;
    Fcref : string;
    Fcx : string;
    FdateRestrict : string;
    FdisableCnTwTranslation : string;
    FexactTerms : string;
    FexcludeTerms : string;
    FfileType : string;
    Ffilter : string;
    Fgl : string;
    FgoogleHost : string;
    FhighRange : string;
    Fhl : string;
    Fhq : string;
    FimgColorType : string;
    FimgDominantColor : string;
    FimgSize : string;
    FimgType : string;
    FinputEncoding : string;
    Flanguage : string;
    FlinkSite : string;
    FlowRange : string;
    ForTerms : string;
    FoutputEncoding : string;
    FrelatedSite : string;
    Frights : string;
    Fsafe : string;
    FsearchTerms : string;
    FsearchType : string;
    FsiteSearch : string;
    FsiteSearchFilter : string;
    Fsort : string;
    FstartIndex : integer;
    FstartPage : integer;
    Ftitle : string;
    FtotalResults : string;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setcr(AIndex : Integer; AValue : string); virtual;
    Procedure Setcref(AIndex : Integer; AValue : string); virtual;
    Procedure Setcx(AIndex : Integer; AValue : string); virtual;
    Procedure SetdateRestrict(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisableCnTwTranslation(AIndex : Integer; AValue : string); virtual;
    Procedure SetexactTerms(AIndex : Integer; AValue : string); virtual;
    Procedure SetexcludeTerms(AIndex : Integer; AValue : string); virtual;
    Procedure SetfileType(AIndex : Integer; AValue : string); virtual;
    Procedure Setfilter(AIndex : Integer; AValue : string); virtual;
    Procedure Setgl(AIndex : Integer; AValue : string); virtual;
    Procedure SetgoogleHost(AIndex : Integer; AValue : string); virtual;
    Procedure SethighRange(AIndex : Integer; AValue : string); virtual;
    Procedure Sethl(AIndex : Integer; AValue : string); virtual;
    Procedure Sethq(AIndex : Integer; AValue : string); virtual;
    Procedure SetimgColorType(AIndex : Integer; AValue : string); virtual;
    Procedure SetimgDominantColor(AIndex : Integer; AValue : string); virtual;
    Procedure SetimgSize(AIndex : Integer; AValue : string); virtual;
    Procedure SetimgType(AIndex : Integer; AValue : string); virtual;
    Procedure SetinputEncoding(AIndex : Integer; AValue : string); virtual;
    Procedure Setlanguage(AIndex : Integer; AValue : string); virtual;
    Procedure SetlinkSite(AIndex : Integer; AValue : string); virtual;
    Procedure SetlowRange(AIndex : Integer; AValue : string); virtual;
    Procedure SetorTerms(AIndex : Integer; AValue : string); virtual;
    Procedure SetoutputEncoding(AIndex : Integer; AValue : string); virtual;
    Procedure SetrelatedSite(AIndex : Integer; AValue : string); virtual;
    Procedure Setrights(AIndex : Integer; AValue : string); virtual;
    Procedure Setsafe(AIndex : Integer; AValue : string); virtual;
    Procedure SetsearchTerms(AIndex : Integer; AValue : string); virtual;
    Procedure SetsearchType(AIndex : Integer; AValue : string); virtual;
    Procedure SetsiteSearch(AIndex : Integer; AValue : string); virtual;
    Procedure SetsiteSearchFilter(AIndex : Integer; AValue : string); virtual;
    Procedure Setsort(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartIndex(AIndex : Integer; AValue : integer); virtual;
    Procedure SetstartPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property count : integer Index 0 Read Fcount Write Setcount;
    Property cr : string Index 8 Read Fcr Write Setcr;
    Property cref : string Index 16 Read Fcref Write Setcref;
    Property cx : string Index 24 Read Fcx Write Setcx;
    Property dateRestrict : string Index 32 Read FdateRestrict Write SetdateRestrict;
    Property disableCnTwTranslation : string Index 40 Read FdisableCnTwTranslation Write SetdisableCnTwTranslation;
    Property exactTerms : string Index 48 Read FexactTerms Write SetexactTerms;
    Property excludeTerms : string Index 56 Read FexcludeTerms Write SetexcludeTerms;
    Property fileType : string Index 64 Read FfileType Write SetfileType;
    Property filter : string Index 72 Read Ffilter Write Setfilter;
    Property gl : string Index 80 Read Fgl Write Setgl;
    Property googleHost : string Index 88 Read FgoogleHost Write SetgoogleHost;
    Property highRange : string Index 96 Read FhighRange Write SethighRange;
    Property hl : string Index 104 Read Fhl Write Sethl;
    Property hq : string Index 112 Read Fhq Write Sethq;
    Property imgColorType : string Index 120 Read FimgColorType Write SetimgColorType;
    Property imgDominantColor : string Index 128 Read FimgDominantColor Write SetimgDominantColor;
    Property imgSize : string Index 136 Read FimgSize Write SetimgSize;
    Property imgType : string Index 144 Read FimgType Write SetimgType;
    Property inputEncoding : string Index 152 Read FinputEncoding Write SetinputEncoding;
    Property language : string Index 160 Read Flanguage Write Setlanguage;
    Property linkSite : string Index 168 Read FlinkSite Write SetlinkSite;
    Property lowRange : string Index 176 Read FlowRange Write SetlowRange;
    Property orTerms : string Index 184 Read ForTerms Write SetorTerms;
    Property outputEncoding : string Index 192 Read FoutputEncoding Write SetoutputEncoding;
    Property relatedSite : string Index 200 Read FrelatedSite Write SetrelatedSite;
    Property rights : string Index 208 Read Frights Write Setrights;
    Property safe : string Index 216 Read Fsafe Write Setsafe;
    Property searchTerms : string Index 224 Read FsearchTerms Write SetsearchTerms;
    Property searchType : string Index 232 Read FsearchType Write SetsearchType;
    Property siteSearch : string Index 240 Read FsiteSearch Write SetsiteSearch;
    Property siteSearchFilter : string Index 248 Read FsiteSearchFilter Write SetsiteSearchFilter;
    Property sort : string Index 256 Read Fsort Write Setsort;
    Property startIndex : integer Index 264 Read FstartIndex Write SetstartIndex;
    Property startPage : integer Index 272 Read FstartPage Write SetstartPage;
    Property title : string Index 280 Read Ftitle Write Settitle;
    Property totalResults : string Index 288 Read FtotalResults Write SettotalResults;
  end;
  TQueryClass = Class of TQuery;
  
  { --------------------------------------------------------------------
    TResult
    --------------------------------------------------------------------}
  
  TResult = Class(TGoogleBaseObject)
  Private
    FcacheId : string;
    FdisplayLink : string;
    FfileFormat : string;
    FformattedUrl : string;
    FhtmlFormattedUrl : string;
    FhtmlSnippet : string;
    FhtmlTitle : string;
    Fimage : TResultimage;
    Fkind : string;
    Flabels : TResultlabels;
    Flink : string;
    Fmime : string;
    Fpagemap : TResultpagemap;
    Fsnippet : string;
    Ftitle : string;
  Protected
    //Property setters
    Procedure SetcacheId(AIndex : Integer; AValue : string); virtual;
    Procedure SetdisplayLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetfileFormat(AIndex : Integer; AValue : string); virtual;
    Procedure SetformattedUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SethtmlFormattedUrl(AIndex : Integer; AValue : string); virtual;
    Procedure SethtmlSnippet(AIndex : Integer; AValue : string); virtual;
    Procedure SethtmlTitle(AIndex : Integer; AValue : string); virtual;
    Procedure Setimage(AIndex : Integer; AValue : TResultimage); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlabels(AIndex : Integer; AValue : TResultlabels); virtual;
    Procedure Setlink(AIndex : Integer; AValue : string); virtual;
    Procedure Setmime(AIndex : Integer; AValue : string); virtual;
    Procedure Setpagemap(AIndex : Integer; AValue : TResultpagemap); virtual;
    Procedure Setsnippet(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property cacheId : string Index 0 Read FcacheId Write SetcacheId;
    Property displayLink : string Index 8 Read FdisplayLink Write SetdisplayLink;
    Property fileFormat : string Index 16 Read FfileFormat Write SetfileFormat;
    Property formattedUrl : string Index 24 Read FformattedUrl Write SetformattedUrl;
    Property htmlFormattedUrl : string Index 32 Read FhtmlFormattedUrl Write SethtmlFormattedUrl;
    Property htmlSnippet : string Index 40 Read FhtmlSnippet Write SethtmlSnippet;
    Property htmlTitle : string Index 48 Read FhtmlTitle Write SethtmlTitle;
    Property image : TResultimage Index 56 Read Fimage Write Setimage;
    Property kind : string Index 64 Read Fkind Write Setkind;
    Property labels : TResultlabels Index 72 Read Flabels Write Setlabels;
    Property link : string Index 80 Read Flink Write Setlink;
    Property mime : string Index 88 Read Fmime Write Setmime;
    Property pagemap : TResultpagemap Index 96 Read Fpagemap Write Setpagemap;
    Property snippet : string Index 104 Read Fsnippet Write Setsnippet;
    Property title : string Index 112 Read Ftitle Write Settitle;
  end;
  TResultClass = Class of TResult;
  
  { --------------------------------------------------------------------
    TResultimage
    --------------------------------------------------------------------}
  
  TResultimage = Class(TGoogleBaseObject)
  Private
    FbyteSize : integer;
    FcontextLink : string;
    Fheight : integer;
    FthumbnailHeight : integer;
    FthumbnailLink : string;
    FthumbnailWidth : integer;
    Fwidth : integer;
  Protected
    //Property setters
    Procedure SetbyteSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetcontextLink(AIndex : Integer; AValue : string); virtual;
    Procedure Setheight(AIndex : Integer; AValue : integer); virtual;
    Procedure SetthumbnailHeight(AIndex : Integer; AValue : integer); virtual;
    Procedure SetthumbnailLink(AIndex : Integer; AValue : string); virtual;
    Procedure SetthumbnailWidth(AIndex : Integer; AValue : integer); virtual;
    Procedure Setwidth(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property byteSize : integer Index 0 Read FbyteSize Write SetbyteSize;
    Property contextLink : string Index 8 Read FcontextLink Write SetcontextLink;
    Property height : integer Index 16 Read Fheight Write Setheight;
    Property thumbnailHeight : integer Index 24 Read FthumbnailHeight Write SetthumbnailHeight;
    Property thumbnailLink : string Index 32 Read FthumbnailLink Write SetthumbnailLink;
    Property thumbnailWidth : integer Index 40 Read FthumbnailWidth Write SetthumbnailWidth;
    Property width : integer Index 48 Read Fwidth Write Setwidth;
  end;
  TResultimageClass = Class of TResultimage;
  
  { --------------------------------------------------------------------
    TResultlabels
    --------------------------------------------------------------------}
  
  TResultlabels = Class(TGoogleBaseObject)
  Private
    FdisplayName : string;
    Flabel_with_op : string;
    Fname : string;
  Protected
    //Property setters
    Procedure SetdisplayName(AIndex : Integer; AValue : string); virtual;
    Procedure Setlabel_with_op(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property displayName : string Index 0 Read FdisplayName Write SetdisplayName;
    Property label_with_op : string Index 8 Read Flabel_with_op Write Setlabel_with_op;
    Property name : string Index 16 Read Fname Write Setname;
  end;
  TResultlabelsClass = Class of TResultlabels;
  
  { --------------------------------------------------------------------
    TResultpagemap
    --------------------------------------------------------------------}
  
  TResultpagemap = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TResultpagemapClass = Class of TResultpagemap;
  
  { --------------------------------------------------------------------
    TSearch
    --------------------------------------------------------------------}
  
  TSearch = Class(TGoogleBaseObject)
  Private
    Fcontext : TContext;
    Fitems : TSearchitems;
    Fkind : string;
    Fpromotions : TSearchpromotions;
    Fqueries : TSearchqueries;
    FsearchInformation : TSearchsearchInformation;
    Fspelling : TSearchspelling;
    Furl : TSearchurl;
  Protected
    //Property setters
    Procedure Setcontext(AIndex : Integer; AValue : TContext); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TSearchitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setpromotions(AIndex : Integer; AValue : TSearchpromotions); virtual;
    Procedure Setqueries(AIndex : Integer; AValue : TSearchqueries); virtual;
    Procedure SetsearchInformation(AIndex : Integer; AValue : TSearchsearchInformation); virtual;
    Procedure Setspelling(AIndex : Integer; AValue : TSearchspelling); virtual;
    Procedure Seturl(AIndex : Integer; AValue : TSearchurl); virtual;
  Public
  Published
    Property context : TContext Index 0 Read Fcontext Write Setcontext;
    Property items : TSearchitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property promotions : TSearchpromotions Index 24 Read Fpromotions Write Setpromotions;
    Property queries : TSearchqueries Index 32 Read Fqueries Write Setqueries;
    Property searchInformation : TSearchsearchInformation Index 40 Read FsearchInformation Write SetsearchInformation;
    Property spelling : TSearchspelling Index 48 Read Fspelling Write Setspelling;
    Property url : TSearchurl Index 56 Read Furl Write Seturl;
  end;
  TSearchClass = Class of TSearch;
  
  { --------------------------------------------------------------------
    TSearchitems
    --------------------------------------------------------------------}
  
  TSearchitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchitemsClass = Class of TSearchitems;
  
  { --------------------------------------------------------------------
    TSearchpromotions
    --------------------------------------------------------------------}
  
  TSearchpromotions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchpromotionsClass = Class of TSearchpromotions;
  
  { --------------------------------------------------------------------
    TSearchqueries
    --------------------------------------------------------------------}
  
  TSearchqueries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TSearchqueriesClass = Class of TSearchqueries;
  
  { --------------------------------------------------------------------
    TSearchsearchInformation
    --------------------------------------------------------------------}
  
  TSearchsearchInformation = Class(TGoogleBaseObject)
  Private
    FformattedSearchTime : string;
    FformattedTotalResults : string;
    FsearchTime : double;
    FtotalResults : string;
  Protected
    //Property setters
    Procedure SetformattedSearchTime(AIndex : Integer; AValue : string); virtual;
    Procedure SetformattedTotalResults(AIndex : Integer; AValue : string); virtual;
    Procedure SetsearchTime(AIndex : Integer; AValue : double); virtual;
    Procedure SettotalResults(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property formattedSearchTime : string Index 0 Read FformattedSearchTime Write SetformattedSearchTime;
    Property formattedTotalResults : string Index 8 Read FformattedTotalResults Write SetformattedTotalResults;
    Property searchTime : double Index 16 Read FsearchTime Write SetsearchTime;
    Property totalResults : string Index 24 Read FtotalResults Write SettotalResults;
  end;
  TSearchsearchInformationClass = Class of TSearchsearchInformation;
  
  { --------------------------------------------------------------------
    TSearchspelling
    --------------------------------------------------------------------}
  
  TSearchspelling = Class(TGoogleBaseObject)
  Private
    FcorrectedQuery : string;
    FhtmlCorrectedQuery : string;
  Protected
    //Property setters
    Procedure SetcorrectedQuery(AIndex : Integer; AValue : string); virtual;
    Procedure SethtmlCorrectedQuery(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property correctedQuery : string Index 0 Read FcorrectedQuery Write SetcorrectedQuery;
    Property htmlCorrectedQuery : string Index 8 Read FhtmlCorrectedQuery Write SethtmlCorrectedQuery;
  end;
  TSearchspellingClass = Class of TSearchspelling;
  
  { --------------------------------------------------------------------
    TSearchurl
    --------------------------------------------------------------------}
  
  TSearchurl = Class(TGoogleBaseObject)
  Private
    Ftemplate : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Settemplate(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property template : string Index 0 Read Ftemplate Write Settemplate;
    Property _type : string Index 8 Read F_type Write Set_type;
  end;
  TSearchurlClass = Class of TSearchurl;
  
  { --------------------------------------------------------------------
    TCseResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCseResource, method List
  
  TCseListOptions = Record
    c2coff : string;
    cr : string;
    cref : string;
    cx : string;
    dateRestrict : string;
    exactTerms : string;
    excludeTerms : string;
    fileType : string;
    filter : string;
    gl : string;
    googlehost : string;
    highRange : string;
    hl : string;
    hq : string;
    imgColorType : string;
    imgDominantColor : string;
    imgSize : string;
    imgType : string;
    linkSite : string;
    lowRange : string;
    lr : string;
    num : integer;
    orTerms : string;
    q : string;
    relatedSite : string;
    rights : string;
    safe : string;
    searchType : string;
    siteSearch : string;
    siteSearchFilter : string;
    sort : string;
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
  TContext
  --------------------------------------------------------------------}


Procedure TContext.Setfacets(AIndex : Integer; AValue : TContextfacets); 

begin
  If (Ffacets=AValue) then exit;
  Ffacets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TContext.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TContextfacets
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPromotion
  --------------------------------------------------------------------}


Procedure TPromotion.SetbodyLines(AIndex : Integer; AValue : TPromotionbodyLines); 

begin
  If (FbodyLines=AValue) then exit;
  FbodyLines:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotion.SetdisplayLink(AIndex : Integer; AValue : string); 

begin
  If (FdisplayLink=AValue) then exit;
  FdisplayLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotion.SethtmlTitle(AIndex : Integer; AValue : string); 

begin
  If (FhtmlTitle=AValue) then exit;
  FhtmlTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotion.Setimage(AIndex : Integer; AValue : TPromotionimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotion.Setlink(AIndex : Integer; AValue : string); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotion.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPromotionbodyLines
  --------------------------------------------------------------------}


Procedure TPromotionbodyLines.SethtmlTitle(AIndex : Integer; AValue : string); 

begin
  If (FhtmlTitle=AValue) then exit;
  FhtmlTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotionbodyLines.Setlink(AIndex : Integer; AValue : string); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotionbodyLines.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotionbodyLines.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPromotionimage
  --------------------------------------------------------------------}


Procedure TPromotionimage.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotionimage.Setsource(AIndex : Integer; AValue : string); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPromotionimage.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQuery
  --------------------------------------------------------------------}


Procedure TQuery.Setcount(AIndex : Integer; AValue : integer); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setcr(AIndex : Integer; AValue : string); 

begin
  If (Fcr=AValue) then exit;
  Fcr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setcref(AIndex : Integer; AValue : string); 

begin
  If (Fcref=AValue) then exit;
  Fcref:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setcx(AIndex : Integer; AValue : string); 

begin
  If (Fcx=AValue) then exit;
  Fcx:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetdateRestrict(AIndex : Integer; AValue : string); 

begin
  If (FdateRestrict=AValue) then exit;
  FdateRestrict:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetdisableCnTwTranslation(AIndex : Integer; AValue : string); 

begin
  If (FdisableCnTwTranslation=AValue) then exit;
  FdisableCnTwTranslation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetexactTerms(AIndex : Integer; AValue : string); 

begin
  If (FexactTerms=AValue) then exit;
  FexactTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetexcludeTerms(AIndex : Integer; AValue : string); 

begin
  If (FexcludeTerms=AValue) then exit;
  FexcludeTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetfileType(AIndex : Integer; AValue : string); 

begin
  If (FfileType=AValue) then exit;
  FfileType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setfilter(AIndex : Integer; AValue : string); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setgl(AIndex : Integer; AValue : string); 

begin
  If (Fgl=AValue) then exit;
  Fgl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetgoogleHost(AIndex : Integer; AValue : string); 

begin
  If (FgoogleHost=AValue) then exit;
  FgoogleHost:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SethighRange(AIndex : Integer; AValue : string); 

begin
  If (FhighRange=AValue) then exit;
  FhighRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Sethl(AIndex : Integer; AValue : string); 

begin
  If (Fhl=AValue) then exit;
  Fhl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Sethq(AIndex : Integer; AValue : string); 

begin
  If (Fhq=AValue) then exit;
  Fhq:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetimgColorType(AIndex : Integer; AValue : string); 

begin
  If (FimgColorType=AValue) then exit;
  FimgColorType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetimgDominantColor(AIndex : Integer; AValue : string); 

begin
  If (FimgDominantColor=AValue) then exit;
  FimgDominantColor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetimgSize(AIndex : Integer; AValue : string); 

begin
  If (FimgSize=AValue) then exit;
  FimgSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetimgType(AIndex : Integer; AValue : string); 

begin
  If (FimgType=AValue) then exit;
  FimgType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetinputEncoding(AIndex : Integer; AValue : string); 

begin
  If (FinputEncoding=AValue) then exit;
  FinputEncoding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setlanguage(AIndex : Integer; AValue : string); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetlinkSite(AIndex : Integer; AValue : string); 

begin
  If (FlinkSite=AValue) then exit;
  FlinkSite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetlowRange(AIndex : Integer; AValue : string); 

begin
  If (FlowRange=AValue) then exit;
  FlowRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetorTerms(AIndex : Integer; AValue : string); 

begin
  If (ForTerms=AValue) then exit;
  ForTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetoutputEncoding(AIndex : Integer; AValue : string); 

begin
  If (FoutputEncoding=AValue) then exit;
  FoutputEncoding:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetrelatedSite(AIndex : Integer; AValue : string); 

begin
  If (FrelatedSite=AValue) then exit;
  FrelatedSite:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setrights(AIndex : Integer; AValue : string); 

begin
  If (Frights=AValue) then exit;
  Frights:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setsafe(AIndex : Integer; AValue : string); 

begin
  If (Fsafe=AValue) then exit;
  Fsafe:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetsearchTerms(AIndex : Integer; AValue : string); 

begin
  If (FsearchTerms=AValue) then exit;
  FsearchTerms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetsearchType(AIndex : Integer; AValue : string); 

begin
  If (FsearchType=AValue) then exit;
  FsearchType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetsiteSearch(AIndex : Integer; AValue : string); 

begin
  If (FsiteSearch=AValue) then exit;
  FsiteSearch:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetsiteSearchFilter(AIndex : Integer; AValue : string); 

begin
  If (FsiteSearchFilter=AValue) then exit;
  FsiteSearchFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setsort(AIndex : Integer; AValue : string); 

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



Procedure TQuery.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SettotalResults(AIndex : Integer; AValue : string); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResult
  --------------------------------------------------------------------}


Procedure TResult.SetcacheId(AIndex : Integer; AValue : string); 

begin
  If (FcacheId=AValue) then exit;
  FcacheId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SetdisplayLink(AIndex : Integer; AValue : string); 

begin
  If (FdisplayLink=AValue) then exit;
  FdisplayLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SetfileFormat(AIndex : Integer; AValue : string); 

begin
  If (FfileFormat=AValue) then exit;
  FfileFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SetformattedUrl(AIndex : Integer; AValue : string); 

begin
  If (FformattedUrl=AValue) then exit;
  FformattedUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SethtmlFormattedUrl(AIndex : Integer; AValue : string); 

begin
  If (FhtmlFormattedUrl=AValue) then exit;
  FhtmlFormattedUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SethtmlSnippet(AIndex : Integer; AValue : string); 

begin
  If (FhtmlSnippet=AValue) then exit;
  FhtmlSnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.SethtmlTitle(AIndex : Integer; AValue : string); 

begin
  If (FhtmlTitle=AValue) then exit;
  FhtmlTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setimage(AIndex : Integer; AValue : TResultimage); 

begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setlabels(AIndex : Integer; AValue : TResultlabels); 

begin
  If (Flabels=AValue) then exit;
  Flabels:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setlink(AIndex : Integer; AValue : string); 

begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setmime(AIndex : Integer; AValue : string); 

begin
  If (Fmime=AValue) then exit;
  Fmime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setpagemap(AIndex : Integer; AValue : TResultpagemap); 

begin
  If (Fpagemap=AValue) then exit;
  Fpagemap:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Setsnippet(AIndex : Integer; AValue : string); 

begin
  If (Fsnippet=AValue) then exit;
  Fsnippet:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResult.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultimage
  --------------------------------------------------------------------}


Procedure TResultimage.SetbyteSize(AIndex : Integer; AValue : integer); 

begin
  If (FbyteSize=AValue) then exit;
  FbyteSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultimage.SetcontextLink(AIndex : Integer; AValue : string); 

begin
  If (FcontextLink=AValue) then exit;
  FcontextLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultimage.Setheight(AIndex : Integer; AValue : integer); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultimage.SetthumbnailHeight(AIndex : Integer; AValue : integer); 

begin
  If (FthumbnailHeight=AValue) then exit;
  FthumbnailHeight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultimage.SetthumbnailLink(AIndex : Integer; AValue : string); 

begin
  If (FthumbnailLink=AValue) then exit;
  FthumbnailLink:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultimage.SetthumbnailWidth(AIndex : Integer; AValue : integer); 

begin
  If (FthumbnailWidth=AValue) then exit;
  FthumbnailWidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultimage.Setwidth(AIndex : Integer; AValue : integer); 

begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultlabels
  --------------------------------------------------------------------}


Procedure TResultlabels.SetdisplayName(AIndex : Integer; AValue : string); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultlabels.Setlabel_with_op(AIndex : Integer; AValue : string); 

begin
  If (Flabel_with_op=AValue) then exit;
  Flabel_with_op:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResultlabels.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResultpagemap
  --------------------------------------------------------------------}


Class Function TResultpagemap.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
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



Procedure TSearch.Setitems(AIndex : Integer; AValue : TSearchitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearch.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearch.Setpromotions(AIndex : Integer; AValue : TSearchpromotions); 

begin
  If (Fpromotions=AValue) then exit;
  Fpromotions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearch.Setqueries(AIndex : Integer; AValue : TSearchqueries); 

begin
  If (Fqueries=AValue) then exit;
  Fqueries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearch.SetsearchInformation(AIndex : Integer; AValue : TSearchsearchInformation); 

begin
  If (FsearchInformation=AValue) then exit;
  FsearchInformation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearch.Setspelling(AIndex : Integer; AValue : TSearchspelling); 

begin
  If (Fspelling=AValue) then exit;
  Fspelling:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearch.Seturl(AIndex : Integer; AValue : TSearchurl); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchpromotions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchqueries
  --------------------------------------------------------------------}


Class Function TSearchqueries.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TSearchsearchInformation
  --------------------------------------------------------------------}


Procedure TSearchsearchInformation.SetformattedSearchTime(AIndex : Integer; AValue : string); 

begin
  If (FformattedSearchTime=AValue) then exit;
  FformattedSearchTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchsearchInformation.SetformattedTotalResults(AIndex : Integer; AValue : string); 

begin
  If (FformattedTotalResults=AValue) then exit;
  FformattedTotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchsearchInformation.SetsearchTime(AIndex : Integer; AValue : double); 

begin
  If (FsearchTime=AValue) then exit;
  FsearchTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchsearchInformation.SettotalResults(AIndex : Integer; AValue : string); 

begin
  If (FtotalResults=AValue) then exit;
  FtotalResults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchspelling
  --------------------------------------------------------------------}


Procedure TSearchspelling.SetcorrectedQuery(AIndex : Integer; AValue : string); 

begin
  If (FcorrectedQuery=AValue) then exit;
  FcorrectedQuery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchspelling.SethtmlCorrectedQuery(AIndex : Integer; AValue : string); 

begin
  If (FhtmlCorrectedQuery=AValue) then exit;
  FhtmlCorrectedQuery:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchurl
  --------------------------------------------------------------------}


Procedure TSearchurl.Settemplate(AIndex : Integer; AValue : string); 

begin
  If (Ftemplate=AValue) then exit;
  Ftemplate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchurl.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSearchurl.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




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
  Result:='https://www.googleapis.com/';
end;

Class Function TCustomsearchAPI.APIbasePath : string;

begin
  Result:='/customsearch/';
end;

Class Function TCustomsearchAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/customsearch/';
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
  TContext.RegisterObject;
  TContextfacets.RegisterObject;
  TPromotion.RegisterObject;
  TPromotionbodyLines.RegisterObject;
  TPromotionimage.RegisterObject;
  TQuery.RegisterObject;
  TResult.RegisterObject;
  TResultimage.RegisterObject;
  TResultlabels.RegisterObject;
  TResultpagemap.RegisterObject;
  TSearch.RegisterObject;
  TSearchitems.RegisterObject;
  TSearchpromotions.RegisterObject;
  TSearchqueries.RegisterObject;
  TSearchsearchInformation.RegisterObject;
  TSearchspelling.RegisterObject;
  TSearchurl.RegisterObject;
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
  Result.API:=Self;
end;



initialization
  TCustomsearchAPI.RegisterAPI;
end.
