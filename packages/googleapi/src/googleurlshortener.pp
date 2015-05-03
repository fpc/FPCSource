unit googleurlshortener;
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
  TAnalyticsSnapshot = class;
  TAnalyticsSnapshotArray = Array of TAnalyticsSnapshot;
  TAnalyticsSnapshotbrowsers = class;
  TAnalyticsSnapshotbrowsersArray = Array of TAnalyticsSnapshotbrowsers;
  TAnalyticsSnapshotcountries = class;
  TAnalyticsSnapshotcountriesArray = Array of TAnalyticsSnapshotcountries;
  TAnalyticsSnapshotplatforms = class;
  TAnalyticsSnapshotplatformsArray = Array of TAnalyticsSnapshotplatforms;
  TAnalyticsSnapshotreferrers = class;
  TAnalyticsSnapshotreferrersArray = Array of TAnalyticsSnapshotreferrers;
  TAnalyticsSummary = class;
  TAnalyticsSummaryArray = Array of TAnalyticsSummary;
  TStringCount = class;
  TStringCountArray = Array of TStringCount;
  TUrl = class;
  TUrlArray = Array of TUrl;
  TUrlHistory = class;
  TUrlHistoryArray = Array of TUrlHistory;
  TUrlHistoryitems = class;
  TUrlHistoryitemsArray = Array of TUrlHistoryitems;
  
  { --------------------------------------------------------------------
    TAnalyticsSnapshot
    --------------------------------------------------------------------}
  
  TAnalyticsSnapshot = Class(TGoogleBaseObject)
  Private
    Fbrowsers : TAnalyticsSnapshotbrowsers;
    Fcountries : TAnalyticsSnapshotcountries;
    FlongUrlClicks : string;
    Fplatforms : TAnalyticsSnapshotplatforms;
    Freferrers : TAnalyticsSnapshotreferrers;
    FshortUrlClicks : string;
  Protected
    //Property setters
    Procedure Setbrowsers(AIndex : Integer; AValue : TAnalyticsSnapshotbrowsers); virtual;
    Procedure Setcountries(AIndex : Integer; AValue : TAnalyticsSnapshotcountries); virtual;
    Procedure SetlongUrlClicks(AIndex : Integer; AValue : string); virtual;
    Procedure Setplatforms(AIndex : Integer; AValue : TAnalyticsSnapshotplatforms); virtual;
    Procedure Setreferrers(AIndex : Integer; AValue : TAnalyticsSnapshotreferrers); virtual;
    Procedure SetshortUrlClicks(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property browsers : TAnalyticsSnapshotbrowsers Index 0 Read Fbrowsers Write Setbrowsers;
    Property countries : TAnalyticsSnapshotcountries Index 8 Read Fcountries Write Setcountries;
    Property longUrlClicks : string Index 16 Read FlongUrlClicks Write SetlongUrlClicks;
    Property platforms : TAnalyticsSnapshotplatforms Index 24 Read Fplatforms Write Setplatforms;
    Property referrers : TAnalyticsSnapshotreferrers Index 32 Read Freferrers Write Setreferrers;
    Property shortUrlClicks : string Index 40 Read FshortUrlClicks Write SetshortUrlClicks;
  end;
  TAnalyticsSnapshotClass = Class of TAnalyticsSnapshot;
  
  { --------------------------------------------------------------------
    TAnalyticsSnapshotbrowsers
    --------------------------------------------------------------------}
  
  TAnalyticsSnapshotbrowsers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAnalyticsSnapshotbrowsersClass = Class of TAnalyticsSnapshotbrowsers;
  
  { --------------------------------------------------------------------
    TAnalyticsSnapshotcountries
    --------------------------------------------------------------------}
  
  TAnalyticsSnapshotcountries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAnalyticsSnapshotcountriesClass = Class of TAnalyticsSnapshotcountries;
  
  { --------------------------------------------------------------------
    TAnalyticsSnapshotplatforms
    --------------------------------------------------------------------}
  
  TAnalyticsSnapshotplatforms = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAnalyticsSnapshotplatformsClass = Class of TAnalyticsSnapshotplatforms;
  
  { --------------------------------------------------------------------
    TAnalyticsSnapshotreferrers
    --------------------------------------------------------------------}
  
  TAnalyticsSnapshotreferrers = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAnalyticsSnapshotreferrersClass = Class of TAnalyticsSnapshotreferrers;
  
  { --------------------------------------------------------------------
    TAnalyticsSummary
    --------------------------------------------------------------------}
  
  TAnalyticsSummary = Class(TGoogleBaseObject)
  Private
    FallTime : TAnalyticsSnapshot;
    Fday : TAnalyticsSnapshot;
    Fmonth : TAnalyticsSnapshot;
    FtwoHours : TAnalyticsSnapshot;
    Fweek : TAnalyticsSnapshot;
  Protected
    //Property setters
    Procedure SetallTime(AIndex : Integer; AValue : TAnalyticsSnapshot); virtual;
    Procedure Setday(AIndex : Integer; AValue : TAnalyticsSnapshot); virtual;
    Procedure Setmonth(AIndex : Integer; AValue : TAnalyticsSnapshot); virtual;
    Procedure SettwoHours(AIndex : Integer; AValue : TAnalyticsSnapshot); virtual;
    Procedure Setweek(AIndex : Integer; AValue : TAnalyticsSnapshot); virtual;
  Public
  Published
    Property allTime : TAnalyticsSnapshot Index 0 Read FallTime Write SetallTime;
    Property day : TAnalyticsSnapshot Index 8 Read Fday Write Setday;
    Property month : TAnalyticsSnapshot Index 16 Read Fmonth Write Setmonth;
    Property twoHours : TAnalyticsSnapshot Index 24 Read FtwoHours Write SettwoHours;
    Property week : TAnalyticsSnapshot Index 32 Read Fweek Write Setweek;
  end;
  TAnalyticsSummaryClass = Class of TAnalyticsSummary;
  
  { --------------------------------------------------------------------
    TStringCount
    --------------------------------------------------------------------}
  
  TStringCount = Class(TGoogleBaseObject)
  Private
    Fcount : string;
    Fid : string;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property count : string Index 0 Read Fcount Write Setcount;
    Property id : string Index 8 Read Fid Write Setid;
  end;
  TStringCountClass = Class of TStringCount;
  
  { --------------------------------------------------------------------
    TUrl
    --------------------------------------------------------------------}
  
  TUrl = Class(TGoogleBaseObject)
  Private
    Fanalytics : TAnalyticsSummary;
    Fcreated : string;
    Fid : string;
    Fkind : string;
    FlongUrl : string;
    Fstatus : string;
  Protected
    //Property setters
    Procedure Setanalytics(AIndex : Integer; AValue : TAnalyticsSummary); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlongUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property analytics : TAnalyticsSummary Index 0 Read Fanalytics Write Setanalytics;
    Property created : string Index 8 Read Fcreated Write Setcreated;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property longUrl : string Index 32 Read FlongUrl Write SetlongUrl;
    Property status : string Index 40 Read Fstatus Write Setstatus;
  end;
  TUrlClass = Class of TUrl;
  
  { --------------------------------------------------------------------
    TUrlHistory
    --------------------------------------------------------------------}
  
  TUrlHistory = Class(TGoogleBaseObject)
  Private
    Fitems : TUrlHistoryitems;
    FitemsPerPage : integer;
    Fkind : string;
    FnextPageToken : string;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TUrlHistoryitems); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property items : TUrlHistoryitems Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 32 Read FtotalItems Write SettotalItems;
  end;
  TUrlHistoryClass = Class of TUrlHistory;
  
  { --------------------------------------------------------------------
    TUrlHistoryitems
    --------------------------------------------------------------------}
  
  TUrlHistoryitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUrlHistoryitemsClass = Class of TUrlHistoryitems;
  
  { --------------------------------------------------------------------
    TUrlResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUrlResource, method Get
  
  TUrlGetOptions = Record
    projection : string;
    shortUrl : string;
  end;
  
  
  //Optional query Options for TUrlResource, method List
  
  TUrlListOptions = Record
    projection : string;
    starttoken : string;
  end;
  
  TUrlResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(AQuery : string  = '') : TUrl;
    Function Get(AQuery : TUrlgetOptions) : TUrl;
    Function Insert(aUrl : TUrl) : TUrl;
    Function List(AQuery : string  = '') : TUrlHistory;
    Function List(AQuery : TUrllistOptions) : TUrlHistory;
  end;
  
  
  { --------------------------------------------------------------------
    TUrlshortenerAPI
    --------------------------------------------------------------------}
  
  TUrlshortenerAPI = Class(TGoogleAPI)
  Private
    FUrlInstance : TUrlResource;
    Function GetUrlInstance : TUrlResource;virtual;
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
    Function CreateUrlResource(AOwner : TComponent) : TUrlResource;virtual;overload;
    Function CreateUrlResource : TUrlResource;virtual;overload;
    //Add default on-demand instances for resources
    Property UrlResource : TUrlResource Read GetUrlInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAnalyticsSnapshot
  --------------------------------------------------------------------}


Procedure TAnalyticsSnapshot.Setbrowsers(AIndex : Integer; AValue : TAnalyticsSnapshotbrowsers); 

begin
  If (Fbrowsers=AValue) then exit;
  Fbrowsers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyticsSnapshot.Setcountries(AIndex : Integer; AValue : TAnalyticsSnapshotcountries); 

begin
  If (Fcountries=AValue) then exit;
  Fcountries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyticsSnapshot.SetlongUrlClicks(AIndex : Integer; AValue : string); 

begin
  If (FlongUrlClicks=AValue) then exit;
  FlongUrlClicks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyticsSnapshot.Setplatforms(AIndex : Integer; AValue : TAnalyticsSnapshotplatforms); 

begin
  If (Fplatforms=AValue) then exit;
  Fplatforms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyticsSnapshot.Setreferrers(AIndex : Integer; AValue : TAnalyticsSnapshotreferrers); 

begin
  If (Freferrers=AValue) then exit;
  Freferrers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyticsSnapshot.SetshortUrlClicks(AIndex : Integer; AValue : string); 

begin
  If (FshortUrlClicks=AValue) then exit;
  FshortUrlClicks:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnalyticsSnapshotbrowsers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAnalyticsSnapshotcountries
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAnalyticsSnapshotplatforms
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAnalyticsSnapshotreferrers
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAnalyticsSummary
  --------------------------------------------------------------------}


Procedure TAnalyticsSummary.SetallTime(AIndex : Integer; AValue : TAnalyticsSnapshot); 

begin
  If (FallTime=AValue) then exit;
  FallTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyticsSummary.Setday(AIndex : Integer; AValue : TAnalyticsSnapshot); 

begin
  If (Fday=AValue) then exit;
  Fday:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyticsSummary.Setmonth(AIndex : Integer; AValue : TAnalyticsSnapshot); 

begin
  If (Fmonth=AValue) then exit;
  Fmonth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyticsSummary.SettwoHours(AIndex : Integer; AValue : TAnalyticsSnapshot); 

begin
  If (FtwoHours=AValue) then exit;
  FtwoHours:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyticsSummary.Setweek(AIndex : Integer; AValue : TAnalyticsSnapshot); 

begin
  If (Fweek=AValue) then exit;
  Fweek:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStringCount
  --------------------------------------------------------------------}


Procedure TStringCount.Setcount(AIndex : Integer; AValue : string); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStringCount.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrl
  --------------------------------------------------------------------}


Procedure TUrl.Setanalytics(AIndex : Integer; AValue : TAnalyticsSummary); 

begin
  If (Fanalytics=AValue) then exit;
  Fanalytics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrl.Setcreated(AIndex : Integer; AValue : string); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrl.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrl.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrl.SetlongUrl(AIndex : Integer; AValue : string); 

begin
  If (FlongUrl=AValue) then exit;
  FlongUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrl.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlHistory
  --------------------------------------------------------------------}


Procedure TUrlHistory.Setitems(AIndex : Integer; AValue : TUrlHistoryitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlHistory.SetitemsPerPage(AIndex : Integer; AValue : integer); 

begin
  If (FitemsPerPage=AValue) then exit;
  FitemsPerPage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlHistory.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlHistory.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlHistory.SettotalItems(AIndex : Integer; AValue : integer); 

begin
  If (FtotalItems=AValue) then exit;
  FtotalItems:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlHistoryitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUrlResource
  --------------------------------------------------------------------}


Class Function TUrlResource.ResourceName : String;

begin
  Result:='url';
end;

Class Function TUrlResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TurlshortenerAPI;
end;

Function TUrlResource.Get(AQuery : string = '') : TUrl;

Const
  _HTTPMethod = 'GET';
  _Path       = 'url';
  _Methodid   = 'urlshortener.url.get';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TUrl) as TUrl;
end;


Function TUrlResource.Get(AQuery : TUrlgetOptions) : TUrl;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'projection',AQuery.projection);
  AddToQuery(_Q,'shortUrl',AQuery.shortUrl);
  Result:=Get(_Q);
end;

Function TUrlResource.Insert(aUrl : TUrl) : TUrl;

Const
  _HTTPMethod = 'POST';
  _Path       = 'url';
  _Methodid   = 'urlshortener.url.insert';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aUrl,TUrl) as TUrl;
end;

Function TUrlResource.List(AQuery : string = '') : TUrlHistory;

Const
  _HTTPMethod = 'GET';
  _Path       = 'url/history';
  _Methodid   = 'urlshortener.url.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TUrlHistory) as TUrlHistory;
end;


Function TUrlResource.List(AQuery : TUrllistOptions) : TUrlHistory;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'projection',AQuery.projection);
  AddToQuery(_Q,'start-token',AQuery.starttoken);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TUrlshortenerAPI
  --------------------------------------------------------------------}

Class Function TUrlshortenerAPI.APIName : String;

begin
  Result:='urlshortener';
end;

Class Function TUrlshortenerAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TUrlshortenerAPI.APIRevision : String;

begin
  Result:='20150319';
end;

Class Function TUrlshortenerAPI.APIID : String;

begin
  Result:='urlshortener:v1';
end;

Class Function TUrlshortenerAPI.APITitle : String;

begin
  Result:='URL Shortener API';
end;

Class Function TUrlshortenerAPI.APIDescription : String;

begin
  Result:='Lets you create, inspect, and manage goo.gl short URLs';
end;

Class Function TUrlshortenerAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TUrlshortenerAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TUrlshortenerAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TUrlshortenerAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TUrlshortenerAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/url-shortener/v1/getting_started';
end;

Class Function TUrlshortenerAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TUrlshortenerAPI.APIbasePath : string;

begin
  Result:='/urlshortener/v1/';
end;

Class Function TUrlshortenerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/urlshortener/v1/';
end;

Class Function TUrlshortenerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TUrlshortenerAPI.APIservicePath : string;

begin
  Result:='urlshortener/v1/';
end;

Class Function TUrlshortenerAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TUrlshortenerAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/urlshortener';
  Result[0].Description:='Manage your goo.gl short URLs';
  
end;

Class Function TUrlshortenerAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TUrlshortenerAPI.RegisterAPIResources;

begin
  TAnalyticsSnapshot.RegisterObject;
  TAnalyticsSnapshotbrowsers.RegisterObject;
  TAnalyticsSnapshotcountries.RegisterObject;
  TAnalyticsSnapshotplatforms.RegisterObject;
  TAnalyticsSnapshotreferrers.RegisterObject;
  TAnalyticsSummary.RegisterObject;
  TStringCount.RegisterObject;
  TUrl.RegisterObject;
  TUrlHistory.RegisterObject;
  TUrlHistoryitems.RegisterObject;
end;


Function TUrlshortenerAPI.GetUrlInstance : TUrlResource;

begin
  if (FUrlInstance=Nil) then
    FUrlInstance:=CreateUrlResource;
  Result:=FUrlInstance;
end;

Function TUrlshortenerAPI.CreateUrlResource : TUrlResource;

begin
  Result:=CreateUrlResource(Self);
end;


Function TUrlshortenerAPI.CreateUrlResource(AOwner : TComponent) : TUrlResource;

begin
  Result:=TUrlResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TUrlshortenerAPI.RegisterAPI;
end.
