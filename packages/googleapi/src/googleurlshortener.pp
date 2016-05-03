unit googleurlshortener;
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
//Generated on: 16-5-15 08:53:09
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAnalyticsSnapshot = Class;
  TAnalyticsSummary = Class;
  TStringCount = Class;
  TUrl = Class;
  TUrlHistory = Class;
  TAnalyticsSnapshotArray = Array of TAnalyticsSnapshot;
  TAnalyticsSummaryArray = Array of TAnalyticsSummary;
  TStringCountArray = Array of TStringCount;
  TUrlArray = Array of TUrl;
  TUrlHistoryArray = Array of TUrlHistory;
  //Anonymous types, using auto-generated names
  TAnalyticsSnapshotTypebrowsersArray = Array of TStringCount;
  TAnalyticsSnapshotTypecountriesArray = Array of TStringCount;
  TAnalyticsSnapshotTypeplatformsArray = Array of TStringCount;
  TAnalyticsSnapshotTypereferrersArray = Array of TStringCount;
  TUrlHistoryTypeitemsArray = Array of TUrl;
  
  { --------------------------------------------------------------------
    TAnalyticsSnapshot
    --------------------------------------------------------------------}
  
  TAnalyticsSnapshot = Class(TGoogleBaseObject)
  Private
    Fbrowsers : TAnalyticsSnapshotTypebrowsersArray;
    Fcountries : TAnalyticsSnapshotTypecountriesArray;
    FlongUrlClicks : String;
    Fplatforms : TAnalyticsSnapshotTypeplatformsArray;
    Freferrers : TAnalyticsSnapshotTypereferrersArray;
    FshortUrlClicks : String;
  Protected
    //Property setters
    Procedure Setbrowsers(AIndex : Integer; AValue : TAnalyticsSnapshotTypebrowsersArray); virtual;
    Procedure Setcountries(AIndex : Integer; AValue : TAnalyticsSnapshotTypecountriesArray); virtual;
    Procedure SetlongUrlClicks(AIndex : Integer; AValue : String); virtual;
    Procedure Setplatforms(AIndex : Integer; AValue : TAnalyticsSnapshotTypeplatformsArray); virtual;
    Procedure Setreferrers(AIndex : Integer; AValue : TAnalyticsSnapshotTypereferrersArray); virtual;
    Procedure SetshortUrlClicks(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property browsers : TAnalyticsSnapshotTypebrowsersArray Index 0 Read Fbrowsers Write Setbrowsers;
    Property countries : TAnalyticsSnapshotTypecountriesArray Index 8 Read Fcountries Write Setcountries;
    Property longUrlClicks : String Index 16 Read FlongUrlClicks Write SetlongUrlClicks;
    Property platforms : TAnalyticsSnapshotTypeplatformsArray Index 24 Read Fplatforms Write Setplatforms;
    Property referrers : TAnalyticsSnapshotTypereferrersArray Index 32 Read Freferrers Write Setreferrers;
    Property shortUrlClicks : String Index 40 Read FshortUrlClicks Write SetshortUrlClicks;
  end;
  TAnalyticsSnapshotClass = Class of TAnalyticsSnapshot;
  
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
    Fcount : String;
    Fid : String;
  Protected
    //Property setters
    Procedure Setcount(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property count : String Index 0 Read Fcount Write Setcount;
    Property id : String Index 8 Read Fid Write Setid;
  end;
  TStringCountClass = Class of TStringCount;
  
  { --------------------------------------------------------------------
    TUrl
    --------------------------------------------------------------------}
  
  TUrl = Class(TGoogleBaseObject)
  Private
    Fanalytics : TAnalyticsSummary;
    Fcreated : String;
    Fid : String;
    Fkind : String;
    FlongUrl : String;
    Fstatus : String;
  Protected
    //Property setters
    Procedure Setanalytics(AIndex : Integer; AValue : TAnalyticsSummary); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlongUrl(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property analytics : TAnalyticsSummary Index 0 Read Fanalytics Write Setanalytics;
    Property created : String Index 8 Read Fcreated Write Setcreated;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property longUrl : String Index 32 Read FlongUrl Write SetlongUrl;
    Property status : String Index 40 Read Fstatus Write Setstatus;
  end;
  TUrlClass = Class of TUrl;
  
  { --------------------------------------------------------------------
    TUrlHistory
    --------------------------------------------------------------------}
  
  TUrlHistory = Class(TGoogleBaseObject)
  Private
    Fitems : TUrlHistoryTypeitemsArray;
    FitemsPerPage : integer;
    Fkind : String;
    FnextPageToken : String;
    FtotalItems : integer;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TUrlHistoryTypeitemsArray); virtual;
    Procedure SetitemsPerPage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SettotalItems(AIndex : Integer; AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TUrlHistoryTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property itemsPerPage : integer Index 8 Read FitemsPerPage Write SetitemsPerPage;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
    Property totalItems : integer Index 32 Read FtotalItems Write SettotalItems;
  end;
  TUrlHistoryClass = Class of TUrlHistory;
  
  { --------------------------------------------------------------------
    TUrlResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUrlResource, method Get
  
  TUrlGetOptions = Record
    projection : String;
    shortUrl : String;
  end;
  
  
  //Optional query Options for TUrlResource, method List
  
  TUrlListOptions = Record
    projection : String;
    starttoken : String;
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


Procedure TAnalyticsSnapshot.Setbrowsers(AIndex : Integer; AValue : TAnalyticsSnapshotTypebrowsersArray); 

begin
  If (Fbrowsers=AValue) then exit;
  Fbrowsers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyticsSnapshot.Setcountries(AIndex : Integer; AValue : TAnalyticsSnapshotTypecountriesArray); 

begin
  If (Fcountries=AValue) then exit;
  Fcountries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyticsSnapshot.SetlongUrlClicks(AIndex : Integer; AValue : String); 

begin
  If (FlongUrlClicks=AValue) then exit;
  FlongUrlClicks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyticsSnapshot.Setplatforms(AIndex : Integer; AValue : TAnalyticsSnapshotTypeplatformsArray); 

begin
  If (Fplatforms=AValue) then exit;
  Fplatforms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyticsSnapshot.Setreferrers(AIndex : Integer; AValue : TAnalyticsSnapshotTypereferrersArray); 

begin
  If (Freferrers=AValue) then exit;
  Freferrers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnalyticsSnapshot.SetshortUrlClicks(AIndex : Integer; AValue : String); 

begin
  If (FshortUrlClicks=AValue) then exit;
  FshortUrlClicks:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAnalyticsSnapshot.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'browsers' : SetLength(Fbrowsers,ALength);
  'countries' : SetLength(Fcountries,ALength);
  'platforms' : SetLength(Fplatforms,ALength);
  'referrers' : SetLength(Freferrers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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


Procedure TStringCount.Setcount(AIndex : Integer; AValue : String); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStringCount.Setid(AIndex : Integer; AValue : String); 

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



Procedure TUrl.Setcreated(AIndex : Integer; AValue : String); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrl.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrl.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrl.SetlongUrl(AIndex : Integer; AValue : String); 

begin
  If (FlongUrl=AValue) then exit;
  FlongUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrl.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlHistory
  --------------------------------------------------------------------}


Procedure TUrlHistory.Setitems(AIndex : Integer; AValue : TUrlHistoryTypeitemsArray); 

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



Procedure TUrlHistory.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlHistory.SetnextPageToken(AIndex : Integer; AValue : String); 

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


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUrlHistory.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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
  Result:='https://www.googleapis.com:443/';
end;

Class Function TUrlshortenerAPI.APIbasePath : string;

begin
  Result:='/urlshortener/v1/';
end;

Class Function TUrlshortenerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/urlshortener/v1/';
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
  TAnalyticsSummary.RegisterObject;
  TStringCount.RegisterObject;
  TUrl.RegisterObject;
  TUrlHistory.RegisterObject;
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
  Result.API:=Self.API;
end;



initialization
  TUrlshortenerAPI.RegisterAPI;
end.
