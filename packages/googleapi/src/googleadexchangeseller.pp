unit googleadexchangeseller;
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
//Generated on: 16-5-15 08:52:57
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAccount = Class;
  TAccounts = Class;
  TAdClient = Class;
  TAdClients = Class;
  TAlert = Class;
  TAlerts = Class;
  TCustomChannel = Class;
  TCustomChannels = Class;
  TMetadata = Class;
  TPreferredDeal = Class;
  TPreferredDeals = Class;
  TReport = Class;
  TReportingMetadataEntry = Class;
  TSavedReport = Class;
  TSavedReports = Class;
  TUrlChannel = Class;
  TUrlChannels = Class;
  TAccountArray = Array of TAccount;
  TAccountsArray = Array of TAccounts;
  TAdClientArray = Array of TAdClient;
  TAdClientsArray = Array of TAdClients;
  TAlertArray = Array of TAlert;
  TAlertsArray = Array of TAlerts;
  TCustomChannelArray = Array of TCustomChannel;
  TCustomChannelsArray = Array of TCustomChannels;
  TMetadataArray = Array of TMetadata;
  TPreferredDealArray = Array of TPreferredDeal;
  TPreferredDealsArray = Array of TPreferredDeals;
  TReportArray = Array of TReport;
  TReportingMetadataEntryArray = Array of TReportingMetadataEntry;
  TSavedReportArray = Array of TSavedReport;
  TSavedReportsArray = Array of TSavedReports;
  TUrlChannelArray = Array of TUrlChannel;
  TUrlChannelsArray = Array of TUrlChannels;
  //Anonymous types, using auto-generated names
  TCustomChannelTypetargetingInfo = Class;
  TReportTypeheadersItem = Class;
  TAccountsTypeitemsArray = Array of TAccount;
  TAdClientsTypeitemsArray = Array of TAdClient;
  TAlertsTypeitemsArray = Array of TAlert;
  TCustomChannelsTypeitemsArray = Array of TCustomChannel;
  TMetadataTypeitemsArray = Array of TReportingMetadataEntry;
  TPreferredDealsTypeitemsArray = Array of TPreferredDeal;
  TReportTypeheadersArray = Array of TReportTypeheadersItem;
  TReportTyperowsArray = Array of TStringArray;
  TSavedReportsTypeitemsArray = Array of TSavedReport;
  TUrlChannelsTypeitemsArray = Array of TUrlChannel;
  
  { --------------------------------------------------------------------
    TAccount
    --------------------------------------------------------------------}
  
  TAccount = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TAccountClass = Class of TAccount;
  
  { --------------------------------------------------------------------
    TAccounts
    --------------------------------------------------------------------}
  
  TAccounts = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TAccountsTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TAccountsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TAccountsTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TAccountsClass = Class of TAccounts;
  
  { --------------------------------------------------------------------
    TAdClient
    --------------------------------------------------------------------}
  
  TAdClient = Class(TGoogleBaseObject)
  Private
    FarcOptIn : boolean;
    Fid : String;
    Fkind : String;
    FproductCode : String;
    FsupportsReporting : boolean;
  Protected
    //Property setters
    Procedure SetarcOptIn(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetsupportsReporting(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property arcOptIn : boolean Index 0 Read FarcOptIn Write SetarcOptIn;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property productCode : String Index 24 Read FproductCode Write SetproductCode;
    Property supportsReporting : boolean Index 32 Read FsupportsReporting Write SetsupportsReporting;
  end;
  TAdClientClass = Class of TAdClient;
  
  { --------------------------------------------------------------------
    TAdClients
    --------------------------------------------------------------------}
  
  TAdClients = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TAdClientsTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TAdClientsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TAdClientsTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TAdClientsClass = Class of TAdClients;
  
  { --------------------------------------------------------------------
    TAlert
    --------------------------------------------------------------------}
  
  TAlert = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    Fmessage : String;
    Fseverity : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
    Procedure Setseverity(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property message : String Index 16 Read Fmessage Write Setmessage;
    Property severity : String Index 24 Read Fseverity Write Setseverity;
    Property _type : String Index 32 Read F_type Write Set_type;
  end;
  TAlertClass = Class of TAlert;
  
  { --------------------------------------------------------------------
    TAlerts
    --------------------------------------------------------------------}
  
  TAlerts = Class(TGoogleBaseObject)
  Private
    Fitems : TAlertsTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAlertsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TAlertsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TAlertsClass = Class of TAlerts;
  
  { --------------------------------------------------------------------
    TCustomChannelTypetargetingInfo
    --------------------------------------------------------------------}
  
  TCustomChannelTypetargetingInfo = Class(TGoogleBaseObject)
  Private
    FadsAppearOn : String;
    Fdescription : String;
    Flocation : String;
    FsiteLanguage : String;
  Protected
    //Property setters
    Procedure SetadsAppearOn(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : String); virtual;
    Procedure SetsiteLanguage(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property adsAppearOn : String Index 0 Read FadsAppearOn Write SetadsAppearOn;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property location : String Index 16 Read Flocation Write Setlocation;
    Property siteLanguage : String Index 24 Read FsiteLanguage Write SetsiteLanguage;
  end;
  TCustomChannelTypetargetingInfoClass = Class of TCustomChannelTypetargetingInfo;
  
  { --------------------------------------------------------------------
    TCustomChannel
    --------------------------------------------------------------------}
  
  TCustomChannel = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    Fid : String;
    Fkind : String;
    Fname : String;
    FtargetingInfo : TCustomChannelTypetargetingInfo;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SettargetingInfo(AIndex : Integer; AValue : TCustomChannelTypetargetingInfo); virtual;
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property name : String Index 24 Read Fname Write Setname;
    Property targetingInfo : TCustomChannelTypetargetingInfo Index 32 Read FtargetingInfo Write SettargetingInfo;
  end;
  TCustomChannelClass = Class of TCustomChannel;
  
  { --------------------------------------------------------------------
    TCustomChannels
    --------------------------------------------------------------------}
  
  TCustomChannels = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TCustomChannelsTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TCustomChannelsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TCustomChannelsTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TCustomChannelsClass = Class of TCustomChannels;
  
  { --------------------------------------------------------------------
    TMetadata
    --------------------------------------------------------------------}
  
  TMetadata = Class(TGoogleBaseObject)
  Private
    Fitems : TMetadataTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TMetadataTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TMetadataTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TMetadataClass = Class of TMetadata;
  
  { --------------------------------------------------------------------
    TPreferredDeal
    --------------------------------------------------------------------}
  
  TPreferredDeal = Class(TGoogleBaseObject)
  Private
    FadvertiserName : String;
    FbuyerNetworkName : String;
    FcurrencyCode : String;
    FendTime : String;
    FfixedCpm : String;
    Fid : String;
    Fkind : String;
    FstartTime : String;
  Protected
    //Property setters
    Procedure SetadvertiserName(AIndex : Integer; AValue : String); virtual;
    Procedure SetbuyerNetworkName(AIndex : Integer; AValue : String); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : String); virtual;
    Procedure SetfixedCpm(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property advertiserName : String Index 0 Read FadvertiserName Write SetadvertiserName;
    Property buyerNetworkName : String Index 8 Read FbuyerNetworkName Write SetbuyerNetworkName;
    Property currencyCode : String Index 16 Read FcurrencyCode Write SetcurrencyCode;
    Property endTime : String Index 24 Read FendTime Write SetendTime;
    Property fixedCpm : String Index 32 Read FfixedCpm Write SetfixedCpm;
    Property id : String Index 40 Read Fid Write Setid;
    Property kind : String Index 48 Read Fkind Write Setkind;
    Property startTime : String Index 56 Read FstartTime Write SetstartTime;
  end;
  TPreferredDealClass = Class of TPreferredDeal;
  
  { --------------------------------------------------------------------
    TPreferredDeals
    --------------------------------------------------------------------}
  
  TPreferredDeals = Class(TGoogleBaseObject)
  Private
    Fitems : TPreferredDealsTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TPreferredDealsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TPreferredDealsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TPreferredDealsClass = Class of TPreferredDeals;
  
  { --------------------------------------------------------------------
    TReportTypeheadersItem
    --------------------------------------------------------------------}
  
  TReportTypeheadersItem = Class(TGoogleBaseObject)
  Private
    Fcurrency : String;
    Fname : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcurrency(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property currency : String Index 0 Read Fcurrency Write Setcurrency;
    Property name : String Index 8 Read Fname Write Setname;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TReportTypeheadersItemClass = Class of TReportTypeheadersItem;
  
  { --------------------------------------------------------------------
    TReport
    --------------------------------------------------------------------}
  
  TReport = Class(TGoogleBaseObject)
  Private
    Faverages : TStringArray;
    Fheaders : TReportTypeheadersArray;
    Fkind : String;
    Frows : TReportTyperowsArray;
    FtotalMatchedRows : String;
    Ftotals : TStringArray;
    Fwarnings : TStringArray;
  Protected
    //Property setters
    Procedure Setaverages(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setheaders(AIndex : Integer; AValue : TReportTypeheadersArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TReportTyperowsArray); virtual;
    Procedure SettotalMatchedRows(AIndex : Integer; AValue : String); virtual;
    Procedure Settotals(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property averages : TStringArray Index 0 Read Faverages Write Setaverages;
    Property headers : TReportTypeheadersArray Index 8 Read Fheaders Write Setheaders;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property rows : TReportTyperowsArray Index 24 Read Frows Write Setrows;
    Property totalMatchedRows : String Index 32 Read FtotalMatchedRows Write SettotalMatchedRows;
    Property totals : TStringArray Index 40 Read Ftotals Write Settotals;
    Property warnings : TStringArray Index 48 Read Fwarnings Write Setwarnings;
  end;
  TReportClass = Class of TReport;
  
  { --------------------------------------------------------------------
    TReportingMetadataEntry
    --------------------------------------------------------------------}
  
  TReportingMetadataEntry = Class(TGoogleBaseObject)
  Private
    FcompatibleDimensions : TStringArray;
    FcompatibleMetrics : TStringArray;
    Fid : String;
    Fkind : String;
    FrequiredDimensions : TStringArray;
    FrequiredMetrics : TStringArray;
    FsupportedProducts : TStringArray;
  Protected
    //Property setters
    Procedure SetcompatibleDimensions(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetcompatibleMetrics(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetrequiredDimensions(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetrequiredMetrics(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetsupportedProducts(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property compatibleDimensions : TStringArray Index 0 Read FcompatibleDimensions Write SetcompatibleDimensions;
    Property compatibleMetrics : TStringArray Index 8 Read FcompatibleMetrics Write SetcompatibleMetrics;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property requiredDimensions : TStringArray Index 32 Read FrequiredDimensions Write SetrequiredDimensions;
    Property requiredMetrics : TStringArray Index 40 Read FrequiredMetrics Write SetrequiredMetrics;
    Property supportedProducts : TStringArray Index 48 Read FsupportedProducts Write SetsupportedProducts;
  end;
  TReportingMetadataEntryClass = Class of TReportingMetadataEntry;
  
  { --------------------------------------------------------------------
    TSavedReport
    --------------------------------------------------------------------}
  
  TSavedReport = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TSavedReportClass = Class of TSavedReport;
  
  { --------------------------------------------------------------------
    TSavedReports
    --------------------------------------------------------------------}
  
  TSavedReports = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TSavedReportsTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TSavedReportsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TSavedReportsTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TSavedReportsClass = Class of TSavedReports;
  
  { --------------------------------------------------------------------
    TUrlChannel
    --------------------------------------------------------------------}
  
  TUrlChannel = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    FurlPattern : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SeturlPattern(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property urlPattern : String Index 16 Read FurlPattern Write SeturlPattern;
  end;
  TUrlChannelClass = Class of TUrlChannel;
  
  { --------------------------------------------------------------------
    TUrlChannels
    --------------------------------------------------------------------}
  
  TUrlChannels = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TUrlChannelsTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TUrlChannelsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TUrlChannelsTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TUrlChannelsClass = Class of TUrlChannels;
  
  { --------------------------------------------------------------------
    TAccountsAdclientsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsAdclientsResource, method List
  
  TAccountsAdclientsListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TAccountsAdclientsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(accountId: string; AQuery : string  = '') : TAdClients;
    Function List(accountId: string; AQuery : TAccountsAdclientslistOptions) : TAdClients;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsAlertsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsAlertsResource, method List
  
  TAccountsAlertsListOptions = Record
    locale : String;
  end;
  
  TAccountsAlertsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(accountId: string; AQuery : string  = '') : TAlerts;
    Function List(accountId: string; AQuery : TAccountsAlertslistOptions) : TAlerts;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsCustomchannelsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsCustomchannelsResource, method List
  
  TAccountsCustomchannelsListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TAccountsCustomchannelsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; adClientId: string; customChannelId: string) : TCustomChannel;
    Function List(accountId: string; adClientId: string; AQuery : string  = '') : TCustomChannels;
    Function List(accountId: string; adClientId: string; AQuery : TAccountsCustomchannelslistOptions) : TCustomChannels;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsMetadataDimensionsResource
    --------------------------------------------------------------------}
  
  TAccountsMetadataDimensionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(accountId: string) : TMetadata;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsMetadataMetricsResource
    --------------------------------------------------------------------}
  
  TAccountsMetadataMetricsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(accountId: string) : TMetadata;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsMetadataResource
    --------------------------------------------------------------------}
  
  TAccountsMetadataResource = Class(TGoogleResource)
  Private
    FDimensionsInstance : TAccountsMetadataDimensionsResource;
    FMetricsInstance : TAccountsMetadataMetricsResource;
    Function GetDimensionsInstance : TAccountsMetadataDimensionsResource;virtual;
    Function GetMetricsInstance : TAccountsMetadataMetricsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateDimensionsResource(AOwner : TComponent) : TAccountsMetadataDimensionsResource;virtual;overload;
    Function CreateDimensionsResource : TAccountsMetadataDimensionsResource;virtual;overload;
    Function CreateMetricsResource(AOwner : TComponent) : TAccountsMetadataMetricsResource;virtual;overload;
    Function CreateMetricsResource : TAccountsMetadataMetricsResource;virtual;overload;
    Property DimensionsResource : TAccountsMetadataDimensionsResource Read GetDimensionsInstance;
    Property MetricsResource : TAccountsMetadataMetricsResource Read GetMetricsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsPreferreddealsResource
    --------------------------------------------------------------------}
  
  TAccountsPreferreddealsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; dealId: string) : TPreferredDeal;
    Function List(accountId: string) : TPreferredDeals;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsReportsSavedResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsReportsSavedResource, method Generate
  
  TAccountsReportsSavedGenerateOptions = Record
    locale : String;
    maxResults : integer;
    startIndex : integer;
  end;
  
  
  //Optional query Options for TAccountsReportsSavedResource, method List
  
  TAccountsReportsSavedListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TAccountsReportsSavedResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Generate(accountId: string; savedReportId: string; AQuery : string  = '') : TReport;
    Function Generate(accountId: string; savedReportId: string; AQuery : TAccountsReportsSavedgenerateOptions) : TReport;
    Function List(accountId: string; AQuery : string  = '') : TSavedReports;
    Function List(accountId: string; AQuery : TAccountsReportsSavedlistOptions) : TSavedReports;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsReportsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsReportsResource, method Generate
  
  TAccountsReportsGenerateOptions = Record
    dimension : String;
    endDate : String;
    filter : String;
    locale : String;
    maxResults : integer;
    metric : String;
    sort : String;
    startDate : String;
    startIndex : integer;
  end;
  
  TAccountsReportsResource = Class(TGoogleResource)
  Private
    FSavedInstance : TAccountsReportsSavedResource;
    Function GetSavedInstance : TAccountsReportsSavedResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Generate(accountId: string; AQuery : string  = '') : TReport;
    Function Generate(accountId: string; AQuery : TAccountsReportsgenerateOptions) : TReport;
    Function CreateSavedResource(AOwner : TComponent) : TAccountsReportsSavedResource;virtual;overload;
    Function CreateSavedResource : TAccountsReportsSavedResource;virtual;overload;
    Property SavedResource : TAccountsReportsSavedResource Read GetSavedInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsUrlchannelsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsUrlchannelsResource, method List
  
  TAccountsUrlchannelsListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TAccountsUrlchannelsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(accountId: string; adClientId: string; AQuery : string  = '') : TUrlChannels;
    Function List(accountId: string; adClientId: string; AQuery : TAccountsUrlchannelslistOptions) : TUrlChannels;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsResource, method List
  
  TAccountsListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TAccountsResource = Class(TGoogleResource)
  Private
    FAdclientsInstance : TAccountsAdclientsResource;
    FAlertsInstance : TAccountsAlertsResource;
    FCustomchannelsInstance : TAccountsCustomchannelsResource;
    FMetadataDimensionsInstance : TAccountsMetadataDimensionsResource;
    FMetadataMetricsInstance : TAccountsMetadataMetricsResource;
    FMetadataInstance : TAccountsMetadataResource;
    FPreferreddealsInstance : TAccountsPreferreddealsResource;
    FReportsSavedInstance : TAccountsReportsSavedResource;
    FReportsInstance : TAccountsReportsResource;
    FUrlchannelsInstance : TAccountsUrlchannelsResource;
    Function GetAdclientsInstance : TAccountsAdclientsResource;virtual;
    Function GetAlertsInstance : TAccountsAlertsResource;virtual;
    Function GetCustomchannelsInstance : TAccountsCustomchannelsResource;virtual;
    Function GetMetadataDimensionsInstance : TAccountsMetadataDimensionsResource;virtual;
    Function GetMetadataMetricsInstance : TAccountsMetadataMetricsResource;virtual;
    Function GetMetadataInstance : TAccountsMetadataResource;virtual;
    Function GetPreferreddealsInstance : TAccountsPreferreddealsResource;virtual;
    Function GetReportsSavedInstance : TAccountsReportsSavedResource;virtual;
    Function GetReportsInstance : TAccountsReportsResource;virtual;
    Function GetUrlchannelsInstance : TAccountsUrlchannelsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string) : TAccount;
    Function List(AQuery : string  = '') : TAccounts;
    Function List(AQuery : TAccountslistOptions) : TAccounts;
    Function CreateAdclientsResource(AOwner : TComponent) : TAccountsAdclientsResource;virtual;overload;
    Function CreateAdclientsResource : TAccountsAdclientsResource;virtual;overload;
    Function CreateAlertsResource(AOwner : TComponent) : TAccountsAlertsResource;virtual;overload;
    Function CreateAlertsResource : TAccountsAlertsResource;virtual;overload;
    Function CreateCustomchannelsResource(AOwner : TComponent) : TAccountsCustomchannelsResource;virtual;overload;
    Function CreateCustomchannelsResource : TAccountsCustomchannelsResource;virtual;overload;
    Function CreateMetadataDimensionsResource(AOwner : TComponent) : TAccountsMetadataDimensionsResource;virtual;overload;
    Function CreateMetadataDimensionsResource : TAccountsMetadataDimensionsResource;virtual;overload;
    Function CreateMetadataMetricsResource(AOwner : TComponent) : TAccountsMetadataMetricsResource;virtual;overload;
    Function CreateMetadataMetricsResource : TAccountsMetadataMetricsResource;virtual;overload;
    Function CreateMetadataResource(AOwner : TComponent) : TAccountsMetadataResource;virtual;overload;
    Function CreateMetadataResource : TAccountsMetadataResource;virtual;overload;
    Function CreatePreferreddealsResource(AOwner : TComponent) : TAccountsPreferreddealsResource;virtual;overload;
    Function CreatePreferreddealsResource : TAccountsPreferreddealsResource;virtual;overload;
    Function CreateReportsSavedResource(AOwner : TComponent) : TAccountsReportsSavedResource;virtual;overload;
    Function CreateReportsSavedResource : TAccountsReportsSavedResource;virtual;overload;
    Function CreateReportsResource(AOwner : TComponent) : TAccountsReportsResource;virtual;overload;
    Function CreateReportsResource : TAccountsReportsResource;virtual;overload;
    Function CreateUrlchannelsResource(AOwner : TComponent) : TAccountsUrlchannelsResource;virtual;overload;
    Function CreateUrlchannelsResource : TAccountsUrlchannelsResource;virtual;overload;
    Property AdclientsResource : TAccountsAdclientsResource Read GetAdclientsInstance;
    Property AlertsResource : TAccountsAlertsResource Read GetAlertsInstance;
    Property CustomchannelsResource : TAccountsCustomchannelsResource Read GetCustomchannelsInstance;
    Property MetadataDimensionsResource : TAccountsMetadataDimensionsResource Read GetMetadataDimensionsInstance;
    Property MetadataMetricsResource : TAccountsMetadataMetricsResource Read GetMetadataMetricsInstance;
    Property MetadataResource : TAccountsMetadataResource Read GetMetadataInstance;
    Property PreferreddealsResource : TAccountsPreferreddealsResource Read GetPreferreddealsInstance;
    Property ReportsSavedResource : TAccountsReportsSavedResource Read GetReportsSavedInstance;
    Property ReportsResource : TAccountsReportsResource Read GetReportsInstance;
    Property UrlchannelsResource : TAccountsUrlchannelsResource Read GetUrlchannelsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAdexchangesellerAPI
    --------------------------------------------------------------------}
  
  TAdexchangesellerAPI = Class(TGoogleAPI)
  Private
    FAccountsAdclientsInstance : TAccountsAdclientsResource;
    FAccountsAlertsInstance : TAccountsAlertsResource;
    FAccountsCustomchannelsInstance : TAccountsCustomchannelsResource;
    FAccountsMetadataDimensionsInstance : TAccountsMetadataDimensionsResource;
    FAccountsMetadataMetricsInstance : TAccountsMetadataMetricsResource;
    FAccountsMetadataInstance : TAccountsMetadataResource;
    FAccountsPreferreddealsInstance : TAccountsPreferreddealsResource;
    FAccountsReportsSavedInstance : TAccountsReportsSavedResource;
    FAccountsReportsInstance : TAccountsReportsResource;
    FAccountsUrlchannelsInstance : TAccountsUrlchannelsResource;
    FAccountsInstance : TAccountsResource;
    Function GetAccountsAdclientsInstance : TAccountsAdclientsResource;virtual;
    Function GetAccountsAlertsInstance : TAccountsAlertsResource;virtual;
    Function GetAccountsCustomchannelsInstance : TAccountsCustomchannelsResource;virtual;
    Function GetAccountsMetadataDimensionsInstance : TAccountsMetadataDimensionsResource;virtual;
    Function GetAccountsMetadataMetricsInstance : TAccountsMetadataMetricsResource;virtual;
    Function GetAccountsMetadataInstance : TAccountsMetadataResource;virtual;
    Function GetAccountsPreferreddealsInstance : TAccountsPreferreddealsResource;virtual;
    Function GetAccountsReportsSavedInstance : TAccountsReportsSavedResource;virtual;
    Function GetAccountsReportsInstance : TAccountsReportsResource;virtual;
    Function GetAccountsUrlchannelsInstance : TAccountsUrlchannelsResource;virtual;
    Function GetAccountsInstance : TAccountsResource;virtual;
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
    Function CreateAccountsAdclientsResource(AOwner : TComponent) : TAccountsAdclientsResource;virtual;overload;
    Function CreateAccountsAdclientsResource : TAccountsAdclientsResource;virtual;overload;
    Function CreateAccountsAlertsResource(AOwner : TComponent) : TAccountsAlertsResource;virtual;overload;
    Function CreateAccountsAlertsResource : TAccountsAlertsResource;virtual;overload;
    Function CreateAccountsCustomchannelsResource(AOwner : TComponent) : TAccountsCustomchannelsResource;virtual;overload;
    Function CreateAccountsCustomchannelsResource : TAccountsCustomchannelsResource;virtual;overload;
    Function CreateAccountsMetadataDimensionsResource(AOwner : TComponent) : TAccountsMetadataDimensionsResource;virtual;overload;
    Function CreateAccountsMetadataDimensionsResource : TAccountsMetadataDimensionsResource;virtual;overload;
    Function CreateAccountsMetadataMetricsResource(AOwner : TComponent) : TAccountsMetadataMetricsResource;virtual;overload;
    Function CreateAccountsMetadataMetricsResource : TAccountsMetadataMetricsResource;virtual;overload;
    Function CreateAccountsMetadataResource(AOwner : TComponent) : TAccountsMetadataResource;virtual;overload;
    Function CreateAccountsMetadataResource : TAccountsMetadataResource;virtual;overload;
    Function CreateAccountsPreferreddealsResource(AOwner : TComponent) : TAccountsPreferreddealsResource;virtual;overload;
    Function CreateAccountsPreferreddealsResource : TAccountsPreferreddealsResource;virtual;overload;
    Function CreateAccountsReportsSavedResource(AOwner : TComponent) : TAccountsReportsSavedResource;virtual;overload;
    Function CreateAccountsReportsSavedResource : TAccountsReportsSavedResource;virtual;overload;
    Function CreateAccountsReportsResource(AOwner : TComponent) : TAccountsReportsResource;virtual;overload;
    Function CreateAccountsReportsResource : TAccountsReportsResource;virtual;overload;
    Function CreateAccountsUrlchannelsResource(AOwner : TComponent) : TAccountsUrlchannelsResource;virtual;overload;
    Function CreateAccountsUrlchannelsResource : TAccountsUrlchannelsResource;virtual;overload;
    Function CreateAccountsResource(AOwner : TComponent) : TAccountsResource;virtual;overload;
    Function CreateAccountsResource : TAccountsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AccountsAdclientsResource : TAccountsAdclientsResource Read GetAccountsAdclientsInstance;
    Property AccountsAlertsResource : TAccountsAlertsResource Read GetAccountsAlertsInstance;
    Property AccountsCustomchannelsResource : TAccountsCustomchannelsResource Read GetAccountsCustomchannelsInstance;
    Property AccountsMetadataDimensionsResource : TAccountsMetadataDimensionsResource Read GetAccountsMetadataDimensionsInstance;
    Property AccountsMetadataMetricsResource : TAccountsMetadataMetricsResource Read GetAccountsMetadataMetricsInstance;
    Property AccountsMetadataResource : TAccountsMetadataResource Read GetAccountsMetadataInstance;
    Property AccountsPreferreddealsResource : TAccountsPreferreddealsResource Read GetAccountsPreferreddealsInstance;
    Property AccountsReportsSavedResource : TAccountsReportsSavedResource Read GetAccountsReportsSavedInstance;
    Property AccountsReportsResource : TAccountsReportsResource Read GetAccountsReportsInstance;
    Property AccountsUrlchannelsResource : TAccountsUrlchannelsResource Read GetAccountsUrlchannelsInstance;
    Property AccountsResource : TAccountsResource Read GetAccountsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAccount
  --------------------------------------------------------------------}


Procedure TAccount.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccounts
  --------------------------------------------------------------------}


Procedure TAccounts.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounts.Setitems(AIndex : Integer; AValue : TAccountsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounts.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounts.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccounts.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAdClient
  --------------------------------------------------------------------}


Procedure TAdClient.SetarcOptIn(AIndex : Integer; AValue : boolean); 

begin
  If (FarcOptIn=AValue) then exit;
  FarcOptIn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdClient.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdClient.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdClient.SetproductCode(AIndex : Integer; AValue : String); 

begin
  If (FproductCode=AValue) then exit;
  FproductCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdClient.SetsupportsReporting(AIndex : Integer; AValue : boolean); 

begin
  If (FsupportsReporting=AValue) then exit;
  FsupportsReporting:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdClients
  --------------------------------------------------------------------}


Procedure TAdClients.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdClients.Setitems(AIndex : Integer; AValue : TAdClientsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdClients.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdClients.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAdClients.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAlert
  --------------------------------------------------------------------}


Procedure TAlert.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAlert.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAlert.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAlert.Setseverity(AIndex : Integer; AValue : String); 

begin
  If (Fseverity=AValue) then exit;
  Fseverity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAlert.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAlert.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAlerts
  --------------------------------------------------------------------}


Procedure TAlerts.Setitems(AIndex : Integer; AValue : TAlertsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAlerts.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAlerts.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCustomChannelTypetargetingInfo
  --------------------------------------------------------------------}


Procedure TCustomChannelTypetargetingInfo.SetadsAppearOn(AIndex : Integer; AValue : String); 

begin
  If (FadsAppearOn=AValue) then exit;
  FadsAppearOn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannelTypetargetingInfo.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannelTypetargetingInfo.Setlocation(AIndex : Integer; AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannelTypetargetingInfo.SetsiteLanguage(AIndex : Integer; AValue : String); 

begin
  If (FsiteLanguage=AValue) then exit;
  FsiteLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomChannel
  --------------------------------------------------------------------}


Procedure TCustomChannel.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannel.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannel.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannel.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannel.SettargetingInfo(AIndex : Integer; AValue : TCustomChannelTypetargetingInfo); 

begin
  If (FtargetingInfo=AValue) then exit;
  FtargetingInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomChannels
  --------------------------------------------------------------------}


Procedure TCustomChannels.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannels.Setitems(AIndex : Integer; AValue : TCustomChannelsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannels.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannels.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCustomChannels.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMetadata
  --------------------------------------------------------------------}


Procedure TMetadata.Setitems(AIndex : Integer; AValue : TMetadataTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMetadata.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPreferredDeal
  --------------------------------------------------------------------}


Procedure TPreferredDeal.SetadvertiserName(AIndex : Integer; AValue : String); 

begin
  If (FadvertiserName=AValue) then exit;
  FadvertiserName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPreferredDeal.SetbuyerNetworkName(AIndex : Integer; AValue : String); 

begin
  If (FbuyerNetworkName=AValue) then exit;
  FbuyerNetworkName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPreferredDeal.SetcurrencyCode(AIndex : Integer; AValue : String); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPreferredDeal.SetendTime(AIndex : Integer; AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPreferredDeal.SetfixedCpm(AIndex : Integer; AValue : String); 

begin
  If (FfixedCpm=AValue) then exit;
  FfixedCpm:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPreferredDeal.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPreferredDeal.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPreferredDeal.SetstartTime(AIndex : Integer; AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPreferredDeals
  --------------------------------------------------------------------}


Procedure TPreferredDeals.Setitems(AIndex : Integer; AValue : TPreferredDealsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPreferredDeals.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPreferredDeals.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReportTypeheadersItem
  --------------------------------------------------------------------}


Procedure TReportTypeheadersItem.Setcurrency(AIndex : Integer; AValue : String); 

begin
  If (Fcurrency=AValue) then exit;
  Fcurrency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypeheadersItem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportTypeheadersItem.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TReportTypeheadersItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TReport
  --------------------------------------------------------------------}


Procedure TReport.Setaverages(AIndex : Integer; AValue : TStringArray); 

begin
  If (Faverages=AValue) then exit;
  Faverages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setheaders(AIndex : Integer; AValue : TReportTypeheadersArray); 

begin
  If (Fheaders=AValue) then exit;
  Fheaders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setrows(AIndex : Integer; AValue : TReportTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SettotalMatchedRows(AIndex : Integer; AValue : String); 

begin
  If (FtotalMatchedRows=AValue) then exit;
  FtotalMatchedRows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Settotals(AIndex : Integer; AValue : TStringArray); 

begin
  If (Ftotals=AValue) then exit;
  Ftotals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setwarnings(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TReport.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'averages' : SetLength(Faverages,ALength);
  'headers' : SetLength(Fheaders,ALength);
  'rows' : SetLength(Frows,ALength);
  'totals' : SetLength(Ftotals,ALength);
  'warnings' : SetLength(Fwarnings,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReportingMetadataEntry
  --------------------------------------------------------------------}


Procedure TReportingMetadataEntry.SetcompatibleDimensions(AIndex : Integer; AValue : TStringArray); 

begin
  If (FcompatibleDimensions=AValue) then exit;
  FcompatibleDimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportingMetadataEntry.SetcompatibleMetrics(AIndex : Integer; AValue : TStringArray); 

begin
  If (FcompatibleMetrics=AValue) then exit;
  FcompatibleMetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportingMetadataEntry.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportingMetadataEntry.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportingMetadataEntry.SetrequiredDimensions(AIndex : Integer; AValue : TStringArray); 

begin
  If (FrequiredDimensions=AValue) then exit;
  FrequiredDimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportingMetadataEntry.SetrequiredMetrics(AIndex : Integer; AValue : TStringArray); 

begin
  If (FrequiredMetrics=AValue) then exit;
  FrequiredMetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportingMetadataEntry.SetsupportedProducts(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsupportedProducts=AValue) then exit;
  FsupportedProducts:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TReportingMetadataEntry.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'compatibledimensions' : SetLength(FcompatibleDimensions,ALength);
  'compatiblemetrics' : SetLength(FcompatibleMetrics,ALength);
  'requireddimensions' : SetLength(FrequiredDimensions,ALength);
  'requiredmetrics' : SetLength(FrequiredMetrics,ALength);
  'supportedproducts' : SetLength(FsupportedProducts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSavedReport
  --------------------------------------------------------------------}


Procedure TSavedReport.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedReport.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedReport.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSavedReports
  --------------------------------------------------------------------}


Procedure TSavedReports.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedReports.Setitems(AIndex : Integer; AValue : TSavedReportsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedReports.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedReports.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSavedReports.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TUrlChannel
  --------------------------------------------------------------------}


Procedure TUrlChannel.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlChannel.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlChannel.SeturlPattern(AIndex : Integer; AValue : String); 

begin
  If (FurlPattern=AValue) then exit;
  FurlPattern:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlChannels
  --------------------------------------------------------------------}


Procedure TUrlChannels.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlChannels.Setitems(AIndex : Integer; AValue : TUrlChannelsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlChannels.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlChannels.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TUrlChannels.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountsAdclientsResource
  --------------------------------------------------------------------}


Class Function TAccountsAdclientsResource.ResourceName : String;

begin
  Result:='adclients';
end;

Class Function TAccountsAdclientsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangesellerAPI;
end;

Function TAccountsAdclientsResource.List(accountId: string; AQuery : string = '') : TAdClients;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/adclients';
  _Methodid   = 'adexchangeseller.accounts.adclients.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAdClients) as TAdClients;
end;


Function TAccountsAdclientsResource.List(accountId: string; AQuery : TAccountsAdclientslistOptions) : TAdClients;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(accountId,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsAlertsResource
  --------------------------------------------------------------------}


Class Function TAccountsAlertsResource.ResourceName : String;

begin
  Result:='alerts';
end;

Class Function TAccountsAlertsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangesellerAPI;
end;

Function TAccountsAlertsResource.List(accountId: string; AQuery : string = '') : TAlerts;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/alerts';
  _Methodid   = 'adexchangeseller.accounts.alerts.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAlerts) as TAlerts;
end;


Function TAccountsAlertsResource.List(accountId: string; AQuery : TAccountsAlertslistOptions) : TAlerts;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'locale',AQuery.locale);
  Result:=List(accountId,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsCustomchannelsResource
  --------------------------------------------------------------------}


Class Function TAccountsCustomchannelsResource.ResourceName : String;

begin
  Result:='customchannels';
end;

Class Function TAccountsCustomchannelsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangesellerAPI;
end;

Function TAccountsCustomchannelsResource.Get(accountId: string; adClientId: string; customChannelId: string) : TCustomChannel;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/adclients/{adClientId}/customchannels/{customChannelId}';
  _Methodid   = 'adexchangeseller.accounts.customchannels.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'adClientId',adClientId,'customChannelId',customChannelId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCustomChannel) as TCustomChannel;
end;

Function TAccountsCustomchannelsResource.List(accountId: string; adClientId: string; AQuery : string = '') : TCustomChannels;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/adclients/{adClientId}/customchannels';
  _Methodid   = 'adexchangeseller.accounts.customchannels.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'adClientId',adClientId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCustomChannels) as TCustomChannels;
end;


Function TAccountsCustomchannelsResource.List(accountId: string; adClientId: string; AQuery : TAccountsCustomchannelslistOptions) : TCustomChannels;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(accountId,adClientId,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsMetadataDimensionsResource
  --------------------------------------------------------------------}


Class Function TAccountsMetadataDimensionsResource.ResourceName : String;

begin
  Result:='dimensions';
end;

Class Function TAccountsMetadataDimensionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangesellerAPI;
end;

Function TAccountsMetadataDimensionsResource.List(accountId: string) : TMetadata;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/metadata/dimensions';
  _Methodid   = 'adexchangeseller.accounts.metadata.dimensions.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TMetadata) as TMetadata;
end;



{ --------------------------------------------------------------------
  TAccountsMetadataMetricsResource
  --------------------------------------------------------------------}


Class Function TAccountsMetadataMetricsResource.ResourceName : String;

begin
  Result:='metrics';
end;

Class Function TAccountsMetadataMetricsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangesellerAPI;
end;

Function TAccountsMetadataMetricsResource.List(accountId: string) : TMetadata;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/metadata/metrics';
  _Methodid   = 'adexchangeseller.accounts.metadata.metrics.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TMetadata) as TMetadata;
end;



{ --------------------------------------------------------------------
  TAccountsMetadataResource
  --------------------------------------------------------------------}


Class Function TAccountsMetadataResource.ResourceName : String;

begin
  Result:='metadata';
end;

Class Function TAccountsMetadataResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangesellerAPI;
end;



Function TAccountsMetadataResource.GetDimensionsInstance : TAccountsMetadataDimensionsResource;

begin
  if (FDimensionsInstance=Nil) then
    FDimensionsInstance:=CreateDimensionsResource;
  Result:=FDimensionsInstance;
end;

Function TAccountsMetadataResource.CreateDimensionsResource : TAccountsMetadataDimensionsResource;

begin
  Result:=CreateDimensionsResource(Self);
end;


Function TAccountsMetadataResource.CreateDimensionsResource(AOwner : TComponent) : TAccountsMetadataDimensionsResource;

begin
  Result:=TAccountsMetadataDimensionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsMetadataResource.GetMetricsInstance : TAccountsMetadataMetricsResource;

begin
  if (FMetricsInstance=Nil) then
    FMetricsInstance:=CreateMetricsResource;
  Result:=FMetricsInstance;
end;

Function TAccountsMetadataResource.CreateMetricsResource : TAccountsMetadataMetricsResource;

begin
  Result:=CreateMetricsResource(Self);
end;


Function TAccountsMetadataResource.CreateMetricsResource(AOwner : TComponent) : TAccountsMetadataMetricsResource;

begin
  Result:=TAccountsMetadataMetricsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TAccountsPreferreddealsResource
  --------------------------------------------------------------------}


Class Function TAccountsPreferreddealsResource.ResourceName : String;

begin
  Result:='preferreddeals';
end;

Class Function TAccountsPreferreddealsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangesellerAPI;
end;

Function TAccountsPreferreddealsResource.Get(accountId: string; dealId: string) : TPreferredDeal;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/preferreddeals/{dealId}';
  _Methodid   = 'adexchangeseller.accounts.preferreddeals.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'dealId',dealId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPreferredDeal) as TPreferredDeal;
end;

Function TAccountsPreferreddealsResource.List(accountId: string) : TPreferredDeals;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/preferreddeals';
  _Methodid   = 'adexchangeseller.accounts.preferreddeals.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPreferredDeals) as TPreferredDeals;
end;



{ --------------------------------------------------------------------
  TAccountsReportsSavedResource
  --------------------------------------------------------------------}


Class Function TAccountsReportsSavedResource.ResourceName : String;

begin
  Result:='saved';
end;

Class Function TAccountsReportsSavedResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangesellerAPI;
end;

Function TAccountsReportsSavedResource.Generate(accountId: string; savedReportId: string; AQuery : string = '') : TReport;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/reports/{savedReportId}';
  _Methodid   = 'adexchangeseller.accounts.reports.saved.generate';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'savedReportId',savedReportId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TReport) as TReport;
end;


Function TAccountsReportsSavedResource.Generate(accountId: string; savedReportId: string; AQuery : TAccountsReportsSavedgenerateOptions) : TReport;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  Result:=Generate(accountId,savedReportId,_Q);
end;

Function TAccountsReportsSavedResource.List(accountId: string; AQuery : string = '') : TSavedReports;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/reports/saved';
  _Methodid   = 'adexchangeseller.accounts.reports.saved.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TSavedReports) as TSavedReports;
end;


Function TAccountsReportsSavedResource.List(accountId: string; AQuery : TAccountsReportsSavedlistOptions) : TSavedReports;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(accountId,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsReportsResource
  --------------------------------------------------------------------}


Class Function TAccountsReportsResource.ResourceName : String;

begin
  Result:='reports';
end;

Class Function TAccountsReportsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangesellerAPI;
end;

Function TAccountsReportsResource.Generate(accountId: string; AQuery : string = '') : TReport;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/reports';
  _Methodid   = 'adexchangeseller.accounts.reports.generate';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TReport) as TReport;
end;


Function TAccountsReportsResource.Generate(accountId: string; AQuery : TAccountsReportsgenerateOptions) : TReport;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'dimension',AQuery.dimension);
  AddToQuery(_Q,'endDate',AQuery.endDate);
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'metric',AQuery.metric);
  AddToQuery(_Q,'sort',AQuery.sort);
  AddToQuery(_Q,'startDate',AQuery.startDate);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  Result:=Generate(accountId,_Q);
end;



Function TAccountsReportsResource.GetSavedInstance : TAccountsReportsSavedResource;

begin
  if (FSavedInstance=Nil) then
    FSavedInstance:=CreateSavedResource;
  Result:=FSavedInstance;
end;

Function TAccountsReportsResource.CreateSavedResource : TAccountsReportsSavedResource;

begin
  Result:=CreateSavedResource(Self);
end;


Function TAccountsReportsResource.CreateSavedResource(AOwner : TComponent) : TAccountsReportsSavedResource;

begin
  Result:=TAccountsReportsSavedResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TAccountsUrlchannelsResource
  --------------------------------------------------------------------}


Class Function TAccountsUrlchannelsResource.ResourceName : String;

begin
  Result:='urlchannels';
end;

Class Function TAccountsUrlchannelsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangesellerAPI;
end;

Function TAccountsUrlchannelsResource.List(accountId: string; adClientId: string; AQuery : string = '') : TUrlChannels;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/adclients/{adClientId}/urlchannels';
  _Methodid   = 'adexchangeseller.accounts.urlchannels.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'adClientId',adClientId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TUrlChannels) as TUrlChannels;
end;


Function TAccountsUrlchannelsResource.List(accountId: string; adClientId: string; AQuery : TAccountsUrlchannelslistOptions) : TUrlChannels;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(accountId,adClientId,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsResource
  --------------------------------------------------------------------}


Class Function TAccountsResource.ResourceName : String;

begin
  Result:='accounts';
end;

Class Function TAccountsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadexchangesellerAPI;
end;

Function TAccountsResource.Get(accountId: string) : TAccount;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}';
  _Methodid   = 'adexchangeseller.accounts.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAccount) as TAccount;
end;

Function TAccountsResource.List(AQuery : string = '') : TAccounts;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts';
  _Methodid   = 'adexchangeseller.accounts.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TAccounts) as TAccounts;
end;


Function TAccountsResource.List(AQuery : TAccountslistOptions) : TAccounts;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;



Function TAccountsResource.GetAdclientsInstance : TAccountsAdclientsResource;

begin
  if (FAdclientsInstance=Nil) then
    FAdclientsInstance:=CreateAdclientsResource;
  Result:=FAdclientsInstance;
end;

Function TAccountsResource.CreateAdclientsResource : TAccountsAdclientsResource;

begin
  Result:=CreateAdclientsResource(Self);
end;


Function TAccountsResource.CreateAdclientsResource(AOwner : TComponent) : TAccountsAdclientsResource;

begin
  Result:=TAccountsAdclientsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetAlertsInstance : TAccountsAlertsResource;

begin
  if (FAlertsInstance=Nil) then
    FAlertsInstance:=CreateAlertsResource;
  Result:=FAlertsInstance;
end;

Function TAccountsResource.CreateAlertsResource : TAccountsAlertsResource;

begin
  Result:=CreateAlertsResource(Self);
end;


Function TAccountsResource.CreateAlertsResource(AOwner : TComponent) : TAccountsAlertsResource;

begin
  Result:=TAccountsAlertsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetCustomchannelsInstance : TAccountsCustomchannelsResource;

begin
  if (FCustomchannelsInstance=Nil) then
    FCustomchannelsInstance:=CreateCustomchannelsResource;
  Result:=FCustomchannelsInstance;
end;

Function TAccountsResource.CreateCustomchannelsResource : TAccountsCustomchannelsResource;

begin
  Result:=CreateCustomchannelsResource(Self);
end;


Function TAccountsResource.CreateCustomchannelsResource(AOwner : TComponent) : TAccountsCustomchannelsResource;

begin
  Result:=TAccountsCustomchannelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetMetadataDimensionsInstance : TAccountsMetadataDimensionsResource;

begin
  if (FMetadataDimensionsInstance=Nil) then
    FMetadataDimensionsInstance:=CreateMetadataDimensionsResource;
  Result:=FMetadataDimensionsInstance;
end;

Function TAccountsResource.CreateMetadataDimensionsResource : TAccountsMetadataDimensionsResource;

begin
  Result:=CreateMetadataDimensionsResource(Self);
end;


Function TAccountsResource.CreateMetadataDimensionsResource(AOwner : TComponent) : TAccountsMetadataDimensionsResource;

begin
  Result:=TAccountsMetadataDimensionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetMetadataMetricsInstance : TAccountsMetadataMetricsResource;

begin
  if (FMetadataMetricsInstance=Nil) then
    FMetadataMetricsInstance:=CreateMetadataMetricsResource;
  Result:=FMetadataMetricsInstance;
end;

Function TAccountsResource.CreateMetadataMetricsResource : TAccountsMetadataMetricsResource;

begin
  Result:=CreateMetadataMetricsResource(Self);
end;


Function TAccountsResource.CreateMetadataMetricsResource(AOwner : TComponent) : TAccountsMetadataMetricsResource;

begin
  Result:=TAccountsMetadataMetricsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetMetadataInstance : TAccountsMetadataResource;

begin
  if (FMetadataInstance=Nil) then
    FMetadataInstance:=CreateMetadataResource;
  Result:=FMetadataInstance;
end;

Function TAccountsResource.CreateMetadataResource : TAccountsMetadataResource;

begin
  Result:=CreateMetadataResource(Self);
end;


Function TAccountsResource.CreateMetadataResource(AOwner : TComponent) : TAccountsMetadataResource;

begin
  Result:=TAccountsMetadataResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetPreferreddealsInstance : TAccountsPreferreddealsResource;

begin
  if (FPreferreddealsInstance=Nil) then
    FPreferreddealsInstance:=CreatePreferreddealsResource;
  Result:=FPreferreddealsInstance;
end;

Function TAccountsResource.CreatePreferreddealsResource : TAccountsPreferreddealsResource;

begin
  Result:=CreatePreferreddealsResource(Self);
end;


Function TAccountsResource.CreatePreferreddealsResource(AOwner : TComponent) : TAccountsPreferreddealsResource;

begin
  Result:=TAccountsPreferreddealsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetReportsSavedInstance : TAccountsReportsSavedResource;

begin
  if (FReportsSavedInstance=Nil) then
    FReportsSavedInstance:=CreateReportsSavedResource;
  Result:=FReportsSavedInstance;
end;

Function TAccountsResource.CreateReportsSavedResource : TAccountsReportsSavedResource;

begin
  Result:=CreateReportsSavedResource(Self);
end;


Function TAccountsResource.CreateReportsSavedResource(AOwner : TComponent) : TAccountsReportsSavedResource;

begin
  Result:=TAccountsReportsSavedResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetReportsInstance : TAccountsReportsResource;

begin
  if (FReportsInstance=Nil) then
    FReportsInstance:=CreateReportsResource;
  Result:=FReportsInstance;
end;

Function TAccountsResource.CreateReportsResource : TAccountsReportsResource;

begin
  Result:=CreateReportsResource(Self);
end;


Function TAccountsResource.CreateReportsResource(AOwner : TComponent) : TAccountsReportsResource;

begin
  Result:=TAccountsReportsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetUrlchannelsInstance : TAccountsUrlchannelsResource;

begin
  if (FUrlchannelsInstance=Nil) then
    FUrlchannelsInstance:=CreateUrlchannelsResource;
  Result:=FUrlchannelsInstance;
end;

Function TAccountsResource.CreateUrlchannelsResource : TAccountsUrlchannelsResource;

begin
  Result:=CreateUrlchannelsResource(Self);
end;


Function TAccountsResource.CreateUrlchannelsResource(AOwner : TComponent) : TAccountsUrlchannelsResource;

begin
  Result:=TAccountsUrlchannelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TAdexchangesellerAPI
  --------------------------------------------------------------------}

Class Function TAdexchangesellerAPI.APIName : String;

begin
  Result:='adexchangeseller';
end;

Class Function TAdexchangesellerAPI.APIVersion : String;

begin
  Result:='v2.0';
end;

Class Function TAdexchangesellerAPI.APIRevision : String;

begin
  Result:='20150401';
end;

Class Function TAdexchangesellerAPI.APIID : String;

begin
  Result:='adexchangeseller:v2.0';
end;

Class Function TAdexchangesellerAPI.APITitle : String;

begin
  Result:='Ad Exchange Seller API';
end;

Class Function TAdexchangesellerAPI.APIDescription : String;

begin
  Result:='Gives Ad Exchange seller users access to their inventory and the ability to generate reports';
end;

Class Function TAdexchangesellerAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TAdexchangesellerAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TAdexchangesellerAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/doubleclick-16.gif';
end;

Class Function TAdexchangesellerAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/doubleclick-32.gif';
end;

Class Function TAdexchangesellerAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/ad-exchange/seller-rest/';
end;

Class Function TAdexchangesellerAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TAdexchangesellerAPI.APIbasePath : string;

begin
  Result:='/adexchangeseller/v2.0/';
end;

Class Function TAdexchangesellerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/adexchangeseller/v2.0/';
end;

Class Function TAdexchangesellerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TAdexchangesellerAPI.APIservicePath : string;

begin
  Result:='adexchangeseller/v2.0/';
end;

Class Function TAdexchangesellerAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TAdexchangesellerAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/adexchange.seller';
  Result[0].Description:='View and manage your Ad Exchange data';
  Result[1].Name:='https://www.googleapis.com/auth/adexchange.seller.readonly';
  Result[1].Description:='View your Ad Exchange data';
  
end;

Class Function TAdexchangesellerAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TAdexchangesellerAPI.RegisterAPIResources;

begin
  TAccount.RegisterObject;
  TAccounts.RegisterObject;
  TAdClient.RegisterObject;
  TAdClients.RegisterObject;
  TAlert.RegisterObject;
  TAlerts.RegisterObject;
  TCustomChannelTypetargetingInfo.RegisterObject;
  TCustomChannel.RegisterObject;
  TCustomChannels.RegisterObject;
  TMetadata.RegisterObject;
  TPreferredDeal.RegisterObject;
  TPreferredDeals.RegisterObject;
  TReportTypeheadersItem.RegisterObject;
  TReport.RegisterObject;
  TReportingMetadataEntry.RegisterObject;
  TSavedReport.RegisterObject;
  TSavedReports.RegisterObject;
  TUrlChannel.RegisterObject;
  TUrlChannels.RegisterObject;
end;


Function TAdexchangesellerAPI.GetAccountsAdclientsInstance : TAccountsAdclientsResource;

begin
  if (FAccountsAdclientsInstance=Nil) then
    FAccountsAdclientsInstance:=CreateAccountsAdclientsResource;
  Result:=FAccountsAdclientsInstance;
end;

Function TAdexchangesellerAPI.CreateAccountsAdclientsResource : TAccountsAdclientsResource;

begin
  Result:=CreateAccountsAdclientsResource(Self);
end;


Function TAdexchangesellerAPI.CreateAccountsAdclientsResource(AOwner : TComponent) : TAccountsAdclientsResource;

begin
  Result:=TAccountsAdclientsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangesellerAPI.GetAccountsAlertsInstance : TAccountsAlertsResource;

begin
  if (FAccountsAlertsInstance=Nil) then
    FAccountsAlertsInstance:=CreateAccountsAlertsResource;
  Result:=FAccountsAlertsInstance;
end;

Function TAdexchangesellerAPI.CreateAccountsAlertsResource : TAccountsAlertsResource;

begin
  Result:=CreateAccountsAlertsResource(Self);
end;


Function TAdexchangesellerAPI.CreateAccountsAlertsResource(AOwner : TComponent) : TAccountsAlertsResource;

begin
  Result:=TAccountsAlertsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangesellerAPI.GetAccountsCustomchannelsInstance : TAccountsCustomchannelsResource;

begin
  if (FAccountsCustomchannelsInstance=Nil) then
    FAccountsCustomchannelsInstance:=CreateAccountsCustomchannelsResource;
  Result:=FAccountsCustomchannelsInstance;
end;

Function TAdexchangesellerAPI.CreateAccountsCustomchannelsResource : TAccountsCustomchannelsResource;

begin
  Result:=CreateAccountsCustomchannelsResource(Self);
end;


Function TAdexchangesellerAPI.CreateAccountsCustomchannelsResource(AOwner : TComponent) : TAccountsCustomchannelsResource;

begin
  Result:=TAccountsCustomchannelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangesellerAPI.GetAccountsMetadataDimensionsInstance : TAccountsMetadataDimensionsResource;

begin
  if (FAccountsMetadataDimensionsInstance=Nil) then
    FAccountsMetadataDimensionsInstance:=CreateAccountsMetadataDimensionsResource;
  Result:=FAccountsMetadataDimensionsInstance;
end;

Function TAdexchangesellerAPI.CreateAccountsMetadataDimensionsResource : TAccountsMetadataDimensionsResource;

begin
  Result:=CreateAccountsMetadataDimensionsResource(Self);
end;


Function TAdexchangesellerAPI.CreateAccountsMetadataDimensionsResource(AOwner : TComponent) : TAccountsMetadataDimensionsResource;

begin
  Result:=TAccountsMetadataDimensionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangesellerAPI.GetAccountsMetadataMetricsInstance : TAccountsMetadataMetricsResource;

begin
  if (FAccountsMetadataMetricsInstance=Nil) then
    FAccountsMetadataMetricsInstance:=CreateAccountsMetadataMetricsResource;
  Result:=FAccountsMetadataMetricsInstance;
end;

Function TAdexchangesellerAPI.CreateAccountsMetadataMetricsResource : TAccountsMetadataMetricsResource;

begin
  Result:=CreateAccountsMetadataMetricsResource(Self);
end;


Function TAdexchangesellerAPI.CreateAccountsMetadataMetricsResource(AOwner : TComponent) : TAccountsMetadataMetricsResource;

begin
  Result:=TAccountsMetadataMetricsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangesellerAPI.GetAccountsMetadataInstance : TAccountsMetadataResource;

begin
  if (FAccountsMetadataInstance=Nil) then
    FAccountsMetadataInstance:=CreateAccountsMetadataResource;
  Result:=FAccountsMetadataInstance;
end;

Function TAdexchangesellerAPI.CreateAccountsMetadataResource : TAccountsMetadataResource;

begin
  Result:=CreateAccountsMetadataResource(Self);
end;


Function TAdexchangesellerAPI.CreateAccountsMetadataResource(AOwner : TComponent) : TAccountsMetadataResource;

begin
  Result:=TAccountsMetadataResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangesellerAPI.GetAccountsPreferreddealsInstance : TAccountsPreferreddealsResource;

begin
  if (FAccountsPreferreddealsInstance=Nil) then
    FAccountsPreferreddealsInstance:=CreateAccountsPreferreddealsResource;
  Result:=FAccountsPreferreddealsInstance;
end;

Function TAdexchangesellerAPI.CreateAccountsPreferreddealsResource : TAccountsPreferreddealsResource;

begin
  Result:=CreateAccountsPreferreddealsResource(Self);
end;


Function TAdexchangesellerAPI.CreateAccountsPreferreddealsResource(AOwner : TComponent) : TAccountsPreferreddealsResource;

begin
  Result:=TAccountsPreferreddealsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangesellerAPI.GetAccountsReportsSavedInstance : TAccountsReportsSavedResource;

begin
  if (FAccountsReportsSavedInstance=Nil) then
    FAccountsReportsSavedInstance:=CreateAccountsReportsSavedResource;
  Result:=FAccountsReportsSavedInstance;
end;

Function TAdexchangesellerAPI.CreateAccountsReportsSavedResource : TAccountsReportsSavedResource;

begin
  Result:=CreateAccountsReportsSavedResource(Self);
end;


Function TAdexchangesellerAPI.CreateAccountsReportsSavedResource(AOwner : TComponent) : TAccountsReportsSavedResource;

begin
  Result:=TAccountsReportsSavedResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangesellerAPI.GetAccountsReportsInstance : TAccountsReportsResource;

begin
  if (FAccountsReportsInstance=Nil) then
    FAccountsReportsInstance:=CreateAccountsReportsResource;
  Result:=FAccountsReportsInstance;
end;

Function TAdexchangesellerAPI.CreateAccountsReportsResource : TAccountsReportsResource;

begin
  Result:=CreateAccountsReportsResource(Self);
end;


Function TAdexchangesellerAPI.CreateAccountsReportsResource(AOwner : TComponent) : TAccountsReportsResource;

begin
  Result:=TAccountsReportsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangesellerAPI.GetAccountsUrlchannelsInstance : TAccountsUrlchannelsResource;

begin
  if (FAccountsUrlchannelsInstance=Nil) then
    FAccountsUrlchannelsInstance:=CreateAccountsUrlchannelsResource;
  Result:=FAccountsUrlchannelsInstance;
end;

Function TAdexchangesellerAPI.CreateAccountsUrlchannelsResource : TAccountsUrlchannelsResource;

begin
  Result:=CreateAccountsUrlchannelsResource(Self);
end;


Function TAdexchangesellerAPI.CreateAccountsUrlchannelsResource(AOwner : TComponent) : TAccountsUrlchannelsResource;

begin
  Result:=TAccountsUrlchannelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdexchangesellerAPI.GetAccountsInstance : TAccountsResource;

begin
  if (FAccountsInstance=Nil) then
    FAccountsInstance:=CreateAccountsResource;
  Result:=FAccountsInstance;
end;

Function TAdexchangesellerAPI.CreateAccountsResource : TAccountsResource;

begin
  Result:=CreateAccountsResource(Self);
end;


Function TAdexchangesellerAPI.CreateAccountsResource(AOwner : TComponent) : TAccountsResource;

begin
  Result:=TAccountsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TAdexchangesellerAPI.RegisterAPI;
end.
