unit googleadsense;
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
  TAdCode = Class;
  TAdStyle = Class;
  TAdUnit = Class;
  TAdUnits = Class;
  TAdsenseReportsGenerateResponse = Class;
  TAlert = Class;
  TAlerts = Class;
  TCustomChannel = Class;
  TCustomChannels = Class;
  TMetadata = Class;
  TPayment = Class;
  TPayments = Class;
  TReportingMetadataEntry = Class;
  TSavedAdStyle = Class;
  TSavedAdStyles = Class;
  TSavedReport = Class;
  TSavedReports = Class;
  TUrlChannel = Class;
  TUrlChannels = Class;
  TAccountArray = Array of TAccount;
  TAccountsArray = Array of TAccounts;
  TAdClientArray = Array of TAdClient;
  TAdClientsArray = Array of TAdClients;
  TAdCodeArray = Array of TAdCode;
  TAdStyleArray = Array of TAdStyle;
  TAdUnitArray = Array of TAdUnit;
  TAdUnitsArray = Array of TAdUnits;
  TAdsenseReportsGenerateResponseArray = Array of TAdsenseReportsGenerateResponse;
  TAlertArray = Array of TAlert;
  TAlertsArray = Array of TAlerts;
  TCustomChannelArray = Array of TCustomChannel;
  TCustomChannelsArray = Array of TCustomChannels;
  TMetadataArray = Array of TMetadata;
  TPaymentArray = Array of TPayment;
  TPaymentsArray = Array of TPayments;
  TReportingMetadataEntryArray = Array of TReportingMetadataEntry;
  TSavedAdStyleArray = Array of TSavedAdStyle;
  TSavedAdStylesArray = Array of TSavedAdStyles;
  TSavedReportArray = Array of TSavedReport;
  TSavedReportsArray = Array of TSavedReports;
  TUrlChannelArray = Array of TUrlChannel;
  TUrlChannelsArray = Array of TUrlChannels;
  //Anonymous types, using auto-generated names
  TAdStyleTypecolors = Class;
  TAdStyleTypefont = Class;
  TAdUnitTypecontentAdsSettingsTypebackupOption = Class;
  TAdUnitTypecontentAdsSettings = Class;
  TAdUnitTypefeedAdsSettings = Class;
  TAdUnitTypemobileContentAdsSettings = Class;
  TAdsenseReportsGenerateResponseTypeheadersItem = Class;
  TCustomChannelTypetargetingInfo = Class;
  TAccountTypesubAccountsArray = Array of TAccount;
  TAccountsTypeitemsArray = Array of TAccount;
  TAdClientsTypeitemsArray = Array of TAdClient;
  TAdUnitsTypeitemsArray = Array of TAdUnit;
  TAdsenseReportsGenerateResponseTypeheadersArray = Array of TAdsenseReportsGenerateResponseTypeheadersItem;
  TAdsenseReportsGenerateResponseTyperowsArray = Array of TStringArray;
  TAlertsTypeitemsArray = Array of TAlert;
  TCustomChannelsTypeitemsArray = Array of TCustomChannel;
  TMetadataTypeitemsArray = Array of TReportingMetadataEntry;
  TPaymentsTypeitemsArray = Array of TPayment;
  TSavedAdStylesTypeitemsArray = Array of TSavedAdStyle;
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
    Fpremium : boolean;
    FsubAccounts : TAccountTypesubAccountsArray;
    Ftimezone : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setpremium(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsubAccounts(AIndex : Integer; AValue : TAccountTypesubAccountsArray); virtual;
    Procedure Settimezone(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property name : String Index 16 Read Fname Write Setname;
    Property premium : boolean Index 24 Read Fpremium Write Setpremium;
    Property subAccounts : TAccountTypesubAccountsArray Index 32 Read FsubAccounts Write SetsubAccounts;
    Property timezone : String Index 40 Read Ftimezone Write Settimezone;
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
    FarcReviewMode : String;
    Fid : String;
    Fkind : String;
    FproductCode : String;
    FsupportsReporting : boolean;
  Protected
    //Property setters
    Procedure SetarcOptIn(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetarcReviewMode(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetproductCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetsupportsReporting(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property arcOptIn : boolean Index 0 Read FarcOptIn Write SetarcOptIn;
    Property arcReviewMode : String Index 8 Read FarcReviewMode Write SetarcReviewMode;
    Property id : String Index 16 Read Fid Write Setid;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property productCode : String Index 32 Read FproductCode Write SetproductCode;
    Property supportsReporting : boolean Index 40 Read FsupportsReporting Write SetsupportsReporting;
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
    TAdCode
    --------------------------------------------------------------------}
  
  TAdCode = Class(TGoogleBaseObject)
  Private
    FadCode : String;
    Fkind : String;
  Protected
    //Property setters
    Procedure SetadCode(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property adCode : String Index 0 Read FadCode Write SetadCode;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TAdCodeClass = Class of TAdCode;
  
  { --------------------------------------------------------------------
    TAdStyleTypecolors
    --------------------------------------------------------------------}
  
  TAdStyleTypecolors = Class(TGoogleBaseObject)
  Private
    Fbackground : String;
    Fborder : String;
    Ftext : String;
    Ftitle : String;
    Furl : String;
  Protected
    //Property setters
    Procedure Setbackground(AIndex : Integer; AValue : String); virtual;
    Procedure Setborder(AIndex : Integer; AValue : String); virtual;
    Procedure Settext(AIndex : Integer; AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property background : String Index 0 Read Fbackground Write Setbackground;
    Property border : String Index 8 Read Fborder Write Setborder;
    Property text : String Index 16 Read Ftext Write Settext;
    Property title : String Index 24 Read Ftitle Write Settitle;
    Property url : String Index 32 Read Furl Write Seturl;
  end;
  TAdStyleTypecolorsClass = Class of TAdStyleTypecolors;
  
  { --------------------------------------------------------------------
    TAdStyleTypefont
    --------------------------------------------------------------------}
  
  TAdStyleTypefont = Class(TGoogleBaseObject)
  Private
    Ffamily : String;
    Fsize : String;
  Protected
    //Property setters
    Procedure Setfamily(AIndex : Integer; AValue : String); virtual;
    Procedure Setsize(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property family : String Index 0 Read Ffamily Write Setfamily;
    Property size : String Index 8 Read Fsize Write Setsize;
  end;
  TAdStyleTypefontClass = Class of TAdStyleTypefont;
  
  { --------------------------------------------------------------------
    TAdStyle
    --------------------------------------------------------------------}
  
  TAdStyle = Class(TGoogleBaseObject)
  Private
    Fcolors : TAdStyleTypecolors;
    Fcorners : String;
    Ffont : TAdStyleTypefont;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setcolors(AIndex : Integer; AValue : TAdStyleTypecolors); virtual;
    Procedure Setcorners(AIndex : Integer; AValue : String); virtual;
    Procedure Setfont(AIndex : Integer; AValue : TAdStyleTypefont); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property colors : TAdStyleTypecolors Index 0 Read Fcolors Write Setcolors;
    Property corners : String Index 8 Read Fcorners Write Setcorners;
    Property font : TAdStyleTypefont Index 16 Read Ffont Write Setfont;
    Property kind : String Index 24 Read Fkind Write Setkind;
  end;
  TAdStyleClass = Class of TAdStyle;
  
  { --------------------------------------------------------------------
    TAdUnitTypecontentAdsSettingsTypebackupOption
    --------------------------------------------------------------------}
  
  TAdUnitTypecontentAdsSettingsTypebackupOption = Class(TGoogleBaseObject)
  Private
    Fcolor : String;
    F_type : String;
    Furl : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcolor(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property color : String Index 0 Read Fcolor Write Setcolor;
    Property _type : String Index 8 Read F_type Write Set_type;
    Property url : String Index 16 Read Furl Write Seturl;
  end;
  TAdUnitTypecontentAdsSettingsTypebackupOptionClass = Class of TAdUnitTypecontentAdsSettingsTypebackupOption;
  
  { --------------------------------------------------------------------
    TAdUnitTypecontentAdsSettings
    --------------------------------------------------------------------}
  
  TAdUnitTypecontentAdsSettings = Class(TGoogleBaseObject)
  Private
    FbackupOption : TAdUnitTypecontentAdsSettingsTypebackupOption;
    Fsize : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetbackupOption(AIndex : Integer; AValue : TAdUnitTypecontentAdsSettingsTypebackupOption); virtual;
    Procedure Setsize(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property backupOption : TAdUnitTypecontentAdsSettingsTypebackupOption Index 0 Read FbackupOption Write SetbackupOption;
    Property size : String Index 8 Read Fsize Write Setsize;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TAdUnitTypecontentAdsSettingsClass = Class of TAdUnitTypecontentAdsSettings;
  
  { --------------------------------------------------------------------
    TAdUnitTypefeedAdsSettings
    --------------------------------------------------------------------}
  
  TAdUnitTypefeedAdsSettings = Class(TGoogleBaseObject)
  Private
    FadPosition : String;
    Ffrequency : integer;
    FminimumWordCount : integer;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetadPosition(AIndex : Integer; AValue : String); virtual;
    Procedure Setfrequency(AIndex : Integer; AValue : integer); virtual;
    Procedure SetminimumWordCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property adPosition : String Index 0 Read FadPosition Write SetadPosition;
    Property frequency : integer Index 8 Read Ffrequency Write Setfrequency;
    Property minimumWordCount : integer Index 16 Read FminimumWordCount Write SetminimumWordCount;
    Property _type : String Index 24 Read F_type Write Set_type;
  end;
  TAdUnitTypefeedAdsSettingsClass = Class of TAdUnitTypefeedAdsSettings;
  
  { --------------------------------------------------------------------
    TAdUnitTypemobileContentAdsSettings
    --------------------------------------------------------------------}
  
  TAdUnitTypemobileContentAdsSettings = Class(TGoogleBaseObject)
  Private
    FmarkupLanguage : String;
    FscriptingLanguage : String;
    Fsize : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetmarkupLanguage(AIndex : Integer; AValue : String); virtual;
    Procedure SetscriptingLanguage(AIndex : Integer; AValue : String); virtual;
    Procedure Setsize(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property markupLanguage : String Index 0 Read FmarkupLanguage Write SetmarkupLanguage;
    Property scriptingLanguage : String Index 8 Read FscriptingLanguage Write SetscriptingLanguage;
    Property size : String Index 16 Read Fsize Write Setsize;
    Property _type : String Index 24 Read F_type Write Set_type;
  end;
  TAdUnitTypemobileContentAdsSettingsClass = Class of TAdUnitTypemobileContentAdsSettings;
  
  { --------------------------------------------------------------------
    TAdUnit
    --------------------------------------------------------------------}
  
  TAdUnit = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    FcontentAdsSettings : TAdUnitTypecontentAdsSettings;
    FcustomStyle : TAdStyle;
    FfeedAdsSettings : TAdUnitTypefeedAdsSettings;
    Fid : String;
    Fkind : String;
    FmobileContentAdsSettings : TAdUnitTypemobileContentAdsSettings;
    Fname : String;
    FsavedStyleId : String;
    Fstatus : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : String); virtual;
    Procedure SetcontentAdsSettings(AIndex : Integer; AValue : TAdUnitTypecontentAdsSettings); virtual;
    Procedure SetcustomStyle(AIndex : Integer; AValue : TAdStyle); virtual;
    Procedure SetfeedAdsSettings(AIndex : Integer; AValue : TAdUnitTypefeedAdsSettings); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmobileContentAdsSettings(AIndex : Integer; AValue : TAdUnitTypemobileContentAdsSettings); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetsavedStyleId(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property contentAdsSettings : TAdUnitTypecontentAdsSettings Index 8 Read FcontentAdsSettings Write SetcontentAdsSettings;
    Property customStyle : TAdStyle Index 16 Read FcustomStyle Write SetcustomStyle;
    Property feedAdsSettings : TAdUnitTypefeedAdsSettings Index 24 Read FfeedAdsSettings Write SetfeedAdsSettings;
    Property id : String Index 32 Read Fid Write Setid;
    Property kind : String Index 40 Read Fkind Write Setkind;
    Property mobileContentAdsSettings : TAdUnitTypemobileContentAdsSettings Index 48 Read FmobileContentAdsSettings Write SetmobileContentAdsSettings;
    Property name : String Index 56 Read Fname Write Setname;
    Property savedStyleId : String Index 64 Read FsavedStyleId Write SetsavedStyleId;
    Property status : String Index 72 Read Fstatus Write Setstatus;
  end;
  TAdUnitClass = Class of TAdUnit;
  
  { --------------------------------------------------------------------
    TAdUnits
    --------------------------------------------------------------------}
  
  TAdUnits = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TAdUnitsTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TAdUnitsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TAdUnitsTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TAdUnitsClass = Class of TAdUnits;
  
  { --------------------------------------------------------------------
    TAdsenseReportsGenerateResponseTypeheadersItem
    --------------------------------------------------------------------}
  
  TAdsenseReportsGenerateResponseTypeheadersItem = Class(TGoogleBaseObject)
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
  TAdsenseReportsGenerateResponseTypeheadersItemClass = Class of TAdsenseReportsGenerateResponseTypeheadersItem;
  
  { --------------------------------------------------------------------
    TAdsenseReportsGenerateResponse
    --------------------------------------------------------------------}
  
  TAdsenseReportsGenerateResponse = Class(TGoogleBaseObject)
  Private
    Faverages : TStringArray;
    FendDate : String;
    Fheaders : TAdsenseReportsGenerateResponseTypeheadersArray;
    Fkind : String;
    Frows : TAdsenseReportsGenerateResponseTyperowsArray;
    FstartDate : String;
    FtotalMatchedRows : String;
    Ftotals : TStringArray;
    Fwarnings : TStringArray;
  Protected
    //Property setters
    Procedure Setaverages(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : String); virtual;
    Procedure Setheaders(AIndex : Integer; AValue : TAdsenseReportsGenerateResponseTypeheadersArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TAdsenseReportsGenerateResponseTyperowsArray); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : String); virtual;
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
    Property endDate : String Index 8 Read FendDate Write SetendDate;
    Property headers : TAdsenseReportsGenerateResponseTypeheadersArray Index 16 Read Fheaders Write Setheaders;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property rows : TAdsenseReportsGenerateResponseTyperowsArray Index 32 Read Frows Write Setrows;
    Property startDate : String Index 40 Read FstartDate Write SetstartDate;
    Property totalMatchedRows : String Index 48 Read FtotalMatchedRows Write SettotalMatchedRows;
    Property totals : TStringArray Index 56 Read Ftotals Write Settotals;
    Property warnings : TStringArray Index 64 Read Fwarnings Write Setwarnings;
  end;
  TAdsenseReportsGenerateResponseClass = Class of TAdsenseReportsGenerateResponse;
  
  { --------------------------------------------------------------------
    TAlert
    --------------------------------------------------------------------}
  
  TAlert = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FisDismissible : boolean;
    Fkind : String;
    Fmessage : String;
    Fseverity : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetisDismissible(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
    Procedure Setseverity(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property isDismissible : boolean Index 8 Read FisDismissible Write SetisDismissible;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property message : String Index 24 Read Fmessage Write Setmessage;
    Property severity : String Index 32 Read Fseverity Write Setseverity;
    Property _type : String Index 40 Read F_type Write Set_type;
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
    TPayment
    --------------------------------------------------------------------}
  
  TPayment = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    FpaymentAmount : String;
    FpaymentAmountCurrencyCode : String;
    FpaymentDate : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetpaymentAmount(AIndex : Integer; AValue : String); virtual;
    Procedure SetpaymentAmountCurrencyCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetpaymentDate(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property paymentAmount : String Index 16 Read FpaymentAmount Write SetpaymentAmount;
    Property paymentAmountCurrencyCode : String Index 24 Read FpaymentAmountCurrencyCode Write SetpaymentAmountCurrencyCode;
    Property paymentDate : String Index 32 Read FpaymentDate Write SetpaymentDate;
  end;
  TPaymentClass = Class of TPayment;
  
  { --------------------------------------------------------------------
    TPayments
    --------------------------------------------------------------------}
  
  TPayments = Class(TGoogleBaseObject)
  Private
    Fitems : TPaymentsTypeitemsArray;
    Fkind : String;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TPaymentsTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property items : TPaymentsTypeitemsArray Index 0 Read Fitems Write Setitems;
    Property kind : String Index 8 Read Fkind Write Setkind;
  end;
  TPaymentsClass = Class of TPayments;
  
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
    TSavedAdStyle
    --------------------------------------------------------------------}
  
  TSavedAdStyle = Class(TGoogleBaseObject)
  Private
    FadStyle : TAdStyle;
    Fid : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure SetadStyle(AIndex : Integer; AValue : TAdStyle); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property adStyle : TAdStyle Index 0 Read FadStyle Write SetadStyle;
    Property id : String Index 8 Read Fid Write Setid;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property name : String Index 24 Read Fname Write Setname;
  end;
  TSavedAdStyleClass = Class of TSavedAdStyle;
  
  { --------------------------------------------------------------------
    TSavedAdStyles
    --------------------------------------------------------------------}
  
  TSavedAdStyles = Class(TGoogleBaseObject)
  Private
    Fetag : String;
    Fitems : TSavedAdStylesTypeitemsArray;
    Fkind : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : String); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TSavedAdStylesTypeitemsArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etag : String Index 0 Read Fetag Write Setetag;
    Property items : TSavedAdStylesTypeitemsArray Index 8 Read Fitems Write Setitems;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property nextPageToken : String Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TSavedAdStylesClass = Class of TSavedAdStyles;
  
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
    TAccountsAdunitsCustomchannelsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsAdunitsCustomchannelsResource, method List
  
  TAccountsAdunitsCustomchannelsListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TAccountsAdunitsCustomchannelsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(accountId: string; adClientId: string; adUnitId: string; AQuery : string  = '') : TCustomChannels;
    Function List(accountId: string; adClientId: string; adUnitId: string; AQuery : TAccountsAdunitsCustomchannelslistOptions) : TCustomChannels;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsAdunitsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsAdunitsResource, method List
  
  TAccountsAdunitsListOptions = Record
    includeInactive : boolean;
    maxResults : integer;
    pageToken : String;
  end;
  
  TAccountsAdunitsResource = Class(TGoogleResource)
  Private
    FCustomchannelsInstance : TAccountsAdunitsCustomchannelsResource;
    Function GetCustomchannelsInstance : TAccountsAdunitsCustomchannelsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; adClientId: string; adUnitId: string) : TAdUnit;
    Function GetAdCode(accountId: string; adClientId: string; adUnitId: string) : TAdCode;
    Function List(accountId: string; adClientId: string; AQuery : string  = '') : TAdUnits;
    Function List(accountId: string; adClientId: string; AQuery : TAccountsAdunitslistOptions) : TAdUnits;
    Function CreateCustomchannelsResource(AOwner : TComponent) : TAccountsAdunitsCustomchannelsResource;virtual;overload;
    Function CreateCustomchannelsResource : TAccountsAdunitsCustomchannelsResource;virtual;overload;
    Property CustomchannelsResource : TAccountsAdunitsCustomchannelsResource Read GetCustomchannelsInstance;
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
    Procedure Delete(accountId: string; alertId: string);
    Function List(accountId: string; AQuery : string  = '') : TAlerts;
    Function List(accountId: string; AQuery : TAccountsAlertslistOptions) : TAlerts;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsCustomchannelsAdunitsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsCustomchannelsAdunitsResource, method List
  
  TAccountsCustomchannelsAdunitsListOptions = Record
    includeInactive : boolean;
    maxResults : integer;
    pageToken : String;
  end;
  
  TAccountsCustomchannelsAdunitsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(accountId: string; adClientId: string; customChannelId: string; AQuery : string  = '') : TAdUnits;
    Function List(accountId: string; adClientId: string; customChannelId: string; AQuery : TAccountsCustomchannelsAdunitslistOptions) : TAdUnits;
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
  Private
    FAdunitsInstance : TAccountsCustomchannelsAdunitsResource;
    Function GetAdunitsInstance : TAccountsCustomchannelsAdunitsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; adClientId: string; customChannelId: string) : TCustomChannel;
    Function List(accountId: string; adClientId: string; AQuery : string  = '') : TCustomChannels;
    Function List(accountId: string; adClientId: string; AQuery : TAccountsCustomchannelslistOptions) : TCustomChannels;
    Function CreateAdunitsResource(AOwner : TComponent) : TAccountsCustomchannelsAdunitsResource;virtual;overload;
    Function CreateAdunitsResource : TAccountsCustomchannelsAdunitsResource;virtual;overload;
    Property AdunitsResource : TAccountsCustomchannelsAdunitsResource Read GetAdunitsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsPaymentsResource
    --------------------------------------------------------------------}
  
  TAccountsPaymentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(accountId: string) : TPayments;
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
    Function Generate(accountId: string; savedReportId: string; AQuery : string  = '') : TAdsenseReportsGenerateResponse;
    Function Generate(accountId: string; savedReportId: string; AQuery : TAccountsReportsSavedgenerateOptions) : TAdsenseReportsGenerateResponse;
    Function List(accountId: string; AQuery : string  = '') : TSavedReports;
    Function List(accountId: string; AQuery : TAccountsReportsSavedlistOptions) : TSavedReports;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsReportsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsReportsResource, method Generate
  
  TAccountsReportsGenerateOptions = Record
    currency : String;
    dimension : String;
    endDate : String;
    filter : String;
    locale : String;
    maxResults : integer;
    metric : String;
    sort : String;
    startDate : String;
    startIndex : integer;
    useTimezoneReporting : boolean;
  end;
  
  TAccountsReportsResource = Class(TGoogleResource)
  Private
    FSavedInstance : TAccountsReportsSavedResource;
    Function GetSavedInstance : TAccountsReportsSavedResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Generate(accountId: string; AQuery : string  = '') : TAdsenseReportsGenerateResponse;
    Function Generate(accountId: string; AQuery : TAccountsReportsgenerateOptions) : TAdsenseReportsGenerateResponse;
    Function CreateSavedResource(AOwner : TComponent) : TAccountsReportsSavedResource;virtual;overload;
    Function CreateSavedResource : TAccountsReportsSavedResource;virtual;overload;
    Property SavedResource : TAccountsReportsSavedResource Read GetSavedInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsSavedadstylesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsSavedadstylesResource, method List
  
  TAccountsSavedadstylesListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TAccountsSavedadstylesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; savedAdStyleId: string) : TSavedAdStyle;
    Function List(accountId: string; AQuery : string  = '') : TSavedAdStyles;
    Function List(accountId: string; AQuery : TAccountsSavedadstyleslistOptions) : TSavedAdStyles;
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
  
  
  //Optional query Options for TAccountsResource, method Get
  
  TAccountsGetOptions = Record
    tree : boolean;
  end;
  
  
  //Optional query Options for TAccountsResource, method List
  
  TAccountsListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TAccountsResource = Class(TGoogleResource)
  Private
    FAdclientsInstance : TAccountsAdclientsResource;
    FAdunitsCustomchannelsInstance : TAccountsAdunitsCustomchannelsResource;
    FAdunitsInstance : TAccountsAdunitsResource;
    FAlertsInstance : TAccountsAlertsResource;
    FCustomchannelsAdunitsInstance : TAccountsCustomchannelsAdunitsResource;
    FCustomchannelsInstance : TAccountsCustomchannelsResource;
    FPaymentsInstance : TAccountsPaymentsResource;
    FReportsSavedInstance : TAccountsReportsSavedResource;
    FReportsInstance : TAccountsReportsResource;
    FSavedadstylesInstance : TAccountsSavedadstylesResource;
    FUrlchannelsInstance : TAccountsUrlchannelsResource;
    Function GetAdclientsInstance : TAccountsAdclientsResource;virtual;
    Function GetAdunitsCustomchannelsInstance : TAccountsAdunitsCustomchannelsResource;virtual;
    Function GetAdunitsInstance : TAccountsAdunitsResource;virtual;
    Function GetAlertsInstance : TAccountsAlertsResource;virtual;
    Function GetCustomchannelsAdunitsInstance : TAccountsCustomchannelsAdunitsResource;virtual;
    Function GetCustomchannelsInstance : TAccountsCustomchannelsResource;virtual;
    Function GetPaymentsInstance : TAccountsPaymentsResource;virtual;
    Function GetReportsSavedInstance : TAccountsReportsSavedResource;virtual;
    Function GetReportsInstance : TAccountsReportsResource;virtual;
    Function GetSavedadstylesInstance : TAccountsSavedadstylesResource;virtual;
    Function GetUrlchannelsInstance : TAccountsUrlchannelsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; AQuery : string  = '') : TAccount;
    Function Get(accountId: string; AQuery : TAccountsgetOptions) : TAccount;
    Function List(AQuery : string  = '') : TAccounts;
    Function List(AQuery : TAccountslistOptions) : TAccounts;
    Function CreateAdclientsResource(AOwner : TComponent) : TAccountsAdclientsResource;virtual;overload;
    Function CreateAdclientsResource : TAccountsAdclientsResource;virtual;overload;
    Function CreateAdunitsCustomchannelsResource(AOwner : TComponent) : TAccountsAdunitsCustomchannelsResource;virtual;overload;
    Function CreateAdunitsCustomchannelsResource : TAccountsAdunitsCustomchannelsResource;virtual;overload;
    Function CreateAdunitsResource(AOwner : TComponent) : TAccountsAdunitsResource;virtual;overload;
    Function CreateAdunitsResource : TAccountsAdunitsResource;virtual;overload;
    Function CreateAlertsResource(AOwner : TComponent) : TAccountsAlertsResource;virtual;overload;
    Function CreateAlertsResource : TAccountsAlertsResource;virtual;overload;
    Function CreateCustomchannelsAdunitsResource(AOwner : TComponent) : TAccountsCustomchannelsAdunitsResource;virtual;overload;
    Function CreateCustomchannelsAdunitsResource : TAccountsCustomchannelsAdunitsResource;virtual;overload;
    Function CreateCustomchannelsResource(AOwner : TComponent) : TAccountsCustomchannelsResource;virtual;overload;
    Function CreateCustomchannelsResource : TAccountsCustomchannelsResource;virtual;overload;
    Function CreatePaymentsResource(AOwner : TComponent) : TAccountsPaymentsResource;virtual;overload;
    Function CreatePaymentsResource : TAccountsPaymentsResource;virtual;overload;
    Function CreateReportsSavedResource(AOwner : TComponent) : TAccountsReportsSavedResource;virtual;overload;
    Function CreateReportsSavedResource : TAccountsReportsSavedResource;virtual;overload;
    Function CreateReportsResource(AOwner : TComponent) : TAccountsReportsResource;virtual;overload;
    Function CreateReportsResource : TAccountsReportsResource;virtual;overload;
    Function CreateSavedadstylesResource(AOwner : TComponent) : TAccountsSavedadstylesResource;virtual;overload;
    Function CreateSavedadstylesResource : TAccountsSavedadstylesResource;virtual;overload;
    Function CreateUrlchannelsResource(AOwner : TComponent) : TAccountsUrlchannelsResource;virtual;overload;
    Function CreateUrlchannelsResource : TAccountsUrlchannelsResource;virtual;overload;
    Property AdclientsResource : TAccountsAdclientsResource Read GetAdclientsInstance;
    Property AdunitsCustomchannelsResource : TAccountsAdunitsCustomchannelsResource Read GetAdunitsCustomchannelsInstance;
    Property AdunitsResource : TAccountsAdunitsResource Read GetAdunitsInstance;
    Property AlertsResource : TAccountsAlertsResource Read GetAlertsInstance;
    Property CustomchannelsAdunitsResource : TAccountsCustomchannelsAdunitsResource Read GetCustomchannelsAdunitsInstance;
    Property CustomchannelsResource : TAccountsCustomchannelsResource Read GetCustomchannelsInstance;
    Property PaymentsResource : TAccountsPaymentsResource Read GetPaymentsInstance;
    Property ReportsSavedResource : TAccountsReportsSavedResource Read GetReportsSavedInstance;
    Property ReportsResource : TAccountsReportsResource Read GetReportsInstance;
    Property SavedadstylesResource : TAccountsSavedadstylesResource Read GetSavedadstylesInstance;
    Property UrlchannelsResource : TAccountsUrlchannelsResource Read GetUrlchannelsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAdclientsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAdclientsResource, method List
  
  TAdclientsListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TAdclientsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TAdClients;
    Function List(AQuery : TAdclientslistOptions) : TAdClients;
  end;
  
  
  { --------------------------------------------------------------------
    TAdunitsCustomchannelsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAdunitsCustomchannelsResource, method List
  
  TAdunitsCustomchannelsListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TAdunitsCustomchannelsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(adClientId: string; adUnitId: string; AQuery : string  = '') : TCustomChannels;
    Function List(adClientId: string; adUnitId: string; AQuery : TAdunitsCustomchannelslistOptions) : TCustomChannels;
  end;
  
  
  { --------------------------------------------------------------------
    TAdunitsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAdunitsResource, method List
  
  TAdunitsListOptions = Record
    includeInactive : boolean;
    maxResults : integer;
    pageToken : String;
  end;
  
  TAdunitsResource = Class(TGoogleResource)
  Private
    FCustomchannelsInstance : TAdunitsCustomchannelsResource;
    Function GetCustomchannelsInstance : TAdunitsCustomchannelsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(adClientId: string; adUnitId: string) : TAdUnit;
    Function GetAdCode(adClientId: string; adUnitId: string) : TAdCode;
    Function List(adClientId: string; AQuery : string  = '') : TAdUnits;
    Function List(adClientId: string; AQuery : TAdunitslistOptions) : TAdUnits;
    Function CreateCustomchannelsResource(AOwner : TComponent) : TAdunitsCustomchannelsResource;virtual;overload;
    Function CreateCustomchannelsResource : TAdunitsCustomchannelsResource;virtual;overload;
    Property CustomchannelsResource : TAdunitsCustomchannelsResource Read GetCustomchannelsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAlertsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAlertsResource, method List
  
  TAlertsListOptions = Record
    locale : String;
  end;
  
  TAlertsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(alertId: string);
    Function List(AQuery : string  = '') : TAlerts;
    Function List(AQuery : TAlertslistOptions) : TAlerts;
  end;
  
  
  { --------------------------------------------------------------------
    TCustomchannelsAdunitsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCustomchannelsAdunitsResource, method List
  
  TCustomchannelsAdunitsListOptions = Record
    includeInactive : boolean;
    maxResults : integer;
    pageToken : String;
  end;
  
  TCustomchannelsAdunitsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(adClientId: string; customChannelId: string; AQuery : string  = '') : TAdUnits;
    Function List(adClientId: string; customChannelId: string; AQuery : TCustomchannelsAdunitslistOptions) : TAdUnits;
  end;
  
  
  { --------------------------------------------------------------------
    TCustomchannelsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCustomchannelsResource, method List
  
  TCustomchannelsListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TCustomchannelsResource = Class(TGoogleResource)
  Private
    FAdunitsInstance : TCustomchannelsAdunitsResource;
    Function GetAdunitsInstance : TCustomchannelsAdunitsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(adClientId: string; customChannelId: string) : TCustomChannel;
    Function List(adClientId: string; AQuery : string  = '') : TCustomChannels;
    Function List(adClientId: string; AQuery : TCustomchannelslistOptions) : TCustomChannels;
    Function CreateAdunitsResource(AOwner : TComponent) : TCustomchannelsAdunitsResource;virtual;overload;
    Function CreateAdunitsResource : TCustomchannelsAdunitsResource;virtual;overload;
    Property AdunitsResource : TCustomchannelsAdunitsResource Read GetAdunitsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TMetadataDimensionsResource
    --------------------------------------------------------------------}
  
  TMetadataDimensionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List : TMetadata;
  end;
  
  
  { --------------------------------------------------------------------
    TMetadataMetricsResource
    --------------------------------------------------------------------}
  
  TMetadataMetricsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List : TMetadata;
  end;
  
  
  { --------------------------------------------------------------------
    TMetadataResource
    --------------------------------------------------------------------}
  
  TMetadataResource = Class(TGoogleResource)
  Private
    FDimensionsInstance : TMetadataDimensionsResource;
    FMetricsInstance : TMetadataMetricsResource;
    Function GetDimensionsInstance : TMetadataDimensionsResource;virtual;
    Function GetMetricsInstance : TMetadataMetricsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateDimensionsResource(AOwner : TComponent) : TMetadataDimensionsResource;virtual;overload;
    Function CreateDimensionsResource : TMetadataDimensionsResource;virtual;overload;
    Function CreateMetricsResource(AOwner : TComponent) : TMetadataMetricsResource;virtual;overload;
    Function CreateMetricsResource : TMetadataMetricsResource;virtual;overload;
    Property DimensionsResource : TMetadataDimensionsResource Read GetDimensionsInstance;
    Property MetricsResource : TMetadataMetricsResource Read GetMetricsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TPaymentsResource
    --------------------------------------------------------------------}
  
  TPaymentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List : TPayments;
  end;
  
  
  { --------------------------------------------------------------------
    TReportsSavedResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TReportsSavedResource, method Generate
  
  TReportsSavedGenerateOptions = Record
    locale : String;
    maxResults : integer;
    startIndex : integer;
  end;
  
  
  //Optional query Options for TReportsSavedResource, method List
  
  TReportsSavedListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TReportsSavedResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Generate(savedReportId: string; AQuery : string  = '') : TAdsenseReportsGenerateResponse;
    Function Generate(savedReportId: string; AQuery : TReportsSavedgenerateOptions) : TAdsenseReportsGenerateResponse;
    Function List(AQuery : string  = '') : TSavedReports;
    Function List(AQuery : TReportsSavedlistOptions) : TSavedReports;
  end;
  
  
  { --------------------------------------------------------------------
    TReportsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TReportsResource, method Generate
  
  TReportsGenerateOptions = Record
    accountId : String;
    currency : String;
    dimension : String;
    endDate : String;
    filter : String;
    locale : String;
    maxResults : integer;
    metric : String;
    sort : String;
    startDate : String;
    startIndex : integer;
    useTimezoneReporting : boolean;
  end;
  
  TReportsResource = Class(TGoogleResource)
  Private
    FSavedInstance : TReportsSavedResource;
    Function GetSavedInstance : TReportsSavedResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Generate(AQuery : string  = '') : TAdsenseReportsGenerateResponse;
    Function Generate(AQuery : TReportsgenerateOptions) : TAdsenseReportsGenerateResponse;
    Function CreateSavedResource(AOwner : TComponent) : TReportsSavedResource;virtual;overload;
    Function CreateSavedResource : TReportsSavedResource;virtual;overload;
    Property SavedResource : TReportsSavedResource Read GetSavedInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TSavedadstylesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSavedadstylesResource, method List
  
  TSavedadstylesListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TSavedadstylesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(savedAdStyleId: string) : TSavedAdStyle;
    Function List(AQuery : string  = '') : TSavedAdStyles;
    Function List(AQuery : TSavedadstyleslistOptions) : TSavedAdStyles;
  end;
  
  
  { --------------------------------------------------------------------
    TUrlchannelsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TUrlchannelsResource, method List
  
  TUrlchannelsListOptions = Record
    maxResults : integer;
    pageToken : String;
  end;
  
  TUrlchannelsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(adClientId: string; AQuery : string  = '') : TUrlChannels;
    Function List(adClientId: string; AQuery : TUrlchannelslistOptions) : TUrlChannels;
  end;
  
  
  { --------------------------------------------------------------------
    TAdsenseAPI
    --------------------------------------------------------------------}
  
  TAdsenseAPI = Class(TGoogleAPI)
  Private
    FAccountsAdclientsInstance : TAccountsAdclientsResource;
    FAccountsAdunitsCustomchannelsInstance : TAccountsAdunitsCustomchannelsResource;
    FAccountsAdunitsInstance : TAccountsAdunitsResource;
    FAccountsAlertsInstance : TAccountsAlertsResource;
    FAccountsCustomchannelsAdunitsInstance : TAccountsCustomchannelsAdunitsResource;
    FAccountsCustomchannelsInstance : TAccountsCustomchannelsResource;
    FAccountsPaymentsInstance : TAccountsPaymentsResource;
    FAccountsReportsSavedInstance : TAccountsReportsSavedResource;
    FAccountsReportsInstance : TAccountsReportsResource;
    FAccountsSavedadstylesInstance : TAccountsSavedadstylesResource;
    FAccountsUrlchannelsInstance : TAccountsUrlchannelsResource;
    FAccountsInstance : TAccountsResource;
    FAdclientsInstance : TAdclientsResource;
    FAdunitsCustomchannelsInstance : TAdunitsCustomchannelsResource;
    FAdunitsInstance : TAdunitsResource;
    FAlertsInstance : TAlertsResource;
    FCustomchannelsAdunitsInstance : TCustomchannelsAdunitsResource;
    FCustomchannelsInstance : TCustomchannelsResource;
    FMetadataDimensionsInstance : TMetadataDimensionsResource;
    FMetadataMetricsInstance : TMetadataMetricsResource;
    FMetadataInstance : TMetadataResource;
    FPaymentsInstance : TPaymentsResource;
    FReportsSavedInstance : TReportsSavedResource;
    FReportsInstance : TReportsResource;
    FSavedadstylesInstance : TSavedadstylesResource;
    FUrlchannelsInstance : TUrlchannelsResource;
    Function GetAccountsAdclientsInstance : TAccountsAdclientsResource;virtual;
    Function GetAccountsAdunitsCustomchannelsInstance : TAccountsAdunitsCustomchannelsResource;virtual;
    Function GetAccountsAdunitsInstance : TAccountsAdunitsResource;virtual;
    Function GetAccountsAlertsInstance : TAccountsAlertsResource;virtual;
    Function GetAccountsCustomchannelsAdunitsInstance : TAccountsCustomchannelsAdunitsResource;virtual;
    Function GetAccountsCustomchannelsInstance : TAccountsCustomchannelsResource;virtual;
    Function GetAccountsPaymentsInstance : TAccountsPaymentsResource;virtual;
    Function GetAccountsReportsSavedInstance : TAccountsReportsSavedResource;virtual;
    Function GetAccountsReportsInstance : TAccountsReportsResource;virtual;
    Function GetAccountsSavedadstylesInstance : TAccountsSavedadstylesResource;virtual;
    Function GetAccountsUrlchannelsInstance : TAccountsUrlchannelsResource;virtual;
    Function GetAccountsInstance : TAccountsResource;virtual;
    Function GetAdclientsInstance : TAdclientsResource;virtual;
    Function GetAdunitsCustomchannelsInstance : TAdunitsCustomchannelsResource;virtual;
    Function GetAdunitsInstance : TAdunitsResource;virtual;
    Function GetAlertsInstance : TAlertsResource;virtual;
    Function GetCustomchannelsAdunitsInstance : TCustomchannelsAdunitsResource;virtual;
    Function GetCustomchannelsInstance : TCustomchannelsResource;virtual;
    Function GetMetadataDimensionsInstance : TMetadataDimensionsResource;virtual;
    Function GetMetadataMetricsInstance : TMetadataMetricsResource;virtual;
    Function GetMetadataInstance : TMetadataResource;virtual;
    Function GetPaymentsInstance : TPaymentsResource;virtual;
    Function GetReportsSavedInstance : TReportsSavedResource;virtual;
    Function GetReportsInstance : TReportsResource;virtual;
    Function GetSavedadstylesInstance : TSavedadstylesResource;virtual;
    Function GetUrlchannelsInstance : TUrlchannelsResource;virtual;
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
    Function CreateAccountsAdunitsCustomchannelsResource(AOwner : TComponent) : TAccountsAdunitsCustomchannelsResource;virtual;overload;
    Function CreateAccountsAdunitsCustomchannelsResource : TAccountsAdunitsCustomchannelsResource;virtual;overload;
    Function CreateAccountsAdunitsResource(AOwner : TComponent) : TAccountsAdunitsResource;virtual;overload;
    Function CreateAccountsAdunitsResource : TAccountsAdunitsResource;virtual;overload;
    Function CreateAccountsAlertsResource(AOwner : TComponent) : TAccountsAlertsResource;virtual;overload;
    Function CreateAccountsAlertsResource : TAccountsAlertsResource;virtual;overload;
    Function CreateAccountsCustomchannelsAdunitsResource(AOwner : TComponent) : TAccountsCustomchannelsAdunitsResource;virtual;overload;
    Function CreateAccountsCustomchannelsAdunitsResource : TAccountsCustomchannelsAdunitsResource;virtual;overload;
    Function CreateAccountsCustomchannelsResource(AOwner : TComponent) : TAccountsCustomchannelsResource;virtual;overload;
    Function CreateAccountsCustomchannelsResource : TAccountsCustomchannelsResource;virtual;overload;
    Function CreateAccountsPaymentsResource(AOwner : TComponent) : TAccountsPaymentsResource;virtual;overload;
    Function CreateAccountsPaymentsResource : TAccountsPaymentsResource;virtual;overload;
    Function CreateAccountsReportsSavedResource(AOwner : TComponent) : TAccountsReportsSavedResource;virtual;overload;
    Function CreateAccountsReportsSavedResource : TAccountsReportsSavedResource;virtual;overload;
    Function CreateAccountsReportsResource(AOwner : TComponent) : TAccountsReportsResource;virtual;overload;
    Function CreateAccountsReportsResource : TAccountsReportsResource;virtual;overload;
    Function CreateAccountsSavedadstylesResource(AOwner : TComponent) : TAccountsSavedadstylesResource;virtual;overload;
    Function CreateAccountsSavedadstylesResource : TAccountsSavedadstylesResource;virtual;overload;
    Function CreateAccountsUrlchannelsResource(AOwner : TComponent) : TAccountsUrlchannelsResource;virtual;overload;
    Function CreateAccountsUrlchannelsResource : TAccountsUrlchannelsResource;virtual;overload;
    Function CreateAccountsResource(AOwner : TComponent) : TAccountsResource;virtual;overload;
    Function CreateAccountsResource : TAccountsResource;virtual;overload;
    Function CreateAdclientsResource(AOwner : TComponent) : TAdclientsResource;virtual;overload;
    Function CreateAdclientsResource : TAdclientsResource;virtual;overload;
    Function CreateAdunitsCustomchannelsResource(AOwner : TComponent) : TAdunitsCustomchannelsResource;virtual;overload;
    Function CreateAdunitsCustomchannelsResource : TAdunitsCustomchannelsResource;virtual;overload;
    Function CreateAdunitsResource(AOwner : TComponent) : TAdunitsResource;virtual;overload;
    Function CreateAdunitsResource : TAdunitsResource;virtual;overload;
    Function CreateAlertsResource(AOwner : TComponent) : TAlertsResource;virtual;overload;
    Function CreateAlertsResource : TAlertsResource;virtual;overload;
    Function CreateCustomchannelsAdunitsResource(AOwner : TComponent) : TCustomchannelsAdunitsResource;virtual;overload;
    Function CreateCustomchannelsAdunitsResource : TCustomchannelsAdunitsResource;virtual;overload;
    Function CreateCustomchannelsResource(AOwner : TComponent) : TCustomchannelsResource;virtual;overload;
    Function CreateCustomchannelsResource : TCustomchannelsResource;virtual;overload;
    Function CreateMetadataDimensionsResource(AOwner : TComponent) : TMetadataDimensionsResource;virtual;overload;
    Function CreateMetadataDimensionsResource : TMetadataDimensionsResource;virtual;overload;
    Function CreateMetadataMetricsResource(AOwner : TComponent) : TMetadataMetricsResource;virtual;overload;
    Function CreateMetadataMetricsResource : TMetadataMetricsResource;virtual;overload;
    Function CreateMetadataResource(AOwner : TComponent) : TMetadataResource;virtual;overload;
    Function CreateMetadataResource : TMetadataResource;virtual;overload;
    Function CreatePaymentsResource(AOwner : TComponent) : TPaymentsResource;virtual;overload;
    Function CreatePaymentsResource : TPaymentsResource;virtual;overload;
    Function CreateReportsSavedResource(AOwner : TComponent) : TReportsSavedResource;virtual;overload;
    Function CreateReportsSavedResource : TReportsSavedResource;virtual;overload;
    Function CreateReportsResource(AOwner : TComponent) : TReportsResource;virtual;overload;
    Function CreateReportsResource : TReportsResource;virtual;overload;
    Function CreateSavedadstylesResource(AOwner : TComponent) : TSavedadstylesResource;virtual;overload;
    Function CreateSavedadstylesResource : TSavedadstylesResource;virtual;overload;
    Function CreateUrlchannelsResource(AOwner : TComponent) : TUrlchannelsResource;virtual;overload;
    Function CreateUrlchannelsResource : TUrlchannelsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AccountsAdclientsResource : TAccountsAdclientsResource Read GetAccountsAdclientsInstance;
    Property AccountsAdunitsCustomchannelsResource : TAccountsAdunitsCustomchannelsResource Read GetAccountsAdunitsCustomchannelsInstance;
    Property AccountsAdunitsResource : TAccountsAdunitsResource Read GetAccountsAdunitsInstance;
    Property AccountsAlertsResource : TAccountsAlertsResource Read GetAccountsAlertsInstance;
    Property AccountsCustomchannelsAdunitsResource : TAccountsCustomchannelsAdunitsResource Read GetAccountsCustomchannelsAdunitsInstance;
    Property AccountsCustomchannelsResource : TAccountsCustomchannelsResource Read GetAccountsCustomchannelsInstance;
    Property AccountsPaymentsResource : TAccountsPaymentsResource Read GetAccountsPaymentsInstance;
    Property AccountsReportsSavedResource : TAccountsReportsSavedResource Read GetAccountsReportsSavedInstance;
    Property AccountsReportsResource : TAccountsReportsResource Read GetAccountsReportsInstance;
    Property AccountsSavedadstylesResource : TAccountsSavedadstylesResource Read GetAccountsSavedadstylesInstance;
    Property AccountsUrlchannelsResource : TAccountsUrlchannelsResource Read GetAccountsUrlchannelsInstance;
    Property AccountsResource : TAccountsResource Read GetAccountsInstance;
    Property AdclientsResource : TAdclientsResource Read GetAdclientsInstance;
    Property AdunitsCustomchannelsResource : TAdunitsCustomchannelsResource Read GetAdunitsCustomchannelsInstance;
    Property AdunitsResource : TAdunitsResource Read GetAdunitsInstance;
    Property AlertsResource : TAlertsResource Read GetAlertsInstance;
    Property CustomchannelsAdunitsResource : TCustomchannelsAdunitsResource Read GetCustomchannelsAdunitsInstance;
    Property CustomchannelsResource : TCustomchannelsResource Read GetCustomchannelsInstance;
    Property MetadataDimensionsResource : TMetadataDimensionsResource Read GetMetadataDimensionsInstance;
    Property MetadataMetricsResource : TMetadataMetricsResource Read GetMetadataMetricsInstance;
    Property MetadataResource : TMetadataResource Read GetMetadataInstance;
    Property PaymentsResource : TPaymentsResource Read GetPaymentsInstance;
    Property ReportsSavedResource : TReportsSavedResource Read GetReportsSavedInstance;
    Property ReportsResource : TReportsResource Read GetReportsInstance;
    Property SavedadstylesResource : TSavedadstylesResource Read GetSavedadstylesInstance;
    Property UrlchannelsResource : TUrlchannelsResource Read GetUrlchannelsInstance;
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



Procedure TAccount.Setpremium(AIndex : Integer; AValue : boolean); 

begin
  If (Fpremium=AValue) then exit;
  Fpremium:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.SetsubAccounts(AIndex : Integer; AValue : TAccountTypesubAccountsArray); 

begin
  If (FsubAccounts=AValue) then exit;
  FsubAccounts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Settimezone(AIndex : Integer; AValue : String); 

begin
  If (Ftimezone=AValue) then exit;
  Ftimezone:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAccount.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'subaccounts' : SetLength(FsubAccounts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




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



Procedure TAdClient.SetarcReviewMode(AIndex : Integer; AValue : String); 

begin
  If (FarcReviewMode=AValue) then exit;
  FarcReviewMode:=AValue;
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
  TAdCode
  --------------------------------------------------------------------}


Procedure TAdCode.SetadCode(AIndex : Integer; AValue : String); 

begin
  If (FadCode=AValue) then exit;
  FadCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdCode.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdStyleTypecolors
  --------------------------------------------------------------------}


Procedure TAdStyleTypecolors.Setbackground(AIndex : Integer; AValue : String); 

begin
  If (Fbackground=AValue) then exit;
  Fbackground:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdStyleTypecolors.Setborder(AIndex : Integer; AValue : String); 

begin
  If (Fborder=AValue) then exit;
  Fborder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdStyleTypecolors.Settext(AIndex : Integer; AValue : String); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdStyleTypecolors.Settitle(AIndex : Integer; AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdStyleTypecolors.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdStyleTypefont
  --------------------------------------------------------------------}


Procedure TAdStyleTypefont.Setfamily(AIndex : Integer; AValue : String); 

begin
  If (Ffamily=AValue) then exit;
  Ffamily:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdStyleTypefont.Setsize(AIndex : Integer; AValue : String); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdStyle
  --------------------------------------------------------------------}


Procedure TAdStyle.Setcolors(AIndex : Integer; AValue : TAdStyleTypecolors); 

begin
  If (Fcolors=AValue) then exit;
  Fcolors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdStyle.Setcorners(AIndex : Integer; AValue : String); 

begin
  If (Fcorners=AValue) then exit;
  Fcorners:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdStyle.Setfont(AIndex : Integer; AValue : TAdStyleTypefont); 

begin
  If (Ffont=AValue) then exit;
  Ffont:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdStyle.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdUnitTypecontentAdsSettingsTypebackupOption
  --------------------------------------------------------------------}


Procedure TAdUnitTypecontentAdsSettingsTypebackupOption.Setcolor(AIndex : Integer; AValue : String); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitTypecontentAdsSettingsTypebackupOption.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitTypecontentAdsSettingsTypebackupOption.Seturl(AIndex : Integer; AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAdUnitTypecontentAdsSettingsTypebackupOption.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAdUnitTypecontentAdsSettings
  --------------------------------------------------------------------}


Procedure TAdUnitTypecontentAdsSettings.SetbackupOption(AIndex : Integer; AValue : TAdUnitTypecontentAdsSettingsTypebackupOption); 

begin
  If (FbackupOption=AValue) then exit;
  FbackupOption:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitTypecontentAdsSettings.Setsize(AIndex : Integer; AValue : String); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitTypecontentAdsSettings.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAdUnitTypecontentAdsSettings.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAdUnitTypefeedAdsSettings
  --------------------------------------------------------------------}


Procedure TAdUnitTypefeedAdsSettings.SetadPosition(AIndex : Integer; AValue : String); 

begin
  If (FadPosition=AValue) then exit;
  FadPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitTypefeedAdsSettings.Setfrequency(AIndex : Integer; AValue : integer); 

begin
  If (Ffrequency=AValue) then exit;
  Ffrequency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitTypefeedAdsSettings.SetminimumWordCount(AIndex : Integer; AValue : integer); 

begin
  If (FminimumWordCount=AValue) then exit;
  FminimumWordCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitTypefeedAdsSettings.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAdUnitTypefeedAdsSettings.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAdUnitTypemobileContentAdsSettings
  --------------------------------------------------------------------}


Procedure TAdUnitTypemobileContentAdsSettings.SetmarkupLanguage(AIndex : Integer; AValue : String); 

begin
  If (FmarkupLanguage=AValue) then exit;
  FmarkupLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitTypemobileContentAdsSettings.SetscriptingLanguage(AIndex : Integer; AValue : String); 

begin
  If (FscriptingLanguage=AValue) then exit;
  FscriptingLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitTypemobileContentAdsSettings.Setsize(AIndex : Integer; AValue : String); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitTypemobileContentAdsSettings.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAdUnitTypemobileContentAdsSettings.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAdUnit
  --------------------------------------------------------------------}


Procedure TAdUnit.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnit.SetcontentAdsSettings(AIndex : Integer; AValue : TAdUnitTypecontentAdsSettings); 

begin
  If (FcontentAdsSettings=AValue) then exit;
  FcontentAdsSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnit.SetcustomStyle(AIndex : Integer; AValue : TAdStyle); 

begin
  If (FcustomStyle=AValue) then exit;
  FcustomStyle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnit.SetfeedAdsSettings(AIndex : Integer; AValue : TAdUnitTypefeedAdsSettings); 

begin
  If (FfeedAdsSettings=AValue) then exit;
  FfeedAdsSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnit.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnit.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnit.SetmobileContentAdsSettings(AIndex : Integer; AValue : TAdUnitTypemobileContentAdsSettings); 

begin
  If (FmobileContentAdsSettings=AValue) then exit;
  FmobileContentAdsSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnit.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnit.SetsavedStyleId(AIndex : Integer; AValue : String); 

begin
  If (FsavedStyleId=AValue) then exit;
  FsavedStyleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnit.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdUnits
  --------------------------------------------------------------------}


Procedure TAdUnits.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnits.Setitems(AIndex : Integer; AValue : TAdUnitsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnits.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnits.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAdUnits.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAdsenseReportsGenerateResponseTypeheadersItem
  --------------------------------------------------------------------}


Procedure TAdsenseReportsGenerateResponseTypeheadersItem.Setcurrency(AIndex : Integer; AValue : String); 

begin
  If (Fcurrency=AValue) then exit;
  Fcurrency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponseTypeheadersItem.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponseTypeheadersItem.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAdsenseReportsGenerateResponseTypeheadersItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAdsenseReportsGenerateResponse
  --------------------------------------------------------------------}


Procedure TAdsenseReportsGenerateResponse.Setaverages(AIndex : Integer; AValue : TStringArray); 

begin
  If (Faverages=AValue) then exit;
  Faverages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponse.SetendDate(AIndex : Integer; AValue : String); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponse.Setheaders(AIndex : Integer; AValue : TAdsenseReportsGenerateResponseTypeheadersArray); 

begin
  If (Fheaders=AValue) then exit;
  Fheaders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponse.Setrows(AIndex : Integer; AValue : TAdsenseReportsGenerateResponseTyperowsArray); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponse.SetstartDate(AIndex : Integer; AValue : String); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponse.SettotalMatchedRows(AIndex : Integer; AValue : String); 

begin
  If (FtotalMatchedRows=AValue) then exit;
  FtotalMatchedRows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponse.Settotals(AIndex : Integer; AValue : TStringArray); 

begin
  If (Ftotals=AValue) then exit;
  Ftotals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponse.Setwarnings(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAdsenseReportsGenerateResponse.SetArrayLength(Const AName : String; ALength : Longint); 

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
  TAlert
  --------------------------------------------------------------------}


Procedure TAlert.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAlert.SetisDismissible(AIndex : Integer; AValue : boolean); 

begin
  If (FisDismissible=AValue) then exit;
  FisDismissible:=AValue;
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
  TPayment
  --------------------------------------------------------------------}


Procedure TPayment.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPayment.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPayment.SetpaymentAmount(AIndex : Integer; AValue : String); 

begin
  If (FpaymentAmount=AValue) then exit;
  FpaymentAmount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPayment.SetpaymentAmountCurrencyCode(AIndex : Integer; AValue : String); 

begin
  If (FpaymentAmountCurrencyCode=AValue) then exit;
  FpaymentAmountCurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPayment.SetpaymentDate(AIndex : Integer; AValue : String); 

begin
  If (FpaymentDate=AValue) then exit;
  FpaymentDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPayments
  --------------------------------------------------------------------}


Procedure TPayments.Setitems(AIndex : Integer; AValue : TPaymentsTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPayments.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPayments.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
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
  TSavedAdStyle
  --------------------------------------------------------------------}


Procedure TSavedAdStyle.SetadStyle(AIndex : Integer; AValue : TAdStyle); 

begin
  If (FadStyle=AValue) then exit;
  FadStyle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedAdStyle.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedAdStyle.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedAdStyle.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSavedAdStyles
  --------------------------------------------------------------------}


Procedure TSavedAdStyles.Setetag(AIndex : Integer; AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedAdStyles.Setitems(AIndex : Integer; AValue : TSavedAdStylesTypeitemsArray); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedAdStyles.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedAdStyles.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSavedAdStyles.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'items' : SetLength(Fitems,ALength);
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
  Result:=TadsenseAPI;
end;

Function TAccountsAdclientsResource.List(accountId: string; AQuery : string = '') : TAdClients;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/adclients';
  _Methodid   = 'adsense.accounts.adclients.list';

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
  TAccountsAdunitsCustomchannelsResource
  --------------------------------------------------------------------}


Class Function TAccountsAdunitsCustomchannelsResource.ResourceName : String;

begin
  Result:='customchannels';
end;

Class Function TAccountsAdunitsCustomchannelsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TAccountsAdunitsCustomchannelsResource.List(accountId: string; adClientId: string; adUnitId: string; AQuery : string = '') : TCustomChannels;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/adclients/{adClientId}/adunits/{adUnitId}/customchannels';
  _Methodid   = 'adsense.accounts.adunits.customchannels.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'adClientId',adClientId,'adUnitId',adUnitId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCustomChannels) as TCustomChannels;
end;


Function TAccountsAdunitsCustomchannelsResource.List(accountId: string; adClientId: string; adUnitId: string; AQuery : TAccountsAdunitsCustomchannelslistOptions) : TCustomChannels;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(accountId,adClientId,adUnitId,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsAdunitsResource
  --------------------------------------------------------------------}


Class Function TAccountsAdunitsResource.ResourceName : String;

begin
  Result:='adunits';
end;

Class Function TAccountsAdunitsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TAccountsAdunitsResource.Get(accountId: string; adClientId: string; adUnitId: string) : TAdUnit;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/adclients/{adClientId}/adunits/{adUnitId}';
  _Methodid   = 'adsense.accounts.adunits.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'adClientId',adClientId,'adUnitId',adUnitId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAdUnit) as TAdUnit;
end;

Function TAccountsAdunitsResource.GetAdCode(accountId: string; adClientId: string; adUnitId: string) : TAdCode;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/adclients/{adClientId}/adunits/{adUnitId}/adcode';
  _Methodid   = 'adsense.accounts.adunits.getAdCode';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'adClientId',adClientId,'adUnitId',adUnitId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAdCode) as TAdCode;
end;

Function TAccountsAdunitsResource.List(accountId: string; adClientId: string; AQuery : string = '') : TAdUnits;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/adclients/{adClientId}/adunits';
  _Methodid   = 'adsense.accounts.adunits.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'adClientId',adClientId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAdUnits) as TAdUnits;
end;


Function TAccountsAdunitsResource.List(accountId: string; adClientId: string; AQuery : TAccountsAdunitslistOptions) : TAdUnits;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeInactive',AQuery.includeInactive);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(accountId,adClientId,_Q);
end;



Function TAccountsAdunitsResource.GetCustomchannelsInstance : TAccountsAdunitsCustomchannelsResource;

begin
  if (FCustomchannelsInstance=Nil) then
    FCustomchannelsInstance:=CreateCustomchannelsResource;
  Result:=FCustomchannelsInstance;
end;

Function TAccountsAdunitsResource.CreateCustomchannelsResource : TAccountsAdunitsCustomchannelsResource;

begin
  Result:=CreateCustomchannelsResource(Self);
end;


Function TAccountsAdunitsResource.CreateCustomchannelsResource(AOwner : TComponent) : TAccountsAdunitsCustomchannelsResource;

begin
  Result:=TAccountsAdunitsCustomchannelsResource.Create(AOwner);
  Result.API:=Self.API;
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
  Result:=TadsenseAPI;
end;

Procedure TAccountsAlertsResource.Delete(accountId: string; alertId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'accounts/{accountId}/alerts/{alertId}';
  _Methodid   = 'adsense.accounts.alerts.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'alertId',alertId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAccountsAlertsResource.List(accountId: string; AQuery : string = '') : TAlerts;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/alerts';
  _Methodid   = 'adsense.accounts.alerts.list';

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
  TAccountsCustomchannelsAdunitsResource
  --------------------------------------------------------------------}


Class Function TAccountsCustomchannelsAdunitsResource.ResourceName : String;

begin
  Result:='adunits';
end;

Class Function TAccountsCustomchannelsAdunitsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TAccountsCustomchannelsAdunitsResource.List(accountId: string; adClientId: string; customChannelId: string; AQuery : string = '') : TAdUnits;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/adclients/{adClientId}/customchannels/{customChannelId}/adunits';
  _Methodid   = 'adsense.accounts.customchannels.adunits.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'adClientId',adClientId,'customChannelId',customChannelId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAdUnits) as TAdUnits;
end;


Function TAccountsCustomchannelsAdunitsResource.List(accountId: string; adClientId: string; customChannelId: string; AQuery : TAccountsCustomchannelsAdunitslistOptions) : TAdUnits;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeInactive',AQuery.includeInactive);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(accountId,adClientId,customChannelId,_Q);
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
  Result:=TadsenseAPI;
end;

Function TAccountsCustomchannelsResource.Get(accountId: string; adClientId: string; customChannelId: string) : TCustomChannel;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/adclients/{adClientId}/customchannels/{customChannelId}';
  _Methodid   = 'adsense.accounts.customchannels.get';

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
  _Methodid   = 'adsense.accounts.customchannels.list';

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



Function TAccountsCustomchannelsResource.GetAdunitsInstance : TAccountsCustomchannelsAdunitsResource;

begin
  if (FAdunitsInstance=Nil) then
    FAdunitsInstance:=CreateAdunitsResource;
  Result:=FAdunitsInstance;
end;

Function TAccountsCustomchannelsResource.CreateAdunitsResource : TAccountsCustomchannelsAdunitsResource;

begin
  Result:=CreateAdunitsResource(Self);
end;


Function TAccountsCustomchannelsResource.CreateAdunitsResource(AOwner : TComponent) : TAccountsCustomchannelsAdunitsResource;

begin
  Result:=TAccountsCustomchannelsAdunitsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TAccountsPaymentsResource
  --------------------------------------------------------------------}


Class Function TAccountsPaymentsResource.ResourceName : String;

begin
  Result:='payments';
end;

Class Function TAccountsPaymentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TAccountsPaymentsResource.List(accountId: string) : TPayments;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/payments';
  _Methodid   = 'adsense.accounts.payments.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TPayments) as TPayments;
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
  Result:=TadsenseAPI;
end;

Function TAccountsReportsSavedResource.Generate(accountId: string; savedReportId: string; AQuery : string = '') : TAdsenseReportsGenerateResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/reports/{savedReportId}';
  _Methodid   = 'adsense.accounts.reports.saved.generate';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'savedReportId',savedReportId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAdsenseReportsGenerateResponse) as TAdsenseReportsGenerateResponse;
end;


Function TAccountsReportsSavedResource.Generate(accountId: string; savedReportId: string; AQuery : TAccountsReportsSavedgenerateOptions) : TAdsenseReportsGenerateResponse;

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
  _Methodid   = 'adsense.accounts.reports.saved.list';

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
  Result:=TadsenseAPI;
end;

Function TAccountsReportsResource.Generate(accountId: string; AQuery : string = '') : TAdsenseReportsGenerateResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/reports';
  _Methodid   = 'adsense.accounts.reports.generate';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAdsenseReportsGenerateResponse) as TAdsenseReportsGenerateResponse;
end;


Function TAccountsReportsResource.Generate(accountId: string; AQuery : TAccountsReportsgenerateOptions) : TAdsenseReportsGenerateResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'currency',AQuery.currency);
  AddToQuery(_Q,'dimension',AQuery.dimension);
  AddToQuery(_Q,'endDate',AQuery.endDate);
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'metric',AQuery.metric);
  AddToQuery(_Q,'sort',AQuery.sort);
  AddToQuery(_Q,'startDate',AQuery.startDate);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  AddToQuery(_Q,'useTimezoneReporting',AQuery.useTimezoneReporting);
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
  TAccountsSavedadstylesResource
  --------------------------------------------------------------------}


Class Function TAccountsSavedadstylesResource.ResourceName : String;

begin
  Result:='savedadstyles';
end;

Class Function TAccountsSavedadstylesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TAccountsSavedadstylesResource.Get(accountId: string; savedAdStyleId: string) : TSavedAdStyle;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/savedadstyles/{savedAdStyleId}';
  _Methodid   = 'adsense.accounts.savedadstyles.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'savedAdStyleId',savedAdStyleId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSavedAdStyle) as TSavedAdStyle;
end;

Function TAccountsSavedadstylesResource.List(accountId: string; AQuery : string = '') : TSavedAdStyles;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/savedadstyles';
  _Methodid   = 'adsense.accounts.savedadstyles.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TSavedAdStyles) as TSavedAdStyles;
end;


Function TAccountsSavedadstylesResource.List(accountId: string; AQuery : TAccountsSavedadstyleslistOptions) : TSavedAdStyles;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(accountId,_Q);
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
  Result:=TadsenseAPI;
end;

Function TAccountsUrlchannelsResource.List(accountId: string; adClientId: string; AQuery : string = '') : TUrlChannels;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}/adclients/{adClientId}/urlchannels';
  _Methodid   = 'adsense.accounts.urlchannels.list';

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
  Result:=TadsenseAPI;
end;

Function TAccountsResource.Get(accountId: string; AQuery : string = '') : TAccount;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}';
  _Methodid   = 'adsense.accounts.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAccount) as TAccount;
end;


Function TAccountsResource.Get(accountId: string; AQuery : TAccountsgetOptions) : TAccount;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'tree',AQuery.tree);
  Result:=Get(accountId,_Q);
end;

Function TAccountsResource.List(AQuery : string = '') : TAccounts;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts';
  _Methodid   = 'adsense.accounts.list';

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



Function TAccountsResource.GetAdunitsCustomchannelsInstance : TAccountsAdunitsCustomchannelsResource;

begin
  if (FAdunitsCustomchannelsInstance=Nil) then
    FAdunitsCustomchannelsInstance:=CreateAdunitsCustomchannelsResource;
  Result:=FAdunitsCustomchannelsInstance;
end;

Function TAccountsResource.CreateAdunitsCustomchannelsResource : TAccountsAdunitsCustomchannelsResource;

begin
  Result:=CreateAdunitsCustomchannelsResource(Self);
end;


Function TAccountsResource.CreateAdunitsCustomchannelsResource(AOwner : TComponent) : TAccountsAdunitsCustomchannelsResource;

begin
  Result:=TAccountsAdunitsCustomchannelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetAdunitsInstance : TAccountsAdunitsResource;

begin
  if (FAdunitsInstance=Nil) then
    FAdunitsInstance:=CreateAdunitsResource;
  Result:=FAdunitsInstance;
end;

Function TAccountsResource.CreateAdunitsResource : TAccountsAdunitsResource;

begin
  Result:=CreateAdunitsResource(Self);
end;


Function TAccountsResource.CreateAdunitsResource(AOwner : TComponent) : TAccountsAdunitsResource;

begin
  Result:=TAccountsAdunitsResource.Create(AOwner);
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



Function TAccountsResource.GetCustomchannelsAdunitsInstance : TAccountsCustomchannelsAdunitsResource;

begin
  if (FCustomchannelsAdunitsInstance=Nil) then
    FCustomchannelsAdunitsInstance:=CreateCustomchannelsAdunitsResource;
  Result:=FCustomchannelsAdunitsInstance;
end;

Function TAccountsResource.CreateCustomchannelsAdunitsResource : TAccountsCustomchannelsAdunitsResource;

begin
  Result:=CreateCustomchannelsAdunitsResource(Self);
end;


Function TAccountsResource.CreateCustomchannelsAdunitsResource(AOwner : TComponent) : TAccountsCustomchannelsAdunitsResource;

begin
  Result:=TAccountsCustomchannelsAdunitsResource.Create(AOwner);
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



Function TAccountsResource.GetPaymentsInstance : TAccountsPaymentsResource;

begin
  if (FPaymentsInstance=Nil) then
    FPaymentsInstance:=CreatePaymentsResource;
  Result:=FPaymentsInstance;
end;

Function TAccountsResource.CreatePaymentsResource : TAccountsPaymentsResource;

begin
  Result:=CreatePaymentsResource(Self);
end;


Function TAccountsResource.CreatePaymentsResource(AOwner : TComponent) : TAccountsPaymentsResource;

begin
  Result:=TAccountsPaymentsResource.Create(AOwner);
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



Function TAccountsResource.GetSavedadstylesInstance : TAccountsSavedadstylesResource;

begin
  if (FSavedadstylesInstance=Nil) then
    FSavedadstylesInstance:=CreateSavedadstylesResource;
  Result:=FSavedadstylesInstance;
end;

Function TAccountsResource.CreateSavedadstylesResource : TAccountsSavedadstylesResource;

begin
  Result:=CreateSavedadstylesResource(Self);
end;


Function TAccountsResource.CreateSavedadstylesResource(AOwner : TComponent) : TAccountsSavedadstylesResource;

begin
  Result:=TAccountsSavedadstylesResource.Create(AOwner);
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
  TAdclientsResource
  --------------------------------------------------------------------}


Class Function TAdclientsResource.ResourceName : String;

begin
  Result:='adclients';
end;

Class Function TAdclientsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TAdclientsResource.List(AQuery : string = '') : TAdClients;

Const
  _HTTPMethod = 'GET';
  _Path       = 'adclients';
  _Methodid   = 'adsense.adclients.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TAdClients) as TAdClients;
end;


Function TAdclientsResource.List(AQuery : TAdclientslistOptions) : TAdClients;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TAdunitsCustomchannelsResource
  --------------------------------------------------------------------}


Class Function TAdunitsCustomchannelsResource.ResourceName : String;

begin
  Result:='customchannels';
end;

Class Function TAdunitsCustomchannelsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TAdunitsCustomchannelsResource.List(adClientId: string; adUnitId: string; AQuery : string = '') : TCustomChannels;

Const
  _HTTPMethod = 'GET';
  _Path       = 'adclients/{adClientId}/adunits/{adUnitId}/customchannels';
  _Methodid   = 'adsense.adunits.customchannels.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['adClientId',adClientId,'adUnitId',adUnitId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCustomChannels) as TCustomChannels;
end;


Function TAdunitsCustomchannelsResource.List(adClientId: string; adUnitId: string; AQuery : TAdunitsCustomchannelslistOptions) : TCustomChannels;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(adClientId,adUnitId,_Q);
end;



{ --------------------------------------------------------------------
  TAdunitsResource
  --------------------------------------------------------------------}


Class Function TAdunitsResource.ResourceName : String;

begin
  Result:='adunits';
end;

Class Function TAdunitsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TAdunitsResource.Get(adClientId: string; adUnitId: string) : TAdUnit;

Const
  _HTTPMethod = 'GET';
  _Path       = 'adclients/{adClientId}/adunits/{adUnitId}';
  _Methodid   = 'adsense.adunits.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['adClientId',adClientId,'adUnitId',adUnitId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAdUnit) as TAdUnit;
end;

Function TAdunitsResource.GetAdCode(adClientId: string; adUnitId: string) : TAdCode;

Const
  _HTTPMethod = 'GET';
  _Path       = 'adclients/{adClientId}/adunits/{adUnitId}/adcode';
  _Methodid   = 'adsense.adunits.getAdCode';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['adClientId',adClientId,'adUnitId',adUnitId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAdCode) as TAdCode;
end;

Function TAdunitsResource.List(adClientId: string; AQuery : string = '') : TAdUnits;

Const
  _HTTPMethod = 'GET';
  _Path       = 'adclients/{adClientId}/adunits';
  _Methodid   = 'adsense.adunits.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['adClientId',adClientId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAdUnits) as TAdUnits;
end;


Function TAdunitsResource.List(adClientId: string; AQuery : TAdunitslistOptions) : TAdUnits;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeInactive',AQuery.includeInactive);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(adClientId,_Q);
end;



Function TAdunitsResource.GetCustomchannelsInstance : TAdunitsCustomchannelsResource;

begin
  if (FCustomchannelsInstance=Nil) then
    FCustomchannelsInstance:=CreateCustomchannelsResource;
  Result:=FCustomchannelsInstance;
end;

Function TAdunitsResource.CreateCustomchannelsResource : TAdunitsCustomchannelsResource;

begin
  Result:=CreateCustomchannelsResource(Self);
end;


Function TAdunitsResource.CreateCustomchannelsResource(AOwner : TComponent) : TAdunitsCustomchannelsResource;

begin
  Result:=TAdunitsCustomchannelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TAlertsResource
  --------------------------------------------------------------------}


Class Function TAlertsResource.ResourceName : String;

begin
  Result:='alerts';
end;

Class Function TAlertsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Procedure TAlertsResource.Delete(alertId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'alerts/{alertId}';
  _Methodid   = 'adsense.alerts.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['alertId',alertId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAlertsResource.List(AQuery : string = '') : TAlerts;

Const
  _HTTPMethod = 'GET';
  _Path       = 'alerts';
  _Methodid   = 'adsense.alerts.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TAlerts) as TAlerts;
end;


Function TAlertsResource.List(AQuery : TAlertslistOptions) : TAlerts;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'locale',AQuery.locale);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TCustomchannelsAdunitsResource
  --------------------------------------------------------------------}


Class Function TCustomchannelsAdunitsResource.ResourceName : String;

begin
  Result:='adunits';
end;

Class Function TCustomchannelsAdunitsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TCustomchannelsAdunitsResource.List(adClientId: string; customChannelId: string; AQuery : string = '') : TAdUnits;

Const
  _HTTPMethod = 'GET';
  _Path       = 'adclients/{adClientId}/customchannels/{customChannelId}/adunits';
  _Methodid   = 'adsense.customchannels.adunits.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['adClientId',adClientId,'customChannelId',customChannelId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAdUnits) as TAdUnits;
end;


Function TCustomchannelsAdunitsResource.List(adClientId: string; customChannelId: string; AQuery : TCustomchannelsAdunitslistOptions) : TAdUnits;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'includeInactive',AQuery.includeInactive);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(adClientId,customChannelId,_Q);
end;



{ --------------------------------------------------------------------
  TCustomchannelsResource
  --------------------------------------------------------------------}


Class Function TCustomchannelsResource.ResourceName : String;

begin
  Result:='customchannels';
end;

Class Function TCustomchannelsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TCustomchannelsResource.Get(adClientId: string; customChannelId: string) : TCustomChannel;

Const
  _HTTPMethod = 'GET';
  _Path       = 'adclients/{adClientId}/customchannels/{customChannelId}';
  _Methodid   = 'adsense.customchannels.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['adClientId',adClientId,'customChannelId',customChannelId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCustomChannel) as TCustomChannel;
end;

Function TCustomchannelsResource.List(adClientId: string; AQuery : string = '') : TCustomChannels;

Const
  _HTTPMethod = 'GET';
  _Path       = 'adclients/{adClientId}/customchannels';
  _Methodid   = 'adsense.customchannels.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['adClientId',adClientId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TCustomChannels) as TCustomChannels;
end;


Function TCustomchannelsResource.List(adClientId: string; AQuery : TCustomchannelslistOptions) : TCustomChannels;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(adClientId,_Q);
end;



Function TCustomchannelsResource.GetAdunitsInstance : TCustomchannelsAdunitsResource;

begin
  if (FAdunitsInstance=Nil) then
    FAdunitsInstance:=CreateAdunitsResource;
  Result:=FAdunitsInstance;
end;

Function TCustomchannelsResource.CreateAdunitsResource : TCustomchannelsAdunitsResource;

begin
  Result:=CreateAdunitsResource(Self);
end;


Function TCustomchannelsResource.CreateAdunitsResource(AOwner : TComponent) : TCustomchannelsAdunitsResource;

begin
  Result:=TCustomchannelsAdunitsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TMetadataDimensionsResource
  --------------------------------------------------------------------}


Class Function TMetadataDimensionsResource.ResourceName : String;

begin
  Result:='dimensions';
end;

Class Function TMetadataDimensionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TMetadataDimensionsResource.List : TMetadata;

Const
  _HTTPMethod = 'GET';
  _Path       = 'metadata/dimensions';
  _Methodid   = 'adsense.metadata.dimensions.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TMetadata) as TMetadata;
end;



{ --------------------------------------------------------------------
  TMetadataMetricsResource
  --------------------------------------------------------------------}


Class Function TMetadataMetricsResource.ResourceName : String;

begin
  Result:='metrics';
end;

Class Function TMetadataMetricsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TMetadataMetricsResource.List : TMetadata;

Const
  _HTTPMethod = 'GET';
  _Path       = 'metadata/metrics';
  _Methodid   = 'adsense.metadata.metrics.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TMetadata) as TMetadata;
end;



{ --------------------------------------------------------------------
  TMetadataResource
  --------------------------------------------------------------------}


Class Function TMetadataResource.ResourceName : String;

begin
  Result:='metadata';
end;

Class Function TMetadataResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;



Function TMetadataResource.GetDimensionsInstance : TMetadataDimensionsResource;

begin
  if (FDimensionsInstance=Nil) then
    FDimensionsInstance:=CreateDimensionsResource;
  Result:=FDimensionsInstance;
end;

Function TMetadataResource.CreateDimensionsResource : TMetadataDimensionsResource;

begin
  Result:=CreateDimensionsResource(Self);
end;


Function TMetadataResource.CreateDimensionsResource(AOwner : TComponent) : TMetadataDimensionsResource;

begin
  Result:=TMetadataDimensionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TMetadataResource.GetMetricsInstance : TMetadataMetricsResource;

begin
  if (FMetricsInstance=Nil) then
    FMetricsInstance:=CreateMetricsResource;
  Result:=FMetricsInstance;
end;

Function TMetadataResource.CreateMetricsResource : TMetadataMetricsResource;

begin
  Result:=CreateMetricsResource(Self);
end;


Function TMetadataResource.CreateMetricsResource(AOwner : TComponent) : TMetadataMetricsResource;

begin
  Result:=TMetadataMetricsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TPaymentsResource
  --------------------------------------------------------------------}


Class Function TPaymentsResource.ResourceName : String;

begin
  Result:='payments';
end;

Class Function TPaymentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TPaymentsResource.List : TPayments;

Const
  _HTTPMethod = 'GET';
  _Path       = 'payments';
  _Methodid   = 'adsense.payments.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TPayments) as TPayments;
end;



{ --------------------------------------------------------------------
  TReportsSavedResource
  --------------------------------------------------------------------}


Class Function TReportsSavedResource.ResourceName : String;

begin
  Result:='saved';
end;

Class Function TReportsSavedResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TReportsSavedResource.Generate(savedReportId: string; AQuery : string = '') : TAdsenseReportsGenerateResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'reports/{savedReportId}';
  _Methodid   = 'adsense.reports.saved.generate';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['savedReportId',savedReportId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAdsenseReportsGenerateResponse) as TAdsenseReportsGenerateResponse;
end;


Function TReportsSavedResource.Generate(savedReportId: string; AQuery : TReportsSavedgenerateOptions) : TAdsenseReportsGenerateResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  Result:=Generate(savedReportId,_Q);
end;

Function TReportsSavedResource.List(AQuery : string = '') : TSavedReports;

Const
  _HTTPMethod = 'GET';
  _Path       = 'reports/saved';
  _Methodid   = 'adsense.reports.saved.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TSavedReports) as TSavedReports;
end;


Function TReportsSavedResource.List(AQuery : TReportsSavedlistOptions) : TSavedReports;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TReportsResource
  --------------------------------------------------------------------}


Class Function TReportsResource.ResourceName : String;

begin
  Result:='reports';
end;

Class Function TReportsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TReportsResource.Generate(AQuery : string = '') : TAdsenseReportsGenerateResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'reports';
  _Methodid   = 'adsense.reports.generate';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TAdsenseReportsGenerateResponse) as TAdsenseReportsGenerateResponse;
end;


Function TReportsResource.Generate(AQuery : TReportsgenerateOptions) : TAdsenseReportsGenerateResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'accountId',AQuery.accountId);
  AddToQuery(_Q,'currency',AQuery.currency);
  AddToQuery(_Q,'dimension',AQuery.dimension);
  AddToQuery(_Q,'endDate',AQuery.endDate);
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'locale',AQuery.locale);
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'metric',AQuery.metric);
  AddToQuery(_Q,'sort',AQuery.sort);
  AddToQuery(_Q,'startDate',AQuery.startDate);
  AddToQuery(_Q,'startIndex',AQuery.startIndex);
  AddToQuery(_Q,'useTimezoneReporting',AQuery.useTimezoneReporting);
  Result:=Generate(_Q);
end;



Function TReportsResource.GetSavedInstance : TReportsSavedResource;

begin
  if (FSavedInstance=Nil) then
    FSavedInstance:=CreateSavedResource;
  Result:=FSavedInstance;
end;

Function TReportsResource.CreateSavedResource : TReportsSavedResource;

begin
  Result:=CreateSavedResource(Self);
end;


Function TReportsResource.CreateSavedResource(AOwner : TComponent) : TReportsSavedResource;

begin
  Result:=TReportsSavedResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TSavedadstylesResource
  --------------------------------------------------------------------}


Class Function TSavedadstylesResource.ResourceName : String;

begin
  Result:='savedadstyles';
end;

Class Function TSavedadstylesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TSavedadstylesResource.Get(savedAdStyleId: string) : TSavedAdStyle;

Const
  _HTTPMethod = 'GET';
  _Path       = 'savedadstyles/{savedAdStyleId}';
  _Methodid   = 'adsense.savedadstyles.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['savedAdStyleId',savedAdStyleId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TSavedAdStyle) as TSavedAdStyle;
end;

Function TSavedadstylesResource.List(AQuery : string = '') : TSavedAdStyles;

Const
  _HTTPMethod = 'GET';
  _Path       = 'savedadstyles';
  _Methodid   = 'adsense.savedadstyles.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TSavedAdStyles) as TSavedAdStyles;
end;


Function TSavedadstylesResource.List(AQuery : TSavedadstyleslistOptions) : TSavedAdStyles;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;



{ --------------------------------------------------------------------
  TUrlchannelsResource
  --------------------------------------------------------------------}


Class Function TUrlchannelsResource.ResourceName : String;

begin
  Result:='urlchannels';
end;

Class Function TUrlchannelsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsenseAPI;
end;

Function TUrlchannelsResource.List(adClientId: string; AQuery : string = '') : TUrlChannels;

Const
  _HTTPMethod = 'GET';
  _Path       = 'adclients/{adClientId}/urlchannels';
  _Methodid   = 'adsense.urlchannels.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['adClientId',adClientId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TUrlChannels) as TUrlChannels;
end;


Function TUrlchannelsResource.List(adClientId: string; AQuery : TUrlchannelslistOptions) : TUrlChannels;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(adClientId,_Q);
end;



{ --------------------------------------------------------------------
  TAdsenseAPI
  --------------------------------------------------------------------}

Class Function TAdsenseAPI.APIName : String;

begin
  Result:='adsense';
end;

Class Function TAdsenseAPI.APIVersion : String;

begin
  Result:='v1.4';
end;

Class Function TAdsenseAPI.APIRevision : String;

begin
  Result:='20150401';
end;

Class Function TAdsenseAPI.APIID : String;

begin
  Result:='adsense:v1.4';
end;

Class Function TAdsenseAPI.APITitle : String;

begin
  Result:='AdSense Management API';
end;

Class Function TAdsenseAPI.APIDescription : String;

begin
  Result:='Gives AdSense publishers access to their inventory and the ability to generate reports';
end;

Class Function TAdsenseAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TAdsenseAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TAdsenseAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/adsense-16.png';
end;

Class Function TAdsenseAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/adsense-32.png';
end;

Class Function TAdsenseAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/adsense/management/';
end;

Class Function TAdsenseAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TAdsenseAPI.APIbasePath : string;

begin
  Result:='/adsense/v1.4/';
end;

Class Function TAdsenseAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/adsense/v1.4/';
end;

Class Function TAdsenseAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TAdsenseAPI.APIservicePath : string;

begin
  Result:='adsense/v1.4/';
end;

Class Function TAdsenseAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TAdsenseAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/adsense';
  Result[0].Description:='View and manage your AdSense data';
  Result[1].Name:='https://www.googleapis.com/auth/adsense.readonly';
  Result[1].Description:='View your AdSense data';
  
end;

Class Function TAdsenseAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TAdsenseAPI.RegisterAPIResources;

begin
  TAccount.RegisterObject;
  TAccounts.RegisterObject;
  TAdClient.RegisterObject;
  TAdClients.RegisterObject;
  TAdCode.RegisterObject;
  TAdStyleTypecolors.RegisterObject;
  TAdStyleTypefont.RegisterObject;
  TAdStyle.RegisterObject;
  TAdUnitTypecontentAdsSettingsTypebackupOption.RegisterObject;
  TAdUnitTypecontentAdsSettings.RegisterObject;
  TAdUnitTypefeedAdsSettings.RegisterObject;
  TAdUnitTypemobileContentAdsSettings.RegisterObject;
  TAdUnit.RegisterObject;
  TAdUnits.RegisterObject;
  TAdsenseReportsGenerateResponseTypeheadersItem.RegisterObject;
  TAdsenseReportsGenerateResponse.RegisterObject;
  TAlert.RegisterObject;
  TAlerts.RegisterObject;
  TCustomChannelTypetargetingInfo.RegisterObject;
  TCustomChannel.RegisterObject;
  TCustomChannels.RegisterObject;
  TMetadata.RegisterObject;
  TPayment.RegisterObject;
  TPayments.RegisterObject;
  TReportingMetadataEntry.RegisterObject;
  TSavedAdStyle.RegisterObject;
  TSavedAdStyles.RegisterObject;
  TSavedReport.RegisterObject;
  TSavedReports.RegisterObject;
  TUrlChannel.RegisterObject;
  TUrlChannels.RegisterObject;
end;


Function TAdsenseAPI.GetAccountsAdclientsInstance : TAccountsAdclientsResource;

begin
  if (FAccountsAdclientsInstance=Nil) then
    FAccountsAdclientsInstance:=CreateAccountsAdclientsResource;
  Result:=FAccountsAdclientsInstance;
end;

Function TAdsenseAPI.CreateAccountsAdclientsResource : TAccountsAdclientsResource;

begin
  Result:=CreateAccountsAdclientsResource(Self);
end;


Function TAdsenseAPI.CreateAccountsAdclientsResource(AOwner : TComponent) : TAccountsAdclientsResource;

begin
  Result:=TAccountsAdclientsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetAccountsAdunitsCustomchannelsInstance : TAccountsAdunitsCustomchannelsResource;

begin
  if (FAccountsAdunitsCustomchannelsInstance=Nil) then
    FAccountsAdunitsCustomchannelsInstance:=CreateAccountsAdunitsCustomchannelsResource;
  Result:=FAccountsAdunitsCustomchannelsInstance;
end;

Function TAdsenseAPI.CreateAccountsAdunitsCustomchannelsResource : TAccountsAdunitsCustomchannelsResource;

begin
  Result:=CreateAccountsAdunitsCustomchannelsResource(Self);
end;


Function TAdsenseAPI.CreateAccountsAdunitsCustomchannelsResource(AOwner : TComponent) : TAccountsAdunitsCustomchannelsResource;

begin
  Result:=TAccountsAdunitsCustomchannelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetAccountsAdunitsInstance : TAccountsAdunitsResource;

begin
  if (FAccountsAdunitsInstance=Nil) then
    FAccountsAdunitsInstance:=CreateAccountsAdunitsResource;
  Result:=FAccountsAdunitsInstance;
end;

Function TAdsenseAPI.CreateAccountsAdunitsResource : TAccountsAdunitsResource;

begin
  Result:=CreateAccountsAdunitsResource(Self);
end;


Function TAdsenseAPI.CreateAccountsAdunitsResource(AOwner : TComponent) : TAccountsAdunitsResource;

begin
  Result:=TAccountsAdunitsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetAccountsAlertsInstance : TAccountsAlertsResource;

begin
  if (FAccountsAlertsInstance=Nil) then
    FAccountsAlertsInstance:=CreateAccountsAlertsResource;
  Result:=FAccountsAlertsInstance;
end;

Function TAdsenseAPI.CreateAccountsAlertsResource : TAccountsAlertsResource;

begin
  Result:=CreateAccountsAlertsResource(Self);
end;


Function TAdsenseAPI.CreateAccountsAlertsResource(AOwner : TComponent) : TAccountsAlertsResource;

begin
  Result:=TAccountsAlertsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetAccountsCustomchannelsAdunitsInstance : TAccountsCustomchannelsAdunitsResource;

begin
  if (FAccountsCustomchannelsAdunitsInstance=Nil) then
    FAccountsCustomchannelsAdunitsInstance:=CreateAccountsCustomchannelsAdunitsResource;
  Result:=FAccountsCustomchannelsAdunitsInstance;
end;

Function TAdsenseAPI.CreateAccountsCustomchannelsAdunitsResource : TAccountsCustomchannelsAdunitsResource;

begin
  Result:=CreateAccountsCustomchannelsAdunitsResource(Self);
end;


Function TAdsenseAPI.CreateAccountsCustomchannelsAdunitsResource(AOwner : TComponent) : TAccountsCustomchannelsAdunitsResource;

begin
  Result:=TAccountsCustomchannelsAdunitsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetAccountsCustomchannelsInstance : TAccountsCustomchannelsResource;

begin
  if (FAccountsCustomchannelsInstance=Nil) then
    FAccountsCustomchannelsInstance:=CreateAccountsCustomchannelsResource;
  Result:=FAccountsCustomchannelsInstance;
end;

Function TAdsenseAPI.CreateAccountsCustomchannelsResource : TAccountsCustomchannelsResource;

begin
  Result:=CreateAccountsCustomchannelsResource(Self);
end;


Function TAdsenseAPI.CreateAccountsCustomchannelsResource(AOwner : TComponent) : TAccountsCustomchannelsResource;

begin
  Result:=TAccountsCustomchannelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetAccountsPaymentsInstance : TAccountsPaymentsResource;

begin
  if (FAccountsPaymentsInstance=Nil) then
    FAccountsPaymentsInstance:=CreateAccountsPaymentsResource;
  Result:=FAccountsPaymentsInstance;
end;

Function TAdsenseAPI.CreateAccountsPaymentsResource : TAccountsPaymentsResource;

begin
  Result:=CreateAccountsPaymentsResource(Self);
end;


Function TAdsenseAPI.CreateAccountsPaymentsResource(AOwner : TComponent) : TAccountsPaymentsResource;

begin
  Result:=TAccountsPaymentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetAccountsReportsSavedInstance : TAccountsReportsSavedResource;

begin
  if (FAccountsReportsSavedInstance=Nil) then
    FAccountsReportsSavedInstance:=CreateAccountsReportsSavedResource;
  Result:=FAccountsReportsSavedInstance;
end;

Function TAdsenseAPI.CreateAccountsReportsSavedResource : TAccountsReportsSavedResource;

begin
  Result:=CreateAccountsReportsSavedResource(Self);
end;


Function TAdsenseAPI.CreateAccountsReportsSavedResource(AOwner : TComponent) : TAccountsReportsSavedResource;

begin
  Result:=TAccountsReportsSavedResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetAccountsReportsInstance : TAccountsReportsResource;

begin
  if (FAccountsReportsInstance=Nil) then
    FAccountsReportsInstance:=CreateAccountsReportsResource;
  Result:=FAccountsReportsInstance;
end;

Function TAdsenseAPI.CreateAccountsReportsResource : TAccountsReportsResource;

begin
  Result:=CreateAccountsReportsResource(Self);
end;


Function TAdsenseAPI.CreateAccountsReportsResource(AOwner : TComponent) : TAccountsReportsResource;

begin
  Result:=TAccountsReportsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetAccountsSavedadstylesInstance : TAccountsSavedadstylesResource;

begin
  if (FAccountsSavedadstylesInstance=Nil) then
    FAccountsSavedadstylesInstance:=CreateAccountsSavedadstylesResource;
  Result:=FAccountsSavedadstylesInstance;
end;

Function TAdsenseAPI.CreateAccountsSavedadstylesResource : TAccountsSavedadstylesResource;

begin
  Result:=CreateAccountsSavedadstylesResource(Self);
end;


Function TAdsenseAPI.CreateAccountsSavedadstylesResource(AOwner : TComponent) : TAccountsSavedadstylesResource;

begin
  Result:=TAccountsSavedadstylesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetAccountsUrlchannelsInstance : TAccountsUrlchannelsResource;

begin
  if (FAccountsUrlchannelsInstance=Nil) then
    FAccountsUrlchannelsInstance:=CreateAccountsUrlchannelsResource;
  Result:=FAccountsUrlchannelsInstance;
end;

Function TAdsenseAPI.CreateAccountsUrlchannelsResource : TAccountsUrlchannelsResource;

begin
  Result:=CreateAccountsUrlchannelsResource(Self);
end;


Function TAdsenseAPI.CreateAccountsUrlchannelsResource(AOwner : TComponent) : TAccountsUrlchannelsResource;

begin
  Result:=TAccountsUrlchannelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetAccountsInstance : TAccountsResource;

begin
  if (FAccountsInstance=Nil) then
    FAccountsInstance:=CreateAccountsResource;
  Result:=FAccountsInstance;
end;

Function TAdsenseAPI.CreateAccountsResource : TAccountsResource;

begin
  Result:=CreateAccountsResource(Self);
end;


Function TAdsenseAPI.CreateAccountsResource(AOwner : TComponent) : TAccountsResource;

begin
  Result:=TAccountsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetAdclientsInstance : TAdclientsResource;

begin
  if (FAdclientsInstance=Nil) then
    FAdclientsInstance:=CreateAdclientsResource;
  Result:=FAdclientsInstance;
end;

Function TAdsenseAPI.CreateAdclientsResource : TAdclientsResource;

begin
  Result:=CreateAdclientsResource(Self);
end;


Function TAdsenseAPI.CreateAdclientsResource(AOwner : TComponent) : TAdclientsResource;

begin
  Result:=TAdclientsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetAdunitsCustomchannelsInstance : TAdunitsCustomchannelsResource;

begin
  if (FAdunitsCustomchannelsInstance=Nil) then
    FAdunitsCustomchannelsInstance:=CreateAdunitsCustomchannelsResource;
  Result:=FAdunitsCustomchannelsInstance;
end;

Function TAdsenseAPI.CreateAdunitsCustomchannelsResource : TAdunitsCustomchannelsResource;

begin
  Result:=CreateAdunitsCustomchannelsResource(Self);
end;


Function TAdsenseAPI.CreateAdunitsCustomchannelsResource(AOwner : TComponent) : TAdunitsCustomchannelsResource;

begin
  Result:=TAdunitsCustomchannelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetAdunitsInstance : TAdunitsResource;

begin
  if (FAdunitsInstance=Nil) then
    FAdunitsInstance:=CreateAdunitsResource;
  Result:=FAdunitsInstance;
end;

Function TAdsenseAPI.CreateAdunitsResource : TAdunitsResource;

begin
  Result:=CreateAdunitsResource(Self);
end;


Function TAdsenseAPI.CreateAdunitsResource(AOwner : TComponent) : TAdunitsResource;

begin
  Result:=TAdunitsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetAlertsInstance : TAlertsResource;

begin
  if (FAlertsInstance=Nil) then
    FAlertsInstance:=CreateAlertsResource;
  Result:=FAlertsInstance;
end;

Function TAdsenseAPI.CreateAlertsResource : TAlertsResource;

begin
  Result:=CreateAlertsResource(Self);
end;


Function TAdsenseAPI.CreateAlertsResource(AOwner : TComponent) : TAlertsResource;

begin
  Result:=TAlertsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetCustomchannelsAdunitsInstance : TCustomchannelsAdunitsResource;

begin
  if (FCustomchannelsAdunitsInstance=Nil) then
    FCustomchannelsAdunitsInstance:=CreateCustomchannelsAdunitsResource;
  Result:=FCustomchannelsAdunitsInstance;
end;

Function TAdsenseAPI.CreateCustomchannelsAdunitsResource : TCustomchannelsAdunitsResource;

begin
  Result:=CreateCustomchannelsAdunitsResource(Self);
end;


Function TAdsenseAPI.CreateCustomchannelsAdunitsResource(AOwner : TComponent) : TCustomchannelsAdunitsResource;

begin
  Result:=TCustomchannelsAdunitsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetCustomchannelsInstance : TCustomchannelsResource;

begin
  if (FCustomchannelsInstance=Nil) then
    FCustomchannelsInstance:=CreateCustomchannelsResource;
  Result:=FCustomchannelsInstance;
end;

Function TAdsenseAPI.CreateCustomchannelsResource : TCustomchannelsResource;

begin
  Result:=CreateCustomchannelsResource(Self);
end;


Function TAdsenseAPI.CreateCustomchannelsResource(AOwner : TComponent) : TCustomchannelsResource;

begin
  Result:=TCustomchannelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetMetadataDimensionsInstance : TMetadataDimensionsResource;

begin
  if (FMetadataDimensionsInstance=Nil) then
    FMetadataDimensionsInstance:=CreateMetadataDimensionsResource;
  Result:=FMetadataDimensionsInstance;
end;

Function TAdsenseAPI.CreateMetadataDimensionsResource : TMetadataDimensionsResource;

begin
  Result:=CreateMetadataDimensionsResource(Self);
end;


Function TAdsenseAPI.CreateMetadataDimensionsResource(AOwner : TComponent) : TMetadataDimensionsResource;

begin
  Result:=TMetadataDimensionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetMetadataMetricsInstance : TMetadataMetricsResource;

begin
  if (FMetadataMetricsInstance=Nil) then
    FMetadataMetricsInstance:=CreateMetadataMetricsResource;
  Result:=FMetadataMetricsInstance;
end;

Function TAdsenseAPI.CreateMetadataMetricsResource : TMetadataMetricsResource;

begin
  Result:=CreateMetadataMetricsResource(Self);
end;


Function TAdsenseAPI.CreateMetadataMetricsResource(AOwner : TComponent) : TMetadataMetricsResource;

begin
  Result:=TMetadataMetricsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetMetadataInstance : TMetadataResource;

begin
  if (FMetadataInstance=Nil) then
    FMetadataInstance:=CreateMetadataResource;
  Result:=FMetadataInstance;
end;

Function TAdsenseAPI.CreateMetadataResource : TMetadataResource;

begin
  Result:=CreateMetadataResource(Self);
end;


Function TAdsenseAPI.CreateMetadataResource(AOwner : TComponent) : TMetadataResource;

begin
  Result:=TMetadataResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetPaymentsInstance : TPaymentsResource;

begin
  if (FPaymentsInstance=Nil) then
    FPaymentsInstance:=CreatePaymentsResource;
  Result:=FPaymentsInstance;
end;

Function TAdsenseAPI.CreatePaymentsResource : TPaymentsResource;

begin
  Result:=CreatePaymentsResource(Self);
end;


Function TAdsenseAPI.CreatePaymentsResource(AOwner : TComponent) : TPaymentsResource;

begin
  Result:=TPaymentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetReportsSavedInstance : TReportsSavedResource;

begin
  if (FReportsSavedInstance=Nil) then
    FReportsSavedInstance:=CreateReportsSavedResource;
  Result:=FReportsSavedInstance;
end;

Function TAdsenseAPI.CreateReportsSavedResource : TReportsSavedResource;

begin
  Result:=CreateReportsSavedResource(Self);
end;


Function TAdsenseAPI.CreateReportsSavedResource(AOwner : TComponent) : TReportsSavedResource;

begin
  Result:=TReportsSavedResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetReportsInstance : TReportsResource;

begin
  if (FReportsInstance=Nil) then
    FReportsInstance:=CreateReportsResource;
  Result:=FReportsInstance;
end;

Function TAdsenseAPI.CreateReportsResource : TReportsResource;

begin
  Result:=CreateReportsResource(Self);
end;


Function TAdsenseAPI.CreateReportsResource(AOwner : TComponent) : TReportsResource;

begin
  Result:=TReportsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetSavedadstylesInstance : TSavedadstylesResource;

begin
  if (FSavedadstylesInstance=Nil) then
    FSavedadstylesInstance:=CreateSavedadstylesResource;
  Result:=FSavedadstylesInstance;
end;

Function TAdsenseAPI.CreateSavedadstylesResource : TSavedadstylesResource;

begin
  Result:=CreateSavedadstylesResource(Self);
end;


Function TAdsenseAPI.CreateSavedadstylesResource(AOwner : TComponent) : TSavedadstylesResource;

begin
  Result:=TSavedadstylesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAdsenseAPI.GetUrlchannelsInstance : TUrlchannelsResource;

begin
  if (FUrlchannelsInstance=Nil) then
    FUrlchannelsInstance:=CreateUrlchannelsResource;
  Result:=FUrlchannelsInstance;
end;

Function TAdsenseAPI.CreateUrlchannelsResource : TUrlchannelsResource;

begin
  Result:=CreateUrlchannelsResource(Self);
end;


Function TAdsenseAPI.CreateUrlchannelsResource(AOwner : TComponent) : TUrlchannelsResource;

begin
  Result:=TUrlchannelsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TAdsenseAPI.RegisterAPI;
end.
