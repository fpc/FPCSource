unit googleadsense;
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
  TAccount = class;
  TAccountArray = Array of TAccount;
  TAccountsubAccounts = class;
  TAccountsubAccountsArray = Array of TAccountsubAccounts;
  TAccounts = class;
  TAccountsArray = Array of TAccounts;
  TAccountsitems = class;
  TAccountsitemsArray = Array of TAccountsitems;
  TAdClient = class;
  TAdClientArray = Array of TAdClient;
  TAdClients = class;
  TAdClientsArray = Array of TAdClients;
  TAdClientsitems = class;
  TAdClientsitemsArray = Array of TAdClientsitems;
  TAdCode = class;
  TAdCodeArray = Array of TAdCode;
  TAdStyle = class;
  TAdStyleArray = Array of TAdStyle;
  TAdStylecolors = class;
  TAdStylecolorsArray = Array of TAdStylecolors;
  TAdStylefont = class;
  TAdStylefontArray = Array of TAdStylefont;
  TAdUnit = class;
  TAdUnitArray = Array of TAdUnit;
  TAdUnitcontentAdsSettings = class;
  TAdUnitcontentAdsSettingsArray = Array of TAdUnitcontentAdsSettings;
  TAdUnitcontentAdsSettingsbackupOption = class;
  TAdUnitcontentAdsSettingsbackupOptionArray = Array of TAdUnitcontentAdsSettingsbackupOption;
  TAdUnitfeedAdsSettings = class;
  TAdUnitfeedAdsSettingsArray = Array of TAdUnitfeedAdsSettings;
  TAdUnitmobileContentAdsSettings = class;
  TAdUnitmobileContentAdsSettingsArray = Array of TAdUnitmobileContentAdsSettings;
  TAdUnits = class;
  TAdUnitsArray = Array of TAdUnits;
  TAdUnitsitems = class;
  TAdUnitsitemsArray = Array of TAdUnitsitems;
  TAdsenseReportsGenerateResponse = class;
  TAdsenseReportsGenerateResponseArray = Array of TAdsenseReportsGenerateResponse;
  TAdsenseReportsGenerateResponseaverages = class;
  TAdsenseReportsGenerateResponseaveragesArray = Array of TAdsenseReportsGenerateResponseaverages;
  TAdsenseReportsGenerateResponseheaders = class;
  TAdsenseReportsGenerateResponseheadersArray = Array of TAdsenseReportsGenerateResponseheaders;
  TAdsenseReportsGenerateResponserows = class;
  TAdsenseReportsGenerateResponserowsArray = Array of TAdsenseReportsGenerateResponserows;
  TAdsenseReportsGenerateResponsetotals = class;
  TAdsenseReportsGenerateResponsetotalsArray = Array of TAdsenseReportsGenerateResponsetotals;
  TAdsenseReportsGenerateResponsewarnings = class;
  TAdsenseReportsGenerateResponsewarningsArray = Array of TAdsenseReportsGenerateResponsewarnings;
  TAlert = class;
  TAlertArray = Array of TAlert;
  TAlerts = class;
  TAlertsArray = Array of TAlerts;
  TAlertsitems = class;
  TAlertsitemsArray = Array of TAlertsitems;
  TCustomChannel = class;
  TCustomChannelArray = Array of TCustomChannel;
  TCustomChanneltargetingInfo = class;
  TCustomChanneltargetingInfoArray = Array of TCustomChanneltargetingInfo;
  TCustomChannels = class;
  TCustomChannelsArray = Array of TCustomChannels;
  TCustomChannelsitems = class;
  TCustomChannelsitemsArray = Array of TCustomChannelsitems;
  TMetadata = class;
  TMetadataArray = Array of TMetadata;
  TMetadataitems = class;
  TMetadataitemsArray = Array of TMetadataitems;
  TPayment = class;
  TPaymentArray = Array of TPayment;
  TPayments = class;
  TPaymentsArray = Array of TPayments;
  TPaymentsitems = class;
  TPaymentsitemsArray = Array of TPaymentsitems;
  TReportingMetadataEntry = class;
  TReportingMetadataEntryArray = Array of TReportingMetadataEntry;
  TReportingMetadataEntrycompatibleDimensions = class;
  TReportingMetadataEntrycompatibleDimensionsArray = Array of TReportingMetadataEntrycompatibleDimensions;
  TReportingMetadataEntrycompatibleMetrics = class;
  TReportingMetadataEntrycompatibleMetricsArray = Array of TReportingMetadataEntrycompatibleMetrics;
  TReportingMetadataEntryrequiredDimensions = class;
  TReportingMetadataEntryrequiredDimensionsArray = Array of TReportingMetadataEntryrequiredDimensions;
  TReportingMetadataEntryrequiredMetrics = class;
  TReportingMetadataEntryrequiredMetricsArray = Array of TReportingMetadataEntryrequiredMetrics;
  TReportingMetadataEntrysupportedProducts = class;
  TReportingMetadataEntrysupportedProductsArray = Array of TReportingMetadataEntrysupportedProducts;
  TSavedAdStyle = class;
  TSavedAdStyleArray = Array of TSavedAdStyle;
  TSavedAdStyles = class;
  TSavedAdStylesArray = Array of TSavedAdStyles;
  TSavedAdStylesitems = class;
  TSavedAdStylesitemsArray = Array of TSavedAdStylesitems;
  TSavedReport = class;
  TSavedReportArray = Array of TSavedReport;
  TSavedReports = class;
  TSavedReportsArray = Array of TSavedReports;
  TSavedReportsitems = class;
  TSavedReportsitemsArray = Array of TSavedReportsitems;
  TUrlChannel = class;
  TUrlChannelArray = Array of TUrlChannel;
  TUrlChannels = class;
  TUrlChannelsArray = Array of TUrlChannels;
  TUrlChannelsitems = class;
  TUrlChannelsitemsArray = Array of TUrlChannelsitems;
  
  { --------------------------------------------------------------------
    TAccount
    --------------------------------------------------------------------}
  
  TAccount = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
    Fname : string;
    Fpremium : boolean;
    FsubAccounts : TAccountsubAccounts;
    Ftimezone : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setpremium(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsubAccounts(AIndex : Integer; AValue : TAccountsubAccounts); virtual;
    Procedure Settimezone(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property name : string Index 16 Read Fname Write Setname;
    Property premium : boolean Index 24 Read Fpremium Write Setpremium;
    Property subAccounts : TAccountsubAccounts Index 32 Read FsubAccounts Write SetsubAccounts;
    Property timezone : string Index 40 Read Ftimezone Write Settimezone;
  end;
  TAccountClass = Class of TAccount;
  
  { --------------------------------------------------------------------
    TAccountsubAccounts
    --------------------------------------------------------------------}
  
  TAccountsubAccounts = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountsubAccountsClass = Class of TAccountsubAccounts;
  
  { --------------------------------------------------------------------
    TAccounts
    --------------------------------------------------------------------}
  
  TAccounts = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TAccountsitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TAccountsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TAccountsitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TAccountsClass = Class of TAccounts;
  
  { --------------------------------------------------------------------
    TAccountsitems
    --------------------------------------------------------------------}
  
  TAccountsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAccountsitemsClass = Class of TAccountsitems;
  
  { --------------------------------------------------------------------
    TAdClient
    --------------------------------------------------------------------}
  
  TAdClient = Class(TGoogleBaseObject)
  Private
    FarcOptIn : boolean;
    FarcReviewMode : string;
    Fid : string;
    Fkind : string;
    FproductCode : string;
    FsupportsReporting : boolean;
  Protected
    //Property setters
    Procedure SetarcOptIn(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetarcReviewMode(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductCode(AIndex : Integer; AValue : string); virtual;
    Procedure SetsupportsReporting(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property arcOptIn : boolean Index 0 Read FarcOptIn Write SetarcOptIn;
    Property arcReviewMode : string Index 8 Read FarcReviewMode Write SetarcReviewMode;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property productCode : string Index 32 Read FproductCode Write SetproductCode;
    Property supportsReporting : boolean Index 40 Read FsupportsReporting Write SetsupportsReporting;
  end;
  TAdClientClass = Class of TAdClient;
  
  { --------------------------------------------------------------------
    TAdClients
    --------------------------------------------------------------------}
  
  TAdClients = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TAdClientsitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TAdClientsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TAdClientsitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TAdClientsClass = Class of TAdClients;
  
  { --------------------------------------------------------------------
    TAdClientsitems
    --------------------------------------------------------------------}
  
  TAdClientsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAdClientsitemsClass = Class of TAdClientsitems;
  
  { --------------------------------------------------------------------
    TAdCode
    --------------------------------------------------------------------}
  
  TAdCode = Class(TGoogleBaseObject)
  Private
    FadCode : string;
    Fkind : string;
  Protected
    //Property setters
    Procedure SetadCode(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property adCode : string Index 0 Read FadCode Write SetadCode;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TAdCodeClass = Class of TAdCode;
  
  { --------------------------------------------------------------------
    TAdStyle
    --------------------------------------------------------------------}
  
  TAdStyle = Class(TGoogleBaseObject)
  Private
    Fcolors : TAdStylecolors;
    Fcorners : string;
    Ffont : TAdStylefont;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setcolors(AIndex : Integer; AValue : TAdStylecolors); virtual;
    Procedure Setcorners(AIndex : Integer; AValue : string); virtual;
    Procedure Setfont(AIndex : Integer; AValue : TAdStylefont); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property colors : TAdStylecolors Index 0 Read Fcolors Write Setcolors;
    Property corners : string Index 8 Read Fcorners Write Setcorners;
    Property font : TAdStylefont Index 16 Read Ffont Write Setfont;
    Property kind : string Index 24 Read Fkind Write Setkind;
  end;
  TAdStyleClass = Class of TAdStyle;
  
  { --------------------------------------------------------------------
    TAdStylecolors
    --------------------------------------------------------------------}
  
  TAdStylecolors = Class(TGoogleBaseObject)
  Private
    Fbackground : string;
    Fborder : string;
    Ftext : string;
    Ftitle : string;
    Furl : string;
  Protected
    //Property setters
    Procedure Setbackground(AIndex : Integer; AValue : string); virtual;
    Procedure Setborder(AIndex : Integer; AValue : string); virtual;
    Procedure Settext(AIndex : Integer; AValue : string); virtual;
    Procedure Settitle(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property background : string Index 0 Read Fbackground Write Setbackground;
    Property border : string Index 8 Read Fborder Write Setborder;
    Property text : string Index 16 Read Ftext Write Settext;
    Property title : string Index 24 Read Ftitle Write Settitle;
    Property url : string Index 32 Read Furl Write Seturl;
  end;
  TAdStylecolorsClass = Class of TAdStylecolors;
  
  { --------------------------------------------------------------------
    TAdStylefont
    --------------------------------------------------------------------}
  
  TAdStylefont = Class(TGoogleBaseObject)
  Private
    Ffamily : string;
    Fsize : string;
  Protected
    //Property setters
    Procedure Setfamily(AIndex : Integer; AValue : string); virtual;
    Procedure Setsize(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property family : string Index 0 Read Ffamily Write Setfamily;
    Property size : string Index 8 Read Fsize Write Setsize;
  end;
  TAdStylefontClass = Class of TAdStylefont;
  
  { --------------------------------------------------------------------
    TAdUnit
    --------------------------------------------------------------------}
  
  TAdUnit = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    FcontentAdsSettings : TAdUnitcontentAdsSettings;
    FcustomStyle : TAdStyle;
    FfeedAdsSettings : TAdUnitfeedAdsSettings;
    Fid : string;
    Fkind : string;
    FmobileContentAdsSettings : TAdUnitmobileContentAdsSettings;
    Fname : string;
    FsavedStyleId : string;
    Fstatus : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontentAdsSettings(AIndex : Integer; AValue : TAdUnitcontentAdsSettings); virtual;
    Procedure SetcustomStyle(AIndex : Integer; AValue : TAdStyle); virtual;
    Procedure SetfeedAdsSettings(AIndex : Integer; AValue : TAdUnitfeedAdsSettings); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmobileContentAdsSettings(AIndex : Integer; AValue : TAdUnitmobileContentAdsSettings); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetsavedStyleId(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property contentAdsSettings : TAdUnitcontentAdsSettings Index 8 Read FcontentAdsSettings Write SetcontentAdsSettings;
    Property customStyle : TAdStyle Index 16 Read FcustomStyle Write SetcustomStyle;
    Property feedAdsSettings : TAdUnitfeedAdsSettings Index 24 Read FfeedAdsSettings Write SetfeedAdsSettings;
    Property id : string Index 32 Read Fid Write Setid;
    Property kind : string Index 40 Read Fkind Write Setkind;
    Property mobileContentAdsSettings : TAdUnitmobileContentAdsSettings Index 48 Read FmobileContentAdsSettings Write SetmobileContentAdsSettings;
    Property name : string Index 56 Read Fname Write Setname;
    Property savedStyleId : string Index 64 Read FsavedStyleId Write SetsavedStyleId;
    Property status : string Index 72 Read Fstatus Write Setstatus;
  end;
  TAdUnitClass = Class of TAdUnit;
  
  { --------------------------------------------------------------------
    TAdUnitcontentAdsSettings
    --------------------------------------------------------------------}
  
  TAdUnitcontentAdsSettings = Class(TGoogleBaseObject)
  Private
    FbackupOption : TAdUnitcontentAdsSettingsbackupOption;
    Fsize : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetbackupOption(AIndex : Integer; AValue : TAdUnitcontentAdsSettingsbackupOption); virtual;
    Procedure Setsize(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property backupOption : TAdUnitcontentAdsSettingsbackupOption Index 0 Read FbackupOption Write SetbackupOption;
    Property size : string Index 8 Read Fsize Write Setsize;
    Property _type : string Index 16 Read F_type Write Set_type;
  end;
  TAdUnitcontentAdsSettingsClass = Class of TAdUnitcontentAdsSettings;
  
  { --------------------------------------------------------------------
    TAdUnitcontentAdsSettingsbackupOption
    --------------------------------------------------------------------}
  
  TAdUnitcontentAdsSettingsbackupOption = Class(TGoogleBaseObject)
  Private
    Fcolor : string;
    F_type : string;
    Furl : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcolor(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property color : string Index 0 Read Fcolor Write Setcolor;
    Property _type : string Index 8 Read F_type Write Set_type;
    Property url : string Index 16 Read Furl Write Seturl;
  end;
  TAdUnitcontentAdsSettingsbackupOptionClass = Class of TAdUnitcontentAdsSettingsbackupOption;
  
  { --------------------------------------------------------------------
    TAdUnitfeedAdsSettings
    --------------------------------------------------------------------}
  
  TAdUnitfeedAdsSettings = Class(TGoogleBaseObject)
  Private
    FadPosition : string;
    Ffrequency : integer;
    FminimumWordCount : integer;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetadPosition(AIndex : Integer; AValue : string); virtual;
    Procedure Setfrequency(AIndex : Integer; AValue : integer); virtual;
    Procedure SetminimumWordCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property adPosition : string Index 0 Read FadPosition Write SetadPosition;
    Property frequency : integer Index 8 Read Ffrequency Write Setfrequency;
    Property minimumWordCount : integer Index 16 Read FminimumWordCount Write SetminimumWordCount;
    Property _type : string Index 24 Read F_type Write Set_type;
  end;
  TAdUnitfeedAdsSettingsClass = Class of TAdUnitfeedAdsSettings;
  
  { --------------------------------------------------------------------
    TAdUnitmobileContentAdsSettings
    --------------------------------------------------------------------}
  
  TAdUnitmobileContentAdsSettings = Class(TGoogleBaseObject)
  Private
    FmarkupLanguage : string;
    FscriptingLanguage : string;
    Fsize : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetmarkupLanguage(AIndex : Integer; AValue : string); virtual;
    Procedure SetscriptingLanguage(AIndex : Integer; AValue : string); virtual;
    Procedure Setsize(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property markupLanguage : string Index 0 Read FmarkupLanguage Write SetmarkupLanguage;
    Property scriptingLanguage : string Index 8 Read FscriptingLanguage Write SetscriptingLanguage;
    Property size : string Index 16 Read Fsize Write Setsize;
    Property _type : string Index 24 Read F_type Write Set_type;
  end;
  TAdUnitmobileContentAdsSettingsClass = Class of TAdUnitmobileContentAdsSettings;
  
  { --------------------------------------------------------------------
    TAdUnits
    --------------------------------------------------------------------}
  
  TAdUnits = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TAdUnitsitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TAdUnitsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TAdUnitsitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TAdUnitsClass = Class of TAdUnits;
  
  { --------------------------------------------------------------------
    TAdUnitsitems
    --------------------------------------------------------------------}
  
  TAdUnitsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAdUnitsitemsClass = Class of TAdUnitsitems;
  
  { --------------------------------------------------------------------
    TAdsenseReportsGenerateResponse
    --------------------------------------------------------------------}
  
  TAdsenseReportsGenerateResponse = Class(TGoogleBaseObject)
  Private
    Faverages : TAdsenseReportsGenerateResponseaverages;
    FendDate : string;
    Fheaders : TAdsenseReportsGenerateResponseheaders;
    Fkind : string;
    Frows : TAdsenseReportsGenerateResponserows;
    FstartDate : string;
    FtotalMatchedRows : string;
    Ftotals : TAdsenseReportsGenerateResponsetotals;
    Fwarnings : TAdsenseReportsGenerateResponsewarnings;
  Protected
    //Property setters
    Procedure Setaverages(AIndex : Integer; AValue : TAdsenseReportsGenerateResponseaverages); virtual;
    Procedure SetendDate(AIndex : Integer; AValue : string); virtual;
    Procedure Setheaders(AIndex : Integer; AValue : TAdsenseReportsGenerateResponseheaders); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TAdsenseReportsGenerateResponserows); virtual;
    Procedure SetstartDate(AIndex : Integer; AValue : string); virtual;
    Procedure SettotalMatchedRows(AIndex : Integer; AValue : string); virtual;
    Procedure Settotals(AIndex : Integer; AValue : TAdsenseReportsGenerateResponsetotals); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TAdsenseReportsGenerateResponsewarnings); virtual;
  Public
  Published
    Property averages : TAdsenseReportsGenerateResponseaverages Index 0 Read Faverages Write Setaverages;
    Property endDate : string Index 8 Read FendDate Write SetendDate;
    Property headers : TAdsenseReportsGenerateResponseheaders Index 16 Read Fheaders Write Setheaders;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property rows : TAdsenseReportsGenerateResponserows Index 32 Read Frows Write Setrows;
    Property startDate : string Index 40 Read FstartDate Write SetstartDate;
    Property totalMatchedRows : string Index 48 Read FtotalMatchedRows Write SettotalMatchedRows;
    Property totals : TAdsenseReportsGenerateResponsetotals Index 56 Read Ftotals Write Settotals;
    Property warnings : TAdsenseReportsGenerateResponsewarnings Index 64 Read Fwarnings Write Setwarnings;
  end;
  TAdsenseReportsGenerateResponseClass = Class of TAdsenseReportsGenerateResponse;
  
  { --------------------------------------------------------------------
    TAdsenseReportsGenerateResponseaverages
    --------------------------------------------------------------------}
  
  TAdsenseReportsGenerateResponseaverages = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAdsenseReportsGenerateResponseaveragesClass = Class of TAdsenseReportsGenerateResponseaverages;
  
  { --------------------------------------------------------------------
    TAdsenseReportsGenerateResponseheaders
    --------------------------------------------------------------------}
  
  TAdsenseReportsGenerateResponseheaders = Class(TGoogleBaseObject)
  Private
    Fcurrency : string;
    Fname : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setcurrency(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property currency : string Index 0 Read Fcurrency Write Setcurrency;
    Property name : string Index 8 Read Fname Write Setname;
    Property _type : string Index 16 Read F_type Write Set_type;
  end;
  TAdsenseReportsGenerateResponseheadersClass = Class of TAdsenseReportsGenerateResponseheaders;
  
  { --------------------------------------------------------------------
    TAdsenseReportsGenerateResponserows
    --------------------------------------------------------------------}
  
  TAdsenseReportsGenerateResponserows = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAdsenseReportsGenerateResponserowsClass = Class of TAdsenseReportsGenerateResponserows;
  
  { --------------------------------------------------------------------
    TAdsenseReportsGenerateResponsetotals
    --------------------------------------------------------------------}
  
  TAdsenseReportsGenerateResponsetotals = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAdsenseReportsGenerateResponsetotalsClass = Class of TAdsenseReportsGenerateResponsetotals;
  
  { --------------------------------------------------------------------
    TAdsenseReportsGenerateResponsewarnings
    --------------------------------------------------------------------}
  
  TAdsenseReportsGenerateResponsewarnings = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAdsenseReportsGenerateResponsewarningsClass = Class of TAdsenseReportsGenerateResponsewarnings;
  
  { --------------------------------------------------------------------
    TAlert
    --------------------------------------------------------------------}
  
  TAlert = Class(TGoogleBaseObject)
  Private
    Fid : string;
    FisDismissible : boolean;
    Fkind : string;
    Fmessage : string;
    Fseverity : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetisDismissible(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
    Procedure Setseverity(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property isDismissible : boolean Index 8 Read FisDismissible Write SetisDismissible;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property message : string Index 24 Read Fmessage Write Setmessage;
    Property severity : string Index 32 Read Fseverity Write Setseverity;
    Property _type : string Index 40 Read F_type Write Set_type;
  end;
  TAlertClass = Class of TAlert;
  
  { --------------------------------------------------------------------
    TAlerts
    --------------------------------------------------------------------}
  
  TAlerts = Class(TGoogleBaseObject)
  Private
    Fitems : TAlertsitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAlertsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TAlertsitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TAlertsClass = Class of TAlerts;
  
  { --------------------------------------------------------------------
    TAlertsitems
    --------------------------------------------------------------------}
  
  TAlertsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAlertsitemsClass = Class of TAlertsitems;
  
  { --------------------------------------------------------------------
    TCustomChannel
    --------------------------------------------------------------------}
  
  TCustomChannel = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fid : string;
    Fkind : string;
    Fname : string;
    FtargetingInfo : TCustomChanneltargetingInfo;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SettargetingInfo(AIndex : Integer; AValue : TCustomChanneltargetingInfo); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property name : string Index 24 Read Fname Write Setname;
    Property targetingInfo : TCustomChanneltargetingInfo Index 32 Read FtargetingInfo Write SettargetingInfo;
  end;
  TCustomChannelClass = Class of TCustomChannel;
  
  { --------------------------------------------------------------------
    TCustomChanneltargetingInfo
    --------------------------------------------------------------------}
  
  TCustomChanneltargetingInfo = Class(TGoogleBaseObject)
  Private
    FadsAppearOn : string;
    Fdescription : string;
    Flocation : string;
    FsiteLanguage : string;
  Protected
    //Property setters
    Procedure SetadsAppearOn(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : string); virtual;
    Procedure SetsiteLanguage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property adsAppearOn : string Index 0 Read FadsAppearOn Write SetadsAppearOn;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property location : string Index 16 Read Flocation Write Setlocation;
    Property siteLanguage : string Index 24 Read FsiteLanguage Write SetsiteLanguage;
  end;
  TCustomChanneltargetingInfoClass = Class of TCustomChanneltargetingInfo;
  
  { --------------------------------------------------------------------
    TCustomChannels
    --------------------------------------------------------------------}
  
  TCustomChannels = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TCustomChannelsitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TCustomChannelsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TCustomChannelsitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TCustomChannelsClass = Class of TCustomChannels;
  
  { --------------------------------------------------------------------
    TCustomChannelsitems
    --------------------------------------------------------------------}
  
  TCustomChannelsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCustomChannelsitemsClass = Class of TCustomChannelsitems;
  
  { --------------------------------------------------------------------
    TMetadata
    --------------------------------------------------------------------}
  
  TMetadata = Class(TGoogleBaseObject)
  Private
    Fitems : TMetadataitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TMetadataitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TMetadataitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TMetadataClass = Class of TMetadata;
  
  { --------------------------------------------------------------------
    TMetadataitems
    --------------------------------------------------------------------}
  
  TMetadataitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMetadataitemsClass = Class of TMetadataitems;
  
  { --------------------------------------------------------------------
    TPayment
    --------------------------------------------------------------------}
  
  TPayment = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
    FpaymentAmount : string;
    FpaymentAmountCurrencyCode : string;
    FpaymentDate : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetpaymentAmount(AIndex : Integer; AValue : string); virtual;
    Procedure SetpaymentAmountCurrencyCode(AIndex : Integer; AValue : string); virtual;
    Procedure SetpaymentDate(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property paymentAmount : string Index 16 Read FpaymentAmount Write SetpaymentAmount;
    Property paymentAmountCurrencyCode : string Index 24 Read FpaymentAmountCurrencyCode Write SetpaymentAmountCurrencyCode;
    Property paymentDate : string Index 32 Read FpaymentDate Write SetpaymentDate;
  end;
  TPaymentClass = Class of TPayment;
  
  { --------------------------------------------------------------------
    TPayments
    --------------------------------------------------------------------}
  
  TPayments = Class(TGoogleBaseObject)
  Private
    Fitems : TPaymentsitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TPaymentsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TPaymentsitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TPaymentsClass = Class of TPayments;
  
  { --------------------------------------------------------------------
    TPaymentsitems
    --------------------------------------------------------------------}
  
  TPaymentsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPaymentsitemsClass = Class of TPaymentsitems;
  
  { --------------------------------------------------------------------
    TReportingMetadataEntry
    --------------------------------------------------------------------}
  
  TReportingMetadataEntry = Class(TGoogleBaseObject)
  Private
    FcompatibleDimensions : TReportingMetadataEntrycompatibleDimensions;
    FcompatibleMetrics : TReportingMetadataEntrycompatibleMetrics;
    Fid : string;
    Fkind : string;
    FrequiredDimensions : TReportingMetadataEntryrequiredDimensions;
    FrequiredMetrics : TReportingMetadataEntryrequiredMetrics;
    FsupportedProducts : TReportingMetadataEntrysupportedProducts;
  Protected
    //Property setters
    Procedure SetcompatibleDimensions(AIndex : Integer; AValue : TReportingMetadataEntrycompatibleDimensions); virtual;
    Procedure SetcompatibleMetrics(AIndex : Integer; AValue : TReportingMetadataEntrycompatibleMetrics); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetrequiredDimensions(AIndex : Integer; AValue : TReportingMetadataEntryrequiredDimensions); virtual;
    Procedure SetrequiredMetrics(AIndex : Integer; AValue : TReportingMetadataEntryrequiredMetrics); virtual;
    Procedure SetsupportedProducts(AIndex : Integer; AValue : TReportingMetadataEntrysupportedProducts); virtual;
  Public
  Published
    Property compatibleDimensions : TReportingMetadataEntrycompatibleDimensions Index 0 Read FcompatibleDimensions Write SetcompatibleDimensions;
    Property compatibleMetrics : TReportingMetadataEntrycompatibleMetrics Index 8 Read FcompatibleMetrics Write SetcompatibleMetrics;
    Property id : string Index 16 Read Fid Write Setid;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property requiredDimensions : TReportingMetadataEntryrequiredDimensions Index 32 Read FrequiredDimensions Write SetrequiredDimensions;
    Property requiredMetrics : TReportingMetadataEntryrequiredMetrics Index 40 Read FrequiredMetrics Write SetrequiredMetrics;
    Property supportedProducts : TReportingMetadataEntrysupportedProducts Index 48 Read FsupportedProducts Write SetsupportedProducts;
  end;
  TReportingMetadataEntryClass = Class of TReportingMetadataEntry;
  
  { --------------------------------------------------------------------
    TReportingMetadataEntrycompatibleDimensions
    --------------------------------------------------------------------}
  
  TReportingMetadataEntrycompatibleDimensions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReportingMetadataEntrycompatibleDimensionsClass = Class of TReportingMetadataEntrycompatibleDimensions;
  
  { --------------------------------------------------------------------
    TReportingMetadataEntrycompatibleMetrics
    --------------------------------------------------------------------}
  
  TReportingMetadataEntrycompatibleMetrics = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReportingMetadataEntrycompatibleMetricsClass = Class of TReportingMetadataEntrycompatibleMetrics;
  
  { --------------------------------------------------------------------
    TReportingMetadataEntryrequiredDimensions
    --------------------------------------------------------------------}
  
  TReportingMetadataEntryrequiredDimensions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReportingMetadataEntryrequiredDimensionsClass = Class of TReportingMetadataEntryrequiredDimensions;
  
  { --------------------------------------------------------------------
    TReportingMetadataEntryrequiredMetrics
    --------------------------------------------------------------------}
  
  TReportingMetadataEntryrequiredMetrics = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReportingMetadataEntryrequiredMetricsClass = Class of TReportingMetadataEntryrequiredMetrics;
  
  { --------------------------------------------------------------------
    TReportingMetadataEntrysupportedProducts
    --------------------------------------------------------------------}
  
  TReportingMetadataEntrysupportedProducts = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReportingMetadataEntrysupportedProductsClass = Class of TReportingMetadataEntrysupportedProducts;
  
  { --------------------------------------------------------------------
    TSavedAdStyle
    --------------------------------------------------------------------}
  
  TSavedAdStyle = Class(TGoogleBaseObject)
  Private
    FadStyle : TAdStyle;
    Fid : string;
    Fkind : string;
    Fname : string;
  Protected
    //Property setters
    Procedure SetadStyle(AIndex : Integer; AValue : TAdStyle); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property adStyle : TAdStyle Index 0 Read FadStyle Write SetadStyle;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property name : string Index 24 Read Fname Write Setname;
  end;
  TSavedAdStyleClass = Class of TSavedAdStyle;
  
  { --------------------------------------------------------------------
    TSavedAdStyles
    --------------------------------------------------------------------}
  
  TSavedAdStyles = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TSavedAdStylesitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TSavedAdStylesitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TSavedAdStylesitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TSavedAdStylesClass = Class of TSavedAdStyles;
  
  { --------------------------------------------------------------------
    TSavedAdStylesitems
    --------------------------------------------------------------------}
  
  TSavedAdStylesitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSavedAdStylesitemsClass = Class of TSavedAdStylesitems;
  
  { --------------------------------------------------------------------
    TSavedReport
    --------------------------------------------------------------------}
  
  TSavedReport = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
    Fname : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property name : string Index 16 Read Fname Write Setname;
  end;
  TSavedReportClass = Class of TSavedReport;
  
  { --------------------------------------------------------------------
    TSavedReports
    --------------------------------------------------------------------}
  
  TSavedReports = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TSavedReportsitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TSavedReportsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TSavedReportsitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TSavedReportsClass = Class of TSavedReports;
  
  { --------------------------------------------------------------------
    TSavedReportsitems
    --------------------------------------------------------------------}
  
  TSavedReportsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSavedReportsitemsClass = Class of TSavedReportsitems;
  
  { --------------------------------------------------------------------
    TUrlChannel
    --------------------------------------------------------------------}
  
  TUrlChannel = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
    FurlPattern : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SeturlPattern(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property urlPattern : string Index 16 Read FurlPattern Write SeturlPattern;
  end;
  TUrlChannelClass = Class of TUrlChannel;
  
  { --------------------------------------------------------------------
    TUrlChannels
    --------------------------------------------------------------------}
  
  TUrlChannels = Class(TGoogleBaseObject)
  Private
    Fetag : string;
    Fitems : TUrlChannelsitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TUrlChannelsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TUrlChannelsitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property nextPageToken : string Index 24 Read FnextPageToken Write SetnextPageToken;
  end;
  TUrlChannelsClass = Class of TUrlChannels;
  
  { --------------------------------------------------------------------
    TUrlChannelsitems
    --------------------------------------------------------------------}
  
  TUrlChannelsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUrlChannelsitemsClass = Class of TUrlChannelsitems;
  
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
    pageToken : string;
  end;
  
  TAccountsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; AQuery : string  = '') : TAccount;
    Function Get(accountId: string; AQuery : TAccountsgetOptions) : TAccount;
    Function List(AQuery : string  = '') : TAccounts;
    Function List(AQuery : TAccountslistOptions) : TAccounts;
  end;
  
  
  { --------------------------------------------------------------------
    TAdclientsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAdclientsResource, method List
  
  TAdclientsListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TAdclientsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TAdClients;
    Function List(AQuery : TAdclientslistOptions) : TAdClients;
  end;
  
  
  { --------------------------------------------------------------------
    TAdunitsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAdunitsResource, method List
  
  TAdunitsListOptions = Record
    includeInactive : boolean;
    maxResults : integer;
    pageToken : string;
  end;
  
  TAdunitsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(adClientId: string; adUnitId: string) : TAdUnit;
    Function GetAdCode(adClientId: string; adUnitId: string) : TAdCode;
    Function List(adClientId: string; AQuery : string  = '') : TAdUnits;
    Function List(adClientId: string; AQuery : TAdunitslistOptions) : TAdUnits;
  end;
  
  
  { --------------------------------------------------------------------
    TAlertsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAlertsResource, method List
  
  TAlertsListOptions = Record
    locale : string;
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
    TCustomchannelsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCustomchannelsResource, method List
  
  TCustomchannelsListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TCustomchannelsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(adClientId: string; customChannelId: string) : TCustomChannel;
    Function List(adClientId: string; AQuery : string  = '') : TCustomChannels;
    Function List(adClientId: string; AQuery : TCustomchannelslistOptions) : TCustomChannels;
  end;
  
  
  { --------------------------------------------------------------------
    TMetadataResource
    --------------------------------------------------------------------}
  
  TMetadataResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
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
    TReportsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TReportsResource, method Generate
  
  TReportsGenerateOptions = Record
    accountId : string;
    currency : string;
    dimension : string;
    endDate : string;
    filter : string;
    locale : string;
    maxResults : integer;
    metric : string;
    sort : string;
    startDate : string;
    startIndex : integer;
    useTimezoneReporting : boolean;
  end;
  
  TReportsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Generate(AQuery : string  = '') : TAdsenseReportsGenerateResponse;
    Function Generate(AQuery : TReportsgenerateOptions) : TAdsenseReportsGenerateResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TSavedadstylesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TSavedadstylesResource, method List
  
  TSavedadstylesListOptions = Record
    maxResults : integer;
    pageToken : string;
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
    pageToken : string;
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
    FAccountsInstance : TAccountsResource;
    FAdclientsInstance : TAdclientsResource;
    FAdunitsInstance : TAdunitsResource;
    FAlertsInstance : TAlertsResource;
    FCustomchannelsInstance : TCustomchannelsResource;
    FMetadataInstance : TMetadataResource;
    FPaymentsInstance : TPaymentsResource;
    FReportsInstance : TReportsResource;
    FSavedadstylesInstance : TSavedadstylesResource;
    FUrlchannelsInstance : TUrlchannelsResource;
    Function GetAccountsInstance : TAccountsResource;virtual;
    Function GetAdclientsInstance : TAdclientsResource;virtual;
    Function GetAdunitsInstance : TAdunitsResource;virtual;
    Function GetAlertsInstance : TAlertsResource;virtual;
    Function GetCustomchannelsInstance : TCustomchannelsResource;virtual;
    Function GetMetadataInstance : TMetadataResource;virtual;
    Function GetPaymentsInstance : TPaymentsResource;virtual;
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
    Function CreateAccountsResource(AOwner : TComponent) : TAccountsResource;virtual;overload;
    Function CreateAccountsResource : TAccountsResource;virtual;overload;
    Function CreateAdclientsResource(AOwner : TComponent) : TAdclientsResource;virtual;overload;
    Function CreateAdclientsResource : TAdclientsResource;virtual;overload;
    Function CreateAdunitsResource(AOwner : TComponent) : TAdunitsResource;virtual;overload;
    Function CreateAdunitsResource : TAdunitsResource;virtual;overload;
    Function CreateAlertsResource(AOwner : TComponent) : TAlertsResource;virtual;overload;
    Function CreateAlertsResource : TAlertsResource;virtual;overload;
    Function CreateCustomchannelsResource(AOwner : TComponent) : TCustomchannelsResource;virtual;overload;
    Function CreateCustomchannelsResource : TCustomchannelsResource;virtual;overload;
    Function CreateMetadataResource(AOwner : TComponent) : TMetadataResource;virtual;overload;
    Function CreateMetadataResource : TMetadataResource;virtual;overload;
    Function CreatePaymentsResource(AOwner : TComponent) : TPaymentsResource;virtual;overload;
    Function CreatePaymentsResource : TPaymentsResource;virtual;overload;
    Function CreateReportsResource(AOwner : TComponent) : TReportsResource;virtual;overload;
    Function CreateReportsResource : TReportsResource;virtual;overload;
    Function CreateSavedadstylesResource(AOwner : TComponent) : TSavedadstylesResource;virtual;overload;
    Function CreateSavedadstylesResource : TSavedadstylesResource;virtual;overload;
    Function CreateUrlchannelsResource(AOwner : TComponent) : TUrlchannelsResource;virtual;overload;
    Function CreateUrlchannelsResource : TUrlchannelsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AccountsResource : TAccountsResource Read GetAccountsInstance;
    Property AdclientsResource : TAdclientsResource Read GetAdclientsInstance;
    Property AdunitsResource : TAdunitsResource Read GetAdunitsInstance;
    Property AlertsResource : TAlertsResource Read GetAlertsInstance;
    Property CustomchannelsResource : TCustomchannelsResource Read GetCustomchannelsInstance;
    Property MetadataResource : TMetadataResource Read GetMetadataInstance;
    Property PaymentsResource : TPaymentsResource Read GetPaymentsInstance;
    Property ReportsResource : TReportsResource Read GetReportsInstance;
    Property SavedadstylesResource : TSavedadstylesResource Read GetSavedadstylesInstance;
    Property UrlchannelsResource : TUrlchannelsResource Read GetUrlchannelsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAccount
  --------------------------------------------------------------------}


Procedure TAccount.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Setname(AIndex : Integer; AValue : string); 

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



Procedure TAccount.SetsubAccounts(AIndex : Integer; AValue : TAccountsubAccounts); 

begin
  If (FsubAccounts=AValue) then exit;
  FsubAccounts:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccount.Settimezone(AIndex : Integer; AValue : string); 

begin
  If (Ftimezone=AValue) then exit;
  Ftimezone:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsubAccounts
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAccounts
  --------------------------------------------------------------------}


Procedure TAccounts.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounts.Setitems(AIndex : Integer; AValue : TAccountsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounts.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAccounts.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAccountsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAdClient
  --------------------------------------------------------------------}


Procedure TAdClient.SetarcOptIn(AIndex : Integer; AValue : boolean); 

begin
  If (FarcOptIn=AValue) then exit;
  FarcOptIn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdClient.SetarcReviewMode(AIndex : Integer; AValue : string); 

begin
  If (FarcReviewMode=AValue) then exit;
  FarcReviewMode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdClient.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdClient.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdClient.SetproductCode(AIndex : Integer; AValue : string); 

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


Procedure TAdClients.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdClients.Setitems(AIndex : Integer; AValue : TAdClientsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdClients.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdClients.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdClientsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAdCode
  --------------------------------------------------------------------}


Procedure TAdCode.SetadCode(AIndex : Integer; AValue : string); 

begin
  If (FadCode=AValue) then exit;
  FadCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdCode.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdStyle
  --------------------------------------------------------------------}


Procedure TAdStyle.Setcolors(AIndex : Integer; AValue : TAdStylecolors); 

begin
  If (Fcolors=AValue) then exit;
  Fcolors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdStyle.Setcorners(AIndex : Integer; AValue : string); 

begin
  If (Fcorners=AValue) then exit;
  Fcorners:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdStyle.Setfont(AIndex : Integer; AValue : TAdStylefont); 

begin
  If (Ffont=AValue) then exit;
  Ffont:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdStyle.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdStylecolors
  --------------------------------------------------------------------}


Procedure TAdStylecolors.Setbackground(AIndex : Integer; AValue : string); 

begin
  If (Fbackground=AValue) then exit;
  Fbackground:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdStylecolors.Setborder(AIndex : Integer; AValue : string); 

begin
  If (Fborder=AValue) then exit;
  Fborder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdStylecolors.Settext(AIndex : Integer; AValue : string); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdStylecolors.Settitle(AIndex : Integer; AValue : string); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdStylecolors.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdStylefont
  --------------------------------------------------------------------}


Procedure TAdStylefont.Setfamily(AIndex : Integer; AValue : string); 

begin
  If (Ffamily=AValue) then exit;
  Ffamily:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdStylefont.Setsize(AIndex : Integer; AValue : string); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdUnit
  --------------------------------------------------------------------}


Procedure TAdUnit.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnit.SetcontentAdsSettings(AIndex : Integer; AValue : TAdUnitcontentAdsSettings); 

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



Procedure TAdUnit.SetfeedAdsSettings(AIndex : Integer; AValue : TAdUnitfeedAdsSettings); 

begin
  If (FfeedAdsSettings=AValue) then exit;
  FfeedAdsSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnit.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnit.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnit.SetmobileContentAdsSettings(AIndex : Integer; AValue : TAdUnitmobileContentAdsSettings); 

begin
  If (FmobileContentAdsSettings=AValue) then exit;
  FmobileContentAdsSettings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnit.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnit.SetsavedStyleId(AIndex : Integer; AValue : string); 

begin
  If (FsavedStyleId=AValue) then exit;
  FsavedStyleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnit.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdUnitcontentAdsSettings
  --------------------------------------------------------------------}


Procedure TAdUnitcontentAdsSettings.SetbackupOption(AIndex : Integer; AValue : TAdUnitcontentAdsSettingsbackupOption); 

begin
  If (FbackupOption=AValue) then exit;
  FbackupOption:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitcontentAdsSettings.Setsize(AIndex : Integer; AValue : string); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitcontentAdsSettings.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAdUnitcontentAdsSettings.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAdUnitcontentAdsSettingsbackupOption
  --------------------------------------------------------------------}


Procedure TAdUnitcontentAdsSettingsbackupOption.Setcolor(AIndex : Integer; AValue : string); 

begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitcontentAdsSettingsbackupOption.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitcontentAdsSettingsbackupOption.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAdUnitcontentAdsSettingsbackupOption.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAdUnitfeedAdsSettings
  --------------------------------------------------------------------}


Procedure TAdUnitfeedAdsSettings.SetadPosition(AIndex : Integer; AValue : string); 

begin
  If (FadPosition=AValue) then exit;
  FadPosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitfeedAdsSettings.Setfrequency(AIndex : Integer; AValue : integer); 

begin
  If (Ffrequency=AValue) then exit;
  Ffrequency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitfeedAdsSettings.SetminimumWordCount(AIndex : Integer; AValue : integer); 

begin
  If (FminimumWordCount=AValue) then exit;
  FminimumWordCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitfeedAdsSettings.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAdUnitfeedAdsSettings.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAdUnitmobileContentAdsSettings
  --------------------------------------------------------------------}


Procedure TAdUnitmobileContentAdsSettings.SetmarkupLanguage(AIndex : Integer; AValue : string); 

begin
  If (FmarkupLanguage=AValue) then exit;
  FmarkupLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitmobileContentAdsSettings.SetscriptingLanguage(AIndex : Integer; AValue : string); 

begin
  If (FscriptingLanguage=AValue) then exit;
  FscriptingLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitmobileContentAdsSettings.Setsize(AIndex : Integer; AValue : string); 

begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnitmobileContentAdsSettings.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAdUnitmobileContentAdsSettings.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAdUnits
  --------------------------------------------------------------------}


Procedure TAdUnits.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnits.Setitems(AIndex : Integer; AValue : TAdUnitsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnits.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdUnits.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdUnitsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAdsenseReportsGenerateResponse
  --------------------------------------------------------------------}


Procedure TAdsenseReportsGenerateResponse.Setaverages(AIndex : Integer; AValue : TAdsenseReportsGenerateResponseaverages); 

begin
  If (Faverages=AValue) then exit;
  Faverages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponse.SetendDate(AIndex : Integer; AValue : string); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponse.Setheaders(AIndex : Integer; AValue : TAdsenseReportsGenerateResponseheaders); 

begin
  If (Fheaders=AValue) then exit;
  Fheaders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponse.Setrows(AIndex : Integer; AValue : TAdsenseReportsGenerateResponserows); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponse.SetstartDate(AIndex : Integer; AValue : string); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponse.SettotalMatchedRows(AIndex : Integer; AValue : string); 

begin
  If (FtotalMatchedRows=AValue) then exit;
  FtotalMatchedRows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponse.Settotals(AIndex : Integer; AValue : TAdsenseReportsGenerateResponsetotals); 

begin
  If (Ftotals=AValue) then exit;
  Ftotals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponse.Setwarnings(AIndex : Integer; AValue : TAdsenseReportsGenerateResponsewarnings); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdsenseReportsGenerateResponseaverages
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAdsenseReportsGenerateResponseheaders
  --------------------------------------------------------------------}


Procedure TAdsenseReportsGenerateResponseheaders.Setcurrency(AIndex : Integer; AValue : string); 

begin
  If (Fcurrency=AValue) then exit;
  Fcurrency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponseheaders.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdsenseReportsGenerateResponseheaders.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAdsenseReportsGenerateResponseheaders.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAdsenseReportsGenerateResponserows
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAdsenseReportsGenerateResponsetotals
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAdsenseReportsGenerateResponsewarnings
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAlert
  --------------------------------------------------------------------}


Procedure TAlert.Setid(AIndex : Integer; AValue : string); 

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



Procedure TAlert.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAlert.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAlert.Setseverity(AIndex : Integer; AValue : string); 

begin
  If (Fseverity=AValue) then exit;
  Fseverity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAlert.Set_type(AIndex : Integer; AValue : string); 

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


Procedure TAlerts.Setitems(AIndex : Integer; AValue : TAlertsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAlerts.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAlertsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCustomChannel
  --------------------------------------------------------------------}


Procedure TCustomChannel.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannel.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannel.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannel.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannel.SettargetingInfo(AIndex : Integer; AValue : TCustomChanneltargetingInfo); 

begin
  If (FtargetingInfo=AValue) then exit;
  FtargetingInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomChanneltargetingInfo
  --------------------------------------------------------------------}


Procedure TCustomChanneltargetingInfo.SetadsAppearOn(AIndex : Integer; AValue : string); 

begin
  If (FadsAppearOn=AValue) then exit;
  FadsAppearOn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChanneltargetingInfo.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChanneltargetingInfo.Setlocation(AIndex : Integer; AValue : string); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChanneltargetingInfo.SetsiteLanguage(AIndex : Integer; AValue : string); 

begin
  If (FsiteLanguage=AValue) then exit;
  FsiteLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomChannels
  --------------------------------------------------------------------}


Procedure TCustomChannels.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannels.Setitems(AIndex : Integer; AValue : TCustomChannelsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannels.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCustomChannels.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCustomChannelsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMetadata
  --------------------------------------------------------------------}


Procedure TMetadata.Setitems(AIndex : Integer; AValue : TMetadataitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMetadataitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPayment
  --------------------------------------------------------------------}


Procedure TPayment.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPayment.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPayment.SetpaymentAmount(AIndex : Integer; AValue : string); 

begin
  If (FpaymentAmount=AValue) then exit;
  FpaymentAmount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPayment.SetpaymentAmountCurrencyCode(AIndex : Integer; AValue : string); 

begin
  If (FpaymentAmountCurrencyCode=AValue) then exit;
  FpaymentAmountCurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPayment.SetpaymentDate(AIndex : Integer; AValue : string); 

begin
  If (FpaymentDate=AValue) then exit;
  FpaymentDate:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPayments
  --------------------------------------------------------------------}


Procedure TPayments.Setitems(AIndex : Integer; AValue : TPaymentsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPayments.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPaymentsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReportingMetadataEntry
  --------------------------------------------------------------------}


Procedure TReportingMetadataEntry.SetcompatibleDimensions(AIndex : Integer; AValue : TReportingMetadataEntrycompatibleDimensions); 

begin
  If (FcompatibleDimensions=AValue) then exit;
  FcompatibleDimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportingMetadataEntry.SetcompatibleMetrics(AIndex : Integer; AValue : TReportingMetadataEntrycompatibleMetrics); 

begin
  If (FcompatibleMetrics=AValue) then exit;
  FcompatibleMetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportingMetadataEntry.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportingMetadataEntry.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportingMetadataEntry.SetrequiredDimensions(AIndex : Integer; AValue : TReportingMetadataEntryrequiredDimensions); 

begin
  If (FrequiredDimensions=AValue) then exit;
  FrequiredDimensions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportingMetadataEntry.SetrequiredMetrics(AIndex : Integer; AValue : TReportingMetadataEntryrequiredMetrics); 

begin
  If (FrequiredMetrics=AValue) then exit;
  FrequiredMetrics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportingMetadataEntry.SetsupportedProducts(AIndex : Integer; AValue : TReportingMetadataEntrysupportedProducts); 

begin
  If (FsupportedProducts=AValue) then exit;
  FsupportedProducts:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportingMetadataEntrycompatibleDimensions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReportingMetadataEntrycompatibleMetrics
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReportingMetadataEntryrequiredDimensions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReportingMetadataEntryrequiredMetrics
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReportingMetadataEntrysupportedProducts
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSavedAdStyle
  --------------------------------------------------------------------}


Procedure TSavedAdStyle.SetadStyle(AIndex : Integer; AValue : TAdStyle); 

begin
  If (FadStyle=AValue) then exit;
  FadStyle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedAdStyle.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedAdStyle.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedAdStyle.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSavedAdStyles
  --------------------------------------------------------------------}


Procedure TSavedAdStyles.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedAdStyles.Setitems(AIndex : Integer; AValue : TSavedAdStylesitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedAdStyles.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedAdStyles.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSavedAdStylesitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSavedReport
  --------------------------------------------------------------------}


Procedure TSavedReport.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedReport.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedReport.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSavedReports
  --------------------------------------------------------------------}


Procedure TSavedReports.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedReports.Setitems(AIndex : Integer; AValue : TSavedReportsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedReports.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSavedReports.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSavedReportsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TUrlChannel
  --------------------------------------------------------------------}


Procedure TUrlChannel.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlChannel.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlChannel.SeturlPattern(AIndex : Integer; AValue : string); 

begin
  If (FurlPattern=AValue) then exit;
  FurlPattern:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlChannels
  --------------------------------------------------------------------}


Procedure TUrlChannels.Setetag(AIndex : Integer; AValue : string); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlChannels.Setitems(AIndex : Integer; AValue : TUrlChannelsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlChannels.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrlChannels.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrlChannelsitems
  --------------------------------------------------------------------}




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
  Result:='20150303';
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
  Result:='https://www.googleapis.com/';
end;

Class Function TAdsenseAPI.APIbasePath : string;

begin
  Result:='/adsense/v1.4/';
end;

Class Function TAdsenseAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/adsense/v1.4/';
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
  TAccountsubAccounts.RegisterObject;
  TAccounts.RegisterObject;
  TAccountsitems.RegisterObject;
  TAdClient.RegisterObject;
  TAdClients.RegisterObject;
  TAdClientsitems.RegisterObject;
  TAdCode.RegisterObject;
  TAdStyle.RegisterObject;
  TAdStylecolors.RegisterObject;
  TAdStylefont.RegisterObject;
  TAdUnit.RegisterObject;
  TAdUnitcontentAdsSettings.RegisterObject;
  TAdUnitcontentAdsSettingsbackupOption.RegisterObject;
  TAdUnitfeedAdsSettings.RegisterObject;
  TAdUnitmobileContentAdsSettings.RegisterObject;
  TAdUnits.RegisterObject;
  TAdUnitsitems.RegisterObject;
  TAdsenseReportsGenerateResponse.RegisterObject;
  TAdsenseReportsGenerateResponseaverages.RegisterObject;
  TAdsenseReportsGenerateResponseheaders.RegisterObject;
  TAdsenseReportsGenerateResponserows.RegisterObject;
  TAdsenseReportsGenerateResponsetotals.RegisterObject;
  TAdsenseReportsGenerateResponsewarnings.RegisterObject;
  TAlert.RegisterObject;
  TAlerts.RegisterObject;
  TAlertsitems.RegisterObject;
  TCustomChannel.RegisterObject;
  TCustomChanneltargetingInfo.RegisterObject;
  TCustomChannels.RegisterObject;
  TCustomChannelsitems.RegisterObject;
  TMetadata.RegisterObject;
  TMetadataitems.RegisterObject;
  TPayment.RegisterObject;
  TPayments.RegisterObject;
  TPaymentsitems.RegisterObject;
  TReportingMetadataEntry.RegisterObject;
  TReportingMetadataEntrycompatibleDimensions.RegisterObject;
  TReportingMetadataEntrycompatibleMetrics.RegisterObject;
  TReportingMetadataEntryrequiredDimensions.RegisterObject;
  TReportingMetadataEntryrequiredMetrics.RegisterObject;
  TReportingMetadataEntrysupportedProducts.RegisterObject;
  TSavedAdStyle.RegisterObject;
  TSavedAdStyles.RegisterObject;
  TSavedAdStylesitems.RegisterObject;
  TSavedReport.RegisterObject;
  TSavedReports.RegisterObject;
  TSavedReportsitems.RegisterObject;
  TUrlChannel.RegisterObject;
  TUrlChannels.RegisterObject;
  TUrlChannelsitems.RegisterObject;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
end;



initialization
  TAdsenseAPI.RegisterAPI;
end.
