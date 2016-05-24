unit googleadexchangeseller;
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
  TPreferredDeal = class;
  TPreferredDealArray = Array of TPreferredDeal;
  TPreferredDeals = class;
  TPreferredDealsArray = Array of TPreferredDeals;
  TPreferredDealsitems = class;
  TPreferredDealsitemsArray = Array of TPreferredDealsitems;
  TReport = class;
  TReportArray = Array of TReport;
  TReportaverages = class;
  TReportaveragesArray = Array of TReportaverages;
  TReportheaders = class;
  TReportheadersArray = Array of TReportheaders;
  TReportrows = class;
  TReportrowsArray = Array of TReportrows;
  TReporttotals = class;
  TReporttotalsArray = Array of TReporttotals;
  TReportwarnings = class;
  TReportwarningsArray = Array of TReportwarnings;
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
  TAccountClass = Class of TAccount;
  
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
    Fid : string;
    Fkind : string;
    FproductCode : string;
    FsupportsReporting : boolean;
  Protected
    //Property setters
    Procedure SetarcOptIn(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductCode(AIndex : Integer; AValue : string); virtual;
    Procedure SetsupportsReporting(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property arcOptIn : boolean Index 0 Read FarcOptIn Write SetarcOptIn;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property productCode : string Index 24 Read FproductCode Write SetproductCode;
    Property supportsReporting : boolean Index 32 Read FsupportsReporting Write SetsupportsReporting;
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
    TAlert
    --------------------------------------------------------------------}
  
  TAlert = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
    Fmessage : string;
    Fseverity : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
    Procedure Setseverity(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property message : string Index 16 Read Fmessage Write Setmessage;
    Property severity : string Index 24 Read Fseverity Write Setseverity;
    Property _type : string Index 32 Read F_type Write Set_type;
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
    TPreferredDeal
    --------------------------------------------------------------------}
  
  TPreferredDeal = Class(TGoogleBaseObject)
  Private
    FadvertiserName : string;
    FbuyerNetworkName : string;
    FcurrencyCode : string;
    FendTime : string;
    FfixedCpm : string;
    Fid : string;
    Fkind : string;
    FstartTime : string;
  Protected
    //Property setters
    Procedure SetadvertiserName(AIndex : Integer; AValue : string); virtual;
    Procedure SetbuyerNetworkName(AIndex : Integer; AValue : string); virtual;
    Procedure SetcurrencyCode(AIndex : Integer; AValue : string); virtual;
    Procedure SetendTime(AIndex : Integer; AValue : string); virtual;
    Procedure SetfixedCpm(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetstartTime(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property advertiserName : string Index 0 Read FadvertiserName Write SetadvertiserName;
    Property buyerNetworkName : string Index 8 Read FbuyerNetworkName Write SetbuyerNetworkName;
    Property currencyCode : string Index 16 Read FcurrencyCode Write SetcurrencyCode;
    Property endTime : string Index 24 Read FendTime Write SetendTime;
    Property fixedCpm : string Index 32 Read FfixedCpm Write SetfixedCpm;
    Property id : string Index 40 Read Fid Write Setid;
    Property kind : string Index 48 Read Fkind Write Setkind;
    Property startTime : string Index 56 Read FstartTime Write SetstartTime;
  end;
  TPreferredDealClass = Class of TPreferredDeal;
  
  { --------------------------------------------------------------------
    TPreferredDeals
    --------------------------------------------------------------------}
  
  TPreferredDeals = Class(TGoogleBaseObject)
  Private
    Fitems : TPreferredDealsitems;
    Fkind : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TPreferredDealsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TPreferredDealsitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
  end;
  TPreferredDealsClass = Class of TPreferredDeals;
  
  { --------------------------------------------------------------------
    TPreferredDealsitems
    --------------------------------------------------------------------}
  
  TPreferredDealsitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPreferredDealsitemsClass = Class of TPreferredDealsitems;
  
  { --------------------------------------------------------------------
    TReport
    --------------------------------------------------------------------}
  
  TReport = Class(TGoogleBaseObject)
  Private
    Faverages : TReportaverages;
    Fheaders : TReportheaders;
    Fkind : string;
    Frows : TReportrows;
    FtotalMatchedRows : string;
    Ftotals : TReporttotals;
    Fwarnings : TReportwarnings;
  Protected
    //Property setters
    Procedure Setaverages(AIndex : Integer; AValue : TReportaverages); virtual;
    Procedure Setheaders(AIndex : Integer; AValue : TReportheaders); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setrows(AIndex : Integer; AValue : TReportrows); virtual;
    Procedure SettotalMatchedRows(AIndex : Integer; AValue : string); virtual;
    Procedure Settotals(AIndex : Integer; AValue : TReporttotals); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TReportwarnings); virtual;
  Public
  Published
    Property averages : TReportaverages Index 0 Read Faverages Write Setaverages;
    Property headers : TReportheaders Index 8 Read Fheaders Write Setheaders;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property rows : TReportrows Index 24 Read Frows Write Setrows;
    Property totalMatchedRows : string Index 32 Read FtotalMatchedRows Write SettotalMatchedRows;
    Property totals : TReporttotals Index 40 Read Ftotals Write Settotals;
    Property warnings : TReportwarnings Index 48 Read Fwarnings Write Setwarnings;
  end;
  TReportClass = Class of TReport;
  
  { --------------------------------------------------------------------
    TReportaverages
    --------------------------------------------------------------------}
  
  TReportaverages = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReportaveragesClass = Class of TReportaverages;
  
  { --------------------------------------------------------------------
    TReportheaders
    --------------------------------------------------------------------}
  
  TReportheaders = Class(TGoogleBaseObject)
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
  TReportheadersClass = Class of TReportheaders;
  
  { --------------------------------------------------------------------
    TReportrows
    --------------------------------------------------------------------}
  
  TReportrows = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReportrowsClass = Class of TReportrows;
  
  { --------------------------------------------------------------------
    TReporttotals
    --------------------------------------------------------------------}
  
  TReporttotals = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReporttotalsClass = Class of TReporttotals;
  
  { --------------------------------------------------------------------
    TReportwarnings
    --------------------------------------------------------------------}
  
  TReportwarnings = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReportwarningsClass = Class of TReportwarnings;
  
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
  
  
  //Optional query Options for TAccountsResource, method List
  
  TAccountsListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TAccountsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string) : TAccount;
    Function List(AQuery : string  = '') : TAccounts;
    Function List(AQuery : TAccountslistOptions) : TAccounts;
  end;
  
  
  { --------------------------------------------------------------------
    TAdexchangesellerAPI
    --------------------------------------------------------------------}
  
  TAdexchangesellerAPI = Class(TGoogleAPI)
  Private
    FAccountsInstance : TAccountsResource;
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
    Function CreateAccountsResource(AOwner : TComponent) : TAccountsResource;virtual;overload;
    Function CreateAccountsResource : TAccountsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AccountsResource : TAccountsResource Read GetAccountsInstance;
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
  TAlert
  --------------------------------------------------------------------}


Procedure TAlert.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
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
  TPreferredDeal
  --------------------------------------------------------------------}


Procedure TPreferredDeal.SetadvertiserName(AIndex : Integer; AValue : string); 

begin
  If (FadvertiserName=AValue) then exit;
  FadvertiserName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPreferredDeal.SetbuyerNetworkName(AIndex : Integer; AValue : string); 

begin
  If (FbuyerNetworkName=AValue) then exit;
  FbuyerNetworkName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPreferredDeal.SetcurrencyCode(AIndex : Integer; AValue : string); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPreferredDeal.SetendTime(AIndex : Integer; AValue : string); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPreferredDeal.SetfixedCpm(AIndex : Integer; AValue : string); 

begin
  If (FfixedCpm=AValue) then exit;
  FfixedCpm:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPreferredDeal.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPreferredDeal.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPreferredDeal.SetstartTime(AIndex : Integer; AValue : string); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPreferredDeals
  --------------------------------------------------------------------}


Procedure TPreferredDeals.Setitems(AIndex : Integer; AValue : TPreferredDealsitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPreferredDeals.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPreferredDealsitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReport
  --------------------------------------------------------------------}


Procedure TReport.Setaverages(AIndex : Integer; AValue : TReportaverages); 

begin
  If (Faverages=AValue) then exit;
  Faverages:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setheaders(AIndex : Integer; AValue : TReportheaders); 

begin
  If (Fheaders=AValue) then exit;
  Fheaders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setrows(AIndex : Integer; AValue : TReportrows); 

begin
  If (Frows=AValue) then exit;
  Frows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.SettotalMatchedRows(AIndex : Integer; AValue : string); 

begin
  If (FtotalMatchedRows=AValue) then exit;
  FtotalMatchedRows:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Settotals(AIndex : Integer; AValue : TReporttotals); 

begin
  If (Ftotals=AValue) then exit;
  Ftotals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReport.Setwarnings(AIndex : Integer; AValue : TReportwarnings); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReportaverages
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReportheaders
  --------------------------------------------------------------------}


Procedure TReportheaders.Setcurrency(AIndex : Integer; AValue : string); 

begin
  If (Fcurrency=AValue) then exit;
  Fcurrency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportheaders.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReportheaders.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TReportheaders.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TReportrows
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReporttotals
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReportwarnings
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
  Result:='20150326';
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
  Result:='https://www.googleapis.com/';
end;

Class Function TAdexchangesellerAPI.APIbasePath : string;

begin
  Result:='/adexchangeseller/v2.0/';
end;

Class Function TAdexchangesellerAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/adexchangeseller/v2.0/';
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
  TAccountsitems.RegisterObject;
  TAdClient.RegisterObject;
  TAdClients.RegisterObject;
  TAdClientsitems.RegisterObject;
  TAlert.RegisterObject;
  TAlerts.RegisterObject;
  TAlertsitems.RegisterObject;
  TCustomChannel.RegisterObject;
  TCustomChanneltargetingInfo.RegisterObject;
  TCustomChannels.RegisterObject;
  TCustomChannelsitems.RegisterObject;
  TMetadata.RegisterObject;
  TMetadataitems.RegisterObject;
  TPreferredDeal.RegisterObject;
  TPreferredDeals.RegisterObject;
  TPreferredDealsitems.RegisterObject;
  TReport.RegisterObject;
  TReportaverages.RegisterObject;
  TReportheaders.RegisterObject;
  TReportrows.RegisterObject;
  TReporttotals.RegisterObject;
  TReportwarnings.RegisterObject;
  TReportingMetadataEntry.RegisterObject;
  TReportingMetadataEntrycompatibleDimensions.RegisterObject;
  TReportingMetadataEntrycompatibleMetrics.RegisterObject;
  TReportingMetadataEntryrequiredDimensions.RegisterObject;
  TReportingMetadataEntryrequiredMetrics.RegisterObject;
  TReportingMetadataEntrysupportedProducts.RegisterObject;
  TSavedReport.RegisterObject;
  TSavedReports.RegisterObject;
  TSavedReportsitems.RegisterObject;
  TUrlChannel.RegisterObject;
  TUrlChannels.RegisterObject;
  TUrlChannelsitems.RegisterObject;
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
  Result.API:=Self;
end;



initialization
  TAdexchangesellerAPI.RegisterAPI;
end.
