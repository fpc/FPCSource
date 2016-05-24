unit googleadsensehost;
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
  TAdUnitmobileContentAdsSettings = class;
  TAdUnitmobileContentAdsSettingsArray = Array of TAdUnitmobileContentAdsSettings;
  TAdUnits = class;
  TAdUnitsArray = Array of TAdUnits;
  TAdUnitsitems = class;
  TAdUnitsitemsArray = Array of TAdUnitsitems;
  TAssociationSession = class;
  TAssociationSessionArray = Array of TAssociationSession;
  TAssociationSessionproductCodes = class;
  TAssociationSessionproductCodesArray = Array of TAssociationSessionproductCodes;
  TCustomChannel = class;
  TCustomChannelArray = Array of TCustomChannel;
  TCustomChannels = class;
  TCustomChannelsArray = Array of TCustomChannels;
  TCustomChannelsitems = class;
  TCustomChannelsitemsArray = Array of TCustomChannelsitems;
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
    Fstatus : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property name : string Index 16 Read Fname Write Setname;
    Property status : string Index 24 Read Fstatus Write Setstatus;
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
  Protected
    //Property setters
    Procedure Setetag(AIndex : Integer; AValue : string); virtual;
    Procedure Setitems(AIndex : Integer; AValue : TAccountsitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property etag : string Index 0 Read Fetag Write Setetag;
    Property items : TAccountsitems Index 8 Read Fitems Write Setitems;
    Property kind : string Index 16 Read Fkind Write Setkind;
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
    Fid : string;
    Fkind : string;
    FmobileContentAdsSettings : TAdUnitmobileContentAdsSettings;
    Fname : string;
    Fstatus : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure SetcontentAdsSettings(AIndex : Integer; AValue : TAdUnitcontentAdsSettings); virtual;
    Procedure SetcustomStyle(AIndex : Integer; AValue : TAdStyle); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmobileContentAdsSettings(AIndex : Integer; AValue : TAdUnitmobileContentAdsSettings); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property contentAdsSettings : TAdUnitcontentAdsSettings Index 8 Read FcontentAdsSettings Write SetcontentAdsSettings;
    Property customStyle : TAdStyle Index 16 Read FcustomStyle Write SetcustomStyle;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property mobileContentAdsSettings : TAdUnitmobileContentAdsSettings Index 40 Read FmobileContentAdsSettings Write SetmobileContentAdsSettings;
    Property name : string Index 48 Read Fname Write Setname;
    Property status : string Index 56 Read Fstatus Write Setstatus;
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
    TAssociationSession
    --------------------------------------------------------------------}
  
  TAssociationSession = Class(TGoogleBaseObject)
  Private
    FaccountId : string;
    Fid : string;
    Fkind : string;
    FproductCodes : TAssociationSessionproductCodes;
    FredirectUrl : string;
    Fstatus : string;
    FuserLocale : string;
    FwebsiteLocale : string;
    FwebsiteUrl : string;
  Protected
    //Property setters
    Procedure SetaccountId(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetproductCodes(AIndex : Integer; AValue : TAssociationSessionproductCodes); virtual;
    Procedure SetredirectUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure SetuserLocale(AIndex : Integer; AValue : string); virtual;
    Procedure SetwebsiteLocale(AIndex : Integer; AValue : string); virtual;
    Procedure SetwebsiteUrl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accountId : string Index 0 Read FaccountId Write SetaccountId;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property productCodes : TAssociationSessionproductCodes Index 24 Read FproductCodes Write SetproductCodes;
    Property redirectUrl : string Index 32 Read FredirectUrl Write SetredirectUrl;
    Property status : string Index 40 Read Fstatus Write Setstatus;
    Property userLocale : string Index 48 Read FuserLocale Write SetuserLocale;
    Property websiteLocale : string Index 56 Read FwebsiteLocale Write SetwebsiteLocale;
    Property websiteUrl : string Index 64 Read FwebsiteUrl Write SetwebsiteUrl;
  end;
  TAssociationSessionClass = Class of TAssociationSession;
  
  { --------------------------------------------------------------------
    TAssociationSessionproductCodes
    --------------------------------------------------------------------}
  
  TAssociationSessionproductCodes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAssociationSessionproductCodesClass = Class of TAssociationSessionproductCodes;
  
  { --------------------------------------------------------------------
    TCustomChannel
    --------------------------------------------------------------------}
  
  TCustomChannel = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fid : string;
    Fkind : string;
    Fname : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property name : string Index 24 Read Fname Write Setname;
  end;
  TCustomChannelClass = Class of TCustomChannel;
  
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
    filterAdClientId : string;
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
    Function Get(adClientId: string) : TAdClient;
    Function List(AQuery : string  = '') : TAdClients;
    Function List(AQuery : TAdclientslistOptions) : TAdClients;
  end;
  
  
  { --------------------------------------------------------------------
    TAssociationsessionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAssociationsessionsResource, method Start
  
  TAssociationsessionsStartOptions = Record
    productCode : string;
    userLocale : string;
    websiteLocale : string;
    websiteUrl : string;
  end;
  
  
  //Optional query Options for TAssociationsessionsResource, method Verify
  
  TAssociationsessionsVerifyOptions = Record
    token : string;
  end;
  
  TAssociationsessionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Start(AQuery : string  = '') : TAssociationSession;
    Function Start(AQuery : TAssociationsessionsstartOptions) : TAssociationSession;
    Function Verify(AQuery : string  = '') : TAssociationSession;
    Function Verify(AQuery : TAssociationsessionsverifyOptions) : TAssociationSession;
  end;
  
  
  { --------------------------------------------------------------------
    TCustomchannelsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCustomchannelsResource, method List
  
  TCustomchannelsListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  
  //Optional query Options for TCustomchannelsResource, method Patch
  
  TCustomchannelsPatchOptions = Record
    customChannelId : string;
  end;
  
  TCustomchannelsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Delete(adClientId: string; customChannelId: string) : TCustomChannel;
    Function Get(adClientId: string; customChannelId: string) : TCustomChannel;
    Function Insert(adClientId: string; aCustomChannel : TCustomChannel) : TCustomChannel;
    Function List(adClientId: string; AQuery : string  = '') : TCustomChannels;
    Function List(adClientId: string; AQuery : TCustomchannelslistOptions) : TCustomChannels;
    Function Patch(adClientId: string; aCustomChannel : TCustomChannel; AQuery : string  = '') : TCustomChannel;
    Function Patch(adClientId: string; aCustomChannel : TCustomChannel; AQuery : TCustomchannelspatchOptions) : TCustomChannel;
    Function Update(adClientId: string; aCustomChannel : TCustomChannel) : TCustomChannel;
  end;
  
  
  { --------------------------------------------------------------------
    TReportsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TReportsResource, method Generate
  
  TReportsGenerateOptions = Record
    dimension : string;
    endDate : string;
    filter : string;
    locale : string;
    maxResults : integer;
    metric : string;
    sort : string;
    startDate : string;
    startIndex : integer;
  end;
  
  TReportsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Generate(AQuery : string  = '') : TReport;
    Function Generate(AQuery : TReportsgenerateOptions) : TReport;
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
    Function Delete(adClientId: string; urlChannelId: string) : TUrlChannel;
    Function Insert(adClientId: string; aUrlChannel : TUrlChannel) : TUrlChannel;
    Function List(adClientId: string; AQuery : string  = '') : TUrlChannels;
    Function List(adClientId: string; AQuery : TUrlchannelslistOptions) : TUrlChannels;
  end;
  
  
  { --------------------------------------------------------------------
    TAdsensehostAPI
    --------------------------------------------------------------------}
  
  TAdsensehostAPI = Class(TGoogleAPI)
  Private
    FAccountsInstance : TAccountsResource;
    FAdclientsInstance : TAdclientsResource;
    FAssociationsessionsInstance : TAssociationsessionsResource;
    FCustomchannelsInstance : TCustomchannelsResource;
    FReportsInstance : TReportsResource;
    FUrlchannelsInstance : TUrlchannelsResource;
    Function GetAccountsInstance : TAccountsResource;virtual;
    Function GetAdclientsInstance : TAdclientsResource;virtual;
    Function GetAssociationsessionsInstance : TAssociationsessionsResource;virtual;
    Function GetCustomchannelsInstance : TCustomchannelsResource;virtual;
    Function GetReportsInstance : TReportsResource;virtual;
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
    Function CreateAssociationsessionsResource(AOwner : TComponent) : TAssociationsessionsResource;virtual;overload;
    Function CreateAssociationsessionsResource : TAssociationsessionsResource;virtual;overload;
    Function CreateCustomchannelsResource(AOwner : TComponent) : TCustomchannelsResource;virtual;overload;
    Function CreateCustomchannelsResource : TCustomchannelsResource;virtual;overload;
    Function CreateReportsResource(AOwner : TComponent) : TReportsResource;virtual;overload;
    Function CreateReportsResource : TReportsResource;virtual;overload;
    Function CreateUrlchannelsResource(AOwner : TComponent) : TUrlchannelsResource;virtual;overload;
    Function CreateUrlchannelsResource : TUrlchannelsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AccountsResource : TAccountsResource Read GetAccountsInstance;
    Property AdclientsResource : TAdclientsResource Read GetAdclientsInstance;
    Property AssociationsessionsResource : TAssociationsessionsResource Read GetAssociationsessionsInstance;
    Property CustomchannelsResource : TCustomchannelsResource Read GetCustomchannelsInstance;
    Property ReportsResource : TReportsResource Read GetReportsInstance;
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



Procedure TAccount.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
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
  TAssociationSession
  --------------------------------------------------------------------}


Procedure TAssociationSession.SetaccountId(AIndex : Integer; AValue : string); 

begin
  If (FaccountId=AValue) then exit;
  FaccountId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAssociationSession.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAssociationSession.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAssociationSession.SetproductCodes(AIndex : Integer; AValue : TAssociationSessionproductCodes); 

begin
  If (FproductCodes=AValue) then exit;
  FproductCodes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAssociationSession.SetredirectUrl(AIndex : Integer; AValue : string); 

begin
  If (FredirectUrl=AValue) then exit;
  FredirectUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAssociationSession.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAssociationSession.SetuserLocale(AIndex : Integer; AValue : string); 

begin
  If (FuserLocale=AValue) then exit;
  FuserLocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAssociationSession.SetwebsiteLocale(AIndex : Integer; AValue : string); 

begin
  If (FwebsiteLocale=AValue) then exit;
  FwebsiteLocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAssociationSession.SetwebsiteUrl(AIndex : Integer; AValue : string); 

begin
  If (FwebsiteUrl=AValue) then exit;
  FwebsiteUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAssociationSessionproductCodes
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
  Result:=TadsensehostAPI;
end;

Function TAccountsResource.Get(accountId: string) : TAccount;

Const
  _HTTPMethod = 'GET';
  _Path       = 'accounts/{accountId}';
  _Methodid   = 'adsensehost.accounts.get';

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
  _Methodid   = 'adsensehost.accounts.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TAccounts) as TAccounts;
end;


Function TAccountsResource.List(AQuery : TAccountslistOptions) : TAccounts;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filterAdClientId',AQuery.filterAdClientId);
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
  Result:=TadsensehostAPI;
end;

Function TAdclientsResource.Get(adClientId: string) : TAdClient;

Const
  _HTTPMethod = 'GET';
  _Path       = 'adclients/{adClientId}';
  _Methodid   = 'adsensehost.adclients.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['adClientId',adClientId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAdClient) as TAdClient;
end;

Function TAdclientsResource.List(AQuery : string = '') : TAdClients;

Const
  _HTTPMethod = 'GET';
  _Path       = 'adclients';
  _Methodid   = 'adsensehost.adclients.list';

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
  TAssociationsessionsResource
  --------------------------------------------------------------------}


Class Function TAssociationsessionsResource.ResourceName : String;

begin
  Result:='associationsessions';
end;

Class Function TAssociationsessionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TadsensehostAPI;
end;

Function TAssociationsessionsResource.Start(AQuery : string = '') : TAssociationSession;

Const
  _HTTPMethod = 'GET';
  _Path       = 'associationsessions/start';
  _Methodid   = 'adsensehost.associationsessions.start';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TAssociationSession) as TAssociationSession;
end;


Function TAssociationsessionsResource.Start(AQuery : TAssociationsessionsstartOptions) : TAssociationSession;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'productCode',AQuery.productCode);
  AddToQuery(_Q,'userLocale',AQuery.userLocale);
  AddToQuery(_Q,'websiteLocale',AQuery.websiteLocale);
  AddToQuery(_Q,'websiteUrl',AQuery.websiteUrl);
  Result:=Start(_Q);
end;

Function TAssociationsessionsResource.Verify(AQuery : string = '') : TAssociationSession;

Const
  _HTTPMethod = 'GET';
  _Path       = 'associationsessions/verify';
  _Methodid   = 'adsensehost.associationsessions.verify';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TAssociationSession) as TAssociationSession;
end;


Function TAssociationsessionsResource.Verify(AQuery : TAssociationsessionsverifyOptions) : TAssociationSession;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'token',AQuery.token);
  Result:=Verify(_Q);
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
  Result:=TadsensehostAPI;
end;

Function TCustomchannelsResource.Delete(adClientId: string; customChannelId: string) : TCustomChannel;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'adclients/{adClientId}/customchannels/{customChannelId}';
  _Methodid   = 'adsensehost.customchannels.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['adClientId',adClientId,'customChannelId',customChannelId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCustomChannel) as TCustomChannel;
end;

Function TCustomchannelsResource.Get(adClientId: string; customChannelId: string) : TCustomChannel;

Const
  _HTTPMethod = 'GET';
  _Path       = 'adclients/{adClientId}/customchannels/{customChannelId}';
  _Methodid   = 'adsensehost.customchannels.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['adClientId',adClientId,'customChannelId',customChannelId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCustomChannel) as TCustomChannel;
end;

Function TCustomchannelsResource.Insert(adClientId: string; aCustomChannel : TCustomChannel) : TCustomChannel;

Const
  _HTTPMethod = 'POST';
  _Path       = 'adclients/{adClientId}/customchannels';
  _Methodid   = 'adsensehost.customchannels.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['adClientId',adClientId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCustomChannel,TCustomChannel) as TCustomChannel;
end;

Function TCustomchannelsResource.List(adClientId: string; AQuery : string = '') : TCustomChannels;

Const
  _HTTPMethod = 'GET';
  _Path       = 'adclients/{adClientId}/customchannels';
  _Methodid   = 'adsensehost.customchannels.list';

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

Function TCustomchannelsResource.Patch(adClientId: string; aCustomChannel : TCustomChannel; AQuery : string = '') : TCustomChannel;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'adclients/{adClientId}/customchannels';
  _Methodid   = 'adsensehost.customchannels.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['adClientId',adClientId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aCustomChannel,TCustomChannel) as TCustomChannel;
end;


Function TCustomchannelsResource.Patch(adClientId: string; aCustomChannel : TCustomChannel; AQuery : TCustomchannelspatchOptions) : TCustomChannel;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'customChannelId',AQuery.customChannelId);
  Result:=Patch(adClientId,aCustomChannel,_Q);
end;

Function TCustomchannelsResource.Update(adClientId: string; aCustomChannel : TCustomChannel) : TCustomChannel;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'adclients/{adClientId}/customchannels';
  _Methodid   = 'adsensehost.customchannels.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['adClientId',adClientId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCustomChannel,TCustomChannel) as TCustomChannel;
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
  Result:=TadsensehostAPI;
end;

Function TReportsResource.Generate(AQuery : string = '') : TReport;

Const
  _HTTPMethod = 'GET';
  _Path       = 'reports';
  _Methodid   = 'adsensehost.reports.generate';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TReport) as TReport;
end;


Function TReportsResource.Generate(AQuery : TReportsgenerateOptions) : TReport;

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
  Result:=Generate(_Q);
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
  Result:=TadsensehostAPI;
end;

Function TUrlchannelsResource.Delete(adClientId: string; urlChannelId: string) : TUrlChannel;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'adclients/{adClientId}/urlchannels/{urlChannelId}';
  _Methodid   = 'adsensehost.urlchannels.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['adClientId',adClientId,'urlChannelId',urlChannelId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TUrlChannel) as TUrlChannel;
end;

Function TUrlchannelsResource.Insert(adClientId: string; aUrlChannel : TUrlChannel) : TUrlChannel;

Const
  _HTTPMethod = 'POST';
  _Path       = 'adclients/{adClientId}/urlchannels';
  _Methodid   = 'adsensehost.urlchannels.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['adClientId',adClientId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUrlChannel,TUrlChannel) as TUrlChannel;
end;

Function TUrlchannelsResource.List(adClientId: string; AQuery : string = '') : TUrlChannels;

Const
  _HTTPMethod = 'GET';
  _Path       = 'adclients/{adClientId}/urlchannels';
  _Methodid   = 'adsensehost.urlchannels.list';

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
  TAdsensehostAPI
  --------------------------------------------------------------------}

Class Function TAdsensehostAPI.APIName : String;

begin
  Result:='adsensehost';
end;

Class Function TAdsensehostAPI.APIVersion : String;

begin
  Result:='v4.1';
end;

Class Function TAdsensehostAPI.APIRevision : String;

begin
  Result:='20150309';
end;

Class Function TAdsensehostAPI.APIID : String;

begin
  Result:='adsensehost:v4.1';
end;

Class Function TAdsensehostAPI.APITitle : String;

begin
  Result:='AdSense Host API';
end;

Class Function TAdsensehostAPI.APIDescription : String;

begin
  Result:='Gives AdSense Hosts access to report generation, ad code generation, and publisher management capabilities.';
end;

Class Function TAdsensehostAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TAdsensehostAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TAdsensehostAPI.APIIcon16 : String;

begin
  Result:='https://www.google.com/images/icons/product/adsense-16.png';
end;

Class Function TAdsensehostAPI.APIIcon32 : String;

begin
  Result:='https://www.google.com/images/icons/product/adsense-32.png';
end;

Class Function TAdsensehostAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/adsense/host/';
end;

Class Function TAdsensehostAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TAdsensehostAPI.APIbasePath : string;

begin
  Result:='/adsensehost/v4.1/';
end;

Class Function TAdsensehostAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/adsensehost/v4.1/';
end;

Class Function TAdsensehostAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TAdsensehostAPI.APIservicePath : string;

begin
  Result:='adsensehost/v4.1/';
end;

Class Function TAdsensehostAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TAdsensehostAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/adsensehost';
  Result[0].Description:='View and manage your AdSense host data and associated accounts';
  
end;

Class Function TAdsensehostAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TAdsensehostAPI.RegisterAPIResources;

begin
  TAccount.RegisterObject;
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
  TAdUnitmobileContentAdsSettings.RegisterObject;
  TAdUnits.RegisterObject;
  TAdUnitsitems.RegisterObject;
  TAssociationSession.RegisterObject;
  TAssociationSessionproductCodes.RegisterObject;
  TCustomChannel.RegisterObject;
  TCustomChannels.RegisterObject;
  TCustomChannelsitems.RegisterObject;
  TReport.RegisterObject;
  TReportaverages.RegisterObject;
  TReportheaders.RegisterObject;
  TReportrows.RegisterObject;
  TReporttotals.RegisterObject;
  TReportwarnings.RegisterObject;
  TUrlChannel.RegisterObject;
  TUrlChannels.RegisterObject;
  TUrlChannelsitems.RegisterObject;
end;


Function TAdsensehostAPI.GetAccountsInstance : TAccountsResource;

begin
  if (FAccountsInstance=Nil) then
    FAccountsInstance:=CreateAccountsResource;
  Result:=FAccountsInstance;
end;

Function TAdsensehostAPI.CreateAccountsResource : TAccountsResource;

begin
  Result:=CreateAccountsResource(Self);
end;


Function TAdsensehostAPI.CreateAccountsResource(AOwner : TComponent) : TAccountsResource;

begin
  Result:=TAccountsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAdsensehostAPI.GetAdclientsInstance : TAdclientsResource;

begin
  if (FAdclientsInstance=Nil) then
    FAdclientsInstance:=CreateAdclientsResource;
  Result:=FAdclientsInstance;
end;

Function TAdsensehostAPI.CreateAdclientsResource : TAdclientsResource;

begin
  Result:=CreateAdclientsResource(Self);
end;


Function TAdsensehostAPI.CreateAdclientsResource(AOwner : TComponent) : TAdclientsResource;

begin
  Result:=TAdclientsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAdsensehostAPI.GetAssociationsessionsInstance : TAssociationsessionsResource;

begin
  if (FAssociationsessionsInstance=Nil) then
    FAssociationsessionsInstance:=CreateAssociationsessionsResource;
  Result:=FAssociationsessionsInstance;
end;

Function TAdsensehostAPI.CreateAssociationsessionsResource : TAssociationsessionsResource;

begin
  Result:=CreateAssociationsessionsResource(Self);
end;


Function TAdsensehostAPI.CreateAssociationsessionsResource(AOwner : TComponent) : TAssociationsessionsResource;

begin
  Result:=TAssociationsessionsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAdsensehostAPI.GetCustomchannelsInstance : TCustomchannelsResource;

begin
  if (FCustomchannelsInstance=Nil) then
    FCustomchannelsInstance:=CreateCustomchannelsResource;
  Result:=FCustomchannelsInstance;
end;

Function TAdsensehostAPI.CreateCustomchannelsResource : TCustomchannelsResource;

begin
  Result:=CreateCustomchannelsResource(Self);
end;


Function TAdsensehostAPI.CreateCustomchannelsResource(AOwner : TComponent) : TCustomchannelsResource;

begin
  Result:=TCustomchannelsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAdsensehostAPI.GetReportsInstance : TReportsResource;

begin
  if (FReportsInstance=Nil) then
    FReportsInstance:=CreateReportsResource;
  Result:=FReportsInstance;
end;

Function TAdsensehostAPI.CreateReportsResource : TReportsResource;

begin
  Result:=CreateReportsResource(Self);
end;


Function TAdsensehostAPI.CreateReportsResource(AOwner : TComponent) : TReportsResource;

begin
  Result:=TReportsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TAdsensehostAPI.GetUrlchannelsInstance : TUrlchannelsResource;

begin
  if (FUrlchannelsInstance=Nil) then
    FUrlchannelsInstance:=CreateUrlchannelsResource;
  Result:=FUrlchannelsInstance;
end;

Function TAdsensehostAPI.CreateUrlchannelsResource : TUrlchannelsResource;

begin
  Result:=CreateUrlchannelsResource(Self);
end;


Function TAdsensehostAPI.CreateUrlchannelsResource(AOwner : TComponent) : TUrlchannelsResource;

begin
  Result:=TUrlchannelsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TAdsensehostAPI.RegisterAPI;
end.
