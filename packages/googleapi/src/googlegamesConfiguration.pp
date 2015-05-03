unit googlegamesConfiguration;
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
  TAchievementConfiguration = class;
  TAchievementConfigurationArray = Array of TAchievementConfiguration;
  TAchievementConfigurationDetail = class;
  TAchievementConfigurationDetailArray = Array of TAchievementConfigurationDetail;
  TAchievementConfigurationListResponse = class;
  TAchievementConfigurationListResponseArray = Array of TAchievementConfigurationListResponse;
  TAchievementConfigurationListResponseitems = class;
  TAchievementConfigurationListResponseitemsArray = Array of TAchievementConfigurationListResponseitems;
  TGamesNumberAffixConfiguration = class;
  TGamesNumberAffixConfigurationArray = Array of TGamesNumberAffixConfiguration;
  TGamesNumberFormatConfiguration = class;
  TGamesNumberFormatConfigurationArray = Array of TGamesNumberFormatConfiguration;
  TImageConfiguration = class;
  TImageConfigurationArray = Array of TImageConfiguration;
  TLeaderboardConfiguration = class;
  TLeaderboardConfigurationArray = Array of TLeaderboardConfiguration;
  TLeaderboardConfigurationDetail = class;
  TLeaderboardConfigurationDetailArray = Array of TLeaderboardConfigurationDetail;
  TLeaderboardConfigurationListResponse = class;
  TLeaderboardConfigurationListResponseArray = Array of TLeaderboardConfigurationListResponse;
  TLeaderboardConfigurationListResponseitems = class;
  TLeaderboardConfigurationListResponseitemsArray = Array of TLeaderboardConfigurationListResponseitems;
  TLocalizedString = class;
  TLocalizedStringArray = Array of TLocalizedString;
  TLocalizedStringBundle = class;
  TLocalizedStringBundleArray = Array of TLocalizedStringBundle;
  TLocalizedStringBundletranslations = class;
  TLocalizedStringBundletranslationsArray = Array of TLocalizedStringBundletranslations;
  
  { --------------------------------------------------------------------
    TAchievementConfiguration
    --------------------------------------------------------------------}
  
  TAchievementConfiguration = Class(TGoogleBaseObject)
  Private
    FachievementType : string;
    Fdraft : TAchievementConfigurationDetail;
    Fid : string;
    FinitialState : string;
    Fkind : string;
    F_published : TAchievementConfigurationDetail;
    FstepsToUnlock : integer;
    Ftoken : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetachievementType(AIndex : Integer; AValue : string); virtual;
    Procedure Setdraft(AIndex : Integer; AValue : TAchievementConfigurationDetail); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetinitialState(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Set_published(AIndex : Integer; AValue : TAchievementConfigurationDetail); virtual;
    Procedure SetstepsToUnlock(AIndex : Integer; AValue : integer); virtual;
    Procedure Settoken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property achievementType : string Index 0 Read FachievementType Write SetachievementType;
    Property draft : TAchievementConfigurationDetail Index 8 Read Fdraft Write Setdraft;
    Property id : string Index 16 Read Fid Write Setid;
    Property initialState : string Index 24 Read FinitialState Write SetinitialState;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property _published : TAchievementConfigurationDetail Index 40 Read F_published Write Set_published;
    Property stepsToUnlock : integer Index 48 Read FstepsToUnlock Write SetstepsToUnlock;
    Property token : string Index 56 Read Ftoken Write Settoken;
  end;
  TAchievementConfigurationClass = Class of TAchievementConfiguration;
  
  { --------------------------------------------------------------------
    TAchievementConfigurationDetail
    --------------------------------------------------------------------}
  
  TAchievementConfigurationDetail = Class(TGoogleBaseObject)
  Private
    Fdescription : TLocalizedStringBundle;
    FiconUrl : string;
    Fkind : string;
    Fname : TLocalizedStringBundle;
    FpointValue : integer;
    FsortRank : integer;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : TLocalizedStringBundle); virtual;
    Procedure SeticonUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : TLocalizedStringBundle); virtual;
    Procedure SetpointValue(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsortRank(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property description : TLocalizedStringBundle Index 0 Read Fdescription Write Setdescription;
    Property iconUrl : string Index 8 Read FiconUrl Write SeticonUrl;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property name : TLocalizedStringBundle Index 24 Read Fname Write Setname;
    Property pointValue : integer Index 32 Read FpointValue Write SetpointValue;
    Property sortRank : integer Index 40 Read FsortRank Write SetsortRank;
  end;
  TAchievementConfigurationDetailClass = Class of TAchievementConfigurationDetail;
  
  { --------------------------------------------------------------------
    TAchievementConfigurationListResponse
    --------------------------------------------------------------------}
  
  TAchievementConfigurationListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TAchievementConfigurationListResponseitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TAchievementConfigurationListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TAchievementConfigurationListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TAchievementConfigurationListResponseClass = Class of TAchievementConfigurationListResponse;
  
  { --------------------------------------------------------------------
    TAchievementConfigurationListResponseitems
    --------------------------------------------------------------------}
  
  TAchievementConfigurationListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAchievementConfigurationListResponseitemsClass = Class of TAchievementConfigurationListResponseitems;
  
  { --------------------------------------------------------------------
    TGamesNumberAffixConfiguration
    --------------------------------------------------------------------}
  
  TGamesNumberAffixConfiguration = Class(TGoogleBaseObject)
  Private
    Ffew : TLocalizedStringBundle;
    Fmany : TLocalizedStringBundle;
    Fone : TLocalizedStringBundle;
    Fother : TLocalizedStringBundle;
    Ftwo : TLocalizedStringBundle;
    Fzero : TLocalizedStringBundle;
  Protected
    //Property setters
    Procedure Setfew(AIndex : Integer; AValue : TLocalizedStringBundle); virtual;
    Procedure Setmany(AIndex : Integer; AValue : TLocalizedStringBundle); virtual;
    Procedure Setone(AIndex : Integer; AValue : TLocalizedStringBundle); virtual;
    Procedure Setother(AIndex : Integer; AValue : TLocalizedStringBundle); virtual;
    Procedure Settwo(AIndex : Integer; AValue : TLocalizedStringBundle); virtual;
    Procedure Setzero(AIndex : Integer; AValue : TLocalizedStringBundle); virtual;
  Public
  Published
    Property few : TLocalizedStringBundle Index 0 Read Ffew Write Setfew;
    Property many : TLocalizedStringBundle Index 8 Read Fmany Write Setmany;
    Property one : TLocalizedStringBundle Index 16 Read Fone Write Setone;
    Property other : TLocalizedStringBundle Index 24 Read Fother Write Setother;
    Property two : TLocalizedStringBundle Index 32 Read Ftwo Write Settwo;
    Property zero : TLocalizedStringBundle Index 40 Read Fzero Write Setzero;
  end;
  TGamesNumberAffixConfigurationClass = Class of TGamesNumberAffixConfiguration;
  
  { --------------------------------------------------------------------
    TGamesNumberFormatConfiguration
    --------------------------------------------------------------------}
  
  TGamesNumberFormatConfiguration = Class(TGoogleBaseObject)
  Private
    FcurrencyCode : string;
    FnumDecimalPlaces : integer;
    FnumberFormatType : string;
    Fsuffix : TGamesNumberAffixConfiguration;
  Protected
    //Property setters
    Procedure SetcurrencyCode(AIndex : Integer; AValue : string); virtual;
    Procedure SetnumDecimalPlaces(AIndex : Integer; AValue : integer); virtual;
    Procedure SetnumberFormatType(AIndex : Integer; AValue : string); virtual;
    Procedure Setsuffix(AIndex : Integer; AValue : TGamesNumberAffixConfiguration); virtual;
  Public
  Published
    Property currencyCode : string Index 0 Read FcurrencyCode Write SetcurrencyCode;
    Property numDecimalPlaces : integer Index 8 Read FnumDecimalPlaces Write SetnumDecimalPlaces;
    Property numberFormatType : string Index 16 Read FnumberFormatType Write SetnumberFormatType;
    Property suffix : TGamesNumberAffixConfiguration Index 24 Read Fsuffix Write Setsuffix;
  end;
  TGamesNumberFormatConfigurationClass = Class of TGamesNumberFormatConfiguration;
  
  { --------------------------------------------------------------------
    TImageConfiguration
    --------------------------------------------------------------------}
  
  TImageConfiguration = Class(TGoogleBaseObject)
  Private
    FimageType : string;
    Fkind : string;
    FresourceId : string;
    Furl : string;
  Protected
    //Property setters
    Procedure SetimageType(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetresourceId(AIndex : Integer; AValue : string); virtual;
    Procedure Seturl(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property imageType : string Index 0 Read FimageType Write SetimageType;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property resourceId : string Index 16 Read FresourceId Write SetresourceId;
    Property url : string Index 24 Read Furl Write Seturl;
  end;
  TImageConfigurationClass = Class of TImageConfiguration;
  
  { --------------------------------------------------------------------
    TLeaderboardConfiguration
    --------------------------------------------------------------------}
  
  TLeaderboardConfiguration = Class(TGoogleBaseObject)
  Private
    Fdraft : TLeaderboardConfigurationDetail;
    Fid : string;
    Fkind : string;
    F_published : TLeaderboardConfigurationDetail;
    FscoreMax : string;
    FscoreMin : string;
    FscoreOrder : string;
    Ftoken : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdraft(AIndex : Integer; AValue : TLeaderboardConfigurationDetail); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Set_published(AIndex : Integer; AValue : TLeaderboardConfigurationDetail); virtual;
    Procedure SetscoreMax(AIndex : Integer; AValue : string); virtual;
    Procedure SetscoreMin(AIndex : Integer; AValue : string); virtual;
    Procedure SetscoreOrder(AIndex : Integer; AValue : string); virtual;
    Procedure Settoken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property draft : TLeaderboardConfigurationDetail Index 0 Read Fdraft Write Setdraft;
    Property id : string Index 8 Read Fid Write Setid;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property _published : TLeaderboardConfigurationDetail Index 24 Read F_published Write Set_published;
    Property scoreMax : string Index 32 Read FscoreMax Write SetscoreMax;
    Property scoreMin : string Index 40 Read FscoreMin Write SetscoreMin;
    Property scoreOrder : string Index 48 Read FscoreOrder Write SetscoreOrder;
    Property token : string Index 56 Read Ftoken Write Settoken;
  end;
  TLeaderboardConfigurationClass = Class of TLeaderboardConfiguration;
  
  { --------------------------------------------------------------------
    TLeaderboardConfigurationDetail
    --------------------------------------------------------------------}
  
  TLeaderboardConfigurationDetail = Class(TGoogleBaseObject)
  Private
    FiconUrl : string;
    Fkind : string;
    Fname : TLocalizedStringBundle;
    FscoreFormat : TGamesNumberFormatConfiguration;
    FsortRank : integer;
  Protected
    //Property setters
    Procedure SeticonUrl(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : TLocalizedStringBundle); virtual;
    Procedure SetscoreFormat(AIndex : Integer; AValue : TGamesNumberFormatConfiguration); virtual;
    Procedure SetsortRank(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property iconUrl : string Index 0 Read FiconUrl Write SeticonUrl;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property name : TLocalizedStringBundle Index 16 Read Fname Write Setname;
    Property scoreFormat : TGamesNumberFormatConfiguration Index 24 Read FscoreFormat Write SetscoreFormat;
    Property sortRank : integer Index 32 Read FsortRank Write SetsortRank;
  end;
  TLeaderboardConfigurationDetailClass = Class of TLeaderboardConfigurationDetail;
  
  { --------------------------------------------------------------------
    TLeaderboardConfigurationListResponse
    --------------------------------------------------------------------}
  
  TLeaderboardConfigurationListResponse = Class(TGoogleBaseObject)
  Private
    Fitems : TLeaderboardConfigurationListResponseitems;
    Fkind : string;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setitems(AIndex : Integer; AValue : TLeaderboardConfigurationListResponseitems); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property items : TLeaderboardConfigurationListResponseitems Index 0 Read Fitems Write Setitems;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TLeaderboardConfigurationListResponseClass = Class of TLeaderboardConfigurationListResponse;
  
  { --------------------------------------------------------------------
    TLeaderboardConfigurationListResponseitems
    --------------------------------------------------------------------}
  
  TLeaderboardConfigurationListResponseitems = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLeaderboardConfigurationListResponseitemsClass = Class of TLeaderboardConfigurationListResponseitems;
  
  { --------------------------------------------------------------------
    TLocalizedString
    --------------------------------------------------------------------}
  
  TLocalizedString = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Flocale : string;
    Fvalue : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setlocale(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property locale : string Index 8 Read Flocale Write Setlocale;
    Property value : string Index 16 Read Fvalue Write Setvalue;
  end;
  TLocalizedStringClass = Class of TLocalizedString;
  
  { --------------------------------------------------------------------
    TLocalizedStringBundle
    --------------------------------------------------------------------}
  
  TLocalizedStringBundle = Class(TGoogleBaseObject)
  Private
    Fkind : string;
    Ftranslations : TLocalizedStringBundletranslations;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Settranslations(AIndex : Integer; AValue : TLocalizedStringBundletranslations); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
    Property translations : TLocalizedStringBundletranslations Index 8 Read Ftranslations Write Settranslations;
  end;
  TLocalizedStringBundleClass = Class of TLocalizedStringBundle;
  
  { --------------------------------------------------------------------
    TLocalizedStringBundletranslations
    --------------------------------------------------------------------}
  
  TLocalizedStringBundletranslations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLocalizedStringBundletranslationsClass = Class of TLocalizedStringBundletranslations;
  
  { --------------------------------------------------------------------
    TAchievementConfigurationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAchievementConfigurationsResource, method List
  
  TAchievementConfigurationsListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TAchievementConfigurationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(achievementId: string);
    Function Get(achievementId: string) : TAchievementConfiguration;
    Function Insert(applicationId: string; aAchievementConfiguration : TAchievementConfiguration) : TAchievementConfiguration;
    Function List(applicationId: string; AQuery : string  = '') : TAchievementConfigurationListResponse;
    Function List(applicationId: string; AQuery : TAchievementConfigurationslistOptions) : TAchievementConfigurationListResponse;
    Function Patch(achievementId: string; aAchievementConfiguration : TAchievementConfiguration) : TAchievementConfiguration;
    Function Update(achievementId: string; aAchievementConfiguration : TAchievementConfiguration) : TAchievementConfiguration;
  end;
  
  
  { --------------------------------------------------------------------
    TImageConfigurationsResource
    --------------------------------------------------------------------}
  
  TImageConfigurationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Upload(imageType: string; resourceId: string) : TImageConfiguration;
  end;
  
  
  { --------------------------------------------------------------------
    TLeaderboardConfigurationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TLeaderboardConfigurationsResource, method List
  
  TLeaderboardConfigurationsListOptions = Record
    maxResults : integer;
    pageToken : string;
  end;
  
  TLeaderboardConfigurationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(leaderboardId: string);
    Function Get(leaderboardId: string) : TLeaderboardConfiguration;
    Function Insert(applicationId: string; aLeaderboardConfiguration : TLeaderboardConfiguration) : TLeaderboardConfiguration;
    Function List(applicationId: string; AQuery : string  = '') : TLeaderboardConfigurationListResponse;
    Function List(applicationId: string; AQuery : TLeaderboardConfigurationslistOptions) : TLeaderboardConfigurationListResponse;
    Function Patch(leaderboardId: string; aLeaderboardConfiguration : TLeaderboardConfiguration) : TLeaderboardConfiguration;
    Function Update(leaderboardId: string; aLeaderboardConfiguration : TLeaderboardConfiguration) : TLeaderboardConfiguration;
  end;
  
  
  { --------------------------------------------------------------------
    TGamesConfigurationAPI
    --------------------------------------------------------------------}
  
  TGamesConfigurationAPI = Class(TGoogleAPI)
  Private
    FAchievementConfigurationsInstance : TAchievementConfigurationsResource;
    FImageConfigurationsInstance : TImageConfigurationsResource;
    FLeaderboardConfigurationsInstance : TLeaderboardConfigurationsResource;
    Function GetAchievementConfigurationsInstance : TAchievementConfigurationsResource;virtual;
    Function GetImageConfigurationsInstance : TImageConfigurationsResource;virtual;
    Function GetLeaderboardConfigurationsInstance : TLeaderboardConfigurationsResource;virtual;
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
    Function CreateAchievementConfigurationsResource(AOwner : TComponent) : TAchievementConfigurationsResource;virtual;overload;
    Function CreateAchievementConfigurationsResource : TAchievementConfigurationsResource;virtual;overload;
    Function CreateImageConfigurationsResource(AOwner : TComponent) : TImageConfigurationsResource;virtual;overload;
    Function CreateImageConfigurationsResource : TImageConfigurationsResource;virtual;overload;
    Function CreateLeaderboardConfigurationsResource(AOwner : TComponent) : TLeaderboardConfigurationsResource;virtual;overload;
    Function CreateLeaderboardConfigurationsResource : TLeaderboardConfigurationsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AchievementConfigurationsResource : TAchievementConfigurationsResource Read GetAchievementConfigurationsInstance;
    Property ImageConfigurationsResource : TImageConfigurationsResource Read GetImageConfigurationsInstance;
    Property LeaderboardConfigurationsResource : TLeaderboardConfigurationsResource Read GetLeaderboardConfigurationsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAchievementConfiguration
  --------------------------------------------------------------------}


Procedure TAchievementConfiguration.SetachievementType(AIndex : Integer; AValue : string); 

begin
  If (FachievementType=AValue) then exit;
  FachievementType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementConfiguration.Setdraft(AIndex : Integer; AValue : TAchievementConfigurationDetail); 

begin
  If (Fdraft=AValue) then exit;
  Fdraft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementConfiguration.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementConfiguration.SetinitialState(AIndex : Integer; AValue : string); 

begin
  If (FinitialState=AValue) then exit;
  FinitialState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementConfiguration.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementConfiguration.Set_published(AIndex : Integer; AValue : TAchievementConfigurationDetail); 

begin
  If (F_published=AValue) then exit;
  F_published:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementConfiguration.SetstepsToUnlock(AIndex : Integer; AValue : integer); 

begin
  If (FstepsToUnlock=AValue) then exit;
  FstepsToUnlock:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementConfiguration.Settoken(AIndex : Integer; AValue : string); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAchievementConfiguration.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_published' : Result:='published';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAchievementConfigurationDetail
  --------------------------------------------------------------------}


Procedure TAchievementConfigurationDetail.Setdescription(AIndex : Integer; AValue : TLocalizedStringBundle); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementConfigurationDetail.SeticonUrl(AIndex : Integer; AValue : string); 

begin
  If (FiconUrl=AValue) then exit;
  FiconUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementConfigurationDetail.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementConfigurationDetail.Setname(AIndex : Integer; AValue : TLocalizedStringBundle); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementConfigurationDetail.SetpointValue(AIndex : Integer; AValue : integer); 

begin
  If (FpointValue=AValue) then exit;
  FpointValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementConfigurationDetail.SetsortRank(AIndex : Integer; AValue : integer); 

begin
  If (FsortRank=AValue) then exit;
  FsortRank:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAchievementConfigurationListResponse
  --------------------------------------------------------------------}


Procedure TAchievementConfigurationListResponse.Setitems(AIndex : Integer; AValue : TAchievementConfigurationListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementConfigurationListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAchievementConfigurationListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAchievementConfigurationListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGamesNumberAffixConfiguration
  --------------------------------------------------------------------}


Procedure TGamesNumberAffixConfiguration.Setfew(AIndex : Integer; AValue : TLocalizedStringBundle); 

begin
  If (Ffew=AValue) then exit;
  Ffew:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesNumberAffixConfiguration.Setmany(AIndex : Integer; AValue : TLocalizedStringBundle); 

begin
  If (Fmany=AValue) then exit;
  Fmany:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesNumberAffixConfiguration.Setone(AIndex : Integer; AValue : TLocalizedStringBundle); 

begin
  If (Fone=AValue) then exit;
  Fone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesNumberAffixConfiguration.Setother(AIndex : Integer; AValue : TLocalizedStringBundle); 

begin
  If (Fother=AValue) then exit;
  Fother:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesNumberAffixConfiguration.Settwo(AIndex : Integer; AValue : TLocalizedStringBundle); 

begin
  If (Ftwo=AValue) then exit;
  Ftwo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesNumberAffixConfiguration.Setzero(AIndex : Integer; AValue : TLocalizedStringBundle); 

begin
  If (Fzero=AValue) then exit;
  Fzero:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGamesNumberFormatConfiguration
  --------------------------------------------------------------------}


Procedure TGamesNumberFormatConfiguration.SetcurrencyCode(AIndex : Integer; AValue : string); 

begin
  If (FcurrencyCode=AValue) then exit;
  FcurrencyCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesNumberFormatConfiguration.SetnumDecimalPlaces(AIndex : Integer; AValue : integer); 

begin
  If (FnumDecimalPlaces=AValue) then exit;
  FnumDecimalPlaces:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesNumberFormatConfiguration.SetnumberFormatType(AIndex : Integer; AValue : string); 

begin
  If (FnumberFormatType=AValue) then exit;
  FnumberFormatType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGamesNumberFormatConfiguration.Setsuffix(AIndex : Integer; AValue : TGamesNumberAffixConfiguration); 

begin
  If (Fsuffix=AValue) then exit;
  Fsuffix:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImageConfiguration
  --------------------------------------------------------------------}


Procedure TImageConfiguration.SetimageType(AIndex : Integer; AValue : string); 

begin
  If (FimageType=AValue) then exit;
  FimageType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageConfiguration.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageConfiguration.SetresourceId(AIndex : Integer; AValue : string); 

begin
  If (FresourceId=AValue) then exit;
  FresourceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImageConfiguration.Seturl(AIndex : Integer; AValue : string); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLeaderboardConfiguration
  --------------------------------------------------------------------}


Procedure TLeaderboardConfiguration.Setdraft(AIndex : Integer; AValue : TLeaderboardConfigurationDetail); 

begin
  If (Fdraft=AValue) then exit;
  Fdraft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardConfiguration.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardConfiguration.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardConfiguration.Set_published(AIndex : Integer; AValue : TLeaderboardConfigurationDetail); 

begin
  If (F_published=AValue) then exit;
  F_published:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardConfiguration.SetscoreMax(AIndex : Integer; AValue : string); 

begin
  If (FscoreMax=AValue) then exit;
  FscoreMax:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardConfiguration.SetscoreMin(AIndex : Integer; AValue : string); 

begin
  If (FscoreMin=AValue) then exit;
  FscoreMin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardConfiguration.SetscoreOrder(AIndex : Integer; AValue : string); 

begin
  If (FscoreOrder=AValue) then exit;
  FscoreOrder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardConfiguration.Settoken(AIndex : Integer; AValue : string); 

begin
  If (Ftoken=AValue) then exit;
  Ftoken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TLeaderboardConfiguration.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_published' : Result:='published';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TLeaderboardConfigurationDetail
  --------------------------------------------------------------------}


Procedure TLeaderboardConfigurationDetail.SeticonUrl(AIndex : Integer; AValue : string); 

begin
  If (FiconUrl=AValue) then exit;
  FiconUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardConfigurationDetail.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardConfigurationDetail.Setname(AIndex : Integer; AValue : TLocalizedStringBundle); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardConfigurationDetail.SetscoreFormat(AIndex : Integer; AValue : TGamesNumberFormatConfiguration); 

begin
  If (FscoreFormat=AValue) then exit;
  FscoreFormat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardConfigurationDetail.SetsortRank(AIndex : Integer; AValue : integer); 

begin
  If (FsortRank=AValue) then exit;
  FsortRank:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLeaderboardConfigurationListResponse
  --------------------------------------------------------------------}


Procedure TLeaderboardConfigurationListResponse.Setitems(AIndex : Integer; AValue : TLeaderboardConfigurationListResponseitems); 

begin
  If (Fitems=AValue) then exit;
  Fitems:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardConfigurationListResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLeaderboardConfigurationListResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLeaderboardConfigurationListResponseitems
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLocalizedString
  --------------------------------------------------------------------}


Procedure TLocalizedString.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocalizedString.Setlocale(AIndex : Integer; AValue : string); 

begin
  If (Flocale=AValue) then exit;
  Flocale:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocalizedString.Setvalue(AIndex : Integer; AValue : string); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLocalizedStringBundle
  --------------------------------------------------------------------}


Procedure TLocalizedStringBundle.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocalizedStringBundle.Settranslations(AIndex : Integer; AValue : TLocalizedStringBundletranslations); 

begin
  If (Ftranslations=AValue) then exit;
  Ftranslations:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLocalizedStringBundletranslations
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAchievementConfigurationsResource
  --------------------------------------------------------------------}


Class Function TAchievementConfigurationsResource.ResourceName : String;

begin
  Result:='achievementConfigurations';
end;

Class Function TAchievementConfigurationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesConfigurationAPI;
end;

Procedure TAchievementConfigurationsResource.Delete(achievementId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'achievements/{achievementId}';
  _Methodid   = 'gamesConfiguration.achievementConfigurations.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['achievementId',achievementId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAchievementConfigurationsResource.Get(achievementId: string) : TAchievementConfiguration;

Const
  _HTTPMethod = 'GET';
  _Path       = 'achievements/{achievementId}';
  _Methodid   = 'gamesConfiguration.achievementConfigurations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['achievementId',achievementId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAchievementConfiguration) as TAchievementConfiguration;
end;

Function TAchievementConfigurationsResource.Insert(applicationId: string; aAchievementConfiguration : TAchievementConfiguration) : TAchievementConfiguration;

Const
  _HTTPMethod = 'POST';
  _Path       = 'applications/{applicationId}/achievements';
  _Methodid   = 'gamesConfiguration.achievementConfigurations.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['applicationId',applicationId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAchievementConfiguration,TAchievementConfiguration) as TAchievementConfiguration;
end;

Function TAchievementConfigurationsResource.List(applicationId: string; AQuery : string = '') : TAchievementConfigurationListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'applications/{applicationId}/achievements';
  _Methodid   = 'gamesConfiguration.achievementConfigurations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['applicationId',applicationId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TAchievementConfigurationListResponse) as TAchievementConfigurationListResponse;
end;


Function TAchievementConfigurationsResource.List(applicationId: string; AQuery : TAchievementConfigurationslistOptions) : TAchievementConfigurationListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(applicationId,_Q);
end;

Function TAchievementConfigurationsResource.Patch(achievementId: string; aAchievementConfiguration : TAchievementConfiguration) : TAchievementConfiguration;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'achievements/{achievementId}';
  _Methodid   = 'gamesConfiguration.achievementConfigurations.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['achievementId',achievementId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAchievementConfiguration,TAchievementConfiguration) as TAchievementConfiguration;
end;

Function TAchievementConfigurationsResource.Update(achievementId: string; aAchievementConfiguration : TAchievementConfiguration) : TAchievementConfiguration;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'achievements/{achievementId}';
  _Methodid   = 'gamesConfiguration.achievementConfigurations.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['achievementId',achievementId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAchievementConfiguration,TAchievementConfiguration) as TAchievementConfiguration;
end;



{ --------------------------------------------------------------------
  TImageConfigurationsResource
  --------------------------------------------------------------------}


Class Function TImageConfigurationsResource.ResourceName : String;

begin
  Result:='imageConfigurations';
end;

Class Function TImageConfigurationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesConfigurationAPI;
end;

Function TImageConfigurationsResource.Upload(imageType: string; resourceId: string) : TImageConfiguration;

Const
  _HTTPMethod = 'POST';
  _Path       = 'images/{resourceId}/imageType/{imageType}';
  _Methodid   = 'gamesConfiguration.imageConfigurations.upload';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['imageType',imageType,'resourceId',resourceId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TImageConfiguration) as TImageConfiguration;
end;



{ --------------------------------------------------------------------
  TLeaderboardConfigurationsResource
  --------------------------------------------------------------------}


Class Function TLeaderboardConfigurationsResource.ResourceName : String;

begin
  Result:='leaderboardConfigurations';
end;

Class Function TLeaderboardConfigurationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgamesConfigurationAPI;
end;

Procedure TLeaderboardConfigurationsResource.Delete(leaderboardId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'leaderboards/{leaderboardId}';
  _Methodid   = 'gamesConfiguration.leaderboardConfigurations.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['leaderboardId',leaderboardId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TLeaderboardConfigurationsResource.Get(leaderboardId: string) : TLeaderboardConfiguration;

Const
  _HTTPMethod = 'GET';
  _Path       = 'leaderboards/{leaderboardId}';
  _Methodid   = 'gamesConfiguration.leaderboardConfigurations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['leaderboardId',leaderboardId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TLeaderboardConfiguration) as TLeaderboardConfiguration;
end;

Function TLeaderboardConfigurationsResource.Insert(applicationId: string; aLeaderboardConfiguration : TLeaderboardConfiguration) : TLeaderboardConfiguration;

Const
  _HTTPMethod = 'POST';
  _Path       = 'applications/{applicationId}/leaderboards';
  _Methodid   = 'gamesConfiguration.leaderboardConfigurations.insert';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['applicationId',applicationId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLeaderboardConfiguration,TLeaderboardConfiguration) as TLeaderboardConfiguration;
end;

Function TLeaderboardConfigurationsResource.List(applicationId: string; AQuery : string = '') : TLeaderboardConfigurationListResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'applications/{applicationId}/leaderboards';
  _Methodid   = 'gamesConfiguration.leaderboardConfigurations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['applicationId',applicationId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TLeaderboardConfigurationListResponse) as TLeaderboardConfigurationListResponse;
end;


Function TLeaderboardConfigurationsResource.List(applicationId: string; AQuery : TLeaderboardConfigurationslistOptions) : TLeaderboardConfigurationListResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'maxResults',AQuery.maxResults);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(applicationId,_Q);
end;

Function TLeaderboardConfigurationsResource.Patch(leaderboardId: string; aLeaderboardConfiguration : TLeaderboardConfiguration) : TLeaderboardConfiguration;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'leaderboards/{leaderboardId}';
  _Methodid   = 'gamesConfiguration.leaderboardConfigurations.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['leaderboardId',leaderboardId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLeaderboardConfiguration,TLeaderboardConfiguration) as TLeaderboardConfiguration;
end;

Function TLeaderboardConfigurationsResource.Update(leaderboardId: string; aLeaderboardConfiguration : TLeaderboardConfiguration) : TLeaderboardConfiguration;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'leaderboards/{leaderboardId}';
  _Methodid   = 'gamesConfiguration.leaderboardConfigurations.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['leaderboardId',leaderboardId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLeaderboardConfiguration,TLeaderboardConfiguration) as TLeaderboardConfiguration;
end;



{ --------------------------------------------------------------------
  TGamesConfigurationAPI
  --------------------------------------------------------------------}

Class Function TGamesConfigurationAPI.APIName : String;

begin
  Result:='gamesConfiguration';
end;

Class Function TGamesConfigurationAPI.APIVersion : String;

begin
  Result:='v1configuration';
end;

Class Function TGamesConfigurationAPI.APIRevision : String;

begin
  Result:='20150421';
end;

Class Function TGamesConfigurationAPI.APIID : String;

begin
  Result:='gamesConfiguration:v1configuration';
end;

Class Function TGamesConfigurationAPI.APITitle : String;

begin
  Result:='Google Play Game Services Publishing API';
end;

Class Function TGamesConfigurationAPI.APIDescription : String;

begin
  Result:='The Publishing API for Google Play Game Services.';
end;

Class Function TGamesConfigurationAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TGamesConfigurationAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TGamesConfigurationAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TGamesConfigurationAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TGamesConfigurationAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/games/services';
end;

Class Function TGamesConfigurationAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TGamesConfigurationAPI.APIbasePath : string;

begin
  Result:='/games/v1configuration/';
end;

Class Function TGamesConfigurationAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/games/v1configuration/';
end;

Class Function TGamesConfigurationAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TGamesConfigurationAPI.APIservicePath : string;

begin
  Result:='games/v1configuration/';
end;

Class Function TGamesConfigurationAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TGamesConfigurationAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/androidpublisher';
  Result[0].Description:='View and manage your Google Play Developer account';
  
end;

Class Function TGamesConfigurationAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TGamesConfigurationAPI.RegisterAPIResources;

begin
  TAchievementConfiguration.RegisterObject;
  TAchievementConfigurationDetail.RegisterObject;
  TAchievementConfigurationListResponse.RegisterObject;
  TAchievementConfigurationListResponseitems.RegisterObject;
  TGamesNumberAffixConfiguration.RegisterObject;
  TGamesNumberFormatConfiguration.RegisterObject;
  TImageConfiguration.RegisterObject;
  TLeaderboardConfiguration.RegisterObject;
  TLeaderboardConfigurationDetail.RegisterObject;
  TLeaderboardConfigurationListResponse.RegisterObject;
  TLeaderboardConfigurationListResponseitems.RegisterObject;
  TLocalizedString.RegisterObject;
  TLocalizedStringBundle.RegisterObject;
  TLocalizedStringBundletranslations.RegisterObject;
end;


Function TGamesConfigurationAPI.GetAchievementConfigurationsInstance : TAchievementConfigurationsResource;

begin
  if (FAchievementConfigurationsInstance=Nil) then
    FAchievementConfigurationsInstance:=CreateAchievementConfigurationsResource;
  Result:=FAchievementConfigurationsInstance;
end;

Function TGamesConfigurationAPI.CreateAchievementConfigurationsResource : TAchievementConfigurationsResource;

begin
  Result:=CreateAchievementConfigurationsResource(Self);
end;


Function TGamesConfigurationAPI.CreateAchievementConfigurationsResource(AOwner : TComponent) : TAchievementConfigurationsResource;

begin
  Result:=TAchievementConfigurationsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesConfigurationAPI.GetImageConfigurationsInstance : TImageConfigurationsResource;

begin
  if (FImageConfigurationsInstance=Nil) then
    FImageConfigurationsInstance:=CreateImageConfigurationsResource;
  Result:=FImageConfigurationsInstance;
end;

Function TGamesConfigurationAPI.CreateImageConfigurationsResource : TImageConfigurationsResource;

begin
  Result:=CreateImageConfigurationsResource(Self);
end;


Function TGamesConfigurationAPI.CreateImageConfigurationsResource(AOwner : TComponent) : TImageConfigurationsResource;

begin
  Result:=TImageConfigurationsResource.Create(AOwner);
  Result.API:=Self;
end;



Function TGamesConfigurationAPI.GetLeaderboardConfigurationsInstance : TLeaderboardConfigurationsResource;

begin
  if (FLeaderboardConfigurationsInstance=Nil) then
    FLeaderboardConfigurationsInstance:=CreateLeaderboardConfigurationsResource;
  Result:=FLeaderboardConfigurationsInstance;
end;

Function TGamesConfigurationAPI.CreateLeaderboardConfigurationsResource : TLeaderboardConfigurationsResource;

begin
  Result:=CreateLeaderboardConfigurationsResource(Self);
end;


Function TGamesConfigurationAPI.CreateLeaderboardConfigurationsResource(AOwner : TComponent) : TLeaderboardConfigurationsResource;

begin
  Result:=TLeaderboardConfigurationsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TGamesConfigurationAPI.RegisterAPI;
end.
