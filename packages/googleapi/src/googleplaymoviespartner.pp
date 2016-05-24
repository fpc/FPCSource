unit googleplaymoviespartner;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAvail = Class;
  TListAvailsResponse = Class;
  TOrder = Class;
  TListOrdersResponse = Class;
  TExperienceLocale = Class;
  TListExperienceLocalesResponse = Class;
  T_Component = Class;
  TListComponentsResponse = Class;
  TStoreInfo = Class;
  TListStoreInfosResponse = Class;
  TAvailArray = Array of TAvail;
  TListAvailsResponseArray = Array of TListAvailsResponse;
  TOrderArray = Array of TOrder;
  TListOrdersResponseArray = Array of TListOrdersResponse;
  TExperienceLocaleArray = Array of TExperienceLocale;
  TListExperienceLocalesResponseArray = Array of TListExperienceLocalesResponse;
  T_ComponentArray = Array of T_Component;
  TListComponentsResponseArray = Array of TListComponentsResponse;
  TStoreInfoArray = Array of TStoreInfo;
  TListStoreInfosResponseArray = Array of TListStoreInfosResponse;
  //Anonymous types, using auto-generated names
  TListAvailsResponseTypeavailsArray = Array of TAvail;
  TListOrdersResponseTypeordersArray = Array of TOrder;
  TListExperienceLocalesResponseTypeexperienceLocalesArray = Array of TExperienceLocale;
  TListComponentsResponseTypecomponentsArray = Array of T_Component;
  TListStoreInfosResponseTypestoreInfosArray = Array of TStoreInfo;
  
  { --------------------------------------------------------------------
    TAvail
    --------------------------------------------------------------------}
  
  TAvail = Class(TGoogleBaseObject)
  Private
    FavailId : String;
    FdisplayName : String;
    FstoreLanguage : String;
    Fterritory : String;
    FworkType : String;
    FseriesTitleInternalAlias : String;
    FseasonNumber : String;
    FepisodeNumber : String;
    FseasonTitleInternalAlias : String;
    FepisodeTitleInternalAlias : String;
    FtitleInternalAlias : String;
    FlicenseType : String;
    FformatProfile : String;
    Fstart : String;
    F_end : String;
    FpriceType : String;
    FpriceValue : String;
    FcontentId : String;
    FproductId : String;
    FencodeId : String;
    FseriesAltId : String;
    FseasonAltId : String;
    FepisodeAltId : String;
    FaltId : String;
    FsuppressionLiftDate : String;
    FreleaseDate : String;
    FratingSystem : String;
    FratingValue : String;
    FratingReason : String;
    FcaptionIncluded : boolean;
    FcaptionExemption : String;
    FvideoId : String;
    FpphNames : TStringArray;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetavailId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstoreLanguage(AIndex : Integer; const AValue : String); virtual;
    Procedure Setterritory(AIndex : Integer; const AValue : String); virtual;
    Procedure SetworkType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetseriesTitleInternalAlias(AIndex : Integer; const AValue : String); virtual;
    Procedure SetseasonNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetepisodeNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetseasonTitleInternalAlias(AIndex : Integer; const AValue : String); virtual;
    Procedure SetepisodeTitleInternalAlias(AIndex : Integer; const AValue : String); virtual;
    Procedure SettitleInternalAlias(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlicenseType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetformatProfile(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpriceType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpriceValue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcontentId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproductId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetencodeId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetseriesAltId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetseasonAltId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetepisodeAltId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetaltId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsuppressionLiftDate(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreleaseDate(AIndex : Integer; const AValue : String); virtual;
    Procedure SetratingSystem(AIndex : Integer; const AValue : String); virtual;
    Procedure SetratingValue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetratingReason(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcaptionIncluded(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetcaptionExemption(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvideoId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpphNames(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property availId : String Index 0 Read FavailId Write SetavailId;
    Property displayName : String Index 8 Read FdisplayName Write SetdisplayName;
    Property storeLanguage : String Index 16 Read FstoreLanguage Write SetstoreLanguage;
    Property territory : String Index 24 Read Fterritory Write Setterritory;
    Property workType : String Index 32 Read FworkType Write SetworkType;
    Property seriesTitleInternalAlias : String Index 40 Read FseriesTitleInternalAlias Write SetseriesTitleInternalAlias;
    Property seasonNumber : String Index 48 Read FseasonNumber Write SetseasonNumber;
    Property episodeNumber : String Index 56 Read FepisodeNumber Write SetepisodeNumber;
    Property seasonTitleInternalAlias : String Index 64 Read FseasonTitleInternalAlias Write SetseasonTitleInternalAlias;
    Property episodeTitleInternalAlias : String Index 72 Read FepisodeTitleInternalAlias Write SetepisodeTitleInternalAlias;
    Property titleInternalAlias : String Index 80 Read FtitleInternalAlias Write SettitleInternalAlias;
    Property licenseType : String Index 88 Read FlicenseType Write SetlicenseType;
    Property formatProfile : String Index 96 Read FformatProfile Write SetformatProfile;
    Property start : String Index 104 Read Fstart Write Setstart;
    Property _end : String Index 112 Read F_end Write Set_end;
    Property priceType : String Index 120 Read FpriceType Write SetpriceType;
    Property priceValue : String Index 128 Read FpriceValue Write SetpriceValue;
    Property contentId : String Index 136 Read FcontentId Write SetcontentId;
    Property productId : String Index 144 Read FproductId Write SetproductId;
    Property encodeId : String Index 152 Read FencodeId Write SetencodeId;
    Property seriesAltId : String Index 160 Read FseriesAltId Write SetseriesAltId;
    Property seasonAltId : String Index 168 Read FseasonAltId Write SetseasonAltId;
    Property episodeAltId : String Index 176 Read FepisodeAltId Write SetepisodeAltId;
    Property altId : String Index 184 Read FaltId Write SetaltId;
    Property suppressionLiftDate : String Index 192 Read FsuppressionLiftDate Write SetsuppressionLiftDate;
    Property releaseDate : String Index 200 Read FreleaseDate Write SetreleaseDate;
    Property ratingSystem : String Index 208 Read FratingSystem Write SetratingSystem;
    Property ratingValue : String Index 216 Read FratingValue Write SetratingValue;
    Property ratingReason : String Index 224 Read FratingReason Write SetratingReason;
    Property captionIncluded : boolean Index 232 Read FcaptionIncluded Write SetcaptionIncluded;
    Property captionExemption : String Index 240 Read FcaptionExemption Write SetcaptionExemption;
    Property videoId : String Index 248 Read FvideoId Write SetvideoId;
    Property pphNames : TStringArray Index 256 Read FpphNames Write SetpphNames;
  end;
  TAvailClass = Class of TAvail;
  
  { --------------------------------------------------------------------
    TListAvailsResponse
    --------------------------------------------------------------------}
  
  TListAvailsResponse = Class(TGoogleBaseObject)
  Private
    Favails : TListAvailsResponseTypeavailsArray;
    FnextPageToken : String;
    FtotalSize : integer;
  Protected
    //Property setters
    Procedure Setavails(AIndex : Integer; const AValue : TListAvailsResponseTypeavailsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalSize(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property avails : TListAvailsResponseTypeavailsArray Index 0 Read Favails Write Setavails;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property totalSize : integer Index 16 Read FtotalSize Write SettotalSize;
  end;
  TListAvailsResponseClass = Class of TListAvailsResponse;
  
  { --------------------------------------------------------------------
    TOrder
    --------------------------------------------------------------------}
  
  TOrder = Class(TGoogleBaseObject)
  Private
    ForderId : String;
    FcustomId : String;
    FvideoId : String;
    Fcountries : TStringArray;
    F_type : String;
    Fname : String;
    FepisodeName : String;
    FseasonName : String;
    FshowName : String;
    Fstatus : String;
    FstatusDetail : String;
    FrejectionNote : String;
    ForderedTime : String;
    FapprovedTime : String;
    FreceivedTime : String;
    FearliestAvailStartTime : String;
    Fpriority : double;
    FlegacyPriority : String;
    FchannelId : String;
    FchannelName : String;
    FstudioName : String;
    FpphName : String;
    FnormalizedPriority : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetorderId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcustomId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvideoId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcountries(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetepisodeName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetseasonName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetshowName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstatusDetail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrejectionNote(AIndex : Integer; const AValue : String); virtual;
    Procedure SetorderedTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetapprovedTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreceivedTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetearliestAvailStartTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpriority(AIndex : Integer; const AValue : double); virtual;
    Procedure SetlegacyPriority(AIndex : Integer; const AValue : String); virtual;
    Procedure SetchannelId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetchannelName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstudioName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpphName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnormalizedPriority(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property orderId : String Index 0 Read ForderId Write SetorderId;
    Property customId : String Index 8 Read FcustomId Write SetcustomId;
    Property videoId : String Index 16 Read FvideoId Write SetvideoId;
    Property countries : TStringArray Index 24 Read Fcountries Write Setcountries;
    Property _type : String Index 32 Read F_type Write Set_type;
    Property name : String Index 40 Read Fname Write Setname;
    Property episodeName : String Index 48 Read FepisodeName Write SetepisodeName;
    Property seasonName : String Index 56 Read FseasonName Write SetseasonName;
    Property showName : String Index 64 Read FshowName Write SetshowName;
    Property status : String Index 72 Read Fstatus Write Setstatus;
    Property statusDetail : String Index 80 Read FstatusDetail Write SetstatusDetail;
    Property rejectionNote : String Index 88 Read FrejectionNote Write SetrejectionNote;
    Property orderedTime : String Index 96 Read ForderedTime Write SetorderedTime;
    Property approvedTime : String Index 104 Read FapprovedTime Write SetapprovedTime;
    Property receivedTime : String Index 112 Read FreceivedTime Write SetreceivedTime;
    Property earliestAvailStartTime : String Index 120 Read FearliestAvailStartTime Write SetearliestAvailStartTime;
    Property priority : double Index 128 Read Fpriority Write Setpriority;
    Property legacyPriority : String Index 136 Read FlegacyPriority Write SetlegacyPriority;
    Property channelId : String Index 144 Read FchannelId Write SetchannelId;
    Property channelName : String Index 152 Read FchannelName Write SetchannelName;
    Property studioName : String Index 160 Read FstudioName Write SetstudioName;
    Property pphName : String Index 168 Read FpphName Write SetpphName;
    Property normalizedPriority : String Index 176 Read FnormalizedPriority Write SetnormalizedPriority;
  end;
  TOrderClass = Class of TOrder;
  
  { --------------------------------------------------------------------
    TListOrdersResponse
    --------------------------------------------------------------------}
  
  TListOrdersResponse = Class(TGoogleBaseObject)
  Private
    Forders : TListOrdersResponseTypeordersArray;
    FnextPageToken : String;
    FtotalSize : integer;
  Protected
    //Property setters
    Procedure Setorders(AIndex : Integer; const AValue : TListOrdersResponseTypeordersArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalSize(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property orders : TListOrdersResponseTypeordersArray Index 0 Read Forders Write Setorders;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property totalSize : integer Index 16 Read FtotalSize Write SettotalSize;
  end;
  TListOrdersResponseClass = Class of TListOrdersResponse;
  
  { --------------------------------------------------------------------
    TExperienceLocale
    --------------------------------------------------------------------}
  
  TExperienceLocale = Class(TGoogleBaseObject)
  Private
    FelId : String;
    Fcountry : String;
    Flanguage : String;
    FvideoId : String;
    FtrailerId : String;
    FtitleLevelEidr : String;
    FeditLevelEidr : String;
    FaltCutId : String;
    FcustomIds : TStringArray;
    FpresentationId : String;
    FinventoryId : String;
    FplayableSequenceId : String;
    F_type : String;
    Fname : String;
    Fstatus : String;
    Fpriority : double;
    FcreatedTime : String;
    FapprovedTime : String;
    FearliestAvailStartTime : String;
    FchannelId : String;
    FstudioName : String;
    FpphNames : TStringArray;
    FnormalizedPriority : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetelId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcountry(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlanguage(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvideoId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettrailerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettitleLevelEidr(AIndex : Integer; const AValue : String); virtual;
    Procedure SeteditLevelEidr(AIndex : Integer; const AValue : String); virtual;
    Procedure SetaltCutId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcustomIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetpresentationId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinventoryId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetplayableSequenceId(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : String); virtual;
    Procedure Setpriority(AIndex : Integer; const AValue : double); virtual;
    Procedure SetcreatedTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetapprovedTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetearliestAvailStartTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetchannelId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstudioName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpphNames(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetnormalizedPriority(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property elId : String Index 0 Read FelId Write SetelId;
    Property country : String Index 8 Read Fcountry Write Setcountry;
    Property language : String Index 16 Read Flanguage Write Setlanguage;
    Property videoId : String Index 24 Read FvideoId Write SetvideoId;
    Property trailerId : String Index 32 Read FtrailerId Write SettrailerId;
    Property titleLevelEidr : String Index 40 Read FtitleLevelEidr Write SettitleLevelEidr;
    Property editLevelEidr : String Index 48 Read FeditLevelEidr Write SeteditLevelEidr;
    Property altCutId : String Index 56 Read FaltCutId Write SetaltCutId;
    Property customIds : TStringArray Index 64 Read FcustomIds Write SetcustomIds;
    Property presentationId : String Index 72 Read FpresentationId Write SetpresentationId;
    Property inventoryId : String Index 80 Read FinventoryId Write SetinventoryId;
    Property playableSequenceId : String Index 88 Read FplayableSequenceId Write SetplayableSequenceId;
    Property _type : String Index 96 Read F_type Write Set_type;
    Property name : String Index 104 Read Fname Write Setname;
    Property status : String Index 112 Read Fstatus Write Setstatus;
    Property priority : double Index 120 Read Fpriority Write Setpriority;
    Property createdTime : String Index 128 Read FcreatedTime Write SetcreatedTime;
    Property approvedTime : String Index 136 Read FapprovedTime Write SetapprovedTime;
    Property earliestAvailStartTime : String Index 144 Read FearliestAvailStartTime Write SetearliestAvailStartTime;
    Property channelId : String Index 152 Read FchannelId Write SetchannelId;
    Property studioName : String Index 160 Read FstudioName Write SetstudioName;
    Property pphNames : TStringArray Index 168 Read FpphNames Write SetpphNames;
    Property normalizedPriority : String Index 176 Read FnormalizedPriority Write SetnormalizedPriority;
  end;
  TExperienceLocaleClass = Class of TExperienceLocale;
  
  { --------------------------------------------------------------------
    TListExperienceLocalesResponse
    --------------------------------------------------------------------}
  
  TListExperienceLocalesResponse = Class(TGoogleBaseObject)
  Private
    FexperienceLocales : TListExperienceLocalesResponseTypeexperienceLocalesArray;
    FnextPageToken : String;
    FtotalSize : integer;
  Protected
    //Property setters
    Procedure SetexperienceLocales(AIndex : Integer; const AValue : TListExperienceLocalesResponseTypeexperienceLocalesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalSize(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property experienceLocales : TListExperienceLocalesResponseTypeexperienceLocalesArray Index 0 Read FexperienceLocales Write SetexperienceLocales;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property totalSize : integer Index 16 Read FtotalSize Write SettotalSize;
  end;
  TListExperienceLocalesResponseClass = Class of TListExperienceLocalesResponse;
  
  { --------------------------------------------------------------------
    T_Component
    --------------------------------------------------------------------}
  
  T_Component = Class(TGoogleBaseObject)
  Private
    FcomponentId : String;
    F_type : String;
    FelIds : TStringArray;
    Flanguage : String;
    FtitleLevelEidrs : TStringArray;
    FeditLevelEidrs : TStringArray;
    FaltCutIds : TStringArray;
    FcustomIds : TStringArray;
    FcomponentDetailType : String;
    FplayableUnitType : String;
    Fstatus : String;
    FstatusDetail : String;
    FrejectionNote : String;
    FapprovedTime : String;
    Fname : String;
    FprocessingErrors : TStringArray;
    Fpriority : double;
    FreceivedTime : String;
    Ffilename : String;
    FstudioName : String;
    FpphName : String;
    FnormalizedPriority : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetcomponentId(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetelIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setlanguage(AIndex : Integer; const AValue : String); virtual;
    Procedure SettitleLevelEidrs(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SeteditLevelEidrs(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetaltCutIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetcustomIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetcomponentDetailType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetplayableUnitType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstatusDetail(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrejectionNote(AIndex : Integer; const AValue : String); virtual;
    Procedure SetapprovedTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprocessingErrors(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setpriority(AIndex : Integer; const AValue : double); virtual;
    Procedure SetreceivedTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfilename(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstudioName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpphName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnormalizedPriority(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property componentId : String Index 0 Read FcomponentId Write SetcomponentId;
    Property _type : String Index 8 Read F_type Write Set_type;
    Property elIds : TStringArray Index 16 Read FelIds Write SetelIds;
    Property language : String Index 24 Read Flanguage Write Setlanguage;
    Property titleLevelEidrs : TStringArray Index 32 Read FtitleLevelEidrs Write SettitleLevelEidrs;
    Property editLevelEidrs : TStringArray Index 40 Read FeditLevelEidrs Write SeteditLevelEidrs;
    Property altCutIds : TStringArray Index 48 Read FaltCutIds Write SetaltCutIds;
    Property customIds : TStringArray Index 56 Read FcustomIds Write SetcustomIds;
    Property componentDetailType : String Index 64 Read FcomponentDetailType Write SetcomponentDetailType;
    Property playableUnitType : String Index 72 Read FplayableUnitType Write SetplayableUnitType;
    Property status : String Index 80 Read Fstatus Write Setstatus;
    Property statusDetail : String Index 88 Read FstatusDetail Write SetstatusDetail;
    Property rejectionNote : String Index 96 Read FrejectionNote Write SetrejectionNote;
    Property approvedTime : String Index 104 Read FapprovedTime Write SetapprovedTime;
    Property name : String Index 112 Read Fname Write Setname;
    Property processingErrors : TStringArray Index 120 Read FprocessingErrors Write SetprocessingErrors;
    Property priority : double Index 128 Read Fpriority Write Setpriority;
    Property receivedTime : String Index 136 Read FreceivedTime Write SetreceivedTime;
    Property filename : String Index 144 Read Ffilename Write Setfilename;
    Property studioName : String Index 152 Read FstudioName Write SetstudioName;
    Property pphName : String Index 160 Read FpphName Write SetpphName;
    Property normalizedPriority : String Index 168 Read FnormalizedPriority Write SetnormalizedPriority;
  end;
  T_ComponentClass = Class of T_Component;
  
  { --------------------------------------------------------------------
    TListComponentsResponse
    --------------------------------------------------------------------}
  
  TListComponentsResponse = Class(TGoogleBaseObject)
  Private
    Fcomponents : TListComponentsResponseTypecomponentsArray;
    FnextPageToken : String;
    FtotalSize : integer;
  Protected
    //Property setters
    Procedure Setcomponents(AIndex : Integer; const AValue : TListComponentsResponseTypecomponentsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalSize(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property components : TListComponentsResponseTypecomponentsArray Index 0 Read Fcomponents Write Setcomponents;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property totalSize : integer Index 16 Read FtotalSize Write SettotalSize;
  end;
  TListComponentsResponseClass = Class of TListComponentsResponse;
  
  { --------------------------------------------------------------------
    TStoreInfo
    --------------------------------------------------------------------}
  
  TStoreInfo = Class(TGoogleBaseObject)
  Private
    FvideoId : String;
    FseasonId : String;
    FshowId : String;
    Fcountry : String;
    FliveTime : String;
    F_type : String;
    Fname : String;
    FtitleLevelEidr : String;
    FeditLevelEidr : String;
    FseasonName : String;
    FshowName : String;
    FseasonNumber : String;
    FepisodeNumber : String;
    FhasSdOffer : boolean;
    FhasHdOffer : boolean;
    FhasVodOffer : boolean;
    FhasEstOffer : boolean;
    FhasAudio51 : boolean;
    FaudioTracks : TStringArray;
    Fsubtitles : TStringArray;
    FhasInfoCards : boolean;
    Fmid : String;
    FtrailerId : String;
    FstudioName : String;
    FpphNames : TStringArray;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetvideoId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetseasonId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetshowId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcountry(AIndex : Integer; const AValue : String); virtual;
    Procedure SetliveTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SettitleLevelEidr(AIndex : Integer; const AValue : String); virtual;
    Procedure SeteditLevelEidr(AIndex : Integer; const AValue : String); virtual;
    Procedure SetseasonName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetshowName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetseasonNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SetepisodeNumber(AIndex : Integer; const AValue : String); virtual;
    Procedure SethasSdOffer(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SethasHdOffer(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SethasVodOffer(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SethasEstOffer(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SethasAudio51(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetaudioTracks(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setsubtitles(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SethasInfoCards(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setmid(AIndex : Integer; const AValue : String); virtual;
    Procedure SettrailerId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstudioName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpphNames(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property videoId : String Index 0 Read FvideoId Write SetvideoId;
    Property seasonId : String Index 8 Read FseasonId Write SetseasonId;
    Property showId : String Index 16 Read FshowId Write SetshowId;
    Property country : String Index 24 Read Fcountry Write Setcountry;
    Property liveTime : String Index 32 Read FliveTime Write SetliveTime;
    Property _type : String Index 40 Read F_type Write Set_type;
    Property name : String Index 48 Read Fname Write Setname;
    Property titleLevelEidr : String Index 56 Read FtitleLevelEidr Write SettitleLevelEidr;
    Property editLevelEidr : String Index 64 Read FeditLevelEidr Write SeteditLevelEidr;
    Property seasonName : String Index 72 Read FseasonName Write SetseasonName;
    Property showName : String Index 80 Read FshowName Write SetshowName;
    Property seasonNumber : String Index 88 Read FseasonNumber Write SetseasonNumber;
    Property episodeNumber : String Index 96 Read FepisodeNumber Write SetepisodeNumber;
    Property hasSdOffer : boolean Index 104 Read FhasSdOffer Write SethasSdOffer;
    Property hasHdOffer : boolean Index 112 Read FhasHdOffer Write SethasHdOffer;
    Property hasVodOffer : boolean Index 120 Read FhasVodOffer Write SethasVodOffer;
    Property hasEstOffer : boolean Index 128 Read FhasEstOffer Write SethasEstOffer;
    Property hasAudio51 : boolean Index 136 Read FhasAudio51 Write SethasAudio51;
    Property audioTracks : TStringArray Index 144 Read FaudioTracks Write SetaudioTracks;
    Property subtitles : TStringArray Index 152 Read Fsubtitles Write Setsubtitles;
    Property hasInfoCards : boolean Index 160 Read FhasInfoCards Write SethasInfoCards;
    Property mid : String Index 168 Read Fmid Write Setmid;
    Property trailerId : String Index 176 Read FtrailerId Write SettrailerId;
    Property studioName : String Index 184 Read FstudioName Write SetstudioName;
    Property pphNames : TStringArray Index 192 Read FpphNames Write SetpphNames;
  end;
  TStoreInfoClass = Class of TStoreInfo;
  
  { --------------------------------------------------------------------
    TListStoreInfosResponse
    --------------------------------------------------------------------}
  
  TListStoreInfosResponse = Class(TGoogleBaseObject)
  Private
    FstoreInfos : TListStoreInfosResponseTypestoreInfosArray;
    FnextPageToken : String;
    FtotalSize : integer;
  Protected
    //Property setters
    Procedure SetstoreInfos(AIndex : Integer; const AValue : TListStoreInfosResponseTypestoreInfosArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalSize(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property storeInfos : TListStoreInfosResponseTypestoreInfosArray Index 0 Read FstoreInfos Write SetstoreInfos;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property totalSize : integer Index 16 Read FtotalSize Write SettotalSize;
  end;
  TListStoreInfosResponseClass = Class of TListStoreInfosResponse;
  
  { --------------------------------------------------------------------
    TAccountsAvailsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsAvailsResource, method List
  
  TAccountsAvailsListOptions = Record
    pageSize : integer;
    pageToken : String;
    pphNames : String;
    studioNames : String;
    title : String;
    territories : String;
    altId : String;
    videoIds : String;
    altIds : String;
  end;
  
  TAccountsAvailsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; availId: string) : TAvail;
    Function List(accountId: string; AQuery : string  = '') : TListAvailsResponse;
    Function List(accountId: string; AQuery : TAccountsAvailslistOptions) : TListAvailsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsOrdersResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsOrdersResource, method List
  
  TAccountsOrdersListOptions = Record
    pageSize : integer;
    pageToken : String;
    pphNames : String;
    studioNames : String;
    _name : String;
    status : String;
    customId : String;
    videoIds : String;
  end;
  
  TAccountsOrdersResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; orderId: string) : TOrder;
    Function List(accountId: string; AQuery : string  = '') : TListOrdersResponse;
    Function List(accountId: string; AQuery : TAccountsOrderslistOptions) : TListOrdersResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsExperienceLocalesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsExperienceLocalesResource, method List
  
  TAccountsExperienceLocalesListOptions = Record
    pageSize : integer;
    pageToken : String;
    pphNames : String;
    studioNames : String;
    titleLevelEidr : String;
    editLevelEidr : String;
    status : String;
    customId : String;
    altCutId : String;
  end;
  
  TAccountsExperienceLocalesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; elId: string) : TExperienceLocale;
    Function List(accountId: string; AQuery : string  = '') : TListExperienceLocalesResponse;
    Function List(accountId: string; AQuery : TAccountsExperienceLocaleslistOptions) : TListExperienceLocalesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsComponentsTypeResource
    --------------------------------------------------------------------}
  
  TAccountsComponentsTypeResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; componentId: string; _type: string) : T_Component;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsComponentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsComponentsResource, method List
  
  TAccountsComponentsListOptions = Record
    pageSize : integer;
    pageToken : String;
    pphNames : String;
    studioNames : String;
    titleLevelEidr : String;
    editLevelEidr : String;
    status : String;
    customId : String;
    inventoryId : String;
    presentationId : String;
    playableSequenceId : String;
    elId : String;
    altCutId : String;
    filename : String;
  end;
  
  TAccountsComponentsResource = Class(TGoogleResource)
  Private
    FTypeInstance : TAccountsComponentsTypeResource;
    Function GetTypeInstance : TAccountsComponentsTypeResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(accountId: string; AQuery : string  = '') : TListComponentsResponse;
    Function List(accountId: string; AQuery : TAccountsComponentslistOptions) : TListComponentsResponse;
    Function CreateTypeResource(AOwner : TComponent) : TAccountsComponentsTypeResource;virtual;overload;
    Function CreateTypeResource : TAccountsComponentsTypeResource;virtual;overload;
    Property TypeResource : TAccountsComponentsTypeResource Read GetTypeInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsStoreInfosCountryResource
    --------------------------------------------------------------------}
  
  TAccountsStoreInfosCountryResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(accountId: string; videoId: string; country: string) : TStoreInfo;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsStoreInfosResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAccountsStoreInfosResource, method List
  
  TAccountsStoreInfosListOptions = Record
    pageSize : integer;
    pageToken : String;
    pphNames : String;
    studioNames : String;
    videoId : String;
    countries : String;
    _name : String;
    videoIds : String;
    mids : String;
    seasonIds : String;
  end;
  
  TAccountsStoreInfosResource = Class(TGoogleResource)
  Private
    FCountryInstance : TAccountsStoreInfosCountryResource;
    Function GetCountryInstance : TAccountsStoreInfosCountryResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(accountId: string; AQuery : string  = '') : TListStoreInfosResponse;
    Function List(accountId: string; AQuery : TAccountsStoreInfoslistOptions) : TListStoreInfosResponse;
    Function CreateCountryResource(AOwner : TComponent) : TAccountsStoreInfosCountryResource;virtual;overload;
    Function CreateCountryResource : TAccountsStoreInfosCountryResource;virtual;overload;
    Property CountryResource : TAccountsStoreInfosCountryResource Read GetCountryInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TAccountsResource
    --------------------------------------------------------------------}
  
  TAccountsResource = Class(TGoogleResource)
  Private
    FAvailsInstance : TAccountsAvailsResource;
    FOrdersInstance : TAccountsOrdersResource;
    FExperienceLocalesInstance : TAccountsExperienceLocalesResource;
    FComponentsTypeInstance : TAccountsComponentsTypeResource;
    FComponentsInstance : TAccountsComponentsResource;
    FStoreInfosCountryInstance : TAccountsStoreInfosCountryResource;
    FStoreInfosInstance : TAccountsStoreInfosResource;
    Function GetAvailsInstance : TAccountsAvailsResource;virtual;
    Function GetOrdersInstance : TAccountsOrdersResource;virtual;
    Function GetExperienceLocalesInstance : TAccountsExperienceLocalesResource;virtual;
    Function GetComponentsTypeInstance : TAccountsComponentsTypeResource;virtual;
    Function GetComponentsInstance : TAccountsComponentsResource;virtual;
    Function GetStoreInfosCountryInstance : TAccountsStoreInfosCountryResource;virtual;
    Function GetStoreInfosInstance : TAccountsStoreInfosResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateAvailsResource(AOwner : TComponent) : TAccountsAvailsResource;virtual;overload;
    Function CreateAvailsResource : TAccountsAvailsResource;virtual;overload;
    Function CreateOrdersResource(AOwner : TComponent) : TAccountsOrdersResource;virtual;overload;
    Function CreateOrdersResource : TAccountsOrdersResource;virtual;overload;
    Function CreateExperienceLocalesResource(AOwner : TComponent) : TAccountsExperienceLocalesResource;virtual;overload;
    Function CreateExperienceLocalesResource : TAccountsExperienceLocalesResource;virtual;overload;
    Function CreateComponentsTypeResource(AOwner : TComponent) : TAccountsComponentsTypeResource;virtual;overload;
    Function CreateComponentsTypeResource : TAccountsComponentsTypeResource;virtual;overload;
    Function CreateComponentsResource(AOwner : TComponent) : TAccountsComponentsResource;virtual;overload;
    Function CreateComponentsResource : TAccountsComponentsResource;virtual;overload;
    Function CreateStoreInfosCountryResource(AOwner : TComponent) : TAccountsStoreInfosCountryResource;virtual;overload;
    Function CreateStoreInfosCountryResource : TAccountsStoreInfosCountryResource;virtual;overload;
    Function CreateStoreInfosResource(AOwner : TComponent) : TAccountsStoreInfosResource;virtual;overload;
    Function CreateStoreInfosResource : TAccountsStoreInfosResource;virtual;overload;
    Property AvailsResource : TAccountsAvailsResource Read GetAvailsInstance;
    Property OrdersResource : TAccountsOrdersResource Read GetOrdersInstance;
    Property ExperienceLocalesResource : TAccountsExperienceLocalesResource Read GetExperienceLocalesInstance;
    Property ComponentsTypeResource : TAccountsComponentsTypeResource Read GetComponentsTypeInstance;
    Property ComponentsResource : TAccountsComponentsResource Read GetComponentsInstance;
    Property StoreInfosCountryResource : TAccountsStoreInfosCountryResource Read GetStoreInfosCountryInstance;
    Property StoreInfosResource : TAccountsStoreInfosResource Read GetStoreInfosInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TPlaymoviespartnerAPI
    --------------------------------------------------------------------}
  
  TPlaymoviespartnerAPI = Class(TGoogleAPI)
  Private
    FAccountsAvailsInstance : TAccountsAvailsResource;
    FAccountsOrdersInstance : TAccountsOrdersResource;
    FAccountsExperienceLocalesInstance : TAccountsExperienceLocalesResource;
    FAccountsComponentsTypeInstance : TAccountsComponentsTypeResource;
    FAccountsComponentsInstance : TAccountsComponentsResource;
    FAccountsStoreInfosCountryInstance : TAccountsStoreInfosCountryResource;
    FAccountsStoreInfosInstance : TAccountsStoreInfosResource;
    FAccountsInstance : TAccountsResource;
    Function GetAccountsAvailsInstance : TAccountsAvailsResource;virtual;
    Function GetAccountsOrdersInstance : TAccountsOrdersResource;virtual;
    Function GetAccountsExperienceLocalesInstance : TAccountsExperienceLocalesResource;virtual;
    Function GetAccountsComponentsTypeInstance : TAccountsComponentsTypeResource;virtual;
    Function GetAccountsComponentsInstance : TAccountsComponentsResource;virtual;
    Function GetAccountsStoreInfosCountryInstance : TAccountsStoreInfosCountryResource;virtual;
    Function GetAccountsStoreInfosInstance : TAccountsStoreInfosResource;virtual;
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
    Function CreateAccountsAvailsResource(AOwner : TComponent) : TAccountsAvailsResource;virtual;overload;
    Function CreateAccountsAvailsResource : TAccountsAvailsResource;virtual;overload;
    Function CreateAccountsOrdersResource(AOwner : TComponent) : TAccountsOrdersResource;virtual;overload;
    Function CreateAccountsOrdersResource : TAccountsOrdersResource;virtual;overload;
    Function CreateAccountsExperienceLocalesResource(AOwner : TComponent) : TAccountsExperienceLocalesResource;virtual;overload;
    Function CreateAccountsExperienceLocalesResource : TAccountsExperienceLocalesResource;virtual;overload;
    Function CreateAccountsComponentsTypeResource(AOwner : TComponent) : TAccountsComponentsTypeResource;virtual;overload;
    Function CreateAccountsComponentsTypeResource : TAccountsComponentsTypeResource;virtual;overload;
    Function CreateAccountsComponentsResource(AOwner : TComponent) : TAccountsComponentsResource;virtual;overload;
    Function CreateAccountsComponentsResource : TAccountsComponentsResource;virtual;overload;
    Function CreateAccountsStoreInfosCountryResource(AOwner : TComponent) : TAccountsStoreInfosCountryResource;virtual;overload;
    Function CreateAccountsStoreInfosCountryResource : TAccountsStoreInfosCountryResource;virtual;overload;
    Function CreateAccountsStoreInfosResource(AOwner : TComponent) : TAccountsStoreInfosResource;virtual;overload;
    Function CreateAccountsStoreInfosResource : TAccountsStoreInfosResource;virtual;overload;
    Function CreateAccountsResource(AOwner : TComponent) : TAccountsResource;virtual;overload;
    Function CreateAccountsResource : TAccountsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AccountsAvailsResource : TAccountsAvailsResource Read GetAccountsAvailsInstance;
    Property AccountsOrdersResource : TAccountsOrdersResource Read GetAccountsOrdersInstance;
    Property AccountsExperienceLocalesResource : TAccountsExperienceLocalesResource Read GetAccountsExperienceLocalesInstance;
    Property AccountsComponentsTypeResource : TAccountsComponentsTypeResource Read GetAccountsComponentsTypeInstance;
    Property AccountsComponentsResource : TAccountsComponentsResource Read GetAccountsComponentsInstance;
    Property AccountsStoreInfosCountryResource : TAccountsStoreInfosCountryResource Read GetAccountsStoreInfosCountryInstance;
    Property AccountsStoreInfosResource : TAccountsStoreInfosResource Read GetAccountsStoreInfosInstance;
    Property AccountsResource : TAccountsResource Read GetAccountsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAvail
  --------------------------------------------------------------------}


Procedure TAvail.SetavailId(AIndex : Integer; const AValue : String); 

begin
  If (FavailId=AValue) then exit;
  FavailId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetstoreLanguage(AIndex : Integer; const AValue : String); 

begin
  If (FstoreLanguage=AValue) then exit;
  FstoreLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.Setterritory(AIndex : Integer; const AValue : String); 

begin
  If (Fterritory=AValue) then exit;
  Fterritory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetworkType(AIndex : Integer; const AValue : String); 

begin
  If (FworkType=AValue) then exit;
  FworkType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetseriesTitleInternalAlias(AIndex : Integer; const AValue : String); 

begin
  If (FseriesTitleInternalAlias=AValue) then exit;
  FseriesTitleInternalAlias:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetseasonNumber(AIndex : Integer; const AValue : String); 

begin
  If (FseasonNumber=AValue) then exit;
  FseasonNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetepisodeNumber(AIndex : Integer; const AValue : String); 

begin
  If (FepisodeNumber=AValue) then exit;
  FepisodeNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetseasonTitleInternalAlias(AIndex : Integer; const AValue : String); 

begin
  If (FseasonTitleInternalAlias=AValue) then exit;
  FseasonTitleInternalAlias:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetepisodeTitleInternalAlias(AIndex : Integer; const AValue : String); 

begin
  If (FepisodeTitleInternalAlias=AValue) then exit;
  FepisodeTitleInternalAlias:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SettitleInternalAlias(AIndex : Integer; const AValue : String); 

begin
  If (FtitleInternalAlias=AValue) then exit;
  FtitleInternalAlias:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetlicenseType(AIndex : Integer; const AValue : String); 

begin
  If (FlicenseType=AValue) then exit;
  FlicenseType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetformatProfile(AIndex : Integer; const AValue : String); 

begin
  If (FformatProfile=AValue) then exit;
  FformatProfile:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.Setstart(AIndex : Integer; const AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.Set_end(AIndex : Integer; const AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetpriceType(AIndex : Integer; const AValue : String); 

begin
  If (FpriceType=AValue) then exit;
  FpriceType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetpriceValue(AIndex : Integer; const AValue : String); 

begin
  If (FpriceValue=AValue) then exit;
  FpriceValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetcontentId(AIndex : Integer; const AValue : String); 

begin
  If (FcontentId=AValue) then exit;
  FcontentId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetproductId(AIndex : Integer; const AValue : String); 

begin
  If (FproductId=AValue) then exit;
  FproductId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetencodeId(AIndex : Integer; const AValue : String); 

begin
  If (FencodeId=AValue) then exit;
  FencodeId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetseriesAltId(AIndex : Integer; const AValue : String); 

begin
  If (FseriesAltId=AValue) then exit;
  FseriesAltId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetseasonAltId(AIndex : Integer; const AValue : String); 

begin
  If (FseasonAltId=AValue) then exit;
  FseasonAltId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetepisodeAltId(AIndex : Integer; const AValue : String); 

begin
  If (FepisodeAltId=AValue) then exit;
  FepisodeAltId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetaltId(AIndex : Integer; const AValue : String); 

begin
  If (FaltId=AValue) then exit;
  FaltId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetsuppressionLiftDate(AIndex : Integer; const AValue : String); 

begin
  If (FsuppressionLiftDate=AValue) then exit;
  FsuppressionLiftDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetreleaseDate(AIndex : Integer; const AValue : String); 

begin
  If (FreleaseDate=AValue) then exit;
  FreleaseDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetratingSystem(AIndex : Integer; const AValue : String); 

begin
  If (FratingSystem=AValue) then exit;
  FratingSystem:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetratingValue(AIndex : Integer; const AValue : String); 

begin
  If (FratingValue=AValue) then exit;
  FratingValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetratingReason(AIndex : Integer; const AValue : String); 

begin
  If (FratingReason=AValue) then exit;
  FratingReason:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetcaptionIncluded(AIndex : Integer; const AValue : boolean); 

begin
  If (FcaptionIncluded=AValue) then exit;
  FcaptionIncluded:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetcaptionExemption(AIndex : Integer; const AValue : String); 

begin
  If (FcaptionExemption=AValue) then exit;
  FcaptionExemption:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetvideoId(AIndex : Integer; const AValue : String); 

begin
  If (FvideoId=AValue) then exit;
  FvideoId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAvail.SetpphNames(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FpphNames=AValue) then exit;
  FpphNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAvail.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAvail.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'pphnames' : SetLength(FpphNames,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListAvailsResponse
  --------------------------------------------------------------------}


Procedure TListAvailsResponse.Setavails(AIndex : Integer; const AValue : TListAvailsResponseTypeavailsArray); 

begin
  If (Favails=AValue) then exit;
  Favails:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListAvailsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListAvailsResponse.SettotalSize(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalSize=AValue) then exit;
  FtotalSize:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListAvailsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'avails' : SetLength(Favails,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOrder
  --------------------------------------------------------------------}


Procedure TOrder.SetorderId(AIndex : Integer; const AValue : String); 

begin
  If (ForderId=AValue) then exit;
  ForderId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetcustomId(AIndex : Integer; const AValue : String); 

begin
  If (FcustomId=AValue) then exit;
  FcustomId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetvideoId(AIndex : Integer; const AValue : String); 

begin
  If (FvideoId=AValue) then exit;
  FvideoId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.Setcountries(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fcountries=AValue) then exit;
  Fcountries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetepisodeName(AIndex : Integer; const AValue : String); 

begin
  If (FepisodeName=AValue) then exit;
  FepisodeName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetseasonName(AIndex : Integer; const AValue : String); 

begin
  If (FseasonName=AValue) then exit;
  FseasonName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetshowName(AIndex : Integer; const AValue : String); 

begin
  If (FshowName=AValue) then exit;
  FshowName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.Setstatus(AIndex : Integer; const AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetstatusDetail(AIndex : Integer; const AValue : String); 

begin
  If (FstatusDetail=AValue) then exit;
  FstatusDetail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetrejectionNote(AIndex : Integer; const AValue : String); 

begin
  If (FrejectionNote=AValue) then exit;
  FrejectionNote:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetorderedTime(AIndex : Integer; const AValue : String); 

begin
  If (ForderedTime=AValue) then exit;
  ForderedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetapprovedTime(AIndex : Integer; const AValue : String); 

begin
  If (FapprovedTime=AValue) then exit;
  FapprovedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetreceivedTime(AIndex : Integer; const AValue : String); 

begin
  If (FreceivedTime=AValue) then exit;
  FreceivedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetearliestAvailStartTime(AIndex : Integer; const AValue : String); 

begin
  If (FearliestAvailStartTime=AValue) then exit;
  FearliestAvailStartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.Setpriority(AIndex : Integer; const AValue : double); 

begin
  If (Fpriority=AValue) then exit;
  Fpriority:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetlegacyPriority(AIndex : Integer; const AValue : String); 

begin
  If (FlegacyPriority=AValue) then exit;
  FlegacyPriority:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetchannelId(AIndex : Integer; const AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetchannelName(AIndex : Integer; const AValue : String); 

begin
  If (FchannelName=AValue) then exit;
  FchannelName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetstudioName(AIndex : Integer; const AValue : String); 

begin
  If (FstudioName=AValue) then exit;
  FstudioName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetpphName(AIndex : Integer; const AValue : String); 

begin
  If (FpphName=AValue) then exit;
  FpphName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrder.SetnormalizedPriority(AIndex : Integer; const AValue : String); 

begin
  If (FnormalizedPriority=AValue) then exit;
  FnormalizedPriority:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TOrder.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOrder.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'countries' : SetLength(Fcountries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListOrdersResponse
  --------------------------------------------------------------------}


Procedure TListOrdersResponse.Setorders(AIndex : Integer; const AValue : TListOrdersResponseTypeordersArray); 

begin
  If (Forders=AValue) then exit;
  Forders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListOrdersResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListOrdersResponse.SettotalSize(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalSize=AValue) then exit;
  FtotalSize:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListOrdersResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'orders' : SetLength(Forders,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TExperienceLocale
  --------------------------------------------------------------------}


Procedure TExperienceLocale.SetelId(AIndex : Integer; const AValue : String); 

begin
  If (FelId=AValue) then exit;
  FelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.Setcountry(AIndex : Integer; const AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.Setlanguage(AIndex : Integer; const AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.SetvideoId(AIndex : Integer; const AValue : String); 

begin
  If (FvideoId=AValue) then exit;
  FvideoId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.SettrailerId(AIndex : Integer; const AValue : String); 

begin
  If (FtrailerId=AValue) then exit;
  FtrailerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.SettitleLevelEidr(AIndex : Integer; const AValue : String); 

begin
  If (FtitleLevelEidr=AValue) then exit;
  FtitleLevelEidr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.SeteditLevelEidr(AIndex : Integer; const AValue : String); 

begin
  If (FeditLevelEidr=AValue) then exit;
  FeditLevelEidr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.SetaltCutId(AIndex : Integer; const AValue : String); 

begin
  If (FaltCutId=AValue) then exit;
  FaltCutId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.SetcustomIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FcustomIds=AValue) then exit;
  FcustomIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.SetpresentationId(AIndex : Integer; const AValue : String); 

begin
  If (FpresentationId=AValue) then exit;
  FpresentationId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.SetinventoryId(AIndex : Integer; const AValue : String); 

begin
  If (FinventoryId=AValue) then exit;
  FinventoryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.SetplayableSequenceId(AIndex : Integer; const AValue : String); 

begin
  If (FplayableSequenceId=AValue) then exit;
  FplayableSequenceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.Setstatus(AIndex : Integer; const AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.Setpriority(AIndex : Integer; const AValue : double); 

begin
  If (Fpriority=AValue) then exit;
  Fpriority:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.SetcreatedTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreatedTime=AValue) then exit;
  FcreatedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.SetapprovedTime(AIndex : Integer; const AValue : String); 

begin
  If (FapprovedTime=AValue) then exit;
  FapprovedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.SetearliestAvailStartTime(AIndex : Integer; const AValue : String); 

begin
  If (FearliestAvailStartTime=AValue) then exit;
  FearliestAvailStartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.SetchannelId(AIndex : Integer; const AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.SetstudioName(AIndex : Integer; const AValue : String); 

begin
  If (FstudioName=AValue) then exit;
  FstudioName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.SetpphNames(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FpphNames=AValue) then exit;
  FpphNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperienceLocale.SetnormalizedPriority(AIndex : Integer; const AValue : String); 

begin
  If (FnormalizedPriority=AValue) then exit;
  FnormalizedPriority:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TExperienceLocale.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TExperienceLocale.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'customids' : SetLength(FcustomIds,ALength);
  'pphnames' : SetLength(FpphNames,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListExperienceLocalesResponse
  --------------------------------------------------------------------}


Procedure TListExperienceLocalesResponse.SetexperienceLocales(AIndex : Integer; const AValue : TListExperienceLocalesResponseTypeexperienceLocalesArray); 

begin
  If (FexperienceLocales=AValue) then exit;
  FexperienceLocales:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListExperienceLocalesResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListExperienceLocalesResponse.SettotalSize(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalSize=AValue) then exit;
  FtotalSize:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListExperienceLocalesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'experiencelocales' : SetLength(FexperienceLocales,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  T_Component
  --------------------------------------------------------------------}


Procedure T_Component.SetcomponentId(AIndex : Integer; const AValue : String); 

begin
  If (FcomponentId=AValue) then exit;
  FcomponentId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.SetelIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FelIds=AValue) then exit;
  FelIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.Setlanguage(AIndex : Integer; const AValue : String); 

begin
  If (Flanguage=AValue) then exit;
  Flanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.SettitleLevelEidrs(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FtitleLevelEidrs=AValue) then exit;
  FtitleLevelEidrs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.SeteditLevelEidrs(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FeditLevelEidrs=AValue) then exit;
  FeditLevelEidrs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.SetaltCutIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FaltCutIds=AValue) then exit;
  FaltCutIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.SetcustomIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FcustomIds=AValue) then exit;
  FcustomIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.SetcomponentDetailType(AIndex : Integer; const AValue : String); 

begin
  If (FcomponentDetailType=AValue) then exit;
  FcomponentDetailType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.SetplayableUnitType(AIndex : Integer; const AValue : String); 

begin
  If (FplayableUnitType=AValue) then exit;
  FplayableUnitType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.Setstatus(AIndex : Integer; const AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.SetstatusDetail(AIndex : Integer; const AValue : String); 

begin
  If (FstatusDetail=AValue) then exit;
  FstatusDetail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.SetrejectionNote(AIndex : Integer; const AValue : String); 

begin
  If (FrejectionNote=AValue) then exit;
  FrejectionNote:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.SetapprovedTime(AIndex : Integer; const AValue : String); 

begin
  If (FapprovedTime=AValue) then exit;
  FapprovedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.SetprocessingErrors(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FprocessingErrors=AValue) then exit;
  FprocessingErrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.Setpriority(AIndex : Integer; const AValue : double); 

begin
  If (Fpriority=AValue) then exit;
  Fpriority:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.SetreceivedTime(AIndex : Integer; const AValue : String); 

begin
  If (FreceivedTime=AValue) then exit;
  FreceivedTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.Setfilename(AIndex : Integer; const AValue : String); 

begin
  If (Ffilename=AValue) then exit;
  Ffilename:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.SetstudioName(AIndex : Integer; const AValue : String); 

begin
  If (FstudioName=AValue) then exit;
  FstudioName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.SetpphName(AIndex : Integer; const AValue : String); 

begin
  If (FpphName=AValue) then exit;
  FpphName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure T_Component.SetnormalizedPriority(AIndex : Integer; const AValue : String); 

begin
  If (FnormalizedPriority=AValue) then exit;
  FnormalizedPriority:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function T_Component.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure T_Component.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'elids' : SetLength(FelIds,ALength);
  'titleleveleidrs' : SetLength(FtitleLevelEidrs,ALength);
  'editleveleidrs' : SetLength(FeditLevelEidrs,ALength);
  'altcutids' : SetLength(FaltCutIds,ALength);
  'customids' : SetLength(FcustomIds,ALength);
  'processingerrors' : SetLength(FprocessingErrors,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListComponentsResponse
  --------------------------------------------------------------------}


Procedure TListComponentsResponse.Setcomponents(AIndex : Integer; const AValue : TListComponentsResponseTypecomponentsArray); 

begin
  If (Fcomponents=AValue) then exit;
  Fcomponents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListComponentsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListComponentsResponse.SettotalSize(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalSize=AValue) then exit;
  FtotalSize:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListComponentsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'components' : SetLength(Fcomponents,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TStoreInfo
  --------------------------------------------------------------------}


Procedure TStoreInfo.SetvideoId(AIndex : Integer; const AValue : String); 

begin
  If (FvideoId=AValue) then exit;
  FvideoId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SetseasonId(AIndex : Integer; const AValue : String); 

begin
  If (FseasonId=AValue) then exit;
  FseasonId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SetshowId(AIndex : Integer; const AValue : String); 

begin
  If (FshowId=AValue) then exit;
  FshowId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.Setcountry(AIndex : Integer; const AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SetliveTime(AIndex : Integer; const AValue : String); 

begin
  If (FliveTime=AValue) then exit;
  FliveTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SettitleLevelEidr(AIndex : Integer; const AValue : String); 

begin
  If (FtitleLevelEidr=AValue) then exit;
  FtitleLevelEidr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SeteditLevelEidr(AIndex : Integer; const AValue : String); 

begin
  If (FeditLevelEidr=AValue) then exit;
  FeditLevelEidr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SetseasonName(AIndex : Integer; const AValue : String); 

begin
  If (FseasonName=AValue) then exit;
  FseasonName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SetshowName(AIndex : Integer; const AValue : String); 

begin
  If (FshowName=AValue) then exit;
  FshowName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SetseasonNumber(AIndex : Integer; const AValue : String); 

begin
  If (FseasonNumber=AValue) then exit;
  FseasonNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SetepisodeNumber(AIndex : Integer; const AValue : String); 

begin
  If (FepisodeNumber=AValue) then exit;
  FepisodeNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SethasSdOffer(AIndex : Integer; const AValue : boolean); 

begin
  If (FhasSdOffer=AValue) then exit;
  FhasSdOffer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SethasHdOffer(AIndex : Integer; const AValue : boolean); 

begin
  If (FhasHdOffer=AValue) then exit;
  FhasHdOffer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SethasVodOffer(AIndex : Integer; const AValue : boolean); 

begin
  If (FhasVodOffer=AValue) then exit;
  FhasVodOffer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SethasEstOffer(AIndex : Integer; const AValue : boolean); 

begin
  If (FhasEstOffer=AValue) then exit;
  FhasEstOffer:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SethasAudio51(AIndex : Integer; const AValue : boolean); 

begin
  If (FhasAudio51=AValue) then exit;
  FhasAudio51:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SetaudioTracks(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FaudioTracks=AValue) then exit;
  FaudioTracks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.Setsubtitles(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fsubtitles=AValue) then exit;
  Fsubtitles:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SethasInfoCards(AIndex : Integer; const AValue : boolean); 

begin
  If (FhasInfoCards=AValue) then exit;
  FhasInfoCards:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.Setmid(AIndex : Integer; const AValue : String); 

begin
  If (Fmid=AValue) then exit;
  Fmid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SettrailerId(AIndex : Integer; const AValue : String); 

begin
  If (FtrailerId=AValue) then exit;
  FtrailerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SetstudioName(AIndex : Integer; const AValue : String); 

begin
  If (FstudioName=AValue) then exit;
  FstudioName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStoreInfo.SetpphNames(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FpphNames=AValue) then exit;
  FpphNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TStoreInfo.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TStoreInfo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'audiotracks' : SetLength(FaudioTracks,ALength);
  'subtitles' : SetLength(Fsubtitles,ALength);
  'pphnames' : SetLength(FpphNames,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListStoreInfosResponse
  --------------------------------------------------------------------}


Procedure TListStoreInfosResponse.SetstoreInfos(AIndex : Integer; const AValue : TListStoreInfosResponseTypestoreInfosArray); 

begin
  If (FstoreInfos=AValue) then exit;
  FstoreInfos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListStoreInfosResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListStoreInfosResponse.SettotalSize(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalSize=AValue) then exit;
  FtotalSize:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListStoreInfosResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'storeinfos' : SetLength(FstoreInfos,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAccountsAvailsResource
  --------------------------------------------------------------------}


Class Function TAccountsAvailsResource.ResourceName : String;

begin
  Result:='avails';
end;

Class Function TAccountsAvailsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TplaymoviespartnerAPI;
end;

Function TAccountsAvailsResource.Get(accountId: string; availId: string) : TAvail;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/accounts/{accountId}/avails/{availId}';
  _Methodid   = 'playmoviespartner.accounts.avails.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'availId',availId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAvail) as TAvail;
end;

Function TAccountsAvailsResource.List(accountId: string; AQuery : string = '') : TListAvailsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/accounts/{accountId}/avails';
  _Methodid   = 'playmoviespartner.accounts.avails.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListAvailsResponse) as TListAvailsResponse;
end;


Function TAccountsAvailsResource.List(accountId: string; AQuery : TAccountsAvailslistOptions) : TListAvailsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'pphNames',AQuery.pphNames);
  AddToQuery(_Q,'studioNames',AQuery.studioNames);
  AddToQuery(_Q,'title',AQuery.title);
  AddToQuery(_Q,'territories',AQuery.territories);
  AddToQuery(_Q,'altId',AQuery.altId);
  AddToQuery(_Q,'videoIds',AQuery.videoIds);
  AddToQuery(_Q,'altIds',AQuery.altIds);
  Result:=List(accountId,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsOrdersResource
  --------------------------------------------------------------------}


Class Function TAccountsOrdersResource.ResourceName : String;

begin
  Result:='orders';
end;

Class Function TAccountsOrdersResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TplaymoviespartnerAPI;
end;

Function TAccountsOrdersResource.Get(accountId: string; orderId: string) : TOrder;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/accounts/{accountId}/orders/{orderId}';
  _Methodid   = 'playmoviespartner.accounts.orders.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'orderId',orderId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOrder) as TOrder;
end;

Function TAccountsOrdersResource.List(accountId: string; AQuery : string = '') : TListOrdersResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/accounts/{accountId}/orders';
  _Methodid   = 'playmoviespartner.accounts.orders.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListOrdersResponse) as TListOrdersResponse;
end;


Function TAccountsOrdersResource.List(accountId: string; AQuery : TAccountsOrderslistOptions) : TListOrdersResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'pphNames',AQuery.pphNames);
  AddToQuery(_Q,'studioNames',AQuery.studioNames);
  AddToQuery(_Q,'name',AQuery._name);
  AddToQuery(_Q,'status',AQuery.status);
  AddToQuery(_Q,'customId',AQuery.customId);
  AddToQuery(_Q,'videoIds',AQuery.videoIds);
  Result:=List(accountId,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsExperienceLocalesResource
  --------------------------------------------------------------------}


Class Function TAccountsExperienceLocalesResource.ResourceName : String;

begin
  Result:='experienceLocales';
end;

Class Function TAccountsExperienceLocalesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TplaymoviespartnerAPI;
end;

Function TAccountsExperienceLocalesResource.Get(accountId: string; elId: string) : TExperienceLocale;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/accounts/{accountId}/experienceLocales/{elId}';
  _Methodid   = 'playmoviespartner.accounts.experienceLocales.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'elId',elId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TExperienceLocale) as TExperienceLocale;
end;

Function TAccountsExperienceLocalesResource.List(accountId: string; AQuery : string = '') : TListExperienceLocalesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/accounts/{accountId}/experienceLocales';
  _Methodid   = 'playmoviespartner.accounts.experienceLocales.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListExperienceLocalesResponse) as TListExperienceLocalesResponse;
end;


Function TAccountsExperienceLocalesResource.List(accountId: string; AQuery : TAccountsExperienceLocaleslistOptions) : TListExperienceLocalesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'pphNames',AQuery.pphNames);
  AddToQuery(_Q,'studioNames',AQuery.studioNames);
  AddToQuery(_Q,'titleLevelEidr',AQuery.titleLevelEidr);
  AddToQuery(_Q,'editLevelEidr',AQuery.editLevelEidr);
  AddToQuery(_Q,'status',AQuery.status);
  AddToQuery(_Q,'customId',AQuery.customId);
  AddToQuery(_Q,'altCutId',AQuery.altCutId);
  Result:=List(accountId,_Q);
end;



{ --------------------------------------------------------------------
  TAccountsComponentsTypeResource
  --------------------------------------------------------------------}


Class Function TAccountsComponentsTypeResource.ResourceName : String;

begin
  Result:='type';
end;

Class Function TAccountsComponentsTypeResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TplaymoviespartnerAPI;
end;

Function TAccountsComponentsTypeResource.Get(accountId: string; componentId: string; _type: string) : T_Component;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/accounts/{accountId}/components/{componentId}/type/{type}';
  _Methodid   = 'playmoviespartner.accounts.components.type.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'componentId',componentId,'type',_type]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,T_Component) as T_Component;
end;



{ --------------------------------------------------------------------
  TAccountsComponentsResource
  --------------------------------------------------------------------}


Class Function TAccountsComponentsResource.ResourceName : String;

begin
  Result:='components';
end;

Class Function TAccountsComponentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TplaymoviespartnerAPI;
end;

Function TAccountsComponentsResource.List(accountId: string; AQuery : string = '') : TListComponentsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/accounts/{accountId}/components';
  _Methodid   = 'playmoviespartner.accounts.components.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListComponentsResponse) as TListComponentsResponse;
end;


Function TAccountsComponentsResource.List(accountId: string; AQuery : TAccountsComponentslistOptions) : TListComponentsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'pphNames',AQuery.pphNames);
  AddToQuery(_Q,'studioNames',AQuery.studioNames);
  AddToQuery(_Q,'titleLevelEidr',AQuery.titleLevelEidr);
  AddToQuery(_Q,'editLevelEidr',AQuery.editLevelEidr);
  AddToQuery(_Q,'status',AQuery.status);
  AddToQuery(_Q,'customId',AQuery.customId);
  AddToQuery(_Q,'inventoryId',AQuery.inventoryId);
  AddToQuery(_Q,'presentationId',AQuery.presentationId);
  AddToQuery(_Q,'playableSequenceId',AQuery.playableSequenceId);
  AddToQuery(_Q,'elId',AQuery.elId);
  AddToQuery(_Q,'altCutId',AQuery.altCutId);
  AddToQuery(_Q,'filename',AQuery.filename);
  Result:=List(accountId,_Q);
end;



Function TAccountsComponentsResource.GetTypeInstance : TAccountsComponentsTypeResource;

begin
  if (FTypeInstance=Nil) then
    FTypeInstance:=CreateTypeResource;
  Result:=FTypeInstance;
end;

Function TAccountsComponentsResource.CreateTypeResource : TAccountsComponentsTypeResource;

begin
  Result:=CreateTypeResource(Self);
end;


Function TAccountsComponentsResource.CreateTypeResource(AOwner : TComponent) : TAccountsComponentsTypeResource;

begin
  Result:=TAccountsComponentsTypeResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TAccountsStoreInfosCountryResource
  --------------------------------------------------------------------}


Class Function TAccountsStoreInfosCountryResource.ResourceName : String;

begin
  Result:='country';
end;

Class Function TAccountsStoreInfosCountryResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TplaymoviespartnerAPI;
end;

Function TAccountsStoreInfosCountryResource.Get(accountId: string; videoId: string; country: string) : TStoreInfo;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/accounts/{accountId}/storeInfos/{videoId}/country/{country}';
  _Methodid   = 'playmoviespartner.accounts.storeInfos.country.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId,'videoId',videoId,'country',country]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TStoreInfo) as TStoreInfo;
end;



{ --------------------------------------------------------------------
  TAccountsStoreInfosResource
  --------------------------------------------------------------------}


Class Function TAccountsStoreInfosResource.ResourceName : String;

begin
  Result:='storeInfos';
end;

Class Function TAccountsStoreInfosResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TplaymoviespartnerAPI;
end;

Function TAccountsStoreInfosResource.List(accountId: string; AQuery : string = '') : TListStoreInfosResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/accounts/{accountId}/storeInfos';
  _Methodid   = 'playmoviespartner.accounts.storeInfos.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['accountId',accountId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListStoreInfosResponse) as TListStoreInfosResponse;
end;


Function TAccountsStoreInfosResource.List(accountId: string; AQuery : TAccountsStoreInfoslistOptions) : TListStoreInfosResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'pphNames',AQuery.pphNames);
  AddToQuery(_Q,'studioNames',AQuery.studioNames);
  AddToQuery(_Q,'videoId',AQuery.videoId);
  AddToQuery(_Q,'countries',AQuery.countries);
  AddToQuery(_Q,'name',AQuery._name);
  AddToQuery(_Q,'videoIds',AQuery.videoIds);
  AddToQuery(_Q,'mids',AQuery.mids);
  AddToQuery(_Q,'seasonIds',AQuery.seasonIds);
  Result:=List(accountId,_Q);
end;



Function TAccountsStoreInfosResource.GetCountryInstance : TAccountsStoreInfosCountryResource;

begin
  if (FCountryInstance=Nil) then
    FCountryInstance:=CreateCountryResource;
  Result:=FCountryInstance;
end;

Function TAccountsStoreInfosResource.CreateCountryResource : TAccountsStoreInfosCountryResource;

begin
  Result:=CreateCountryResource(Self);
end;


Function TAccountsStoreInfosResource.CreateCountryResource(AOwner : TComponent) : TAccountsStoreInfosCountryResource;

begin
  Result:=TAccountsStoreInfosCountryResource.Create(AOwner);
  Result.API:=Self.API;
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
  Result:=TplaymoviespartnerAPI;
end;



Function TAccountsResource.GetAvailsInstance : TAccountsAvailsResource;

begin
  if (FAvailsInstance=Nil) then
    FAvailsInstance:=CreateAvailsResource;
  Result:=FAvailsInstance;
end;

Function TAccountsResource.CreateAvailsResource : TAccountsAvailsResource;

begin
  Result:=CreateAvailsResource(Self);
end;


Function TAccountsResource.CreateAvailsResource(AOwner : TComponent) : TAccountsAvailsResource;

begin
  Result:=TAccountsAvailsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetOrdersInstance : TAccountsOrdersResource;

begin
  if (FOrdersInstance=Nil) then
    FOrdersInstance:=CreateOrdersResource;
  Result:=FOrdersInstance;
end;

Function TAccountsResource.CreateOrdersResource : TAccountsOrdersResource;

begin
  Result:=CreateOrdersResource(Self);
end;


Function TAccountsResource.CreateOrdersResource(AOwner : TComponent) : TAccountsOrdersResource;

begin
  Result:=TAccountsOrdersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetExperienceLocalesInstance : TAccountsExperienceLocalesResource;

begin
  if (FExperienceLocalesInstance=Nil) then
    FExperienceLocalesInstance:=CreateExperienceLocalesResource;
  Result:=FExperienceLocalesInstance;
end;

Function TAccountsResource.CreateExperienceLocalesResource : TAccountsExperienceLocalesResource;

begin
  Result:=CreateExperienceLocalesResource(Self);
end;


Function TAccountsResource.CreateExperienceLocalesResource(AOwner : TComponent) : TAccountsExperienceLocalesResource;

begin
  Result:=TAccountsExperienceLocalesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetComponentsTypeInstance : TAccountsComponentsTypeResource;

begin
  if (FComponentsTypeInstance=Nil) then
    FComponentsTypeInstance:=CreateComponentsTypeResource;
  Result:=FComponentsTypeInstance;
end;

Function TAccountsResource.CreateComponentsTypeResource : TAccountsComponentsTypeResource;

begin
  Result:=CreateComponentsTypeResource(Self);
end;


Function TAccountsResource.CreateComponentsTypeResource(AOwner : TComponent) : TAccountsComponentsTypeResource;

begin
  Result:=TAccountsComponentsTypeResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetComponentsInstance : TAccountsComponentsResource;

begin
  if (FComponentsInstance=Nil) then
    FComponentsInstance:=CreateComponentsResource;
  Result:=FComponentsInstance;
end;

Function TAccountsResource.CreateComponentsResource : TAccountsComponentsResource;

begin
  Result:=CreateComponentsResource(Self);
end;


Function TAccountsResource.CreateComponentsResource(AOwner : TComponent) : TAccountsComponentsResource;

begin
  Result:=TAccountsComponentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetStoreInfosCountryInstance : TAccountsStoreInfosCountryResource;

begin
  if (FStoreInfosCountryInstance=Nil) then
    FStoreInfosCountryInstance:=CreateStoreInfosCountryResource;
  Result:=FStoreInfosCountryInstance;
end;

Function TAccountsResource.CreateStoreInfosCountryResource : TAccountsStoreInfosCountryResource;

begin
  Result:=CreateStoreInfosCountryResource(Self);
end;


Function TAccountsResource.CreateStoreInfosCountryResource(AOwner : TComponent) : TAccountsStoreInfosCountryResource;

begin
  Result:=TAccountsStoreInfosCountryResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TAccountsResource.GetStoreInfosInstance : TAccountsStoreInfosResource;

begin
  if (FStoreInfosInstance=Nil) then
    FStoreInfosInstance:=CreateStoreInfosResource;
  Result:=FStoreInfosInstance;
end;

Function TAccountsResource.CreateStoreInfosResource : TAccountsStoreInfosResource;

begin
  Result:=CreateStoreInfosResource(Self);
end;


Function TAccountsResource.CreateStoreInfosResource(AOwner : TComponent) : TAccountsStoreInfosResource;

begin
  Result:=TAccountsStoreInfosResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TPlaymoviespartnerAPI
  --------------------------------------------------------------------}

Class Function TPlaymoviespartnerAPI.APIName : String;

begin
  Result:='playmoviespartner';
end;

Class Function TPlaymoviespartnerAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TPlaymoviespartnerAPI.APIRevision : String;

begin
  Result:='20160518';
end;

Class Function TPlaymoviespartnerAPI.APIID : String;

begin
  Result:='playmoviespartner:v1';
end;

Class Function TPlaymoviespartnerAPI.APITitle : String;

begin
  Result:='Google Play Movies Partner API';
end;

Class Function TPlaymoviespartnerAPI.APIDescription : String;

begin
  Result:='Gets the delivery status of titles for Google Play Movies Partners.';
end;

Class Function TPlaymoviespartnerAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TPlaymoviespartnerAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TPlaymoviespartnerAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TPlaymoviespartnerAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TPlaymoviespartnerAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/playmoviespartner/';
end;

Class Function TPlaymoviespartnerAPI.APIrootUrl : string;

begin
  Result:='https://playmoviespartner.googleapis.com/';
end;

Class Function TPlaymoviespartnerAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TPlaymoviespartnerAPI.APIbaseURL : String;

begin
  Result:='https://playmoviespartner.googleapis.com/';
end;

Class Function TPlaymoviespartnerAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TPlaymoviespartnerAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TPlaymoviespartnerAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TPlaymoviespartnerAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/playmovies_partner.readonly';
  Result[0].Description:='View the digital assets you publish on Google Play Movies and TV';
  
end;

Class Function TPlaymoviespartnerAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TPlaymoviespartnerAPI.RegisterAPIResources;

begin
  TAvail.RegisterObject;
  TListAvailsResponse.RegisterObject;
  TOrder.RegisterObject;
  TListOrdersResponse.RegisterObject;
  TExperienceLocale.RegisterObject;
  TListExperienceLocalesResponse.RegisterObject;
  T_Component.RegisterObject;
  TListComponentsResponse.RegisterObject;
  TStoreInfo.RegisterObject;
  TListStoreInfosResponse.RegisterObject;
end;


Function TPlaymoviespartnerAPI.GetAccountsAvailsInstance : TAccountsAvailsResource;

begin
  if (FAccountsAvailsInstance=Nil) then
    FAccountsAvailsInstance:=CreateAccountsAvailsResource;
  Result:=FAccountsAvailsInstance;
end;

Function TPlaymoviespartnerAPI.CreateAccountsAvailsResource : TAccountsAvailsResource;

begin
  Result:=CreateAccountsAvailsResource(Self);
end;


Function TPlaymoviespartnerAPI.CreateAccountsAvailsResource(AOwner : TComponent) : TAccountsAvailsResource;

begin
  Result:=TAccountsAvailsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPlaymoviespartnerAPI.GetAccountsOrdersInstance : TAccountsOrdersResource;

begin
  if (FAccountsOrdersInstance=Nil) then
    FAccountsOrdersInstance:=CreateAccountsOrdersResource;
  Result:=FAccountsOrdersInstance;
end;

Function TPlaymoviespartnerAPI.CreateAccountsOrdersResource : TAccountsOrdersResource;

begin
  Result:=CreateAccountsOrdersResource(Self);
end;


Function TPlaymoviespartnerAPI.CreateAccountsOrdersResource(AOwner : TComponent) : TAccountsOrdersResource;

begin
  Result:=TAccountsOrdersResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPlaymoviespartnerAPI.GetAccountsExperienceLocalesInstance : TAccountsExperienceLocalesResource;

begin
  if (FAccountsExperienceLocalesInstance=Nil) then
    FAccountsExperienceLocalesInstance:=CreateAccountsExperienceLocalesResource;
  Result:=FAccountsExperienceLocalesInstance;
end;

Function TPlaymoviespartnerAPI.CreateAccountsExperienceLocalesResource : TAccountsExperienceLocalesResource;

begin
  Result:=CreateAccountsExperienceLocalesResource(Self);
end;


Function TPlaymoviespartnerAPI.CreateAccountsExperienceLocalesResource(AOwner : TComponent) : TAccountsExperienceLocalesResource;

begin
  Result:=TAccountsExperienceLocalesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPlaymoviespartnerAPI.GetAccountsComponentsTypeInstance : TAccountsComponentsTypeResource;

begin
  if (FAccountsComponentsTypeInstance=Nil) then
    FAccountsComponentsTypeInstance:=CreateAccountsComponentsTypeResource;
  Result:=FAccountsComponentsTypeInstance;
end;

Function TPlaymoviespartnerAPI.CreateAccountsComponentsTypeResource : TAccountsComponentsTypeResource;

begin
  Result:=CreateAccountsComponentsTypeResource(Self);
end;


Function TPlaymoviespartnerAPI.CreateAccountsComponentsTypeResource(AOwner : TComponent) : TAccountsComponentsTypeResource;

begin
  Result:=TAccountsComponentsTypeResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPlaymoviespartnerAPI.GetAccountsComponentsInstance : TAccountsComponentsResource;

begin
  if (FAccountsComponentsInstance=Nil) then
    FAccountsComponentsInstance:=CreateAccountsComponentsResource;
  Result:=FAccountsComponentsInstance;
end;

Function TPlaymoviespartnerAPI.CreateAccountsComponentsResource : TAccountsComponentsResource;

begin
  Result:=CreateAccountsComponentsResource(Self);
end;


Function TPlaymoviespartnerAPI.CreateAccountsComponentsResource(AOwner : TComponent) : TAccountsComponentsResource;

begin
  Result:=TAccountsComponentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPlaymoviespartnerAPI.GetAccountsStoreInfosCountryInstance : TAccountsStoreInfosCountryResource;

begin
  if (FAccountsStoreInfosCountryInstance=Nil) then
    FAccountsStoreInfosCountryInstance:=CreateAccountsStoreInfosCountryResource;
  Result:=FAccountsStoreInfosCountryInstance;
end;

Function TPlaymoviespartnerAPI.CreateAccountsStoreInfosCountryResource : TAccountsStoreInfosCountryResource;

begin
  Result:=CreateAccountsStoreInfosCountryResource(Self);
end;


Function TPlaymoviespartnerAPI.CreateAccountsStoreInfosCountryResource(AOwner : TComponent) : TAccountsStoreInfosCountryResource;

begin
  Result:=TAccountsStoreInfosCountryResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPlaymoviespartnerAPI.GetAccountsStoreInfosInstance : TAccountsStoreInfosResource;

begin
  if (FAccountsStoreInfosInstance=Nil) then
    FAccountsStoreInfosInstance:=CreateAccountsStoreInfosResource;
  Result:=FAccountsStoreInfosInstance;
end;

Function TPlaymoviespartnerAPI.CreateAccountsStoreInfosResource : TAccountsStoreInfosResource;

begin
  Result:=CreateAccountsStoreInfosResource(Self);
end;


Function TPlaymoviespartnerAPI.CreateAccountsStoreInfosResource(AOwner : TComponent) : TAccountsStoreInfosResource;

begin
  Result:=TAccountsStoreInfosResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPlaymoviespartnerAPI.GetAccountsInstance : TAccountsResource;

begin
  if (FAccountsInstance=Nil) then
    FAccountsInstance:=CreateAccountsResource;
  Result:=FAccountsInstance;
end;

Function TPlaymoviespartnerAPI.CreateAccountsResource : TAccountsResource;

begin
  Result:=CreateAccountsResource(Self);
end;


Function TPlaymoviespartnerAPI.CreateAccountsResource(AOwner : TComponent) : TAccountsResource;

begin
  Result:=TAccountsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TPlaymoviespartnerAPI.RegisterAPI;
end.
