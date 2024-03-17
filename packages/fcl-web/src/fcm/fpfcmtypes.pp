{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    FCM (Firebase Cloud Messaging) - Types

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit fpfcmtypes;
{$ENDIF}

{$mode ObjFPC}
{$H+}


interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpJson.Data, Jwt.Types;
{$ELSE}
  Classes, SysUtils, fpjson, fpjwt;
{$ENDIF}

Type
  EFCM = Class(Exception)
  end;

  { TBearerToken }

  TBearerToken = Class(TBaseJWT)
  private
    faccess_token: string;
    fexpires_in: Integer;
    fext_expires_in: Integer;
    fid_token: string;
    frefresh_token: string;
    fscope: string;
    FTokenDateTime: TDateTime;
    ftoken_type: string;
    function GetTokenDateTime: string;
    procedure SetTokenDateTime(AValue: string);
  Public
//    function Parse(const AJSON: string): Boolean;
    Procedure SaveToStream(aStream : TStream);
    Procedure SaveToFile(Const aFileName : String);
    Procedure LoadFromStream(aStream : TStream);
    Procedure LoadFromFile(Const aFileName : String);
    function IsValidData: Boolean;
    Function IsExpired : Boolean;
    Property TokenDateTime : TDateTime Read FTokenDateTime Write FTokenDateTime;
  published
    property access_token: string read faccess_token write faccess_token;
    property expires_in: Integer read fexpires_in write fexpires_in;
    property ext_expires_in: Integer read fext_expires_in write fext_expires_in;
    property id_token: string read fid_token write fid_token;
    property refresh_token: string read frefresh_token write frefresh_token;
    property scope: string read fscope write fscope;
    property token_type: string read ftoken_type write ftoken_type;
    property token_dateTime: string read GetTokenDateTime Write SetTokenDateTime;
  end;

  { TServiceAccountData }

  TServiceAccountData = Class(TBaseJWT)
  private
    FAuthURI: string;
    FClientEmail: string;
    FClientID: string;
    FPrivateKey: string;
    FProjectID: string;
    FTokenURI: string;
  Protected
  public
    function Parse(const AJSON: string): Boolean;
    function IsValidData: Boolean;
  Published
    Property auth_uri: string Read FAuthURI Write FAuthURI;
    Property client_email: string read FClientEmail  Write FClientEmail;
    Property client_id: string Read FClientID Write FClientID;
    Property IsValid: Boolean Read IsValidData;
    Property private_key: string Read FPrivateKey Write FPrivateKey;
    Property project_id: string Read FProjectID Write FProjectID;
    Property token_uri: string Read FTokenURI Write FTokenURI;
  end;

  TMessageOption = (moLargeText,moLargeImage,moContent);
  TMessageOptions = set of TMessageOption;

  { TGoogleJWT }

  { TGoogleClaims }

  TGoogleClaims = class(TClaims)
  private
    fscope: string;
  Published
    property scope : string read fscope write fscope;
  end;

  TGoogleJWT = Class(TJWT)
  private
    function GetGoogleClaims: TGoogleClaims;
  Protected
    Function CreateClaims: TClaims; override;
  Public
    Property GoogleClaims : TGoogleClaims Read GetGoogleClaims;
  end;

  { TPlatformOptions }

  // A helper class to ease streaming

  { TJSONPersist }

  TJSONPersist = class(TPersistent)
  private
  Protected
    // All calls return true if the value was added, false if it was not added.
    // Adds if avalue is nonempty string
    Function JSONAdd(Obj : TJSONObject; const aKey,aValue : String) : Boolean;
    // Adds if aValue.count>0
    Function JSONAdd(Obj : TJSONObject; const aKey : String; aValue : TJSONObject) : Boolean;
    // Adds if avalue <>0
    Function JSONAdd(Obj : TJSONObject; const aKey : String; aValue : Integer) : Boolean;
    // Adds if avalue <>0
    Function JSONAdd(Obj : TJSONObject; const aKey : String; aValue : Int64) : Boolean;
    // Adds if avalue = true
    Function JSONAdd(Obj : TJSONObject; const aKey : String; aValue : Boolean) : Boolean;
    // Get sub-object with name key, create if needed.
    Function JSONGetSub(Obj : TJSONObject; const aKey : String) : TJSONObject;
    // Adds as name : value key pairs.
    Function JSONAddStrings(Obj : TJSONObject; aValue : TStrings) : Integer;
    // Adds strings as array.
    function JSONAddStringsRaw(Obj: TJSONObject; aKey: String; aValue: TStrings): Integer;
  end;

  { TPlatformConfig }

  TPlatformConfig = class(TJSONPersist)
  private
    FCustomPayLoad: TJSONObject;
  protected
    procedure AddCustomPayload(aObject : TJSONObject);
  Public
    procedure Assign(Source: TPersistent); override;
    constructor Create;virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure ToJSON(Obj : TJSONObject); virtual;
    // Keys in this object will be added to the top-level platform-specific payload.
    Property CustomPayLoad : TJSONObject Read FCustomPayLoad;
  end;

  { THeadersPlatformConfig }

  THeadersPlatformConfig = class(TPlatformConfig)
    FHeaders: TStrings;
    procedure SetHeaders(AValue: TStrings);
  Protected
    function addHeaders(Obj: TJSONObject; asSub : Boolean = True) : Integer;
  Public
    procedure Assign(Source: TPersistent); override;
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
  Published
    property Headers : TStrings Read FHeaders Write SetHeaders;
  end;

  { TFCMOptions }

  TFCMOptions = class(TPlatformConfig)
  private
    FAnalyticsLabel: String;
  Public
    procedure Assign(Source: TPersistent); override;
    procedure ToJSON(Obj : TJSONObject); override;
    function HaveData : Boolean; virtual;
    procedure Clear; override;
  Published
    Property AnalyticsLabel : String Read FAnalyticsLabel Write FAnalyticsLabel;
  end;

  { TAppleFCMOptions }

  TAppleFCMOptions = class(TFCMoptions)
  private
    FImage: String;
    Public
    procedure Assign(Source: TPersistent); override;
    procedure ToJSON(Obj : TJSONObject); override;
    function HaveData : Boolean; override;
    procedure Clear; override;
  Published
    Property Image : String Read FImage Write FImage;
  end;

  { TAppleConfig }

  TAppleConfig = class(THeadersPlatformConfig)
  private
    FAlert: string;
    FAlertObject: TJSONObject;
    Fattributestype: string;
    FBadge: Integer;
    fcategory: string;
    Fdismissaldate: Int64;
    FEvent: string;
    FFCMOPtions: TAppleFCMOptions;
    FFiltercriteria: string;
    Finterruptionlevel: string;
    fmutablecontent: integer;
    Frelevancescore: string;
    FSound: string;
    FStaleDate: Int64;
    Ftargetcontentid: string;
    Fthreadid: string;
    FTimeStamp: Int64;
    function CreateFCMOptions: TAppleFCMOptions;
    procedure SetFFCMoptions(AValue: TAppleFCMOptions);
  protected
    procedure AddPayLoad(Obj : TJSONObject); virtual;
  Public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ToJSON(Obj : TJSONObject); override;
    procedure Clear; override;
    Property AlertObject : TJSONObject Read FAlertObject;
  Published
    Property Alert : string Read FAlert Write falert;
    property Badge : Integer Read FBadge write fbadge;
    Property ThreadID : string read Fthreadid Write Fthreadid;
    property Category : string read fcategory write fcategory;
    property MutableContent : integer Read fmutablecontent write fmutablecontent;
    Property TargetContentID : string read Ftargetcontentid Write Ftargetcontentid;
    Property InterruptionLevel : string read Finterruptionlevel Write Finterruptionlevel;
    Property RelevanceScore : string read Frelevancescore Write Frelevancescore;
    Property FilterCriteria : string read FFiltercriteria Write FFiltercriteria;
    Property StaleDate : Int64 read FStaleDate Write FStaleDate;
    property Timestamp : Int64 read FTimeStamp Write FTimeStamp;
    property DismissalDate : Int64 read Fdismissaldate Write Fdismissaldate;
    property Sound: string read FSound write FSound;
    Property Event : string read FEvent Write FEvent;
    Property AttributesType : string Read fattributestype Write Fattributestype;
    property FCMoptions : TAppleFCMOptions Read FFCMOPtions Write SetFFCMoptions;
  end;


  { TAndroidConfig }

  TAndroidMessageVisibility = (mvUnspecified,mvPrivate,mvPublic,mvSecret);
  TAndroidNotificationPriority = (npUnspecified,npMin,npLog,npDefault,npHigh,npMax);
  TAndroidMessagePriority = (mpUnspecified,mpNormal,mpHigh);

  TAndroidConfig = class(TPlatformConfig)
  private
    Fbody: string;
    fbodylocargs: TStrings;
    fbodylockey: string;
    FChannelID: string;
    FClickAction: string;
    FFCMoptions: TFCMOptions;
    FCollapseKey: string;
    fcolor: string;
    FData: TJSONObject;
    fdefaultlightsettings: boolean;
    Fdefaultsound: boolean;
    Fdefaultvibratetimings: boolean;
    fdirectbootOK: Boolean;
    Feventtime: string;
    Ficon: string;
    fimage: string;
    Flightsettings: TJSONObject;
    flocalonly: boolean;
    fnotificationcount: integer;
    FNotificationPriority: TAndroidNotificationPriority;
    FOwnsData: boolean;
    FPriority: TAndroidMessagePriority;
    Frestrictedpackagename: string;
    fsound: string;
    fsticky: boolean;
    ftag: string;
    fticker: string;
    Ftitle: string;
    ftitlelocargs: TStrings;
    ftitlelockey: string;
    Fttl: string;
    Fvibratetimings: TStrings;
    FVisibility: TAndroidMessageVisibility;
    procedure setbodylocargs(AValue: TStrings);
    procedure SetData(AValue: TJSONObject);
    procedure SetFCMOptions(AValue: TFCMOptions);
    procedure settitlelocargs(AValue: TStrings);
    procedure Setvibratetimings(AValue: TStrings);
  Public
    procedure Assign(Source: TPersistent); override;
    constructor create; override;
    destructor destroy; override;
    procedure Clear; override;
    procedure ToJSON(Obj : TJSONObject); override;
    property data : TJSONObject Read FData Write SetData;
    Property OwnsData : boolean read FOwnsData Write FOwnsData;
    property lightsettings : TJSONObject Read Flightsettings;
  Published
    property ChannelID: string read FChannelID write FChannelID;
    property CollapseKey : string read FCollapseKey Write FCollapseKey;
    property Priority: TAndroidMessagePriority read FPriority write FPriority;
    property TTL : string read Fttl Write FTTL;
    Property RestrictedPackageName : string read Frestrictedpackagename Write Frestrictedpackagename;
    property DirectBootOk : Boolean Read fdirectbootOK write fdirectbootOK;
    property title : string read Ftitle write FTitle;
    property Body : string read Fbody write Fbody;
    property Icon : string read Ficon write Ficon;
    property Color : string read fcolor write fcolor;
    property Sound : string read fsound write fsound;
    property Tag : string read ftag write ftag;
    property ClickAction: string read FClickAction write FClickAction;
    property BodyLocKey : string read fbodylockey write Fbodylockey;
    property BodyLocArgs : TStrings read fbodylocargs write setbodylocargs;
    property TitleLocKey : string read ftitlelockey write Ftitlelockey;
    property TitleLocArgs : TStrings read ftitlelocargs write settitlelocargs;
    property Ticker : string read fticker write Fticker;
    property Sticky : boolean read fsticky write Fsticky;
    property EventTime : string read feventtime write Feventtime;
    property LocalOnly : boolean read flocalonly write flocalonly;
    property DefaultSound : boolean read Fdefaultsound write Fdefaultsound;
    property DefaultVibrateTimings : boolean read Fdefaultvibratetimings write Fdefaultvibratetimings;
    property DefaultLightSettings : boolean read fdefaultlightsettings write Fdefaultlightsettings;
    property VibrateTimings : TStrings read Fvibratetimings Write Setvibratetimings;
    property NotificationCount : integer read fnotificationcount write fnotificationcount;
    property NotificationPriority : TAndroidNotificationPriority Read FNotificationPriority Write FNotificationPriority;
    property Image : string read fimage write Fimage;
    property FCMOptions : TFCMOptions Read FFCMoptions Write SetFCMOptions;
    property Visibility : TAndroidMessageVisibility Read FVisibility Write FVisibility;
  end;

  { TWebPushConfig }

  { TWebPushFCMOptions }

  TWebPushFCMOptions = class(TFCMOptions)
  private
    FLink: string;
  Public
    procedure Assign(Source: TPersistent); override;
    Procedure ToJSON(Obj: TJSONObject); override;
    function HaveData : Boolean; override;
    procedure Clear; override;

  Published
    property link : string read FLink Write FLink;
  end;

  TWebPushConfig = class(THeadersPlatformConfig)
  private
    FData: TStrings;
    FNotification: TJSONObject;
    FOPtions: TWebPushFCMOptions;
    procedure SetData(AValue: TStrings);
    procedure SetOptions(AValue: TWebPushFCMOptions);
  Public
    procedure Assign(Source: TPersistent); override;
    Constructor Create; override;
    Destructor destroy; override;
    procedure Clear; override;
    procedure ToJSON(Obj : TJSONObject); override;
    Property notification : TJSONObject Read FNotification;
  Published
    property Data : TStrings Read FData Write SetData;
    property FCMOptions : TWebPushFCMOptions Read FOPtions Write SetOptions;
  end;

  { TNotificationMessage }
  TNotificationSendOption = (nsAndroid,nsApple,nsWebPush,nsFCMOptions,nsDataOnly);
  TNotificationSendOptions = set of TNotificationSendOption;
  TRecipientType = (rtToken,rtTopic,rtCondition);

  TNotificationMessage = class(TJSONPersist)
  Public
    Const DefaultSendOptions = [nsAndroid,nsApple,nsWebPush,nsFCMoptions];
  private
    FAndroidConfig: TAndroidConfig;
    FAppleConfig: TAppleConfig;
    FBody: string;
    FData: TStrings;
    FFCMOptions: TFCMOptions;
    FImageURL: string;
    FOptions: TMessageOptions;
    FRecipient: String;
    FRecipientType: TRecipientType;
    FSendOptions: TNotificationSendOptions;
    FTitle: string;
    FWebConfig: TWebPushConfig;
    function IsSendOptionsStored: Boolean;
    procedure SetAndroidConfig(AValue: TAndroidConfig);
    procedure SetAppleConfig(AValue: TAppleConfig);
    procedure SetBody(AValue: string);
    procedure SetCMOptions(AValue: TFCMOptions);
    procedure setdata(AValue: TStrings);
    procedure SetImageURL(AValue: string);
    procedure SetTitle(AValue: string);
    procedure SetWebConfig(AValue: TWebPushConfig);
  Protected
    function CreateFCMoptions: TFCMOptions; virtual;
    function CreateAndroidConfig: TAndroidConfig; virtual;
    function CreateAppleConfig: TAppleConfig; virtual;
    function CreateWebPushConfig: TWebPushConfig; virtual;
  public
    constructor Create;
    destructor destroy; override;

    procedure ToJSON(aObj : TJSONObject);
    function Encode : string;
    procedure Clear;
    // toplevel properties, valid for all platforms.
    // Notification data.
    Property Recipient : String Read FRecipient Write FRecipient;
    Property RecipientType : TRecipientType Read FRecipientType Write FRecipientType;
    property Data: TStrings read FData write setdata;
    property Title: string read FTitle write SetTitle;
    property Body: string read FBody write SetBody;
    property Image: string read FImageURL write SetImageURL;
    // available in Apple and Android, not in web.
    property Options: TMessageOptions read FOptions write FOptions;
    Property SendOptions : TNotificationSendOptions Read FSendOptions Write FSendOptions Stored IsSendOptionsStored;
    // Apple specific
    property AppleConfig : TAppleConfig Read FAppleConfig Write SetAppleConfig;
    // Android specific
    property AndroidConfig : TAndroidConfig Read FAndroidConfig Write SetAndroidConfig;
    // Web specific
    Property WebPushConfig : TWebPushConfig Read FWebConfig Write SetWebConfig;
    // FCM options
    Property FCMOptions : TFCMOptions Read FFCMOptions Write SetCMOptions;
  end;



implementation


{$IFDEF FPC_DOTTEDUNITS}
uses System.DateUtils, FCM.Strings;
{$ELSE}
uses dateutils, fpfcmstrings;
{$ENDIF}
{ TServiceAccountData }

function TServiceAccountData.Parse(const AJSON: string): Boolean;

var
  D : TJSONData;
  O : TJSONObject absolute D;

begin
  D:=GetJSON(aJSON);
  try
    Result:=D is TJSONObject;
    if Result then
      begin
      DoLoadFromJSON(O);
      Result:=IsValidData;
      end;
  finally
    D.Free;
  end;
end;

function TServiceAccountData.IsValidData: Boolean;
begin
  Result:=(FAuthURI<>'');
  Result:=Result and (FClientEmail<>'');
  Result:=Result and (FClientID<>'');
  Result:=Result and (FPrivateKey<>'');
  Result:=Result and (FProjectID<>'');
  Result:=Result and (FTokenURI<>'');
end;

{ TGoogleJWT }

function TGoogleJWT.GetGoogleClaims: TGoogleClaims;
begin
  Result:=(Claims as TGoogleClaims);
end;

function TGoogleJWT.CreateClaims: TClaims;
begin
  Result:=TGoogleClaims.Create;
end;


{ TJSONPersist }

function TJSONPersist.JSONAdd(Obj: TJSONObject; const aKey, aValue: String): Boolean;
begin
  Result:=assigned(Obj) and (aValue<>'') and (aKey<>'');
  if Result then
    Obj.Add(aKey,aValue);
end;

function TJSONPersist.JSONAdd(Obj: TJSONObject; const aKey: String; aValue: TJSONObject): Boolean;
begin
  Result:=Assigned(Obj) and (aKey<>'') and Assigned(aValue) and (aValue.Count>0);
  if Result then
    Obj.Add(aKey,aValue.Clone);
end;

function TJSONPersist.JSONAdd(Obj: TJSONObject; const aKey: String; aValue: Integer): Boolean;
begin
  Result:=Assigned(Obj) and (aKey<>'') and (aValue<>0);
  if Result then
    Obj.Add(aKey,aValue);
end;

function TJSONPersist.JSONAdd(Obj: TJSONObject; const aKey: String; aValue: Int64): Boolean;
begin
  Result:=Assigned(Obj) and (aKey<>'') and (aValue<>0);
  if Result then
    Obj.Add(aKey,aValue);
end;

function TJSONPersist.JSONAdd(Obj: TJSONObject; const aKey: String; aValue: Boolean): Boolean;
begin
  Result:=Assigned(Obj) and (aKey<>'') and (aValue);
  if Result then
    Obj.Add(aKey,aValue);
end;

function TJSONPersist.JSONGetSub(Obj: TJSONObject; const aKey: String): TJSONObject;

var
  Idx : integer;

begin
  Idx:=Obj.IndexOfName(aKey);
  if Idx<>-1 then
    Result:=TJSONObject(Obj.Items[idx])
  else
    begin
    Result:=TJSONObject.Create;
    Obj.Add(aKey,Result);
    end;
end;

function TJSONPersist.JSONAddStrings(Obj: TJSONObject; aValue: TStrings): Integer;

var
  I : Integer;
  N,V : String;

begin
  Result:=0;
  for I:=0 to aValue.Count-1 do
    begin
    aValue.GetNameValue(I,N,V);
    if (N<>'') then
      begin
      Obj.Add(N,V);
      Inc(Result);
      end;
    end;
end;

function TJSONPersist.JSONAddStringsRaw(Obj: TJSONObject; aKey : String; aValue: TStrings): Integer;

var
  s : string;
  arr : TJSONArray;

begin
  Result:=aValue.Count;
  if Result=0 then exit;
  arr:=TJSONArray.Create;
  obj.Add(aKey,arr);
  for S in aValue do
    arr.add(S)

end;


{ TPlatformConfig }

procedure TPlatformConfig.AddCustomPayload(aObject: TJSONObject);

var
  Enum : TJSONEnum;

begin
  for Enum in CustomPayLoad do
    if aObject.IndexOfName(Enum.Key)<>-1 then
      aObject.Add(Enum.Key,Enum.Value.Clone);
end;

procedure TPlatformConfig.Assign(Source: TPersistent);

var
  aSource: TPlatformConfig absolute Source;

begin
  if Source is TPlatformConfig then
  begin
    FreeAndNil(FCustomPayLoad);
    FCustomPayLoad:=(aSource.FCustomPayLoad.Clone as TJSONObject);
  end else
    inherited Assign(Source);
end;

(*
constructor TPlatformConfig.Create;
begin
  FCustomPayLoad:=TJSONObject.Create;
end;

destructor TPlatformConfig.destroy;
begin
  FreeAndNil(FCustomPayLoad);
  Inherited;
end;
*)
procedure TPlatformConfig.ToJSON(Obj: TJSONObject);

var
  payload : TJSONObject;

begin
  if (FCustomPayLoad.Count=0) then
    exit;
  PayLoad:=JSONGetSub(Obj,'payload');
  AddCustomPayLoad(PayLoad);
end;

{ THeadersPlatformConfig }

procedure THeadersPlatformConfig.SetHeaders(AValue: TStrings);

var
  I : integer;
  N,V : String;

begin
  if FHeaders=AValue then Exit;
  FHeaders.Clear;
  For I:=0 to aValue.Count-1 do
    begin
    aValue.GetNameValue(I,N,V);
    if N<>'' then
      FHeaders.Add(N+': '+V);
    end;
end;


function THeadersPlatformConfig.addHeaders(Obj: TJSONObject; asSub: Boolean = true): Integer;

var
  H : TJSONObject;

begin
  Result:=0;
  if Headers.Count=0 then exit;
  if Not AsSub then
    H:=Obj
  else
    h:=JSONGetSub(Obj,'headers');
  Result:=JSONAddStrings(H,FHeaders);
end;

procedure THeadersPlatformConfig.Assign(Source: TPersistent);
var
  aSource: THeadersPlatformConfig absolute source;
begin
  inherited Assign(Source);
  if Source is THeadersPlatformConfig then
  begin
    FHeaders:=aSource.FHeaders;
  end;
end;

constructor THeadersPlatformConfig.Create;
begin
  inherited Create;
  FHeaders:=TStringList.Create;
  TStringList(FHeaders).NameValueSeparator:=':';
end;

destructor THeadersPlatformConfig.Destroy;
begin
  FreeAndNil(FHeaders);
  inherited destroy;
end;

procedure THeadersPlatformConfig.Clear;
begin
  inherited Clear;
  FHeaders.Clear;
end;

{ TAppleConfig }

procedure TAppleConfig.SetFFCMoptions(AValue: TAppleFCMOptions);
begin
  if FFCMOPtions=AValue then Exit;
  FFCMOPtions.Assign(AValue);
end;

procedure TAppleConfig.AddPayLoad(Obj: TJSONObject);


begin
  if (AlertObject.Count>0) then
    Obj.Add('alert',AlertObject.Clone)
  else
    JSONAdd(Obj,'alert',Alert);
  JSONAdd(Obj,'badge',Badge);
  JSONAdd(Obj,'sound',Sound);
  JSONAdd(Obj,'thread-id',ThreadID);
  JSONAdd(Obj,'category',Category);
  JSONAdd(Obj,'mutable-content',MutableContent);
  JSONAdd(Obj,'target-content-id',TargetContentID);
  JSONAdd(Obj,'interruption-level',InterruptionLevel);
  JSONAdd(Obj,'relevance-score',RelevanceScore);
  JSONAdd(Obj,'filter-criteria',FilterCriteria);
  JSONAdd(Obj,'stale-date',StaleDate);
  JSONAdd(Obj,'timestamp',TimeStamp);
  JSONAdd(Obj,'dismissal-date',DismissalDate);
  JSONAdd(Obj,'event',Event);
  JSONAdd(Obj,'attributes-type',AttributesType);
end;

constructor TAppleConfig.Create;
begin
  inherited Create;
  FAlertObject:=TJSONObject.Create;
  FFCMoptions:=CreateFCMOptions;
end;

destructor TAppleConfig.Destroy;
begin
  FreeAndNil(FAlertObject);
  FreeAndNil(FFCMOptions);
  inherited Destroy;
end;


function TAppleConfig.CreateFCMOptions : TAppleFCMOptions;

begin
  Result:=TAppleFCMOptions.Create;
end;

procedure TAppleConfig.Assign(Source: TPersistent);
var
  aSource: TAppleConfig absolute Source;
begin
  inherited Assign(Source);
  if Source is TAppleConfig then
  begin
    FreeAndNil(FAlertObject);
    FAlertObject:=(aSource.AlertObject.Clone as TJSONObject);
    Timestamp:=aSource.Timestamp;
    ThreadID:=aSource.ThreadID;
    TargetContentID:=aSource.TargetContentID;
    StaleDate:=aSource.StaleDate;
    RelevanceScore:=aSource.RelevanceScore;
    MutableContent:=aSource.MutableContent;
    InterruptionLevel:=aSource.InterruptionLevel;
    Filtercriteria:=aSource.Filtercriteria;
    FAlertObject:=aSource.FAlertObject;
    Event:=aSource.Event;
    DismissalDate:=aSource.DismissalDate;
    Category:=aSource.Category;
    Badge:=aSource.Badge;
    attributestype:=aSource.attributestype;
    Alert:=aSource.Alert;
    Sound:=aSource.Sound;
    FCMOptions:=aSource.FCMOptions;
  end;

end;

procedure TAppleConfig.ToJSON(Obj: TJSONObject);

var
  O : TJSONObject;

begin
  AddHeaders(Obj,true);
  O:=TJSONObject.Create;
  try
    AddPayLoad(O);
    if CustomPayLoad.Count>0 then
      AddCustomPayLoad(O);
  except
    O.Free;
    Raise;
  end;
  if O.Count=0 then
    FreeAndNil(O)
  else
    Obj.Add('payload',O);
  if fcmoptions.HaveData then
    begin
    O:=TJSONObject.Create;
    try
      FCMOptions.ToJSON(O);
      if O.Count=0 then
        FreeAndNil(O)
      else
        Obj.Add('fcm_options',O);
    except
      O.Free;
      Raise;
    end;
    end;
end;

procedure TAppleConfig.Clear;
begin
  inherited Clear;
  FAlert:='';
  FAlertObject.Clear;
  Fattributestype:='';
  FBadge:=0;
  fcategory:='';
  Fdismissaldate:=0;
  FEvent:='';
  FFiltercriteria:='';
  Finterruptionlevel:='';
  fmutablecontent:=0;
  Frelevancescore:='';
  FStaleDate:=0;
  Ftargetcontentid:='';
  Fthreadid:='';
  FTimeStamp:=0;
  FSound:='';
  FCMOptions.Clear;
end;

{ TAndroidConfig }

procedure TAndroidConfig.SetData(AValue: TJSONObject);
begin
  if FData=AValue then Exit;
  if OwnsData then
    FreeAndNil(FData);
  FData:=AValue;
end;

procedure TAndroidConfig.SetFCMOptions(AValue: TFCMOptions);
begin
  if FCMoptions=AValue then Exit;
  FCMoptions.Assign(AValue);
end;

procedure TAndroidConfig.setbodylocargs(AValue: TStrings);
begin
  if fbodylocargs=AValue then Exit;
  fbodylocargs.Assign(AValue);
end;

procedure TAndroidConfig.settitlelocargs(AValue: TStrings);
begin
  if ftitlelocargs=AValue then Exit;
  ftitlelocargs.Assign(AValue);
end;

procedure TAndroidConfig.Setvibratetimings(AValue: TStrings);
begin
  if Fvibratetimings=AValue then Exit;
  Fvibratetimings.Assign(AValue);
end;

procedure TAndroidConfig.Assign(Source: TPersistent);
var
  aSource: TAndroidConfig absolute Source;
begin
  inherited Assign(Source);
  if Source is TAndroidConfig then
  begin
    VibrateTimings:=aSource.VibrateTimings;
    TTL:=aSource.TTL;
    TitleLocKey:=aSource.TitleLocKey;
    TitleLocArgs:=aSource.TitleLocArgs;
    title:=aSource.title;
    Ticker:=aSource.Ticker;
    Tag:=aSource.Tag;
    Sticky:=aSource.Sticky;
    Sound:=aSource.Sound;
    RestrictedPackageName:=aSource.RestrictedPackageName;
    OwnsData:=aSource.OwnsData;
    NotificationCount:=aSource.NotificationCount;
    LocalOnly:=aSource.LocalOnly;
    Image:=aSource.Image;
    Icon:=aSource.Icon;
    Fvibratetimings:=aSource.Fvibratetimings;
    ftitlelocargs:=aSource.ftitlelocargs;
    Flightsettings:=aSource.Flightsettings;
    FData:=aSource.FData;
    fbodylocargs:=aSource.fbodylocargs;
    EventTime:=aSource.EventTime;
    DirectBootOk:=aSource.DirectBootOk;
    DefaultVibrateTimings:=aSource.DefaultVibrateTimings;
    DefaultSound:=aSource.DefaultSound;
    DefaultLightSettings:=aSource.DefaultLightSettings;
    data:=aSource.data;
    Color:=aSource.Color;
    CollapseKey:=aSource.CollapseKey;
    ClickAction:=aSource.ClickAction;
    ChannelID:=aSource.ChannelID;
    BodyLocKey:=aSource.BodyLocKey;
    BodyLocArgs:=aSource.BodyLocArgs;
    Body:=aSource.Body;
    FCMoptions:=aSource.FCMoptions;
    Visibility:=aSource.Visibility;
    NotificationPriority:=aSource.NotificationPriority;
  end;
end;

constructor TAndroidConfig.create;
begin
  inherited create;
  FBodyLocArgs:=TStringList.Create;
  FTitleLocArgs:=TStringList.Create;
  FVibrateTimings:=TStringList.Create;
  FFCMoptions:=TFCMOptions.Create;
  Flightsettings:=TJSONObject.Create;
end;

destructor TAndroidConfig.destroy;
begin
  If OwnsData then
    FreeAndNil(FData);
  FreeAndNil(Flightsettings);
  FreeAndNil(FBodyLocArgs);
  FreeAndNil(FTitleLocArgs);
  FreeAndNil(FVibrateTimings);
  FreeAndNil(FFCMoptions);
  inherited destroy;
end;

procedure TAndroidConfig.Clear;
begin
  inherited Clear;
  Fbody:='';
  fbodylocargs.Clear;
  fbodylockey:='';
  FChannelID:='';
  FClickAction:='';
  FCollapseKey:='';
  fcolor:='';
  if OwnsData then
    FreeAndNil(FData)
  else
    FData:=Nil;
  fdefaultlightsettings:=False;
  Fdefaultsound:=False;
  Fdefaultvibratetimings:=False;
  fdirectbootOK:=False;
  Feventtime:='';
  Ficon:='';
  fimage:='';
  Flightsettings.Clear;
  flocalonly:=False;
  fnotificationcount:=0;
  FOwnsData:=False;
  Frestrictedpackagename:='';
  fsound:='';
  fsticky:=False;
  ftag:='';
  fticker:='';
  Ftitle:='';
  ftitlelocargs.Clear;
  ftitlelockey:='';
  Fttl:='';
  Fvibratetimings.Clear;
  FFCMoptions.Clear;
  Visibility:=mvUnspecified;
  FNotificationPriority:=npUnspecified;
end;

procedure TAndroidConfig.ToJSON(Obj: TJSONObject);

Const
  MsgPrio : Array[TAndroidMessagePriority] of string = ('','normal','high');
  MsgVis : Array[TAndroidMessageVisibility] of integer = (0,0,1,-1);
  NotifPrio : Array[TAndroidNotificationPriority] of string = ('UNSPECIFIED','MIN','LOW','DEFAULT','HIGH','MAX');
var
  O : TJSONObject;

begin
  JSONAdd(Obj,'collapse_key',CollapseKey);
  JSONAdd(Obj,'priority',MsgPrio[priority]);
  JSONAdd(Obj,'ttl',ttl);
  JSONAdd(Obj,'restricted_package_name',RestrictedPackageName);
  if Assigned(Data) and (Data.Count>0) then
    obj.Add('data',Data.Clone);
  O:=JSONGetSub(Obj,'notification');
  JSONAdd(O,'title',Title);
  JSONAdd(O,'body',Body);
  JSONAdd(O,'icon',Icon);
  JSONAdd(O,'color',Color);
  JSONAdd(O,'sound',sound);
  JSONAdd(O,'tag',tag);
  JSONAdd(O,'click_action',ClickAction);
  JSONAdd(O,'body_loc_key',bodylockey);
  JSONAddStringsRaw(O,'body_loc_args',BodyLocArgs);
  JSONAdd(O,'title_loc_key',titlelockey);
  JSONAddStringsRaw(O,'title_loc_args',TitleLocArgs);
  JSONAdd(O,'channel_id',ChannelID);
  JSONAdd(O,'ticker',Ticker);
  JSONAdd(O,'sticky',Sticky);
  JSONAdd(O,'event_time',EventTime);
  JSONAdd(O,'local_only',LocalOnly);
  JSONAdd(O,'default_sound',defaultsound);
  JSONAdd(O,'default_vibrate_timings',DefaultVibrateTimings);
  JSONAdd(O,'default_light_settings',DefaultLightSettings);
  JSONAddStringsRaw(O,'vibrate_timings',VibrateTimings);
  if Visibility<>mvUnspecified then
    JSONAdd(O,'visibility',MsgVis[Visibility]);
  if NotificationPriority<>npUnspecified then
    JSONAdd(O,'notification_priority','PRIORITY_'+NotifPrio[NotificationPriority]);
  JSONAdd(O,'notification_count',NotificationCount);
  if lightsettings.Count>0 then
    O.Add('light_settings',lightsettings.Clone);
  JSONAdd(O,'image',image);
  JSONAdd(Obj,'direct_boot_ok',DirectBootOk);
  if FCMOptions.HaveData then
    begin
    O:=JSONGetSub(Obj,'fcm_options');
    FCMOptions.ToJSON(O);
    end;
end;

{ TWebPushFCMOptions }

procedure TWebPushFCMOptions.Assign(Source: TPersistent);
var
  aSource: TWebPushFCMOptions absolute source;
begin
  inherited Assign(Source);
  if Source is TWebPushFCMOptions then
  begin
    link:=aSource.link;
  end;
end;

procedure TWebPushFCMOptions.ToJSON(Obj: TJSONObject);
begin
  inherited ToJSON(Obj);
  JSONAdd(obj,'link',Link);
end;

function TWebPushFCMOptions.HaveData: Boolean;
begin
  Result:=inherited HaveData;
  Result:=Result and (link<>'');
end;

procedure TWebPushFCMOptions.Clear;
begin
  inherited Clear;
  link:='';
end;

{ TWebPushConfig }

procedure TWebPushConfig.SetData(AValue: TStrings);
begin
  if FData=AValue then Exit;
  FData.Assign(AValue);
end;

procedure TWebPushConfig.SetOptions(AValue: TWebPushFCMOptions);
begin
  if FOPtions=AValue then Exit;
  FOptions.Assign(AValue);
end;

procedure TWebPushConfig.Assign(Source: TPersistent);

var
  aSource: TWebPushConfig absolute source;

begin
  inherited Assign(Source);
  if Source is TWebPushConfig then
  begin
    FreeAndNil(FNotification);
    FNotification:=aSource.FNotification.Clone as TJSONObject;
    Data:=aSource.Data;
    FCMOptions:=aSource.FCMOptions;
  end;
end;

constructor TWebPushConfig.Create;
begin
  inherited Create;
  FData:=TStringList.Create;
  FData.NameValueSeparator:=':';
  FNotification:=TJSONObject.Create;
  FOptions:=TWebPushFCMOptions.Create;
end;

destructor TWebPushConfig.destroy;
begin
  FreeAndNil(FOptions);
  FreeAndNil(FHeaders);
  FreeAndNil(FData);
  FreeAndNil(FNotification);
  inherited destroy;
end;

procedure TWebPushConfig.Clear;
begin
  inherited Clear;
  Data.Clear;
  Notification.Clear;
  FCMOptions.Clear;
end;

procedure TWebPushConfig.ToJSON(Obj: TJSONObject);

var
  O : TJSONObject;

begin
  inherited ToJSON(Obj);
  AddHeaders(Obj,true);
  if FData.Count>0 then
    begin
    O:=JSONGetSub(obj,'data');
    JSONAddStrings(O,FData);
    end;
  if Notification.Count>0 then
    Obj.Add('notification',Notification.clone);
  if FCMOptions.HaveData then
    begin
    O:=JSONGetSub(obj,'fcm_options');
    FCMOptions.ToJSON(O);
    end;
end;

{ TFCMOptions }

procedure TFCMOptions.Assign(Source: TPersistent);
var
  aSource: TFCMOptions absolute Source;
begin
  inherited Assign(Source);
  if Source is TFCMOptions then
  begin
    AnalyticsLabel:=aSource.AnalyticsLabel;
  end;
end;

procedure TFCMOptions.ToJSON(Obj: TJSONObject);
begin
  JSONAdd(Obj,'analytics_label',AnalyticsLabel)
end;

function TFCMOptions.HaveData: Boolean;
begin
  Result:=(AnalyticsLabel<>'');
end;

procedure TFCMOptions.Clear;
begin
  inherited Clear;
  AnalyticsLabel:='';;
end;

{ TAppleFCMOptions }

procedure TAppleFCMOptions.Assign(Source: TPersistent);

var
  aSource : TAppleFCMOptions absolute Source;

begin
  inherited Assign(Source);
  if Source is TAppleFCMOptions then
    Image:=aSource.Image;
end;

procedure TAppleFCMOptions.ToJSON(Obj: TJSONObject);
begin
  inherited ToJSON(Obj);
  JSONAdd(Obj,'image',image)
end;

function TAppleFCMOptions.HaveData: Boolean;
begin
  Result:=inherited HaveData;
  Result:=Result or (Image<>'')
end;

procedure TAppleFCMOptions.Clear;
begin
  inherited Clear;
  Image:='';
end;

{ TPlatformOptions }

constructor TPlatformConfig.Create;
begin
  FCustomPayLoad:=TJSONObject.Create;
end;

destructor TPlatformConfig.Destroy;
begin
  FreeAndNil(FCustomPayLoad);
  Inherited;
end;

procedure TPlatformConfig.Clear;
begin
  FCustomPayLoad.Clear;
end;

{ TNotificationMessage }


function TNotificationMessage.IsSendOptionsStored: Boolean;
begin
  Result:=FSendOptions<>DefaultSendOptions;
end;

procedure TNotificationMessage.SetAndroidConfig(AValue: TAndroidConfig);
begin
  if FAndroidConfig=AValue then Exit;
  FAndroidConfig:=AValue;
end;

procedure TNotificationMessage.SetAppleConfig(AValue: TAppleConfig);
begin
  if FAppleConfig=AValue then Exit;
  FAppleConfig:=AValue;
end;

procedure TNotificationMessage.SetBody(AValue: string);
begin
  if FBody=AValue then Exit;
  FBody:=AValue;
end;

procedure TNotificationMessage.SetCMOptions(AValue: TFCMOptions);
begin
  if FFCMOptions=AValue then Exit;
  FFCMOptions.Assign(AValue);
end;

procedure TNotificationMessage.setdata(AValue: TStrings);
begin
  if FData=AValue then Exit;
  FData.Assign(aValue);
end;

procedure TNotificationMessage.SetImageURL(AValue: string);
begin
  if FImageURL=AValue then Exit;
  FImageURL:=AValue;
end;

procedure TNotificationMessage.SetTitle(AValue: string);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
end;

procedure TNotificationMessage.SetWebConfig(AValue: TWebPushConfig);
begin
  if FWebConfig=AValue then Exit;
  FWebConfig.Assign(aValue);
end;

function TNotificationMessage.CreateAppleConfig : TAppleConfig;

begin
  Result:=TAppleConfig.Create;
end;

function TNotificationMessage.CreateWebPushConfig : TWebPushConfig;

begin
  Result:=TWebPushConfig.Create;
end;

function TNotificationMessage.CreateFCMoptions: TFCMOptions;

begin
  Result:=TFCMOptions.Create;
end;

procedure TNotificationMessage.ToJSON(aObj: TJSONObject);

const
  RecipientNames : Array[TRecipientType] of string
                 = ('token','topic','condition');

var
  O : TJSONObject;

begin
  if Data.Count>0 then
    begin
    O:=JSONGetSub(aObj,'data');
    JSONAddStrings(o,Data);
    end;
  if not (nsDataOnly in SendOptions) then
    begin
    O:=JSONGetSub(aObj,'notification');
    JSONAdd(O,'title',Title);
    JSONAdd(O,'body',body);
    JSONAdd(O,'image',Image);
    end;
  if nsAndroid in SendOptions then
    begin
    O:=JSONGetSub(aObj,'android');
    AndroidConfig.ToJSON(O);
    end;
  if nsWebPush in SendOptions then
    begin
    O:=JSONGetSub(aObj,'webpush');
    WebPushConfig.ToJSON(O);
    end;
  if nsApple in SendOptions then
    begin
    O:=JSONGetSub(aObj,'apns');
    AppleConfig.ToJSON(O);
    end;
  if nsFCMOptions in SendOptions then
    begin
    O:=JSONGetSub(aObj,'fcm_options');
    FCMOptions.ToJSON(O);
    end;
  JSONAdd(aObj,RecipientNames[RecipientType],Recipient);
end;


function TNotificationMessage.CreateAndroidConfig : TAndroidConfig;

begin
  Result:=TAndroidConfig.Create;
end;

constructor TNotificationMessage.Create;
begin
  Inherited;
  FSendOptions:=DefaultSendOptions;
  FAppleConfig:=CreateAppleConfig;
  FAndroidConfig:=CreateAndroidConfig;
  FWebConfig:=CreateWebPushConfig;
  FFCMOptions:=CreateFCMoptions;
  FData:=TStringList.Create;
end;

destructor TNotificationMessage.destroy;
begin
  FreeAndNil(FFCMOptions);
  FreeAndNil(FData);
  FreeAndNil(FAppleConfig);
  FreeAndNil(FAndroidConfig);
  FreeAndNil(FWebConfig);
  inherited destroy;
end;

function TNotificationMessage.Encode: string;

var
  Obj : TJSONObject;

begin
  Obj:=TJSONObject.Create;
  try
    ToJSON(Obj);
    Result:=Obj.AsJSON;
  finally
    Obj.Free;
  end;
end;

procedure TNotificationMessage.Clear;
begin
  Recipient:='';
  Title:='';
  Body:='';
  Image:='';
  FData.Clear;
  AppleConfig.Clear;
  AndroidConfig.Clear;
  WebPushConfig.Clear;
end;

{ TBearerToken }

function TBearerToken.GetTokenDateTime: string;
begin
  result:=DateToISO8601(FTokenDateTime,False);
end;

procedure TBearerToken.SetTokenDateTime(AValue: string);
begin
  if not TryISO8601ToDate(aValue,FTokenDateTime,False) then
    FTokenDateTime:=0;
end;

procedure TBearerToken.SaveToStream(aStream: TStream);

var
  aJSON : TJSONStringType;

begin
  aJSON:=AsString;
  aStream.WriteBuffer(aJSON[1],Length(aJSON)*SizeOf(TJSONCharType));
end;

procedure TBearerToken.SaveToFile(const aFileName: String);

var
  F : TFileStream;

begin
  F:=TFileStream.Create(aFileName,fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TBearerToken.LoadFromStream(aStream: TStream);

var
  D : TJSONData;
  O : TJSONObject absolute D;

begin
  D:=GetJSON(aStream);
  try
    if D.JSONType<>jtObject then
      Raise EFCM.Create('Stream does not contain a valid JSON object');
    LoadFromJSON(O);
  finally
    D.Free;
  end;
end;

procedure TBearerToken.LoadFromFile(const aFileName: String);
var
  F : TFileStream;

begin
  F:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

function TBearerToken.IsValidData: Boolean;

begin
  Result:=(faccess_token<>'') or (frefresh_token<>'');
  Result:=Result and (FExpires_in<>0);
  Result:=Result and (Fid_token<>'');
  Result:=Result and (fscope<>'');
  Result:=Result and (ftoken_type<>'');
end;

function TBearerToken.IsExpired: Boolean;
begin
  Result:=(access_token='');
  Result:=Result or (SecondsBetween(Now,TokenDateTime) > expires_in);
end;

(*
function TBearerToken.Parse(const AJSON: string): Boolean;
var
  D : TJSONData;
  O : TJSONObject absolute D;

begin
  D:=GetJSON(aJSON);
  try
    Result:=D is TJSONObject;
    if Result then
      begin
      DoLoadFromJSON(O);
      Result:=IsValidData;
      end;
  finally
    D.Free;
  end;
end;
*)
end.

