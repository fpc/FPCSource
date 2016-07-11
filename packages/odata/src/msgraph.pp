unit msgraph;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, fpjson, restbase, odatabase, odataservice;

(*
  Options used to generate: 
  OData version : ODataV4
  BasecomplexType : TODataObject
  BaseEntityType : TODataEntity
  BaseEntityContainerType : TODataEntityContainer
  BaseServiceType : TODataService
  BaseEntitySetType : TODataEntitySet
  Aliases[0] : microsoft.graph=
  SchemaAncestor : TObject
  FieldPrefix : F
  ServiceSuffix : 
  EnumerationMode : emScoped
*)
type
  // Needed for binary data
  TByteArray = Array of byte;
  TInt16Array = Array of SmallInt;
  //
  TalternativeSecurityId = class;
  TalternativeSecurityIdArray = Array of TalternativeSecurityId;
  TlicenseUnitsDetail = class;
  TlicenseUnitsDetailArray = Array of TlicenseUnitsDetail;
  TservicePlanInfo = class;
  TservicePlanInfoArray = Array of TservicePlanInfo;
  TassignedPlan = class;
  TassignedPlanArray = Array of TassignedPlan;
  TprovisionedPlan = class;
  TprovisionedPlanArray = Array of TprovisionedPlan;
  TverifiedDomain = class;
  TverifiedDomainArray = Array of TverifiedDomain;
  TassignedLicense = class;
  TassignedLicenseArray = Array of TassignedLicense;
  TpasswordProfile = class;
  TpasswordProfileArray = Array of TpasswordProfile;
  Treminder = class;
  TreminderArray = Array of Treminder;
  TdateTimeTimeZone = class;
  TdateTimeTimeZoneArray = Array of TdateTimeTimeZone;
  Tlocation = class;
  TlocationArray = Array of Tlocation;
  TphysicalAddress = class;
  TphysicalAddressArray = Array of TphysicalAddress;
  TitemBody = class;
  TitemBodyArray = Array of TitemBody;
  Trecipient = class;
  TrecipientArray = Array of Trecipient;
  TemailAddress = class;
  TemailAddressArray = Array of TemailAddress;
  TresponseStatus = class;
  TresponseStatusArray = Array of TresponseStatus;
  TpatternedRecurrence = class;
  TpatternedRecurrenceArray = Array of TpatternedRecurrence;
  TrecurrencePattern = class;
  TrecurrencePatternArray = Array of TrecurrencePattern;
  TrecurrenceRange = class;
  TrecurrenceRangeArray = Array of TrecurrenceRange;
  Tattendee = class;
  TattendeeArray = Array of Tattendee;
  TidentitySet = class;
  TidentitySetArray = Array of TidentitySet;
  Tidentity = class;
  TidentityArray = Array of Tidentity;
  Tquota = class;
  TquotaArray = Array of Tquota;
  TitemReference = class;
  TitemReferenceArray = Array of TitemReference;
  Taudio = class;
  TaudioArray = Array of Taudio;
  Tdeleted = class;
  TdeletedArray = Array of Tdeleted;
  T_file = class;
  T_fileArray = Array of T_file;
  Thashes = class;
  ThashesArray = Array of Thashes;
  TfileSystemInfo = class;
  TfileSystemInfoArray = Array of TfileSystemInfo;
  Tfolder = class;
  TfolderArray = Array of Tfolder;
  Timage = class;
  TimageArray = Array of Timage;
  TgeoCoordinates = class;
  TgeoCoordinatesArray = Array of TgeoCoordinates;
  Tphoto = class;
  TphotoArray = Array of Tphoto;
  TremoteItem = class;
  TremoteItemArray = Array of TremoteItem;
  TsearchResult = class;
  TsearchResultArray = Array of TsearchResult;
  Tshared = class;
  TsharedArray = Array of Tshared;
  TspecialFolder = class;
  TspecialFolderArray = Array of TspecialFolder;
  Tvideo = class;
  TvideoArray = Array of Tvideo;
  Tpackage = class;
  TpackageArray = Array of Tpackage;
  TsharingInvitation = class;
  TsharingInvitationArray = Array of TsharingInvitation;
  TsharingLink = class;
  TsharingLinkArray = Array of TsharingLink;
  Tthumbnail = class;
  TthumbnailArray = Array of Tthumbnail;
  Tentity = class;
  TentityArray = Array of Tentity;
  TdirectoryObject = class;
  TdirectoryObjectArray = Array of TdirectoryObject;
  Tdevice = class;
  TdeviceArray = Array of Tdevice;
  TdirectoryRole = class;
  TdirectoryRoleArray = Array of TdirectoryRole;
  TdirectoryRoleTemplate = class;
  TdirectoryRoleTemplateArray = Array of TdirectoryRoleTemplate;
  Tgroup = class;
  TgroupArray = Array of Tgroup;
  TconversationThread = class;
  TconversationThreadArray = Array of TconversationThread;
  Tcalendar = class;
  TcalendarArray = Array of Tcalendar;
  ToutlookItem = class;
  ToutlookItemArray = Array of ToutlookItem;
  Tevent = class;
  TeventArray = Array of Tevent;
  Tconversation = class;
  TconversationArray = Array of Tconversation;
  TprofilePhoto = class;
  TprofilePhotoArray = Array of TprofilePhoto;
  Tdrive = class;
  TdriveArray = Array of Tdrive;
  TsubscribedSku = class;
  TsubscribedSkuArray = Array of TsubscribedSku;
  Torganization = class;
  TorganizationArray = Array of Torganization;
  Tuser = class;
  TuserArray = Array of Tuser;
  Tmessage = class;
  TmessageArray = Array of Tmessage;
  TmailFolder = class;
  TmailFolderArray = Array of TmailFolder;
  TcalendarGroup = class;
  TcalendarGroupArray = Array of TcalendarGroup;
  Tcontact = class;
  TcontactArray = Array of Tcontact;
  TcontactFolder = class;
  TcontactFolderArray = Array of TcontactFolder;
  Tattachment = class;
  TattachmentArray = Array of Tattachment;
  TfileAttachment = class;
  TfileAttachmentArray = Array of TfileAttachment;
  TitemAttachment = class;
  TitemAttachmentArray = Array of TitemAttachment;
  TeventMessage = class;
  TeventMessageArray = Array of TeventMessage;
  TreferenceAttachment = class;
  TreferenceAttachmentArray = Array of TreferenceAttachment;
  Tpost = class;
  TpostArray = Array of Tpost;
  TdriveItem = class;
  TdriveItemArray = Array of TdriveItem;
  Tpermission = class;
  TpermissionArray = Array of Tpermission;
  TthumbnailSet = class;
  TthumbnailSetArray = Array of TthumbnailSet;
  Tsubscription = class;
  TsubscriptionArray = Array of Tsubscription;
  TGraphService = class;
  TGraphServiceArray = Array of TGraphService;
  TdirectoryObjectsEntitySet = class;
  TdirectoryObjectsEntitySetArray = Array of TdirectoryObjectsEntitySet;
  TdevicesEntitySet = class;
  TdevicesEntitySetArray = Array of TdevicesEntitySet;
  TgroupsEntitySet = class;
  TgroupsEntitySetArray = Array of TgroupsEntitySet;
  TdirectoryRolesEntitySet = class;
  TdirectoryRolesEntitySetArray = Array of TdirectoryRolesEntitySet;
  TdirectoryRoleTemplatesEntitySet = class;
  TdirectoryRoleTemplatesEntitySetArray = Array of TdirectoryRoleTemplatesEntitySet;
  TorganizationEntitySet = class;
  TorganizationEntitySetArray = Array of TorganizationEntitySet;
  TsubscribedSkusEntitySet = class;
  TsubscribedSkusEntitySetArray = Array of TsubscribedSkusEntitySet;
  TusersEntitySet = class;
  TusersEntitySetArray = Array of TusersEntitySet;
  TdrivesEntitySet = class;
  TdrivesEntitySetArray = Array of TdrivesEntitySet;
  TsubscriptionsEntitySet = class;
  TsubscriptionsEntitySetArray = Array of TsubscriptionsEntitySet;
  TconversationThreadImplicitEntitySet = class;
  TconversationThreadImplicitEntitySetArray = Array of TconversationThreadImplicitEntitySet;
  TcalendarImplicitEntitySet = class;
  TcalendarImplicitEntitySetArray = Array of TcalendarImplicitEntitySet;
  TeventImplicitEntitySet = class;
  TeventImplicitEntitySetArray = Array of TeventImplicitEntitySet;
  TconversationImplicitEntitySet = class;
  TconversationImplicitEntitySetArray = Array of TconversationImplicitEntitySet;
  TprofilePhotoImplicitEntitySet = class;
  TprofilePhotoImplicitEntitySetArray = Array of TprofilePhotoImplicitEntitySet;
  TpostImplicitEntitySet = class;
  TpostImplicitEntitySetArray = Array of TpostImplicitEntitySet;
  TattachmentImplicitEntitySet = class;
  TattachmentImplicitEntitySetArray = Array of TattachmentImplicitEntitySet;
  TdriveItemImplicitEntitySet = class;
  TdriveItemImplicitEntitySetArray = Array of TdriveItemImplicitEntitySet;
  TmessageImplicitEntitySet = class;
  TmessageImplicitEntitySetArray = Array of TmessageImplicitEntitySet;
  TmailFolderImplicitEntitySet = class;
  TmailFolderImplicitEntitySetArray = Array of TmailFolderImplicitEntitySet;
  TcalendarGroupImplicitEntitySet = class;
  TcalendarGroupImplicitEntitySetArray = Array of TcalendarGroupImplicitEntitySet;
  TcontactImplicitEntitySet = class;
  TcontactImplicitEntitySetArray = Array of TcontactImplicitEntitySet;
  TcontactFolderImplicitEntitySet = class;
  TcontactFolderImplicitEntitySetArray = Array of TcontactFolderImplicitEntitySet;
  ToutlookItemImplicitEntitySet = class;
  ToutlookItemImplicitEntitySetArray = Array of ToutlookItemImplicitEntitySet;
  TpermissionImplicitEntitySet = class;
  TpermissionImplicitEntitySetArray = Array of TpermissionImplicitEntitySet;
  TthumbnailSetImplicitEntitySet = class;
  TthumbnailSetImplicitEntitySetArray = Array of TthumbnailSetImplicitEntitySet;
  TService = class;
  TServiceArray = Array of TService;
  //
  
  // Enumerations
  
  {$SCOPEDENUMS ON}
  TbodyType = (text,html);
  TbodyTypeArray = Array of TbodyType;
  Timportance = (low,normal,high);
  TimportanceArray = Array of Timportance;
  TcalendarColor = (lightBlue,lightGreen,lightOrange,lightGray,
                  lightYellow,lightTeal,lightPink,lightBrown,lightRed,
                  maxColor,auto);
  TcalendarColorArray = Array of TcalendarColor;
  TresponseType = (none,organizer,tentativelyAccepted,accepted,declined,
                 notResponded);
  TresponseTypeArray = Array of TresponseType;
  Tsensitivity = (normal,personal,&private,confidential);
  TsensitivityArray = Array of Tsensitivity;
  TrecurrencePatternType = (daily,weekly,absoluteMonthly,relativeMonthly,
                          absoluteYearly,relativeYearly);
  TrecurrencePatternTypeArray = Array of TrecurrencePatternType;
  TdayOfWeek = (sunday,monday,tuesday,wednesday,thursday,friday,saturday);
  TdayOfWeekArray = Array of TdayOfWeek;
  TweekIndex = (first,second,third,fourth,last);
  TweekIndexArray = Array of TweekIndex;
  TrecurrenceRangeType = (endDate,noEnd,numbered);
  TrecurrenceRangeTypeArray = Array of TrecurrenceRangeType;
  TfreeBusyStatus = (free,tentative,busy,oof,workingElsewhere,unknown);
  TfreeBusyStatusArray = Array of TfreeBusyStatus;
  TeventType = (singleInstance,occurrence,exception,seriesMaster);
  TeventTypeArray = Array of TeventType;
  TattendeeType = (required,optional,resource);
  TattendeeTypeArray = Array of TattendeeType;
  TmeetingMessageType = (none,meetingRequest,meetingCancelled,
                       meetingAccepted,meetingTenativelyAccepted,
                       meetingDeclined);
  TmeetingMessageTypeArray = Array of TmeetingMessageType;
  
  { --------------------------------------------------------------------
    microsoft.graph: alternativeSecurityId
    --------------------------------------------------------------------}
  
  TalternativeSecurityId = Class(TODataEntity)
  private
    F_type : TInt32;
    FidentityProvider : string;
    Fkey : TBinary;
    procedure Set_type(AIndex: Integer; const AValue: TInt32);
    procedure SetidentityProvider(AIndex: Integer; const AValue: string);
    procedure Setkey(AIndex: Integer; const AValue: TBinary);
  public
    class function ObjectRestKind : String;  Override;
    class function ExportPropertyName(const AName: String) : String
                                 ;  Override;
  published
    Property _type : TInt32 index 0 read F_type write Set_type;
    Property identityProvider : string index 8 read FidentityProvider write SetidentityProvider;
    Property key : TBinary index 16 read Fkey write Setkey;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: licenseUnitsDetail
    --------------------------------------------------------------------}
  
  TlicenseUnitsDetail = Class(TODataEntity)
  private
    Fenabled : TInt32;
    Fsuspended : TInt32;
    Fwarning : TInt32;
    procedure Setenabled(AIndex: Integer; const AValue: TInt32);
    procedure Setsuspended(AIndex: Integer; const AValue: TInt32);
    procedure Setwarning(AIndex: Integer; const AValue: TInt32);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property enabled : TInt32 index 0 read Fenabled write Setenabled;
    Property suspended : TInt32 index 8 read Fsuspended write Setsuspended;
    Property warning : TInt32 index 16 read Fwarning write Setwarning;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: servicePlanInfo
    --------------------------------------------------------------------}
  
  TservicePlanInfo = Class(TODataEntity)
  private
    FservicePlanId : TGUIDString;
    FservicePlanName : string;
    FprovisioningStatus : string;
    FappliesTo : string;
    procedure SetservicePlanId(AIndex: Integer; const AValue: TGUIDString);
    procedure SetservicePlanName(AIndex: Integer; const AValue: string);
    procedure SetprovisioningStatus(AIndex: Integer; const AValue: string);
    procedure SetappliesTo(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property servicePlanId : TGUIDString index 0 read FservicePlanId write SetservicePlanId;
    Property servicePlanName : string index 8 read FservicePlanName write SetservicePlanName;
    Property provisioningStatus : string index 16 read FprovisioningStatus write SetprovisioningStatus;
    Property appliesTo : string index 24 read FappliesTo write SetappliesTo;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: assignedPlan
    --------------------------------------------------------------------}
  
  TassignedPlan = Class(TODataEntity)
  private
    FassignedDateTime : TDateTime;
    FcapabilityStatus : string;
    Fservice : string;
    FservicePlanId : TGUIDString;
    procedure SetassignedDateTime(AIndex: Integer; const AValue: TDateTime);
    procedure SetcapabilityStatus(AIndex: Integer; const AValue: string);
    procedure Setservice(AIndex: Integer; const AValue: string);
    procedure SetservicePlanId(AIndex: Integer; const AValue: TGUIDString);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property assignedDateTime : TDateTime index 0 read FassignedDateTime write SetassignedDateTime;
    Property capabilityStatus : string index 8 read FcapabilityStatus write SetcapabilityStatus;
    Property service : string index 16 read Fservice write Setservice;
    Property servicePlanId : TGUIDString index 24 read FservicePlanId write SetservicePlanId;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: provisionedPlan
    --------------------------------------------------------------------}
  
  TprovisionedPlan = Class(TODataEntity)
  private
    FcapabilityStatus : string;
    FprovisioningStatus : string;
    Fservice : string;
    procedure SetcapabilityStatus(AIndex: Integer; const AValue: string);
    procedure SetprovisioningStatus(AIndex: Integer; const AValue: string);
    procedure Setservice(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property capabilityStatus : string index 0 read FcapabilityStatus write SetcapabilityStatus;
    Property provisioningStatus : string index 8 read FprovisioningStatus write SetprovisioningStatus;
    Property service : string index 16 read Fservice write Setservice;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: verifiedDomain
    --------------------------------------------------------------------}
  
  TverifiedDomain = Class(TODataEntity)
  private
    Fcapabilities : string;
    FisDefault : boolean;
    FisInitial : boolean;
    Fname : string;
    F_type : string;
    procedure Setcapabilities(AIndex: Integer; const AValue: string);
    procedure SetisDefault(AIndex: Integer; const AValue: boolean);
    procedure SetisInitial(AIndex: Integer; const AValue: boolean);
    procedure Setname(AIndex: Integer; const AValue: string);
    procedure Set_type(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
    class function ExportPropertyName(const AName: String) : String
                                 ;  Override;
  published
    Property capabilities : string index 0 read Fcapabilities write Setcapabilities;
    Property isDefault : boolean index 8 read FisDefault write SetisDefault;
    Property isInitial : boolean index 16 read FisInitial write SetisInitial;
    Property name : string index 24 read Fname write Setname;
    Property _type : string index 32 read F_type write Set_type;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: assignedLicense
    --------------------------------------------------------------------}
  
  TassignedLicense = Class(TODataEntity)
  private
    FdisabledPlans : TGuidStringArray;
    FskuId : TGUIDString;
    procedure SetdisabledPlans(AIndex: Integer; 
                          const AValue: TGuidStringArray);
    procedure SetskuId(AIndex: Integer; const AValue: TGUIDString);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    class function ObjectRestKind : String;  Override;
  published
    Property disabledPlans : TGuidStringArray index 0 read FdisabledPlans write SetdisabledPlans;
    Property skuId : TGUIDString index 8 read FskuId write SetskuId;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: passwordProfile
    --------------------------------------------------------------------}
  
  TpasswordProfile = Class(TODataEntity)
  private
    Fpassword : string;
    FforceChangePasswordNextSignIn : boolean;
    procedure Setpassword(AIndex: Integer; const AValue: string);
    procedure SetforceChangePasswordNextSignIn(AIndex: Integer; 
                                          const AValue: boolean);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property password : string index 0 read Fpassword write Setpassword;
    Property forceChangePasswordNextSignIn : boolean index 8 read FforceChangePasswordNextSignIn write SetforceChangePasswordNextSignIn;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: reminder
    --------------------------------------------------------------------}
  
  Treminder = Class(TODataEntity)
  private
    FeventId : string;
    FeventStartTime : TdateTimeTimeZone;
    FeventEndTime : TdateTimeTimeZone;
    FchangeKey : string;
    FeventSubject : string;
    FeventLocation : Tlocation;
    FeventWebLink : string;
    FreminderFireTime : TdateTimeTimeZone;
    procedure SeteventId(AIndex: Integer; const AValue: string);
    procedure SeteventStartTime(AIndex: Integer; 
                           const AValue: TdateTimeTimeZone);
    procedure SeteventEndTime(AIndex: Integer; 
                         const AValue: TdateTimeTimeZone);
    procedure SetchangeKey(AIndex: Integer; const AValue: string);
    procedure SeteventSubject(AIndex: Integer; const AValue: string);
    procedure SeteventLocation(AIndex: Integer; const AValue: Tlocation);
    procedure SeteventWebLink(AIndex: Integer; const AValue: string);
    procedure SetreminderFireTime(AIndex: Integer; 
                             const AValue: TdateTimeTimeZone);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property eventId : string index 0 read FeventId write SeteventId;
    Property eventStartTime : TdateTimeTimeZone index 8 read FeventStartTime write SeteventStartTime;
    Property eventEndTime : TdateTimeTimeZone index 16 read FeventEndTime write SeteventEndTime;
    Property changeKey : string index 24 read FchangeKey write SetchangeKey;
    Property eventSubject : string index 32 read FeventSubject write SeteventSubject;
    Property eventLocation : Tlocation index 40 read FeventLocation write SeteventLocation;
    Property eventWebLink : string index 48 read FeventWebLink write SeteventWebLink;
    Property reminderFireTime : TdateTimeTimeZone index 56 read FreminderFireTime write SetreminderFireTime;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: dateTimeTimeZone
    --------------------------------------------------------------------}
  
  TdateTimeTimeZone = Class(TODataEntity)
  private
    FdateTime : string;
    FtimeZone : string;
    procedure SetdateTime(AIndex: Integer; const AValue: string);
    procedure SettimeZone(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property dateTime : string index 0 read FdateTime write SetdateTime;
    Property timeZone : string index 8 read FtimeZone write SettimeZone;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: location
    --------------------------------------------------------------------}
  
  Tlocation = Class(TODataEntity)
  private
    FdisplayName : string;
    Faddress : TphysicalAddress;
    procedure SetdisplayName(AIndex: Integer; const AValue: string);
    procedure Setaddress(AIndex: Integer; const AValue: TphysicalAddress);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property displayName : string index 0 read FdisplayName write SetdisplayName;
    Property address : TphysicalAddress index 8 read Faddress write Setaddress;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: physicalAddress
    --------------------------------------------------------------------}
  
  TphysicalAddress = Class(TODataEntity)
  private
    Fstreet : string;
    Fcity : string;
    Fstate : string;
    FcountryOrRegion : string;
    FpostalCode : string;
    procedure Setstreet(AIndex: Integer; const AValue: string);
    procedure Setcity(AIndex: Integer; const AValue: string);
    procedure Setstate(AIndex: Integer; const AValue: string);
    procedure SetcountryOrRegion(AIndex: Integer; const AValue: string);
    procedure SetpostalCode(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property street : string index 0 read Fstreet write Setstreet;
    Property city : string index 8 read Fcity write Setcity;
    Property state : string index 16 read Fstate write Setstate;
    Property countryOrRegion : string index 24 read FcountryOrRegion write SetcountryOrRegion;
    Property postalCode : string index 32 read FpostalCode write SetpostalCode;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: itemBody
    --------------------------------------------------------------------}
  
  TitemBody = Class(TODataEntity)
  private
    FcontentType : TbodyType;
    Fcontent : string;
    procedure SetcontentType(AIndex: Integer; const AValue: TbodyType);
    procedure Setcontent(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property contentType : TbodyType index 0 read FcontentType write SetcontentType;
    Property content : string index 8 read Fcontent write Setcontent;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: recipient
    --------------------------------------------------------------------}
  
  Trecipient = Class(TODataEntity)
  private
    FemailAddress : TemailAddress;
    procedure SetemailAddress(AIndex: Integer; const AValue: TemailAddress);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property emailAddress : TemailAddress index 0 read FemailAddress write SetemailAddress;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: emailAddress
    --------------------------------------------------------------------}
  
  TemailAddress = Class(TODataEntity)
  private
    Fname : string;
    Faddress : string;
    procedure Setname(AIndex: Integer; const AValue: string);
    procedure Setaddress(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property name : string index 0 read Fname write Setname;
    Property address : string index 8 read Faddress write Setaddress;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: responseStatus
    --------------------------------------------------------------------}
  
  TresponseStatus = Class(TODataEntity)
  private
    Fresponse : TresponseType;
    Ftime : TDateTime;
    procedure Setresponse(AIndex: Integer; const AValue: TresponseType);
    procedure Settime(AIndex: Integer; const AValue: TDateTime);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property response : TresponseType index 0 read Fresponse write Setresponse;
    Property time : TDateTime index 8 read Ftime write Settime;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: patternedRecurrence
    --------------------------------------------------------------------}
  
  TpatternedRecurrence = Class(TODataEntity)
  private
    Fpattern : TrecurrencePattern;
    Frange : TrecurrenceRange;
    procedure Setpattern(AIndex: Integer; const AValue: TrecurrencePattern);
    procedure Setrange(AIndex: Integer; const AValue: TrecurrenceRange);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property pattern : TrecurrencePattern index 0 read Fpattern write Setpattern;
    Property range : TrecurrenceRange index 8 read Frange write Setrange;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: recurrencePattern
    --------------------------------------------------------------------}
  
  TrecurrencePattern = Class(TODataEntity)
  private
    F_type : TrecurrencePatternType;
    Finterval : TInt32;
    Fmonth : TInt32;
    FdayOfMonth : TInt32;
    FdaysOfWeek : TdayOfWeekArray;
    FfirstDayOfWeek : TdayOfWeek;
    Findex : TweekIndex;
    procedure Set_type(AIndex: Integer; 
                  const AValue: TrecurrencePatternType);
    procedure Setinterval(AIndex: Integer; const AValue: TInt32);
    procedure Setmonth(AIndex: Integer; const AValue: TInt32);
    procedure SetdayOfMonth(AIndex: Integer; const AValue: TInt32);
    procedure SetdaysOfWeek(AIndex: Integer; const AValue: TdayOfWeekArray);
    procedure SetfirstDayOfWeek(AIndex: Integer; const AValue: TdayOfWeek);
    procedure Setindex(AIndex: Integer; const AValue: TweekIndex);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    class function ObjectRestKind : String;  Override;
    class function ExportPropertyName(const AName: String) : String
                                 ;  Override;
  published
    Property _type : TrecurrencePatternType index 0 read F_type write Set_type;
    Property interval : TInt32 index 8 read Finterval write Setinterval;
    Property month : TInt32 index 16 read Fmonth write Setmonth;
    Property dayOfMonth : TInt32 index 24 read FdayOfMonth write SetdayOfMonth;
    Property daysOfWeek : TdayOfWeekArray index 32 read FdaysOfWeek write SetdaysOfWeek;
    Property firstDayOfWeek : TdayOfWeek index 40 read FfirstDayOfWeek write SetfirstDayOfWeek;
    Property index : TweekIndex index 48 read Findex write Setindex;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: recurrenceRange
    --------------------------------------------------------------------}
  
  TrecurrenceRange = Class(TODataEntity)
  private
    F_type : TrecurrenceRangeType;
    FstartDate : TDate;
    FendDate : TDate;
    FrecurrenceTimeZone : string;
    FnumberOfOccurrences : TInt32;
    procedure Set_type(AIndex: Integer; const AValue: TrecurrenceRangeType);
    procedure SetstartDate(AIndex: Integer; const AValue: TDate);
    procedure SetendDate(AIndex: Integer; const AValue: TDate);
    procedure SetrecurrenceTimeZone(AIndex: Integer; const AValue: string);
    procedure SetnumberOfOccurrences(AIndex: Integer; const AValue: TInt32);
  public
    class function ObjectRestKind : String;  Override;
    class function ExportPropertyName(const AName: String) : String
                                 ;  Override;
  published
    Property _type : TrecurrenceRangeType index 0 read F_type write Set_type;
    Property startDate : TDate index 8 read FstartDate write SetstartDate;
    Property endDate : TDate index 16 read FendDate write SetendDate;
    Property recurrenceTimeZone : string index 24 read FrecurrenceTimeZone write SetrecurrenceTimeZone;
    Property numberOfOccurrences : TInt32 index 32 read FnumberOfOccurrences write SetnumberOfOccurrences;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: attendee
    --------------------------------------------------------------------}
  
  Tattendee = Class(Trecipient)
  private
    Fstatus : TresponseStatus;
    F_type : TattendeeType;
    procedure Setstatus(AIndex: Integer; const AValue: TresponseStatus);
    procedure Set_type(AIndex: Integer; const AValue: TattendeeType);
  public
    class function ObjectRestKind : String;  Override;
    class function ExportPropertyName(const AName: String) : String
                                 ;  Override;
  published
    Property status : TresponseStatus index 8 read Fstatus write Setstatus;
    Property _type : TattendeeType index 16 read F_type write Set_type;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: identitySet
    --------------------------------------------------------------------}
  
  TidentitySet = Class(TODataEntity)
  private
    Fapplication : Tidentity;
    Fdevice : Tidentity;
    Fuser : Tidentity;
    procedure Setapplication(AIndex: Integer; const AValue: Tidentity);
    procedure Setdevice(AIndex: Integer; const AValue: Tidentity);
    procedure Setuser(AIndex: Integer; const AValue: Tidentity);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property application : Tidentity index 0 read Fapplication write Setapplication;
    Property device : Tidentity index 8 read Fdevice write Setdevice;
    Property user : Tidentity index 16 read Fuser write Setuser;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: identity
    --------------------------------------------------------------------}
  
  Tidentity = Class(TODataEntity)
  private
    FdisplayName : string;
    Fid : string;
    procedure SetdisplayName(AIndex: Integer; const AValue: string);
    procedure Setid(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property displayName : string index 0 read FdisplayName write SetdisplayName;
    Property id : string index 8 read Fid write Setid;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: quota
    --------------------------------------------------------------------}
  
  Tquota = Class(TODataEntity)
  private
    Fdeleted : int64;
    Fremaining : int64;
    Fstate : string;
    Ftotal : int64;
    Fused : int64;
    procedure Setdeleted(AIndex: Integer; const AValue: int64);
    procedure Setremaining(AIndex: Integer; const AValue: int64);
    procedure Setstate(AIndex: Integer; const AValue: string);
    procedure Settotal(AIndex: Integer; const AValue: int64);
    procedure Setused(AIndex: Integer; const AValue: int64);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property deleted : int64 index 0 read Fdeleted write Setdeleted;
    Property remaining : int64 index 8 read Fremaining write Setremaining;
    Property state : string index 16 read Fstate write Setstate;
    Property total : int64 index 24 read Ftotal write Settotal;
    Property used : int64 index 32 read Fused write Setused;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: itemReference
    --------------------------------------------------------------------}
  
  TitemReference = Class(TODataEntity)
  private
    FdriveId : string;
    Fid : string;
    Fpath : string;
    procedure SetdriveId(AIndex: Integer; const AValue: string);
    procedure Setid(AIndex: Integer; const AValue: string);
    procedure Setpath(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property driveId : string index 0 read FdriveId write SetdriveId;
    Property id : string index 8 read Fid write Setid;
    Property path : string index 16 read Fpath write Setpath;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: audio
    --------------------------------------------------------------------}
  
  Taudio = Class(TODataEntity)
  private
    Falbum : string;
    FalbumArtist : string;
    Fartist : string;
    Fbitrate : int64;
    Fcomposers : string;
    Fcopyright : string;
    Fdisc : TInt16;
    FdiscCount : TInt16;
    Fduration : int64;
    Fgenre : string;
    FhasDrm : boolean;
    FisVariableBitrate : boolean;
    Ftitle : string;
    Ftrack : TInt32;
    FtrackCount : TInt32;
    Fyear : TInt32;
    procedure Setalbum(AIndex: Integer; const AValue: string);
    procedure SetalbumArtist(AIndex: Integer; const AValue: string);
    procedure Setartist(AIndex: Integer; const AValue: string);
    procedure Setbitrate(AIndex: Integer; const AValue: int64);
    procedure Setcomposers(AIndex: Integer; const AValue: string);
    procedure Setcopyright(AIndex: Integer; const AValue: string);
    procedure Setdisc(AIndex: Integer; const AValue: TInt16);
    procedure SetdiscCount(AIndex: Integer; const AValue: TInt16);
    procedure Setduration(AIndex: Integer; const AValue: int64);
    procedure Setgenre(AIndex: Integer; const AValue: string);
    procedure SethasDrm(AIndex: Integer; const AValue: boolean);
    procedure SetisVariableBitrate(AIndex: Integer; const AValue: boolean);
    procedure Settitle(AIndex: Integer; const AValue: string);
    procedure Settrack(AIndex: Integer; const AValue: TInt32);
    procedure SettrackCount(AIndex: Integer; const AValue: TInt32);
    procedure Setyear(AIndex: Integer; const AValue: TInt32);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property album : string index 0 read Falbum write Setalbum;
    Property albumArtist : string index 8 read FalbumArtist write SetalbumArtist;
    Property artist : string index 16 read Fartist write Setartist;
    Property bitrate : int64 index 24 read Fbitrate write Setbitrate;
    Property composers : string index 32 read Fcomposers write Setcomposers;
    Property copyright : string index 40 read Fcopyright write Setcopyright;
    Property disc : TInt16 index 48 read Fdisc write Setdisc;
    Property discCount : TInt16 index 56 read FdiscCount write SetdiscCount;
    Property duration : int64 index 64 read Fduration write Setduration;
    Property genre : string index 72 read Fgenre write Setgenre;
    Property hasDrm : boolean index 80 read FhasDrm write SethasDrm;
    Property isVariableBitrate : boolean index 88 read FisVariableBitrate write SetisVariableBitrate;
    Property title : string index 96 read Ftitle write Settitle;
    Property track : TInt32 index 104 read Ftrack write Settrack;
    Property trackCount : TInt32 index 112 read FtrackCount write SettrackCount;
    Property year : TInt32 index 120 read Fyear write Setyear;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: deleted
    --------------------------------------------------------------------}
  
  Tdeleted = Class(TODataEntity)
  private
    Fstate : string;
    procedure Setstate(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property state : string index 0 read Fstate write Setstate;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: file
    --------------------------------------------------------------------}
  
  T_file = Class(TODataEntity)
  private
    Fhashes : Thashes;
    FmimeType : string;
    procedure Sethashes(AIndex: Integer; const AValue: Thashes);
    procedure SetmimeType(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property hashes : Thashes index 0 read Fhashes write Sethashes;
    Property mimeType : string index 8 read FmimeType write SetmimeType;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: hashes
    --------------------------------------------------------------------}
  
  Thashes = Class(TODataEntity)
  private
    Fcrc32Hash : string;
    Fsha1Hash : string;
    procedure Setcrc32Hash(AIndex: Integer; const AValue: string);
    procedure Setsha1Hash(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property crc32Hash : string index 0 read Fcrc32Hash write Setcrc32Hash;
    Property sha1Hash : string index 8 read Fsha1Hash write Setsha1Hash;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: fileSystemInfo
    --------------------------------------------------------------------}
  
  TfileSystemInfo = Class(TODataEntity)
  private
    FcreatedDateTime : TDateTime;
    FlastModifiedDateTime : TDateTime;
    procedure SetcreatedDateTime(AIndex: Integer; const AValue: TDateTime);
    procedure SetlastModifiedDateTime(AIndex: Integer; 
                                 const AValue: TDateTime);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property createdDateTime : TDateTime index 0 read FcreatedDateTime write SetcreatedDateTime;
    Property lastModifiedDateTime : TDateTime index 8 read FlastModifiedDateTime write SetlastModifiedDateTime;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: folder
    --------------------------------------------------------------------}
  
  Tfolder = Class(TODataEntity)
  private
    FchildCount : TInt32;
    procedure SetchildCount(AIndex: Integer; const AValue: TInt32);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property childCount : TInt32 index 0 read FchildCount write SetchildCount;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: image
    --------------------------------------------------------------------}
  
  Timage = Class(TODataEntity)
  private
    Fheight : TInt32;
    Fwidth : TInt32;
    procedure Setheight(AIndex: Integer; const AValue: TInt32);
    procedure Setwidth(AIndex: Integer; const AValue: TInt32);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property height : TInt32 index 0 read Fheight write Setheight;
    Property width : TInt32 index 8 read Fwidth write Setwidth;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: geoCoordinates
    --------------------------------------------------------------------}
  
  TgeoCoordinates = Class(TODataEntity)
  private
    Faltitude : Double;
    Flatitude : Double;
    Flongitude : Double;
    procedure Setaltitude(AIndex: Integer; const AValue: Double);
    procedure Setlatitude(AIndex: Integer; const AValue: Double);
    procedure Setlongitude(AIndex: Integer; const AValue: Double);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property altitude : Double index 0 read Faltitude write Setaltitude;
    Property latitude : Double index 8 read Flatitude write Setlatitude;
    Property longitude : Double index 16 read Flongitude write Setlongitude;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: photo
    --------------------------------------------------------------------}
  
  Tphoto = Class(TODataEntity)
  private
    FcameraMake : string;
    FcameraModel : string;
    FexposureDenominator : Double;
    FexposureNumerator : Double;
    FfocalLength : Double;
    FfNumber : Double;
    FtakenDateTime : TDateTime;
    Fiso : TInt32;
    procedure SetcameraMake(AIndex: Integer; const AValue: string);
    procedure SetcameraModel(AIndex: Integer; const AValue: string);
    procedure SetexposureDenominator(AIndex: Integer; const AValue: Double);
    procedure SetexposureNumerator(AIndex: Integer; const AValue: Double);
    procedure SetfocalLength(AIndex: Integer; const AValue: Double);
    procedure SetfNumber(AIndex: Integer; const AValue: Double);
    procedure SettakenDateTime(AIndex: Integer; const AValue: TDateTime);
    procedure Setiso(AIndex: Integer; const AValue: TInt32);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property cameraMake : string index 0 read FcameraMake write SetcameraMake;
    Property cameraModel : string index 8 read FcameraModel write SetcameraModel;
    Property exposureDenominator : Double index 16 read FexposureDenominator write SetexposureDenominator;
    Property exposureNumerator : Double index 24 read FexposureNumerator write SetexposureNumerator;
    Property focalLength : Double index 32 read FfocalLength write SetfocalLength;
    Property fNumber : Double index 40 read FfNumber write SetfNumber;
    Property takenDateTime : TDateTime index 48 read FtakenDateTime write SettakenDateTime;
    Property iso : TInt32 index 56 read Fiso write Setiso;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: remoteItem
    --------------------------------------------------------------------}
  
  TremoteItem = Class(TODataEntity)
  private
    F_file : T_file;
    FfileSystemInfo : TfileSystemInfo;
    Ffolder : Tfolder;
    Fid : string;
    Fname : string;
    FparentReference : TitemReference;
    Fsize : int64;
    procedure Set_file(AIndex: Integer; const AValue: T_file);
    procedure SetfileSystemInfo(AIndex: Integer; 
                           const AValue: TfileSystemInfo);
    procedure Setfolder(AIndex: Integer; const AValue: Tfolder);
    procedure Setid(AIndex: Integer; const AValue: string);
    procedure Setname(AIndex: Integer; const AValue: string);
    procedure SetparentReference(AIndex: Integer; 
                            const AValue: TitemReference);
    procedure Setsize(AIndex: Integer; const AValue: int64);
  public
    class function ObjectRestKind : String;  Override;
    class function ExportPropertyName(const AName: String) : String
                                 ;  Override;
  published
    Property _file : T_file index 0 read F_file write Set_file;
    Property fileSystemInfo : TfileSystemInfo index 8 read FfileSystemInfo write SetfileSystemInfo;
    Property folder : Tfolder index 16 read Ffolder write Setfolder;
    Property id : string index 24 read Fid write Setid;
    Property name : string index 32 read Fname write Setname;
    Property parentReference : TitemReference index 40 read FparentReference write SetparentReference;
    Property size : int64 index 48 read Fsize write Setsize;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: searchResult
    --------------------------------------------------------------------}
  
  TsearchResult = Class(TODataEntity)
  private
    FonClickTelemetryUrl : string;
    procedure SetonClickTelemetryUrl(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property onClickTelemetryUrl : string index 0 read FonClickTelemetryUrl write SetonClickTelemetryUrl;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: shared
    --------------------------------------------------------------------}
  
  Tshared = Class(TODataEntity)
  private
    Fowner : TidentitySet;
    Fscope : string;
    procedure Setowner(AIndex: Integer; const AValue: TidentitySet);
    procedure Setscope(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property owner : TidentitySet index 0 read Fowner write Setowner;
    Property scope : string index 8 read Fscope write Setscope;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: specialFolder
    --------------------------------------------------------------------}
  
  TspecialFolder = Class(TODataEntity)
  private
    Fname : string;
    procedure Setname(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property name : string index 0 read Fname write Setname;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: video
    --------------------------------------------------------------------}
  
  Tvideo = Class(TODataEntity)
  private
    Fbitrate : TInt32;
    Fduration : int64;
    Fheight : TInt32;
    Fwidth : TInt32;
    procedure Setbitrate(AIndex: Integer; const AValue: TInt32);
    procedure Setduration(AIndex: Integer; const AValue: int64);
    procedure Setheight(AIndex: Integer; const AValue: TInt32);
    procedure Setwidth(AIndex: Integer; const AValue: TInt32);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property bitrate : TInt32 index 0 read Fbitrate write Setbitrate;
    Property duration : int64 index 8 read Fduration write Setduration;
    Property height : TInt32 index 16 read Fheight write Setheight;
    Property width : TInt32 index 24 read Fwidth write Setwidth;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: package
    --------------------------------------------------------------------}
  
  Tpackage = Class(TODataEntity)
  private
    F_type : string;
    procedure Set_type(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
    class function ExportPropertyName(const AName: String) : String
                                 ;  Override;
  published
    Property _type : string index 0 read F_type write Set_type;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: sharingInvitation
    --------------------------------------------------------------------}
  
  TsharingInvitation = Class(TODataEntity)
  private
    Femail : string;
    FinvitedBy : TidentitySet;
    FredeemedBy : string;
    FsignInRequired : boolean;
    procedure Setemail(AIndex: Integer; const AValue: string);
    procedure SetinvitedBy(AIndex: Integer; const AValue: TidentitySet);
    procedure SetredeemedBy(AIndex: Integer; const AValue: string);
    procedure SetsignInRequired(AIndex: Integer; const AValue: boolean);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property email : string index 0 read Femail write Setemail;
    Property invitedBy : TidentitySet index 8 read FinvitedBy write SetinvitedBy;
    Property redeemedBy : string index 16 read FredeemedBy write SetredeemedBy;
    Property signInRequired : boolean index 24 read FsignInRequired write SetsignInRequired;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: sharingLink
    --------------------------------------------------------------------}
  
  TsharingLink = Class(TODataEntity)
  private
    Fapplication : Tidentity;
    F_type : string;
    FwebUrl : string;
    procedure Setapplication(AIndex: Integer; const AValue: Tidentity);
    procedure Set_type(AIndex: Integer; const AValue: string);
    procedure SetwebUrl(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
    class function ExportPropertyName(const AName: String) : String
                                 ;  Override;
  published
    Property application : Tidentity index 0 read Fapplication write Setapplication;
    Property _type : string index 8 read F_type write Set_type;
    Property webUrl : string index 16 read FwebUrl write SetwebUrl;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: thumbnail
    --------------------------------------------------------------------}
  
  Tthumbnail = Class(TODataEntity)
  private
    Fcontent : TStream;
    Fheight : TInt32;
    Furl : string;
    Fwidth : TInt32;
    procedure Setcontent(AIndex: Integer; const AValue: TStream);
    procedure Setheight(AIndex: Integer; const AValue: TInt32);
    procedure Seturl(AIndex: Integer; const AValue: string);
    procedure Setwidth(AIndex: Integer; const AValue: TInt32);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property content : TStream index 0 read Fcontent write Setcontent;
    Property height : TInt32 index 8 read Fheight write Setheight;
    Property url : string index 16 read Furl write Seturl;
    Property width : TInt32 index 24 read Fwidth write Setwidth;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: entity
    --------------------------------------------------------------------}
  
  Tentity = Class(TODataEntity)
  private
    Fid : string;
    procedure Setid(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
    function KeyAsURLPart : String;  Override;
  published
    Property id : string index 0 read Fid write Setid;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: directoryObject
    --------------------------------------------------------------------}
  
  TdirectoryObject = Class(Tentity)
  public
    function checkMemberGroups(AService: TODataService; 
                          groupIds: TStringArray) : TStringArray;
    function getMemberGroups(AService: TODataService; 
                        securityEnabledOnly: boolean) : TStringArray;
    function getMemberObjects(AService: TODataService; 
                         securityEnabledOnly: boolean) : TStringArray;
    class function ObjectRestKind : String;  Override;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: device
    --------------------------------------------------------------------}
  
  Tdevice = Class(TdirectoryObject)
  private
    FaccountEnabled : boolean;
    FalternativeSecurityIds : TalternativeSecurityIdArray;
    FapproximateLastSignInDateTime : TDateTime;
    FdeviceId : string;
    FdeviceMetadata : string;
    FdeviceVersion : TInt32;
    FdisplayName : string;
    FisCompliant : boolean;
    FisManaged : boolean;
    FonPremisesLastSyncDateTime : TDateTime;
    FonPremisesSyncEnabled : boolean;
    FoperatingSystem : string;
    FoperatingSystemVersion : string;
    FphysicalIds : TStringArray;
    FtrustType : string;
    procedure SetaccountEnabled(AIndex: Integer; const AValue: boolean);
    procedure SetalternativeSecurityIds(AIndex: Integer; 
                                   const AValue: TalternativeSecurityIdArray);
    procedure SetapproximateLastSignInDateTime(AIndex: Integer; 
                                          const AValue: TDateTime);
    procedure SetdeviceId(AIndex: Integer; const AValue: string);
    procedure SetdeviceMetadata(AIndex: Integer; const AValue: string);
    procedure SetdeviceVersion(AIndex: Integer; const AValue: TInt32);
    procedure SetdisplayName(AIndex: Integer; const AValue: string);
    procedure SetisCompliant(AIndex: Integer; const AValue: boolean);
    procedure SetisManaged(AIndex: Integer; const AValue: boolean);
    procedure SetonPremisesLastSyncDateTime(AIndex: Integer; 
                                       const AValue: TDateTime);
    procedure SetonPremisesSyncEnabled(AIndex: Integer; 
                                  const AValue: boolean);
    procedure SetoperatingSystem(AIndex: Integer; const AValue: string);
    procedure SetoperatingSystemVersion(AIndex: Integer; 
                                   const AValue: string);
    procedure SetphysicalIds(AIndex: Integer; const AValue: TStringArray);
    procedure SettrustType(AIndex: Integer; const AValue: string);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    class function ObjectRestKind : String;  Override;
    function registeredOwners(AService: TODataService)
                          : TdirectoryObjectsEntitySet;
    function registeredUsers(AService: TODataService)
                         : TdirectoryObjectsEntitySet;
  published
    Property accountEnabled : boolean index 8 read FaccountEnabled write SetaccountEnabled;
    Property alternativeSecurityIds : TalternativeSecurityIdArray index 16 read FalternativeSecurityIds write SetalternativeSecurityIds;
    Property approximateLastSignInDateTime : TDateTime index 24 read FapproximateLastSignInDateTime write SetapproximateLastSignInDateTime;
    Property deviceId : string index 32 read FdeviceId write SetdeviceId;
    Property deviceMetadata : string index 40 read FdeviceMetadata write SetdeviceMetadata;
    Property deviceVersion : TInt32 index 48 read FdeviceVersion write SetdeviceVersion;
    Property displayName : string index 56 read FdisplayName write SetdisplayName;
    Property isCompliant : boolean index 64 read FisCompliant write SetisCompliant;
    Property isManaged : boolean index 72 read FisManaged write SetisManaged;
    Property onPremisesLastSyncDateTime : TDateTime index 80 read FonPremisesLastSyncDateTime write SetonPremisesLastSyncDateTime;
    Property onPremisesSyncEnabled : boolean index 88 read FonPremisesSyncEnabled write SetonPremisesSyncEnabled;
    Property operatingSystem : string index 96 read FoperatingSystem write SetoperatingSystem;
    Property operatingSystemVersion : string index 104 read FoperatingSystemVersion write SetoperatingSystemVersion;
    Property physicalIds : TStringArray index 112 read FphysicalIds write SetphysicalIds;
    Property trustType : string index 120 read FtrustType write SettrustType;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: directoryRole
    --------------------------------------------------------------------}
  
  TdirectoryRole = Class(TdirectoryObject)
  private
    Fdescription : string;
    FdisplayName : string;
    FroleTemplateId : string;
    procedure Setdescription(AIndex: Integer; const AValue: string);
    procedure SetdisplayName(AIndex: Integer; const AValue: string);
    procedure SetroleTemplateId(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
    function members(AService: TODataService) : TdirectoryObjectsEntitySet;
  published
    Property description : string index 8 read Fdescription write Setdescription;
    Property displayName : string index 16 read FdisplayName write SetdisplayName;
    Property roleTemplateId : string index 24 read FroleTemplateId write SetroleTemplateId;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: directoryRoleTemplate
    --------------------------------------------------------------------}
  
  TdirectoryRoleTemplate = Class(TdirectoryObject)
  private
    Fdescription : string;
    FdisplayName : string;
    procedure Setdescription(AIndex: Integer; const AValue: string);
    procedure SetdisplayName(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property description : string index 8 read Fdescription write Setdescription;
    Property displayName : string index 16 read FdisplayName write SetdisplayName;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: group
    --------------------------------------------------------------------}
  
  Tgroup = Class(TdirectoryObject)
  private
    Fdescription : string;
    FdisplayName : string;
    FgroupTypes : TStringArray;
    Fmail : string;
    FmailEnabled : boolean;
    FmailNickname : string;
    FonPremisesLastSyncDateTime : TDateTime;
    FonPremisesSecurityIdentifier : string;
    FonPremisesSyncEnabled : boolean;
    FproxyAddresses : TStringArray;
    FsecurityEnabled : boolean;
    Fvisibility : string;
    FallowExternalSenders : boolean;
    FautoSubscribeNewMembers : boolean;
    FisSubscribedByMail : boolean;
    FunseenCount : TInt32;
    procedure Setdescription(AIndex: Integer; const AValue: string);
    procedure SetdisplayName(AIndex: Integer; const AValue: string);
    procedure SetgroupTypes(AIndex: Integer; const AValue: TStringArray);
    procedure Setmail(AIndex: Integer; const AValue: string);
    procedure SetmailEnabled(AIndex: Integer; const AValue: boolean);
    procedure SetmailNickname(AIndex: Integer; const AValue: string);
    procedure SetonPremisesLastSyncDateTime(AIndex: Integer; 
                                       const AValue: TDateTime);
    procedure SetonPremisesSecurityIdentifier(AIndex: Integer; 
                                         const AValue: string);
    procedure SetonPremisesSyncEnabled(AIndex: Integer; 
                                  const AValue: boolean);
    procedure SetproxyAddresses(AIndex: Integer; const AValue: TStringArray);
    procedure SetsecurityEnabled(AIndex: Integer; const AValue: boolean);
    procedure Setvisibility(AIndex: Integer; const AValue: string);
    procedure SetallowExternalSenders(AIndex: Integer; 
                                 const AValue: boolean);
    procedure SetautoSubscribeNewMembers(AIndex: Integer; 
                                    const AValue: boolean);
    procedure SetisSubscribedByMail(AIndex: Integer; const AValue: boolean);
    procedure SetunseenCount(AIndex: Integer; const AValue: TInt32);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    procedure subscribeByMail(AService: TODataService);
    procedure unsubscribeByMail(AService: TODataService);
    procedure addFavorite(AService: TODataService);
    procedure removeFavorite(AService: TODataService);
    procedure resetUnseenCount(AService: TODataService);
    class function ObjectRestKind : String;  Override;
    function members(AService: TODataService) : TdirectoryObjectsEntitySet;
    function memberOf(AService: TODataService) : TdirectoryObjectsEntitySet;
    function createdOnBehalfOf(AService: TODataService) : TdirectoryObject;
    function owners(AService: TODataService) : TdirectoryObjectsEntitySet;
    function threads(AService: TODataService)
                 : TconversationThreadImplicitEntitySet;
    function calendar(AService: TODataService) : Tcalendar;
    function calendarView(AService: TODataService) : TeventImplicitEntitySet;
    function events(AService: TODataService) : TeventImplicitEntitySet;
    function conversations(AService: TODataService)
                       : TconversationImplicitEntitySet;
    function photo(AService: TODataService) : TprofilePhoto;
    function acceptedSenders(AService: TODataService)
                         : TdirectoryObjectsEntitySet;
    function rejectedSenders(AService: TODataService)
                         : TdirectoryObjectsEntitySet;
    function drive(AService: TODataService) : Tdrive;
  published
    Property description : string index 8 read Fdescription write Setdescription;
    Property displayName : string index 16 read FdisplayName write SetdisplayName;
    Property groupTypes : TStringArray index 24 read FgroupTypes write SetgroupTypes;
    Property mail : string index 32 read Fmail write Setmail;
    Property mailEnabled : boolean index 40 read FmailEnabled write SetmailEnabled;
    Property mailNickname : string index 48 read FmailNickname write SetmailNickname;
    Property onPremisesLastSyncDateTime : TDateTime index 56 read FonPremisesLastSyncDateTime write SetonPremisesLastSyncDateTime;
    Property onPremisesSecurityIdentifier : string index 64 read FonPremisesSecurityIdentifier write SetonPremisesSecurityIdentifier;
    Property onPremisesSyncEnabled : boolean index 72 read FonPremisesSyncEnabled write SetonPremisesSyncEnabled;
    Property proxyAddresses : TStringArray index 80 read FproxyAddresses write SetproxyAddresses;
    Property securityEnabled : boolean index 88 read FsecurityEnabled write SetsecurityEnabled;
    Property visibility : string index 96 read Fvisibility write Setvisibility;
    Property allowExternalSenders : boolean index 104 read FallowExternalSenders write SetallowExternalSenders;
    Property autoSubscribeNewMembers : boolean index 112 read FautoSubscribeNewMembers write SetautoSubscribeNewMembers;
    Property isSubscribedByMail : boolean index 120 read FisSubscribedByMail write SetisSubscribedByMail;
    Property unseenCount : TInt32 index 128 read FunseenCount write SetunseenCount;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: conversationThread
    --------------------------------------------------------------------}
  
  TconversationThread = Class(Tentity)
  private
    FtoRecipients : TrecipientArray;
    Ftopic : string;
    FhasAttachments : boolean;
    FlastDeliveredDateTime : TDateTime;
    FuniqueSenders : TStringArray;
    FccRecipients : TrecipientArray;
    Fpreview : string;
    FisLocked : boolean;
    procedure SettoRecipients(AIndex: Integer; 
                         const AValue: TrecipientArray);
    procedure Settopic(AIndex: Integer; const AValue: string);
    procedure SethasAttachments(AIndex: Integer; const AValue: boolean);
    procedure SetlastDeliveredDateTime(AIndex: Integer; 
                                  const AValue: TDateTime);
    procedure SetuniqueSenders(AIndex: Integer; const AValue: TStringArray);
    procedure SetccRecipients(AIndex: Integer; 
                         const AValue: TrecipientArray);
    procedure Setpreview(AIndex: Integer; const AValue: string);
    procedure SetisLocked(AIndex: Integer; const AValue: boolean);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    procedure reply(AService: TODataService; _Post: Tpost);
    class function ObjectRestKind : String;  Override;
    function posts(AService: TODataService) : TpostImplicitEntitySet;
  published
    Property toRecipients : TrecipientArray index 8 read FtoRecipients write SettoRecipients;
    Property topic : string index 16 read Ftopic write Settopic;
    Property hasAttachments : boolean index 24 read FhasAttachments write SethasAttachments;
    Property lastDeliveredDateTime : TDateTime index 32 read FlastDeliveredDateTime write SetlastDeliveredDateTime;
    Property uniqueSenders : TStringArray index 40 read FuniqueSenders write SetuniqueSenders;
    Property ccRecipients : TrecipientArray index 48 read FccRecipients write SetccRecipients;
    Property preview : string index 56 read Fpreview write Setpreview;
    Property isLocked : boolean index 64 read FisLocked write SetisLocked;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: calendar
    --------------------------------------------------------------------}
  
  Tcalendar = Class(Tentity)
  private
    Fname : string;
    Fcolor : TcalendarColor;
    FchangeKey : string;
    procedure Setname(AIndex: Integer; const AValue: string);
    procedure Setcolor(AIndex: Integer; const AValue: TcalendarColor);
    procedure SetchangeKey(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
    function events(AService: TODataService) : TeventImplicitEntitySet;
    function calendarView(AService: TODataService) : TeventImplicitEntitySet;
  published
    Property name : string index 8 read Fname write Setname;
    Property color : TcalendarColor index 16 read Fcolor write Setcolor;
    Property changeKey : string index 24 read FchangeKey write SetchangeKey;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: outlookItem
    --------------------------------------------------------------------}
  
  ToutlookItem = Class(Tentity)
  private
    FcreatedDateTime : TDateTime;
    FlastModifiedDateTime : TDateTime;
    FchangeKey : string;
    Fcategories : TStringArray;
    procedure SetcreatedDateTime(AIndex: Integer; const AValue: TDateTime);
    procedure SetlastModifiedDateTime(AIndex: Integer; 
                                 const AValue: TDateTime);
    procedure SetchangeKey(AIndex: Integer; const AValue: string);
    procedure Setcategories(AIndex: Integer; const AValue: TStringArray);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    class function ObjectRestKind : String;  Override;
  published
    Property createdDateTime : TDateTime index 8 read FcreatedDateTime write SetcreatedDateTime;
    Property lastModifiedDateTime : TDateTime index 16 read FlastModifiedDateTime write SetlastModifiedDateTime;
    Property changeKey : string index 24 read FchangeKey write SetchangeKey;
    Property categories : TStringArray index 32 read Fcategories write Setcategories;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: event
    --------------------------------------------------------------------}
  
  Tevent = Class(ToutlookItem)
  private
    ForiginalStartTimeZone : string;
    ForiginalEndTimeZone : string;
    FresponseStatus : TresponseStatus;
    FiCalUId : string;
    FreminderMinutesBeforeStart : TInt32;
    FisReminderOn : boolean;
    FhasAttachments : boolean;
    Fsubject : string;
    Fbody : TitemBody;
    FbodyPreview : string;
    Fimportance : Timportance;
    Fsensitivity : Tsensitivity;
    Fstart : TdateTimeTimeZone;
    ForiginalStart : TDateTime;
    F_end : TdateTimeTimeZone;
    Flocation : Tlocation;
    FisAllDay : boolean;
    FisCancelled : boolean;
    FisOrganizer : boolean;
    Frecurrence : TpatternedRecurrence;
    FresponseRequested : boolean;
    FseriesMasterId : string;
    FshowAs : TfreeBusyStatus;
    F_type : TeventType;
    Fattendees : TattendeeArray;
    Forganizer : Trecipient;
    FwebLink : string;
    procedure SetoriginalStartTimeZone(AIndex: Integer; 
                                  const AValue: string);
    procedure SetoriginalEndTimeZone(AIndex: Integer; const AValue: string);
    procedure SetresponseStatus(AIndex: Integer; 
                           const AValue: TresponseStatus);
    procedure SetiCalUId(AIndex: Integer; const AValue: string);
    procedure SetreminderMinutesBeforeStart(AIndex: Integer; 
                                       const AValue: TInt32);
    procedure SetisReminderOn(AIndex: Integer; const AValue: boolean);
    procedure SethasAttachments(AIndex: Integer; const AValue: boolean);
    procedure Setsubject(AIndex: Integer; const AValue: string);
    procedure Setbody(AIndex: Integer; const AValue: TitemBody);
    procedure SetbodyPreview(AIndex: Integer; const AValue: string);
    procedure Setimportance(AIndex: Integer; const AValue: Timportance);
    procedure Setsensitivity(AIndex: Integer; const AValue: Tsensitivity);
    procedure Setstart(AIndex: Integer; const AValue: TdateTimeTimeZone);
    procedure SetoriginalStart(AIndex: Integer; const AValue: TDateTime);
    procedure Set_end(AIndex: Integer; const AValue: TdateTimeTimeZone);
    procedure Setlocation(AIndex: Integer; const AValue: Tlocation);
    procedure SetisAllDay(AIndex: Integer; const AValue: boolean);
    procedure SetisCancelled(AIndex: Integer; const AValue: boolean);
    procedure SetisOrganizer(AIndex: Integer; const AValue: boolean);
    procedure Setrecurrence(AIndex: Integer; 
                       const AValue: TpatternedRecurrence);
    procedure SetresponseRequested(AIndex: Integer; const AValue: boolean);
    procedure SetseriesMasterId(AIndex: Integer; const AValue: string);
    procedure SetshowAs(AIndex: Integer; const AValue: TfreeBusyStatus);
    procedure Set_type(AIndex: Integer; const AValue: TeventType);
    procedure Setattendees(AIndex: Integer; const AValue: TattendeeArray);
    procedure Setorganizer(AIndex: Integer; const AValue: Trecipient);
    procedure SetwebLink(AIndex: Integer; const AValue: string);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    procedure accept(AService: TODataService; Comment: string; 
                SendResponse: boolean);
    procedure decline(AService: TODataService; Comment: string; 
                 SendResponse: boolean);
    procedure tentativelyAccept(AService: TODataService; Comment: string; 
                           SendResponse: boolean);
    procedure snoozeReminder(AService: TODataService; 
                        NewReminderTime: TdateTimeTimeZone);
    procedure dismissReminder(AService: TODataService);
    class function ObjectRestKind : String;  Override;
    class function ExportPropertyName(const AName: String) : String
                                 ;  Override;
    function calendar(AService: TODataService) : Tcalendar;
    function instances(AService: TODataService) : TeventImplicitEntitySet;
    function attachments(AService: TODataService)
                     : TattachmentImplicitEntitySet;
  published
    Property originalStartTimeZone : string index 40 read ForiginalStartTimeZone write SetoriginalStartTimeZone;
    Property originalEndTimeZone : string index 48 read ForiginalEndTimeZone write SetoriginalEndTimeZone;
    Property responseStatus : TresponseStatus index 56 read FresponseStatus write SetresponseStatus;
    Property iCalUId : string index 64 read FiCalUId write SetiCalUId;
    Property reminderMinutesBeforeStart : TInt32 index 72 read FreminderMinutesBeforeStart write SetreminderMinutesBeforeStart;
    Property isReminderOn : boolean index 80 read FisReminderOn write SetisReminderOn;
    Property hasAttachments : boolean index 88 read FhasAttachments write SethasAttachments;
    Property subject : string index 96 read Fsubject write Setsubject;
    Property body : TitemBody index 104 read Fbody write Setbody;
    Property bodyPreview : string index 112 read FbodyPreview write SetbodyPreview;
    Property importance : Timportance index 120 read Fimportance write Setimportance;
    Property sensitivity : Tsensitivity index 128 read Fsensitivity write Setsensitivity;
    Property start : TdateTimeTimeZone index 136 read Fstart write Setstart;
    Property originalStart : TDateTime index 144 read ForiginalStart write SetoriginalStart;
    Property _end : TdateTimeTimeZone index 152 read F_end write Set_end;
    Property location : Tlocation index 160 read Flocation write Setlocation;
    Property isAllDay : boolean index 168 read FisAllDay write SetisAllDay;
    Property isCancelled : boolean index 176 read FisCancelled write SetisCancelled;
    Property isOrganizer : boolean index 184 read FisOrganizer write SetisOrganizer;
    Property recurrence : TpatternedRecurrence index 192 read Frecurrence write Setrecurrence;
    Property responseRequested : boolean index 200 read FresponseRequested write SetresponseRequested;
    Property seriesMasterId : string index 208 read FseriesMasterId write SetseriesMasterId;
    Property showAs : TfreeBusyStatus index 216 read FshowAs write SetshowAs;
    Property _type : TeventType index 224 read F_type write Set_type;
    Property attendees : TattendeeArray index 232 read Fattendees write Setattendees;
    Property organizer : Trecipient index 240 read Forganizer write Setorganizer;
    Property webLink : string index 248 read FwebLink write SetwebLink;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: conversation
    --------------------------------------------------------------------}
  
  Tconversation = Class(Tentity)
  private
    Ftopic : string;
    FhasAttachments : boolean;
    FlastDeliveredDateTime : TDateTime;
    FuniqueSenders : TStringArray;
    Fpreview : string;
    procedure Settopic(AIndex: Integer; const AValue: string);
    procedure SethasAttachments(AIndex: Integer; const AValue: boolean);
    procedure SetlastDeliveredDateTime(AIndex: Integer; 
                                  const AValue: TDateTime);
    procedure SetuniqueSenders(AIndex: Integer; const AValue: TStringArray);
    procedure Setpreview(AIndex: Integer; const AValue: string);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    class function ObjectRestKind : String;  Override;
    function threads(AService: TODataService)
                 : TconversationThreadImplicitEntitySet;
  published
    Property topic : string index 8 read Ftopic write Settopic;
    Property hasAttachments : boolean index 16 read FhasAttachments write SethasAttachments;
    Property lastDeliveredDateTime : TDateTime index 24 read FlastDeliveredDateTime write SetlastDeliveredDateTime;
    Property uniqueSenders : TStringArray index 32 read FuniqueSenders write SetuniqueSenders;
    Property preview : string index 40 read Fpreview write Setpreview;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: profilePhoto
    --------------------------------------------------------------------}
  
  TprofilePhoto = Class(Tentity)
  private
    Fheight : TInt32;
    Fwidth : TInt32;
    procedure Setheight(AIndex: Integer; const AValue: TInt32);
    procedure Setwidth(AIndex: Integer; const AValue: TInt32);
  public
    class function ObjectRestKind : String;  Override;
    procedure GetStream(AService: TODataService; AContentType: String; 
                   AStream: TStream);
    procedure SetStream(AService: TODataService; AContentType: String; 
                   AStream: TStream);
  published
    Property height : TInt32 index 8 read Fheight write Setheight;
    Property width : TInt32 index 16 read Fwidth write Setwidth;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: drive
    --------------------------------------------------------------------}
  
  Tdrive = Class(Tentity)
  private
    FdriveType : string;
    Fowner : TidentitySet;
    Fquota : Tquota;
    procedure SetdriveType(AIndex: Integer; const AValue: string);
    procedure Setowner(AIndex: Integer; const AValue: TidentitySet);
    procedure Setquota(AIndex: Integer; const AValue: Tquota);
  public
    function recent(AService: TODataService) : TdriveItemArray;
    function sharedWithMe(AService: TODataService) : TdriveItemArray;
    class function ObjectRestKind : String;  Override;
    function items(AService: TODataService) : TdriveItemImplicitEntitySet;
    function special(AService: TODataService) : TdriveItemImplicitEntitySet;
    function root(AService: TODataService) : TdriveItem;
  published
    Property driveType : string index 8 read FdriveType write SetdriveType;
    Property owner : TidentitySet index 16 read Fowner write Setowner;
    Property quota : Tquota index 24 read Fquota write Setquota;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: subscribedSku
    --------------------------------------------------------------------}
  
  TsubscribedSku = Class(Tentity)
  private
    FcapabilityStatus : string;
    FconsumedUnits : TInt32;
    FprepaidUnits : TlicenseUnitsDetail;
    FservicePlans : TservicePlanInfoArray;
    FskuId : TGUIDString;
    FskuPartNumber : string;
    FappliesTo : string;
    procedure SetcapabilityStatus(AIndex: Integer; const AValue: string);
    procedure SetconsumedUnits(AIndex: Integer; const AValue: TInt32);
    procedure SetprepaidUnits(AIndex: Integer; 
                         const AValue: TlicenseUnitsDetail);
    procedure SetservicePlans(AIndex: Integer; 
                         const AValue: TservicePlanInfoArray);
    procedure SetskuId(AIndex: Integer; const AValue: TGUIDString);
    procedure SetskuPartNumber(AIndex: Integer; const AValue: string);
    procedure SetappliesTo(AIndex: Integer; const AValue: string);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    class function ObjectRestKind : String;  Override;
  published
    Property capabilityStatus : string index 8 read FcapabilityStatus write SetcapabilityStatus;
    Property consumedUnits : TInt32 index 16 read FconsumedUnits write SetconsumedUnits;
    Property prepaidUnits : TlicenseUnitsDetail index 24 read FprepaidUnits write SetprepaidUnits;
    Property servicePlans : TservicePlanInfoArray index 32 read FservicePlans write SetservicePlans;
    Property skuId : TGUIDString index 40 read FskuId write SetskuId;
    Property skuPartNumber : string index 48 read FskuPartNumber write SetskuPartNumber;
    Property appliesTo : string index 56 read FappliesTo write SetappliesTo;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: organization
    --------------------------------------------------------------------}
  
  Torganization = Class(TdirectoryObject)
  private
    FassignedPlans : TassignedPlanArray;
    FbusinessPhones : TStringArray;
    Fcity : string;
    Fcountry : string;
    FcountryLetterCode : string;
    FdisplayName : string;
    FmarketingNotificationEmails : TStringArray;
    FonPremisesLastSyncDateTime : TDateTime;
    FonPremisesSyncEnabled : boolean;
    FpostalCode : string;
    FpreferredLanguage : string;
    FprovisionedPlans : TprovisionedPlanArray;
    FsecurityComplianceNotificationMails : TStringArray;
    FsecurityComplianceNotificationPhones : TStringArray;
    Fstate : string;
    Fstreet : string;
    FtechnicalNotificationMails : TStringArray;
    FverifiedDomains : TverifiedDomainArray;
    procedure SetassignedPlans(AIndex: Integer; 
                          const AValue: TassignedPlanArray);
    procedure SetbusinessPhones(AIndex: Integer; const AValue: TStringArray);
    procedure Setcity(AIndex: Integer; const AValue: string);
    procedure Setcountry(AIndex: Integer; const AValue: string);
    procedure SetcountryLetterCode(AIndex: Integer; const AValue: string);
    procedure SetdisplayName(AIndex: Integer; const AValue: string);
    procedure SetmarketingNotificationEmails(AIndex: Integer; 
                                        const AValue: TStringArray);
    procedure SetonPremisesLastSyncDateTime(AIndex: Integer; 
                                       const AValue: TDateTime);
    procedure SetonPremisesSyncEnabled(AIndex: Integer; 
                                  const AValue: boolean);
    procedure SetpostalCode(AIndex: Integer; const AValue: string);
    procedure SetpreferredLanguage(AIndex: Integer; const AValue: string);
    procedure SetprovisionedPlans(AIndex: Integer; 
                             const AValue: TprovisionedPlanArray);
    procedure SetsecurityComplianceNotificationMails(AIndex: Integer; 
                                                const AValue: TStringArray);
    procedure SetsecurityComplianceNotificationPhones(AIndex: Integer; 
                                                 const AValue: TStringArray);
    procedure Setstate(AIndex: Integer; const AValue: string);
    procedure Setstreet(AIndex: Integer; const AValue: string);
    procedure SettechnicalNotificationMails(AIndex: Integer; 
                                       const AValue: TStringArray);
    procedure SetverifiedDomains(AIndex: Integer; 
                            const AValue: TverifiedDomainArray);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    class function ObjectRestKind : String;  Override;
  published
    Property assignedPlans : TassignedPlanArray index 8 read FassignedPlans write SetassignedPlans;
    Property businessPhones : TStringArray index 16 read FbusinessPhones write SetbusinessPhones;
    Property city : string index 24 read Fcity write Setcity;
    Property country : string index 32 read Fcountry write Setcountry;
    Property countryLetterCode : string index 40 read FcountryLetterCode write SetcountryLetterCode;
    Property displayName : string index 48 read FdisplayName write SetdisplayName;
    Property marketingNotificationEmails : TStringArray index 56 read FmarketingNotificationEmails write SetmarketingNotificationEmails;
    Property onPremisesLastSyncDateTime : TDateTime index 64 read FonPremisesLastSyncDateTime write SetonPremisesLastSyncDateTime;
    Property onPremisesSyncEnabled : boolean index 72 read FonPremisesSyncEnabled write SetonPremisesSyncEnabled;
    Property postalCode : string index 80 read FpostalCode write SetpostalCode;
    Property preferredLanguage : string index 88 read FpreferredLanguage write SetpreferredLanguage;
    Property provisionedPlans : TprovisionedPlanArray index 96 read FprovisionedPlans write SetprovisionedPlans;
    Property securityComplianceNotificationMails : TStringArray index 104 read FsecurityComplianceNotificationMails write SetsecurityComplianceNotificationMails;
    Property securityComplianceNotificationPhones : TStringArray index 112 read FsecurityComplianceNotificationPhones write SetsecurityComplianceNotificationPhones;
    Property state : string index 120 read Fstate write Setstate;
    Property street : string index 128 read Fstreet write Setstreet;
    Property technicalNotificationMails : TStringArray index 136 read FtechnicalNotificationMails write SettechnicalNotificationMails;
    Property verifiedDomains : TverifiedDomainArray index 144 read FverifiedDomains write SetverifiedDomains;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: user
    --------------------------------------------------------------------}
  
  Tuser = Class(TdirectoryObject)
  private
    FaccountEnabled : boolean;
    FassignedLicenses : TassignedLicenseArray;
    FassignedPlans : TassignedPlanArray;
    FbusinessPhones : TStringArray;
    Fcity : string;
    FcompanyName : string;
    Fcountry : string;
    Fdepartment : string;
    FdisplayName : string;
    FgivenName : string;
    FjobTitle : string;
    Fmail : string;
    FmailNickname : string;
    FmobilePhone : string;
    FonPremisesImmutableId : string;
    FonPremisesLastSyncDateTime : TDateTime;
    FonPremisesSecurityIdentifier : string;
    FonPremisesSyncEnabled : boolean;
    FpasswordPolicies : string;
    FpasswordProfile : TpasswordProfile;
    FofficeLocation : string;
    FpostalCode : string;
    FpreferredLanguage : string;
    FprovisionedPlans : TprovisionedPlanArray;
    FproxyAddresses : TStringArray;
    Fstate : string;
    FstreetAddress : string;
    Fsurname : string;
    FusageLocation : string;
    FuserPrincipalName : string;
    FuserType : string;
    FaboutMe : string;
    Fbirthday : TDateTime;
    FhireDate : TDateTime;
    Finterests : TStringArray;
    FmySite : string;
    FpastProjects : TStringArray;
    FpreferredName : string;
    Fresponsibilities : TStringArray;
    Fschools : TStringArray;
    Fskills : TStringArray;
    procedure SetaccountEnabled(AIndex: Integer; const AValue: boolean);
    procedure SetassignedLicenses(AIndex: Integer; 
                             const AValue: TassignedLicenseArray);
    procedure SetassignedPlans(AIndex: Integer; 
                          const AValue: TassignedPlanArray);
    procedure SetbusinessPhones(AIndex: Integer; const AValue: TStringArray);
    procedure Setcity(AIndex: Integer; const AValue: string);
    procedure SetcompanyName(AIndex: Integer; const AValue: string);
    procedure Setcountry(AIndex: Integer; const AValue: string);
    procedure Setdepartment(AIndex: Integer; const AValue: string);
    procedure SetdisplayName(AIndex: Integer; const AValue: string);
    procedure SetgivenName(AIndex: Integer; const AValue: string);
    procedure SetjobTitle(AIndex: Integer; const AValue: string);
    procedure Setmail(AIndex: Integer; const AValue: string);
    procedure SetmailNickname(AIndex: Integer; const AValue: string);
    procedure SetmobilePhone(AIndex: Integer; const AValue: string);
    procedure SetonPremisesImmutableId(AIndex: Integer; 
                                  const AValue: string);
    procedure SetonPremisesLastSyncDateTime(AIndex: Integer; 
                                       const AValue: TDateTime);
    procedure SetonPremisesSecurityIdentifier(AIndex: Integer; 
                                         const AValue: string);
    procedure SetonPremisesSyncEnabled(AIndex: Integer; 
                                  const AValue: boolean);
    procedure SetpasswordPolicies(AIndex: Integer; const AValue: string);
    procedure SetpasswordProfile(AIndex: Integer; 
                            const AValue: TpasswordProfile);
    procedure SetofficeLocation(AIndex: Integer; const AValue: string);
    procedure SetpostalCode(AIndex: Integer; const AValue: string);
    procedure SetpreferredLanguage(AIndex: Integer; const AValue: string);
    procedure SetprovisionedPlans(AIndex: Integer; 
                             const AValue: TprovisionedPlanArray);
    procedure SetproxyAddresses(AIndex: Integer; const AValue: TStringArray);
    procedure Setstate(AIndex: Integer; const AValue: string);
    procedure SetstreetAddress(AIndex: Integer; const AValue: string);
    procedure Setsurname(AIndex: Integer; const AValue: string);
    procedure SetusageLocation(AIndex: Integer; const AValue: string);
    procedure SetuserPrincipalName(AIndex: Integer; const AValue: string);
    procedure SetuserType(AIndex: Integer; const AValue: string);
    procedure SetaboutMe(AIndex: Integer; const AValue: string);
    procedure Setbirthday(AIndex: Integer; const AValue: TDateTime);
    procedure SethireDate(AIndex: Integer; const AValue: TDateTime);
    procedure Setinterests(AIndex: Integer; const AValue: TStringArray);
    procedure SetmySite(AIndex: Integer; const AValue: string);
    procedure SetpastProjects(AIndex: Integer; const AValue: TStringArray);
    procedure SetpreferredName(AIndex: Integer; const AValue: string);
    procedure Setresponsibilities(AIndex: Integer; 
                             const AValue: TStringArray);
    procedure Setschools(AIndex: Integer; const AValue: TStringArray);
    procedure Setskills(AIndex: Integer; const AValue: TStringArray);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    function reminderView(AService: TODataService; StartDateTime: string; 
                     EndDateTime: string) : TreminderArray;
    function assignLicense(AService: TODataService; 
                      addLicenses: TassignedLicenseArray; 
                      removeLicenses: TGuidStringArray) : Tuser;
    procedure changePassword(AService: TODataService; 
                        currentPassword: string; newPassword: string);
    procedure sendMail(AService: TODataService; Message: Tmessage; 
                  SaveToSentItems: boolean);
    class function ObjectRestKind : String;  Override;
    function ownedDevices(AService: TODataService)
                      : TdirectoryObjectsEntitySet;
    function registeredDevices(AService: TODataService)
                           : TdirectoryObjectsEntitySet;
    function manager(AService: TODataService) : TdirectoryObject;
    function directReports(AService: TODataService)
                       : TdirectoryObjectsEntitySet;
    function memberOf(AService: TODataService) : TdirectoryObjectsEntitySet;
    function createdObjects(AService: TODataService)
                        : TdirectoryObjectsEntitySet;
    function ownedObjects(AService: TODataService)
                      : TdirectoryObjectsEntitySet;
    function messages(AService: TODataService) : TmessageImplicitEntitySet;
    function mailFolders(AService: TODataService)
                     : TmailFolderImplicitEntitySet;
    function calendar(AService: TODataService) : Tcalendar;
    function calendars(AService: TODataService) : TcalendarImplicitEntitySet;
    function calendarGroups(AService: TODataService)
                        : TcalendarGroupImplicitEntitySet;
    function calendarView(AService: TODataService) : TeventImplicitEntitySet;
    function events(AService: TODataService) : TeventImplicitEntitySet;
    function contacts(AService: TODataService) : TcontactImplicitEntitySet;
    function contactFolders(AService: TODataService)
                        : TcontactFolderImplicitEntitySet;
    function photo(AService: TODataService) : TprofilePhoto;
    function drive(AService: TODataService) : Tdrive;
  published
    Property accountEnabled : boolean index 8 read FaccountEnabled write SetaccountEnabled;
    Property assignedLicenses : TassignedLicenseArray index 16 read FassignedLicenses write SetassignedLicenses;
    Property assignedPlans : TassignedPlanArray index 24 read FassignedPlans write SetassignedPlans;
    Property businessPhones : TStringArray index 32 read FbusinessPhones write SetbusinessPhones;
    Property city : string index 40 read Fcity write Setcity;
    Property companyName : string index 48 read FcompanyName write SetcompanyName;
    Property country : string index 56 read Fcountry write Setcountry;
    Property department : string index 64 read Fdepartment write Setdepartment;
    Property displayName : string index 72 read FdisplayName write SetdisplayName;
    Property givenName : string index 80 read FgivenName write SetgivenName;
    Property jobTitle : string index 88 read FjobTitle write SetjobTitle;
    Property mail : string index 96 read Fmail write Setmail;
    Property mailNickname : string index 104 read FmailNickname write SetmailNickname;
    Property mobilePhone : string index 112 read FmobilePhone write SetmobilePhone;
    Property onPremisesImmutableId : string index 120 read FonPremisesImmutableId write SetonPremisesImmutableId;
    Property onPremisesLastSyncDateTime : TDateTime index 128 read FonPremisesLastSyncDateTime write SetonPremisesLastSyncDateTime;
    Property onPremisesSecurityIdentifier : string index 136 read FonPremisesSecurityIdentifier write SetonPremisesSecurityIdentifier;
    Property onPremisesSyncEnabled : boolean index 144 read FonPremisesSyncEnabled write SetonPremisesSyncEnabled;
    Property passwordPolicies : string index 152 read FpasswordPolicies write SetpasswordPolicies;
    Property passwordProfile : TpasswordProfile index 160 read FpasswordProfile write SetpasswordProfile;
    Property officeLocation : string index 168 read FofficeLocation write SetofficeLocation;
    Property postalCode : string index 176 read FpostalCode write SetpostalCode;
    Property preferredLanguage : string index 184 read FpreferredLanguage write SetpreferredLanguage;
    Property provisionedPlans : TprovisionedPlanArray index 192 read FprovisionedPlans write SetprovisionedPlans;
    Property proxyAddresses : TStringArray index 200 read FproxyAddresses write SetproxyAddresses;
    Property state : string index 208 read Fstate write Setstate;
    Property streetAddress : string index 216 read FstreetAddress write SetstreetAddress;
    Property surname : string index 224 read Fsurname write Setsurname;
    Property usageLocation : string index 232 read FusageLocation write SetusageLocation;
    Property userPrincipalName : string index 240 read FuserPrincipalName write SetuserPrincipalName;
    Property userType : string index 248 read FuserType write SetuserType;
    Property aboutMe : string index 256 read FaboutMe write SetaboutMe;
    Property birthday : TDateTime index 264 read Fbirthday write Setbirthday;
    Property hireDate : TDateTime index 272 read FhireDate write SethireDate;
    Property interests : TStringArray index 280 read Finterests write Setinterests;
    Property mySite : string index 288 read FmySite write SetmySite;
    Property pastProjects : TStringArray index 296 read FpastProjects write SetpastProjects;
    Property preferredName : string index 304 read FpreferredName write SetpreferredName;
    Property responsibilities : TStringArray index 312 read Fresponsibilities write Setresponsibilities;
    Property schools : TStringArray index 320 read Fschools write Setschools;
    Property skills : TStringArray index 328 read Fskills write Setskills;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: message
    --------------------------------------------------------------------}
  
  Tmessage = Class(ToutlookItem)
  private
    FreceivedDateTime : TDateTime;
    FsentDateTime : TDateTime;
    FhasAttachments : boolean;
    FinternetMessageId : string;
    Fsubject : string;
    Fbody : TitemBody;
    FbodyPreview : string;
    Fimportance : Timportance;
    FparentFolderId : string;
    Fsender : Trecipient;
    Ffrom : Trecipient;
    FtoRecipients : TrecipientArray;
    FccRecipients : TrecipientArray;
    FbccRecipients : TrecipientArray;
    FreplyTo : TrecipientArray;
    FconversationId : string;
    FuniqueBody : TitemBody;
    FisDeliveryReceiptRequested : boolean;
    FisReadReceiptRequested : boolean;
    FisRead : boolean;
    FisDraft : boolean;
    FwebLink : string;
    procedure SetreceivedDateTime(AIndex: Integer; const AValue: TDateTime);
    procedure SetsentDateTime(AIndex: Integer; const AValue: TDateTime);
    procedure SethasAttachments(AIndex: Integer; const AValue: boolean);
    procedure SetinternetMessageId(AIndex: Integer; const AValue: string);
    procedure Setsubject(AIndex: Integer; const AValue: string);
    procedure Setbody(AIndex: Integer; const AValue: TitemBody);
    procedure SetbodyPreview(AIndex: Integer; const AValue: string);
    procedure Setimportance(AIndex: Integer; const AValue: Timportance);
    procedure SetparentFolderId(AIndex: Integer; const AValue: string);
    procedure Setsender(AIndex: Integer; const AValue: Trecipient);
    procedure Setfrom(AIndex: Integer; const AValue: Trecipient);
    procedure SettoRecipients(AIndex: Integer; 
                         const AValue: TrecipientArray);
    procedure SetccRecipients(AIndex: Integer; 
                         const AValue: TrecipientArray);
    procedure SetbccRecipients(AIndex: Integer; 
                          const AValue: TrecipientArray);
    procedure SetreplyTo(AIndex: Integer; const AValue: TrecipientArray);
    procedure SetconversationId(AIndex: Integer; const AValue: string);
    procedure SetuniqueBody(AIndex: Integer; const AValue: TitemBody);
    procedure SetisDeliveryReceiptRequested(AIndex: Integer; 
                                       const AValue: boolean);
    procedure SetisReadReceiptRequested(AIndex: Integer; 
                                   const AValue: boolean);
    procedure SetisRead(AIndex: Integer; const AValue: boolean);
    procedure SetisDraft(AIndex: Integer; const AValue: boolean);
    procedure SetwebLink(AIndex: Integer; const AValue: string);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    function copy(AService: TODataService; DestinationId: string) : Tmessage;
    function move(AService: TODataService; DestinationId: string) : Tmessage;
    function createReply(AService: TODataService) : Tmessage;
    function createReplyAll(AService: TODataService) : Tmessage;
    function createForward(AService: TODataService) : Tmessage;
    procedure reply(AService: TODataService; Comment: string);
    procedure replyAll(AService: TODataService; Comment: string);
    procedure forward(AService: TODataService; Comment: string; 
                 ToRecipients: TrecipientArray);
    procedure send(AService: TODataService);
    class function ObjectRestKind : String;  Override;
    function attachments(AService: TODataService)
                     : TattachmentImplicitEntitySet;
  published
    Property receivedDateTime : TDateTime index 40 read FreceivedDateTime write SetreceivedDateTime;
    Property sentDateTime : TDateTime index 48 read FsentDateTime write SetsentDateTime;
    Property hasAttachments : boolean index 56 read FhasAttachments write SethasAttachments;
    Property internetMessageId : string index 64 read FinternetMessageId write SetinternetMessageId;
    Property subject : string index 72 read Fsubject write Setsubject;
    Property body : TitemBody index 80 read Fbody write Setbody;
    Property bodyPreview : string index 88 read FbodyPreview write SetbodyPreview;
    Property importance : Timportance index 96 read Fimportance write Setimportance;
    Property parentFolderId : string index 104 read FparentFolderId write SetparentFolderId;
    Property sender : Trecipient index 112 read Fsender write Setsender;
    Property from : Trecipient index 120 read Ffrom write Setfrom;
    Property toRecipients : TrecipientArray index 128 read FtoRecipients write SettoRecipients;
    Property ccRecipients : TrecipientArray index 136 read FccRecipients write SetccRecipients;
    Property bccRecipients : TrecipientArray index 144 read FbccRecipients write SetbccRecipients;
    Property replyTo : TrecipientArray index 152 read FreplyTo write SetreplyTo;
    Property conversationId : string index 160 read FconversationId write SetconversationId;
    Property uniqueBody : TitemBody index 168 read FuniqueBody write SetuniqueBody;
    Property isDeliveryReceiptRequested : boolean index 176 read FisDeliveryReceiptRequested write SetisDeliveryReceiptRequested;
    Property isReadReceiptRequested : boolean index 184 read FisReadReceiptRequested write SetisReadReceiptRequested;
    Property isRead : boolean index 192 read FisRead write SetisRead;
    Property isDraft : boolean index 200 read FisDraft write SetisDraft;
    Property webLink : string index 208 read FwebLink write SetwebLink;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: mailFolder
    --------------------------------------------------------------------}
  
  TmailFolder = Class(Tentity)
  private
    FdisplayName : string;
    FparentFolderId : string;
    FchildFolderCount : TInt32;
    FunreadItemCount : TInt32;
    FtotalItemCount : TInt32;
    procedure SetdisplayName(AIndex: Integer; const AValue: string);
    procedure SetparentFolderId(AIndex: Integer; const AValue: string);
    procedure SetchildFolderCount(AIndex: Integer; const AValue: TInt32);
    procedure SetunreadItemCount(AIndex: Integer; const AValue: TInt32);
    procedure SettotalItemCount(AIndex: Integer; const AValue: TInt32);
  public
    function copy(AService: TODataService; DestinationId: string)
              : TmailFolder;
    function move(AService: TODataService; DestinationId: string)
              : TmailFolder;
    class function ObjectRestKind : String;  Override;
    function messages(AService: TODataService) : TmessageImplicitEntitySet;
    function childFolders(AService: TODataService)
                      : TmailFolderImplicitEntitySet;
  published
    Property displayName : string index 8 read FdisplayName write SetdisplayName;
    Property parentFolderId : string index 16 read FparentFolderId write SetparentFolderId;
    Property childFolderCount : TInt32 index 24 read FchildFolderCount write SetchildFolderCount;
    Property unreadItemCount : TInt32 index 32 read FunreadItemCount write SetunreadItemCount;
    Property totalItemCount : TInt32 index 40 read FtotalItemCount write SettotalItemCount;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: calendarGroup
    --------------------------------------------------------------------}
  
  TcalendarGroup = Class(Tentity)
  private
    Fname : string;
    FclassId : TGUIDString;
    FchangeKey : string;
    procedure Setname(AIndex: Integer; const AValue: string);
    procedure SetclassId(AIndex: Integer; const AValue: TGUIDString);
    procedure SetchangeKey(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
    function calendars(AService: TODataService) : TcalendarImplicitEntitySet;
  published
    Property name : string index 8 read Fname write Setname;
    Property classId : TGUIDString index 16 read FclassId write SetclassId;
    Property changeKey : string index 24 read FchangeKey write SetchangeKey;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: contact
    --------------------------------------------------------------------}
  
  Tcontact = Class(ToutlookItem)
  private
    FparentFolderId : string;
    Fbirthday : TDateTime;
    FfileAs : string;
    FdisplayName : string;
    FgivenName : string;
    Finitials : string;
    FmiddleName : string;
    FnickName : string;
    Fsurname : string;
    Ftitle : string;
    FyomiGivenName : string;
    FyomiSurname : string;
    FyomiCompanyName : string;
    Fgeneration : string;
    FemailAddresses : TemailAddressArray;
    FimAddresses : TStringArray;
    FjobTitle : string;
    FcompanyName : string;
    Fdepartment : string;
    FofficeLocation : string;
    Fprofession : string;
    FbusinessHomePage : string;
    FassistantName : string;
    Fmanager : string;
    FhomePhones : TStringArray;
    FmobilePhone : string;
    FbusinessPhones : TStringArray;
    FhomeAddress : TphysicalAddress;
    FbusinessAddress : TphysicalAddress;
    FotherAddress : TphysicalAddress;
    FspouseName : string;
    FpersonalNotes : string;
    Fchildren : TStringArray;
    procedure SetparentFolderId(AIndex: Integer; const AValue: string);
    procedure Setbirthday(AIndex: Integer; const AValue: TDateTime);
    procedure SetfileAs(AIndex: Integer; const AValue: string);
    procedure SetdisplayName(AIndex: Integer; const AValue: string);
    procedure SetgivenName(AIndex: Integer; const AValue: string);
    procedure Setinitials(AIndex: Integer; const AValue: string);
    procedure SetmiddleName(AIndex: Integer; const AValue: string);
    procedure SetnickName(AIndex: Integer; const AValue: string);
    procedure Setsurname(AIndex: Integer; const AValue: string);
    procedure Settitle(AIndex: Integer; const AValue: string);
    procedure SetyomiGivenName(AIndex: Integer; const AValue: string);
    procedure SetyomiSurname(AIndex: Integer; const AValue: string);
    procedure SetyomiCompanyName(AIndex: Integer; const AValue: string);
    procedure Setgeneration(AIndex: Integer; const AValue: string);
    procedure SetemailAddresses(AIndex: Integer; 
                           const AValue: TemailAddressArray);
    procedure SetimAddresses(AIndex: Integer; const AValue: TStringArray);
    procedure SetjobTitle(AIndex: Integer; const AValue: string);
    procedure SetcompanyName(AIndex: Integer; const AValue: string);
    procedure Setdepartment(AIndex: Integer; const AValue: string);
    procedure SetofficeLocation(AIndex: Integer; const AValue: string);
    procedure Setprofession(AIndex: Integer; const AValue: string);
    procedure SetbusinessHomePage(AIndex: Integer; const AValue: string);
    procedure SetassistantName(AIndex: Integer; const AValue: string);
    procedure Setmanager(AIndex: Integer; const AValue: string);
    procedure SethomePhones(AIndex: Integer; const AValue: TStringArray);
    procedure SetmobilePhone(AIndex: Integer; const AValue: string);
    procedure SetbusinessPhones(AIndex: Integer; const AValue: TStringArray);
    procedure SethomeAddress(AIndex: Integer; 
                        const AValue: TphysicalAddress);
    procedure SetbusinessAddress(AIndex: Integer; 
                            const AValue: TphysicalAddress);
    procedure SetotherAddress(AIndex: Integer; 
                         const AValue: TphysicalAddress);
    procedure SetspouseName(AIndex: Integer; const AValue: string);
    procedure SetpersonalNotes(AIndex: Integer; const AValue: string);
    procedure Setchildren(AIndex: Integer; const AValue: TStringArray);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    class function ObjectRestKind : String;  Override;
    function photo(AService: TODataService) : TprofilePhoto;
  published
    Property parentFolderId : string index 40 read FparentFolderId write SetparentFolderId;
    Property birthday : TDateTime index 48 read Fbirthday write Setbirthday;
    Property fileAs : string index 56 read FfileAs write SetfileAs;
    Property displayName : string index 64 read FdisplayName write SetdisplayName;
    Property givenName : string index 72 read FgivenName write SetgivenName;
    Property initials : string index 80 read Finitials write Setinitials;
    Property middleName : string index 88 read FmiddleName write SetmiddleName;
    Property nickName : string index 96 read FnickName write SetnickName;
    Property surname : string index 104 read Fsurname write Setsurname;
    Property title : string index 112 read Ftitle write Settitle;
    Property yomiGivenName : string index 120 read FyomiGivenName write SetyomiGivenName;
    Property yomiSurname : string index 128 read FyomiSurname write SetyomiSurname;
    Property yomiCompanyName : string index 136 read FyomiCompanyName write SetyomiCompanyName;
    Property generation : string index 144 read Fgeneration write Setgeneration;
    Property emailAddresses : TemailAddressArray index 152 read FemailAddresses write SetemailAddresses;
    Property imAddresses : TStringArray index 160 read FimAddresses write SetimAddresses;
    Property jobTitle : string index 168 read FjobTitle write SetjobTitle;
    Property companyName : string index 176 read FcompanyName write SetcompanyName;
    Property department : string index 184 read Fdepartment write Setdepartment;
    Property officeLocation : string index 192 read FofficeLocation write SetofficeLocation;
    Property profession : string index 200 read Fprofession write Setprofession;
    Property businessHomePage : string index 208 read FbusinessHomePage write SetbusinessHomePage;
    Property assistantName : string index 216 read FassistantName write SetassistantName;
    Property manager : string index 224 read Fmanager write Setmanager;
    Property homePhones : TStringArray index 232 read FhomePhones write SethomePhones;
    Property mobilePhone : string index 240 read FmobilePhone write SetmobilePhone;
    Property businessPhones : TStringArray index 248 read FbusinessPhones write SetbusinessPhones;
    Property homeAddress : TphysicalAddress index 256 read FhomeAddress write SethomeAddress;
    Property businessAddress : TphysicalAddress index 264 read FbusinessAddress write SetbusinessAddress;
    Property otherAddress : TphysicalAddress index 272 read FotherAddress write SetotherAddress;
    Property spouseName : string index 280 read FspouseName write SetspouseName;
    Property personalNotes : string index 288 read FpersonalNotes write SetpersonalNotes;
    Property children : TStringArray index 296 read Fchildren write Setchildren;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: contactFolder
    --------------------------------------------------------------------}
  
  TcontactFolder = Class(Tentity)
  private
    FparentFolderId : string;
    FdisplayName : string;
    procedure SetparentFolderId(AIndex: Integer; const AValue: string);
    procedure SetdisplayName(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
    function contacts(AService: TODataService) : TcontactImplicitEntitySet;
    function childFolders(AService: TODataService)
                      : TcontactFolderImplicitEntitySet;
  published
    Property parentFolderId : string index 8 read FparentFolderId write SetparentFolderId;
    Property displayName : string index 16 read FdisplayName write SetdisplayName;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: attachment
    --------------------------------------------------------------------}
  
  Tattachment = Class(Tentity)
  private
    FlastModifiedDateTime : TDateTime;
    Fname : string;
    FcontentType : string;
    Fsize : TInt32;
    FisInline : boolean;
    procedure SetlastModifiedDateTime(AIndex: Integer; 
                                 const AValue: TDateTime);
    procedure Setname(AIndex: Integer; const AValue: string);
    procedure SetcontentType(AIndex: Integer; const AValue: string);
    procedure Setsize(AIndex: Integer; const AValue: TInt32);
    procedure SetisInline(AIndex: Integer; const AValue: boolean);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property lastModifiedDateTime : TDateTime index 8 read FlastModifiedDateTime write SetlastModifiedDateTime;
    Property name : string index 16 read Fname write Setname;
    Property contentType : string index 24 read FcontentType write SetcontentType;
    Property size : TInt32 index 32 read Fsize write Setsize;
    Property isInline : boolean index 40 read FisInline write SetisInline;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: fileAttachment
    --------------------------------------------------------------------}
  
  TfileAttachment = Class(Tattachment)
  private
    FcontentId : string;
    FcontentLocation : string;
    FcontentBytes : TBinary;
    procedure SetcontentId(AIndex: Integer; const AValue: string);
    procedure SetcontentLocation(AIndex: Integer; const AValue: string);
    procedure SetcontentBytes(AIndex: Integer; const AValue: TBinary);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property contentId : string index 48 read FcontentId write SetcontentId;
    Property contentLocation : string index 56 read FcontentLocation write SetcontentLocation;
    Property contentBytes : TBinary index 64 read FcontentBytes write SetcontentBytes;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: itemAttachment
    --------------------------------------------------------------------}
  
  TitemAttachment = Class(Tattachment)
  public
    class function ObjectRestKind : String;  Override;
    function item(AService: TODataService) : ToutlookItem;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: eventMessage
    --------------------------------------------------------------------}
  
  TeventMessage = Class(Tmessage)
  private
    FmeetingMessageType : TmeetingMessageType;
    procedure SetmeetingMessageType(AIndex: Integer; 
                               const AValue: TmeetingMessageType);
  public
    class function ObjectRestKind : String;  Override;
    function event(AService: TODataService) : Tevent;
  published
    Property meetingMessageType : TmeetingMessageType index 216 read FmeetingMessageType write SetmeetingMessageType;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: referenceAttachment
    --------------------------------------------------------------------}
  
  TreferenceAttachment = Class(Tattachment)
  public
    class function ObjectRestKind : String;  Override;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: post
    --------------------------------------------------------------------}
  
  Tpost = Class(ToutlookItem)
  private
    Fbody : TitemBody;
    FreceivedDateTime : TDateTime;
    FhasAttachments : boolean;
    Ffrom : Trecipient;
    Fsender : Trecipient;
    FconversationThreadId : string;
    FnewParticipants : TrecipientArray;
    FconversationId : string;
    procedure Setbody(AIndex: Integer; const AValue: TitemBody);
    procedure SetreceivedDateTime(AIndex: Integer; const AValue: TDateTime);
    procedure SethasAttachments(AIndex: Integer; const AValue: boolean);
    procedure Setfrom(AIndex: Integer; const AValue: Trecipient);
    procedure Setsender(AIndex: Integer; const AValue: Trecipient);
    procedure SetconversationThreadId(AIndex: Integer; const AValue: string);
    procedure SetnewParticipants(AIndex: Integer; 
                            const AValue: TrecipientArray);
    procedure SetconversationId(AIndex: Integer; const AValue: string);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    procedure forward(AService: TODataService; Comment: string; 
                 ToRecipients: TrecipientArray);
    procedure reply(AService: TODataService; _Post: Tpost);
    class function ObjectRestKind : String;  Override;
    function inReplyTo(AService: TODataService) : Tpost;
    function attachments(AService: TODataService)
                     : TattachmentImplicitEntitySet;
  published
    Property body : TitemBody index 40 read Fbody write Setbody;
    Property receivedDateTime : TDateTime index 48 read FreceivedDateTime write SetreceivedDateTime;
    Property hasAttachments : boolean index 56 read FhasAttachments write SethasAttachments;
    Property from : Trecipient index 64 read Ffrom write Setfrom;
    Property sender : Trecipient index 72 read Fsender write Setsender;
    Property conversationThreadId : string index 80 read FconversationThreadId write SetconversationThreadId;
    Property newParticipants : TrecipientArray index 88 read FnewParticipants write SetnewParticipants;
    Property conversationId : string index 96 read FconversationId write SetconversationId;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: driveItem
    --------------------------------------------------------------------}
  
  TdriveItem = Class(Tentity)
  private
    Fcontent : TStream;
    FcreatedBy : TidentitySet;
    FcreatedDateTime : TDateTime;
    FcTag : string;
    Fdescription : string;
    FeTag : string;
    FlastModifiedBy : TidentitySet;
    FlastModifiedDateTime : TDateTime;
    Fname : string;
    FparentReference : TitemReference;
    Fsize : int64;
    FwebDavUrl : string;
    FwebUrl : string;
    Faudio : Taudio;
    Fdeleted : Tdeleted;
    F_file : T_file;
    FfileSystemInfo : TfileSystemInfo;
    Ffolder : Tfolder;
    Fimage : Timage;
    Flocation : TgeoCoordinates;
    Fphoto : Tphoto;
    FremoteItem : TremoteItem;
    FsearchResult : TsearchResult;
    Fshared : Tshared;
    FspecialFolder : TspecialFolder;
    Fvideo : Tvideo;
    Fpackage : Tpackage;
    procedure Setcontent(AIndex: Integer; const AValue: TStream);
    procedure SetcreatedBy(AIndex: Integer; const AValue: TidentitySet);
    procedure SetcreatedDateTime(AIndex: Integer; const AValue: TDateTime);
    procedure SetcTag(AIndex: Integer; const AValue: string);
    procedure Setdescription(AIndex: Integer; const AValue: string);
    procedure SeteTag(AIndex: Integer; const AValue: string);
    procedure SetlastModifiedBy(AIndex: Integer; const AValue: TidentitySet);
    procedure SetlastModifiedDateTime(AIndex: Integer; 
                                 const AValue: TDateTime);
    procedure Setname(AIndex: Integer; const AValue: string);
    procedure SetparentReference(AIndex: Integer; 
                            const AValue: TitemReference);
    procedure Setsize(AIndex: Integer; const AValue: int64);
    procedure SetwebDavUrl(AIndex: Integer; const AValue: string);
    procedure SetwebUrl(AIndex: Integer; const AValue: string);
    procedure Setaudio(AIndex: Integer; const AValue: Taudio);
    procedure Setdeleted(AIndex: Integer; const AValue: Tdeleted);
    procedure Set_file(AIndex: Integer; const AValue: T_file);
    procedure SetfileSystemInfo(AIndex: Integer; 
                           const AValue: TfileSystemInfo);
    procedure Setfolder(AIndex: Integer; const AValue: Tfolder);
    procedure Setimage(AIndex: Integer; const AValue: Timage);
    procedure Setlocation(AIndex: Integer; const AValue: TgeoCoordinates);
    procedure Setphoto(AIndex: Integer; const AValue: Tphoto);
    procedure SetremoteItem(AIndex: Integer; const AValue: TremoteItem);
    procedure SetsearchResult(AIndex: Integer; const AValue: TsearchResult);
    procedure Setshared(AIndex: Integer; const AValue: Tshared);
    procedure SetspecialFolder(AIndex: Integer; 
                          const AValue: TspecialFolder);
    procedure Setvideo(AIndex: Integer; const AValue: Tvideo);
    procedure Setpackage(AIndex: Integer; const AValue: Tpackage);
  public
    function search(AService: TODataService; q: string) : TdriveItemArray;
    function delta(AService: TODataService; token: string) : TdriveItemArray;
    function delta(AService: TODataService) : TdriveItemArray;
    function createLink(AService: TODataService; _type: string; 
                   scope: string) : Tpermission;
    function copy(AService: TODataService; name: string; 
             parentReference: TitemReference) : TdriveItem;
    class function ObjectRestKind : String;  Override;
    class function ExportPropertyName(const AName: String) : String
                                 ;  Override;
    function createdByUser(AService: TODataService) : Tuser;
    function lastModifiedByUser(AService: TODataService) : Tuser;
    function permissions(AService: TODataService)
                     : TpermissionImplicitEntitySet;
    function children(AService: TODataService) : TdriveItemImplicitEntitySet;
    function thumbnails(AService: TODataService)
                    : TthumbnailSetImplicitEntitySet;
  published
    Property content : TStream index 8 read Fcontent write Setcontent;
    Property createdBy : TidentitySet index 16 read FcreatedBy write SetcreatedBy;
    Property createdDateTime : TDateTime index 24 read FcreatedDateTime write SetcreatedDateTime;
    Property cTag : string index 32 read FcTag write SetcTag;
    Property description : string index 40 read Fdescription write Setdescription;
    Property eTag : string index 48 read FeTag write SeteTag;
    Property lastModifiedBy : TidentitySet index 56 read FlastModifiedBy write SetlastModifiedBy;
    Property lastModifiedDateTime : TDateTime index 64 read FlastModifiedDateTime write SetlastModifiedDateTime;
    Property name : string index 72 read Fname write Setname;
    Property parentReference : TitemReference index 80 read FparentReference write SetparentReference;
    Property size : int64 index 88 read Fsize write Setsize;
    Property webDavUrl : string index 96 read FwebDavUrl write SetwebDavUrl;
    Property webUrl : string index 104 read FwebUrl write SetwebUrl;
    Property audio : Taudio index 112 read Faudio write Setaudio;
    Property deleted : Tdeleted index 120 read Fdeleted write Setdeleted;
    Property _file : T_file index 128 read F_file write Set_file;
    Property fileSystemInfo : TfileSystemInfo index 136 read FfileSystemInfo write SetfileSystemInfo;
    Property folder : Tfolder index 144 read Ffolder write Setfolder;
    Property image : Timage index 152 read Fimage write Setimage;
    Property location : TgeoCoordinates index 160 read Flocation write Setlocation;
    Property photo : Tphoto index 168 read Fphoto write Setphoto;
    Property remoteItem : TremoteItem index 176 read FremoteItem write SetremoteItem;
    Property searchResult : TsearchResult index 184 read FsearchResult write SetsearchResult;
    Property shared : Tshared index 192 read Fshared write Setshared;
    Property specialFolder : TspecialFolder index 200 read FspecialFolder write SetspecialFolder;
    Property video : Tvideo index 208 read Fvideo write Setvideo;
    Property package : Tpackage index 216 read Fpackage write Setpackage;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: permission
    --------------------------------------------------------------------}
  
  Tpermission = Class(Tentity)
  private
    FgrantedTo : TidentitySet;
    Finvitation : TsharingInvitation;
    FinheritedFrom : TitemReference;
    Flink : TsharingLink;
    Froles : TStringArray;
    FshareId : string;
    procedure SetgrantedTo(AIndex: Integer; const AValue: TidentitySet);
    procedure Setinvitation(AIndex: Integer; 
                       const AValue: TsharingInvitation);
    procedure SetinheritedFrom(AIndex: Integer; 
                          const AValue: TitemReference);
    procedure Setlink(AIndex: Integer; const AValue: TsharingLink);
    procedure Setroles(AIndex: Integer; const AValue: TStringArray);
    procedure SetshareId(AIndex: Integer; const AValue: string);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    class function ObjectRestKind : String;  Override;
  published
    Property grantedTo : TidentitySet index 8 read FgrantedTo write SetgrantedTo;
    Property invitation : TsharingInvitation index 16 read Finvitation write Setinvitation;
    Property inheritedFrom : TitemReference index 24 read FinheritedFrom write SetinheritedFrom;
    Property link : TsharingLink index 32 read Flink write Setlink;
    Property roles : TStringArray index 40 read Froles write Setroles;
    Property shareId : string index 48 read FshareId write SetshareId;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: thumbnailSet
    --------------------------------------------------------------------}
  
  TthumbnailSet = Class(Tentity)
  private
    Flarge : Tthumbnail;
    Fmedium : Tthumbnail;
    Fsmall : Tthumbnail;
    Fsource : Tthumbnail;
    procedure Setlarge(AIndex: Integer; const AValue: Tthumbnail);
    procedure Setmedium(AIndex: Integer; const AValue: Tthumbnail);
    procedure Setsmall(AIndex: Integer; const AValue: Tthumbnail);
    procedure Setsource(AIndex: Integer; const AValue: Tthumbnail);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property large : Tthumbnail index 8 read Flarge write Setlarge;
    Property medium : Tthumbnail index 16 read Fmedium write Setmedium;
    Property small : Tthumbnail index 24 read Fsmall write Setsmall;
    Property source : Tthumbnail index 32 read Fsource write Setsource;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: subscription
    --------------------------------------------------------------------}
  
  Tsubscription = Class(Tentity)
  private
    Fresource : string;
    FchangeType : string;
    FclientState : string;
    FnotificationUrl : string;
    FexpirationDateTime : TDateTime;
    procedure Setresource(AIndex: Integer; const AValue: string);
    procedure SetchangeType(AIndex: Integer; const AValue: string);
    procedure SetclientState(AIndex: Integer; const AValue: string);
    procedure SetnotificationUrl(AIndex: Integer; const AValue: string);
    procedure SetexpirationDateTime(AIndex: Integer; 
                               const AValue: TDateTime);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property resource : string index 8 read Fresource write Setresource;
    Property changeType : string index 16 read FchangeType write SetchangeType;
    Property clientState : string index 24 read FclientState write SetclientState;
    Property notificationUrl : string index 32 read FnotificationUrl write SetnotificationUrl;
    Property expirationDateTime : TDateTime index 40 read FexpirationDateTime write SetexpirationDateTime;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: GraphService
    --------------------------------------------------------------------}
  
  TGraphService = Class(TODataEntityContainer)
  private
    FdirectoryObjects : TdirectoryObjectsEntitySet;
    Fdevices : TdevicesEntitySet;
    Fgroups : TgroupsEntitySet;
    FdirectoryRoles : TdirectoryRolesEntitySet;
    FdirectoryRoleTemplates : TdirectoryRoleTemplatesEntitySet;
    Forganization : TorganizationEntitySet;
    FsubscribedSkus : TsubscribedSkusEntitySet;
    Fusers : TusersEntitySet;
    Fdrives : TdrivesEntitySet;
    Fsubscriptions : TsubscriptionsEntitySet;
    Fme : Tuser;
    Fdrive : Tdrive;
    function GetdirectoryObjects : TdirectoryObjectsEntitySet;
    function Getdevices : TdevicesEntitySet;
    function Getgroups : TgroupsEntitySet;
    function GetdirectoryRoles : TdirectoryRolesEntitySet;
    function GetdirectoryRoleTemplates : TdirectoryRoleTemplatesEntitySet;
    function Getorganization : TorganizationEntitySet;
    function GetsubscribedSkus : TsubscribedSkusEntitySet;
    function Getusers : TusersEntitySet;
    function Getdrives : TdrivesEntitySet;
    function Getsubscriptions : TsubscriptionsEntitySet;
    function Getme : Tuser;
    function Getdrive : Tdrive;
  public
    class function ObjectRestKind : String;  Override;
    function CreateNewdirectoryObjects : TdirectoryObjectsEntitySet;
    function CreateNewdevices : TdevicesEntitySet;
    function CreateNewgroups : TgroupsEntitySet;
    function CreateNewdirectoryRoles : TdirectoryRolesEntitySet;
    function CreateNewdirectoryRoleTemplates
                                         : TdirectoryRoleTemplatesEntitySet;
    function CreateNeworganization : TorganizationEntitySet;
    function CreateNewsubscribedSkus : TsubscribedSkusEntitySet;
    function CreateNewusers : TusersEntitySet;
    function CreateNewdrives : TdrivesEntitySet;
    function CreateNewsubscriptions : TsubscriptionsEntitySet;
    function Fetchme : Tuser;
    function Fetchdrive : Tdrive;
  published
    Property directoryObjects : TdirectoryObjectsEntitySet read GetdirectoryObjects;
    Property devices : TdevicesEntitySet read Getdevices;
    Property groups : TgroupsEntitySet read Getgroups;
    Property directoryRoles : TdirectoryRolesEntitySet read GetdirectoryRoles;
    Property directoryRoleTemplates : TdirectoryRoleTemplatesEntitySet read GetdirectoryRoleTemplates;
    Property organization : TorganizationEntitySet read Getorganization;
    Property subscribedSkus : TsubscribedSkusEntitySet read GetsubscribedSkus;
    Property users : TusersEntitySet read Getusers;
    Property drives : TdrivesEntitySet read Getdrives;
    Property subscriptions : TsubscriptionsEntitySet read Getsubscriptions;
    Property me : Tuser read Getme;
    Property drive : Tdrive read Getdrive;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: directoryObjects
    --------------------------------------------------------------------}
  
  TdirectoryObjectsEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : TdirectoryObject;
    function List(const AQuery: String; out NextLink: String)
              : TdirectoryObjectArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TdirectoryObjectArray;
    function ListAll(const AQuery: String) : TdirectoryObjectArray;
    function ListAll(const AQuery: TQueryParams) : TdirectoryObjectArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: devices
    --------------------------------------------------------------------}
  
  TdevicesEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : Tdevice;
    function List(const AQuery: String; out NextLink: String) : TdeviceArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TdeviceArray;
    function ListAll(const AQuery: String) : TdeviceArray;
    function ListAll(const AQuery: TQueryParams) : TdeviceArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: groups
    --------------------------------------------------------------------}
  
  TgroupsEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : Tgroup;
    function List(const AQuery: String; out NextLink: String) : TgroupArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TgroupArray;
    function ListAll(const AQuery: String) : TgroupArray;
    function ListAll(const AQuery: TQueryParams) : TgroupArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: directoryRoles
    --------------------------------------------------------------------}
  
  TdirectoryRolesEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : TdirectoryRole;
    function List(const AQuery: String; out NextLink: String)
              : TdirectoryRoleArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TdirectoryRoleArray;
    function ListAll(const AQuery: String) : TdirectoryRoleArray;
    function ListAll(const AQuery: TQueryParams) : TdirectoryRoleArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: directoryRoleTemplates
    --------------------------------------------------------------------}
  
  TdirectoryRoleTemplatesEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : TdirectoryRoleTemplate;
    function List(const AQuery: String; out NextLink: String)
              : TdirectoryRoleTemplateArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TdirectoryRoleTemplateArray;
    function ListAll(const AQuery: String) : TdirectoryRoleTemplateArray;
    function ListAll(const AQuery: TQueryParams)
                 : TdirectoryRoleTemplateArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: organization
    --------------------------------------------------------------------}
  
  TorganizationEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : Torganization;
    function List(const AQuery: String; out NextLink: String)
              : TorganizationArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TorganizationArray;
    function ListAll(const AQuery: String) : TorganizationArray;
    function ListAll(const AQuery: TQueryParams) : TorganizationArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: subscribedSkus
    --------------------------------------------------------------------}
  
  TsubscribedSkusEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : TsubscribedSku;
    function List(const AQuery: String; out NextLink: String)
              : TsubscribedSkuArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TsubscribedSkuArray;
    function ListAll(const AQuery: String) : TsubscribedSkuArray;
    function ListAll(const AQuery: TQueryParams) : TsubscribedSkuArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: users
    --------------------------------------------------------------------}
  
  TusersEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : Tuser;
    function List(const AQuery: String; out NextLink: String) : TuserArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TuserArray;
    function ListAll(const AQuery: String) : TuserArray;
    function ListAll(const AQuery: TQueryParams) : TuserArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: drives
    --------------------------------------------------------------------}
  
  TdrivesEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : Tdrive;
    function List(const AQuery: String; out NextLink: String) : TdriveArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TdriveArray;
    function ListAll(const AQuery: String) : TdriveArray;
    function ListAll(const AQuery: TQueryParams) : TdriveArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: subscriptions
    --------------------------------------------------------------------}
  
  TsubscriptionsEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : Tsubscription;
    function List(const AQuery: String; out NextLink: String)
              : TsubscriptionArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TsubscriptionArray;
    function ListAll(const AQuery: String) : TsubscriptionArray;
    function ListAll(const AQuery: TQueryParams) : TsubscriptionArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: conversationThreadImplicitEntitySet
    --------------------------------------------------------------------}
  
  TconversationThreadImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : TconversationThread;
    function List(const AQuery: String; out NextLink: String)
              : TconversationThreadArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TconversationThreadArray;
    function ListAll(const AQuery: String) : TconversationThreadArray;
    function ListAll(const AQuery: TQueryParams) : TconversationThreadArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: calendarImplicitEntitySet
    --------------------------------------------------------------------}
  
  TcalendarImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : Tcalendar;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: eventImplicitEntitySet
    --------------------------------------------------------------------}
  
  TeventImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : Tevent;
    function List(const AQuery: String; out NextLink: String) : TeventArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TeventArray;
    function ListAll(const AQuery: String) : TeventArray;
    function ListAll(const AQuery: TQueryParams) : TeventArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: conversationImplicitEntitySet
    --------------------------------------------------------------------}
  
  TconversationImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : Tconversation;
    function List(const AQuery: String; out NextLink: String)
              : TconversationArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TconversationArray;
    function ListAll(const AQuery: String) : TconversationArray;
    function ListAll(const AQuery: TQueryParams) : TconversationArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: profilePhotoImplicitEntitySet
    --------------------------------------------------------------------}
  
  TprofilePhotoImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : TprofilePhoto;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: postImplicitEntitySet
    --------------------------------------------------------------------}
  
  TpostImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : Tpost;
    function List(const AQuery: String; out NextLink: String) : TpostArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TpostArray;
    function ListAll(const AQuery: String) : TpostArray;
    function ListAll(const AQuery: TQueryParams) : TpostArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: attachmentImplicitEntitySet
    --------------------------------------------------------------------}
  
  TattachmentImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : Tattachment;
    function List(const AQuery: String; out NextLink: String)
              : TattachmentArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TattachmentArray;
    function ListAll(const AQuery: String) : TattachmentArray;
    function ListAll(const AQuery: TQueryParams) : TattachmentArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: driveItemImplicitEntitySet
    --------------------------------------------------------------------}
  
  TdriveItemImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : TdriveItem;
    function List(const AQuery: String; out NextLink: String)
              : TdriveItemArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TdriveItemArray;
    function ListAll(const AQuery: String) : TdriveItemArray;
    function ListAll(const AQuery: TQueryParams) : TdriveItemArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: messageImplicitEntitySet
    --------------------------------------------------------------------}
  
  TmessageImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : Tmessage;
    function List(const AQuery: String; out NextLink: String)
              : TmessageArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TmessageArray;
    function ListAll(const AQuery: String) : TmessageArray;
    function ListAll(const AQuery: TQueryParams) : TmessageArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: mailFolderImplicitEntitySet
    --------------------------------------------------------------------}
  
  TmailFolderImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : TmailFolder;
    function List(const AQuery: String; out NextLink: String)
              : TmailFolderArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TmailFolderArray;
    function ListAll(const AQuery: String) : TmailFolderArray;
    function ListAll(const AQuery: TQueryParams) : TmailFolderArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: calendarGroupImplicitEntitySet
    --------------------------------------------------------------------}
  
  TcalendarGroupImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : TcalendarGroup;
    function List(const AQuery: String; out NextLink: String)
              : TcalendarGroupArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TcalendarGroupArray;
    function ListAll(const AQuery: String) : TcalendarGroupArray;
    function ListAll(const AQuery: TQueryParams) : TcalendarGroupArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: contactImplicitEntitySet
    --------------------------------------------------------------------}
  
  TcontactImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : Tcontact;
    function List(const AQuery: String; out NextLink: String)
              : TcontactArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TcontactArray;
    function ListAll(const AQuery: String) : TcontactArray;
    function ListAll(const AQuery: TQueryParams) : TcontactArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: contactFolderImplicitEntitySet
    --------------------------------------------------------------------}
  
  TcontactFolderImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : TcontactFolder;
    function List(const AQuery: String; out NextLink: String)
              : TcontactFolderArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TcontactFolderArray;
    function ListAll(const AQuery: String) : TcontactFolderArray;
    function ListAll(const AQuery: TQueryParams) : TcontactFolderArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: outlookItemImplicitEntitySet
    --------------------------------------------------------------------}
  
  ToutlookItemImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : ToutlookItem;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: permissionImplicitEntitySet
    --------------------------------------------------------------------}
  
  TpermissionImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : Tpermission;
    function List(const AQuery: String; out NextLink: String)
              : TpermissionArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TpermissionArray;
    function ListAll(const AQuery: String) : TpermissionArray;
    function ListAll(const AQuery: TQueryParams) : TpermissionArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: thumbnailSetImplicitEntitySet
    --------------------------------------------------------------------}
  
  TthumbnailSetImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const id: string) : TthumbnailSet;
    function List(const AQuery: String; out NextLink: String)
              : TthumbnailSetArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TthumbnailSetArray;
    function ListAll(const AQuery: String) : TthumbnailSetArray;
    function ListAll(const AQuery: TQueryParams) : TthumbnailSetArray;
  end;
  
  
  { --------------------------------------------------------------------
    microsoft.graph: microsoft.graph
    --------------------------------------------------------------------}
  
  TService = Class(TODataService)
  private
    FGraphService : TGraphService;
    function GetGraphService : TGraphService;
  public
    class function ObjectRestKind : String;  Override;
    function CreateNewGraphService : TGraphService;
  published
    Property GraphService : TGraphService read GetGraphService;
  end;
  

implementation


{ --------------------------------------------------------------------
  TalternativeSecurityId
  --------------------------------------------------------------------}


Class Function TalternativeSecurityId.ObjectRestKind : String; 

begin
  Result:='alternativeSecurityId';
end;


Procedure TalternativeSecurityId.Set_type(AIndex: Integer; const AValue: TInt32); 


begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TalternativeSecurityId.SetidentityProvider(AIndex: Integer; const AValue: string); 


begin
  If (FidentityProvider=AValue) then exit;
  FidentityProvider:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TalternativeSecurityId.Setkey(AIndex: Integer; const AValue: TBinary); 


begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;


Class Function TalternativeSecurityId.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;


{ --------------------------------------------------------------------
  TlicenseUnitsDetail
  --------------------------------------------------------------------}


Class Function TlicenseUnitsDetail.ObjectRestKind : String; 

begin
  Result:='licenseUnitsDetail';
end;


Procedure TlicenseUnitsDetail.Setenabled(AIndex: Integer; const AValue: TInt32); 


begin
  If (Fenabled=AValue) then exit;
  Fenabled:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TlicenseUnitsDetail.Setsuspended(AIndex: Integer; const AValue: TInt32); 


begin
  If (Fsuspended=AValue) then exit;
  Fsuspended:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TlicenseUnitsDetail.Setwarning(AIndex: Integer; const AValue: TInt32); 


begin
  If (Fwarning=AValue) then exit;
  Fwarning:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TservicePlanInfo
  --------------------------------------------------------------------}


Class Function TservicePlanInfo.ObjectRestKind : String; 

begin
  Result:='servicePlanInfo';
end;


Procedure TservicePlanInfo.SetservicePlanId(AIndex: Integer; const AValue: TGUIDString); 


begin
  If (FservicePlanId=AValue) then exit;
  FservicePlanId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TservicePlanInfo.SetservicePlanName(AIndex: Integer; const AValue: string); 


begin
  If (FservicePlanName=AValue) then exit;
  FservicePlanName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TservicePlanInfo.SetprovisioningStatus(AIndex: Integer; const AValue: string); 


begin
  If (FprovisioningStatus=AValue) then exit;
  FprovisioningStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TservicePlanInfo.SetappliesTo(AIndex: Integer; const AValue: string); 


begin
  If (FappliesTo=AValue) then exit;
  FappliesTo:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TassignedPlan
  --------------------------------------------------------------------}


Class Function TassignedPlan.ObjectRestKind : String; 

begin
  Result:='assignedPlan';
end;


Procedure TassignedPlan.SetassignedDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FassignedDateTime=AValue) then exit;
  FassignedDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TassignedPlan.SetcapabilityStatus(AIndex: Integer; const AValue: string); 


begin
  If (FcapabilityStatus=AValue) then exit;
  FcapabilityStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TassignedPlan.Setservice(AIndex: Integer; const AValue: string); 


begin
  If (Fservice=AValue) then exit;
  Fservice:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TassignedPlan.SetservicePlanId(AIndex: Integer; const AValue: TGUIDString); 


begin
  If (FservicePlanId=AValue) then exit;
  FservicePlanId:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TprovisionedPlan
  --------------------------------------------------------------------}


Class Function TprovisionedPlan.ObjectRestKind : String; 

begin
  Result:='provisionedPlan';
end;


Procedure TprovisionedPlan.SetcapabilityStatus(AIndex: Integer; const AValue: string); 


begin
  If (FcapabilityStatus=AValue) then exit;
  FcapabilityStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TprovisionedPlan.SetprovisioningStatus(AIndex: Integer; const AValue: string); 


begin
  If (FprovisioningStatus=AValue) then exit;
  FprovisioningStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TprovisionedPlan.Setservice(AIndex: Integer; const AValue: string); 


begin
  If (Fservice=AValue) then exit;
  Fservice:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TverifiedDomain
  --------------------------------------------------------------------}


Class Function TverifiedDomain.ObjectRestKind : String; 

begin
  Result:='verifiedDomain';
end;


Procedure TverifiedDomain.Setcapabilities(AIndex: Integer; const AValue: string); 


begin
  If (Fcapabilities=AValue) then exit;
  Fcapabilities:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TverifiedDomain.SetisDefault(AIndex: Integer; const AValue: boolean); 


begin
  If (FisDefault=AValue) then exit;
  FisDefault:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TverifiedDomain.SetisInitial(AIndex: Integer; const AValue: boolean); 


begin
  If (FisInitial=AValue) then exit;
  FisInitial:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TverifiedDomain.Setname(AIndex: Integer; const AValue: string); 


begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TverifiedDomain.Set_type(AIndex: Integer; const AValue: string); 


begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;


Class Function TverifiedDomain.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;


{ --------------------------------------------------------------------
  TassignedLicense
  --------------------------------------------------------------------}


Class Function TassignedLicense.ObjectRestKind : String; 

begin
  Result:='assignedLicense';
end;


Procedure TassignedLicense.SetdisabledPlans(AIndex: Integer; const AValue: TGuidStringArray); 


begin
  If (FdisabledPlans=AValue) then exit;
  FdisabledPlans:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TassignedLicense.SetskuId(AIndex: Integer; const AValue: TGUIDString); 


begin
  If (FskuId=AValue) then exit;
  FskuId:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure TassignedLicense.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'disabledplans' : SetLength(FdisabledPlans,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


{ --------------------------------------------------------------------
  TpasswordProfile
  --------------------------------------------------------------------}


Class Function TpasswordProfile.ObjectRestKind : String; 

begin
  Result:='passwordProfile';
end;


Procedure TpasswordProfile.Setpassword(AIndex: Integer; const AValue: string); 


begin
  If (Fpassword=AValue) then exit;
  Fpassword:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TpasswordProfile.SetforceChangePasswordNextSignIn(AIndex: Integer; const AValue: boolean); 


begin
  If (FforceChangePasswordNextSignIn=AValue) then exit;
  FforceChangePasswordNextSignIn:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Treminder
  --------------------------------------------------------------------}


Class Function Treminder.ObjectRestKind : String; 

begin
  Result:='reminder';
end;


Procedure Treminder.SeteventId(AIndex: Integer; const AValue: string); 


begin
  If (FeventId=AValue) then exit;
  FeventId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Treminder.SeteventStartTime(AIndex: Integer; const AValue: TdateTimeTimeZone); 


begin
  If (FeventStartTime=AValue) then exit;
  FeventStartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Treminder.SeteventEndTime(AIndex: Integer; const AValue: TdateTimeTimeZone); 


begin
  If (FeventEndTime=AValue) then exit;
  FeventEndTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Treminder.SetchangeKey(AIndex: Integer; const AValue: string); 


begin
  If (FchangeKey=AValue) then exit;
  FchangeKey:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Treminder.SeteventSubject(AIndex: Integer; const AValue: string); 


begin
  If (FeventSubject=AValue) then exit;
  FeventSubject:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Treminder.SeteventLocation(AIndex: Integer; const AValue: Tlocation); 


begin
  If (FeventLocation=AValue) then exit;
  FeventLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Treminder.SeteventWebLink(AIndex: Integer; const AValue: string); 


begin
  If (FeventWebLink=AValue) then exit;
  FeventWebLink:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Treminder.SetreminderFireTime(AIndex: Integer; const AValue: TdateTimeTimeZone); 


begin
  If (FreminderFireTime=AValue) then exit;
  FreminderFireTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TdateTimeTimeZone
  --------------------------------------------------------------------}


Class Function TdateTimeTimeZone.ObjectRestKind : String; 

begin
  Result:='dateTimeTimeZone';
end;


Procedure TdateTimeTimeZone.SetdateTime(AIndex: Integer; const AValue: string); 


begin
  If (FdateTime=AValue) then exit;
  FdateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdateTimeTimeZone.SettimeZone(AIndex: Integer; const AValue: string); 


begin
  If (FtimeZone=AValue) then exit;
  FtimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Tlocation
  --------------------------------------------------------------------}


Class Function Tlocation.ObjectRestKind : String; 

begin
  Result:='location';
end;


Procedure Tlocation.SetdisplayName(AIndex: Integer; const AValue: string); 


begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tlocation.Setaddress(AIndex: Integer; const AValue: TphysicalAddress); 


begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TphysicalAddress
  --------------------------------------------------------------------}


Class Function TphysicalAddress.ObjectRestKind : String; 

begin
  Result:='physicalAddress';
end;


Procedure TphysicalAddress.Setstreet(AIndex: Integer; const AValue: string); 


begin
  If (Fstreet=AValue) then exit;
  Fstreet:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TphysicalAddress.Setcity(AIndex: Integer; const AValue: string); 


begin
  If (Fcity=AValue) then exit;
  Fcity:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TphysicalAddress.Setstate(AIndex: Integer; const AValue: string); 


begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TphysicalAddress.SetcountryOrRegion(AIndex: Integer; const AValue: string); 


begin
  If (FcountryOrRegion=AValue) then exit;
  FcountryOrRegion:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TphysicalAddress.SetpostalCode(AIndex: Integer; const AValue: string); 


begin
  If (FpostalCode=AValue) then exit;
  FpostalCode:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TitemBody
  --------------------------------------------------------------------}


Class Function TitemBody.ObjectRestKind : String; 

begin
  Result:='itemBody';
end;


Procedure TitemBody.SetcontentType(AIndex: Integer; const AValue: TbodyType); 


begin
  If (FcontentType=AValue) then exit;
  FcontentType:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TitemBody.Setcontent(AIndex: Integer; const AValue: string); 


begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Trecipient
  --------------------------------------------------------------------}


Class Function Trecipient.ObjectRestKind : String; 

begin
  Result:='recipient';
end;


Procedure Trecipient.SetemailAddress(AIndex: Integer; const AValue: TemailAddress); 


begin
  If (FemailAddress=AValue) then exit;
  FemailAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TemailAddress
  --------------------------------------------------------------------}


Class Function TemailAddress.ObjectRestKind : String; 

begin
  Result:='emailAddress';
end;


Procedure TemailAddress.Setname(AIndex: Integer; const AValue: string); 


begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TemailAddress.Setaddress(AIndex: Integer; const AValue: string); 


begin
  If (Faddress=AValue) then exit;
  Faddress:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TresponseStatus
  --------------------------------------------------------------------}


Class Function TresponseStatus.ObjectRestKind : String; 

begin
  Result:='responseStatus';
end;


Procedure TresponseStatus.Setresponse(AIndex: Integer; const AValue: TresponseType); 


begin
  If (Fresponse=AValue) then exit;
  Fresponse:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TresponseStatus.Settime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (Ftime=AValue) then exit;
  Ftime:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TpatternedRecurrence
  --------------------------------------------------------------------}


Class Function TpatternedRecurrence.ObjectRestKind : String; 

begin
  Result:='patternedRecurrence';
end;


Procedure TpatternedRecurrence.Setpattern(AIndex: Integer; const AValue: TrecurrencePattern); 


begin
  If (Fpattern=AValue) then exit;
  Fpattern:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TpatternedRecurrence.Setrange(AIndex: Integer; const AValue: TrecurrenceRange); 


begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TrecurrencePattern
  --------------------------------------------------------------------}


Class Function TrecurrencePattern.ObjectRestKind : String; 

begin
  Result:='recurrencePattern';
end;


Procedure TrecurrencePattern.Set_type(AIndex: Integer; const AValue: TrecurrencePatternType); 


begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TrecurrencePattern.Setinterval(AIndex: Integer; const AValue: TInt32); 


begin
  If (Finterval=AValue) then exit;
  Finterval:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TrecurrencePattern.Setmonth(AIndex: Integer; const AValue: TInt32); 


begin
  If (Fmonth=AValue) then exit;
  Fmonth:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TrecurrencePattern.SetdayOfMonth(AIndex: Integer; const AValue: TInt32); 


begin
  If (FdayOfMonth=AValue) then exit;
  FdayOfMonth:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TrecurrencePattern.SetdaysOfWeek(AIndex: Integer; const AValue: TdayOfWeekArray); 


begin
  If (FdaysOfWeek=AValue) then exit;
  FdaysOfWeek:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TrecurrencePattern.SetfirstDayOfWeek(AIndex: Integer; const AValue: TdayOfWeek); 


begin
  If (FfirstDayOfWeek=AValue) then exit;
  FfirstDayOfWeek:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TrecurrencePattern.Setindex(AIndex: Integer; const AValue: TweekIndex); 


begin
  If (Findex=AValue) then exit;
  Findex:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure TrecurrencePattern.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'daysofweek' : SetLength(FdaysOfWeek,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


Class Function TrecurrencePattern.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;


{ --------------------------------------------------------------------
  TrecurrenceRange
  --------------------------------------------------------------------}


Class Function TrecurrenceRange.ObjectRestKind : String; 

begin
  Result:='recurrenceRange';
end;


Procedure TrecurrenceRange.Set_type(AIndex: Integer; const AValue: TrecurrenceRangeType); 


begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TrecurrenceRange.SetstartDate(AIndex: Integer; const AValue: TDate); 


begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TrecurrenceRange.SetendDate(AIndex: Integer; const AValue: TDate); 


begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TrecurrenceRange.SetrecurrenceTimeZone(AIndex: Integer; const AValue: string); 


begin
  If (FrecurrenceTimeZone=AValue) then exit;
  FrecurrenceTimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TrecurrenceRange.SetnumberOfOccurrences(AIndex: Integer; const AValue: TInt32); 


begin
  If (FnumberOfOccurrences=AValue) then exit;
  FnumberOfOccurrences:=AValue;
  MarkPropertyChanged(AIndex);
end;


Class Function TrecurrenceRange.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;


{ --------------------------------------------------------------------
  Tattendee
  --------------------------------------------------------------------}


Class Function Tattendee.ObjectRestKind : String; 

begin
  Result:='attendee';
end;


Procedure Tattendee.Setstatus(AIndex: Integer; const AValue: TresponseStatus); 


begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tattendee.Set_type(AIndex: Integer; const AValue: TattendeeType); 


begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;


Class Function Tattendee.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;


{ --------------------------------------------------------------------
  TidentitySet
  --------------------------------------------------------------------}


Class Function TidentitySet.ObjectRestKind : String; 

begin
  Result:='identitySet';
end;


Procedure TidentitySet.Setapplication(AIndex: Integer; const AValue: Tidentity); 


begin
  If (Fapplication=AValue) then exit;
  Fapplication:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TidentitySet.Setdevice(AIndex: Integer; const AValue: Tidentity); 


begin
  If (Fdevice=AValue) then exit;
  Fdevice:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TidentitySet.Setuser(AIndex: Integer; const AValue: Tidentity); 


begin
  If (Fuser=AValue) then exit;
  Fuser:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Tidentity
  --------------------------------------------------------------------}


Class Function Tidentity.ObjectRestKind : String; 

begin
  Result:='identity';
end;


Procedure Tidentity.SetdisplayName(AIndex: Integer; const AValue: string); 


begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tidentity.Setid(AIndex: Integer; const AValue: string); 


begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Tquota
  --------------------------------------------------------------------}


Class Function Tquota.ObjectRestKind : String; 

begin
  Result:='quota';
end;


Procedure Tquota.Setdeleted(AIndex: Integer; const AValue: int64); 


begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tquota.Setremaining(AIndex: Integer; const AValue: int64); 


begin
  If (Fremaining=AValue) then exit;
  Fremaining:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tquota.Setstate(AIndex: Integer; const AValue: string); 


begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tquota.Settotal(AIndex: Integer; const AValue: int64); 


begin
  If (Ftotal=AValue) then exit;
  Ftotal:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tquota.Setused(AIndex: Integer; const AValue: int64); 


begin
  If (Fused=AValue) then exit;
  Fused:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TitemReference
  --------------------------------------------------------------------}


Class Function TitemReference.ObjectRestKind : String; 

begin
  Result:='itemReference';
end;


Procedure TitemReference.SetdriveId(AIndex: Integer; const AValue: string); 


begin
  If (FdriveId=AValue) then exit;
  FdriveId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TitemReference.Setid(AIndex: Integer; const AValue: string); 


begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TitemReference.Setpath(AIndex: Integer; const AValue: string); 


begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Taudio
  --------------------------------------------------------------------}


Class Function Taudio.ObjectRestKind : String; 

begin
  Result:='audio';
end;


Procedure Taudio.Setalbum(AIndex: Integer; const AValue: string); 


begin
  If (Falbum=AValue) then exit;
  Falbum:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Taudio.SetalbumArtist(AIndex: Integer; const AValue: string); 


begin
  If (FalbumArtist=AValue) then exit;
  FalbumArtist:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Taudio.Setartist(AIndex: Integer; const AValue: string); 


begin
  If (Fartist=AValue) then exit;
  Fartist:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Taudio.Setbitrate(AIndex: Integer; const AValue: int64); 


begin
  If (Fbitrate=AValue) then exit;
  Fbitrate:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Taudio.Setcomposers(AIndex: Integer; const AValue: string); 


begin
  If (Fcomposers=AValue) then exit;
  Fcomposers:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Taudio.Setcopyright(AIndex: Integer; const AValue: string); 


begin
  If (Fcopyright=AValue) then exit;
  Fcopyright:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Taudio.Setdisc(AIndex: Integer; const AValue: TInt16); 


begin
  If (Fdisc=AValue) then exit;
  Fdisc:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Taudio.SetdiscCount(AIndex: Integer; const AValue: TInt16); 


begin
  If (FdiscCount=AValue) then exit;
  FdiscCount:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Taudio.Setduration(AIndex: Integer; const AValue: int64); 


begin
  If (Fduration=AValue) then exit;
  Fduration:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Taudio.Setgenre(AIndex: Integer; const AValue: string); 


begin
  If (Fgenre=AValue) then exit;
  Fgenre:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Taudio.SethasDrm(AIndex: Integer; const AValue: boolean); 


begin
  If (FhasDrm=AValue) then exit;
  FhasDrm:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Taudio.SetisVariableBitrate(AIndex: Integer; const AValue: boolean); 


begin
  If (FisVariableBitrate=AValue) then exit;
  FisVariableBitrate:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Taudio.Settitle(AIndex: Integer; const AValue: string); 


begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Taudio.Settrack(AIndex: Integer; const AValue: TInt32); 


begin
  If (Ftrack=AValue) then exit;
  Ftrack:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Taudio.SettrackCount(AIndex: Integer; const AValue: TInt32); 


begin
  If (FtrackCount=AValue) then exit;
  FtrackCount:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Taudio.Setyear(AIndex: Integer; const AValue: TInt32); 


begin
  If (Fyear=AValue) then exit;
  Fyear:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Tdeleted
  --------------------------------------------------------------------}


Class Function Tdeleted.ObjectRestKind : String; 

begin
  Result:='deleted';
end;


Procedure Tdeleted.Setstate(AIndex: Integer; const AValue: string); 


begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  T_file
  --------------------------------------------------------------------}


Class Function T_file.ObjectRestKind : String; 

begin
  Result:='file';
end;


Procedure T_file.Sethashes(AIndex: Integer; const AValue: Thashes); 


begin
  If (Fhashes=AValue) then exit;
  Fhashes:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure T_file.SetmimeType(AIndex: Integer; const AValue: string); 


begin
  If (FmimeType=AValue) then exit;
  FmimeType:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Thashes
  --------------------------------------------------------------------}


Class Function Thashes.ObjectRestKind : String; 

begin
  Result:='hashes';
end;


Procedure Thashes.Setcrc32Hash(AIndex: Integer; const AValue: string); 


begin
  If (Fcrc32Hash=AValue) then exit;
  Fcrc32Hash:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Thashes.Setsha1Hash(AIndex: Integer; const AValue: string); 


begin
  If (Fsha1Hash=AValue) then exit;
  Fsha1Hash:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TfileSystemInfo
  --------------------------------------------------------------------}


Class Function TfileSystemInfo.ObjectRestKind : String; 

begin
  Result:='fileSystemInfo';
end;


Procedure TfileSystemInfo.SetcreatedDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FcreatedDateTime=AValue) then exit;
  FcreatedDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TfileSystemInfo.SetlastModifiedDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FlastModifiedDateTime=AValue) then exit;
  FlastModifiedDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Tfolder
  --------------------------------------------------------------------}


Class Function Tfolder.ObjectRestKind : String; 

begin
  Result:='folder';
end;


Procedure Tfolder.SetchildCount(AIndex: Integer; const AValue: TInt32); 


begin
  If (FchildCount=AValue) then exit;
  FchildCount:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Timage
  --------------------------------------------------------------------}


Class Function Timage.ObjectRestKind : String; 

begin
  Result:='image';
end;


Procedure Timage.Setheight(AIndex: Integer; const AValue: TInt32); 


begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Timage.Setwidth(AIndex: Integer; const AValue: TInt32); 


begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TgeoCoordinates
  --------------------------------------------------------------------}


Class Function TgeoCoordinates.ObjectRestKind : String; 

begin
  Result:='geoCoordinates';
end;


Procedure TgeoCoordinates.Setaltitude(AIndex: Integer; const AValue: Double); 


begin
  If (Faltitude=AValue) then exit;
  Faltitude:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TgeoCoordinates.Setlatitude(AIndex: Integer; const AValue: Double); 


begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TgeoCoordinates.Setlongitude(AIndex: Integer; const AValue: Double); 


begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Tphoto
  --------------------------------------------------------------------}


Class Function Tphoto.ObjectRestKind : String; 

begin
  Result:='photo';
end;


Procedure Tphoto.SetcameraMake(AIndex: Integer; const AValue: string); 


begin
  If (FcameraMake=AValue) then exit;
  FcameraMake:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tphoto.SetcameraModel(AIndex: Integer; const AValue: string); 


begin
  If (FcameraModel=AValue) then exit;
  FcameraModel:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tphoto.SetexposureDenominator(AIndex: Integer; const AValue: Double); 


begin
  If (FexposureDenominator=AValue) then exit;
  FexposureDenominator:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tphoto.SetexposureNumerator(AIndex: Integer; const AValue: Double); 


begin
  If (FexposureNumerator=AValue) then exit;
  FexposureNumerator:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tphoto.SetfocalLength(AIndex: Integer; const AValue: Double); 


begin
  If (FfocalLength=AValue) then exit;
  FfocalLength:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tphoto.SetfNumber(AIndex: Integer; const AValue: Double); 


begin
  If (FfNumber=AValue) then exit;
  FfNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tphoto.SettakenDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FtakenDateTime=AValue) then exit;
  FtakenDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tphoto.Setiso(AIndex: Integer; const AValue: TInt32); 


begin
  If (Fiso=AValue) then exit;
  Fiso:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TremoteItem
  --------------------------------------------------------------------}


Class Function TremoteItem.ObjectRestKind : String; 

begin
  Result:='remoteItem';
end;


Procedure TremoteItem.Set_file(AIndex: Integer; const AValue: T_file); 


begin
  If (F_file=AValue) then exit;
  F_file:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TremoteItem.SetfileSystemInfo(AIndex: Integer; const AValue: TfileSystemInfo); 


begin
  If (FfileSystemInfo=AValue) then exit;
  FfileSystemInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TremoteItem.Setfolder(AIndex: Integer; const AValue: Tfolder); 


begin
  If (Ffolder=AValue) then exit;
  Ffolder:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TremoteItem.Setid(AIndex: Integer; const AValue: string); 


begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TremoteItem.Setname(AIndex: Integer; const AValue: string); 


begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TremoteItem.SetparentReference(AIndex: Integer; const AValue: TitemReference); 


begin
  If (FparentReference=AValue) then exit;
  FparentReference:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TremoteItem.Setsize(AIndex: Integer; const AValue: int64); 


begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;


Class Function TremoteItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_file' : Result:='file';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;


{ --------------------------------------------------------------------
  TsearchResult
  --------------------------------------------------------------------}


Class Function TsearchResult.ObjectRestKind : String; 

begin
  Result:='searchResult';
end;


Procedure TsearchResult.SetonClickTelemetryUrl(AIndex: Integer; const AValue: string); 


begin
  If (FonClickTelemetryUrl=AValue) then exit;
  FonClickTelemetryUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Tshared
  --------------------------------------------------------------------}


Class Function Tshared.ObjectRestKind : String; 

begin
  Result:='shared';
end;


Procedure Tshared.Setowner(AIndex: Integer; const AValue: TidentitySet); 


begin
  If (Fowner=AValue) then exit;
  Fowner:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tshared.Setscope(AIndex: Integer; const AValue: string); 


begin
  If (Fscope=AValue) then exit;
  Fscope:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TspecialFolder
  --------------------------------------------------------------------}


Class Function TspecialFolder.ObjectRestKind : String; 

begin
  Result:='specialFolder';
end;


Procedure TspecialFolder.Setname(AIndex: Integer; const AValue: string); 


begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Tvideo
  --------------------------------------------------------------------}


Class Function Tvideo.ObjectRestKind : String; 

begin
  Result:='video';
end;


Procedure Tvideo.Setbitrate(AIndex: Integer; const AValue: TInt32); 


begin
  If (Fbitrate=AValue) then exit;
  Fbitrate:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tvideo.Setduration(AIndex: Integer; const AValue: int64); 


begin
  If (Fduration=AValue) then exit;
  Fduration:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tvideo.Setheight(AIndex: Integer; const AValue: TInt32); 


begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tvideo.Setwidth(AIndex: Integer; const AValue: TInt32); 


begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Tpackage
  --------------------------------------------------------------------}


Class Function Tpackage.ObjectRestKind : String; 

begin
  Result:='package';
end;


Procedure Tpackage.Set_type(AIndex: Integer; const AValue: string); 


begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;


Class Function Tpackage.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;


{ --------------------------------------------------------------------
  TsharingInvitation
  --------------------------------------------------------------------}


Class Function TsharingInvitation.ObjectRestKind : String; 

begin
  Result:='sharingInvitation';
end;


Procedure TsharingInvitation.Setemail(AIndex: Integer; const AValue: string); 


begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TsharingInvitation.SetinvitedBy(AIndex: Integer; const AValue: TidentitySet); 


begin
  If (FinvitedBy=AValue) then exit;
  FinvitedBy:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TsharingInvitation.SetredeemedBy(AIndex: Integer; const AValue: string); 


begin
  If (FredeemedBy=AValue) then exit;
  FredeemedBy:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TsharingInvitation.SetsignInRequired(AIndex: Integer; const AValue: boolean); 


begin
  If (FsignInRequired=AValue) then exit;
  FsignInRequired:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TsharingLink
  --------------------------------------------------------------------}


Class Function TsharingLink.ObjectRestKind : String; 

begin
  Result:='sharingLink';
end;


Procedure TsharingLink.Setapplication(AIndex: Integer; const AValue: Tidentity); 


begin
  If (Fapplication=AValue) then exit;
  Fapplication:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TsharingLink.Set_type(AIndex: Integer; const AValue: string); 


begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TsharingLink.SetwebUrl(AIndex: Integer; const AValue: string); 


begin
  If (FwebUrl=AValue) then exit;
  FwebUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;


Class Function TsharingLink.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;


{ --------------------------------------------------------------------
  Tthumbnail
  --------------------------------------------------------------------}


Class Function Tthumbnail.ObjectRestKind : String; 

begin
  Result:='thumbnail';
end;


Procedure Tthumbnail.Setcontent(AIndex: Integer; const AValue: TStream); 


begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tthumbnail.Setheight(AIndex: Integer; const AValue: TInt32); 


begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tthumbnail.Seturl(AIndex: Integer; const AValue: string); 


begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tthumbnail.Setwidth(AIndex: Integer; const AValue: TInt32); 


begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Tentity
  --------------------------------------------------------------------}


Class Function Tentity.ObjectRestKind : String; 

begin
  Result:='entity';
end;


Procedure Tentity.Setid(AIndex: Integer; const AValue: string); 


begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;


Function Tentity.KeyAsURLPart : string;

begin
  Result:=TODataObject.MakeKeyString(id);
end;


{ --------------------------------------------------------------------
  TdirectoryObject
  --------------------------------------------------------------------}


Function TdirectoryObject.checkMemberGroups(AService: TODataService; groupIds: TStringArray) : TStringArray; 

Var
  _JSON : TJSONObject;
  _ARR : TJSONArray;
  _res : String;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('groupIds',DynArrayToJSONArray(Pointer(groupIds),'String',Nil));
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.checkMemberGroups';
  _Res:=AService.ServiceCall('POST',_Path,'',_Data);
  _arr:=GetJSON(_res) as TJSONArray;
  try
    Result:=TStringArray(JSONArrayToDynArray(_arr,'String',Nil));
  finally
    _arr.Free;
  end
end;


Function TdirectoryObject.getMemberGroups(AService: TODataService; securityEnabledOnly: boolean) : TStringArray; 

Var
  _JSON : TJSONObject;
  _ARR : TJSONArray;
  _res : String;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('securityEnabledOnly',securityEnabledOnly);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.getMemberGroups';
  _Res:=AService.ServiceCall('POST',_Path,'',_Data);
  _arr:=GetJSON(_res) as TJSONArray;
  try
    Result:=TStringArray(JSONArrayToDynArray(_arr,'String',Nil));
  finally
    _arr.Free;
  end
end;


Function TdirectoryObject.getMemberObjects(AService: TODataService; securityEnabledOnly: boolean) : TStringArray; 

Var
  _JSON : TJSONObject;
  _ARR : TJSONArray;
  _res : String;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('securityEnabledOnly',securityEnabledOnly);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.getMemberObjects';
  _Res:=AService.ServiceCall('POST',_Path,'',_Data);
  _arr:=GetJSON(_res) as TJSONArray;
  try
    Result:=TStringArray(JSONArrayToDynArray(_arr,'String',Nil));
  finally
    _arr.Free;
  end
end;


Class Function TdirectoryObject.ObjectRestKind : String; 

begin
  Result:='directoryObject';
end;


{ --------------------------------------------------------------------
  Tdevice
  --------------------------------------------------------------------}


Class Function Tdevice.ObjectRestKind : String; 

begin
  Result:='device';
end;


Procedure Tdevice.SetaccountEnabled(AIndex: Integer; const AValue: boolean); 


begin
  If (FaccountEnabled=AValue) then exit;
  FaccountEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tdevice.SetalternativeSecurityIds(AIndex: Integer; const AValue: TalternativeSecurityIdArray); 


begin
  If (FalternativeSecurityIds=AValue) then exit;
  FalternativeSecurityIds:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tdevice.SetapproximateLastSignInDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FapproximateLastSignInDateTime=AValue) then exit;
  FapproximateLastSignInDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tdevice.SetdeviceId(AIndex: Integer; const AValue: string); 


begin
  If (FdeviceId=AValue) then exit;
  FdeviceId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tdevice.SetdeviceMetadata(AIndex: Integer; const AValue: string); 


begin
  If (FdeviceMetadata=AValue) then exit;
  FdeviceMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tdevice.SetdeviceVersion(AIndex: Integer; const AValue: TInt32); 


begin
  If (FdeviceVersion=AValue) then exit;
  FdeviceVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tdevice.SetdisplayName(AIndex: Integer; const AValue: string); 


begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tdevice.SetisCompliant(AIndex: Integer; const AValue: boolean); 


begin
  If (FisCompliant=AValue) then exit;
  FisCompliant:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tdevice.SetisManaged(AIndex: Integer; const AValue: boolean); 


begin
  If (FisManaged=AValue) then exit;
  FisManaged:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tdevice.SetonPremisesLastSyncDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FonPremisesLastSyncDateTime=AValue) then exit;
  FonPremisesLastSyncDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tdevice.SetonPremisesSyncEnabled(AIndex: Integer; const AValue: boolean); 


begin
  If (FonPremisesSyncEnabled=AValue) then exit;
  FonPremisesSyncEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tdevice.SetoperatingSystem(AIndex: Integer; const AValue: string); 


begin
  If (FoperatingSystem=AValue) then exit;
  FoperatingSystem:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tdevice.SetoperatingSystemVersion(AIndex: Integer; const AValue: string); 


begin
  If (FoperatingSystemVersion=AValue) then exit;
  FoperatingSystemVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tdevice.SetphysicalIds(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FphysicalIds=AValue) then exit;
  FphysicalIds:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tdevice.SettrustType(AIndex: Integer; const AValue: string); 


begin
  If (FtrustType=AValue) then exit;
  FtrustType:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure Tdevice.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'alternativesecurityids' : SetLength(FalternativeSecurityIds,aLength);
  'physicalids' : SetLength(FphysicalIds,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


Function Tdevice.registeredOwners(AService: TODataService) : TdirectoryObjectsEntitySet; 


begin
  Result:=TdirectoryObjectsEntitySet(CreateContainedEntitySet(AService,'registeredOwners', TdirectoryObjectsEntitySet));
end;


Function Tdevice.registeredUsers(AService: TODataService) : TdirectoryObjectsEntitySet; 


begin
  Result:=TdirectoryObjectsEntitySet(CreateContainedEntitySet(AService,'registeredUsers', TdirectoryObjectsEntitySet));
end;


{ --------------------------------------------------------------------
  TdirectoryRole
  --------------------------------------------------------------------}


Class Function TdirectoryRole.ObjectRestKind : String; 

begin
  Result:='directoryRole';
end;


Procedure TdirectoryRole.Setdescription(AIndex: Integer; const AValue: string); 


begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdirectoryRole.SetdisplayName(AIndex: Integer; const AValue: string); 


begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdirectoryRole.SetroleTemplateId(AIndex: Integer; const AValue: string); 


begin
  If (FroleTemplateId=AValue) then exit;
  FroleTemplateId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Function TdirectoryRole.members(AService: TODataService) : TdirectoryObjectsEntitySet; 


begin
  Result:=TdirectoryObjectsEntitySet(CreateContainedEntitySet(AService,'members', TdirectoryObjectsEntitySet));
end;


{ --------------------------------------------------------------------
  TdirectoryRoleTemplate
  --------------------------------------------------------------------}


Class Function TdirectoryRoleTemplate.ObjectRestKind : String; 

begin
  Result:='directoryRoleTemplate';
end;


Procedure TdirectoryRoleTemplate.Setdescription(AIndex: Integer; const AValue: string); 


begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdirectoryRoleTemplate.SetdisplayName(AIndex: Integer; const AValue: string); 


begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Tgroup
  --------------------------------------------------------------------}


Procedure Tgroup.subscribeByMail(AService: TODataService); 

Var
  _data : String;
  _Path : String;
begin
  _data:='';
  _Path:=BaseURL(AService)+'/microsoft.graph.subscribeByMail';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Procedure Tgroup.unsubscribeByMail(AService: TODataService); 

Var
  _data : String;
  _Path : String;
begin
  _data:='';
  _Path:=BaseURL(AService)+'/microsoft.graph.unsubscribeByMail';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Procedure Tgroup.addFavorite(AService: TODataService); 

Var
  _data : String;
  _Path : String;
begin
  _data:='';
  _Path:=BaseURL(AService)+'/microsoft.graph.addFavorite';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Procedure Tgroup.removeFavorite(AService: TODataService); 

Var
  _data : String;
  _Path : String;
begin
  _data:='';
  _Path:=BaseURL(AService)+'/microsoft.graph.removeFavorite';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Procedure Tgroup.resetUnseenCount(AService: TODataService); 

Var
  _data : String;
  _Path : String;
begin
  _data:='';
  _Path:=BaseURL(AService)+'/microsoft.graph.resetUnseenCount';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Class Function Tgroup.ObjectRestKind : String; 

begin
  Result:='group';
end;


Procedure Tgroup.Setdescription(AIndex: Integer; const AValue: string); 


begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tgroup.SetdisplayName(AIndex: Integer; const AValue: string); 


begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tgroup.SetgroupTypes(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FgroupTypes=AValue) then exit;
  FgroupTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tgroup.Setmail(AIndex: Integer; const AValue: string); 


begin
  If (Fmail=AValue) then exit;
  Fmail:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tgroup.SetmailEnabled(AIndex: Integer; const AValue: boolean); 


begin
  If (FmailEnabled=AValue) then exit;
  FmailEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tgroup.SetmailNickname(AIndex: Integer; const AValue: string); 


begin
  If (FmailNickname=AValue) then exit;
  FmailNickname:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tgroup.SetonPremisesLastSyncDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FonPremisesLastSyncDateTime=AValue) then exit;
  FonPremisesLastSyncDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tgroup.SetonPremisesSecurityIdentifier(AIndex: Integer; const AValue: string); 


begin
  If (FonPremisesSecurityIdentifier=AValue) then exit;
  FonPremisesSecurityIdentifier:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tgroup.SetonPremisesSyncEnabled(AIndex: Integer; const AValue: boolean); 


begin
  If (FonPremisesSyncEnabled=AValue) then exit;
  FonPremisesSyncEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tgroup.SetproxyAddresses(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FproxyAddresses=AValue) then exit;
  FproxyAddresses:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tgroup.SetsecurityEnabled(AIndex: Integer; const AValue: boolean); 


begin
  If (FsecurityEnabled=AValue) then exit;
  FsecurityEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tgroup.Setvisibility(AIndex: Integer; const AValue: string); 


begin
  If (Fvisibility=AValue) then exit;
  Fvisibility:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tgroup.SetallowExternalSenders(AIndex: Integer; const AValue: boolean); 


begin
  If (FallowExternalSenders=AValue) then exit;
  FallowExternalSenders:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tgroup.SetautoSubscribeNewMembers(AIndex: Integer; const AValue: boolean); 


begin
  If (FautoSubscribeNewMembers=AValue) then exit;
  FautoSubscribeNewMembers:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tgroup.SetisSubscribedByMail(AIndex: Integer; const AValue: boolean); 


begin
  If (FisSubscribedByMail=AValue) then exit;
  FisSubscribedByMail:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tgroup.SetunseenCount(AIndex: Integer; const AValue: TInt32); 


begin
  If (FunseenCount=AValue) then exit;
  FunseenCount:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure Tgroup.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'grouptypes' : SetLength(FgroupTypes,aLength);
  'proxyaddresses' : SetLength(FproxyAddresses,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


Function Tgroup.members(AService: TODataService) : TdirectoryObjectsEntitySet; 


begin
  Result:=TdirectoryObjectsEntitySet(CreateContainedEntitySet(AService,'members', TdirectoryObjectsEntitySet));
end;


Function Tgroup.memberOf(AService: TODataService) : TdirectoryObjectsEntitySet; 


begin
  Result:=TdirectoryObjectsEntitySet(CreateContainedEntitySet(AService,'memberOf', TdirectoryObjectsEntitySet));
end;


Function Tgroup.createdOnBehalfOf(AService: TODataService) : TdirectoryObject; 


begin
  Result:=TdirectoryObject(GetContainedSingleTon(AService,'createdOnBehalfOf', TdirectoryObject));
end;


Function Tgroup.owners(AService: TODataService) : TdirectoryObjectsEntitySet; 


begin
  Result:=TdirectoryObjectsEntitySet(CreateContainedEntitySet(AService,'owners', TdirectoryObjectsEntitySet));
end;


Function Tgroup.threads(AService: TODataService) : TconversationThreadImplicitEntitySet; 


begin
  Result:=TconversationThreadImplicitEntitySet(CreateContainedEntitySet(AService,'threads', TconversationThreadImplicitEntitySet));
end;


Function Tgroup.calendar(AService: TODataService) : Tcalendar; 


begin
  Result:=Tcalendar(GetContainedSingleTon(AService,'calendar', Tcalendar));
end;


Function Tgroup.calendarView(AService: TODataService) : TeventImplicitEntitySet; 


begin
  Result:=TeventImplicitEntitySet(CreateContainedEntitySet(AService,'calendarView', TeventImplicitEntitySet));
end;


Function Tgroup.events(AService: TODataService) : TeventImplicitEntitySet; 


begin
  Result:=TeventImplicitEntitySet(CreateContainedEntitySet(AService,'events', TeventImplicitEntitySet));
end;


Function Tgroup.conversations(AService: TODataService) : TconversationImplicitEntitySet; 


begin
  Result:=TconversationImplicitEntitySet(CreateContainedEntitySet(AService,'conversations', TconversationImplicitEntitySet));
end;


Function Tgroup.photo(AService: TODataService) : TprofilePhoto; 


begin
  Result:=TprofilePhoto(GetContainedSingleTon(AService,'photo', TprofilePhoto));
end;


Function Tgroup.acceptedSenders(AService: TODataService) : TdirectoryObjectsEntitySet; 


begin
  Result:=TdirectoryObjectsEntitySet(CreateContainedEntitySet(AService,'acceptedSenders', TdirectoryObjectsEntitySet));
end;


Function Tgroup.rejectedSenders(AService: TODataService) : TdirectoryObjectsEntitySet; 


begin
  Result:=TdirectoryObjectsEntitySet(CreateContainedEntitySet(AService,'rejectedSenders', TdirectoryObjectsEntitySet));
end;


Function Tgroup.drive(AService: TODataService) : Tdrive; 


begin
  Result:=Tdrive(GetContainedSingleTon(AService,'drive', Tdrive));
end;


{ --------------------------------------------------------------------
  TconversationThread
  --------------------------------------------------------------------}


Procedure TconversationThread.reply(AService: TODataService; _Post: Tpost); 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('Post',_Post.SaveToJSON);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.reply';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Class Function TconversationThread.ObjectRestKind : String; 

begin
  Result:='conversationThread';
end;


Procedure TconversationThread.SettoRecipients(AIndex: Integer; const AValue: TrecipientArray); 


begin
  If (FtoRecipients=AValue) then exit;
  FtoRecipients:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TconversationThread.Settopic(AIndex: Integer; const AValue: string); 


begin
  If (Ftopic=AValue) then exit;
  Ftopic:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TconversationThread.SethasAttachments(AIndex: Integer; const AValue: boolean); 


begin
  If (FhasAttachments=AValue) then exit;
  FhasAttachments:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TconversationThread.SetlastDeliveredDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FlastDeliveredDateTime=AValue) then exit;
  FlastDeliveredDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TconversationThread.SetuniqueSenders(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FuniqueSenders=AValue) then exit;
  FuniqueSenders:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TconversationThread.SetccRecipients(AIndex: Integer; const AValue: TrecipientArray); 


begin
  If (FccRecipients=AValue) then exit;
  FccRecipients:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TconversationThread.Setpreview(AIndex: Integer; const AValue: string); 


begin
  If (Fpreview=AValue) then exit;
  Fpreview:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TconversationThread.SetisLocked(AIndex: Integer; const AValue: boolean); 


begin
  If (FisLocked=AValue) then exit;
  FisLocked:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure TconversationThread.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'torecipients' : SetLength(FtoRecipients,aLength);
  'uniquesenders' : SetLength(FuniqueSenders,aLength);
  'ccrecipients' : SetLength(FccRecipients,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


Function TconversationThread.posts(AService: TODataService) : TpostImplicitEntitySet; 


begin
  Result:=TpostImplicitEntitySet(CreateContainedEntitySet(AService,'posts', TpostImplicitEntitySet));
end;


{ --------------------------------------------------------------------
  Tcalendar
  --------------------------------------------------------------------}


Class Function Tcalendar.ObjectRestKind : String; 

begin
  Result:='calendar';
end;


Procedure Tcalendar.Setname(AIndex: Integer; const AValue: string); 


begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcalendar.Setcolor(AIndex: Integer; const AValue: TcalendarColor); 


begin
  If (Fcolor=AValue) then exit;
  Fcolor:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcalendar.SetchangeKey(AIndex: Integer; const AValue: string); 


begin
  If (FchangeKey=AValue) then exit;
  FchangeKey:=AValue;
  MarkPropertyChanged(AIndex);
end;


Function Tcalendar.events(AService: TODataService) : TeventImplicitEntitySet; 


begin
  Result:=TeventImplicitEntitySet(CreateContainedEntitySet(AService,'events', TeventImplicitEntitySet));
end;


Function Tcalendar.calendarView(AService: TODataService) : TeventImplicitEntitySet; 


begin
  Result:=TeventImplicitEntitySet(CreateContainedEntitySet(AService,'calendarView', TeventImplicitEntitySet));
end;


{ --------------------------------------------------------------------
  ToutlookItem
  --------------------------------------------------------------------}


Class Function ToutlookItem.ObjectRestKind : String; 

begin
  Result:='outlookItem';
end;


Procedure ToutlookItem.SetcreatedDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FcreatedDateTime=AValue) then exit;
  FcreatedDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure ToutlookItem.SetlastModifiedDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FlastModifiedDateTime=AValue) then exit;
  FlastModifiedDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure ToutlookItem.SetchangeKey(AIndex: Integer; const AValue: string); 


begin
  If (FchangeKey=AValue) then exit;
  FchangeKey:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure ToutlookItem.Setcategories(AIndex: Integer; const AValue: TStringArray); 


begin
  If (Fcategories=AValue) then exit;
  Fcategories:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure ToutlookItem.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'categories' : SetLength(Fcategories,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


{ --------------------------------------------------------------------
  Tevent
  --------------------------------------------------------------------}


Procedure Tevent.accept(AService: TODataService; Comment: string; SendResponse: boolean); 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('Comment',Comment);
    _JSON.Add('SendResponse',SendResponse);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.accept';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Procedure Tevent.decline(AService: TODataService; Comment: string; SendResponse: boolean); 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('Comment',Comment);
    _JSON.Add('SendResponse',SendResponse);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.decline';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Procedure Tevent.tentativelyAccept(AService: TODataService; Comment: string; SendResponse: boolean); 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('Comment',Comment);
    _JSON.Add('SendResponse',SendResponse);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.tentativelyAccept';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Procedure Tevent.snoozeReminder(AService: TODataService; NewReminderTime: TdateTimeTimeZone); 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('NewReminderTime',NewReminderTime.SaveToJSON);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.snoozeReminder';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Procedure Tevent.dismissReminder(AService: TODataService); 

Var
  _data : String;
  _Path : String;
begin
  _data:='';
  _Path:=BaseURL(AService)+'/microsoft.graph.dismissReminder';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Class Function Tevent.ObjectRestKind : String; 

begin
  Result:='event';
end;


Procedure Tevent.SetoriginalStartTimeZone(AIndex: Integer; const AValue: string); 


begin
  If (ForiginalStartTimeZone=AValue) then exit;
  ForiginalStartTimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.SetoriginalEndTimeZone(AIndex: Integer; const AValue: string); 


begin
  If (ForiginalEndTimeZone=AValue) then exit;
  ForiginalEndTimeZone:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.SetresponseStatus(AIndex: Integer; const AValue: TresponseStatus); 


begin
  If (FresponseStatus=AValue) then exit;
  FresponseStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.SetiCalUId(AIndex: Integer; const AValue: string); 


begin
  If (FiCalUId=AValue) then exit;
  FiCalUId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.SetreminderMinutesBeforeStart(AIndex: Integer; const AValue: TInt32); 


begin
  If (FreminderMinutesBeforeStart=AValue) then exit;
  FreminderMinutesBeforeStart:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.SetisReminderOn(AIndex: Integer; const AValue: boolean); 


begin
  If (FisReminderOn=AValue) then exit;
  FisReminderOn:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.SethasAttachments(AIndex: Integer; const AValue: boolean); 


begin
  If (FhasAttachments=AValue) then exit;
  FhasAttachments:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.Setsubject(AIndex: Integer; const AValue: string); 


begin
  If (Fsubject=AValue) then exit;
  Fsubject:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.Setbody(AIndex: Integer; const AValue: TitemBody); 


begin
  If (Fbody=AValue) then exit;
  Fbody:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.SetbodyPreview(AIndex: Integer; const AValue: string); 


begin
  If (FbodyPreview=AValue) then exit;
  FbodyPreview:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.Setimportance(AIndex: Integer; const AValue: Timportance); 


begin
  If (Fimportance=AValue) then exit;
  Fimportance:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.Setsensitivity(AIndex: Integer; const AValue: Tsensitivity); 


begin
  If (Fsensitivity=AValue) then exit;
  Fsensitivity:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.Setstart(AIndex: Integer; const AValue: TdateTimeTimeZone); 


begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.SetoriginalStart(AIndex: Integer; const AValue: TDateTime); 


begin
  If (ForiginalStart=AValue) then exit;
  ForiginalStart:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.Set_end(AIndex: Integer; const AValue: TdateTimeTimeZone); 


begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.Setlocation(AIndex: Integer; const AValue: Tlocation); 


begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.SetisAllDay(AIndex: Integer; const AValue: boolean); 


begin
  If (FisAllDay=AValue) then exit;
  FisAllDay:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.SetisCancelled(AIndex: Integer; const AValue: boolean); 


begin
  If (FisCancelled=AValue) then exit;
  FisCancelled:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.SetisOrganizer(AIndex: Integer; const AValue: boolean); 


begin
  If (FisOrganizer=AValue) then exit;
  FisOrganizer:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.Setrecurrence(AIndex: Integer; const AValue: TpatternedRecurrence); 


begin
  If (Frecurrence=AValue) then exit;
  Frecurrence:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.SetresponseRequested(AIndex: Integer; const AValue: boolean); 


begin
  If (FresponseRequested=AValue) then exit;
  FresponseRequested:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.SetseriesMasterId(AIndex: Integer; const AValue: string); 


begin
  If (FseriesMasterId=AValue) then exit;
  FseriesMasterId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.SetshowAs(AIndex: Integer; const AValue: TfreeBusyStatus); 


begin
  If (FshowAs=AValue) then exit;
  FshowAs:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.Set_type(AIndex: Integer; const AValue: TeventType); 


begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.Setattendees(AIndex: Integer; const AValue: TattendeeArray); 


begin
  If (Fattendees=AValue) then exit;
  Fattendees:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.Setorganizer(AIndex: Integer; const AValue: Trecipient); 


begin
  If (Forganizer=AValue) then exit;
  Forganizer:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tevent.SetwebLink(AIndex: Integer; const AValue: string); 


begin
  If (FwebLink=AValue) then exit;
  FwebLink:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure Tevent.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'attendees' : SetLength(Fattendees,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


Class Function Tevent.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;


Function Tevent.calendar(AService: TODataService) : Tcalendar; 


begin
  Result:=Tcalendar(GetContainedSingleTon(AService,'calendar', Tcalendar));
end;


Function Tevent.instances(AService: TODataService) : TeventImplicitEntitySet; 


begin
  Result:=TeventImplicitEntitySet(CreateContainedEntitySet(AService,'instances', TeventImplicitEntitySet));
end;


Function Tevent.attachments(AService: TODataService) : TattachmentImplicitEntitySet; 


begin
  Result:=TattachmentImplicitEntitySet(CreateContainedEntitySet(AService,'attachments', TattachmentImplicitEntitySet));
end;


{ --------------------------------------------------------------------
  Tconversation
  --------------------------------------------------------------------}


Class Function Tconversation.ObjectRestKind : String; 

begin
  Result:='conversation';
end;


Procedure Tconversation.Settopic(AIndex: Integer; const AValue: string); 


begin
  If (Ftopic=AValue) then exit;
  Ftopic:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tconversation.SethasAttachments(AIndex: Integer; const AValue: boolean); 


begin
  If (FhasAttachments=AValue) then exit;
  FhasAttachments:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tconversation.SetlastDeliveredDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FlastDeliveredDateTime=AValue) then exit;
  FlastDeliveredDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tconversation.SetuniqueSenders(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FuniqueSenders=AValue) then exit;
  FuniqueSenders:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tconversation.Setpreview(AIndex: Integer; const AValue: string); 


begin
  If (Fpreview=AValue) then exit;
  Fpreview:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure Tconversation.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'uniquesenders' : SetLength(FuniqueSenders,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


Function Tconversation.threads(AService: TODataService) : TconversationThreadImplicitEntitySet; 


begin
  Result:=TconversationThreadImplicitEntitySet(CreateContainedEntitySet(AService,'threads', TconversationThreadImplicitEntitySet));
end;


{ --------------------------------------------------------------------
  TprofilePhoto
  --------------------------------------------------------------------}


Class Function TprofilePhoto.ObjectRestKind : String; 

begin
  Result:='profilePhoto';
end;


Procedure TprofilePhoto.Setheight(AIndex: Integer; const AValue: TInt32); 


begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TprofilePhoto.Setwidth(AIndex: Integer; const AValue: TInt32); 


begin
  If (Fwidth=AValue) then exit;
  Fwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TprofilePhoto.GetStream(AService: TODataService; AContentType: String; AStream: TStream); 


begin
  DoGetStream(AService,AContentType,AStream);
end;


Procedure TprofilePhoto.SetStream(AService: TODataService; AContentType: String; AStream: TStream); 


begin
  DoSetStream(AService,AContentType,AStream);
end;


{ --------------------------------------------------------------------
  Tdrive
  --------------------------------------------------------------------}


Function Tdrive.recent(AService: TODataService) : TdriveItemArray; 

Var
  _Res : String;
  _Path : String;
begin
  _Path:='('+_Path+')';
  _Path:='microsoft.graph.recent'+_Path;
  _Path:=BaseURL(AService)+'/'+_Path;
  Result:=TdriveItemArray(AService.GetMulti(_Path,'',TdriveItem,True,_Res));
end;


Function Tdrive.sharedWithMe(AService: TODataService) : TdriveItemArray; 

Var
  _Res : String;
  _Path : String;
begin
  _Path:='('+_Path+')';
  _Path:='microsoft.graph.sharedWithMe'+_Path;
  _Path:=BaseURL(AService)+'/'+_Path;
  Result:=TdriveItemArray(AService.GetMulti(_Path,'',TdriveItem,True,_Res));
end;


Class Function Tdrive.ObjectRestKind : String; 

begin
  Result:='drive';
end;


Procedure Tdrive.SetdriveType(AIndex: Integer; const AValue: string); 


begin
  If (FdriveType=AValue) then exit;
  FdriveType:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tdrive.Setowner(AIndex: Integer; const AValue: TidentitySet); 


begin
  If (Fowner=AValue) then exit;
  Fowner:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tdrive.Setquota(AIndex: Integer; const AValue: Tquota); 


begin
  If (Fquota=AValue) then exit;
  Fquota:=AValue;
  MarkPropertyChanged(AIndex);
end;


Function Tdrive.items(AService: TODataService) : TdriveItemImplicitEntitySet; 


begin
  Result:=TdriveItemImplicitEntitySet(CreateContainedEntitySet(AService,'items', TdriveItemImplicitEntitySet));
end;


Function Tdrive.special(AService: TODataService) : TdriveItemImplicitEntitySet; 


begin
  Result:=TdriveItemImplicitEntitySet(CreateContainedEntitySet(AService,'special', TdriveItemImplicitEntitySet));
end;


Function Tdrive.root(AService: TODataService) : TdriveItem; 


begin
  Result:=TdriveItem(GetContainedSingleTon(AService,'root', TdriveItem));
end;


{ --------------------------------------------------------------------
  TsubscribedSku
  --------------------------------------------------------------------}


Class Function TsubscribedSku.ObjectRestKind : String; 

begin
  Result:='subscribedSku';
end;


Procedure TsubscribedSku.SetcapabilityStatus(AIndex: Integer; const AValue: string); 


begin
  If (FcapabilityStatus=AValue) then exit;
  FcapabilityStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TsubscribedSku.SetconsumedUnits(AIndex: Integer; const AValue: TInt32); 


begin
  If (FconsumedUnits=AValue) then exit;
  FconsumedUnits:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TsubscribedSku.SetprepaidUnits(AIndex: Integer; const AValue: TlicenseUnitsDetail); 


begin
  If (FprepaidUnits=AValue) then exit;
  FprepaidUnits:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TsubscribedSku.SetservicePlans(AIndex: Integer; const AValue: TservicePlanInfoArray); 


begin
  If (FservicePlans=AValue) then exit;
  FservicePlans:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TsubscribedSku.SetskuId(AIndex: Integer; const AValue: TGUIDString); 


begin
  If (FskuId=AValue) then exit;
  FskuId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TsubscribedSku.SetskuPartNumber(AIndex: Integer; const AValue: string); 


begin
  If (FskuPartNumber=AValue) then exit;
  FskuPartNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TsubscribedSku.SetappliesTo(AIndex: Integer; const AValue: string); 


begin
  If (FappliesTo=AValue) then exit;
  FappliesTo:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure TsubscribedSku.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'serviceplans' : SetLength(FservicePlans,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


{ --------------------------------------------------------------------
  Torganization
  --------------------------------------------------------------------}


Class Function Torganization.ObjectRestKind : String; 

begin
  Result:='organization';
end;


Procedure Torganization.SetassignedPlans(AIndex: Integer; const AValue: TassignedPlanArray); 


begin
  If (FassignedPlans=AValue) then exit;
  FassignedPlans:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.SetbusinessPhones(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FbusinessPhones=AValue) then exit;
  FbusinessPhones:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.Setcity(AIndex: Integer; const AValue: string); 


begin
  If (Fcity=AValue) then exit;
  Fcity:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.Setcountry(AIndex: Integer; const AValue: string); 


begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.SetcountryLetterCode(AIndex: Integer; const AValue: string); 


begin
  If (FcountryLetterCode=AValue) then exit;
  FcountryLetterCode:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.SetdisplayName(AIndex: Integer; const AValue: string); 


begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.SetmarketingNotificationEmails(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FmarketingNotificationEmails=AValue) then exit;
  FmarketingNotificationEmails:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.SetonPremisesLastSyncDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FonPremisesLastSyncDateTime=AValue) then exit;
  FonPremisesLastSyncDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.SetonPremisesSyncEnabled(AIndex: Integer; const AValue: boolean); 


begin
  If (FonPremisesSyncEnabled=AValue) then exit;
  FonPremisesSyncEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.SetpostalCode(AIndex: Integer; const AValue: string); 


begin
  If (FpostalCode=AValue) then exit;
  FpostalCode:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.SetpreferredLanguage(AIndex: Integer; const AValue: string); 


begin
  If (FpreferredLanguage=AValue) then exit;
  FpreferredLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.SetprovisionedPlans(AIndex: Integer; const AValue: TprovisionedPlanArray); 


begin
  If (FprovisionedPlans=AValue) then exit;
  FprovisionedPlans:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.SetsecurityComplianceNotificationMails(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FsecurityComplianceNotificationMails=AValue) then exit;
  FsecurityComplianceNotificationMails:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.SetsecurityComplianceNotificationPhones(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FsecurityComplianceNotificationPhones=AValue) then exit;
  FsecurityComplianceNotificationPhones:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.Setstate(AIndex: Integer; const AValue: string); 


begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.Setstreet(AIndex: Integer; const AValue: string); 


begin
  If (Fstreet=AValue) then exit;
  Fstreet:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.SettechnicalNotificationMails(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FtechnicalNotificationMails=AValue) then exit;
  FtechnicalNotificationMails:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Torganization.SetverifiedDomains(AIndex: Integer; const AValue: TverifiedDomainArray); 


begin
  If (FverifiedDomains=AValue) then exit;
  FverifiedDomains:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure Torganization.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'assignedplans' : SetLength(FassignedPlans,aLength);
  'businessphones' : SetLength(FbusinessPhones,aLength);
  'marketingnotificationemails' : SetLength(FmarketingNotificationEmails,aLength);
  'provisionedplans' : SetLength(FprovisionedPlans,aLength);
  'securitycompliancenotificationmails' : SetLength(FsecurityComplianceNotificationMails,aLength);
  'securitycompliancenotificationphones' : SetLength(FsecurityComplianceNotificationPhones,aLength);
  'technicalnotificationmails' : SetLength(FtechnicalNotificationMails,aLength);
  'verifieddomains' : SetLength(FverifiedDomains,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


{ --------------------------------------------------------------------
  Tuser
  --------------------------------------------------------------------}


Function Tuser.reminderView(AService: TODataService; StartDateTime: string; EndDateTime: string) : TreminderArray; 

Var
  _Res : String;
  _Path : String;
begin
  _Path:='StartDateTime='+TODataObject.MakeKeyString(StartDateTime);
  _Path:=_Path+','+'EndDateTime='+TODataObject.MakeKeyString(EndDateTime);
  _Path:='('+_Path+')';
  _Path:='microsoft.graph.reminderView'+_Path;
  _Path:=BaseURL(AService)+'/'+_Path;
  Result:=TreminderArray(AService.GetMulti(_Path,'',Treminder,True,_Res));
end;


Function Tuser.assignLicense(AService: TODataService; addLicenses: TassignedLicenseArray; removeLicenses: TGuidStringArray) : Tuser; 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('addLicenses',DynArrayToJSONArray(Pointer(addLicenses),'',TassignedLicense));
    _JSON.Add('removeLicenses',DynArrayToJSONArray(Pointer(removeLicenses),'GuidString',Nil));
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.assignLicense';
  Result:=Tuser(AService.SingleServiceCall('POST',_Path,'',_data,Tuser));
end;


Procedure Tuser.changePassword(AService: TODataService; currentPassword: string; newPassword: string); 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('currentPassword',currentPassword);
    _JSON.Add('newPassword',newPassword);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.changePassword';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Procedure Tuser.sendMail(AService: TODataService; Message: Tmessage; SaveToSentItems: boolean); 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('Message',Message.SaveToJSON);
    _JSON.Add('SaveToSentItems',SaveToSentItems);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.sendMail';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Class Function Tuser.ObjectRestKind : String; 

begin
  Result:='user';
end;


Procedure Tuser.SetaccountEnabled(AIndex: Integer; const AValue: boolean); 


begin
  If (FaccountEnabled=AValue) then exit;
  FaccountEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetassignedLicenses(AIndex: Integer; const AValue: TassignedLicenseArray); 


begin
  If (FassignedLicenses=AValue) then exit;
  FassignedLicenses:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetassignedPlans(AIndex: Integer; const AValue: TassignedPlanArray); 


begin
  If (FassignedPlans=AValue) then exit;
  FassignedPlans:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetbusinessPhones(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FbusinessPhones=AValue) then exit;
  FbusinessPhones:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.Setcity(AIndex: Integer; const AValue: string); 


begin
  If (Fcity=AValue) then exit;
  Fcity:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetcompanyName(AIndex: Integer; const AValue: string); 


begin
  If (FcompanyName=AValue) then exit;
  FcompanyName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.Setcountry(AIndex: Integer; const AValue: string); 


begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.Setdepartment(AIndex: Integer; const AValue: string); 


begin
  If (Fdepartment=AValue) then exit;
  Fdepartment:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetdisplayName(AIndex: Integer; const AValue: string); 


begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetgivenName(AIndex: Integer; const AValue: string); 


begin
  If (FgivenName=AValue) then exit;
  FgivenName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetjobTitle(AIndex: Integer; const AValue: string); 


begin
  If (FjobTitle=AValue) then exit;
  FjobTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.Setmail(AIndex: Integer; const AValue: string); 


begin
  If (Fmail=AValue) then exit;
  Fmail:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetmailNickname(AIndex: Integer; const AValue: string); 


begin
  If (FmailNickname=AValue) then exit;
  FmailNickname:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetmobilePhone(AIndex: Integer; const AValue: string); 


begin
  If (FmobilePhone=AValue) then exit;
  FmobilePhone:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetonPremisesImmutableId(AIndex: Integer; const AValue: string); 


begin
  If (FonPremisesImmutableId=AValue) then exit;
  FonPremisesImmutableId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetonPremisesLastSyncDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FonPremisesLastSyncDateTime=AValue) then exit;
  FonPremisesLastSyncDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetonPremisesSecurityIdentifier(AIndex: Integer; const AValue: string); 


begin
  If (FonPremisesSecurityIdentifier=AValue) then exit;
  FonPremisesSecurityIdentifier:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetonPremisesSyncEnabled(AIndex: Integer; const AValue: boolean); 


begin
  If (FonPremisesSyncEnabled=AValue) then exit;
  FonPremisesSyncEnabled:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetpasswordPolicies(AIndex: Integer; const AValue: string); 


begin
  If (FpasswordPolicies=AValue) then exit;
  FpasswordPolicies:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetpasswordProfile(AIndex: Integer; const AValue: TpasswordProfile); 


begin
  If (FpasswordProfile=AValue) then exit;
  FpasswordProfile:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetofficeLocation(AIndex: Integer; const AValue: string); 


begin
  If (FofficeLocation=AValue) then exit;
  FofficeLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetpostalCode(AIndex: Integer; const AValue: string); 


begin
  If (FpostalCode=AValue) then exit;
  FpostalCode:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetpreferredLanguage(AIndex: Integer; const AValue: string); 


begin
  If (FpreferredLanguage=AValue) then exit;
  FpreferredLanguage:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetprovisionedPlans(AIndex: Integer; const AValue: TprovisionedPlanArray); 


begin
  If (FprovisionedPlans=AValue) then exit;
  FprovisionedPlans:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetproxyAddresses(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FproxyAddresses=AValue) then exit;
  FproxyAddresses:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.Setstate(AIndex: Integer; const AValue: string); 


begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetstreetAddress(AIndex: Integer; const AValue: string); 


begin
  If (FstreetAddress=AValue) then exit;
  FstreetAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.Setsurname(AIndex: Integer; const AValue: string); 


begin
  If (Fsurname=AValue) then exit;
  Fsurname:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetusageLocation(AIndex: Integer; const AValue: string); 


begin
  If (FusageLocation=AValue) then exit;
  FusageLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetuserPrincipalName(AIndex: Integer; const AValue: string); 


begin
  If (FuserPrincipalName=AValue) then exit;
  FuserPrincipalName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetuserType(AIndex: Integer; const AValue: string); 


begin
  If (FuserType=AValue) then exit;
  FuserType:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetaboutMe(AIndex: Integer; const AValue: string); 


begin
  If (FaboutMe=AValue) then exit;
  FaboutMe:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.Setbirthday(AIndex: Integer; const AValue: TDateTime); 


begin
  If (Fbirthday=AValue) then exit;
  Fbirthday:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SethireDate(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FhireDate=AValue) then exit;
  FhireDate:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.Setinterests(AIndex: Integer; const AValue: TStringArray); 


begin
  If (Finterests=AValue) then exit;
  Finterests:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetmySite(AIndex: Integer; const AValue: string); 


begin
  If (FmySite=AValue) then exit;
  FmySite:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetpastProjects(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FpastProjects=AValue) then exit;
  FpastProjects:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.SetpreferredName(AIndex: Integer; const AValue: string); 


begin
  If (FpreferredName=AValue) then exit;
  FpreferredName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.Setresponsibilities(AIndex: Integer; const AValue: TStringArray); 


begin
  If (Fresponsibilities=AValue) then exit;
  Fresponsibilities:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.Setschools(AIndex: Integer; const AValue: TStringArray); 


begin
  If (Fschools=AValue) then exit;
  Fschools:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tuser.Setskills(AIndex: Integer; const AValue: TStringArray); 


begin
  If (Fskills=AValue) then exit;
  Fskills:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure Tuser.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'assignedlicenses' : SetLength(FassignedLicenses,aLength);
  'assignedplans' : SetLength(FassignedPlans,aLength);
  'businessphones' : SetLength(FbusinessPhones,aLength);
  'provisionedplans' : SetLength(FprovisionedPlans,aLength);
  'proxyaddresses' : SetLength(FproxyAddresses,aLength);
  'interests' : SetLength(Finterests,aLength);
  'pastprojects' : SetLength(FpastProjects,aLength);
  'responsibilities' : SetLength(Fresponsibilities,aLength);
  'schools' : SetLength(Fschools,aLength);
  'skills' : SetLength(Fskills,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


Function Tuser.ownedDevices(AService: TODataService) : TdirectoryObjectsEntitySet; 


begin
  Result:=TdirectoryObjectsEntitySet(CreateContainedEntitySet(AService,'ownedDevices', TdirectoryObjectsEntitySet));
end;


Function Tuser.registeredDevices(AService: TODataService) : TdirectoryObjectsEntitySet; 


begin
  Result:=TdirectoryObjectsEntitySet(CreateContainedEntitySet(AService,'registeredDevices', TdirectoryObjectsEntitySet));
end;


Function Tuser.manager(AService: TODataService) : TdirectoryObject; 


begin
  Result:=TdirectoryObject(GetContainedSingleTon(AService,'manager', TdirectoryObject));
end;


Function Tuser.directReports(AService: TODataService) : TdirectoryObjectsEntitySet; 


begin
  Result:=TdirectoryObjectsEntitySet(CreateContainedEntitySet(AService,'directReports', TdirectoryObjectsEntitySet));
end;


Function Tuser.memberOf(AService: TODataService) : TdirectoryObjectsEntitySet; 


begin
  Result:=TdirectoryObjectsEntitySet(CreateContainedEntitySet(AService,'memberOf', TdirectoryObjectsEntitySet));
end;


Function Tuser.createdObjects(AService: TODataService) : TdirectoryObjectsEntitySet; 


begin
  Result:=TdirectoryObjectsEntitySet(CreateContainedEntitySet(AService,'createdObjects', TdirectoryObjectsEntitySet));
end;


Function Tuser.ownedObjects(AService: TODataService) : TdirectoryObjectsEntitySet; 


begin
  Result:=TdirectoryObjectsEntitySet(CreateContainedEntitySet(AService,'ownedObjects', TdirectoryObjectsEntitySet));
end;


Function Tuser.messages(AService: TODataService) : TmessageImplicitEntitySet; 


begin
  Result:=TmessageImplicitEntitySet(CreateContainedEntitySet(AService,'messages', TmessageImplicitEntitySet));
end;


Function Tuser.mailFolders(AService: TODataService) : TmailFolderImplicitEntitySet; 


begin
  Result:=TmailFolderImplicitEntitySet(CreateContainedEntitySet(AService,'mailFolders', TmailFolderImplicitEntitySet));
end;


Function Tuser.calendar(AService: TODataService) : Tcalendar; 


begin
  Result:=Tcalendar(GetContainedSingleTon(AService,'calendar', Tcalendar));
end;


Function Tuser.calendars(AService: TODataService) : TcalendarImplicitEntitySet; 


begin
  Result:=TcalendarImplicitEntitySet(CreateContainedEntitySet(AService,'calendars', TcalendarImplicitEntitySet));
end;


Function Tuser.calendarGroups(AService: TODataService) : TcalendarGroupImplicitEntitySet; 


begin
  Result:=TcalendarGroupImplicitEntitySet(CreateContainedEntitySet(AService,'calendarGroups', TcalendarGroupImplicitEntitySet));
end;


Function Tuser.calendarView(AService: TODataService) : TeventImplicitEntitySet; 


begin
  Result:=TeventImplicitEntitySet(CreateContainedEntitySet(AService,'calendarView', TeventImplicitEntitySet));
end;


Function Tuser.events(AService: TODataService) : TeventImplicitEntitySet; 


begin
  Result:=TeventImplicitEntitySet(CreateContainedEntitySet(AService,'events', TeventImplicitEntitySet));
end;


Function Tuser.contacts(AService: TODataService) : TcontactImplicitEntitySet; 


begin
  Result:=TcontactImplicitEntitySet(CreateContainedEntitySet(AService,'contacts', TcontactImplicitEntitySet));
end;


Function Tuser.contactFolders(AService: TODataService) : TcontactFolderImplicitEntitySet; 


begin
  Result:=TcontactFolderImplicitEntitySet(CreateContainedEntitySet(AService,'contactFolders', TcontactFolderImplicitEntitySet));
end;


Function Tuser.photo(AService: TODataService) : TprofilePhoto; 


begin
  Result:=TprofilePhoto(GetContainedSingleTon(AService,'photo', TprofilePhoto));
end;


Function Tuser.drive(AService: TODataService) : Tdrive; 


begin
  Result:=Tdrive(GetContainedSingleTon(AService,'drive', Tdrive));
end;


{ --------------------------------------------------------------------
  Tmessage
  --------------------------------------------------------------------}


Function Tmessage.copy(AService: TODataService; DestinationId: string) : Tmessage; 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('DestinationId',DestinationId);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.copy';
  Result:=Tmessage(AService.SingleServiceCall('POST',_Path,'',_data,Tmessage));
end;


Function Tmessage.move(AService: TODataService; DestinationId: string) : Tmessage; 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('DestinationId',DestinationId);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.move';
  Result:=Tmessage(AService.SingleServiceCall('POST',_Path,'',_data,Tmessage));
end;


Function Tmessage.createReply(AService: TODataService) : Tmessage; 

Var
  _data : String;
  _Path : String;
begin
  _data:='';
  _Path:=BaseURL(AService)+'/microsoft.graph.createReply';
  Result:=Tmessage(AService.SingleServiceCall('POST',_Path,'',_data,Tmessage));
end;


Function Tmessage.createReplyAll(AService: TODataService) : Tmessage; 

Var
  _data : String;
  _Path : String;
begin
  _data:='';
  _Path:=BaseURL(AService)+'/microsoft.graph.createReplyAll';
  Result:=Tmessage(AService.SingleServiceCall('POST',_Path,'',_data,Tmessage));
end;


Function Tmessage.createForward(AService: TODataService) : Tmessage; 

Var
  _data : String;
  _Path : String;
begin
  _data:='';
  _Path:=BaseURL(AService)+'/microsoft.graph.createForward';
  Result:=Tmessage(AService.SingleServiceCall('POST',_Path,'',_data,Tmessage));
end;


Procedure Tmessage.reply(AService: TODataService; Comment: string); 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('Comment',Comment);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.reply';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Procedure Tmessage.replyAll(AService: TODataService; Comment: string); 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('Comment',Comment);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.replyAll';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Procedure Tmessage.forward(AService: TODataService; Comment: string; ToRecipients: TrecipientArray); 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('Comment',Comment);
    _JSON.Add('ToRecipients',DynArrayToJSONArray(Pointer(ToRecipients),'',Trecipient));
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.forward';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Procedure Tmessage.send(AService: TODataService); 

Var
  _data : String;
  _Path : String;
begin
  _data:='';
  _Path:=BaseURL(AService)+'/microsoft.graph.send';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Class Function Tmessage.ObjectRestKind : String; 

begin
  Result:='message';
end;


Procedure Tmessage.SetreceivedDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FreceivedDateTime=AValue) then exit;
  FreceivedDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.SetsentDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FsentDateTime=AValue) then exit;
  FsentDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.SethasAttachments(AIndex: Integer; const AValue: boolean); 


begin
  If (FhasAttachments=AValue) then exit;
  FhasAttachments:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.SetinternetMessageId(AIndex: Integer; const AValue: string); 


begin
  If (FinternetMessageId=AValue) then exit;
  FinternetMessageId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.Setsubject(AIndex: Integer; const AValue: string); 


begin
  If (Fsubject=AValue) then exit;
  Fsubject:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.Setbody(AIndex: Integer; const AValue: TitemBody); 


begin
  If (Fbody=AValue) then exit;
  Fbody:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.SetbodyPreview(AIndex: Integer; const AValue: string); 


begin
  If (FbodyPreview=AValue) then exit;
  FbodyPreview:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.Setimportance(AIndex: Integer; const AValue: Timportance); 


begin
  If (Fimportance=AValue) then exit;
  Fimportance:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.SetparentFolderId(AIndex: Integer; const AValue: string); 


begin
  If (FparentFolderId=AValue) then exit;
  FparentFolderId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.Setsender(AIndex: Integer; const AValue: Trecipient); 


begin
  If (Fsender=AValue) then exit;
  Fsender:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.Setfrom(AIndex: Integer; const AValue: Trecipient); 


begin
  If (Ffrom=AValue) then exit;
  Ffrom:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.SettoRecipients(AIndex: Integer; const AValue: TrecipientArray); 


begin
  If (FtoRecipients=AValue) then exit;
  FtoRecipients:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.SetccRecipients(AIndex: Integer; const AValue: TrecipientArray); 


begin
  If (FccRecipients=AValue) then exit;
  FccRecipients:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.SetbccRecipients(AIndex: Integer; const AValue: TrecipientArray); 


begin
  If (FbccRecipients=AValue) then exit;
  FbccRecipients:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.SetreplyTo(AIndex: Integer; const AValue: TrecipientArray); 


begin
  If (FreplyTo=AValue) then exit;
  FreplyTo:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.SetconversationId(AIndex: Integer; const AValue: string); 


begin
  If (FconversationId=AValue) then exit;
  FconversationId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.SetuniqueBody(AIndex: Integer; const AValue: TitemBody); 


begin
  If (FuniqueBody=AValue) then exit;
  FuniqueBody:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.SetisDeliveryReceiptRequested(AIndex: Integer; const AValue: boolean); 


begin
  If (FisDeliveryReceiptRequested=AValue) then exit;
  FisDeliveryReceiptRequested:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.SetisReadReceiptRequested(AIndex: Integer; const AValue: boolean); 


begin
  If (FisReadReceiptRequested=AValue) then exit;
  FisReadReceiptRequested:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.SetisRead(AIndex: Integer; const AValue: boolean); 


begin
  If (FisRead=AValue) then exit;
  FisRead:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.SetisDraft(AIndex: Integer; const AValue: boolean); 


begin
  If (FisDraft=AValue) then exit;
  FisDraft:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tmessage.SetwebLink(AIndex: Integer; const AValue: string); 


begin
  If (FwebLink=AValue) then exit;
  FwebLink:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure Tmessage.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'torecipients' : SetLength(FtoRecipients,aLength);
  'ccrecipients' : SetLength(FccRecipients,aLength);
  'bccrecipients' : SetLength(FbccRecipients,aLength);
  'replyto' : SetLength(FreplyTo,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


Function Tmessage.attachments(AService: TODataService) : TattachmentImplicitEntitySet; 


begin
  Result:=TattachmentImplicitEntitySet(CreateContainedEntitySet(AService,'attachments', TattachmentImplicitEntitySet));
end;


{ --------------------------------------------------------------------
  TmailFolder
  --------------------------------------------------------------------}


Function TmailFolder.copy(AService: TODataService; DestinationId: string) : TmailFolder; 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('DestinationId',DestinationId);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.copy';
  Result:=TmailFolder(AService.SingleServiceCall('POST',_Path,'',_data,TmailFolder));
end;


Function TmailFolder.move(AService: TODataService; DestinationId: string) : TmailFolder; 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('DestinationId',DestinationId);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.move';
  Result:=TmailFolder(AService.SingleServiceCall('POST',_Path,'',_data,TmailFolder));
end;


Class Function TmailFolder.ObjectRestKind : String; 

begin
  Result:='mailFolder';
end;


Procedure TmailFolder.SetdisplayName(AIndex: Integer; const AValue: string); 


begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TmailFolder.SetparentFolderId(AIndex: Integer; const AValue: string); 


begin
  If (FparentFolderId=AValue) then exit;
  FparentFolderId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TmailFolder.SetchildFolderCount(AIndex: Integer; const AValue: TInt32); 


begin
  If (FchildFolderCount=AValue) then exit;
  FchildFolderCount:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TmailFolder.SetunreadItemCount(AIndex: Integer; const AValue: TInt32); 


begin
  If (FunreadItemCount=AValue) then exit;
  FunreadItemCount:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TmailFolder.SettotalItemCount(AIndex: Integer; const AValue: TInt32); 


begin
  If (FtotalItemCount=AValue) then exit;
  FtotalItemCount:=AValue;
  MarkPropertyChanged(AIndex);
end;


Function TmailFolder.messages(AService: TODataService) : TmessageImplicitEntitySet; 


begin
  Result:=TmessageImplicitEntitySet(CreateContainedEntitySet(AService,'messages', TmessageImplicitEntitySet));
end;


Function TmailFolder.childFolders(AService: TODataService) : TmailFolderImplicitEntitySet; 


begin
  Result:=TmailFolderImplicitEntitySet(CreateContainedEntitySet(AService,'childFolders', TmailFolderImplicitEntitySet));
end;


{ --------------------------------------------------------------------
  TcalendarGroup
  --------------------------------------------------------------------}


Class Function TcalendarGroup.ObjectRestKind : String; 

begin
  Result:='calendarGroup';
end;


Procedure TcalendarGroup.Setname(AIndex: Integer; const AValue: string); 


begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TcalendarGroup.SetclassId(AIndex: Integer; const AValue: TGUIDString); 


begin
  If (FclassId=AValue) then exit;
  FclassId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TcalendarGroup.SetchangeKey(AIndex: Integer; const AValue: string); 


begin
  If (FchangeKey=AValue) then exit;
  FchangeKey:=AValue;
  MarkPropertyChanged(AIndex);
end;


Function TcalendarGroup.calendars(AService: TODataService) : TcalendarImplicitEntitySet; 


begin
  Result:=TcalendarImplicitEntitySet(CreateContainedEntitySet(AService,'calendars', TcalendarImplicitEntitySet));
end;


{ --------------------------------------------------------------------
  Tcontact
  --------------------------------------------------------------------}


Class Function Tcontact.ObjectRestKind : String; 

begin
  Result:='contact';
end;


Procedure Tcontact.SetparentFolderId(AIndex: Integer; const AValue: string); 


begin
  If (FparentFolderId=AValue) then exit;
  FparentFolderId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.Setbirthday(AIndex: Integer; const AValue: TDateTime); 


begin
  If (Fbirthday=AValue) then exit;
  Fbirthday:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetfileAs(AIndex: Integer; const AValue: string); 


begin
  If (FfileAs=AValue) then exit;
  FfileAs:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetdisplayName(AIndex: Integer; const AValue: string); 


begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetgivenName(AIndex: Integer; const AValue: string); 


begin
  If (FgivenName=AValue) then exit;
  FgivenName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.Setinitials(AIndex: Integer; const AValue: string); 


begin
  If (Finitials=AValue) then exit;
  Finitials:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetmiddleName(AIndex: Integer; const AValue: string); 


begin
  If (FmiddleName=AValue) then exit;
  FmiddleName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetnickName(AIndex: Integer; const AValue: string); 


begin
  If (FnickName=AValue) then exit;
  FnickName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.Setsurname(AIndex: Integer; const AValue: string); 


begin
  If (Fsurname=AValue) then exit;
  Fsurname:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.Settitle(AIndex: Integer; const AValue: string); 


begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetyomiGivenName(AIndex: Integer; const AValue: string); 


begin
  If (FyomiGivenName=AValue) then exit;
  FyomiGivenName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetyomiSurname(AIndex: Integer; const AValue: string); 


begin
  If (FyomiSurname=AValue) then exit;
  FyomiSurname:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetyomiCompanyName(AIndex: Integer; const AValue: string); 


begin
  If (FyomiCompanyName=AValue) then exit;
  FyomiCompanyName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.Setgeneration(AIndex: Integer; const AValue: string); 


begin
  If (Fgeneration=AValue) then exit;
  Fgeneration:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetemailAddresses(AIndex: Integer; const AValue: TemailAddressArray); 


begin
  If (FemailAddresses=AValue) then exit;
  FemailAddresses:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetimAddresses(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FimAddresses=AValue) then exit;
  FimAddresses:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetjobTitle(AIndex: Integer; const AValue: string); 


begin
  If (FjobTitle=AValue) then exit;
  FjobTitle:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetcompanyName(AIndex: Integer; const AValue: string); 


begin
  If (FcompanyName=AValue) then exit;
  FcompanyName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.Setdepartment(AIndex: Integer; const AValue: string); 


begin
  If (Fdepartment=AValue) then exit;
  Fdepartment:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetofficeLocation(AIndex: Integer; const AValue: string); 


begin
  If (FofficeLocation=AValue) then exit;
  FofficeLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.Setprofession(AIndex: Integer; const AValue: string); 


begin
  If (Fprofession=AValue) then exit;
  Fprofession:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetbusinessHomePage(AIndex: Integer; const AValue: string); 


begin
  If (FbusinessHomePage=AValue) then exit;
  FbusinessHomePage:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetassistantName(AIndex: Integer; const AValue: string); 


begin
  If (FassistantName=AValue) then exit;
  FassistantName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.Setmanager(AIndex: Integer; const AValue: string); 


begin
  If (Fmanager=AValue) then exit;
  Fmanager:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SethomePhones(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FhomePhones=AValue) then exit;
  FhomePhones:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetmobilePhone(AIndex: Integer; const AValue: string); 


begin
  If (FmobilePhone=AValue) then exit;
  FmobilePhone:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetbusinessPhones(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FbusinessPhones=AValue) then exit;
  FbusinessPhones:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SethomeAddress(AIndex: Integer; const AValue: TphysicalAddress); 


begin
  If (FhomeAddress=AValue) then exit;
  FhomeAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetbusinessAddress(AIndex: Integer; const AValue: TphysicalAddress); 


begin
  If (FbusinessAddress=AValue) then exit;
  FbusinessAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetotherAddress(AIndex: Integer; const AValue: TphysicalAddress); 


begin
  If (FotherAddress=AValue) then exit;
  FotherAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetspouseName(AIndex: Integer; const AValue: string); 


begin
  If (FspouseName=AValue) then exit;
  FspouseName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.SetpersonalNotes(AIndex: Integer; const AValue: string); 


begin
  If (FpersonalNotes=AValue) then exit;
  FpersonalNotes:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tcontact.Setchildren(AIndex: Integer; const AValue: TStringArray); 


begin
  If (Fchildren=AValue) then exit;
  Fchildren:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure Tcontact.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'emailaddresses' : SetLength(FemailAddresses,aLength);
  'imaddresses' : SetLength(FimAddresses,aLength);
  'homephones' : SetLength(FhomePhones,aLength);
  'businessphones' : SetLength(FbusinessPhones,aLength);
  'children' : SetLength(Fchildren,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


Function Tcontact.photo(AService: TODataService) : TprofilePhoto; 


begin
  Result:=TprofilePhoto(GetContainedSingleTon(AService,'photo', TprofilePhoto));
end;


{ --------------------------------------------------------------------
  TcontactFolder
  --------------------------------------------------------------------}


Class Function TcontactFolder.ObjectRestKind : String; 

begin
  Result:='contactFolder';
end;


Procedure TcontactFolder.SetparentFolderId(AIndex: Integer; const AValue: string); 


begin
  If (FparentFolderId=AValue) then exit;
  FparentFolderId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TcontactFolder.SetdisplayName(AIndex: Integer; const AValue: string); 


begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Function TcontactFolder.contacts(AService: TODataService) : TcontactImplicitEntitySet; 


begin
  Result:=TcontactImplicitEntitySet(CreateContainedEntitySet(AService,'contacts', TcontactImplicitEntitySet));
end;


Function TcontactFolder.childFolders(AService: TODataService) : TcontactFolderImplicitEntitySet; 


begin
  Result:=TcontactFolderImplicitEntitySet(CreateContainedEntitySet(AService,'childFolders', TcontactFolderImplicitEntitySet));
end;


{ --------------------------------------------------------------------
  Tattachment
  --------------------------------------------------------------------}


Class Function Tattachment.ObjectRestKind : String; 

begin
  Result:='attachment';
end;


Procedure Tattachment.SetlastModifiedDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FlastModifiedDateTime=AValue) then exit;
  FlastModifiedDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tattachment.Setname(AIndex: Integer; const AValue: string); 


begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tattachment.SetcontentType(AIndex: Integer; const AValue: string); 


begin
  If (FcontentType=AValue) then exit;
  FcontentType:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tattachment.Setsize(AIndex: Integer; const AValue: TInt32); 


begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tattachment.SetisInline(AIndex: Integer; const AValue: boolean); 


begin
  If (FisInline=AValue) then exit;
  FisInline:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TfileAttachment
  --------------------------------------------------------------------}


Class Function TfileAttachment.ObjectRestKind : String; 

begin
  Result:='fileAttachment';
end;


Procedure TfileAttachment.SetcontentId(AIndex: Integer; const AValue: string); 


begin
  If (FcontentId=AValue) then exit;
  FcontentId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TfileAttachment.SetcontentLocation(AIndex: Integer; const AValue: string); 


begin
  If (FcontentLocation=AValue) then exit;
  FcontentLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TfileAttachment.SetcontentBytes(AIndex: Integer; const AValue: TBinary); 


begin
  If (FcontentBytes=AValue) then exit;
  FcontentBytes:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TitemAttachment
  --------------------------------------------------------------------}


Class Function TitemAttachment.ObjectRestKind : String; 

begin
  Result:='itemAttachment';
end;


Function TitemAttachment.item(AService: TODataService) : ToutlookItem; 


begin
  Result:=ToutlookItem(GetContainedSingleTon(AService,'item', ToutlookItem));
end;


{ --------------------------------------------------------------------
  TeventMessage
  --------------------------------------------------------------------}


Class Function TeventMessage.ObjectRestKind : String; 

begin
  Result:='eventMessage';
end;


Procedure TeventMessage.SetmeetingMessageType(AIndex: Integer; const AValue: TmeetingMessageType); 


begin
  If (FmeetingMessageType=AValue) then exit;
  FmeetingMessageType:=AValue;
  MarkPropertyChanged(AIndex);
end;


Function TeventMessage.event(AService: TODataService) : Tevent; 


begin
  Result:=Tevent(GetContainedSingleTon(AService,'event', Tevent));
end;


{ --------------------------------------------------------------------
  TreferenceAttachment
  --------------------------------------------------------------------}


Class Function TreferenceAttachment.ObjectRestKind : String; 

begin
  Result:='referenceAttachment';
end;


{ --------------------------------------------------------------------
  Tpost
  --------------------------------------------------------------------}


Procedure Tpost.forward(AService: TODataService; Comment: string; ToRecipients: TrecipientArray); 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('Comment',Comment);
    _JSON.Add('ToRecipients',DynArrayToJSONArray(Pointer(ToRecipients),'',Trecipient));
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.forward';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Procedure Tpost.reply(AService: TODataService; _Post: Tpost); 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('Post',_Post.SaveToJSON);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.reply';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Class Function Tpost.ObjectRestKind : String; 

begin
  Result:='post';
end;


Procedure Tpost.Setbody(AIndex: Integer; const AValue: TitemBody); 


begin
  If (Fbody=AValue) then exit;
  Fbody:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tpost.SetreceivedDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FreceivedDateTime=AValue) then exit;
  FreceivedDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tpost.SethasAttachments(AIndex: Integer; const AValue: boolean); 


begin
  If (FhasAttachments=AValue) then exit;
  FhasAttachments:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tpost.Setfrom(AIndex: Integer; const AValue: Trecipient); 


begin
  If (Ffrom=AValue) then exit;
  Ffrom:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tpost.Setsender(AIndex: Integer; const AValue: Trecipient); 


begin
  If (Fsender=AValue) then exit;
  Fsender:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tpost.SetconversationThreadId(AIndex: Integer; const AValue: string); 


begin
  If (FconversationThreadId=AValue) then exit;
  FconversationThreadId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tpost.SetnewParticipants(AIndex: Integer; const AValue: TrecipientArray); 


begin
  If (FnewParticipants=AValue) then exit;
  FnewParticipants:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tpost.SetconversationId(AIndex: Integer; const AValue: string); 


begin
  If (FconversationId=AValue) then exit;
  FconversationId:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure Tpost.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'newparticipants' : SetLength(FnewParticipants,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


Function Tpost.inReplyTo(AService: TODataService) : Tpost; 


begin
  Result:=Tpost(GetContainedSingleTon(AService,'inReplyTo', Tpost));
end;


Function Tpost.attachments(AService: TODataService) : TattachmentImplicitEntitySet; 


begin
  Result:=TattachmentImplicitEntitySet(CreateContainedEntitySet(AService,'attachments', TattachmentImplicitEntitySet));
end;


{ --------------------------------------------------------------------
  TdriveItem
  --------------------------------------------------------------------}


Function TdriveItem.search(AService: TODataService; q: string) : TdriveItemArray; 

Var
  _Res : String;
  _Path : String;
begin
  _Path:='q='+TODataObject.MakeKeyString(q);
  _Path:='('+_Path+')';
  _Path:='microsoft.graph.search'+_Path;
  _Path:=BaseURL(AService)+'/'+_Path;
  Result:=TdriveItemArray(AService.GetMulti(_Path,'',TdriveItem,True,_Res));
end;


Function TdriveItem.delta(AService: TODataService; token: string) : TdriveItemArray; 

Var
  _Res : String;
  _Path : String;
begin
  _Path:='token='+TODataObject.MakeKeyString(token);
  _Path:='('+_Path+')';
  _Path:='microsoft.graph.delta'+_Path;
  _Path:=BaseURL(AService)+'/'+_Path;
  Result:=TdriveItemArray(AService.GetMulti(_Path,'',TdriveItem,True,_Res));
end;


Function TdriveItem.delta(AService: TODataService) : TdriveItemArray; 

Var
  _Res : String;
  _Path : String;
begin
  _Path:='('+_Path+')';
  _Path:='microsoft.graph.delta'+_Path;
  _Path:=BaseURL(AService)+'/'+_Path;
  Result:=TdriveItemArray(AService.GetMulti(_Path,'',TdriveItem,True,_Res));
end;


Function TdriveItem.createLink(AService: TODataService; _type: string; scope: string) : Tpermission; 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('type',_type);
    _JSON.Add('scope',scope);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.createLink';
  Result:=Tpermission(AService.SingleServiceCall('POST',_Path,'',_data,Tpermission));
end;


Function TdriveItem.copy(AService: TODataService; name: string; parentReference: TitemReference) : TdriveItem; 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('name',name);
    _JSON.Add('parentReference',parentReference.SaveToJSON);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/microsoft.graph.copy';
  Result:=TdriveItem(AService.SingleServiceCall('POST',_Path,'',_data,TdriveItem));
end;


Class Function TdriveItem.ObjectRestKind : String; 

begin
  Result:='driveItem';
end;


Procedure TdriveItem.Setcontent(AIndex: Integer; const AValue: TStream); 


begin
  If (Fcontent=AValue) then exit;
  Fcontent:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.SetcreatedBy(AIndex: Integer; const AValue: TidentitySet); 


begin
  If (FcreatedBy=AValue) then exit;
  FcreatedBy:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.SetcreatedDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FcreatedDateTime=AValue) then exit;
  FcreatedDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.SetcTag(AIndex: Integer; const AValue: string); 


begin
  If (FcTag=AValue) then exit;
  FcTag:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.Setdescription(AIndex: Integer; const AValue: string); 


begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.SeteTag(AIndex: Integer; const AValue: string); 


begin
  If (FeTag=AValue) then exit;
  FeTag:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.SetlastModifiedBy(AIndex: Integer; const AValue: TidentitySet); 


begin
  If (FlastModifiedBy=AValue) then exit;
  FlastModifiedBy:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.SetlastModifiedDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FlastModifiedDateTime=AValue) then exit;
  FlastModifiedDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.Setname(AIndex: Integer; const AValue: string); 


begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.SetparentReference(AIndex: Integer; const AValue: TitemReference); 


begin
  If (FparentReference=AValue) then exit;
  FparentReference:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.Setsize(AIndex: Integer; const AValue: int64); 


begin
  If (Fsize=AValue) then exit;
  Fsize:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.SetwebDavUrl(AIndex: Integer; const AValue: string); 


begin
  If (FwebDavUrl=AValue) then exit;
  FwebDavUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.SetwebUrl(AIndex: Integer; const AValue: string); 


begin
  If (FwebUrl=AValue) then exit;
  FwebUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.Setaudio(AIndex: Integer; const AValue: Taudio); 


begin
  If (Faudio=AValue) then exit;
  Faudio:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.Setdeleted(AIndex: Integer; const AValue: Tdeleted); 


begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.Set_file(AIndex: Integer; const AValue: T_file); 


begin
  If (F_file=AValue) then exit;
  F_file:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.SetfileSystemInfo(AIndex: Integer; const AValue: TfileSystemInfo); 


begin
  If (FfileSystemInfo=AValue) then exit;
  FfileSystemInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.Setfolder(AIndex: Integer; const AValue: Tfolder); 


begin
  If (Ffolder=AValue) then exit;
  Ffolder:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.Setimage(AIndex: Integer; const AValue: Timage); 


begin
  If (Fimage=AValue) then exit;
  Fimage:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.Setlocation(AIndex: Integer; const AValue: TgeoCoordinates); 


begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.Setphoto(AIndex: Integer; const AValue: Tphoto); 


begin
  If (Fphoto=AValue) then exit;
  Fphoto:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.SetremoteItem(AIndex: Integer; const AValue: TremoteItem); 


begin
  If (FremoteItem=AValue) then exit;
  FremoteItem:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.SetsearchResult(AIndex: Integer; const AValue: TsearchResult); 


begin
  If (FsearchResult=AValue) then exit;
  FsearchResult:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.Setshared(AIndex: Integer; const AValue: Tshared); 


begin
  If (Fshared=AValue) then exit;
  Fshared:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.SetspecialFolder(AIndex: Integer; const AValue: TspecialFolder); 


begin
  If (FspecialFolder=AValue) then exit;
  FspecialFolder:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.Setvideo(AIndex: Integer; const AValue: Tvideo); 


begin
  If (Fvideo=AValue) then exit;
  Fvideo:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TdriveItem.Setpackage(AIndex: Integer; const AValue: Tpackage); 


begin
  If (Fpackage=AValue) then exit;
  Fpackage:=AValue;
  MarkPropertyChanged(AIndex);
end;


Class Function TdriveItem.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_file' : Result:='file';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;


Function TdriveItem.createdByUser(AService: TODataService) : Tuser; 


begin
  Result:=Tuser(GetContainedSingleTon(AService,'createdByUser', Tuser));
end;


Function TdriveItem.lastModifiedByUser(AService: TODataService) : Tuser; 


begin
  Result:=Tuser(GetContainedSingleTon(AService,'lastModifiedByUser', Tuser));
end;


Function TdriveItem.permissions(AService: TODataService) : TpermissionImplicitEntitySet; 


begin
  Result:=TpermissionImplicitEntitySet(CreateContainedEntitySet(AService,'permissions', TpermissionImplicitEntitySet));
end;


Function TdriveItem.children(AService: TODataService) : TdriveItemImplicitEntitySet; 


begin
  Result:=TdriveItemImplicitEntitySet(CreateContainedEntitySet(AService,'children', TdriveItemImplicitEntitySet));
end;


Function TdriveItem.thumbnails(AService: TODataService) : TthumbnailSetImplicitEntitySet; 


begin
  Result:=TthumbnailSetImplicitEntitySet(CreateContainedEntitySet(AService,'thumbnails', TthumbnailSetImplicitEntitySet));
end;


{ --------------------------------------------------------------------
  Tpermission
  --------------------------------------------------------------------}


Class Function Tpermission.ObjectRestKind : String; 

begin
  Result:='permission';
end;


Procedure Tpermission.SetgrantedTo(AIndex: Integer; const AValue: TidentitySet); 


begin
  If (FgrantedTo=AValue) then exit;
  FgrantedTo:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tpermission.Setinvitation(AIndex: Integer; const AValue: TsharingInvitation); 


begin
  If (Finvitation=AValue) then exit;
  Finvitation:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tpermission.SetinheritedFrom(AIndex: Integer; const AValue: TitemReference); 


begin
  If (FinheritedFrom=AValue) then exit;
  FinheritedFrom:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tpermission.Setlink(AIndex: Integer; const AValue: TsharingLink); 


begin
  If (Flink=AValue) then exit;
  Flink:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tpermission.Setroles(AIndex: Integer; const AValue: TStringArray); 


begin
  If (Froles=AValue) then exit;
  Froles:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tpermission.SetshareId(AIndex: Integer; const AValue: string); 


begin
  If (FshareId=AValue) then exit;
  FshareId:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure Tpermission.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'roles' : SetLength(Froles,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


{ --------------------------------------------------------------------
  TthumbnailSet
  --------------------------------------------------------------------}


Class Function TthumbnailSet.ObjectRestKind : String; 

begin
  Result:='thumbnailSet';
end;


Procedure TthumbnailSet.Setlarge(AIndex: Integer; const AValue: Tthumbnail); 


begin
  If (Flarge=AValue) then exit;
  Flarge:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TthumbnailSet.Setmedium(AIndex: Integer; const AValue: Tthumbnail); 


begin
  If (Fmedium=AValue) then exit;
  Fmedium:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TthumbnailSet.Setsmall(AIndex: Integer; const AValue: Tthumbnail); 


begin
  If (Fsmall=AValue) then exit;
  Fsmall:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TthumbnailSet.Setsource(AIndex: Integer; const AValue: Tthumbnail); 


begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  Tsubscription
  --------------------------------------------------------------------}


Class Function Tsubscription.ObjectRestKind : String; 

begin
  Result:='subscription';
end;


Procedure Tsubscription.Setresource(AIndex: Integer; const AValue: string); 


begin
  If (Fresource=AValue) then exit;
  Fresource:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tsubscription.SetchangeType(AIndex: Integer; const AValue: string); 


begin
  If (FchangeType=AValue) then exit;
  FchangeType:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tsubscription.SetclientState(AIndex: Integer; const AValue: string); 


begin
  If (FclientState=AValue) then exit;
  FclientState:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tsubscription.SetnotificationUrl(AIndex: Integer; const AValue: string); 


begin
  If (FnotificationUrl=AValue) then exit;
  FnotificationUrl:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure Tsubscription.SetexpirationDateTime(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FexpirationDateTime=AValue) then exit;
  FexpirationDateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TGraphService
  --------------------------------------------------------------------}


Class Function TGraphService.ObjectRestKind : String; 

begin
  Result:='GraphService';
end;

Function TGraphService.CreateNewdirectoryObjects : TdirectoryObjectsEntitySet; 

begin
  Result:=TdirectoryObjectsEntitySet(CreateEntitySet(TdirectoryObjectsEntitySet));
end;


Function TGraphService.GetdirectoryObjects : TdirectoryObjectsEntitySet; 


begin
  If Not Assigned(FdirectoryObjects) then
    FdirectoryObjects:=TdirectoryObjectsEntitySet(CreateEntitySet(TdirectoryObjectsEntitySet));
  Result:=FdirectoryObjects;
end;

Function TGraphService.CreateNewdevices : TdevicesEntitySet; 

begin
  Result:=TdevicesEntitySet(CreateEntitySet(TdevicesEntitySet));
end;


Function TGraphService.Getdevices : TdevicesEntitySet; 


begin
  If Not Assigned(Fdevices) then
    Fdevices:=TdevicesEntitySet(CreateEntitySet(TdevicesEntitySet));
  Result:=Fdevices;
end;

Function TGraphService.CreateNewgroups : TgroupsEntitySet; 

begin
  Result:=TgroupsEntitySet(CreateEntitySet(TgroupsEntitySet));
end;


Function TGraphService.Getgroups : TgroupsEntitySet; 


begin
  If Not Assigned(Fgroups) then
    Fgroups:=TgroupsEntitySet(CreateEntitySet(TgroupsEntitySet));
  Result:=Fgroups;
end;

Function TGraphService.CreateNewdirectoryRoles : TdirectoryRolesEntitySet; 

begin
  Result:=TdirectoryRolesEntitySet(CreateEntitySet(TdirectoryRolesEntitySet));
end;


Function TGraphService.GetdirectoryRoles : TdirectoryRolesEntitySet; 


begin
  If Not Assigned(FdirectoryRoles) then
    FdirectoryRoles:=TdirectoryRolesEntitySet(CreateEntitySet(TdirectoryRolesEntitySet));
  Result:=FdirectoryRoles;
end;

Function TGraphService.CreateNewdirectoryRoleTemplates : TdirectoryRoleTemplatesEntitySet; 

begin
  Result:=TdirectoryRoleTemplatesEntitySet(CreateEntitySet(TdirectoryRoleTemplatesEntitySet));
end;


Function TGraphService.GetdirectoryRoleTemplates : TdirectoryRoleTemplatesEntitySet; 


begin
  If Not Assigned(FdirectoryRoleTemplates) then
    FdirectoryRoleTemplates:=TdirectoryRoleTemplatesEntitySet(CreateEntitySet(TdirectoryRoleTemplatesEntitySet));
  Result:=FdirectoryRoleTemplates;
end;

Function TGraphService.CreateNeworganization : TorganizationEntitySet; 

begin
  Result:=TorganizationEntitySet(CreateEntitySet(TorganizationEntitySet));
end;


Function TGraphService.Getorganization : TorganizationEntitySet; 


begin
  If Not Assigned(Forganization) then
    Forganization:=TorganizationEntitySet(CreateEntitySet(TorganizationEntitySet));
  Result:=Forganization;
end;

Function TGraphService.CreateNewsubscribedSkus : TsubscribedSkusEntitySet; 

begin
  Result:=TsubscribedSkusEntitySet(CreateEntitySet(TsubscribedSkusEntitySet));
end;


Function TGraphService.GetsubscribedSkus : TsubscribedSkusEntitySet; 


begin
  If Not Assigned(FsubscribedSkus) then
    FsubscribedSkus:=TsubscribedSkusEntitySet(CreateEntitySet(TsubscribedSkusEntitySet));
  Result:=FsubscribedSkus;
end;

Function TGraphService.CreateNewusers : TusersEntitySet; 

begin
  Result:=TusersEntitySet(CreateEntitySet(TusersEntitySet));
end;


Function TGraphService.Getusers : TusersEntitySet; 


begin
  If Not Assigned(Fusers) then
    Fusers:=TusersEntitySet(CreateEntitySet(TusersEntitySet));
  Result:=Fusers;
end;

Function TGraphService.CreateNewdrives : TdrivesEntitySet; 

begin
  Result:=TdrivesEntitySet(CreateEntitySet(TdrivesEntitySet));
end;


Function TGraphService.Getdrives : TdrivesEntitySet; 


begin
  If Not Assigned(Fdrives) then
    Fdrives:=TdrivesEntitySet(CreateEntitySet(TdrivesEntitySet));
  Result:=Fdrives;
end;

Function TGraphService.CreateNewsubscriptions : TsubscriptionsEntitySet; 

begin
  Result:=TsubscriptionsEntitySet(CreateEntitySet(TsubscriptionsEntitySet));
end;


Function TGraphService.Getsubscriptions : TsubscriptionsEntitySet; 


begin
  If Not Assigned(Fsubscriptions) then
    Fsubscriptions:=TsubscriptionsEntitySet(CreateEntitySet(TsubscriptionsEntitySet));
  Result:=Fsubscriptions;
end;


Function TGraphService.Fetchme : Tuser; 

begin
  CheckService;
  Result:=Tuser(Service.SingleServiceCall('me','',Tuser));
  Result.BasePath:='me';
end;


Function TGraphService.Getme : Tuser; 


begin
  If Not Assigned(Fme) then
    Fme:=Fetchme;
  Result:=Fme;
end;


Function TGraphService.Fetchdrive : Tdrive; 

begin
  CheckService;
  Result:=Tdrive(Service.SingleServiceCall('drive','',Tdrive));
  Result.BasePath:='drive';
end;


Function TGraphService.Getdrive : Tdrive; 


begin
  If Not Assigned(Fdrive) then
    Fdrive:=Fetchdrive;
  Result:=Fdrive;
end;


{ --------------------------------------------------------------------
  TdirectoryObjectsEntitySet
  --------------------------------------------------------------------}


Class Function TdirectoryObjectsEntitySet.ObjectRestKind : String; 

begin
  Result:='directoryObjects';
end;

Class Function TdirectoryObjectsEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TdirectoryObject;
end;


Function TdirectoryObjectsEntitySet.Get(const id: string) : TdirectoryObject; 


begin
  Result:=TdirectoryObject(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TdirectoryObjectsEntitySet.List(const AQuery: String; out NextLink: String) : TdirectoryObjectArray; 


begin
  Result:=TdirectoryObjectArray(GetMulti(AQuery,False,NextLink));
end;


Function TdirectoryObjectsEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TdirectoryObjectArray; 


begin
  Result:=TdirectoryObjectArray(GetMulti(AQuery,False,NextLink));
end;


Function TdirectoryObjectsEntitySet.ListAll(const AQuery: String) : TdirectoryObjectArray; 

var N : String;

begin
  Result:=TdirectoryObjectArray(GetMulti(AQuery,True,N));
end;


Function TdirectoryObjectsEntitySet.ListAll(const AQuery: TQueryParams) : TdirectoryObjectArray; 

var N : String;

begin
  Result:=TdirectoryObjectArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TdevicesEntitySet
  --------------------------------------------------------------------}


Class Function TdevicesEntitySet.ObjectRestKind : String; 

begin
  Result:='devices';
end;

Class Function TdevicesEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=Tdevice;
end;


Function TdevicesEntitySet.Get(const id: string) : Tdevice; 


begin
  Result:=Tdevice(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TdevicesEntitySet.List(const AQuery: String; out NextLink: String) : TdeviceArray; 


begin
  Result:=TdeviceArray(GetMulti(AQuery,False,NextLink));
end;


Function TdevicesEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TdeviceArray; 


begin
  Result:=TdeviceArray(GetMulti(AQuery,False,NextLink));
end;


Function TdevicesEntitySet.ListAll(const AQuery: String) : TdeviceArray; 

var N : String;

begin
  Result:=TdeviceArray(GetMulti(AQuery,True,N));
end;


Function TdevicesEntitySet.ListAll(const AQuery: TQueryParams) : TdeviceArray; 

var N : String;

begin
  Result:=TdeviceArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TgroupsEntitySet
  --------------------------------------------------------------------}


Class Function TgroupsEntitySet.ObjectRestKind : String; 

begin
  Result:='groups';
end;

Class Function TgroupsEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=Tgroup;
end;


Function TgroupsEntitySet.Get(const id: string) : Tgroup; 


begin
  Result:=Tgroup(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TgroupsEntitySet.List(const AQuery: String; out NextLink: String) : TgroupArray; 


begin
  Result:=TgroupArray(GetMulti(AQuery,False,NextLink));
end;


Function TgroupsEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TgroupArray; 


begin
  Result:=TgroupArray(GetMulti(AQuery,False,NextLink));
end;


Function TgroupsEntitySet.ListAll(const AQuery: String) : TgroupArray; 

var N : String;

begin
  Result:=TgroupArray(GetMulti(AQuery,True,N));
end;


Function TgroupsEntitySet.ListAll(const AQuery: TQueryParams) : TgroupArray; 

var N : String;

begin
  Result:=TgroupArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TdirectoryRolesEntitySet
  --------------------------------------------------------------------}


Class Function TdirectoryRolesEntitySet.ObjectRestKind : String; 

begin
  Result:='directoryRoles';
end;

Class Function TdirectoryRolesEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TdirectoryRole;
end;


Function TdirectoryRolesEntitySet.Get(const id: string) : TdirectoryRole; 


begin
  Result:=TdirectoryRole(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TdirectoryRolesEntitySet.List(const AQuery: String; out NextLink: String) : TdirectoryRoleArray; 


begin
  Result:=TdirectoryRoleArray(GetMulti(AQuery,False,NextLink));
end;


Function TdirectoryRolesEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TdirectoryRoleArray; 


begin
  Result:=TdirectoryRoleArray(GetMulti(AQuery,False,NextLink));
end;


Function TdirectoryRolesEntitySet.ListAll(const AQuery: String) : TdirectoryRoleArray; 

var N : String;

begin
  Result:=TdirectoryRoleArray(GetMulti(AQuery,True,N));
end;


Function TdirectoryRolesEntitySet.ListAll(const AQuery: TQueryParams) : TdirectoryRoleArray; 

var N : String;

begin
  Result:=TdirectoryRoleArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TdirectoryRoleTemplatesEntitySet
  --------------------------------------------------------------------}


Class Function TdirectoryRoleTemplatesEntitySet.ObjectRestKind : String; 

begin
  Result:='directoryRoleTemplates';
end;

Class Function TdirectoryRoleTemplatesEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TdirectoryRoleTemplate;
end;


Function TdirectoryRoleTemplatesEntitySet.Get(const id: string) : TdirectoryRoleTemplate; 


begin
  Result:=TdirectoryRoleTemplate(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TdirectoryRoleTemplatesEntitySet.List(const AQuery: String; out NextLink: String) : TdirectoryRoleTemplateArray; 


begin
  Result:=TdirectoryRoleTemplateArray(GetMulti(AQuery,False,NextLink));
end;


Function TdirectoryRoleTemplatesEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TdirectoryRoleTemplateArray; 


begin
  Result:=TdirectoryRoleTemplateArray(GetMulti(AQuery,False,NextLink));
end;


Function TdirectoryRoleTemplatesEntitySet.ListAll(const AQuery: String) : TdirectoryRoleTemplateArray; 

var N : String;

begin
  Result:=TdirectoryRoleTemplateArray(GetMulti(AQuery,True,N));
end;


Function TdirectoryRoleTemplatesEntitySet.ListAll(const AQuery: TQueryParams) : TdirectoryRoleTemplateArray; 

var N : String;

begin
  Result:=TdirectoryRoleTemplateArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TorganizationEntitySet
  --------------------------------------------------------------------}


Class Function TorganizationEntitySet.ObjectRestKind : String; 

begin
  Result:='organization';
end;

Class Function TorganizationEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=Torganization;
end;


Function TorganizationEntitySet.Get(const id: string) : Torganization; 


begin
  Result:=Torganization(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TorganizationEntitySet.List(const AQuery: String; out NextLink: String) : TorganizationArray; 


begin
  Result:=TorganizationArray(GetMulti(AQuery,False,NextLink));
end;


Function TorganizationEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TorganizationArray; 


begin
  Result:=TorganizationArray(GetMulti(AQuery,False,NextLink));
end;


Function TorganizationEntitySet.ListAll(const AQuery: String) : TorganizationArray; 

var N : String;

begin
  Result:=TorganizationArray(GetMulti(AQuery,True,N));
end;


Function TorganizationEntitySet.ListAll(const AQuery: TQueryParams) : TorganizationArray; 

var N : String;

begin
  Result:=TorganizationArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TsubscribedSkusEntitySet
  --------------------------------------------------------------------}


Class Function TsubscribedSkusEntitySet.ObjectRestKind : String; 

begin
  Result:='subscribedSkus';
end;

Class Function TsubscribedSkusEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TsubscribedSku;
end;


Function TsubscribedSkusEntitySet.Get(const id: string) : TsubscribedSku; 


begin
  Result:=TsubscribedSku(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TsubscribedSkusEntitySet.List(const AQuery: String; out NextLink: String) : TsubscribedSkuArray; 


begin
  Result:=TsubscribedSkuArray(GetMulti(AQuery,False,NextLink));
end;


Function TsubscribedSkusEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TsubscribedSkuArray; 


begin
  Result:=TsubscribedSkuArray(GetMulti(AQuery,False,NextLink));
end;


Function TsubscribedSkusEntitySet.ListAll(const AQuery: String) : TsubscribedSkuArray; 

var N : String;

begin
  Result:=TsubscribedSkuArray(GetMulti(AQuery,True,N));
end;


Function TsubscribedSkusEntitySet.ListAll(const AQuery: TQueryParams) : TsubscribedSkuArray; 

var N : String;

begin
  Result:=TsubscribedSkuArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TusersEntitySet
  --------------------------------------------------------------------}


Class Function TusersEntitySet.ObjectRestKind : String; 

begin
  Result:='users';
end;

Class Function TusersEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=Tuser;
end;


Function TusersEntitySet.Get(const id: string) : Tuser; 


begin
  Result:=Tuser(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TusersEntitySet.List(const AQuery: String; out NextLink: String) : TuserArray; 


begin
  Result:=TuserArray(GetMulti(AQuery,False,NextLink));
end;


Function TusersEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TuserArray; 


begin
  Result:=TuserArray(GetMulti(AQuery,False,NextLink));
end;


Function TusersEntitySet.ListAll(const AQuery: String) : TuserArray; 

var N : String;

begin
  Result:=TuserArray(GetMulti(AQuery,True,N));
end;


Function TusersEntitySet.ListAll(const AQuery: TQueryParams) : TuserArray; 

var N : String;

begin
  Result:=TuserArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TdrivesEntitySet
  --------------------------------------------------------------------}


Class Function TdrivesEntitySet.ObjectRestKind : String; 

begin
  Result:='drives';
end;

Class Function TdrivesEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=Tdrive;
end;


Function TdrivesEntitySet.Get(const id: string) : Tdrive; 


begin
  Result:=Tdrive(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TdrivesEntitySet.List(const AQuery: String; out NextLink: String) : TdriveArray; 


begin
  Result:=TdriveArray(GetMulti(AQuery,False,NextLink));
end;


Function TdrivesEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TdriveArray; 


begin
  Result:=TdriveArray(GetMulti(AQuery,False,NextLink));
end;


Function TdrivesEntitySet.ListAll(const AQuery: String) : TdriveArray; 

var N : String;

begin
  Result:=TdriveArray(GetMulti(AQuery,True,N));
end;


Function TdrivesEntitySet.ListAll(const AQuery: TQueryParams) : TdriveArray; 

var N : String;

begin
  Result:=TdriveArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TsubscriptionsEntitySet
  --------------------------------------------------------------------}


Class Function TsubscriptionsEntitySet.ObjectRestKind : String; 

begin
  Result:='subscriptions';
end;

Class Function TsubscriptionsEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=Tsubscription;
end;


Function TsubscriptionsEntitySet.Get(const id: string) : Tsubscription; 


begin
  Result:=Tsubscription(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TsubscriptionsEntitySet.List(const AQuery: String; out NextLink: String) : TsubscriptionArray; 


begin
  Result:=TsubscriptionArray(GetMulti(AQuery,False,NextLink));
end;


Function TsubscriptionsEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TsubscriptionArray; 


begin
  Result:=TsubscriptionArray(GetMulti(AQuery,False,NextLink));
end;


Function TsubscriptionsEntitySet.ListAll(const AQuery: String) : TsubscriptionArray; 

var N : String;

begin
  Result:=TsubscriptionArray(GetMulti(AQuery,True,N));
end;


Function TsubscriptionsEntitySet.ListAll(const AQuery: TQueryParams) : TsubscriptionArray; 

var N : String;

begin
  Result:=TsubscriptionArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TconversationThreadImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TconversationThreadImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='conversationThreadImplicitEntitySet';
end;

Class Function TconversationThreadImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TconversationThread;
end;


Function TconversationThreadImplicitEntitySet.Get(const id: string) : TconversationThread; 


begin
  Result:=TconversationThread(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TconversationThreadImplicitEntitySet.List(const AQuery: String; out NextLink: String) : TconversationThreadArray; 


begin
  Result:=TconversationThreadArray(GetMulti(AQuery,False,NextLink));
end;


Function TconversationThreadImplicitEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TconversationThreadArray; 


begin
  Result:=TconversationThreadArray(GetMulti(AQuery,False,NextLink));
end;


Function TconversationThreadImplicitEntitySet.ListAll(const AQuery: String) : TconversationThreadArray; 

var N : String;

begin
  Result:=TconversationThreadArray(GetMulti(AQuery,True,N));
end;


Function TconversationThreadImplicitEntitySet.ListAll(const AQuery: TQueryParams) : TconversationThreadArray; 

var N : String;

begin
  Result:=TconversationThreadArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TcalendarImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TcalendarImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='calendarImplicitEntitySet';
end;

Class Function TcalendarImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=Tcalendar;
end;


Function TcalendarImplicitEntitySet.Get(const id: string) : Tcalendar; 


begin
  Result:=Tcalendar(GetSingle(TODataObject.MakeKeyString(id)));
end;


{ --------------------------------------------------------------------
  TeventImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TeventImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='eventImplicitEntitySet';
end;

Class Function TeventImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=Tevent;
end;


Function TeventImplicitEntitySet.Get(const id: string) : Tevent; 


begin
  Result:=Tevent(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TeventImplicitEntitySet.List(const AQuery: String; out NextLink: String) : TeventArray; 


begin
  Result:=TeventArray(GetMulti(AQuery,False,NextLink));
end;


Function TeventImplicitEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TeventArray; 


begin
  Result:=TeventArray(GetMulti(AQuery,False,NextLink));
end;


Function TeventImplicitEntitySet.ListAll(const AQuery: String) : TeventArray; 

var N : String;

begin
  Result:=TeventArray(GetMulti(AQuery,True,N));
end;


Function TeventImplicitEntitySet.ListAll(const AQuery: TQueryParams) : TeventArray; 

var N : String;

begin
  Result:=TeventArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TconversationImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TconversationImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='conversationImplicitEntitySet';
end;

Class Function TconversationImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=Tconversation;
end;


Function TconversationImplicitEntitySet.Get(const id: string) : Tconversation; 


begin
  Result:=Tconversation(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TconversationImplicitEntitySet.List(const AQuery: String; out NextLink: String) : TconversationArray; 


begin
  Result:=TconversationArray(GetMulti(AQuery,False,NextLink));
end;


Function TconversationImplicitEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TconversationArray; 


begin
  Result:=TconversationArray(GetMulti(AQuery,False,NextLink));
end;


Function TconversationImplicitEntitySet.ListAll(const AQuery: String) : TconversationArray; 

var N : String;

begin
  Result:=TconversationArray(GetMulti(AQuery,True,N));
end;


Function TconversationImplicitEntitySet.ListAll(const AQuery: TQueryParams) : TconversationArray; 

var N : String;

begin
  Result:=TconversationArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TprofilePhotoImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TprofilePhotoImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='profilePhotoImplicitEntitySet';
end;

Class Function TprofilePhotoImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TprofilePhoto;
end;


Function TprofilePhotoImplicitEntitySet.Get(const id: string) : TprofilePhoto; 


begin
  Result:=TprofilePhoto(GetSingle(TODataObject.MakeKeyString(id)));
end;


{ --------------------------------------------------------------------
  TpostImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TpostImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='postImplicitEntitySet';
end;

Class Function TpostImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=Tpost;
end;


Function TpostImplicitEntitySet.Get(const id: string) : Tpost; 


begin
  Result:=Tpost(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TpostImplicitEntitySet.List(const AQuery: String; out NextLink: String) : TpostArray; 


begin
  Result:=TpostArray(GetMulti(AQuery,False,NextLink));
end;


Function TpostImplicitEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TpostArray; 


begin
  Result:=TpostArray(GetMulti(AQuery,False,NextLink));
end;


Function TpostImplicitEntitySet.ListAll(const AQuery: String) : TpostArray; 

var N : String;

begin
  Result:=TpostArray(GetMulti(AQuery,True,N));
end;


Function TpostImplicitEntitySet.ListAll(const AQuery: TQueryParams) : TpostArray; 

var N : String;

begin
  Result:=TpostArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TattachmentImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TattachmentImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='attachmentImplicitEntitySet';
end;

Class Function TattachmentImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=Tattachment;
end;


Function TattachmentImplicitEntitySet.Get(const id: string) : Tattachment; 


begin
  Result:=Tattachment(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TattachmentImplicitEntitySet.List(const AQuery: String; out NextLink: String) : TattachmentArray; 


begin
  Result:=TattachmentArray(GetMulti(AQuery,False,NextLink));
end;


Function TattachmentImplicitEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TattachmentArray; 


begin
  Result:=TattachmentArray(GetMulti(AQuery,False,NextLink));
end;


Function TattachmentImplicitEntitySet.ListAll(const AQuery: String) : TattachmentArray; 

var N : String;

begin
  Result:=TattachmentArray(GetMulti(AQuery,True,N));
end;


Function TattachmentImplicitEntitySet.ListAll(const AQuery: TQueryParams) : TattachmentArray; 

var N : String;

begin
  Result:=TattachmentArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TdriveItemImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TdriveItemImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='driveItemImplicitEntitySet';
end;

Class Function TdriveItemImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TdriveItem;
end;


Function TdriveItemImplicitEntitySet.Get(const id: string) : TdriveItem; 


begin
  Result:=TdriveItem(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TdriveItemImplicitEntitySet.List(const AQuery: String; out NextLink: String) : TdriveItemArray; 


begin
  Result:=TdriveItemArray(GetMulti(AQuery,False,NextLink));
end;


Function TdriveItemImplicitEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TdriveItemArray; 


begin
  Result:=TdriveItemArray(GetMulti(AQuery,False,NextLink));
end;


Function TdriveItemImplicitEntitySet.ListAll(const AQuery: String) : TdriveItemArray; 

var N : String;

begin
  Result:=TdriveItemArray(GetMulti(AQuery,True,N));
end;


Function TdriveItemImplicitEntitySet.ListAll(const AQuery: TQueryParams) : TdriveItemArray; 

var N : String;

begin
  Result:=TdriveItemArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TmessageImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TmessageImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='messageImplicitEntitySet';
end;

Class Function TmessageImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=Tmessage;
end;


Function TmessageImplicitEntitySet.Get(const id: string) : Tmessage; 


begin
  Result:=Tmessage(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TmessageImplicitEntitySet.List(const AQuery: String; out NextLink: String) : TmessageArray; 


begin
  Result:=TmessageArray(GetMulti(AQuery,False,NextLink));
end;


Function TmessageImplicitEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TmessageArray; 


begin
  Result:=TmessageArray(GetMulti(AQuery,False,NextLink));
end;


Function TmessageImplicitEntitySet.ListAll(const AQuery: String) : TmessageArray; 

var N : String;

begin
  Result:=TmessageArray(GetMulti(AQuery,True,N));
end;


Function TmessageImplicitEntitySet.ListAll(const AQuery: TQueryParams) : TmessageArray; 

var N : String;

begin
  Result:=TmessageArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TmailFolderImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TmailFolderImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='mailFolderImplicitEntitySet';
end;

Class Function TmailFolderImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TmailFolder;
end;


Function TmailFolderImplicitEntitySet.Get(const id: string) : TmailFolder; 


begin
  Result:=TmailFolder(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TmailFolderImplicitEntitySet.List(const AQuery: String; out NextLink: String) : TmailFolderArray; 


begin
  Result:=TmailFolderArray(GetMulti(AQuery,False,NextLink));
end;


Function TmailFolderImplicitEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TmailFolderArray; 


begin
  Result:=TmailFolderArray(GetMulti(AQuery,False,NextLink));
end;


Function TmailFolderImplicitEntitySet.ListAll(const AQuery: String) : TmailFolderArray; 

var N : String;

begin
  Result:=TmailFolderArray(GetMulti(AQuery,True,N));
end;


Function TmailFolderImplicitEntitySet.ListAll(const AQuery: TQueryParams) : TmailFolderArray; 

var N : String;

begin
  Result:=TmailFolderArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TcalendarGroupImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TcalendarGroupImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='calendarGroupImplicitEntitySet';
end;

Class Function TcalendarGroupImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TcalendarGroup;
end;


Function TcalendarGroupImplicitEntitySet.Get(const id: string) : TcalendarGroup; 


begin
  Result:=TcalendarGroup(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TcalendarGroupImplicitEntitySet.List(const AQuery: String; out NextLink: String) : TcalendarGroupArray; 


begin
  Result:=TcalendarGroupArray(GetMulti(AQuery,False,NextLink));
end;


Function TcalendarGroupImplicitEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TcalendarGroupArray; 


begin
  Result:=TcalendarGroupArray(GetMulti(AQuery,False,NextLink));
end;


Function TcalendarGroupImplicitEntitySet.ListAll(const AQuery: String) : TcalendarGroupArray; 

var N : String;

begin
  Result:=TcalendarGroupArray(GetMulti(AQuery,True,N));
end;


Function TcalendarGroupImplicitEntitySet.ListAll(const AQuery: TQueryParams) : TcalendarGroupArray; 

var N : String;

begin
  Result:=TcalendarGroupArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TcontactImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TcontactImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='contactImplicitEntitySet';
end;

Class Function TcontactImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=Tcontact;
end;


Function TcontactImplicitEntitySet.Get(const id: string) : Tcontact; 


begin
  Result:=Tcontact(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TcontactImplicitEntitySet.List(const AQuery: String; out NextLink: String) : TcontactArray; 


begin
  Result:=TcontactArray(GetMulti(AQuery,False,NextLink));
end;


Function TcontactImplicitEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TcontactArray; 


begin
  Result:=TcontactArray(GetMulti(AQuery,False,NextLink));
end;


Function TcontactImplicitEntitySet.ListAll(const AQuery: String) : TcontactArray; 

var N : String;

begin
  Result:=TcontactArray(GetMulti(AQuery,True,N));
end;


Function TcontactImplicitEntitySet.ListAll(const AQuery: TQueryParams) : TcontactArray; 

var N : String;

begin
  Result:=TcontactArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TcontactFolderImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TcontactFolderImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='contactFolderImplicitEntitySet';
end;

Class Function TcontactFolderImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TcontactFolder;
end;


Function TcontactFolderImplicitEntitySet.Get(const id: string) : TcontactFolder; 


begin
  Result:=TcontactFolder(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TcontactFolderImplicitEntitySet.List(const AQuery: String; out NextLink: String) : TcontactFolderArray; 


begin
  Result:=TcontactFolderArray(GetMulti(AQuery,False,NextLink));
end;


Function TcontactFolderImplicitEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TcontactFolderArray; 


begin
  Result:=TcontactFolderArray(GetMulti(AQuery,False,NextLink));
end;


Function TcontactFolderImplicitEntitySet.ListAll(const AQuery: String) : TcontactFolderArray; 

var N : String;

begin
  Result:=TcontactFolderArray(GetMulti(AQuery,True,N));
end;


Function TcontactFolderImplicitEntitySet.ListAll(const AQuery: TQueryParams) : TcontactFolderArray; 

var N : String;

begin
  Result:=TcontactFolderArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  ToutlookItemImplicitEntitySet
  --------------------------------------------------------------------}


Class Function ToutlookItemImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='outlookItemImplicitEntitySet';
end;

Class Function ToutlookItemImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=ToutlookItem;
end;


Function ToutlookItemImplicitEntitySet.Get(const id: string) : ToutlookItem; 


begin
  Result:=ToutlookItem(GetSingle(TODataObject.MakeKeyString(id)));
end;


{ --------------------------------------------------------------------
  TpermissionImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TpermissionImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='permissionImplicitEntitySet';
end;

Class Function TpermissionImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=Tpermission;
end;


Function TpermissionImplicitEntitySet.Get(const id: string) : Tpermission; 


begin
  Result:=Tpermission(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TpermissionImplicitEntitySet.List(const AQuery: String; out NextLink: String) : TpermissionArray; 


begin
  Result:=TpermissionArray(GetMulti(AQuery,False,NextLink));
end;


Function TpermissionImplicitEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TpermissionArray; 


begin
  Result:=TpermissionArray(GetMulti(AQuery,False,NextLink));
end;


Function TpermissionImplicitEntitySet.ListAll(const AQuery: String) : TpermissionArray; 

var N : String;

begin
  Result:=TpermissionArray(GetMulti(AQuery,True,N));
end;


Function TpermissionImplicitEntitySet.ListAll(const AQuery: TQueryParams) : TpermissionArray; 

var N : String;

begin
  Result:=TpermissionArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TthumbnailSetImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TthumbnailSetImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='thumbnailSetImplicitEntitySet';
end;

Class Function TthumbnailSetImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TthumbnailSet;
end;


Function TthumbnailSetImplicitEntitySet.Get(const id: string) : TthumbnailSet; 


begin
  Result:=TthumbnailSet(GetSingle(TODataObject.MakeKeyString(id)));
end;


Function TthumbnailSetImplicitEntitySet.List(const AQuery: String; out NextLink: String) : TthumbnailSetArray; 


begin
  Result:=TthumbnailSetArray(GetMulti(AQuery,False,NextLink));
end;


Function TthumbnailSetImplicitEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TthumbnailSetArray; 


begin
  Result:=TthumbnailSetArray(GetMulti(AQuery,False,NextLink));
end;


Function TthumbnailSetImplicitEntitySet.ListAll(const AQuery: String) : TthumbnailSetArray; 

var N : String;

begin
  Result:=TthumbnailSetArray(GetMulti(AQuery,True,N));
end;


Function TthumbnailSetImplicitEntitySet.ListAll(const AQuery: TQueryParams) : TthumbnailSetArray; 

var N : String;

begin
  Result:=TthumbnailSetArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TService
  --------------------------------------------------------------------}


Class Function TService.ObjectRestKind : String; 

begin
  Result:='microsoft.graph';
end;

Function TService.CreateNewGraphService : TGraphService; 

begin
  Result:=TGraphService(CreateEntityContainer(TGraphService));
end;


Function TService.GetGraphService : TGraphService; 


begin
  If Not Assigned(FGraphService) then
    FGraphService:=TGraphService(CreateEntityContainer(TGraphService));
  Result:=FGraphService;
end;

end.
