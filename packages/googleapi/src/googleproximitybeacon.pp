unit googleproximitybeacon;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TBeacon = Class;
  TAdvertisedId = Class;
  TLatLng = Class;
  TIndoorLevel = Class;
  TEphemeralIdRegistration = Class;
  TEmpty = Class;
  TListBeaconsResponse = Class;
  TBeaconAttachment = Class;
  TListBeaconAttachmentsResponse = Class;
  TDeleteAttachmentsResponse = Class;
  TListNamespacesResponse = Class;
  TNamespace = Class;
  TEphemeralIdRegistrationParams = Class;
  TListDiagnosticsResponse = Class;
  TDiagnostics = Class;
  TDate = Class;
  TGetInfoForObservedBeaconsRequest = Class;
  TObservation = Class;
  TGetInfoForObservedBeaconsResponse = Class;
  TBeaconInfo = Class;
  TAttachmentInfo = Class;
  TBeaconArray = Array of TBeacon;
  TAdvertisedIdArray = Array of TAdvertisedId;
  TLatLngArray = Array of TLatLng;
  TIndoorLevelArray = Array of TIndoorLevel;
  TEphemeralIdRegistrationArray = Array of TEphemeralIdRegistration;
  TEmptyArray = Array of TEmpty;
  TListBeaconsResponseArray = Array of TListBeaconsResponse;
  TBeaconAttachmentArray = Array of TBeaconAttachment;
  TListBeaconAttachmentsResponseArray = Array of TListBeaconAttachmentsResponse;
  TDeleteAttachmentsResponseArray = Array of TDeleteAttachmentsResponse;
  TListNamespacesResponseArray = Array of TListNamespacesResponse;
  TNamespaceArray = Array of TNamespace;
  TEphemeralIdRegistrationParamsArray = Array of TEphemeralIdRegistrationParams;
  TListDiagnosticsResponseArray = Array of TListDiagnosticsResponse;
  TDiagnosticsArray = Array of TDiagnostics;
  TDateArray = Array of TDate;
  TGetInfoForObservedBeaconsRequestArray = Array of TGetInfoForObservedBeaconsRequest;
  TObservationArray = Array of TObservation;
  TGetInfoForObservedBeaconsResponseArray = Array of TGetInfoForObservedBeaconsResponse;
  TBeaconInfoArray = Array of TBeaconInfo;
  TAttachmentInfoArray = Array of TAttachmentInfo;
  //Anonymous types, using auto-generated names
  TBeaconTypeproperties = Class;
  TListBeaconsResponseTypebeaconsArray = Array of TBeacon;
  TListBeaconAttachmentsResponseTypeattachmentsArray = Array of TBeaconAttachment;
  TListNamespacesResponseTypenamespacesArray = Array of TNamespace;
  TListDiagnosticsResponseTypediagnosticsArray = Array of TDiagnostics;
  TGetInfoForObservedBeaconsRequestTypeobservationsArray = Array of TObservation;
  TGetInfoForObservedBeaconsResponseTypebeaconsArray = Array of TBeaconInfo;
  TBeaconInfoTypeattachmentsArray = Array of TAttachmentInfo;
  
  { --------------------------------------------------------------------
    TBeaconTypeproperties
    --------------------------------------------------------------------}
  
  TBeaconTypeproperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TBeaconTypepropertiesClass = Class of TBeaconTypeproperties;
  
  { --------------------------------------------------------------------
    TBeacon
    --------------------------------------------------------------------}
  
  TBeacon = Class(TGoogleBaseObject)
  Private
    FbeaconName : String;
    FadvertisedId : TAdvertisedId;
    Fstatus : String;
    FplaceId : String;
    FlatLng : TLatLng;
    FindoorLevel : TIndoorLevel;
    FexpectedStability : String;
    Fdescription : String;
    Fproperties : TBeaconTypeproperties;
    FephemeralIdRegistration : TEphemeralIdRegistration;
    FprovisioningKey : String;
  Protected
    //Property setters
    Procedure SetbeaconName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetadvertisedId(AIndex : Integer; const AValue : TAdvertisedId); virtual;
    Procedure Setstatus(AIndex : Integer; const AValue : String); virtual;
    Procedure SetplaceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetlatLng(AIndex : Integer; const AValue : TLatLng); virtual;
    Procedure SetindoorLevel(AIndex : Integer; const AValue : TIndoorLevel); virtual;
    Procedure SetexpectedStability(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setproperties(AIndex : Integer; const AValue : TBeaconTypeproperties); virtual;
    Procedure SetephemeralIdRegistration(AIndex : Integer; const AValue : TEphemeralIdRegistration); virtual;
    Procedure SetprovisioningKey(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property beaconName : String Index 0 Read FbeaconName Write SetbeaconName;
    Property advertisedId : TAdvertisedId Index 8 Read FadvertisedId Write SetadvertisedId;
    Property status : String Index 16 Read Fstatus Write Setstatus;
    Property placeId : String Index 24 Read FplaceId Write SetplaceId;
    Property latLng : TLatLng Index 32 Read FlatLng Write SetlatLng;
    Property indoorLevel : TIndoorLevel Index 40 Read FindoorLevel Write SetindoorLevel;
    Property expectedStability : String Index 48 Read FexpectedStability Write SetexpectedStability;
    Property description : String Index 56 Read Fdescription Write Setdescription;
    Property properties : TBeaconTypeproperties Index 64 Read Fproperties Write Setproperties;
    Property ephemeralIdRegistration : TEphemeralIdRegistration Index 72 Read FephemeralIdRegistration Write SetephemeralIdRegistration;
    Property provisioningKey : String Index 80 Read FprovisioningKey Write SetprovisioningKey;
  end;
  TBeaconClass = Class of TBeacon;
  
  { --------------------------------------------------------------------
    TAdvertisedId
    --------------------------------------------------------------------}
  
  TAdvertisedId = Class(TGoogleBaseObject)
  Private
    F_type : String;
    Fid : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property id : String Index 8 Read Fid Write Setid;
  end;
  TAdvertisedIdClass = Class of TAdvertisedId;
  
  { --------------------------------------------------------------------
    TLatLng
    --------------------------------------------------------------------}
  
  TLatLng = Class(TGoogleBaseObject)
  Private
    Flatitude : double;
    Flongitude : double;
  Protected
    //Property setters
    Procedure Setlatitude(AIndex : Integer; const AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property latitude : double Index 0 Read Flatitude Write Setlatitude;
    Property longitude : double Index 8 Read Flongitude Write Setlongitude;
  end;
  TLatLngClass = Class of TLatLng;
  
  { --------------------------------------------------------------------
    TIndoorLevel
    --------------------------------------------------------------------}
  
  TIndoorLevel = Class(TGoogleBaseObject)
  Private
    Fname : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
  end;
  TIndoorLevelClass = Class of TIndoorLevel;
  
  { --------------------------------------------------------------------
    TEphemeralIdRegistration
    --------------------------------------------------------------------}
  
  TEphemeralIdRegistration = Class(TGoogleBaseObject)
  Private
    FbeaconEcdhPublicKey : String;
    FserviceEcdhPublicKey : String;
    FbeaconIdentityKey : String;
    FrotationPeriodExponent : integer;
    FinitialClockValue : String;
    FinitialEid : String;
  Protected
    //Property setters
    Procedure SetbeaconEcdhPublicKey(AIndex : Integer; const AValue : String); virtual;
    Procedure SetserviceEcdhPublicKey(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbeaconIdentityKey(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrotationPeriodExponent(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetinitialClockValue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinitialEid(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property beaconEcdhPublicKey : String Index 0 Read FbeaconEcdhPublicKey Write SetbeaconEcdhPublicKey;
    Property serviceEcdhPublicKey : String Index 8 Read FserviceEcdhPublicKey Write SetserviceEcdhPublicKey;
    Property beaconIdentityKey : String Index 16 Read FbeaconIdentityKey Write SetbeaconIdentityKey;
    Property rotationPeriodExponent : integer Index 24 Read FrotationPeriodExponent Write SetrotationPeriodExponent;
    Property initialClockValue : String Index 32 Read FinitialClockValue Write SetinitialClockValue;
    Property initialEid : String Index 40 Read FinitialEid Write SetinitialEid;
  end;
  TEphemeralIdRegistrationClass = Class of TEphemeralIdRegistration;
  
  { --------------------------------------------------------------------
    TEmpty
    --------------------------------------------------------------------}
  
  TEmpty = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEmptyClass = Class of TEmpty;
  
  { --------------------------------------------------------------------
    TListBeaconsResponse
    --------------------------------------------------------------------}
  
  TListBeaconsResponse = Class(TGoogleBaseObject)
  Private
    Fbeacons : TListBeaconsResponseTypebeaconsArray;
    FnextPageToken : String;
    FtotalCount : String;
  Protected
    //Property setters
    Procedure Setbeacons(AIndex : Integer; const AValue : TListBeaconsResponseTypebeaconsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SettotalCount(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property beacons : TListBeaconsResponseTypebeaconsArray Index 0 Read Fbeacons Write Setbeacons;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property totalCount : String Index 16 Read FtotalCount Write SettotalCount;
  end;
  TListBeaconsResponseClass = Class of TListBeaconsResponse;
  
  { --------------------------------------------------------------------
    TBeaconAttachment
    --------------------------------------------------------------------}
  
  TBeaconAttachment = Class(TGoogleBaseObject)
  Private
    FattachmentName : String;
    FnamespacedType : String;
    Fdata : String;
  Protected
    //Property setters
    Procedure SetattachmentName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnamespacedType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdata(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property attachmentName : String Index 0 Read FattachmentName Write SetattachmentName;
    Property namespacedType : String Index 8 Read FnamespacedType Write SetnamespacedType;
    Property data : String Index 16 Read Fdata Write Setdata;
  end;
  TBeaconAttachmentClass = Class of TBeaconAttachment;
  
  { --------------------------------------------------------------------
    TListBeaconAttachmentsResponse
    --------------------------------------------------------------------}
  
  TListBeaconAttachmentsResponse = Class(TGoogleBaseObject)
  Private
    Fattachments : TListBeaconAttachmentsResponseTypeattachmentsArray;
  Protected
    //Property setters
    Procedure Setattachments(AIndex : Integer; const AValue : TListBeaconAttachmentsResponseTypeattachmentsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property attachments : TListBeaconAttachmentsResponseTypeattachmentsArray Index 0 Read Fattachments Write Setattachments;
  end;
  TListBeaconAttachmentsResponseClass = Class of TListBeaconAttachmentsResponse;
  
  { --------------------------------------------------------------------
    TDeleteAttachmentsResponse
    --------------------------------------------------------------------}
  
  TDeleteAttachmentsResponse = Class(TGoogleBaseObject)
  Private
    FnumDeleted : integer;
  Protected
    //Property setters
    Procedure SetnumDeleted(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property numDeleted : integer Index 0 Read FnumDeleted Write SetnumDeleted;
  end;
  TDeleteAttachmentsResponseClass = Class of TDeleteAttachmentsResponse;
  
  { --------------------------------------------------------------------
    TListNamespacesResponse
    --------------------------------------------------------------------}
  
  TListNamespacesResponse = Class(TGoogleBaseObject)
  Private
    Fnamespaces : TListNamespacesResponseTypenamespacesArray;
  Protected
    //Property setters
    Procedure Setnamespaces(AIndex : Integer; const AValue : TListNamespacesResponseTypenamespacesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property namespaces : TListNamespacesResponseTypenamespacesArray Index 0 Read Fnamespaces Write Setnamespaces;
  end;
  TListNamespacesResponseClass = Class of TListNamespacesResponse;
  
  { --------------------------------------------------------------------
    TNamespace
    --------------------------------------------------------------------}
  
  TNamespace = Class(TGoogleBaseObject)
  Private
    FnamespaceName : String;
    FservingVisibility : String;
  Protected
    //Property setters
    Procedure SetnamespaceName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetservingVisibility(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property namespaceName : String Index 0 Read FnamespaceName Write SetnamespaceName;
    Property servingVisibility : String Index 8 Read FservingVisibility Write SetservingVisibility;
  end;
  TNamespaceClass = Class of TNamespace;
  
  { --------------------------------------------------------------------
    TEphemeralIdRegistrationParams
    --------------------------------------------------------------------}
  
  TEphemeralIdRegistrationParams = Class(TGoogleBaseObject)
  Private
    FserviceEcdhPublicKey : String;
    FminRotationPeriodExponent : integer;
    FmaxRotationPeriodExponent : integer;
  Protected
    //Property setters
    Procedure SetserviceEcdhPublicKey(AIndex : Integer; const AValue : String); virtual;
    Procedure SetminRotationPeriodExponent(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetmaxRotationPeriodExponent(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property serviceEcdhPublicKey : String Index 0 Read FserviceEcdhPublicKey Write SetserviceEcdhPublicKey;
    Property minRotationPeriodExponent : integer Index 8 Read FminRotationPeriodExponent Write SetminRotationPeriodExponent;
    Property maxRotationPeriodExponent : integer Index 16 Read FmaxRotationPeriodExponent Write SetmaxRotationPeriodExponent;
  end;
  TEphemeralIdRegistrationParamsClass = Class of TEphemeralIdRegistrationParams;
  
  { --------------------------------------------------------------------
    TListDiagnosticsResponse
    --------------------------------------------------------------------}
  
  TListDiagnosticsResponse = Class(TGoogleBaseObject)
  Private
    Fdiagnostics : TListDiagnosticsResponseTypediagnosticsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setdiagnostics(AIndex : Integer; const AValue : TListDiagnosticsResponseTypediagnosticsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property diagnostics : TListDiagnosticsResponseTypediagnosticsArray Index 0 Read Fdiagnostics Write Setdiagnostics;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListDiagnosticsResponseClass = Class of TListDiagnosticsResponse;
  
  { --------------------------------------------------------------------
    TDiagnostics
    --------------------------------------------------------------------}
  
  TDiagnostics = Class(TGoogleBaseObject)
  Private
    FbeaconName : String;
    FestimatedLowBatteryDate : TDate;
    Falerts : TStringArray;
  Protected
    //Property setters
    Procedure SetbeaconName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetestimatedLowBatteryDate(AIndex : Integer; const AValue : TDate); virtual;
    Procedure Setalerts(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property beaconName : String Index 0 Read FbeaconName Write SetbeaconName;
    Property estimatedLowBatteryDate : TDate Index 8 Read FestimatedLowBatteryDate Write SetestimatedLowBatteryDate;
    Property alerts : TStringArray Index 16 Read Falerts Write Setalerts;
  end;
  TDiagnosticsClass = Class of TDiagnostics;
  
  { --------------------------------------------------------------------
    TDate
    --------------------------------------------------------------------}
  
  TDate = Class(TGoogleBaseObject)
  Private
    Fyear : integer;
    Fmonth : integer;
    Fday : integer;
  Protected
    //Property setters
    Procedure Setyear(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setmonth(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setday(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property year : integer Index 0 Read Fyear Write Setyear;
    Property month : integer Index 8 Read Fmonth Write Setmonth;
    Property day : integer Index 16 Read Fday Write Setday;
  end;
  TDateClass = Class of TDate;
  
  { --------------------------------------------------------------------
    TGetInfoForObservedBeaconsRequest
    --------------------------------------------------------------------}
  
  TGetInfoForObservedBeaconsRequest = Class(TGoogleBaseObject)
  Private
    Fobservations : TGetInfoForObservedBeaconsRequestTypeobservationsArray;
    FnamespacedTypes : TStringArray;
  Protected
    //Property setters
    Procedure Setobservations(AIndex : Integer; const AValue : TGetInfoForObservedBeaconsRequestTypeobservationsArray); virtual;
    Procedure SetnamespacedTypes(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property observations : TGetInfoForObservedBeaconsRequestTypeobservationsArray Index 0 Read Fobservations Write Setobservations;
    Property namespacedTypes : TStringArray Index 8 Read FnamespacedTypes Write SetnamespacedTypes;
  end;
  TGetInfoForObservedBeaconsRequestClass = Class of TGetInfoForObservedBeaconsRequest;
  
  { --------------------------------------------------------------------
    TObservation
    --------------------------------------------------------------------}
  
  TObservation = Class(TGoogleBaseObject)
  Private
    FadvertisedId : TAdvertisedId;
    Ftelemetry : String;
    FtimestampMs : String;
  Protected
    //Property setters
    Procedure SetadvertisedId(AIndex : Integer; const AValue : TAdvertisedId); virtual;
    Procedure Settelemetry(AIndex : Integer; const AValue : String); virtual;
    Procedure SettimestampMs(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property advertisedId : TAdvertisedId Index 0 Read FadvertisedId Write SetadvertisedId;
    Property telemetry : String Index 8 Read Ftelemetry Write Settelemetry;
    Property timestampMs : String Index 16 Read FtimestampMs Write SettimestampMs;
  end;
  TObservationClass = Class of TObservation;
  
  { --------------------------------------------------------------------
    TGetInfoForObservedBeaconsResponse
    --------------------------------------------------------------------}
  
  TGetInfoForObservedBeaconsResponse = Class(TGoogleBaseObject)
  Private
    Fbeacons : TGetInfoForObservedBeaconsResponseTypebeaconsArray;
  Protected
    //Property setters
    Procedure Setbeacons(AIndex : Integer; const AValue : TGetInfoForObservedBeaconsResponseTypebeaconsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property beacons : TGetInfoForObservedBeaconsResponseTypebeaconsArray Index 0 Read Fbeacons Write Setbeacons;
  end;
  TGetInfoForObservedBeaconsResponseClass = Class of TGetInfoForObservedBeaconsResponse;
  
  { --------------------------------------------------------------------
    TBeaconInfo
    --------------------------------------------------------------------}
  
  TBeaconInfo = Class(TGoogleBaseObject)
  Private
    FadvertisedId : TAdvertisedId;
    FbeaconName : String;
    Fattachments : TBeaconInfoTypeattachmentsArray;
  Protected
    //Property setters
    Procedure SetadvertisedId(AIndex : Integer; const AValue : TAdvertisedId); virtual;
    Procedure SetbeaconName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setattachments(AIndex : Integer; const AValue : TBeaconInfoTypeattachmentsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property advertisedId : TAdvertisedId Index 0 Read FadvertisedId Write SetadvertisedId;
    Property beaconName : String Index 8 Read FbeaconName Write SetbeaconName;
    Property attachments : TBeaconInfoTypeattachmentsArray Index 16 Read Fattachments Write Setattachments;
  end;
  TBeaconInfoClass = Class of TBeaconInfo;
  
  { --------------------------------------------------------------------
    TAttachmentInfo
    --------------------------------------------------------------------}
  
  TAttachmentInfo = Class(TGoogleBaseObject)
  Private
    FnamespacedType : String;
    Fdata : String;
  Protected
    //Property setters
    Procedure SetnamespacedType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdata(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property namespacedType : String Index 0 Read FnamespacedType Write SetnamespacedType;
    Property data : String Index 8 Read Fdata Write Setdata;
  end;
  TAttachmentInfoClass = Class of TAttachmentInfo;
  
  { --------------------------------------------------------------------
    TBeaconsAttachmentsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TBeaconsAttachmentsResource, method Create
  
  TBeaconsAttachmentsCreateOptions = Record
    projectId : String;
  end;
  
  
  //Optional query Options for TBeaconsAttachmentsResource, method List
  
  TBeaconsAttachmentsListOptions = Record
    namespacedType : String;
    projectId : String;
  end;
  
  
  //Optional query Options for TBeaconsAttachmentsResource, method Delete
  
  TBeaconsAttachmentsDeleteOptions = Record
    projectId : String;
  end;
  
  
  //Optional query Options for TBeaconsAttachmentsResource, method BatchDelete
  
  TBeaconsAttachmentsBatchDeleteOptions = Record
    namespacedType : String;
    projectId : String;
  end;
  
  TBeaconsAttachmentsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(beaconName: string; aBeaconAttachment : TBeaconAttachment; AQuery : string  = '') : TBeaconAttachment;overload;
    Function Create(beaconName: string; aBeaconAttachment : TBeaconAttachment; AQuery : TBeaconsAttachmentscreateOptions) : TBeaconAttachment;overload;
    Function List(beaconName: string; AQuery : string  = '') : TListBeaconAttachmentsResponse;
    Function List(beaconName: string; AQuery : TBeaconsAttachmentslistOptions) : TListBeaconAttachmentsResponse;
    Function Delete(attachmentName: string; AQuery : string  = '') : TEmpty;
    Function Delete(attachmentName: string; AQuery : TBeaconsAttachmentsdeleteOptions) : TEmpty;
    Function BatchDelete(beaconName: string; AQuery : string  = '') : TDeleteAttachmentsResponse;
    Function BatchDelete(beaconName: string; AQuery : TBeaconsAttachmentsbatchDeleteOptions) : TDeleteAttachmentsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TBeaconsDiagnosticsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TBeaconsDiagnosticsResource, method List
  
  TBeaconsDiagnosticsListOptions = Record
    pageSize : integer;
    pageToken : String;
    alertFilter : String;
    projectId : String;
  end;
  
  TBeaconsDiagnosticsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(beaconName: string; AQuery : string  = '') : TListDiagnosticsResponse;
    Function List(beaconName: string; AQuery : TBeaconsDiagnosticslistOptions) : TListDiagnosticsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TBeaconsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TBeaconsResource, method Register
  
  TBeaconsRegisterOptions = Record
    projectId : String;
  end;
  
  
  //Optional query Options for TBeaconsResource, method Decommission
  
  TBeaconsDecommissionOptions = Record
    projectId : String;
  end;
  
  
  //Optional query Options for TBeaconsResource, method Get
  
  TBeaconsGetOptions = Record
    projectId : String;
  end;
  
  
  //Optional query Options for TBeaconsResource, method List
  
  TBeaconsListOptions = Record
    q : String;
    pageToken : String;
    pageSize : integer;
    projectId : String;
  end;
  
  
  //Optional query Options for TBeaconsResource, method Update
  
  TBeaconsUpdateOptions = Record
    projectId : String;
  end;
  
  
  //Optional query Options for TBeaconsResource, method Activate
  
  TBeaconsActivateOptions = Record
    projectId : String;
  end;
  
  
  //Optional query Options for TBeaconsResource, method Deactivate
  
  TBeaconsDeactivateOptions = Record
    projectId : String;
  end;
  
  TBeaconsResource = Class(TGoogleResource)
  Private
    FAttachmentsInstance : TBeaconsAttachmentsResource;
    FDiagnosticsInstance : TBeaconsDiagnosticsResource;
    Function GetAttachmentsInstance : TBeaconsAttachmentsResource;virtual;
    Function GetDiagnosticsInstance : TBeaconsDiagnosticsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Register(aBeacon : TBeacon; AQuery : string  = '') : TBeacon;
    Function Register(aBeacon : TBeacon; AQuery : TBeaconsregisterOptions) : TBeacon;
    Function Decommission(beaconName: string; AQuery : string  = '') : TEmpty;
    Function Decommission(beaconName: string; AQuery : TBeaconsdecommissionOptions) : TEmpty;
    Function Get(beaconName: string; AQuery : string  = '') : TBeacon;
    Function Get(beaconName: string; AQuery : TBeaconsgetOptions) : TBeacon;
    Function List(AQuery : string  = '') : TListBeaconsResponse;
    Function List(AQuery : TBeaconslistOptions) : TListBeaconsResponse;
    Function Update(beaconName: string; aBeacon : TBeacon; AQuery : string  = '') : TBeacon;
    Function Update(beaconName: string; aBeacon : TBeacon; AQuery : TBeaconsupdateOptions) : TBeacon;
    Function Activate(beaconName: string; AQuery : string  = '') : TEmpty;
    Function Activate(beaconName: string; AQuery : TBeaconsactivateOptions) : TEmpty;
    Function Deactivate(beaconName: string; AQuery : string  = '') : TEmpty;
    Function Deactivate(beaconName: string; AQuery : TBeaconsdeactivateOptions) : TEmpty;
    Function CreateAttachmentsResource(AOwner : TComponent) : TBeaconsAttachmentsResource;virtual;overload;
    Function CreateAttachmentsResource : TBeaconsAttachmentsResource;virtual;overload;
    Function CreateDiagnosticsResource(AOwner : TComponent) : TBeaconsDiagnosticsResource;virtual;overload;
    Function CreateDiagnosticsResource : TBeaconsDiagnosticsResource;virtual;overload;
    Property AttachmentsResource : TBeaconsAttachmentsResource Read GetAttachmentsInstance;
    Property DiagnosticsResource : TBeaconsDiagnosticsResource Read GetDiagnosticsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TNamespacesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TNamespacesResource, method List
  
  TNamespacesListOptions = Record
    projectId : String;
  end;
  
  
  //Optional query Options for TNamespacesResource, method Update
  
  TNamespacesUpdateOptions = Record
    projectId : String;
  end;
  
  TNamespacesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TListNamespacesResponse;
    Function List(AQuery : TNamespaceslistOptions) : TListNamespacesResponse;
    Function Update(namespaceName: string; aNamespace : TNamespace; AQuery : string  = '') : TNamespace;
    Function Update(namespaceName: string; aNamespace : TNamespace; AQuery : TNamespacesupdateOptions) : TNamespace;
  end;
  
  
  { --------------------------------------------------------------------
    TV1beta1Resource
    --------------------------------------------------------------------}
  
  TV1beta1Resource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function GetEidparams : TEphemeralIdRegistrationParams;
  end;
  
  
  { --------------------------------------------------------------------
    TBeaconinfoResource
    --------------------------------------------------------------------}
  
  TBeaconinfoResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Getforobserved(aGetInfoForObservedBeaconsRequest : TGetInfoForObservedBeaconsRequest) : TGetInfoForObservedBeaconsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TProximitybeaconAPI
    --------------------------------------------------------------------}
  
  TProximitybeaconAPI = Class(TGoogleAPI)
  Private
    FBeaconsAttachmentsInstance : TBeaconsAttachmentsResource;
    FBeaconsDiagnosticsInstance : TBeaconsDiagnosticsResource;
    FBeaconsInstance : TBeaconsResource;
    FNamespacesInstance : TNamespacesResource;
    FV1beta1Instance : TV1beta1Resource;
    FBeaconinfoInstance : TBeaconinfoResource;
    Function GetBeaconsAttachmentsInstance : TBeaconsAttachmentsResource;virtual;
    Function GetBeaconsDiagnosticsInstance : TBeaconsDiagnosticsResource;virtual;
    Function GetBeaconsInstance : TBeaconsResource;virtual;
    Function GetNamespacesInstance : TNamespacesResource;virtual;
    Function GetV1beta1Instance : TV1beta1Resource;virtual;
    Function GetBeaconinfoInstance : TBeaconinfoResource;virtual;
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
    Function CreateBeaconsAttachmentsResource(AOwner : TComponent) : TBeaconsAttachmentsResource;virtual;overload;
    Function CreateBeaconsAttachmentsResource : TBeaconsAttachmentsResource;virtual;overload;
    Function CreateBeaconsDiagnosticsResource(AOwner : TComponent) : TBeaconsDiagnosticsResource;virtual;overload;
    Function CreateBeaconsDiagnosticsResource : TBeaconsDiagnosticsResource;virtual;overload;
    Function CreateBeaconsResource(AOwner : TComponent) : TBeaconsResource;virtual;overload;
    Function CreateBeaconsResource : TBeaconsResource;virtual;overload;
    Function CreateNamespacesResource(AOwner : TComponent) : TNamespacesResource;virtual;overload;
    Function CreateNamespacesResource : TNamespacesResource;virtual;overload;
    Function CreateV1beta1Resource(AOwner : TComponent) : TV1beta1Resource;virtual;overload;
    Function CreateV1beta1Resource : TV1beta1Resource;virtual;overload;
    Function CreateBeaconinfoResource(AOwner : TComponent) : TBeaconinfoResource;virtual;overload;
    Function CreateBeaconinfoResource : TBeaconinfoResource;virtual;overload;
    //Add default on-demand instances for resources
    Property BeaconsAttachmentsResource : TBeaconsAttachmentsResource Read GetBeaconsAttachmentsInstance;
    Property BeaconsDiagnosticsResource : TBeaconsDiagnosticsResource Read GetBeaconsDiagnosticsInstance;
    Property BeaconsResource : TBeaconsResource Read GetBeaconsInstance;
    Property NamespacesResource : TNamespacesResource Read GetNamespacesInstance;
    Property V1beta1Resource : TV1beta1Resource Read GetV1beta1Instance;
    Property BeaconinfoResource : TBeaconinfoResource Read GetBeaconinfoInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TBeaconTypeproperties
  --------------------------------------------------------------------}


Class Function TBeaconTypeproperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TBeacon
  --------------------------------------------------------------------}


Procedure TBeacon.SetbeaconName(AIndex : Integer; const AValue : String); 

begin
  If (FbeaconName=AValue) then exit;
  FbeaconName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBeacon.SetadvertisedId(AIndex : Integer; const AValue : TAdvertisedId); 

begin
  If (FadvertisedId=AValue) then exit;
  FadvertisedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBeacon.Setstatus(AIndex : Integer; const AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBeacon.SetplaceId(AIndex : Integer; const AValue : String); 

begin
  If (FplaceId=AValue) then exit;
  FplaceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBeacon.SetlatLng(AIndex : Integer; const AValue : TLatLng); 

begin
  If (FlatLng=AValue) then exit;
  FlatLng:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBeacon.SetindoorLevel(AIndex : Integer; const AValue : TIndoorLevel); 

begin
  If (FindoorLevel=AValue) then exit;
  FindoorLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBeacon.SetexpectedStability(AIndex : Integer; const AValue : String); 

begin
  If (FexpectedStability=AValue) then exit;
  FexpectedStability:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBeacon.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBeacon.Setproperties(AIndex : Integer; const AValue : TBeaconTypeproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBeacon.SetephemeralIdRegistration(AIndex : Integer; const AValue : TEphemeralIdRegistration); 

begin
  If (FephemeralIdRegistration=AValue) then exit;
  FephemeralIdRegistration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBeacon.SetprovisioningKey(AIndex : Integer; const AValue : String); 

begin
  If (FprovisioningKey=AValue) then exit;
  FprovisioningKey:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAdvertisedId
  --------------------------------------------------------------------}


Procedure TAdvertisedId.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAdvertisedId.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAdvertisedId.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TLatLng
  --------------------------------------------------------------------}


Procedure TLatLng.Setlatitude(AIndex : Integer; const AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLatLng.Setlongitude(AIndex : Integer; const AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TIndoorLevel
  --------------------------------------------------------------------}


Procedure TIndoorLevel.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEphemeralIdRegistration
  --------------------------------------------------------------------}


Procedure TEphemeralIdRegistration.SetbeaconEcdhPublicKey(AIndex : Integer; const AValue : String); 

begin
  If (FbeaconEcdhPublicKey=AValue) then exit;
  FbeaconEcdhPublicKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEphemeralIdRegistration.SetserviceEcdhPublicKey(AIndex : Integer; const AValue : String); 

begin
  If (FserviceEcdhPublicKey=AValue) then exit;
  FserviceEcdhPublicKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEphemeralIdRegistration.SetbeaconIdentityKey(AIndex : Integer; const AValue : String); 

begin
  If (FbeaconIdentityKey=AValue) then exit;
  FbeaconIdentityKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEphemeralIdRegistration.SetrotationPeriodExponent(AIndex : Integer; const AValue : integer); 

begin
  If (FrotationPeriodExponent=AValue) then exit;
  FrotationPeriodExponent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEphemeralIdRegistration.SetinitialClockValue(AIndex : Integer; const AValue : String); 

begin
  If (FinitialClockValue=AValue) then exit;
  FinitialClockValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEphemeralIdRegistration.SetinitialEid(AIndex : Integer; const AValue : String); 

begin
  If (FinitialEid=AValue) then exit;
  FinitialEid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListBeaconsResponse
  --------------------------------------------------------------------}


Procedure TListBeaconsResponse.Setbeacons(AIndex : Integer; const AValue : TListBeaconsResponseTypebeaconsArray); 

begin
  If (Fbeacons=AValue) then exit;
  Fbeacons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListBeaconsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListBeaconsResponse.SettotalCount(AIndex : Integer; const AValue : String); 

begin
  If (FtotalCount=AValue) then exit;
  FtotalCount:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListBeaconsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'beacons' : SetLength(Fbeacons,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBeaconAttachment
  --------------------------------------------------------------------}


Procedure TBeaconAttachment.SetattachmentName(AIndex : Integer; const AValue : String); 

begin
  If (FattachmentName=AValue) then exit;
  FattachmentName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBeaconAttachment.SetnamespacedType(AIndex : Integer; const AValue : String); 

begin
  If (FnamespacedType=AValue) then exit;
  FnamespacedType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBeaconAttachment.Setdata(AIndex : Integer; const AValue : String); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListBeaconAttachmentsResponse
  --------------------------------------------------------------------}


Procedure TListBeaconAttachmentsResponse.Setattachments(AIndex : Integer; const AValue : TListBeaconAttachmentsResponseTypeattachmentsArray); 

begin
  If (Fattachments=AValue) then exit;
  Fattachments:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListBeaconAttachmentsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'attachments' : SetLength(Fattachments,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDeleteAttachmentsResponse
  --------------------------------------------------------------------}


Procedure TDeleteAttachmentsResponse.SetnumDeleted(AIndex : Integer; const AValue : integer); 

begin
  If (FnumDeleted=AValue) then exit;
  FnumDeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListNamespacesResponse
  --------------------------------------------------------------------}


Procedure TListNamespacesResponse.Setnamespaces(AIndex : Integer; const AValue : TListNamespacesResponseTypenamespacesArray); 

begin
  If (Fnamespaces=AValue) then exit;
  Fnamespaces:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListNamespacesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'namespaces' : SetLength(Fnamespaces,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TNamespace
  --------------------------------------------------------------------}


Procedure TNamespace.SetnamespaceName(AIndex : Integer; const AValue : String); 

begin
  If (FnamespaceName=AValue) then exit;
  FnamespaceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNamespace.SetservingVisibility(AIndex : Integer; const AValue : String); 

begin
  If (FservingVisibility=AValue) then exit;
  FservingVisibility:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEphemeralIdRegistrationParams
  --------------------------------------------------------------------}


Procedure TEphemeralIdRegistrationParams.SetserviceEcdhPublicKey(AIndex : Integer; const AValue : String); 

begin
  If (FserviceEcdhPublicKey=AValue) then exit;
  FserviceEcdhPublicKey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEphemeralIdRegistrationParams.SetminRotationPeriodExponent(AIndex : Integer; const AValue : integer); 

begin
  If (FminRotationPeriodExponent=AValue) then exit;
  FminRotationPeriodExponent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEphemeralIdRegistrationParams.SetmaxRotationPeriodExponent(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxRotationPeriodExponent=AValue) then exit;
  FmaxRotationPeriodExponent:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListDiagnosticsResponse
  --------------------------------------------------------------------}


Procedure TListDiagnosticsResponse.Setdiagnostics(AIndex : Integer; const AValue : TListDiagnosticsResponseTypediagnosticsArray); 

begin
  If (Fdiagnostics=AValue) then exit;
  Fdiagnostics:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListDiagnosticsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListDiagnosticsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'diagnostics' : SetLength(Fdiagnostics,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDiagnostics
  --------------------------------------------------------------------}


Procedure TDiagnostics.SetbeaconName(AIndex : Integer; const AValue : String); 

begin
  If (FbeaconName=AValue) then exit;
  FbeaconName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiagnostics.SetestimatedLowBatteryDate(AIndex : Integer; const AValue : TDate); 

begin
  If (FestimatedLowBatteryDate=AValue) then exit;
  FestimatedLowBatteryDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDiagnostics.Setalerts(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Falerts=AValue) then exit;
  Falerts:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDiagnostics.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'alerts' : SetLength(Falerts,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDate
  --------------------------------------------------------------------}


Procedure TDate.Setyear(AIndex : Integer; const AValue : integer); 

begin
  If (Fyear=AValue) then exit;
  Fyear:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDate.Setmonth(AIndex : Integer; const AValue : integer); 

begin
  If (Fmonth=AValue) then exit;
  Fmonth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDate.Setday(AIndex : Integer; const AValue : integer); 

begin
  If (Fday=AValue) then exit;
  Fday:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGetInfoForObservedBeaconsRequest
  --------------------------------------------------------------------}


Procedure TGetInfoForObservedBeaconsRequest.Setobservations(AIndex : Integer; const AValue : TGetInfoForObservedBeaconsRequestTypeobservationsArray); 

begin
  If (Fobservations=AValue) then exit;
  Fobservations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGetInfoForObservedBeaconsRequest.SetnamespacedTypes(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FnamespacedTypes=AValue) then exit;
  FnamespacedTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGetInfoForObservedBeaconsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'observations' : SetLength(Fobservations,ALength);
  'namespacedtypes' : SetLength(FnamespacedTypes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TObservation
  --------------------------------------------------------------------}


Procedure TObservation.SetadvertisedId(AIndex : Integer; const AValue : TAdvertisedId); 

begin
  If (FadvertisedId=AValue) then exit;
  FadvertisedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObservation.Settelemetry(AIndex : Integer; const AValue : String); 

begin
  If (Ftelemetry=AValue) then exit;
  Ftelemetry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TObservation.SettimestampMs(AIndex : Integer; const AValue : String); 

begin
  If (FtimestampMs=AValue) then exit;
  FtimestampMs:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGetInfoForObservedBeaconsResponse
  --------------------------------------------------------------------}


Procedure TGetInfoForObservedBeaconsResponse.Setbeacons(AIndex : Integer; const AValue : TGetInfoForObservedBeaconsResponseTypebeaconsArray); 

begin
  If (Fbeacons=AValue) then exit;
  Fbeacons:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGetInfoForObservedBeaconsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'beacons' : SetLength(Fbeacons,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBeaconInfo
  --------------------------------------------------------------------}


Procedure TBeaconInfo.SetadvertisedId(AIndex : Integer; const AValue : TAdvertisedId); 

begin
  If (FadvertisedId=AValue) then exit;
  FadvertisedId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBeaconInfo.SetbeaconName(AIndex : Integer; const AValue : String); 

begin
  If (FbeaconName=AValue) then exit;
  FbeaconName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBeaconInfo.Setattachments(AIndex : Integer; const AValue : TBeaconInfoTypeattachmentsArray); 

begin
  If (Fattachments=AValue) then exit;
  Fattachments:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBeaconInfo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'attachments' : SetLength(Fattachments,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAttachmentInfo
  --------------------------------------------------------------------}


Procedure TAttachmentInfo.SetnamespacedType(AIndex : Integer; const AValue : String); 

begin
  If (FnamespacedType=AValue) then exit;
  FnamespacedType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAttachmentInfo.Setdata(AIndex : Integer; const AValue : String); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBeaconsAttachmentsResource
  --------------------------------------------------------------------}


Class Function TBeaconsAttachmentsResource.ResourceName : String;

begin
  Result:='attachments';
end;

Class Function TBeaconsAttachmentsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TproximitybeaconAPI;
end;

Function TBeaconsAttachmentsResource.Create(beaconName: string; aBeaconAttachment : TBeaconAttachment; AQuery : string = '') : TBeaconAttachment;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta1/{+beaconName}/attachments';
  _Methodid   = 'proximitybeacon.beacons.attachments.create';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['beaconName',beaconName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aBeaconAttachment,TBeaconAttachment) as TBeaconAttachment;
end;


Function TBeaconsAttachmentsResource.Create(beaconName: string; aBeaconAttachment : TBeaconAttachment; AQuery : TBeaconsAttachmentscreateOptions) : TBeaconAttachment;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=Create(beaconName,aBeaconAttachment,_Q);
end;

Function TBeaconsAttachmentsResource.List(beaconName: string; AQuery : string = '') : TListBeaconAttachmentsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta1/{+beaconName}/attachments';
  _Methodid   = 'proximitybeacon.beacons.attachments.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['beaconName',beaconName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListBeaconAttachmentsResponse) as TListBeaconAttachmentsResponse;
end;


Function TBeaconsAttachmentsResource.List(beaconName: string; AQuery : TBeaconsAttachmentslistOptions) : TListBeaconAttachmentsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'namespacedType',AQuery.namespacedType);
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=List(beaconName,_Q);
end;

Function TBeaconsAttachmentsResource.Delete(attachmentName: string; AQuery : string = '') : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1beta1/{+attachmentName}';
  _Methodid   = 'proximitybeacon.beacons.attachments.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['attachmentName',attachmentName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEmpty) as TEmpty;
end;


Function TBeaconsAttachmentsResource.Delete(attachmentName: string; AQuery : TBeaconsAttachmentsdeleteOptions) : TEmpty;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=Delete(attachmentName,_Q);
end;

Function TBeaconsAttachmentsResource.BatchDelete(beaconName: string; AQuery : string = '') : TDeleteAttachmentsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta1/{+beaconName}/attachments:batchDelete';
  _Methodid   = 'proximitybeacon.beacons.attachments.batchDelete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['beaconName',beaconName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TDeleteAttachmentsResponse) as TDeleteAttachmentsResponse;
end;


Function TBeaconsAttachmentsResource.BatchDelete(beaconName: string; AQuery : TBeaconsAttachmentsbatchDeleteOptions) : TDeleteAttachmentsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'namespacedType',AQuery.namespacedType);
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=BatchDelete(beaconName,_Q);
end;



{ --------------------------------------------------------------------
  TBeaconsDiagnosticsResource
  --------------------------------------------------------------------}


Class Function TBeaconsDiagnosticsResource.ResourceName : String;

begin
  Result:='diagnostics';
end;

Class Function TBeaconsDiagnosticsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TproximitybeaconAPI;
end;

Function TBeaconsDiagnosticsResource.List(beaconName: string; AQuery : string = '') : TListDiagnosticsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta1/{+beaconName}/diagnostics';
  _Methodid   = 'proximitybeacon.beacons.diagnostics.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['beaconName',beaconName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListDiagnosticsResponse) as TListDiagnosticsResponse;
end;


Function TBeaconsDiagnosticsResource.List(beaconName: string; AQuery : TBeaconsDiagnosticslistOptions) : TListDiagnosticsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'alertFilter',AQuery.alertFilter);
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=List(beaconName,_Q);
end;



{ --------------------------------------------------------------------
  TBeaconsResource
  --------------------------------------------------------------------}


Class Function TBeaconsResource.ResourceName : String;

begin
  Result:='beacons';
end;

Class Function TBeaconsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TproximitybeaconAPI;
end;

Function TBeaconsResource.Register(aBeacon : TBeacon; AQuery : string = '') : TBeacon;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta1/beacons:register';
  _Methodid   = 'proximitybeacon.beacons.register';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,aBeacon,TBeacon) as TBeacon;
end;


Function TBeaconsResource.Register(aBeacon : TBeacon; AQuery : TBeaconsregisterOptions) : TBeacon;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=Register(aBeacon,_Q);
end;

Function TBeaconsResource.Decommission(beaconName: string; AQuery : string = '') : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta1/{+beaconName}:decommission';
  _Methodid   = 'proximitybeacon.beacons.decommission';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['beaconName',beaconName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEmpty) as TEmpty;
end;


Function TBeaconsResource.Decommission(beaconName: string; AQuery : TBeaconsdecommissionOptions) : TEmpty;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=Decommission(beaconName,_Q);
end;

Function TBeaconsResource.Get(beaconName: string; AQuery : string = '') : TBeacon;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta1/{+beaconName}';
  _Methodid   = 'proximitybeacon.beacons.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['beaconName',beaconName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TBeacon) as TBeacon;
end;


Function TBeaconsResource.Get(beaconName: string; AQuery : TBeaconsgetOptions) : TBeacon;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=Get(beaconName,_Q);
end;

Function TBeaconsResource.List(AQuery : string = '') : TListBeaconsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta1/beacons';
  _Methodid   = 'proximitybeacon.beacons.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TListBeaconsResponse) as TListBeaconsResponse;
end;


Function TBeaconsResource.List(AQuery : TBeaconslistOptions) : TListBeaconsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'q',AQuery.q);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=List(_Q);
end;

Function TBeaconsResource.Update(beaconName: string; aBeacon : TBeacon; AQuery : string = '') : TBeacon;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v1beta1/{+beaconName}';
  _Methodid   = 'proximitybeacon.beacons.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['beaconName',beaconName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aBeacon,TBeacon) as TBeacon;
end;


Function TBeaconsResource.Update(beaconName: string; aBeacon : TBeacon; AQuery : TBeaconsupdateOptions) : TBeacon;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=Update(beaconName,aBeacon,_Q);
end;

Function TBeaconsResource.Activate(beaconName: string; AQuery : string = '') : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta1/{+beaconName}:activate';
  _Methodid   = 'proximitybeacon.beacons.activate';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['beaconName',beaconName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEmpty) as TEmpty;
end;


Function TBeaconsResource.Activate(beaconName: string; AQuery : TBeaconsactivateOptions) : TEmpty;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=Activate(beaconName,_Q);
end;

Function TBeaconsResource.Deactivate(beaconName: string; AQuery : string = '') : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta1/{+beaconName}:deactivate';
  _Methodid   = 'proximitybeacon.beacons.deactivate';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['beaconName',beaconName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TEmpty) as TEmpty;
end;


Function TBeaconsResource.Deactivate(beaconName: string; AQuery : TBeaconsdeactivateOptions) : TEmpty;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=Deactivate(beaconName,_Q);
end;



Function TBeaconsResource.GetAttachmentsInstance : TBeaconsAttachmentsResource;

begin
  if (FAttachmentsInstance=Nil) then
    FAttachmentsInstance:=CreateAttachmentsResource;
  Result:=FAttachmentsInstance;
end;

Function TBeaconsResource.CreateAttachmentsResource : TBeaconsAttachmentsResource;

begin
  Result:=CreateAttachmentsResource(Self);
end;


Function TBeaconsResource.CreateAttachmentsResource(AOwner : TComponent) : TBeaconsAttachmentsResource;

begin
  Result:=TBeaconsAttachmentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TBeaconsResource.GetDiagnosticsInstance : TBeaconsDiagnosticsResource;

begin
  if (FDiagnosticsInstance=Nil) then
    FDiagnosticsInstance:=CreateDiagnosticsResource;
  Result:=FDiagnosticsInstance;
end;

Function TBeaconsResource.CreateDiagnosticsResource : TBeaconsDiagnosticsResource;

begin
  Result:=CreateDiagnosticsResource(Self);
end;


Function TBeaconsResource.CreateDiagnosticsResource(AOwner : TComponent) : TBeaconsDiagnosticsResource;

begin
  Result:=TBeaconsDiagnosticsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TNamespacesResource
  --------------------------------------------------------------------}


Class Function TNamespacesResource.ResourceName : String;

begin
  Result:='namespaces';
end;

Class Function TNamespacesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TproximitybeaconAPI;
end;

Function TNamespacesResource.List(AQuery : string = '') : TListNamespacesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta1/namespaces';
  _Methodid   = 'proximitybeacon.namespaces.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TListNamespacesResponse) as TListNamespacesResponse;
end;


Function TNamespacesResource.List(AQuery : TNamespaceslistOptions) : TListNamespacesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=List(_Q);
end;

Function TNamespacesResource.Update(namespaceName: string; aNamespace : TNamespace; AQuery : string = '') : TNamespace;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v1beta1/{+namespaceName}';
  _Methodid   = 'proximitybeacon.namespaces.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['namespaceName',namespaceName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aNamespace,TNamespace) as TNamespace;
end;


Function TNamespacesResource.Update(namespaceName: string; aNamespace : TNamespace; AQuery : TNamespacesupdateOptions) : TNamespace;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'projectId',AQuery.projectId);
  Result:=Update(namespaceName,aNamespace,_Q);
end;



{ --------------------------------------------------------------------
  TV1beta1Resource
  --------------------------------------------------------------------}


Class Function TV1beta1Resource.ResourceName : String;

begin
  Result:='v1beta1';
end;

Class Function TV1beta1Resource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TproximitybeaconAPI;
end;

Function TV1beta1Resource.GetEidparams : TEphemeralIdRegistrationParams;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1beta1/eidparams';
  _Methodid   = 'proximitybeacon.getEidparams';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TEphemeralIdRegistrationParams) as TEphemeralIdRegistrationParams;
end;



{ --------------------------------------------------------------------
  TBeaconinfoResource
  --------------------------------------------------------------------}


Class Function TBeaconinfoResource.ResourceName : String;

begin
  Result:='beaconinfo';
end;

Class Function TBeaconinfoResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TproximitybeaconAPI;
end;

Function TBeaconinfoResource.Getforobserved(aGetInfoForObservedBeaconsRequest : TGetInfoForObservedBeaconsRequest) : TGetInfoForObservedBeaconsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta1/beaconinfo:getforobserved';
  _Methodid   = 'proximitybeacon.beaconinfo.getforobserved';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aGetInfoForObservedBeaconsRequest,TGetInfoForObservedBeaconsResponse) as TGetInfoForObservedBeaconsResponse;
end;



{ --------------------------------------------------------------------
  TProximitybeaconAPI
  --------------------------------------------------------------------}

Class Function TProximitybeaconAPI.APIName : String;

begin
  Result:='proximitybeacon';
end;

Class Function TProximitybeaconAPI.APIVersion : String;

begin
  Result:='v1beta1';
end;

Class Function TProximitybeaconAPI.APIRevision : String;

begin
  Result:='20160429';
end;

Class Function TProximitybeaconAPI.APIID : String;

begin
  Result:='proximitybeacon:v1beta1';
end;

Class Function TProximitybeaconAPI.APITitle : String;

begin
  Result:='Google Proximity Beacon API';
end;

Class Function TProximitybeaconAPI.APIDescription : String;

begin
  Result:='Registers, manages, indexes, and searches beacons.';
end;

Class Function TProximitybeaconAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TProximitybeaconAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TProximitybeaconAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TProximitybeaconAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TProximitybeaconAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/beacons/proximity/';
end;

Class Function TProximitybeaconAPI.APIrootUrl : string;

begin
  Result:='https://proximitybeacon.googleapis.com/';
end;

Class Function TProximitybeaconAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TProximitybeaconAPI.APIbaseURL : String;

begin
  Result:='https://proximitybeacon.googleapis.com/';
end;

Class Function TProximitybeaconAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TProximitybeaconAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TProximitybeaconAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TProximitybeaconAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,1);
  Result[0].Name:='https://www.googleapis.com/auth/userlocation.beacon.registry';
  Result[0].Description:='View and modify your beacons';
  
end;

Class Function TProximitybeaconAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TProximitybeaconAPI.RegisterAPIResources;

begin
  TBeaconTypeproperties.RegisterObject;
  TBeacon.RegisterObject;
  TAdvertisedId.RegisterObject;
  TLatLng.RegisterObject;
  TIndoorLevel.RegisterObject;
  TEphemeralIdRegistration.RegisterObject;
  TEmpty.RegisterObject;
  TListBeaconsResponse.RegisterObject;
  TBeaconAttachment.RegisterObject;
  TListBeaconAttachmentsResponse.RegisterObject;
  TDeleteAttachmentsResponse.RegisterObject;
  TListNamespacesResponse.RegisterObject;
  TNamespace.RegisterObject;
  TEphemeralIdRegistrationParams.RegisterObject;
  TListDiagnosticsResponse.RegisterObject;
  TDiagnostics.RegisterObject;
  TDate.RegisterObject;
  TGetInfoForObservedBeaconsRequest.RegisterObject;
  TObservation.RegisterObject;
  TGetInfoForObservedBeaconsResponse.RegisterObject;
  TBeaconInfo.RegisterObject;
  TAttachmentInfo.RegisterObject;
end;


Function TProximitybeaconAPI.GetBeaconsAttachmentsInstance : TBeaconsAttachmentsResource;

begin
  if (FBeaconsAttachmentsInstance=Nil) then
    FBeaconsAttachmentsInstance:=CreateBeaconsAttachmentsResource;
  Result:=FBeaconsAttachmentsInstance;
end;

Function TProximitybeaconAPI.CreateBeaconsAttachmentsResource : TBeaconsAttachmentsResource;

begin
  Result:=CreateBeaconsAttachmentsResource(Self);
end;


Function TProximitybeaconAPI.CreateBeaconsAttachmentsResource(AOwner : TComponent) : TBeaconsAttachmentsResource;

begin
  Result:=TBeaconsAttachmentsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProximitybeaconAPI.GetBeaconsDiagnosticsInstance : TBeaconsDiagnosticsResource;

begin
  if (FBeaconsDiagnosticsInstance=Nil) then
    FBeaconsDiagnosticsInstance:=CreateBeaconsDiagnosticsResource;
  Result:=FBeaconsDiagnosticsInstance;
end;

Function TProximitybeaconAPI.CreateBeaconsDiagnosticsResource : TBeaconsDiagnosticsResource;

begin
  Result:=CreateBeaconsDiagnosticsResource(Self);
end;


Function TProximitybeaconAPI.CreateBeaconsDiagnosticsResource(AOwner : TComponent) : TBeaconsDiagnosticsResource;

begin
  Result:=TBeaconsDiagnosticsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProximitybeaconAPI.GetBeaconsInstance : TBeaconsResource;

begin
  if (FBeaconsInstance=Nil) then
    FBeaconsInstance:=CreateBeaconsResource;
  Result:=FBeaconsInstance;
end;

Function TProximitybeaconAPI.CreateBeaconsResource : TBeaconsResource;

begin
  Result:=CreateBeaconsResource(Self);
end;


Function TProximitybeaconAPI.CreateBeaconsResource(AOwner : TComponent) : TBeaconsResource;

begin
  Result:=TBeaconsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProximitybeaconAPI.GetNamespacesInstance : TNamespacesResource;

begin
  if (FNamespacesInstance=Nil) then
    FNamespacesInstance:=CreateNamespacesResource;
  Result:=FNamespacesInstance;
end;

Function TProximitybeaconAPI.CreateNamespacesResource : TNamespacesResource;

begin
  Result:=CreateNamespacesResource(Self);
end;


Function TProximitybeaconAPI.CreateNamespacesResource(AOwner : TComponent) : TNamespacesResource;

begin
  Result:=TNamespacesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProximitybeaconAPI.GetV1beta1Instance : TV1beta1Resource;

begin
  if (FV1beta1Instance=Nil) then
    FV1beta1Instance:=CreateV1beta1Resource;
  Result:=FV1beta1Instance;
end;

Function TProximitybeaconAPI.CreateV1beta1Resource : TV1beta1Resource;

begin
  Result:=CreateV1beta1Resource(Self);
end;


Function TProximitybeaconAPI.CreateV1beta1Resource(AOwner : TComponent) : TV1beta1Resource;

begin
  Result:=TV1beta1Resource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TProximitybeaconAPI.GetBeaconinfoInstance : TBeaconinfoResource;

begin
  if (FBeaconinfoInstance=Nil) then
    FBeaconinfoInstance:=CreateBeaconinfoResource;
  Result:=FBeaconinfoInstance;
end;

Function TProximitybeaconAPI.CreateBeaconinfoResource : TBeaconinfoResource;

begin
  Result:=CreateBeaconinfoResource(Self);
end;


Function TProximitybeaconAPI.CreateBeaconinfoResource(AOwner : TComponent) : TBeaconinfoResource;

begin
  Result:=TBeaconinfoResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TProximitybeaconAPI.RegisterAPI;
end.
