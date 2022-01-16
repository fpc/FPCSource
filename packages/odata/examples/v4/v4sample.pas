unit v4sample;
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
  Aliases[0] : Microsoft.OData.SampleService.Models.TripPin=
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
  TCity = class;
  TCityArray = Array of TCity;
  TLocation = class;
  TLocationArray = Array of TLocation;
  TEventLocation = class;
  TEventLocationArray = Array of TEventLocation;
  TAirportLocation = class;
  TAirportLocationArray = Array of TAirportLocation;
  TPhoto = class;
  TPhotoArray = Array of TPhoto;
  TPerson = class;
  TPersonArray = Array of TPerson;
  TAirline = class;
  TAirlineArray = Array of TAirline;
  TAirport = class;
  TAirportArray = Array of TAirport;
  TPlanItem = class;
  TPlanItemArray = Array of TPlanItem;
  TPublicTransportation = class;
  TPublicTransportationArray = Array of TPublicTransportation;
  TFlight = class;
  TFlightArray = Array of TFlight;
  TEvent = class;
  TEventArray = Array of TEvent;
  TTrip = class;
  TTripArray = Array of TTrip;
  TDefaultContainer = class;
  TDefaultContainerArray = Array of TDefaultContainer;
  TPhotosEntitySet = class;
  TPhotosEntitySetArray = Array of TPhotosEntitySet;
  TPeopleEntitySet = class;
  TPeopleEntitySetArray = Array of TPeopleEntitySet;
  TAirlinesEntitySet = class;
  TAirlinesEntitySetArray = Array of TAirlinesEntitySet;
  TAirportsEntitySet = class;
  TAirportsEntitySetArray = Array of TAirportsEntitySet;
  TTripImplicitEntitySet = class;
  TTripImplicitEntitySetArray = Array of TTripImplicitEntitySet;
  TPlanItemImplicitEntitySet = class;
  TPlanItemImplicitEntitySetArray = Array of TPlanItemImplicitEntitySet;
  TService = class;
  TServiceArray = Array of TService;
  //
  
  // Enumerations
  
  {$SCOPEDENUMS ON}
  TPersonGender = (Male,Female,Unknown);
  TPersonGenderArray = Array of TPersonGender;
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: City
    --------------------------------------------------------------------}
  
  TCity = Class(TODataEntity)
  private
    FCountryRegion : string;
    FName : string;
    FRegion : string;
    procedure SetCountryRegion(AIndex: Integer; const AValue: string);
    procedure SetName(AIndex: Integer; const AValue: string);
    procedure SetRegion(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property CountryRegion : string index 0 read FCountryRegion write SetCountryRegion;
    Property Name : string index 8 read FName write SetName;
    Property Region : string index 16 read FRegion write SetRegion;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: Location
    --------------------------------------------------------------------}
  
  TLocation = Class(TODataEntity)
  private
    FAddress : string;
    FCity : TCity;
    procedure SetAddress(AIndex: Integer; const AValue: string);
    procedure SetCity(AIndex: Integer; const AValue: TCity);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property Address : string index 0 read FAddress write SetAddress;
    Property City : TCity index 8 read FCity write SetCity;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: EventLocation
    --------------------------------------------------------------------}
  
  TEventLocation = Class(TLocation)
  private
    FBuildingInfo : string;
    procedure SetBuildingInfo(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property BuildingInfo : string index 16 read FBuildingInfo write SetBuildingInfo;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: AirportLocation
    --------------------------------------------------------------------}
  
  TAirportLocation = Class(TLocation)
  private
    FLoc : TGeographyPoint;
    procedure SetLoc(AIndex: Integer; const AValue: TGeographyPoint);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property Loc : TGeographyPoint index 16 read FLoc write SetLoc;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: Photo
    --------------------------------------------------------------------}
  
  TPhoto = Class(TODataEntity)
  private
    FId : int64;
    FName : string;
    procedure SetId(AIndex: Integer; const AValue: int64);
    procedure SetName(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
    function KeyAsURLPart : String;  Override;
    procedure GetStream(AService: TODataService; AContentType: String; 
                   AStream: TStream);
    procedure SetStream(AService: TODataService; AContentType: String; 
                   AStream: TStream);
  published
    Property Id : int64 index 0 read FId write SetId;
    Property Name : string index 8 read FName write SetName;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: Person
    --------------------------------------------------------------------}
  
  TPerson = Class(TODataEntity)
  private
    FUserName : string;
    FFirstName : string;
    FLastName : string;
    FEmails : TStringArray;
    FAddressInfo : TLocationArray;
    FGender : TPersonGender;
    FConcurrency : int64;
    procedure SetUserName(AIndex: Integer; const AValue: string);
    procedure SetFirstName(AIndex: Integer; const AValue: string);
    procedure SetLastName(AIndex: Integer; const AValue: string);
    procedure SetEmails(AIndex: Integer; const AValue: TStringArray);
    procedure SetAddressInfo(AIndex: Integer; const AValue: TLocationArray);
    procedure SetGender(AIndex: Integer; const AValue: TPersonGender);
    procedure SetConcurrency(AIndex: Integer; const AValue: int64);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    function GetFavoriteAirline(AService: TODataService) : TAirline;
    function GetFriendsTrips(AService: TODataService; userName: string)
                         : TTripArray;
    procedure ShareTrip(AService: TODataService; userName: string; 
                   tripId: TInt32);
    class function ObjectRestKind : String;  Override;
    function KeyAsURLPart : String;  Override;
    function Friends(AService: TODataService) : TPeopleEntitySet;
    function Trips(AService: TODataService) : TTripImplicitEntitySet;
    function Photo(AService: TODataService) : TPhoto;
  published
    Property UserName : string index 0 read FUserName write SetUserName;
    Property FirstName : string index 8 read FFirstName write SetFirstName;
    Property LastName : string index 16 read FLastName write SetLastName;
    Property Emails : TStringArray index 24 read FEmails write SetEmails;
    Property AddressInfo : TLocationArray index 32 read FAddressInfo write SetAddressInfo;
    Property Gender : TPersonGender index 40 read FGender write SetGender;
    Property Concurrency : int64 index 48 read FConcurrency write SetConcurrency;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: Airline
    --------------------------------------------------------------------}
  
  TAirline = Class(TODataEntity)
  private
    FAirlineCode : string;
    FName : string;
    procedure SetAirlineCode(AIndex: Integer; const AValue: string);
    procedure SetName(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
    function KeyAsURLPart : String;  Override;
  published
    Property AirlineCode : string index 0 read FAirlineCode write SetAirlineCode;
    Property Name : string index 8 read FName write SetName;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: Airport
    --------------------------------------------------------------------}
  
  TAirport = Class(TODataEntity)
  private
    FIcaoCode : string;
    FName : string;
    FIataCode : string;
    FLocation : TAirportLocation;
    procedure SetIcaoCode(AIndex: Integer; const AValue: string);
    procedure SetName(AIndex: Integer; const AValue: string);
    procedure SetIataCode(AIndex: Integer; const AValue: string);
    procedure SetLocation(AIndex: Integer; const AValue: TAirportLocation);
  public
    class function ObjectRestKind : String;  Override;
    function KeyAsURLPart : String;  Override;
  published
    Property IcaoCode : string index 0 read FIcaoCode write SetIcaoCode;
    Property Name : string index 8 read FName write SetName;
    Property IataCode : string index 16 read FIataCode write SetIataCode;
    Property Location : TAirportLocation index 24 read FLocation write SetLocation;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: PlanItem
    --------------------------------------------------------------------}
  
  TPlanItem = Class(TODataEntity)
  private
    FPlanItemId : TInt32;
    FConfirmationCode : string;
    FStartsAt : TDateTime;
    FEndsAt : TDateTime;
    FDuration : TDuration;
    procedure SetPlanItemId(AIndex: Integer; const AValue: TInt32);
    procedure SetConfirmationCode(AIndex: Integer; const AValue: string);
    procedure SetStartsAt(AIndex: Integer; const AValue: TDateTime);
    procedure SetEndsAt(AIndex: Integer; const AValue: TDateTime);
    procedure SetDuration(AIndex: Integer; const AValue: TDuration);
  public
    class function ObjectRestKind : String;  Override;
    function KeyAsURLPart : String;  Override;
  published
    Property PlanItemId : TInt32 index 0 read FPlanItemId write SetPlanItemId;
    Property ConfirmationCode : string index 8 read FConfirmationCode write SetConfirmationCode;
    Property StartsAt : TDateTime index 16 read FStartsAt write SetStartsAt;
    Property EndsAt : TDateTime index 24 read FEndsAt write SetEndsAt;
    Property Duration : TDuration index 32 read FDuration write SetDuration;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: PublicTransportation
    --------------------------------------------------------------------}
  
  TPublicTransportation = Class(TPlanItem)
  private
    FSeatNumber : string;
    procedure SetSeatNumber(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property SeatNumber : string index 40 read FSeatNumber write SetSeatNumber;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: Flight
    --------------------------------------------------------------------}
  
  TFlight = Class(TPublicTransportation)
  private
    FFlightNumber : string;
    procedure SetFlightNumber(AIndex: Integer; const AValue: string);
  public
    class function ObjectRestKind : String;  Override;
    function From(AService: TODataService) : TAirport;
    function _To(AService: TODataService) : TAirport;
    function Airline(AService: TODataService) : TAirline;
  published
    Property FlightNumber : string index 48 read FFlightNumber write SetFlightNumber;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: Event
    --------------------------------------------------------------------}
  
  TEvent = Class(TPlanItem)
  private
    FDescription : string;
    FOccursAt : TEventLocation;
    procedure SetDescription(AIndex: Integer; const AValue: string);
    procedure SetOccursAt(AIndex: Integer; const AValue: TEventLocation);
  public
    class function ObjectRestKind : String;  Override;
  published
    Property Description : string index 40 read FDescription write SetDescription;
    Property OccursAt : TEventLocation index 48 read FOccursAt write SetOccursAt;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: Trip
    --------------------------------------------------------------------}
  
  TTrip = Class(TODataEntity)
  private
    FTripId : TInt32;
    FShareId : TGUIDString;
    FDescription : string;
    FName : string;
    FBudget : Single;
    FStartsAt : TDateTime;
    FEndsAt : TDateTime;
    FTags : TStringArray;
    procedure SetTripId(AIndex: Integer; const AValue: TInt32);
    procedure SetShareId(AIndex: Integer; const AValue: TGUIDString);
    procedure SetDescription(AIndex: Integer; const AValue: string);
    procedure SetName(AIndex: Integer; const AValue: string);
    procedure SetBudget(AIndex: Integer; const AValue: Single);
    procedure SetStartsAt(AIndex: Integer; const AValue: TDateTime);
    procedure SetEndsAt(AIndex: Integer; const AValue: TDateTime);
    procedure SetTags(AIndex: Integer; const AValue: TStringArray);
  protected
    {$IFDEF VER2_6}
    procedure SetArrayLength(const AName: String; ALength: Longint)
                        ;  Override;
    {$ENDIF VER2_6}
  public
    function GetInvolvedPeople(AService: TODataService) : TPersonArray;
    class function ObjectRestKind : String;  Override;
    function KeyAsURLPart : String;  Override;
    function Photos(AService: TODataService) : TPhotosEntitySet;
    function PlanItems(AService: TODataService) : TPlanItemImplicitEntitySet;
  published
    Property TripId : TInt32 index 0 read FTripId write SetTripId;
    Property ShareId : TGUIDString index 8 read FShareId write SetShareId;
    Property Description : string index 16 read FDescription write SetDescription;
    Property Name : string index 24 read FName write SetName;
    Property Budget : Single index 32 read FBudget write SetBudget;
    Property StartsAt : TDateTime index 40 read FStartsAt write SetStartsAt;
    Property EndsAt : TDateTime index 48 read FEndsAt write SetEndsAt;
    Property Tags : TStringArray index 56 read FTags write SetTags;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: DefaultContainer
    --------------------------------------------------------------------}
  
  TDefaultContainer = Class(TODataEntityContainer)
  private
    FPhotos : TPhotosEntitySet;
    FPeople : TPeopleEntitySet;
    FAirlines : TAirlinesEntitySet;
    FAirports : TAirportsEntitySet;
    FMe : TPerson;
    function GetPhotos : TPhotosEntitySet;
    function GetPeople : TPeopleEntitySet;
    function GetAirlines : TAirlinesEntitySet;
    function GetAirports : TAirportsEntitySet;
    function GetMe : TPerson;
  public
    class function ObjectRestKind : String;  Override;
    function CreateNewPhotos : TPhotosEntitySet;
    function CreateNewPeople : TPeopleEntitySet;
    function CreateNewAirlines : TAirlinesEntitySet;
    function CreateNewAirports : TAirportsEntitySet;
    function FetchMe : TPerson;
    procedure ResetDataSource;
    function GetNearestAirport(lat: Double; lon: Double) : TAirport;
  published
    Property Photos : TPhotosEntitySet read GetPhotos;
    Property People : TPeopleEntitySet read GetPeople;
    Property Airlines : TAirlinesEntitySet read GetAirlines;
    Property Airports : TAirportsEntitySet read GetAirports;
    Property Me : TPerson read GetMe;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: Photos
    --------------------------------------------------------------------}
  
  TPhotosEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const Id: int64) : TPhoto;
    function List(const AQuery: String; out NextLink: String) : TPhotoArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TPhotoArray;
    function ListAll(const AQuery: String) : TPhotoArray;
    function ListAll(const AQuery: TQueryParams) : TPhotoArray;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: People
    --------------------------------------------------------------------}
  
  TPeopleEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const UserName: string) : TPerson;
    function List(const AQuery: String; out NextLink: String) : TPersonArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TPersonArray;
    function ListAll(const AQuery: String) : TPersonArray;
    function ListAll(const AQuery: TQueryParams) : TPersonArray;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: Airlines
    --------------------------------------------------------------------}
  
  TAirlinesEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const AirlineCode: string) : TAirline;
    function List(const AQuery: String; out NextLink: String)
              : TAirlineArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TAirlineArray;
    function ListAll(const AQuery: String) : TAirlineArray;
    function ListAll(const AQuery: TQueryParams) : TAirlineArray;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: Airports
    --------------------------------------------------------------------}
  
  TAirportsEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const IcaoCode: string) : TAirport;
    function List(const AQuery: String; out NextLink: String)
              : TAirportArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TAirportArray;
    function ListAll(const AQuery: String) : TAirportArray;
    function ListAll(const AQuery: TQueryParams) : TAirportArray;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: TripImplicitEntitySet
    --------------------------------------------------------------------}
  
  TTripImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const TripId: TInt32) : TTrip;
    function List(const AQuery: String; out NextLink: String) : TTripArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TTripArray;
    function ListAll(const AQuery: String) : TTripArray;
    function ListAll(const AQuery: TQueryParams) : TTripArray;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: PlanItemImplicitEntitySet
    --------------------------------------------------------------------}
  
  TPlanItemImplicitEntitySet = Class(TODataEntitySet)
  public
    class function ObjectRestKind : String;  Override;
    class function EntityClass : TODataEntityClass;  Override;
    function Get(const PlanItemId: TInt32) : TPlanItem;
    function List(const AQuery: String; out NextLink: String)
              : TPlanItemArray;
    function List(const AQuery: TQueryParams; out NextLink: String)
              : TPlanItemArray;
    function ListAll(const AQuery: String) : TPlanItemArray;
    function ListAll(const AQuery: TQueryParams) : TPlanItemArray;
  end;
  
  
  { --------------------------------------------------------------------
    Microsoft.OData.SampleService.Models.TripPin: Microsoft.OData.SampleService.Models.TripPin
    --------------------------------------------------------------------}
  
  TService = Class(TODataService)
  private
    FDefaultContainer : TDefaultContainer;
    function GetDefaultContainer : TDefaultContainer;
  public
    class function ObjectRestKind : String;  Override;
    function CreateNewDefaultContainer : TDefaultContainer;
  published
    Property DefaultContainer : TDefaultContainer read GetDefaultContainer;
  end;
  

implementation


{ --------------------------------------------------------------------
  TCity
  --------------------------------------------------------------------}


Class Function TCity.ObjectRestKind : String; 

begin
  Result:='City';
end;


Procedure TCity.SetCountryRegion(AIndex: Integer; const AValue: string); 


begin
  If (FCountryRegion=AValue) then exit;
  FCountryRegion:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TCity.SetName(AIndex: Integer; const AValue: string); 


begin
  If (FName=AValue) then exit;
  FName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TCity.SetRegion(AIndex: Integer; const AValue: string); 


begin
  If (FRegion=AValue) then exit;
  FRegion:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TLocation
  --------------------------------------------------------------------}


Class Function TLocation.ObjectRestKind : String; 

begin
  Result:='Location';
end;


Procedure TLocation.SetAddress(AIndex: Integer; const AValue: string); 


begin
  If (FAddress=AValue) then exit;
  FAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TLocation.SetCity(AIndex: Integer; const AValue: TCity); 


begin
  If (FCity=AValue) then exit;
  FCity:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TEventLocation
  --------------------------------------------------------------------}


Class Function TEventLocation.ObjectRestKind : String; 

begin
  Result:='EventLocation';
end;


Procedure TEventLocation.SetBuildingInfo(AIndex: Integer; const AValue: string); 


begin
  If (FBuildingInfo=AValue) then exit;
  FBuildingInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TAirportLocation
  --------------------------------------------------------------------}


Class Function TAirportLocation.ObjectRestKind : String; 

begin
  Result:='AirportLocation';
end;


Procedure TAirportLocation.SetLoc(AIndex: Integer; const AValue: TGeographyPoint); 


begin
  If (FLoc=AValue) then exit;
  FLoc:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TPhoto
  --------------------------------------------------------------------}


Class Function TPhoto.ObjectRestKind : String; 

begin
  Result:='Photo';
end;


Procedure TPhoto.SetId(AIndex: Integer; const AValue: int64); 


begin
  If (FId=AValue) then exit;
  FId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TPhoto.SetName(AIndex: Integer; const AValue: string); 


begin
  If (FName=AValue) then exit;
  FName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Function TPhoto.KeyAsURLPart : string;

begin
  Result:=IntToStr(Id);
end;


Procedure TPhoto.GetStream(AService: TODataService; AContentType: String; AStream: TStream); 


begin
  DoGetStream(AService,AContentType,AStream);
end;


Procedure TPhoto.SetStream(AService: TODataService; AContentType: String; AStream: TStream); 


begin
  DoSetStream(AService,AContentType,AStream);
end;


{ --------------------------------------------------------------------
  TPerson
  --------------------------------------------------------------------}


Function TPerson.GetFavoriteAirline(AService: TODataService) : TAirline; 

Var
  _Res : String;
  _Path : String;
begin
  _Path:='('+_Path+')';
  _Path:='Microsoft.OData.SampleService.Models.TripPin.GetFavoriteAirline'+_Path;
  _Path:=BaseURL(AService)+'/'+_Path;
  Result:=TAirline(AService.SingleServiceCall(_Path,'',TAirline));
end;


Function TPerson.GetFriendsTrips(AService: TODataService; userName: string) : TTripArray; 

Var
  _Res : String;
  _Path : String;
begin
  _Path:='userName='+TODataObject.MakeKeyString(userName);
  _Path:='('+_Path+')';
  _Path:='Microsoft.OData.SampleService.Models.TripPin.GetFriendsTrips'+_Path;
  _Path:=BaseURL(AService)+'/'+_Path;
  Result:=TTripArray(AService.GetMulti(_Path,'',TTrip,True,_Res));
end;


Procedure TPerson.ShareTrip(AService: TODataService; userName: string; tripId: TInt32); 

Var
  _JSON : TJSONObject;
  _data : String;
  _Path : String;
begin
  _JSON:=TJSONObject.Create;
  try
    _JSON.Add('userName',userName);
    _JSON.Add('tripId',tripId);
    _data:=_JSON.AsJSON;
  finally
    FreeAndNil(_JSON);
  end;
  _Path:=BaseURL(AService)+'/Microsoft.OData.SampleService.Models.TripPin.ShareTrip';
  AService.ServiceCall('POST',_Path,'',_Data);
end;


Class Function TPerson.ObjectRestKind : String; 

begin
  Result:='Person';
end;


Procedure TPerson.SetUserName(AIndex: Integer; const AValue: string); 


begin
  If (FUserName=AValue) then exit;
  FUserName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TPerson.SetFirstName(AIndex: Integer; const AValue: string); 


begin
  If (FFirstName=AValue) then exit;
  FFirstName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TPerson.SetLastName(AIndex: Integer; const AValue: string); 


begin
  If (FLastName=AValue) then exit;
  FLastName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TPerson.SetEmails(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FEmails=AValue) then exit;
  FEmails:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TPerson.SetAddressInfo(AIndex: Integer; const AValue: TLocationArray); 


begin
  If (FAddressInfo=AValue) then exit;
  FAddressInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TPerson.SetGender(AIndex: Integer; const AValue: TPersonGender); 


begin
  If (FGender=AValue) then exit;
  FGender:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TPerson.SetConcurrency(AIndex: Integer; const AValue: int64); 


begin
  If (FConcurrency=AValue) then exit;
  FConcurrency:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure TPerson.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'emails' : SetLength(FEmails,aLength);
  'addressinfo' : SetLength(FAddressInfo,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


Function TPerson.KeyAsURLPart : string;

begin
  Result:=TODataObject.MakeKeyString(UserName);
end;


Function TPerson.Friends(AService: TODataService) : TPeopleEntitySet; 


begin
  Result:=TPeopleEntitySet(CreateContainedEntitySet(AService,'Friends', TPeopleEntitySet));
end;


Function TPerson.Trips(AService: TODataService) : TTripImplicitEntitySet; 


begin
  Result:=TTripImplicitEntitySet(CreateContainedEntitySet(AService,'Trips', TTripImplicitEntitySet));
end;


Function TPerson.Photo(AService: TODataService) : TPhoto; 


begin
  Result:=TPhoto(GetContainedSingleTon(AService,'Photo', TPhoto));
end;


{ --------------------------------------------------------------------
  TAirline
  --------------------------------------------------------------------}


Class Function TAirline.ObjectRestKind : String; 

begin
  Result:='Airline';
end;


Procedure TAirline.SetAirlineCode(AIndex: Integer; const AValue: string); 


begin
  If (FAirlineCode=AValue) then exit;
  FAirlineCode:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TAirline.SetName(AIndex: Integer; const AValue: string); 


begin
  If (FName=AValue) then exit;
  FName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Function TAirline.KeyAsURLPart : string;

begin
  Result:=TODataObject.MakeKeyString(AirlineCode);
end;


{ --------------------------------------------------------------------
  TAirport
  --------------------------------------------------------------------}


Class Function TAirport.ObjectRestKind : String; 

begin
  Result:='Airport';
end;


Procedure TAirport.SetIcaoCode(AIndex: Integer; const AValue: string); 


begin
  If (FIcaoCode=AValue) then exit;
  FIcaoCode:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TAirport.SetName(AIndex: Integer; const AValue: string); 


begin
  If (FName=AValue) then exit;
  FName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TAirport.SetIataCode(AIndex: Integer; const AValue: string); 


begin
  If (FIataCode=AValue) then exit;
  FIataCode:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TAirport.SetLocation(AIndex: Integer; const AValue: TAirportLocation); 


begin
  If (FLocation=AValue) then exit;
  FLocation:=AValue;
  MarkPropertyChanged(AIndex);
end;


Function TAirport.KeyAsURLPart : string;

begin
  Result:=TODataObject.MakeKeyString(IcaoCode);
end;


{ --------------------------------------------------------------------
  TPlanItem
  --------------------------------------------------------------------}


Class Function TPlanItem.ObjectRestKind : String; 

begin
  Result:='PlanItem';
end;


Procedure TPlanItem.SetPlanItemId(AIndex: Integer; const AValue: TInt32); 


begin
  If (FPlanItemId=AValue) then exit;
  FPlanItemId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TPlanItem.SetConfirmationCode(AIndex: Integer; const AValue: string); 


begin
  If (FConfirmationCode=AValue) then exit;
  FConfirmationCode:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TPlanItem.SetStartsAt(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FStartsAt=AValue) then exit;
  FStartsAt:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TPlanItem.SetEndsAt(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FEndsAt=AValue) then exit;
  FEndsAt:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TPlanItem.SetDuration(AIndex: Integer; const AValue: TDuration); 


begin
  If (FDuration=AValue) then exit;
  FDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;


Function TPlanItem.KeyAsURLPart : string;

begin
  Result:=IntToStr(PlanItemId);
end;


{ --------------------------------------------------------------------
  TPublicTransportation
  --------------------------------------------------------------------}


Class Function TPublicTransportation.ObjectRestKind : String; 

begin
  Result:='PublicTransportation';
end;


Procedure TPublicTransportation.SetSeatNumber(AIndex: Integer; const AValue: string); 


begin
  If (FSeatNumber=AValue) then exit;
  FSeatNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TFlight
  --------------------------------------------------------------------}


Class Function TFlight.ObjectRestKind : String; 

begin
  Result:='Flight';
end;


Procedure TFlight.SetFlightNumber(AIndex: Integer; const AValue: string); 


begin
  If (FFlightNumber=AValue) then exit;
  FFlightNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;


Function TFlight.From(AService: TODataService) : TAirport; 


begin
  Result:=TAirport(GetContainedSingleTon(AService,'From', TAirport));
end;


Function TFlight._To(AService: TODataService) : TAirport; 


begin
  Result:=TAirport(GetContainedSingleTon(AService,'To', TAirport));
end;


Function TFlight.Airline(AService: TODataService) : TAirline; 


begin
  Result:=TAirline(GetContainedSingleTon(AService,'Airline', TAirline));
end;


{ --------------------------------------------------------------------
  TEvent
  --------------------------------------------------------------------}


Class Function TEvent.ObjectRestKind : String; 

begin
  Result:='Event';
end;


Procedure TEvent.SetDescription(AIndex: Integer; const AValue: string); 


begin
  If (FDescription=AValue) then exit;
  FDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TEvent.SetOccursAt(AIndex: Integer; const AValue: TEventLocation); 


begin
  If (FOccursAt=AValue) then exit;
  FOccursAt:=AValue;
  MarkPropertyChanged(AIndex);
end;


{ --------------------------------------------------------------------
  TTrip
  --------------------------------------------------------------------}


Function TTrip.GetInvolvedPeople(AService: TODataService) : TPersonArray; 

Var
  _Res : String;
  _Path : String;
begin
  _Path:='('+_Path+')';
  _Path:='Microsoft.OData.SampleService.Models.TripPin.GetInvolvedPeople'+_Path;
  _Path:=BaseURL(AService)+'/'+_Path;
  Result:=TPersonArray(AService.GetMulti(_Path,'',TPerson,True,_Res));
end;


Class Function TTrip.ObjectRestKind : String; 

begin
  Result:='Trip';
end;


Procedure TTrip.SetTripId(AIndex: Integer; const AValue: TInt32); 


begin
  If (FTripId=AValue) then exit;
  FTripId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TTrip.SetShareId(AIndex: Integer; const AValue: TGUIDString); 


begin
  If (FShareId=AValue) then exit;
  FShareId:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TTrip.SetDescription(AIndex: Integer; const AValue: string); 


begin
  If (FDescription=AValue) then exit;
  FDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TTrip.SetName(AIndex: Integer; const AValue: string); 


begin
  If (FName=AValue) then exit;
  FName:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TTrip.SetBudget(AIndex: Integer; const AValue: Single); 


begin
  If (FBudget=AValue) then exit;
  FBudget:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TTrip.SetStartsAt(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FStartsAt=AValue) then exit;
  FStartsAt:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TTrip.SetEndsAt(AIndex: Integer; const AValue: TDateTime); 


begin
  If (FEndsAt=AValue) then exit;
  FEndsAt:=AValue;
  MarkPropertyChanged(AIndex);
end;


Procedure TTrip.SetTags(AIndex: Integer; const AValue: TStringArray); 


begin
  If (FTags=AValue) then exit;
  FTags:=AValue;
  MarkPropertyChanged(AIndex);
end;

{$IFDEF VER2_6}

Procedure TTrip.SetArrayLength(const AName: String; ALength: Longint); 

begin
  Case aName of
  'tags' : SetLength(FTags,aLength);
  else
    inherited SetArrayLength(aName,ALength);
  end;
end;
{$ENDIF VER2_6}


Function TTrip.KeyAsURLPart : string;

begin
  Result:=IntToStr(TripId);
end;


Function TTrip.Photos(AService: TODataService) : TPhotosEntitySet; 


begin
  Result:=TPhotosEntitySet(CreateContainedEntitySet(AService,'Photos', TPhotosEntitySet));
end;


Function TTrip.PlanItems(AService: TODataService) : TPlanItemImplicitEntitySet; 


begin
  Result:=TPlanItemImplicitEntitySet(CreateContainedEntitySet(AService,'PlanItems', TPlanItemImplicitEntitySet));
end;


{ --------------------------------------------------------------------
  TDefaultContainer
  --------------------------------------------------------------------}


Class Function TDefaultContainer.ObjectRestKind : String; 

begin
  Result:='DefaultContainer';
end;

Function TDefaultContainer.CreateNewPhotos : TPhotosEntitySet; 

begin
  Result:=TPhotosEntitySet(CreateEntitySet(TPhotosEntitySet));
end;


Function TDefaultContainer.GetPhotos : TPhotosEntitySet; 


begin
  If Not Assigned(FPhotos) then
    FPhotos:=TPhotosEntitySet(CreateEntitySet(TPhotosEntitySet));
  Result:=FPhotos;
end;

Function TDefaultContainer.CreateNewPeople : TPeopleEntitySet; 

begin
  Result:=TPeopleEntitySet(CreateEntitySet(TPeopleEntitySet));
end;


Function TDefaultContainer.GetPeople : TPeopleEntitySet; 


begin
  If Not Assigned(FPeople) then
    FPeople:=TPeopleEntitySet(CreateEntitySet(TPeopleEntitySet));
  Result:=FPeople;
end;

Function TDefaultContainer.CreateNewAirlines : TAirlinesEntitySet; 

begin
  Result:=TAirlinesEntitySet(CreateEntitySet(TAirlinesEntitySet));
end;


Function TDefaultContainer.GetAirlines : TAirlinesEntitySet; 


begin
  If Not Assigned(FAirlines) then
    FAirlines:=TAirlinesEntitySet(CreateEntitySet(TAirlinesEntitySet));
  Result:=FAirlines;
end;

Function TDefaultContainer.CreateNewAirports : TAirportsEntitySet; 

begin
  Result:=TAirportsEntitySet(CreateEntitySet(TAirportsEntitySet));
end;


Function TDefaultContainer.GetAirports : TAirportsEntitySet; 


begin
  If Not Assigned(FAirports) then
    FAirports:=TAirportsEntitySet(CreateEntitySet(TAirportsEntitySet));
  Result:=FAirports;
end;


Function TDefaultContainer.FetchMe : TPerson; 

begin
  CheckService;
  Result:=TPerson(Service.SingleServiceCall('Me','',TPerson));
  Result.BasePath:='Me';
end;


Function TDefaultContainer.GetMe : TPerson; 


begin
  If Not Assigned(FMe) then
    FMe:=FetchMe;
  Result:=FMe;
end;


Procedure TDefaultContainer.ResetDataSource; 

Var
  _data : String;
  _Path : String;
begin
  CheckService;
  _data:='';
  _Path:='/ResetDataSource';
  Service.ServiceCall('POST',_Path,'',_Data);
end;


Function TDefaultContainer.GetNearestAirport(lat: Double; lon: Double) : TAirport; 

Var
  _Res : String;
  _Path : String;
begin
  CheckService;
  _Path:='lat='+FloatToStr(lat);
  _Path:=_Path+','+'lon='+FloatToStr(lon);
  _Path:='('+_Path+')';
  _Path:='GetNearestAirport'+_Path;
  Result:=TAirport(Service.SingleServiceCall(_Path,'',TAirport));
end;


{ --------------------------------------------------------------------
  TPhotosEntitySet
  --------------------------------------------------------------------}


Class Function TPhotosEntitySet.ObjectRestKind : String; 

begin
  Result:='Photos';
end;

Class Function TPhotosEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TPhoto;
end;


Function TPhotosEntitySet.Get(const Id: int64) : TPhoto; 


begin
  Result:=TPhoto(GetSingle(IntToStr(Id)));
end;


Function TPhotosEntitySet.List(const AQuery: String; out NextLink: String) : TPhotoArray; 


begin
  Result:=TPhotoArray(GetMulti(AQuery,False,NextLink));
end;


Function TPhotosEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TPhotoArray; 


begin
  Result:=TPhotoArray(GetMulti(AQuery,False,NextLink));
end;


Function TPhotosEntitySet.ListAll(const AQuery: String) : TPhotoArray; 

var N : String;

begin
  Result:=TPhotoArray(GetMulti(AQuery,True,N));
end;


Function TPhotosEntitySet.ListAll(const AQuery: TQueryParams) : TPhotoArray; 

var N : String;

begin
  Result:=TPhotoArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TPeopleEntitySet
  --------------------------------------------------------------------}


Class Function TPeopleEntitySet.ObjectRestKind : String; 

begin
  Result:='People';
end;

Class Function TPeopleEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TPerson;
end;


Function TPeopleEntitySet.Get(const UserName: string) : TPerson; 


begin
  Result:=TPerson(GetSingle(TODataObject.MakeKeyString(UserName)));
end;


Function TPeopleEntitySet.List(const AQuery: String; out NextLink: String) : TPersonArray; 


begin
  Result:=TPersonArray(GetMulti(AQuery,False,NextLink));
end;


Function TPeopleEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TPersonArray; 


begin
  Result:=TPersonArray(GetMulti(AQuery,False,NextLink));
end;


Function TPeopleEntitySet.ListAll(const AQuery: String) : TPersonArray; 

var N : String;

begin
  Result:=TPersonArray(GetMulti(AQuery,True,N));
end;


Function TPeopleEntitySet.ListAll(const AQuery: TQueryParams) : TPersonArray; 

var N : String;

begin
  Result:=TPersonArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TAirlinesEntitySet
  --------------------------------------------------------------------}


Class Function TAirlinesEntitySet.ObjectRestKind : String; 

begin
  Result:='Airlines';
end;

Class Function TAirlinesEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TAirline;
end;


Function TAirlinesEntitySet.Get(const AirlineCode: string) : TAirline; 


begin
  Result:=TAirline(GetSingle(TODataObject.MakeKeyString(AirlineCode)));
end;


Function TAirlinesEntitySet.List(const AQuery: String; out NextLink: String) : TAirlineArray; 


begin
  Result:=TAirlineArray(GetMulti(AQuery,False,NextLink));
end;


Function TAirlinesEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TAirlineArray; 


begin
  Result:=TAirlineArray(GetMulti(AQuery,False,NextLink));
end;


Function TAirlinesEntitySet.ListAll(const AQuery: String) : TAirlineArray; 

var N : String;

begin
  Result:=TAirlineArray(GetMulti(AQuery,True,N));
end;


Function TAirlinesEntitySet.ListAll(const AQuery: TQueryParams) : TAirlineArray; 

var N : String;

begin
  Result:=TAirlineArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TAirportsEntitySet
  --------------------------------------------------------------------}


Class Function TAirportsEntitySet.ObjectRestKind : String; 

begin
  Result:='Airports';
end;

Class Function TAirportsEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TAirport;
end;


Function TAirportsEntitySet.Get(const IcaoCode: string) : TAirport; 


begin
  Result:=TAirport(GetSingle(TODataObject.MakeKeyString(IcaoCode)));
end;


Function TAirportsEntitySet.List(const AQuery: String; out NextLink: String) : TAirportArray; 


begin
  Result:=TAirportArray(GetMulti(AQuery,False,NextLink));
end;


Function TAirportsEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TAirportArray; 


begin
  Result:=TAirportArray(GetMulti(AQuery,False,NextLink));
end;


Function TAirportsEntitySet.ListAll(const AQuery: String) : TAirportArray; 

var N : String;

begin
  Result:=TAirportArray(GetMulti(AQuery,True,N));
end;


Function TAirportsEntitySet.ListAll(const AQuery: TQueryParams) : TAirportArray; 

var N : String;

begin
  Result:=TAirportArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TTripImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TTripImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='TripImplicitEntitySet';
end;

Class Function TTripImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TTrip;
end;


Function TTripImplicitEntitySet.Get(const TripId: TInt32) : TTrip; 


begin
  Result:=TTrip(GetSingle(IntToStr(TripId)));
end;


Function TTripImplicitEntitySet.List(const AQuery: String; out NextLink: String) : TTripArray; 


begin
  Result:=TTripArray(GetMulti(AQuery,False,NextLink));
end;


Function TTripImplicitEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TTripArray; 


begin
  Result:=TTripArray(GetMulti(AQuery,False,NextLink));
end;


Function TTripImplicitEntitySet.ListAll(const AQuery: String) : TTripArray; 

var N : String;

begin
  Result:=TTripArray(GetMulti(AQuery,True,N));
end;


Function TTripImplicitEntitySet.ListAll(const AQuery: TQueryParams) : TTripArray; 

var N : String;

begin
  Result:=TTripArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TPlanItemImplicitEntitySet
  --------------------------------------------------------------------}


Class Function TPlanItemImplicitEntitySet.ObjectRestKind : String; 

begin
  Result:='PlanItemImplicitEntitySet';
end;

Class Function TPlanItemImplicitEntitySet.EntityClass : TODataEntityClass; 

begin
  Result:=TPlanItem;
end;


Function TPlanItemImplicitEntitySet.Get(const PlanItemId: TInt32) : TPlanItem; 


begin
  Result:=TPlanItem(GetSingle(IntToStr(PlanItemId)));
end;


Function TPlanItemImplicitEntitySet.List(const AQuery: String; out NextLink: String) : TPlanItemArray; 


begin
  Result:=TPlanItemArray(GetMulti(AQuery,False,NextLink));
end;


Function TPlanItemImplicitEntitySet.List(const AQuery: TQueryParams; out NextLink: String) : TPlanItemArray; 


begin
  Result:=TPlanItemArray(GetMulti(AQuery,False,NextLink));
end;


Function TPlanItemImplicitEntitySet.ListAll(const AQuery: String) : TPlanItemArray; 

var N : String;

begin
  Result:=TPlanItemArray(GetMulti(AQuery,True,N));
end;


Function TPlanItemImplicitEntitySet.ListAll(const AQuery: TQueryParams) : TPlanItemArray; 

var N : String;

begin
  Result:=TPlanItemArray(GetMulti(AQuery,True,N));
end;


{ --------------------------------------------------------------------
  TService
  --------------------------------------------------------------------}


Class Function TService.ObjectRestKind : String; 

begin
  Result:='Microsoft.OData.SampleService.Models.TripPin';
end;

Function TService.CreateNewDefaultContainer : TDefaultContainer; 

begin
  Result:=TDefaultContainer(CreateEntityContainer(TDefaultContainer));
end;


Function TService.GetDefaultContainer : TDefaultContainer; 


begin
  If Not Assigned(FDefaultContainer) then
    FDefaultContainer:=TDefaultContainer(CreateEntityContainer(TDefaultContainer));
  Result:=FDefaultContainer;
end;

end.
