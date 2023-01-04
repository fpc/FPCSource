unit googlepeople;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TPerson = Class;
  TPersonMetadata = Class;
  TSource = Class;
  TLocale = Class;
  TFieldMetadata = Class;
  TName = Class;
  TNickname = Class;
  TCoverPhoto = Class;
  TPhoto = Class;
  TGender = Class;
  TBirthday = Class;
  TDate = Class;
  TEvent = Class;
  TAddress = Class;
  TResidence = Class;
  TEmailAddress = Class;
  TPhoneNumber = Class;
  TImClient = Class;
  TTagline = Class;
  TBiography = Class;
  TUrl = Class;
  TOrganization = Class;
  TOccupation = Class;
  TInterest = Class;
  TSkill = Class;
  TBraggingRights = Class;
  TRelation = Class;
  TRelationshipInterest = Class;
  TRelationshipStatus = Class;
  TMembership = Class;
  TContactGroupMembership = Class;
  TDomainMembership = Class;
  TGetPeopleResponse = Class;
  TPersonResponse = Class;
  TListConnectionsResponse = Class;
  TPersonArray = Array of TPerson;
  TPersonMetadataArray = Array of TPersonMetadata;
  TSourceArray = Array of TSource;
  TLocaleArray = Array of TLocale;
  TFieldMetadataArray = Array of TFieldMetadata;
  TNameArray = Array of TName;
  TNicknameArray = Array of TNickname;
  TCoverPhotoArray = Array of TCoverPhoto;
  TPhotoArray = Array of TPhoto;
  TGenderArray = Array of TGender;
  TBirthdayArray = Array of TBirthday;
  TDateArray = Array of TDate;
  TEventArray = Array of TEvent;
  TAddressArray = Array of TAddress;
  TResidenceArray = Array of TResidence;
  TEmailAddressArray = Array of TEmailAddress;
  TPhoneNumberArray = Array of TPhoneNumber;
  TImClientArray = Array of TImClient;
  TTaglineArray = Array of TTagline;
  TBiographyArray = Array of TBiography;
  TUrlArray = Array of TUrl;
  TOrganizationArray = Array of TOrganization;
  TOccupationArray = Array of TOccupation;
  TInterestArray = Array of TInterest;
  TSkillArray = Array of TSkill;
  TBraggingRightsArray = Array of TBraggingRights;
  TRelationArray = Array of TRelation;
  TRelationshipInterestArray = Array of TRelationshipInterest;
  TRelationshipStatusArray = Array of TRelationshipStatus;
  TMembershipArray = Array of TMembership;
  TContactGroupMembershipArray = Array of TContactGroupMembership;
  TDomainMembershipArray = Array of TDomainMembership;
  TGetPeopleResponseArray = Array of TGetPeopleResponse;
  TPersonResponseArray = Array of TPersonResponse;
  TListConnectionsResponseArray = Array of TListConnectionsResponse;
  //Anonymous types, using auto-generated names
  TPersonTypelocalesArray = Array of TLocale;
  TPersonTypenamesArray = Array of TName;
  TPersonTypenicknamesArray = Array of TNickname;
  TPersonTypecoverPhotosArray = Array of TCoverPhoto;
  TPersonTypephotosArray = Array of TPhoto;
  TPersonTypegendersArray = Array of TGender;
  TPersonTypebirthdaysArray = Array of TBirthday;
  TPersonTypeeventsArray = Array of TEvent;
  TPersonTypeaddressesArray = Array of TAddress;
  TPersonTyperesidencesArray = Array of TResidence;
  TPersonTypeemailAddressesArray = Array of TEmailAddress;
  TPersonTypephoneNumbersArray = Array of TPhoneNumber;
  TPersonTypeimClientsArray = Array of TImClient;
  TPersonTypetaglinesArray = Array of TTagline;
  TPersonTypebiographiesArray = Array of TBiography;
  TPersonTypeurlsArray = Array of TUrl;
  TPersonTypeorganizationsArray = Array of TOrganization;
  TPersonTypeoccupationsArray = Array of TOccupation;
  TPersonTypeinterestsArray = Array of TInterest;
  TPersonTypeskillsArray = Array of TSkill;
  TPersonTypebraggingRightsArray = Array of TBraggingRights;
  TPersonTyperelationsArray = Array of TRelation;
  TPersonTyperelationshipInterestsArray = Array of TRelationshipInterest;
  TPersonTyperelationshipStatusesArray = Array of TRelationshipStatus;
  TPersonTypemembershipsArray = Array of TMembership;
  TPersonMetadataTypesourcesArray = Array of TSource;
  TGetPeopleResponseTyperesponsesArray = Array of TPersonResponse;
  TListConnectionsResponseTypeconnectionsArray = Array of TPerson;
  
  { --------------------------------------------------------------------
    TPerson
    --------------------------------------------------------------------}
  
  TPerson = Class(TGoogleBaseObject)
  Private
    FresourceName : String;
    Fetag : String;
    Fmetadata : TPersonMetadata;
    Flocales : TPersonTypelocalesArray;
    Fnames : TPersonTypenamesArray;
    Fnicknames : TPersonTypenicknamesArray;
    FcoverPhotos : TPersonTypecoverPhotosArray;
    Fphotos : TPersonTypephotosArray;
    Fgenders : TPersonTypegendersArray;
    FageRange : String;
    Fbirthdays : TPersonTypebirthdaysArray;
    Fevents : TPersonTypeeventsArray;
    Faddresses : TPersonTypeaddressesArray;
    Fresidences : TPersonTyperesidencesArray;
    FemailAddresses : TPersonTypeemailAddressesArray;
    FphoneNumbers : TPersonTypephoneNumbersArray;
    FimClients : TPersonTypeimClientsArray;
    Ftaglines : TPersonTypetaglinesArray;
    Fbiographies : TPersonTypebiographiesArray;
    Furls : TPersonTypeurlsArray;
    Forganizations : TPersonTypeorganizationsArray;
    Foccupations : TPersonTypeoccupationsArray;
    Finterests : TPersonTypeinterestsArray;
    Fskills : TPersonTypeskillsArray;
    FbraggingRights : TPersonTypebraggingRightsArray;
    Frelations : TPersonTyperelationsArray;
    FrelationshipInterests : TPersonTyperelationshipInterestsArray;
    FrelationshipStatuses : TPersonTyperelationshipStatusesArray;
    Fmemberships : TPersonTypemembershipsArray;
  Protected
    //Property setters
    Procedure SetresourceName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmetadata(AIndex : Integer; const AValue : TPersonMetadata); virtual;
    Procedure Setlocales(AIndex : Integer; const AValue : TPersonTypelocalesArray); virtual;
    Procedure Setnames(AIndex : Integer; const AValue : TPersonTypenamesArray); virtual;
    Procedure Setnicknames(AIndex : Integer; const AValue : TPersonTypenicknamesArray); virtual;
    Procedure SetcoverPhotos(AIndex : Integer; const AValue : TPersonTypecoverPhotosArray); virtual;
    Procedure Setphotos(AIndex : Integer; const AValue : TPersonTypephotosArray); virtual;
    Procedure Setgenders(AIndex : Integer; const AValue : TPersonTypegendersArray); virtual;
    Procedure SetageRange(AIndex : Integer; const AValue : String); virtual;
    Procedure Setbirthdays(AIndex : Integer; const AValue : TPersonTypebirthdaysArray); virtual;
    Procedure Setevents(AIndex : Integer; const AValue : TPersonTypeeventsArray); virtual;
    Procedure Setaddresses(AIndex : Integer; const AValue : TPersonTypeaddressesArray); virtual;
    Procedure Setresidences(AIndex : Integer; const AValue : TPersonTyperesidencesArray); virtual;
    Procedure SetemailAddresses(AIndex : Integer; const AValue : TPersonTypeemailAddressesArray); virtual;
    Procedure SetphoneNumbers(AIndex : Integer; const AValue : TPersonTypephoneNumbersArray); virtual;
    Procedure SetimClients(AIndex : Integer; const AValue : TPersonTypeimClientsArray); virtual;
    Procedure Settaglines(AIndex : Integer; const AValue : TPersonTypetaglinesArray); virtual;
    Procedure Setbiographies(AIndex : Integer; const AValue : TPersonTypebiographiesArray); virtual;
    Procedure Seturls(AIndex : Integer; const AValue : TPersonTypeurlsArray); virtual;
    Procedure Setorganizations(AIndex : Integer; const AValue : TPersonTypeorganizationsArray); virtual;
    Procedure Setoccupations(AIndex : Integer; const AValue : TPersonTypeoccupationsArray); virtual;
    Procedure Setinterests(AIndex : Integer; const AValue : TPersonTypeinterestsArray); virtual;
    Procedure Setskills(AIndex : Integer; const AValue : TPersonTypeskillsArray); virtual;
    Procedure SetbraggingRights(AIndex : Integer; const AValue : TPersonTypebraggingRightsArray); virtual;
    Procedure Setrelations(AIndex : Integer; const AValue : TPersonTyperelationsArray); virtual;
    Procedure SetrelationshipInterests(AIndex : Integer; const AValue : TPersonTyperelationshipInterestsArray); virtual;
    Procedure SetrelationshipStatuses(AIndex : Integer; const AValue : TPersonTyperelationshipStatusesArray); virtual;
    Procedure Setmemberships(AIndex : Integer; const AValue : TPersonTypemembershipsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property resourceName : String Index 0 Read FresourceName Write SetresourceName;
    Property etag : String Index 8 Read Fetag Write Setetag;
    Property metadata : TPersonMetadata Index 16 Read Fmetadata Write Setmetadata;
    Property locales : TPersonTypelocalesArray Index 24 Read Flocales Write Setlocales;
    Property names : TPersonTypenamesArray Index 32 Read Fnames Write Setnames;
    Property nicknames : TPersonTypenicknamesArray Index 40 Read Fnicknames Write Setnicknames;
    Property coverPhotos : TPersonTypecoverPhotosArray Index 48 Read FcoverPhotos Write SetcoverPhotos;
    Property photos : TPersonTypephotosArray Index 56 Read Fphotos Write Setphotos;
    Property genders : TPersonTypegendersArray Index 64 Read Fgenders Write Setgenders;
    Property ageRange : String Index 72 Read FageRange Write SetageRange;
    Property birthdays : TPersonTypebirthdaysArray Index 80 Read Fbirthdays Write Setbirthdays;
    Property events : TPersonTypeeventsArray Index 88 Read Fevents Write Setevents;
    Property addresses : TPersonTypeaddressesArray Index 96 Read Faddresses Write Setaddresses;
    Property residences : TPersonTyperesidencesArray Index 104 Read Fresidences Write Setresidences;
    Property emailAddresses : TPersonTypeemailAddressesArray Index 112 Read FemailAddresses Write SetemailAddresses;
    Property phoneNumbers : TPersonTypephoneNumbersArray Index 120 Read FphoneNumbers Write SetphoneNumbers;
    Property imClients : TPersonTypeimClientsArray Index 128 Read FimClients Write SetimClients;
    Property taglines : TPersonTypetaglinesArray Index 136 Read Ftaglines Write Settaglines;
    Property biographies : TPersonTypebiographiesArray Index 144 Read Fbiographies Write Setbiographies;
    Property urls : TPersonTypeurlsArray Index 152 Read Furls Write Seturls;
    Property organizations : TPersonTypeorganizationsArray Index 160 Read Forganizations Write Setorganizations;
    Property occupations : TPersonTypeoccupationsArray Index 168 Read Foccupations Write Setoccupations;
    Property interests : TPersonTypeinterestsArray Index 176 Read Finterests Write Setinterests;
    Property skills : TPersonTypeskillsArray Index 184 Read Fskills Write Setskills;
    Property braggingRights : TPersonTypebraggingRightsArray Index 192 Read FbraggingRights Write SetbraggingRights;
    Property relations : TPersonTyperelationsArray Index 200 Read Frelations Write Setrelations;
    Property relationshipInterests : TPersonTyperelationshipInterestsArray Index 208 Read FrelationshipInterests Write SetrelationshipInterests;
    Property relationshipStatuses : TPersonTyperelationshipStatusesArray Index 216 Read FrelationshipStatuses Write SetrelationshipStatuses;
    Property memberships : TPersonTypemembershipsArray Index 224 Read Fmemberships Write Setmemberships;
  end;
  TPersonClass = Class of TPerson;
  
  { --------------------------------------------------------------------
    TPersonMetadata
    --------------------------------------------------------------------}
  
  TPersonMetadata = Class(TGoogleBaseObject)
  Private
    Fsources : TPersonMetadataTypesourcesArray;
    FpreviousResourceNames : TStringArray;
    Fdeleted : boolean;
    FobjectType : String;
  Protected
    //Property setters
    Procedure Setsources(AIndex : Integer; const AValue : TPersonMetadataTypesourcesArray); virtual;
    Procedure SetpreviousResourceNames(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setdeleted(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetobjectType(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property sources : TPersonMetadataTypesourcesArray Index 0 Read Fsources Write Setsources;
    Property previousResourceNames : TStringArray Index 8 Read FpreviousResourceNames Write SetpreviousResourceNames;
    Property deleted : boolean Index 16 Read Fdeleted Write Setdeleted;
    Property objectType : String Index 24 Read FobjectType Write SetobjectType;
  end;
  TPersonMetadataClass = Class of TPersonMetadata;
  
  { --------------------------------------------------------------------
    TSource
    --------------------------------------------------------------------}
  
  TSource = Class(TGoogleBaseObject)
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
  TSourceClass = Class of TSource;
  
  { --------------------------------------------------------------------
    TLocale
    --------------------------------------------------------------------}
  
  TLocale = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TLocaleClass = Class of TLocale;
  
  { --------------------------------------------------------------------
    TFieldMetadata
    --------------------------------------------------------------------}
  
  TFieldMetadata = Class(TGoogleBaseObject)
  Private
    Fprimary : boolean;
    Fverified : boolean;
    Fsource : TSource;
  Protected
    //Property setters
    Procedure Setprimary(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setverified(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setsource(AIndex : Integer; const AValue : TSource); virtual;
  Public
  Published
    Property primary : boolean Index 0 Read Fprimary Write Setprimary;
    Property verified : boolean Index 8 Read Fverified Write Setverified;
    Property source : TSource Index 16 Read Fsource Write Setsource;
  end;
  TFieldMetadataClass = Class of TFieldMetadata;
  
  { --------------------------------------------------------------------
    TName
    --------------------------------------------------------------------}
  
  TName = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    FdisplayName : String;
    FfamilyName : String;
    FgivenName : String;
    FmiddleName : String;
    FhonorificPrefix : String;
    FhonorificSuffix : String;
    FphoneticFamilyName : String;
    FphoneticGivenName : String;
    FphoneticMiddleName : String;
    FphoneticHonorificPrefix : String;
    FphoneticHonorificSuffix : String;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure SetdisplayName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfamilyName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgivenName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmiddleName(AIndex : Integer; const AValue : String); virtual;
    Procedure SethonorificPrefix(AIndex : Integer; const AValue : String); virtual;
    Procedure SethonorificSuffix(AIndex : Integer; const AValue : String); virtual;
    Procedure SetphoneticFamilyName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetphoneticGivenName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetphoneticMiddleName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetphoneticHonorificPrefix(AIndex : Integer; const AValue : String); virtual;
    Procedure SetphoneticHonorificSuffix(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property displayName : String Index 8 Read FdisplayName Write SetdisplayName;
    Property familyName : String Index 16 Read FfamilyName Write SetfamilyName;
    Property givenName : String Index 24 Read FgivenName Write SetgivenName;
    Property middleName : String Index 32 Read FmiddleName Write SetmiddleName;
    Property honorificPrefix : String Index 40 Read FhonorificPrefix Write SethonorificPrefix;
    Property honorificSuffix : String Index 48 Read FhonorificSuffix Write SethonorificSuffix;
    Property phoneticFamilyName : String Index 56 Read FphoneticFamilyName Write SetphoneticFamilyName;
    Property phoneticGivenName : String Index 64 Read FphoneticGivenName Write SetphoneticGivenName;
    Property phoneticMiddleName : String Index 72 Read FphoneticMiddleName Write SetphoneticMiddleName;
    Property phoneticHonorificPrefix : String Index 80 Read FphoneticHonorificPrefix Write SetphoneticHonorificPrefix;
    Property phoneticHonorificSuffix : String Index 88 Read FphoneticHonorificSuffix Write SetphoneticHonorificSuffix;
  end;
  TNameClass = Class of TName;
  
  { --------------------------------------------------------------------
    TNickname
    --------------------------------------------------------------------}
  
  TNickname = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fvalue : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property value : String Index 8 Read Fvalue Write Setvalue;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TNicknameClass = Class of TNickname;
  
  { --------------------------------------------------------------------
    TCoverPhoto
    --------------------------------------------------------------------}
  
  TCoverPhoto = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Furl : String;
    Fdefault : boolean;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdefault(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property url : String Index 8 Read Furl Write Seturl;
    Property default : boolean Index 16 Read Fdefault Write Setdefault;
  end;
  TCoverPhotoClass = Class of TCoverPhoto;
  
  { --------------------------------------------------------------------
    TPhoto
    --------------------------------------------------------------------}
  
  TPhoto = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Furl : String;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property url : String Index 8 Read Furl Write Seturl;
  end;
  TPhotoClass = Class of TPhoto;
  
  { --------------------------------------------------------------------
    TGender
    --------------------------------------------------------------------}
  
  TGender = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fvalue : String;
    FformattedValue : String;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetformattedValue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property value : String Index 8 Read Fvalue Write Setvalue;
    Property formattedValue : String Index 16 Read FformattedValue Write SetformattedValue;
  end;
  TGenderClass = Class of TGender;
  
  { --------------------------------------------------------------------
    TBirthday
    --------------------------------------------------------------------}
  
  TBirthday = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fdate : TDate;
    Ftext : String;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setdate(AIndex : Integer; const AValue : TDate); virtual;
    Procedure Settext(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property date : TDate Index 8 Read Fdate Write Setdate;
    Property text : String Index 16 Read Ftext Write Settext;
  end;
  TBirthdayClass = Class of TBirthday;
  
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
    TEvent
    --------------------------------------------------------------------}
  
  TEvent = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fdate : TDate;
    F_type : String;
    FformattedType : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setdate(AIndex : Integer; const AValue : TDate); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetformattedType(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property date : TDate Index 8 Read Fdate Write Setdate;
    Property _type : String Index 16 Read F_type Write Set_type;
    Property formattedType : String Index 24 Read FformattedType Write SetformattedType;
  end;
  TEventClass = Class of TEvent;
  
  { --------------------------------------------------------------------
    TAddress
    --------------------------------------------------------------------}
  
  TAddress = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    FformattedValue : String;
    F_type : String;
    FformattedType : String;
    FpoBox : String;
    FstreetAddress : String;
    FextendedAddress : String;
    Fcity : String;
    Fregion : String;
    FpostalCode : String;
    Fcountry : String;
    FcountryCode : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure SetformattedValue(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetformattedType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpoBox(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstreetAddress(AIndex : Integer; const AValue : String); virtual;
    Procedure SetextendedAddress(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcity(AIndex : Integer; const AValue : String); virtual;
    Procedure Setregion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpostalCode(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcountry(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcountryCode(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property formattedValue : String Index 8 Read FformattedValue Write SetformattedValue;
    Property _type : String Index 16 Read F_type Write Set_type;
    Property formattedType : String Index 24 Read FformattedType Write SetformattedType;
    Property poBox : String Index 32 Read FpoBox Write SetpoBox;
    Property streetAddress : String Index 40 Read FstreetAddress Write SetstreetAddress;
    Property extendedAddress : String Index 48 Read FextendedAddress Write SetextendedAddress;
    Property city : String Index 56 Read Fcity Write Setcity;
    Property region : String Index 64 Read Fregion Write Setregion;
    Property postalCode : String Index 72 Read FpostalCode Write SetpostalCode;
    Property country : String Index 80 Read Fcountry Write Setcountry;
    Property countryCode : String Index 88 Read FcountryCode Write SetcountryCode;
  end;
  TAddressClass = Class of TAddress;
  
  { --------------------------------------------------------------------
    TResidence
    --------------------------------------------------------------------}
  
  TResidence = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fvalue : String;
    Fcurrent : boolean;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
    Procedure Setcurrent(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property value : String Index 8 Read Fvalue Write Setvalue;
    Property current : boolean Index 16 Read Fcurrent Write Setcurrent;
  end;
  TResidenceClass = Class of TResidence;
  
  { --------------------------------------------------------------------
    TEmailAddress
    --------------------------------------------------------------------}
  
  TEmailAddress = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fvalue : String;
    F_type : String;
    FformattedType : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetformattedType(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property value : String Index 8 Read Fvalue Write Setvalue;
    Property _type : String Index 16 Read F_type Write Set_type;
    Property formattedType : String Index 24 Read FformattedType Write SetformattedType;
  end;
  TEmailAddressClass = Class of TEmailAddress;
  
  { --------------------------------------------------------------------
    TPhoneNumber
    --------------------------------------------------------------------}
  
  TPhoneNumber = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fvalue : String;
    FcanonicalForm : String;
    F_type : String;
    FformattedType : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcanonicalForm(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetformattedType(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property value : String Index 8 Read Fvalue Write Setvalue;
    Property canonicalForm : String Index 16 Read FcanonicalForm Write SetcanonicalForm;
    Property _type : String Index 24 Read F_type Write Set_type;
    Property formattedType : String Index 32 Read FformattedType Write SetformattedType;
  end;
  TPhoneNumberClass = Class of TPhoneNumber;
  
  { --------------------------------------------------------------------
    TImClient
    --------------------------------------------------------------------}
  
  TImClient = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fusername : String;
    F_type : String;
    FformattedType : String;
    Fprotocol : String;
    FformattedProtocol : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setusername(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetformattedType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setprotocol(AIndex : Integer; const AValue : String); virtual;
    Procedure SetformattedProtocol(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property username : String Index 8 Read Fusername Write Setusername;
    Property _type : String Index 16 Read F_type Write Set_type;
    Property formattedType : String Index 24 Read FformattedType Write SetformattedType;
    Property protocol : String Index 32 Read Fprotocol Write Setprotocol;
    Property formattedProtocol : String Index 40 Read FformattedProtocol Write SetformattedProtocol;
  end;
  TImClientClass = Class of TImClient;
  
  { --------------------------------------------------------------------
    TTagline
    --------------------------------------------------------------------}
  
  TTagline = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TTaglineClass = Class of TTagline;
  
  { --------------------------------------------------------------------
    TBiography
    --------------------------------------------------------------------}
  
  TBiography = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TBiographyClass = Class of TBiography;
  
  { --------------------------------------------------------------------
    TUrl
    --------------------------------------------------------------------}
  
  TUrl = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fvalue : String;
    F_type : String;
    FformattedType : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetformattedType(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property value : String Index 8 Read Fvalue Write Setvalue;
    Property _type : String Index 16 Read F_type Write Set_type;
    Property formattedType : String Index 24 Read FformattedType Write SetformattedType;
  end;
  TUrlClass = Class of TUrl;
  
  { --------------------------------------------------------------------
    TOrganization
    --------------------------------------------------------------------}
  
  TOrganization = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    F_type : String;
    FformattedType : String;
    FstartDate : TDate;
    FendDate : TDate;
    Fcurrent : boolean;
    Fname : String;
    FphoneticName : String;
    Fdepartment : String;
    Ftitle : String;
    FjobDescription : String;
    Fsymbol : String;
    Fdomain : String;
    Flocation : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetformattedType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetstartDate(AIndex : Integer; const AValue : TDate); virtual;
    Procedure SetendDate(AIndex : Integer; const AValue : TDate); virtual;
    Procedure Setcurrent(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetphoneticName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdepartment(AIndex : Integer; const AValue : String); virtual;
    Procedure Settitle(AIndex : Integer; const AValue : String); virtual;
    Procedure SetjobDescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsymbol(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdomain(AIndex : Integer; const AValue : String); virtual;
    Procedure Setlocation(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property _type : String Index 8 Read F_type Write Set_type;
    Property formattedType : String Index 16 Read FformattedType Write SetformattedType;
    Property startDate : TDate Index 24 Read FstartDate Write SetstartDate;
    Property endDate : TDate Index 32 Read FendDate Write SetendDate;
    Property current : boolean Index 40 Read Fcurrent Write Setcurrent;
    Property name : String Index 48 Read Fname Write Setname;
    Property phoneticName : String Index 56 Read FphoneticName Write SetphoneticName;
    Property department : String Index 64 Read Fdepartment Write Setdepartment;
    Property title : String Index 72 Read Ftitle Write Settitle;
    Property jobDescription : String Index 80 Read FjobDescription Write SetjobDescription;
    Property symbol : String Index 88 Read Fsymbol Write Setsymbol;
    Property domain : String Index 96 Read Fdomain Write Setdomain;
    Property location : String Index 104 Read Flocation Write Setlocation;
  end;
  TOrganizationClass = Class of TOrganization;
  
  { --------------------------------------------------------------------
    TOccupation
    --------------------------------------------------------------------}
  
  TOccupation = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TOccupationClass = Class of TOccupation;
  
  { --------------------------------------------------------------------
    TInterest
    --------------------------------------------------------------------}
  
  TInterest = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TInterestClass = Class of TInterest;
  
  { --------------------------------------------------------------------
    TSkill
    --------------------------------------------------------------------}
  
  TSkill = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TSkillClass = Class of TSkill;
  
  { --------------------------------------------------------------------
    TBraggingRights
    --------------------------------------------------------------------}
  
  TBraggingRights = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TBraggingRightsClass = Class of TBraggingRights;
  
  { --------------------------------------------------------------------
    TRelation
    --------------------------------------------------------------------}
  
  TRelation = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fperson : String;
    F_type : String;
    FformattedType : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setperson(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure SetformattedType(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property person : String Index 8 Read Fperson Write Setperson;
    Property _type : String Index 16 Read F_type Write Set_type;
    Property formattedType : String Index 24 Read FformattedType Write SetformattedType;
  end;
  TRelationClass = Class of TRelation;
  
  { --------------------------------------------------------------------
    TRelationshipInterest
    --------------------------------------------------------------------}
  
  TRelationshipInterest = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fvalue : String;
    FformattedValue : String;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetformattedValue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property value : String Index 8 Read Fvalue Write Setvalue;
    Property formattedValue : String Index 16 Read FformattedValue Write SetformattedValue;
  end;
  TRelationshipInterestClass = Class of TRelationshipInterest;
  
  { --------------------------------------------------------------------
    TRelationshipStatus
    --------------------------------------------------------------------}
  
  TRelationshipStatus = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    Fvalue : String;
    FformattedValue : String;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetformattedValue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property value : String Index 8 Read Fvalue Write Setvalue;
    Property formattedValue : String Index 16 Read FformattedValue Write SetformattedValue;
  end;
  TRelationshipStatusClass = Class of TRelationshipStatus;
  
  { --------------------------------------------------------------------
    TMembership
    --------------------------------------------------------------------}
  
  TMembership = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFieldMetadata;
    FcontactGroupMembership : TContactGroupMembership;
    FdomainMembership : TDomainMembership;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); virtual;
    Procedure SetcontactGroupMembership(AIndex : Integer; const AValue : TContactGroupMembership); virtual;
    Procedure SetdomainMembership(AIndex : Integer; const AValue : TDomainMembership); virtual;
  Public
  Published
    Property metadata : TFieldMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property contactGroupMembership : TContactGroupMembership Index 8 Read FcontactGroupMembership Write SetcontactGroupMembership;
    Property domainMembership : TDomainMembership Index 16 Read FdomainMembership Write SetdomainMembership;
  end;
  TMembershipClass = Class of TMembership;
  
  { --------------------------------------------------------------------
    TContactGroupMembership
    --------------------------------------------------------------------}
  
  TContactGroupMembership = Class(TGoogleBaseObject)
  Private
    FcontactGroupId : String;
  Protected
    //Property setters
    Procedure SetcontactGroupId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property contactGroupId : String Index 0 Read FcontactGroupId Write SetcontactGroupId;
  end;
  TContactGroupMembershipClass = Class of TContactGroupMembership;
  
  { --------------------------------------------------------------------
    TDomainMembership
    --------------------------------------------------------------------}
  
  TDomainMembership = Class(TGoogleBaseObject)
  Private
    FinViewerDomain : boolean;
  Protected
    //Property setters
    Procedure SetinViewerDomain(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property inViewerDomain : boolean Index 0 Read FinViewerDomain Write SetinViewerDomain;
  end;
  TDomainMembershipClass = Class of TDomainMembership;
  
  { --------------------------------------------------------------------
    TGetPeopleResponse
    --------------------------------------------------------------------}
  
  TGetPeopleResponse = Class(TGoogleBaseObject)
  Private
    Fresponses : TGetPeopleResponseTyperesponsesArray;
  Protected
    //Property setters
    Procedure Setresponses(AIndex : Integer; const AValue : TGetPeopleResponseTyperesponsesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property responses : TGetPeopleResponseTyperesponsesArray Index 0 Read Fresponses Write Setresponses;
  end;
  TGetPeopleResponseClass = Class of TGetPeopleResponse;
  
  { --------------------------------------------------------------------
    TPersonResponse
    --------------------------------------------------------------------}
  
  TPersonResponse = Class(TGoogleBaseObject)
  Private
    FhttpStatusCode : integer;
    Fperson : TPerson;
    FrequestedResourceName : String;
  Protected
    //Property setters
    Procedure SethttpStatusCode(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setperson(AIndex : Integer; const AValue : TPerson); virtual;
    Procedure SetrequestedResourceName(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property httpStatusCode : integer Index 0 Read FhttpStatusCode Write SethttpStatusCode;
    Property person : TPerson Index 8 Read Fperson Write Setperson;
    Property requestedResourceName : String Index 16 Read FrequestedResourceName Write SetrequestedResourceName;
  end;
  TPersonResponseClass = Class of TPersonResponse;
  
  { --------------------------------------------------------------------
    TListConnectionsResponse
    --------------------------------------------------------------------}
  
  TListConnectionsResponse = Class(TGoogleBaseObject)
  Private
    Fconnections : TListConnectionsResponseTypeconnectionsArray;
    FnextPageToken : String;
    FnextSyncToken : String;
  Protected
    //Property setters
    Procedure Setconnections(AIndex : Integer; const AValue : TListConnectionsResponseTypeconnectionsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextSyncToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property connections : TListConnectionsResponseTypeconnectionsArray Index 0 Read Fconnections Write Setconnections;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
    Property nextSyncToken : String Index 16 Read FnextSyncToken Write SetnextSyncToken;
  end;
  TListConnectionsResponseClass = Class of TListConnectionsResponse;
  
  { --------------------------------------------------------------------
    TPeopleConnectionsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPeopleConnectionsResource, method List
  
  TPeopleConnectionsListOptions = Record
    pageToken : String;
    pageSize : integer;
    sortOrder : String;
    syncToken : String;
    requestMaskincludeField : String;
  end;
  
  TPeopleConnectionsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(_resourceName: string; AQuery : string  = '') : TListConnectionsResponse;
    Function List(_resourceName: string; AQuery : TPeopleConnectionslistOptions) : TListConnectionsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TPeopleResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TPeopleResource, method Get
  
  TPeopleGetOptions = Record
    requestMaskincludeField : String;
  end;
  
  
  //Optional query Options for TPeopleResource, method GetBatchGet
  
  TPeopleGetBatchGetOptions = Record
    resourceNames : String;
    requestMaskincludeField : String;
  end;
  
  TPeopleResource = Class(TGoogleResource)
  Private
    FConnectionsInstance : TPeopleConnectionsResource;
    Function GetConnectionsInstance : TPeopleConnectionsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(_resourceName: string; AQuery : string  = '') : TPerson;
    Function Get(_resourceName: string; AQuery : TPeoplegetOptions) : TPerson;
    Function GetBatchGet(AQuery : string  = '') : TGetPeopleResponse;
    Function GetBatchGet(AQuery : TPeoplegetBatchGetOptions) : TGetPeopleResponse;
    Function CreateConnectionsResource(AOwner : TComponent) : TPeopleConnectionsResource;virtual;overload;
    Function CreateConnectionsResource : TPeopleConnectionsResource;virtual;overload;
    Property ConnectionsResource : TPeopleConnectionsResource Read GetConnectionsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TPeopleAPI
    --------------------------------------------------------------------}
  
  TPeopleAPI = Class(TGoogleAPI)
  Private
    FPeopleConnectionsInstance : TPeopleConnectionsResource;
    FPeopleInstance : TPeopleResource;
    Function GetPeopleConnectionsInstance : TPeopleConnectionsResource;virtual;
    Function GetPeopleInstance : TPeopleResource;virtual;
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
    Function CreatePeopleConnectionsResource(AOwner : TComponent) : TPeopleConnectionsResource;virtual;overload;
    Function CreatePeopleConnectionsResource : TPeopleConnectionsResource;virtual;overload;
    Function CreatePeopleResource(AOwner : TComponent) : TPeopleResource;virtual;overload;
    Function CreatePeopleResource : TPeopleResource;virtual;overload;
    //Add default on-demand instances for resources
    Property PeopleConnectionsResource : TPeopleConnectionsResource Read GetPeopleConnectionsInstance;
    Property PeopleResource : TPeopleResource Read GetPeopleInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TPerson
  --------------------------------------------------------------------}


Procedure TPerson.SetresourceName(AIndex : Integer; const AValue : String); 

begin
  If (FresourceName=AValue) then exit;
  FresourceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setmetadata(AIndex : Integer; const AValue : TPersonMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setlocales(AIndex : Integer; const AValue : TPersonTypelocalesArray); 

begin
  If (Flocales=AValue) then exit;
  Flocales:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setnames(AIndex : Integer; const AValue : TPersonTypenamesArray); 

begin
  If (Fnames=AValue) then exit;
  Fnames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setnicknames(AIndex : Integer; const AValue : TPersonTypenicknamesArray); 

begin
  If (Fnicknames=AValue) then exit;
  Fnicknames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetcoverPhotos(AIndex : Integer; const AValue : TPersonTypecoverPhotosArray); 

begin
  If (FcoverPhotos=AValue) then exit;
  FcoverPhotos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setphotos(AIndex : Integer; const AValue : TPersonTypephotosArray); 

begin
  If (Fphotos=AValue) then exit;
  Fphotos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setgenders(AIndex : Integer; const AValue : TPersonTypegendersArray); 

begin
  If (Fgenders=AValue) then exit;
  Fgenders:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetageRange(AIndex : Integer; const AValue : String); 

begin
  If (FageRange=AValue) then exit;
  FageRange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setbirthdays(AIndex : Integer; const AValue : TPersonTypebirthdaysArray); 

begin
  If (Fbirthdays=AValue) then exit;
  Fbirthdays:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setevents(AIndex : Integer; const AValue : TPersonTypeeventsArray); 

begin
  If (Fevents=AValue) then exit;
  Fevents:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setaddresses(AIndex : Integer; const AValue : TPersonTypeaddressesArray); 

begin
  If (Faddresses=AValue) then exit;
  Faddresses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setresidences(AIndex : Integer; const AValue : TPersonTyperesidencesArray); 

begin
  If (Fresidences=AValue) then exit;
  Fresidences:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetemailAddresses(AIndex : Integer; const AValue : TPersonTypeemailAddressesArray); 

begin
  If (FemailAddresses=AValue) then exit;
  FemailAddresses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetphoneNumbers(AIndex : Integer; const AValue : TPersonTypephoneNumbersArray); 

begin
  If (FphoneNumbers=AValue) then exit;
  FphoneNumbers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetimClients(AIndex : Integer; const AValue : TPersonTypeimClientsArray); 

begin
  If (FimClients=AValue) then exit;
  FimClients:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Settaglines(AIndex : Integer; const AValue : TPersonTypetaglinesArray); 

begin
  If (Ftaglines=AValue) then exit;
  Ftaglines:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setbiographies(AIndex : Integer; const AValue : TPersonTypebiographiesArray); 

begin
  If (Fbiographies=AValue) then exit;
  Fbiographies:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Seturls(AIndex : Integer; const AValue : TPersonTypeurlsArray); 

begin
  If (Furls=AValue) then exit;
  Furls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setorganizations(AIndex : Integer; const AValue : TPersonTypeorganizationsArray); 

begin
  If (Forganizations=AValue) then exit;
  Forganizations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setoccupations(AIndex : Integer; const AValue : TPersonTypeoccupationsArray); 

begin
  If (Foccupations=AValue) then exit;
  Foccupations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setinterests(AIndex : Integer; const AValue : TPersonTypeinterestsArray); 

begin
  If (Finterests=AValue) then exit;
  Finterests:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setskills(AIndex : Integer; const AValue : TPersonTypeskillsArray); 

begin
  If (Fskills=AValue) then exit;
  Fskills:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetbraggingRights(AIndex : Integer; const AValue : TPersonTypebraggingRightsArray); 

begin
  If (FbraggingRights=AValue) then exit;
  FbraggingRights:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setrelations(AIndex : Integer; const AValue : TPersonTyperelationsArray); 

begin
  If (Frelations=AValue) then exit;
  Frelations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetrelationshipInterests(AIndex : Integer; const AValue : TPersonTyperelationshipInterestsArray); 

begin
  If (FrelationshipInterests=AValue) then exit;
  FrelationshipInterests:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.SetrelationshipStatuses(AIndex : Integer; const AValue : TPersonTyperelationshipStatusesArray); 

begin
  If (FrelationshipStatuses=AValue) then exit;
  FrelationshipStatuses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPerson.Setmemberships(AIndex : Integer; const AValue : TPersonTypemembershipsArray); 

begin
  If (Fmemberships=AValue) then exit;
  Fmemberships:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPerson.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'locales' : SetLength(Flocales,ALength);
  'names' : SetLength(Fnames,ALength);
  'nicknames' : SetLength(Fnicknames,ALength);
  'coverphotos' : SetLength(FcoverPhotos,ALength);
  'photos' : SetLength(Fphotos,ALength);
  'genders' : SetLength(Fgenders,ALength);
  'birthdays' : SetLength(Fbirthdays,ALength);
  'events' : SetLength(Fevents,ALength);
  'addresses' : SetLength(Faddresses,ALength);
  'residences' : SetLength(Fresidences,ALength);
  'emailaddresses' : SetLength(FemailAddresses,ALength);
  'phonenumbers' : SetLength(FphoneNumbers,ALength);
  'imclients' : SetLength(FimClients,ALength);
  'taglines' : SetLength(Ftaglines,ALength);
  'biographies' : SetLength(Fbiographies,ALength);
  'urls' : SetLength(Furls,ALength);
  'organizations' : SetLength(Forganizations,ALength);
  'occupations' : SetLength(Foccupations,ALength);
  'interests' : SetLength(Finterests,ALength);
  'skills' : SetLength(Fskills,ALength);
  'braggingrights' : SetLength(FbraggingRights,ALength);
  'relations' : SetLength(Frelations,ALength);
  'relationshipinterests' : SetLength(FrelationshipInterests,ALength);
  'relationshipstatuses' : SetLength(FrelationshipStatuses,ALength);
  'memberships' : SetLength(Fmemberships,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPersonMetadata
  --------------------------------------------------------------------}


Procedure TPersonMetadata.Setsources(AIndex : Integer; const AValue : TPersonMetadataTypesourcesArray); 

begin
  If (Fsources=AValue) then exit;
  Fsources:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonMetadata.SetpreviousResourceNames(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FpreviousResourceNames=AValue) then exit;
  FpreviousResourceNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonMetadata.Setdeleted(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdeleted=AValue) then exit;
  Fdeleted:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonMetadata.SetobjectType(AIndex : Integer; const AValue : String); 

begin
  If (FobjectType=AValue) then exit;
  FobjectType:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPersonMetadata.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sources' : SetLength(Fsources,ALength);
  'previousresourcenames' : SetLength(FpreviousResourceNames,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSource
  --------------------------------------------------------------------}


Procedure TSource.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSource.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSource.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TLocale
  --------------------------------------------------------------------}


Procedure TLocale.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLocale.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFieldMetadata
  --------------------------------------------------------------------}


Procedure TFieldMetadata.Setprimary(AIndex : Integer; const AValue : boolean); 

begin
  If (Fprimary=AValue) then exit;
  Fprimary:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFieldMetadata.Setverified(AIndex : Integer; const AValue : boolean); 

begin
  If (Fverified=AValue) then exit;
  Fverified:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFieldMetadata.Setsource(AIndex : Integer; const AValue : TSource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TName
  --------------------------------------------------------------------}


Procedure TName.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TName.SetdisplayName(AIndex : Integer; const AValue : String); 

begin
  If (FdisplayName=AValue) then exit;
  FdisplayName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TName.SetfamilyName(AIndex : Integer; const AValue : String); 

begin
  If (FfamilyName=AValue) then exit;
  FfamilyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TName.SetgivenName(AIndex : Integer; const AValue : String); 

begin
  If (FgivenName=AValue) then exit;
  FgivenName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TName.SetmiddleName(AIndex : Integer; const AValue : String); 

begin
  If (FmiddleName=AValue) then exit;
  FmiddleName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TName.SethonorificPrefix(AIndex : Integer; const AValue : String); 

begin
  If (FhonorificPrefix=AValue) then exit;
  FhonorificPrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TName.SethonorificSuffix(AIndex : Integer; const AValue : String); 

begin
  If (FhonorificSuffix=AValue) then exit;
  FhonorificSuffix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TName.SetphoneticFamilyName(AIndex : Integer; const AValue : String); 

begin
  If (FphoneticFamilyName=AValue) then exit;
  FphoneticFamilyName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TName.SetphoneticGivenName(AIndex : Integer; const AValue : String); 

begin
  If (FphoneticGivenName=AValue) then exit;
  FphoneticGivenName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TName.SetphoneticMiddleName(AIndex : Integer; const AValue : String); 

begin
  If (FphoneticMiddleName=AValue) then exit;
  FphoneticMiddleName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TName.SetphoneticHonorificPrefix(AIndex : Integer; const AValue : String); 

begin
  If (FphoneticHonorificPrefix=AValue) then exit;
  FphoneticHonorificPrefix:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TName.SetphoneticHonorificSuffix(AIndex : Integer; const AValue : String); 

begin
  If (FphoneticHonorificSuffix=AValue) then exit;
  FphoneticHonorificSuffix:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TNickname
  --------------------------------------------------------------------}


Procedure TNickname.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNickname.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TNickname.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TNickname.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCoverPhoto
  --------------------------------------------------------------------}


Procedure TCoverPhoto.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCoverPhoto.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCoverPhoto.Setdefault(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdefault=AValue) then exit;
  Fdefault:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPhoto
  --------------------------------------------------------------------}


Procedure TPhoto.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPhoto.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGender
  --------------------------------------------------------------------}


Procedure TGender.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGender.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGender.SetformattedValue(AIndex : Integer; const AValue : String); 

begin
  If (FformattedValue=AValue) then exit;
  FformattedValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBirthday
  --------------------------------------------------------------------}


Procedure TBirthday.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBirthday.Setdate(AIndex : Integer; const AValue : TDate); 

begin
  If (Fdate=AValue) then exit;
  Fdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBirthday.Settext(AIndex : Integer; const AValue : String); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;





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
  TEvent
  --------------------------------------------------------------------}


Procedure TEvent.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Setdate(AIndex : Integer; const AValue : TDate); 

begin
  If (Fdate=AValue) then exit;
  Fdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEvent.SetformattedType(AIndex : Integer; const AValue : String); 

begin
  If (FformattedType=AValue) then exit;
  FformattedType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TEvent.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAddress
  --------------------------------------------------------------------}


Procedure TAddress.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.SetformattedValue(AIndex : Integer; const AValue : String); 

begin
  If (FformattedValue=AValue) then exit;
  FformattedValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.SetformattedType(AIndex : Integer; const AValue : String); 

begin
  If (FformattedType=AValue) then exit;
  FformattedType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.SetpoBox(AIndex : Integer; const AValue : String); 

begin
  If (FpoBox=AValue) then exit;
  FpoBox:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.SetstreetAddress(AIndex : Integer; const AValue : String); 

begin
  If (FstreetAddress=AValue) then exit;
  FstreetAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.SetextendedAddress(AIndex : Integer; const AValue : String); 

begin
  If (FextendedAddress=AValue) then exit;
  FextendedAddress:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.Setcity(AIndex : Integer; const AValue : String); 

begin
  If (Fcity=AValue) then exit;
  Fcity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.Setregion(AIndex : Integer; const AValue : String); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.SetpostalCode(AIndex : Integer; const AValue : String); 

begin
  If (FpostalCode=AValue) then exit;
  FpostalCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.Setcountry(AIndex : Integer; const AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAddress.SetcountryCode(AIndex : Integer; const AValue : String); 

begin
  If (FcountryCode=AValue) then exit;
  FcountryCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAddress.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TResidence
  --------------------------------------------------------------------}


Procedure TResidence.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResidence.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TResidence.Setcurrent(AIndex : Integer; const AValue : boolean); 

begin
  If (Fcurrent=AValue) then exit;
  Fcurrent:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEmailAddress
  --------------------------------------------------------------------}


Procedure TEmailAddress.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEmailAddress.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEmailAddress.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEmailAddress.SetformattedType(AIndex : Integer; const AValue : String); 

begin
  If (FformattedType=AValue) then exit;
  FformattedType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TEmailAddress.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPhoneNumber
  --------------------------------------------------------------------}


Procedure TPhoneNumber.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPhoneNumber.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPhoneNumber.SetcanonicalForm(AIndex : Integer; const AValue : String); 

begin
  If (FcanonicalForm=AValue) then exit;
  FcanonicalForm:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPhoneNumber.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPhoneNumber.SetformattedType(AIndex : Integer; const AValue : String); 

begin
  If (FformattedType=AValue) then exit;
  FformattedType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPhoneNumber.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TImClient
  --------------------------------------------------------------------}


Procedure TImClient.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImClient.Setusername(AIndex : Integer; const AValue : String); 

begin
  If (Fusername=AValue) then exit;
  Fusername:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImClient.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImClient.SetformattedType(AIndex : Integer; const AValue : String); 

begin
  If (FformattedType=AValue) then exit;
  FformattedType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImClient.Setprotocol(AIndex : Integer; const AValue : String); 

begin
  If (Fprotocol=AValue) then exit;
  Fprotocol:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImClient.SetformattedProtocol(AIndex : Integer; const AValue : String); 

begin
  If (FformattedProtocol=AValue) then exit;
  FformattedProtocol:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TImClient.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TTagline
  --------------------------------------------------------------------}


Procedure TTagline.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTagline.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBiography
  --------------------------------------------------------------------}


Procedure TBiography.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBiography.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUrl
  --------------------------------------------------------------------}


Procedure TUrl.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrl.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrl.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TUrl.SetformattedType(AIndex : Integer; const AValue : String); 

begin
  If (FformattedType=AValue) then exit;
  FformattedType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TUrl.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TOrganization
  --------------------------------------------------------------------}


Procedure TOrganization.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrganization.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrganization.SetformattedType(AIndex : Integer; const AValue : String); 

begin
  If (FformattedType=AValue) then exit;
  FformattedType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrganization.SetstartDate(AIndex : Integer; const AValue : TDate); 

begin
  If (FstartDate=AValue) then exit;
  FstartDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrganization.SetendDate(AIndex : Integer; const AValue : TDate); 

begin
  If (FendDate=AValue) then exit;
  FendDate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrganization.Setcurrent(AIndex : Integer; const AValue : boolean); 

begin
  If (Fcurrent=AValue) then exit;
  Fcurrent:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrganization.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrganization.SetphoneticName(AIndex : Integer; const AValue : String); 

begin
  If (FphoneticName=AValue) then exit;
  FphoneticName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrganization.Setdepartment(AIndex : Integer; const AValue : String); 

begin
  If (Fdepartment=AValue) then exit;
  Fdepartment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrganization.Settitle(AIndex : Integer; const AValue : String); 

begin
  If (Ftitle=AValue) then exit;
  Ftitle:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrganization.SetjobDescription(AIndex : Integer; const AValue : String); 

begin
  If (FjobDescription=AValue) then exit;
  FjobDescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrganization.Setsymbol(AIndex : Integer; const AValue : String); 

begin
  If (Fsymbol=AValue) then exit;
  Fsymbol:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrganization.Setdomain(AIndex : Integer; const AValue : String); 

begin
  If (Fdomain=AValue) then exit;
  Fdomain:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOrganization.Setlocation(AIndex : Integer; const AValue : String); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TOrganization.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TOccupation
  --------------------------------------------------------------------}


Procedure TOccupation.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOccupation.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInterest
  --------------------------------------------------------------------}


Procedure TInterest.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInterest.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSkill
  --------------------------------------------------------------------}


Procedure TSkill.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSkill.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBraggingRights
  --------------------------------------------------------------------}


Procedure TBraggingRights.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBraggingRights.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRelation
  --------------------------------------------------------------------}


Procedure TRelation.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelation.Setperson(AIndex : Integer; const AValue : String); 

begin
  If (Fperson=AValue) then exit;
  Fperson:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelation.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelation.SetformattedType(AIndex : Integer; const AValue : String); 

begin
  If (FformattedType=AValue) then exit;
  FformattedType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TRelation.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TRelationshipInterest
  --------------------------------------------------------------------}


Procedure TRelationshipInterest.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelationshipInterest.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelationshipInterest.SetformattedValue(AIndex : Integer; const AValue : String); 

begin
  If (FformattedValue=AValue) then exit;
  FformattedValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRelationshipStatus
  --------------------------------------------------------------------}


Procedure TRelationshipStatus.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelationshipStatus.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRelationshipStatus.SetformattedValue(AIndex : Integer; const AValue : String); 

begin
  If (FformattedValue=AValue) then exit;
  FformattedValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMembership
  --------------------------------------------------------------------}


Procedure TMembership.Setmetadata(AIndex : Integer; const AValue : TFieldMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMembership.SetcontactGroupMembership(AIndex : Integer; const AValue : TContactGroupMembership); 

begin
  If (FcontactGroupMembership=AValue) then exit;
  FcontactGroupMembership:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMembership.SetdomainMembership(AIndex : Integer; const AValue : TDomainMembership); 

begin
  If (FdomainMembership=AValue) then exit;
  FdomainMembership:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TContactGroupMembership
  --------------------------------------------------------------------}


Procedure TContactGroupMembership.SetcontactGroupId(AIndex : Integer; const AValue : String); 

begin
  If (FcontactGroupId=AValue) then exit;
  FcontactGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDomainMembership
  --------------------------------------------------------------------}


Procedure TDomainMembership.SetinViewerDomain(AIndex : Integer; const AValue : boolean); 

begin
  If (FinViewerDomain=AValue) then exit;
  FinViewerDomain:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGetPeopleResponse
  --------------------------------------------------------------------}


Procedure TGetPeopleResponse.Setresponses(AIndex : Integer; const AValue : TGetPeopleResponseTyperesponsesArray); 

begin
  If (Fresponses=AValue) then exit;
  Fresponses:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGetPeopleResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'responses' : SetLength(Fresponses,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPersonResponse
  --------------------------------------------------------------------}


Procedure TPersonResponse.SethttpStatusCode(AIndex : Integer; const AValue : integer); 

begin
  If (FhttpStatusCode=AValue) then exit;
  FhttpStatusCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonResponse.Setperson(AIndex : Integer; const AValue : TPerson); 

begin
  If (Fperson=AValue) then exit;
  Fperson:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPersonResponse.SetrequestedResourceName(AIndex : Integer; const AValue : String); 

begin
  If (FrequestedResourceName=AValue) then exit;
  FrequestedResourceName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListConnectionsResponse
  --------------------------------------------------------------------}


Procedure TListConnectionsResponse.Setconnections(AIndex : Integer; const AValue : TListConnectionsResponseTypeconnectionsArray); 

begin
  If (Fconnections=AValue) then exit;
  Fconnections:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListConnectionsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListConnectionsResponse.SetnextSyncToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextSyncToken=AValue) then exit;
  FnextSyncToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListConnectionsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'connections' : SetLength(Fconnections,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPeopleConnectionsResource
  --------------------------------------------------------------------}


Class Function TPeopleConnectionsResource.ResourceName : String;

begin
  Result:='connections';
end;

Class Function TPeopleConnectionsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TpeopleAPI;
end;

Function TPeopleConnectionsResource.List(_resourceName: string; AQuery : string = '') : TListConnectionsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+resourceName}/connections';
  _Methodid   = 'people.people.connections.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resourceName',_resourceName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListConnectionsResponse) as TListConnectionsResponse;
end;


Function TPeopleConnectionsResource.List(_resourceName: string; AQuery : TPeopleConnectionslistOptions) : TListConnectionsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'sortOrder',AQuery.sortOrder);
  AddToQuery(_Q,'syncToken',AQuery.syncToken);
  AddToQuery(_Q,'requestMask.includeField',AQuery.requestMaskincludeField);
  Result:=List(_resourceName,_Q);
end;



{ --------------------------------------------------------------------
  TPeopleResource
  --------------------------------------------------------------------}


Class Function TPeopleResource.ResourceName : String;

begin
  Result:='people';
end;

Class Function TPeopleResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TpeopleAPI;
end;

Function TPeopleResource.Get(_resourceName: string; AQuery : string = '') : TPerson;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+resourceName}';
  _Methodid   = 'people.people.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resourceName',_resourceName]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TPerson) as TPerson;
end;


Function TPeopleResource.Get(_resourceName: string; AQuery : TPeoplegetOptions) : TPerson;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'requestMask.includeField',AQuery.requestMaskincludeField);
  Result:=Get(_resourceName,_Q);
end;

Function TPeopleResource.GetBatchGet(AQuery : string = '') : TGetPeopleResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/people:batchGet';
  _Methodid   = 'people.people.getBatchGet';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TGetPeopleResponse) as TGetPeopleResponse;
end;


Function TPeopleResource.GetBatchGet(AQuery : TPeoplegetBatchGetOptions) : TGetPeopleResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'resourceNames',AQuery.resourceNames);
  AddToQuery(_Q,'requestMask.includeField',AQuery.requestMaskincludeField);
  Result:=GetBatchGet(_Q);
end;



Function TPeopleResource.GetConnectionsInstance : TPeopleConnectionsResource;

begin
  if (FConnectionsInstance=Nil) then
    FConnectionsInstance:=CreateConnectionsResource;
  Result:=FConnectionsInstance;
end;

Function TPeopleResource.CreateConnectionsResource : TPeopleConnectionsResource;

begin
  Result:=CreateConnectionsResource(Self);
end;


Function TPeopleResource.CreateConnectionsResource(AOwner : TComponent) : TPeopleConnectionsResource;

begin
  Result:=TPeopleConnectionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TPeopleAPI
  --------------------------------------------------------------------}

Class Function TPeopleAPI.APIName : String;

begin
  Result:='people';
end;

Class Function TPeopleAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TPeopleAPI.APIRevision : String;

begin
  Result:='20160210';
end;

Class Function TPeopleAPI.APIID : String;

begin
  Result:='people:v1';
end;

Class Function TPeopleAPI.APITitle : String;

begin
  Result:='Google People API';
end;

Class Function TPeopleAPI.APIDescription : String;

begin
  Result:='The Google People API service gives access to information about profiles and contacts.';
end;

Class Function TPeopleAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TPeopleAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TPeopleAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TPeopleAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TPeopleAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/people/';
end;

Class Function TPeopleAPI.APIrootUrl : string;

begin
  Result:='https://people.googleapis.com/';
end;

Class Function TPeopleAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TPeopleAPI.APIbaseURL : String;

begin
  Result:='https://people.googleapis.com/';
end;

Class Function TPeopleAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TPeopleAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TPeopleAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TPeopleAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,9);
  Result[0].Name:='https://www.googleapis.com/auth/contacts';
  Result[0].Description:='Manage your contacts';
  Result[1].Name:='https://www.googleapis.com/auth/contacts.readonly';
  Result[1].Description:='View your contacts';
  Result[2].Name:='https://www.googleapis.com/auth/plus.login';
  Result[2].Description:='Know your basic profile info and list of people in your circles.';
  Result[3].Name:='https://www.googleapis.com/auth/user.addresses.read';
  Result[3].Description:='View your street addresses';
  Result[4].Name:='https://www.googleapis.com/auth/user.birthday.read';
  Result[4].Description:='View your complete date of birth';
  Result[5].Name:='https://www.googleapis.com/auth/user.emails.read';
  Result[5].Description:='View your email addresses';
  Result[6].Name:='https://www.googleapis.com/auth/user.phonenumbers.read';
  Result[6].Description:='View your phone numbers';
  Result[7].Name:='https://www.googleapis.com/auth/userinfo.email';
  Result[7].Description:='View your email address';
  Result[8].Name:='https://www.googleapis.com/auth/userinfo.profile';
  Result[8].Description:='View your basic profile info';
  
end;

Class Function TPeopleAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TPeopleAPI.RegisterAPIResources;

begin
  TPerson.RegisterObject;
  TPersonMetadata.RegisterObject;
  TSource.RegisterObject;
  TLocale.RegisterObject;
  TFieldMetadata.RegisterObject;
  TName.RegisterObject;
  TNickname.RegisterObject;
  TCoverPhoto.RegisterObject;
  TPhoto.RegisterObject;
  TGender.RegisterObject;
  TBirthday.RegisterObject;
  TDate.RegisterObject;
  TEvent.RegisterObject;
  TAddress.RegisterObject;
  TResidence.RegisterObject;
  TEmailAddress.RegisterObject;
  TPhoneNumber.RegisterObject;
  TImClient.RegisterObject;
  TTagline.RegisterObject;
  TBiography.RegisterObject;
  TUrl.RegisterObject;
  TOrganization.RegisterObject;
  TOccupation.RegisterObject;
  TInterest.RegisterObject;
  TSkill.RegisterObject;
  TBraggingRights.RegisterObject;
  TRelation.RegisterObject;
  TRelationshipInterest.RegisterObject;
  TRelationshipStatus.RegisterObject;
  TMembership.RegisterObject;
  TContactGroupMembership.RegisterObject;
  TDomainMembership.RegisterObject;
  TGetPeopleResponse.RegisterObject;
  TPersonResponse.RegisterObject;
  TListConnectionsResponse.RegisterObject;
end;


Function TPeopleAPI.GetPeopleConnectionsInstance : TPeopleConnectionsResource;

begin
  if (FPeopleConnectionsInstance=Nil) then
    FPeopleConnectionsInstance:=CreatePeopleConnectionsResource;
  Result:=FPeopleConnectionsInstance;
end;

Function TPeopleAPI.CreatePeopleConnectionsResource : TPeopleConnectionsResource;

begin
  Result:=CreatePeopleConnectionsResource(Self);
end;


Function TPeopleAPI.CreatePeopleConnectionsResource(AOwner : TComponent) : TPeopleConnectionsResource;

begin
  Result:=TPeopleConnectionsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TPeopleAPI.GetPeopleInstance : TPeopleResource;

begin
  if (FPeopleInstance=Nil) then
    FPeopleInstance:=CreatePeopleResource;
  Result:=FPeopleInstance;
end;

Function TPeopleAPI.CreatePeopleResource : TPeopleResource;

begin
  Result:=CreatePeopleResource(Self);
end;


Function TPeopleAPI.CreatePeopleResource(AOwner : TComponent) : TPeopleResource;

begin
  Result:=TPeopleResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TPeopleAPI.RegisterAPI;
end.
