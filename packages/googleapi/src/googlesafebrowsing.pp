unit googlesafebrowsing;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TFindThreatMatchesRequest = Class;
  TClientInfo = Class;
  TThreatInfo = Class;
  TThreatEntry = Class;
  TFindThreatMatchesResponse = Class;
  TThreatMatch = Class;
  TThreatEntryMetadata = Class;
  TMetadataEntry = Class;
  TFetchThreatListUpdatesRequest = Class;
  TListUpdateRequest = Class;
  TConstraints = Class;
  TFetchThreatListUpdatesResponse = Class;
  TListUpdateResponse = Class;
  TThreatEntrySet = Class;
  TRawHashes = Class;
  TRawIndices = Class;
  TRiceDeltaEncoding = Class;
  TChecksum = Class;
  TFindFullHashesRequest = Class;
  TFindFullHashesResponse = Class;
  TListThreatListsResponse = Class;
  TThreatListDescriptor = Class;
  TFindThreatMatchesRequestArray = Array of TFindThreatMatchesRequest;
  TClientInfoArray = Array of TClientInfo;
  TThreatInfoArray = Array of TThreatInfo;
  TThreatEntryArray = Array of TThreatEntry;
  TFindThreatMatchesResponseArray = Array of TFindThreatMatchesResponse;
  TThreatMatchArray = Array of TThreatMatch;
  TThreatEntryMetadataArray = Array of TThreatEntryMetadata;
  TMetadataEntryArray = Array of TMetadataEntry;
  TFetchThreatListUpdatesRequestArray = Array of TFetchThreatListUpdatesRequest;
  TListUpdateRequestArray = Array of TListUpdateRequest;
  TConstraintsArray = Array of TConstraints;
  TFetchThreatListUpdatesResponseArray = Array of TFetchThreatListUpdatesResponse;
  TListUpdateResponseArray = Array of TListUpdateResponse;
  TThreatEntrySetArray = Array of TThreatEntrySet;
  TRawHashesArray = Array of TRawHashes;
  TRawIndicesArray = Array of TRawIndices;
  TRiceDeltaEncodingArray = Array of TRiceDeltaEncoding;
  TChecksumArray = Array of TChecksum;
  TFindFullHashesRequestArray = Array of TFindFullHashesRequest;
  TFindFullHashesResponseArray = Array of TFindFullHashesResponse;
  TListThreatListsResponseArray = Array of TListThreatListsResponse;
  TThreatListDescriptorArray = Array of TThreatListDescriptor;
  //Anonymous types, using auto-generated names
  TThreatInfoTypethreatEntriesArray = Array of TThreatEntry;
  TFindThreatMatchesResponseTypematchesArray = Array of TThreatMatch;
  TThreatEntryMetadataTypeentriesArray = Array of TMetadataEntry;
  TFetchThreatListUpdatesRequestTypelistUpdateRequestsArray = Array of TListUpdateRequest;
  TFetchThreatListUpdatesResponseTypelistUpdateResponsesArray = Array of TListUpdateResponse;
  TListUpdateResponseTypeadditionsArray = Array of TThreatEntrySet;
  TListUpdateResponseTyperemovalsArray = Array of TThreatEntrySet;
  TFindFullHashesResponseTypematchesArray = Array of TThreatMatch;
  TListThreatListsResponseTypethreatListsArray = Array of TThreatListDescriptor;
  
  { --------------------------------------------------------------------
    TFindThreatMatchesRequest
    --------------------------------------------------------------------}
  
  TFindThreatMatchesRequest = Class(TGoogleBaseObject)
  Private
    Fclient : TClientInfo;
    FthreatInfo : TThreatInfo;
  Protected
    //Property setters
    Procedure Setclient(AIndex : Integer; const AValue : TClientInfo); virtual;
    Procedure SetthreatInfo(AIndex : Integer; const AValue : TThreatInfo); virtual;
  Public
  Published
    Property client : TClientInfo Index 0 Read Fclient Write Setclient;
    Property threatInfo : TThreatInfo Index 8 Read FthreatInfo Write SetthreatInfo;
  end;
  TFindThreatMatchesRequestClass = Class of TFindThreatMatchesRequest;
  
  { --------------------------------------------------------------------
    TClientInfo
    --------------------------------------------------------------------}
  
  TClientInfo = Class(TGoogleBaseObject)
  Private
    FclientId : String;
    FclientVersion : String;
  Protected
    //Property setters
    Procedure SetclientId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetclientVersion(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property clientId : String Index 0 Read FclientId Write SetclientId;
    Property clientVersion : String Index 8 Read FclientVersion Write SetclientVersion;
  end;
  TClientInfoClass = Class of TClientInfo;
  
  { --------------------------------------------------------------------
    TThreatInfo
    --------------------------------------------------------------------}
  
  TThreatInfo = Class(TGoogleBaseObject)
  Private
    FthreatTypes : TStringArray;
    FplatformTypes : TStringArray;
    FthreatEntryTypes : TStringArray;
    FthreatEntries : TThreatInfoTypethreatEntriesArray;
  Protected
    //Property setters
    Procedure SetthreatTypes(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetplatformTypes(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetthreatEntryTypes(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetthreatEntries(AIndex : Integer; const AValue : TThreatInfoTypethreatEntriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property threatTypes : TStringArray Index 0 Read FthreatTypes Write SetthreatTypes;
    Property platformTypes : TStringArray Index 8 Read FplatformTypes Write SetplatformTypes;
    Property threatEntryTypes : TStringArray Index 16 Read FthreatEntryTypes Write SetthreatEntryTypes;
    Property threatEntries : TThreatInfoTypethreatEntriesArray Index 24 Read FthreatEntries Write SetthreatEntries;
  end;
  TThreatInfoClass = Class of TThreatInfo;
  
  { --------------------------------------------------------------------
    TThreatEntry
    --------------------------------------------------------------------}
  
  TThreatEntry = Class(TGoogleBaseObject)
  Private
    Fhash : String;
    Furl : String;
    Fdigest : String;
  Protected
    //Property setters
    Procedure Sethash(AIndex : Integer; const AValue : String); virtual;
    Procedure Seturl(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdigest(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property hash : String Index 0 Read Fhash Write Sethash;
    Property url : String Index 8 Read Furl Write Seturl;
    Property digest : String Index 16 Read Fdigest Write Setdigest;
  end;
  TThreatEntryClass = Class of TThreatEntry;
  
  { --------------------------------------------------------------------
    TFindThreatMatchesResponse
    --------------------------------------------------------------------}
  
  TFindThreatMatchesResponse = Class(TGoogleBaseObject)
  Private
    Fmatches : TFindThreatMatchesResponseTypematchesArray;
  Protected
    //Property setters
    Procedure Setmatches(AIndex : Integer; const AValue : TFindThreatMatchesResponseTypematchesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property matches : TFindThreatMatchesResponseTypematchesArray Index 0 Read Fmatches Write Setmatches;
  end;
  TFindThreatMatchesResponseClass = Class of TFindThreatMatchesResponse;
  
  { --------------------------------------------------------------------
    TThreatMatch
    --------------------------------------------------------------------}
  
  TThreatMatch = Class(TGoogleBaseObject)
  Private
    FthreatType : String;
    FplatformType : String;
    FthreatEntryType : String;
    Fthreat : TThreatEntry;
    FthreatEntryMetadata : TThreatEntryMetadata;
    FcacheDuration : String;
  Protected
    //Property setters
    Procedure SetthreatType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetplatformType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetthreatEntryType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setthreat(AIndex : Integer; const AValue : TThreatEntry); virtual;
    Procedure SetthreatEntryMetadata(AIndex : Integer; const AValue : TThreatEntryMetadata); virtual;
    Procedure SetcacheDuration(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property threatType : String Index 0 Read FthreatType Write SetthreatType;
    Property platformType : String Index 8 Read FplatformType Write SetplatformType;
    Property threatEntryType : String Index 16 Read FthreatEntryType Write SetthreatEntryType;
    Property threat : TThreatEntry Index 24 Read Fthreat Write Setthreat;
    Property threatEntryMetadata : TThreatEntryMetadata Index 32 Read FthreatEntryMetadata Write SetthreatEntryMetadata;
    Property cacheDuration : String Index 40 Read FcacheDuration Write SetcacheDuration;
  end;
  TThreatMatchClass = Class of TThreatMatch;
  
  { --------------------------------------------------------------------
    TThreatEntryMetadata
    --------------------------------------------------------------------}
  
  TThreatEntryMetadata = Class(TGoogleBaseObject)
  Private
    Fentries : TThreatEntryMetadataTypeentriesArray;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; const AValue : TThreatEntryMetadataTypeentriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TThreatEntryMetadataTypeentriesArray Index 0 Read Fentries Write Setentries;
  end;
  TThreatEntryMetadataClass = Class of TThreatEntryMetadata;
  
  { --------------------------------------------------------------------
    TMetadataEntry
    --------------------------------------------------------------------}
  
  TMetadataEntry = Class(TGoogleBaseObject)
  Private
    Fkey : String;
    Fvalue : String;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property key : String Index 0 Read Fkey Write Setkey;
    Property value : String Index 8 Read Fvalue Write Setvalue;
  end;
  TMetadataEntryClass = Class of TMetadataEntry;
  
  { --------------------------------------------------------------------
    TFetchThreatListUpdatesRequest
    --------------------------------------------------------------------}
  
  TFetchThreatListUpdatesRequest = Class(TGoogleBaseObject)
  Private
    Fclient : TClientInfo;
    FlistUpdateRequests : TFetchThreatListUpdatesRequestTypelistUpdateRequestsArray;
  Protected
    //Property setters
    Procedure Setclient(AIndex : Integer; const AValue : TClientInfo); virtual;
    Procedure SetlistUpdateRequests(AIndex : Integer; const AValue : TFetchThreatListUpdatesRequestTypelistUpdateRequestsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property client : TClientInfo Index 0 Read Fclient Write Setclient;
    Property listUpdateRequests : TFetchThreatListUpdatesRequestTypelistUpdateRequestsArray Index 8 Read FlistUpdateRequests Write SetlistUpdateRequests;
  end;
  TFetchThreatListUpdatesRequestClass = Class of TFetchThreatListUpdatesRequest;
  
  { --------------------------------------------------------------------
    TListUpdateRequest
    --------------------------------------------------------------------}
  
  TListUpdateRequest = Class(TGoogleBaseObject)
  Private
    FthreatType : String;
    FplatformType : String;
    FthreatEntryType : String;
    Fstate : String;
    Fconstraints : TConstraints;
  Protected
    //Property setters
    Procedure SetthreatType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetplatformType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetthreatEntryType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstate(AIndex : Integer; const AValue : String); virtual;
    Procedure Setconstraints(AIndex : Integer; const AValue : TConstraints); virtual;
  Public
  Published
    Property threatType : String Index 0 Read FthreatType Write SetthreatType;
    Property platformType : String Index 8 Read FplatformType Write SetplatformType;
    Property threatEntryType : String Index 16 Read FthreatEntryType Write SetthreatEntryType;
    Property state : String Index 24 Read Fstate Write Setstate;
    Property constraints : TConstraints Index 32 Read Fconstraints Write Setconstraints;
  end;
  TListUpdateRequestClass = Class of TListUpdateRequest;
  
  { --------------------------------------------------------------------
    TConstraints
    --------------------------------------------------------------------}
  
  TConstraints = Class(TGoogleBaseObject)
  Private
    FmaxUpdateEntries : integer;
    FmaxDatabaseEntries : integer;
    Fregion : String;
    FsupportedCompressions : TStringArray;
  Protected
    //Property setters
    Procedure SetmaxUpdateEntries(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetmaxDatabaseEntries(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setregion(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsupportedCompressions(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property maxUpdateEntries : integer Index 0 Read FmaxUpdateEntries Write SetmaxUpdateEntries;
    Property maxDatabaseEntries : integer Index 8 Read FmaxDatabaseEntries Write SetmaxDatabaseEntries;
    Property region : String Index 16 Read Fregion Write Setregion;
    Property supportedCompressions : TStringArray Index 24 Read FsupportedCompressions Write SetsupportedCompressions;
  end;
  TConstraintsClass = Class of TConstraints;
  
  { --------------------------------------------------------------------
    TFetchThreatListUpdatesResponse
    --------------------------------------------------------------------}
  
  TFetchThreatListUpdatesResponse = Class(TGoogleBaseObject)
  Private
    FlistUpdateResponses : TFetchThreatListUpdatesResponseTypelistUpdateResponsesArray;
    FminimumWaitDuration : String;
  Protected
    //Property setters
    Procedure SetlistUpdateResponses(AIndex : Integer; const AValue : TFetchThreatListUpdatesResponseTypelistUpdateResponsesArray); virtual;
    Procedure SetminimumWaitDuration(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property listUpdateResponses : TFetchThreatListUpdatesResponseTypelistUpdateResponsesArray Index 0 Read FlistUpdateResponses Write SetlistUpdateResponses;
    Property minimumWaitDuration : String Index 8 Read FminimumWaitDuration Write SetminimumWaitDuration;
  end;
  TFetchThreatListUpdatesResponseClass = Class of TFetchThreatListUpdatesResponse;
  
  { --------------------------------------------------------------------
    TListUpdateResponse
    --------------------------------------------------------------------}
  
  TListUpdateResponse = Class(TGoogleBaseObject)
  Private
    FthreatType : String;
    FthreatEntryType : String;
    FplatformType : String;
    FresponseType : String;
    Fadditions : TListUpdateResponseTypeadditionsArray;
    Fremovals : TListUpdateResponseTyperemovalsArray;
    FnewClientState : String;
    Fchecksum : TChecksum;
  Protected
    //Property setters
    Procedure SetthreatType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetthreatEntryType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetplatformType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetresponseType(AIndex : Integer; const AValue : String); virtual;
    Procedure Setadditions(AIndex : Integer; const AValue : TListUpdateResponseTypeadditionsArray); virtual;
    Procedure Setremovals(AIndex : Integer; const AValue : TListUpdateResponseTyperemovalsArray); virtual;
    Procedure SetnewClientState(AIndex : Integer; const AValue : String); virtual;
    Procedure Setchecksum(AIndex : Integer; const AValue : TChecksum); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property threatType : String Index 0 Read FthreatType Write SetthreatType;
    Property threatEntryType : String Index 8 Read FthreatEntryType Write SetthreatEntryType;
    Property platformType : String Index 16 Read FplatformType Write SetplatformType;
    Property responseType : String Index 24 Read FresponseType Write SetresponseType;
    Property additions : TListUpdateResponseTypeadditionsArray Index 32 Read Fadditions Write Setadditions;
    Property removals : TListUpdateResponseTyperemovalsArray Index 40 Read Fremovals Write Setremovals;
    Property newClientState : String Index 48 Read FnewClientState Write SetnewClientState;
    Property checksum : TChecksum Index 56 Read Fchecksum Write Setchecksum;
  end;
  TListUpdateResponseClass = Class of TListUpdateResponse;
  
  { --------------------------------------------------------------------
    TThreatEntrySet
    --------------------------------------------------------------------}
  
  TThreatEntrySet = Class(TGoogleBaseObject)
  Private
    FcompressionType : String;
    FrawHashes : TRawHashes;
    FrawIndices : TRawIndices;
    FriceHashes : TRiceDeltaEncoding;
    FriceIndices : TRiceDeltaEncoding;
  Protected
    //Property setters
    Procedure SetcompressionType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetrawHashes(AIndex : Integer; const AValue : TRawHashes); virtual;
    Procedure SetrawIndices(AIndex : Integer; const AValue : TRawIndices); virtual;
    Procedure SetriceHashes(AIndex : Integer; const AValue : TRiceDeltaEncoding); virtual;
    Procedure SetriceIndices(AIndex : Integer; const AValue : TRiceDeltaEncoding); virtual;
  Public
  Published
    Property compressionType : String Index 0 Read FcompressionType Write SetcompressionType;
    Property rawHashes : TRawHashes Index 8 Read FrawHashes Write SetrawHashes;
    Property rawIndices : TRawIndices Index 16 Read FrawIndices Write SetrawIndices;
    Property riceHashes : TRiceDeltaEncoding Index 24 Read FriceHashes Write SetriceHashes;
    Property riceIndices : TRiceDeltaEncoding Index 32 Read FriceIndices Write SetriceIndices;
  end;
  TThreatEntrySetClass = Class of TThreatEntrySet;
  
  { --------------------------------------------------------------------
    TRawHashes
    --------------------------------------------------------------------}
  
  TRawHashes = Class(TGoogleBaseObject)
  Private
    FprefixSize : integer;
    FrawHashes : String;
  Protected
    //Property setters
    Procedure SetprefixSize(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetrawHashes(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property prefixSize : integer Index 0 Read FprefixSize Write SetprefixSize;
    Property rawHashes : String Index 8 Read FrawHashes Write SetrawHashes;
  end;
  TRawHashesClass = Class of TRawHashes;
  
  { --------------------------------------------------------------------
    TRawIndices
    --------------------------------------------------------------------}
  
  TRawIndices = Class(TGoogleBaseObject)
  Private
    Findices : TintegerArray;
  Protected
    //Property setters
    Procedure Setindices(AIndex : Integer; const AValue : TintegerArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property indices : TintegerArray Index 0 Read Findices Write Setindices;
  end;
  TRawIndicesClass = Class of TRawIndices;
  
  { --------------------------------------------------------------------
    TRiceDeltaEncoding
    --------------------------------------------------------------------}
  
  TRiceDeltaEncoding = Class(TGoogleBaseObject)
  Private
    FfirstValue : String;
    FriceParameter : integer;
    FnumEntries : integer;
    FencodedData : String;
  Protected
    //Property setters
    Procedure SetfirstValue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetriceParameter(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetnumEntries(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetencodedData(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property firstValue : String Index 0 Read FfirstValue Write SetfirstValue;
    Property riceParameter : integer Index 8 Read FriceParameter Write SetriceParameter;
    Property numEntries : integer Index 16 Read FnumEntries Write SetnumEntries;
    Property encodedData : String Index 24 Read FencodedData Write SetencodedData;
  end;
  TRiceDeltaEncodingClass = Class of TRiceDeltaEncoding;
  
  { --------------------------------------------------------------------
    TChecksum
    --------------------------------------------------------------------}
  
  TChecksum = Class(TGoogleBaseObject)
  Private
    Fsha256 : String;
  Protected
    //Property setters
    Procedure Setsha256(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property sha256 : String Index 0 Read Fsha256 Write Setsha256;
  end;
  TChecksumClass = Class of TChecksum;
  
  { --------------------------------------------------------------------
    TFindFullHashesRequest
    --------------------------------------------------------------------}
  
  TFindFullHashesRequest = Class(TGoogleBaseObject)
  Private
    Fclient : TClientInfo;
    FclientStates : TStringArray;
    FthreatInfo : TThreatInfo;
  Protected
    //Property setters
    Procedure Setclient(AIndex : Integer; const AValue : TClientInfo); virtual;
    Procedure SetclientStates(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetthreatInfo(AIndex : Integer; const AValue : TThreatInfo); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property client : TClientInfo Index 0 Read Fclient Write Setclient;
    Property clientStates : TStringArray Index 8 Read FclientStates Write SetclientStates;
    Property threatInfo : TThreatInfo Index 16 Read FthreatInfo Write SetthreatInfo;
  end;
  TFindFullHashesRequestClass = Class of TFindFullHashesRequest;
  
  { --------------------------------------------------------------------
    TFindFullHashesResponse
    --------------------------------------------------------------------}
  
  TFindFullHashesResponse = Class(TGoogleBaseObject)
  Private
    Fmatches : TFindFullHashesResponseTypematchesArray;
    FminimumWaitDuration : String;
    FnegativeCacheDuration : String;
  Protected
    //Property setters
    Procedure Setmatches(AIndex : Integer; const AValue : TFindFullHashesResponseTypematchesArray); virtual;
    Procedure SetminimumWaitDuration(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnegativeCacheDuration(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property matches : TFindFullHashesResponseTypematchesArray Index 0 Read Fmatches Write Setmatches;
    Property minimumWaitDuration : String Index 8 Read FminimumWaitDuration Write SetminimumWaitDuration;
    Property negativeCacheDuration : String Index 16 Read FnegativeCacheDuration Write SetnegativeCacheDuration;
  end;
  TFindFullHashesResponseClass = Class of TFindFullHashesResponse;
  
  { --------------------------------------------------------------------
    TListThreatListsResponse
    --------------------------------------------------------------------}
  
  TListThreatListsResponse = Class(TGoogleBaseObject)
  Private
    FthreatLists : TListThreatListsResponseTypethreatListsArray;
  Protected
    //Property setters
    Procedure SetthreatLists(AIndex : Integer; const AValue : TListThreatListsResponseTypethreatListsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property threatLists : TListThreatListsResponseTypethreatListsArray Index 0 Read FthreatLists Write SetthreatLists;
  end;
  TListThreatListsResponseClass = Class of TListThreatListsResponse;
  
  { --------------------------------------------------------------------
    TThreatListDescriptor
    --------------------------------------------------------------------}
  
  TThreatListDescriptor = Class(TGoogleBaseObject)
  Private
    FthreatType : String;
    FplatformType : String;
    FthreatEntryType : String;
  Protected
    //Property setters
    Procedure SetthreatType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetplatformType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetthreatEntryType(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property threatType : String Index 0 Read FthreatType Write SetthreatType;
    Property platformType : String Index 8 Read FplatformType Write SetplatformType;
    Property threatEntryType : String Index 16 Read FthreatEntryType Write SetthreatEntryType;
  end;
  TThreatListDescriptorClass = Class of TThreatListDescriptor;
  
  { --------------------------------------------------------------------
    TThreatMatchesResource
    --------------------------------------------------------------------}
  
  TThreatMatchesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Find(aFindThreatMatchesRequest : TFindThreatMatchesRequest) : TFindThreatMatchesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TThreatListUpdatesResource
    --------------------------------------------------------------------}
  
  TThreatListUpdatesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Fetch(aFetchThreatListUpdatesRequest : TFetchThreatListUpdatesRequest) : TFetchThreatListUpdatesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TFullHashesResource
    --------------------------------------------------------------------}
  
  TFullHashesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Find(aFindFullHashesRequest : TFindFullHashesRequest) : TFindFullHashesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TThreatListsResource
    --------------------------------------------------------------------}
  
  TThreatListsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List : TListThreatListsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TSafebrowsingAPI
    --------------------------------------------------------------------}
  
  TSafebrowsingAPI = Class(TGoogleAPI)
  Private
    FThreatMatchesInstance : TThreatMatchesResource;
    FThreatListUpdatesInstance : TThreatListUpdatesResource;
    FFullHashesInstance : TFullHashesResource;
    FThreatListsInstance : TThreatListsResource;
    Function GetThreatMatchesInstance : TThreatMatchesResource;virtual;
    Function GetThreatListUpdatesInstance : TThreatListUpdatesResource;virtual;
    Function GetFullHashesInstance : TFullHashesResource;virtual;
    Function GetThreatListsInstance : TThreatListsResource;virtual;
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
    Function CreateThreatMatchesResource(AOwner : TComponent) : TThreatMatchesResource;virtual;overload;
    Function CreateThreatMatchesResource : TThreatMatchesResource;virtual;overload;
    Function CreateThreatListUpdatesResource(AOwner : TComponent) : TThreatListUpdatesResource;virtual;overload;
    Function CreateThreatListUpdatesResource : TThreatListUpdatesResource;virtual;overload;
    Function CreateFullHashesResource(AOwner : TComponent) : TFullHashesResource;virtual;overload;
    Function CreateFullHashesResource : TFullHashesResource;virtual;overload;
    Function CreateThreatListsResource(AOwner : TComponent) : TThreatListsResource;virtual;overload;
    Function CreateThreatListsResource : TThreatListsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ThreatMatchesResource : TThreatMatchesResource Read GetThreatMatchesInstance;
    Property ThreatListUpdatesResource : TThreatListUpdatesResource Read GetThreatListUpdatesInstance;
    Property FullHashesResource : TFullHashesResource Read GetFullHashesInstance;
    Property ThreatListsResource : TThreatListsResource Read GetThreatListsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TFindThreatMatchesRequest
  --------------------------------------------------------------------}


Procedure TFindThreatMatchesRequest.Setclient(AIndex : Integer; const AValue : TClientInfo); 

begin
  If (Fclient=AValue) then exit;
  Fclient:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindThreatMatchesRequest.SetthreatInfo(AIndex : Integer; const AValue : TThreatInfo); 

begin
  If (FthreatInfo=AValue) then exit;
  FthreatInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TClientInfo
  --------------------------------------------------------------------}


Procedure TClientInfo.SetclientId(AIndex : Integer; const AValue : String); 

begin
  If (FclientId=AValue) then exit;
  FclientId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClientInfo.SetclientVersion(AIndex : Integer; const AValue : String); 

begin
  If (FclientVersion=AValue) then exit;
  FclientVersion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TThreatInfo
  --------------------------------------------------------------------}


Procedure TThreatInfo.SetthreatTypes(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FthreatTypes=AValue) then exit;
  FthreatTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThreatInfo.SetplatformTypes(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FplatformTypes=AValue) then exit;
  FplatformTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThreatInfo.SetthreatEntryTypes(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FthreatEntryTypes=AValue) then exit;
  FthreatEntryTypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThreatInfo.SetthreatEntries(AIndex : Integer; const AValue : TThreatInfoTypethreatEntriesArray); 

begin
  If (FthreatEntries=AValue) then exit;
  FthreatEntries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TThreatInfo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'threattypes' : SetLength(FthreatTypes,ALength);
  'platformtypes' : SetLength(FplatformTypes,ALength);
  'threatentrytypes' : SetLength(FthreatEntryTypes,ALength);
  'threatentries' : SetLength(FthreatEntries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TThreatEntry
  --------------------------------------------------------------------}


Procedure TThreatEntry.Sethash(AIndex : Integer; const AValue : String); 

begin
  If (Fhash=AValue) then exit;
  Fhash:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThreatEntry.Seturl(AIndex : Integer; const AValue : String); 

begin
  If (Furl=AValue) then exit;
  Furl:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThreatEntry.Setdigest(AIndex : Integer; const AValue : String); 

begin
  If (Fdigest=AValue) then exit;
  Fdigest:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFindThreatMatchesResponse
  --------------------------------------------------------------------}


Procedure TFindThreatMatchesResponse.Setmatches(AIndex : Integer; const AValue : TFindThreatMatchesResponseTypematchesArray); 

begin
  If (Fmatches=AValue) then exit;
  Fmatches:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFindThreatMatchesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'matches' : SetLength(Fmatches,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TThreatMatch
  --------------------------------------------------------------------}


Procedure TThreatMatch.SetthreatType(AIndex : Integer; const AValue : String); 

begin
  If (FthreatType=AValue) then exit;
  FthreatType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThreatMatch.SetplatformType(AIndex : Integer; const AValue : String); 

begin
  If (FplatformType=AValue) then exit;
  FplatformType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThreatMatch.SetthreatEntryType(AIndex : Integer; const AValue : String); 

begin
  If (FthreatEntryType=AValue) then exit;
  FthreatEntryType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThreatMatch.Setthreat(AIndex : Integer; const AValue : TThreatEntry); 

begin
  If (Fthreat=AValue) then exit;
  Fthreat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThreatMatch.SetthreatEntryMetadata(AIndex : Integer; const AValue : TThreatEntryMetadata); 

begin
  If (FthreatEntryMetadata=AValue) then exit;
  FthreatEntryMetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThreatMatch.SetcacheDuration(AIndex : Integer; const AValue : String); 

begin
  If (FcacheDuration=AValue) then exit;
  FcacheDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TThreatEntryMetadata
  --------------------------------------------------------------------}


Procedure TThreatEntryMetadata.Setentries(AIndex : Integer; const AValue : TThreatEntryMetadataTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TThreatEntryMetadata.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMetadataEntry
  --------------------------------------------------------------------}


Procedure TMetadataEntry.Setkey(AIndex : Integer; const AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadataEntry.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFetchThreatListUpdatesRequest
  --------------------------------------------------------------------}


Procedure TFetchThreatListUpdatesRequest.Setclient(AIndex : Integer; const AValue : TClientInfo); 

begin
  If (Fclient=AValue) then exit;
  Fclient:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFetchThreatListUpdatesRequest.SetlistUpdateRequests(AIndex : Integer; const AValue : TFetchThreatListUpdatesRequestTypelistUpdateRequestsArray); 

begin
  If (FlistUpdateRequests=AValue) then exit;
  FlistUpdateRequests:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFetchThreatListUpdatesRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'listupdaterequests' : SetLength(FlistUpdateRequests,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListUpdateRequest
  --------------------------------------------------------------------}


Procedure TListUpdateRequest.SetthreatType(AIndex : Integer; const AValue : String); 

begin
  If (FthreatType=AValue) then exit;
  FthreatType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListUpdateRequest.SetplatformType(AIndex : Integer; const AValue : String); 

begin
  If (FplatformType=AValue) then exit;
  FplatformType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListUpdateRequest.SetthreatEntryType(AIndex : Integer; const AValue : String); 

begin
  If (FthreatEntryType=AValue) then exit;
  FthreatEntryType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListUpdateRequest.Setstate(AIndex : Integer; const AValue : String); 

begin
  If (Fstate=AValue) then exit;
  Fstate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListUpdateRequest.Setconstraints(AIndex : Integer; const AValue : TConstraints); 

begin
  If (Fconstraints=AValue) then exit;
  Fconstraints:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TConstraints
  --------------------------------------------------------------------}


Procedure TConstraints.SetmaxUpdateEntries(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxUpdateEntries=AValue) then exit;
  FmaxUpdateEntries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConstraints.SetmaxDatabaseEntries(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxDatabaseEntries=AValue) then exit;
  FmaxDatabaseEntries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConstraints.Setregion(AIndex : Integer; const AValue : String); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TConstraints.SetsupportedCompressions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FsupportedCompressions=AValue) then exit;
  FsupportedCompressions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TConstraints.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'supportedcompressions' : SetLength(FsupportedCompressions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TFetchThreatListUpdatesResponse
  --------------------------------------------------------------------}


Procedure TFetchThreatListUpdatesResponse.SetlistUpdateResponses(AIndex : Integer; const AValue : TFetchThreatListUpdatesResponseTypelistUpdateResponsesArray); 

begin
  If (FlistUpdateResponses=AValue) then exit;
  FlistUpdateResponses:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFetchThreatListUpdatesResponse.SetminimumWaitDuration(AIndex : Integer; const AValue : String); 

begin
  If (FminimumWaitDuration=AValue) then exit;
  FminimumWaitDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFetchThreatListUpdatesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'listupdateresponses' : SetLength(FlistUpdateResponses,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListUpdateResponse
  --------------------------------------------------------------------}


Procedure TListUpdateResponse.SetthreatType(AIndex : Integer; const AValue : String); 

begin
  If (FthreatType=AValue) then exit;
  FthreatType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListUpdateResponse.SetthreatEntryType(AIndex : Integer; const AValue : String); 

begin
  If (FthreatEntryType=AValue) then exit;
  FthreatEntryType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListUpdateResponse.SetplatformType(AIndex : Integer; const AValue : String); 

begin
  If (FplatformType=AValue) then exit;
  FplatformType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListUpdateResponse.SetresponseType(AIndex : Integer; const AValue : String); 

begin
  If (FresponseType=AValue) then exit;
  FresponseType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListUpdateResponse.Setadditions(AIndex : Integer; const AValue : TListUpdateResponseTypeadditionsArray); 

begin
  If (Fadditions=AValue) then exit;
  Fadditions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListUpdateResponse.Setremovals(AIndex : Integer; const AValue : TListUpdateResponseTyperemovalsArray); 

begin
  If (Fremovals=AValue) then exit;
  Fremovals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListUpdateResponse.SetnewClientState(AIndex : Integer; const AValue : String); 

begin
  If (FnewClientState=AValue) then exit;
  FnewClientState:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListUpdateResponse.Setchecksum(AIndex : Integer; const AValue : TChecksum); 

begin
  If (Fchecksum=AValue) then exit;
  Fchecksum:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListUpdateResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'additions' : SetLength(Fadditions,ALength);
  'removals' : SetLength(Fremovals,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TThreatEntrySet
  --------------------------------------------------------------------}


Procedure TThreatEntrySet.SetcompressionType(AIndex : Integer; const AValue : String); 

begin
  If (FcompressionType=AValue) then exit;
  FcompressionType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThreatEntrySet.SetrawHashes(AIndex : Integer; const AValue : TRawHashes); 

begin
  If (FrawHashes=AValue) then exit;
  FrawHashes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThreatEntrySet.SetrawIndices(AIndex : Integer; const AValue : TRawIndices); 

begin
  If (FrawIndices=AValue) then exit;
  FrawIndices:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThreatEntrySet.SetriceHashes(AIndex : Integer; const AValue : TRiceDeltaEncoding); 

begin
  If (FriceHashes=AValue) then exit;
  FriceHashes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThreatEntrySet.SetriceIndices(AIndex : Integer; const AValue : TRiceDeltaEncoding); 

begin
  If (FriceIndices=AValue) then exit;
  FriceIndices:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRawHashes
  --------------------------------------------------------------------}


Procedure TRawHashes.SetprefixSize(AIndex : Integer; const AValue : integer); 

begin
  If (FprefixSize=AValue) then exit;
  FprefixSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRawHashes.SetrawHashes(AIndex : Integer; const AValue : String); 

begin
  If (FrawHashes=AValue) then exit;
  FrawHashes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRawIndices
  --------------------------------------------------------------------}


Procedure TRawIndices.Setindices(AIndex : Integer; const AValue : TintegerArray); 

begin
  If (Findices=AValue) then exit;
  Findices:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TRawIndices.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'indices' : SetLength(Findices,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TRiceDeltaEncoding
  --------------------------------------------------------------------}


Procedure TRiceDeltaEncoding.SetfirstValue(AIndex : Integer; const AValue : String); 

begin
  If (FfirstValue=AValue) then exit;
  FfirstValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRiceDeltaEncoding.SetriceParameter(AIndex : Integer; const AValue : integer); 

begin
  If (FriceParameter=AValue) then exit;
  FriceParameter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRiceDeltaEncoding.SetnumEntries(AIndex : Integer; const AValue : integer); 

begin
  If (FnumEntries=AValue) then exit;
  FnumEntries:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRiceDeltaEncoding.SetencodedData(AIndex : Integer; const AValue : String); 

begin
  If (FencodedData=AValue) then exit;
  FencodedData:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TChecksum
  --------------------------------------------------------------------}


Procedure TChecksum.Setsha256(AIndex : Integer; const AValue : String); 

begin
  If (Fsha256=AValue) then exit;
  Fsha256:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFindFullHashesRequest
  --------------------------------------------------------------------}


Procedure TFindFullHashesRequest.Setclient(AIndex : Integer; const AValue : TClientInfo); 

begin
  If (Fclient=AValue) then exit;
  Fclient:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindFullHashesRequest.SetclientStates(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FclientStates=AValue) then exit;
  FclientStates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindFullHashesRequest.SetthreatInfo(AIndex : Integer; const AValue : TThreatInfo); 

begin
  If (FthreatInfo=AValue) then exit;
  FthreatInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFindFullHashesRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'clientstates' : SetLength(FclientStates,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TFindFullHashesResponse
  --------------------------------------------------------------------}


Procedure TFindFullHashesResponse.Setmatches(AIndex : Integer; const AValue : TFindFullHashesResponseTypematchesArray); 

begin
  If (Fmatches=AValue) then exit;
  Fmatches:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindFullHashesResponse.SetminimumWaitDuration(AIndex : Integer; const AValue : String); 

begin
  If (FminimumWaitDuration=AValue) then exit;
  FminimumWaitDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFindFullHashesResponse.SetnegativeCacheDuration(AIndex : Integer; const AValue : String); 

begin
  If (FnegativeCacheDuration=AValue) then exit;
  FnegativeCacheDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFindFullHashesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'matches' : SetLength(Fmatches,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListThreatListsResponse
  --------------------------------------------------------------------}


Procedure TListThreatListsResponse.SetthreatLists(AIndex : Integer; const AValue : TListThreatListsResponseTypethreatListsArray); 

begin
  If (FthreatLists=AValue) then exit;
  FthreatLists:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListThreatListsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'threatlists' : SetLength(FthreatLists,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TThreatListDescriptor
  --------------------------------------------------------------------}


Procedure TThreatListDescriptor.SetthreatType(AIndex : Integer; const AValue : String); 

begin
  If (FthreatType=AValue) then exit;
  FthreatType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThreatListDescriptor.SetplatformType(AIndex : Integer; const AValue : String); 

begin
  If (FplatformType=AValue) then exit;
  FplatformType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TThreatListDescriptor.SetthreatEntryType(AIndex : Integer; const AValue : String); 

begin
  If (FthreatEntryType=AValue) then exit;
  FthreatEntryType:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TThreatMatchesResource
  --------------------------------------------------------------------}


Class Function TThreatMatchesResource.ResourceName : String;

begin
  Result:='threatMatches';
end;

Class Function TThreatMatchesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TsafebrowsingAPI;
end;

Function TThreatMatchesResource.Find(aFindThreatMatchesRequest : TFindThreatMatchesRequest) : TFindThreatMatchesResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v4/threatMatches:find';
  _Methodid   = 'safebrowsing.threatMatches.find';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aFindThreatMatchesRequest,TFindThreatMatchesResponse) as TFindThreatMatchesResponse;
end;



{ --------------------------------------------------------------------
  TThreatListUpdatesResource
  --------------------------------------------------------------------}


Class Function TThreatListUpdatesResource.ResourceName : String;

begin
  Result:='threatListUpdates';
end;

Class Function TThreatListUpdatesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TsafebrowsingAPI;
end;

Function TThreatListUpdatesResource.Fetch(aFetchThreatListUpdatesRequest : TFetchThreatListUpdatesRequest) : TFetchThreatListUpdatesResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v4/threatListUpdates:fetch';
  _Methodid   = 'safebrowsing.threatListUpdates.fetch';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aFetchThreatListUpdatesRequest,TFetchThreatListUpdatesResponse) as TFetchThreatListUpdatesResponse;
end;



{ --------------------------------------------------------------------
  TFullHashesResource
  --------------------------------------------------------------------}


Class Function TFullHashesResource.ResourceName : String;

begin
  Result:='fullHashes';
end;

Class Function TFullHashesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TsafebrowsingAPI;
end;

Function TFullHashesResource.Find(aFindFullHashesRequest : TFindFullHashesRequest) : TFindFullHashesResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v4/fullHashes:find';
  _Methodid   = 'safebrowsing.fullHashes.find';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aFindFullHashesRequest,TFindFullHashesResponse) as TFindFullHashesResponse;
end;



{ --------------------------------------------------------------------
  TThreatListsResource
  --------------------------------------------------------------------}


Class Function TThreatListsResource.ResourceName : String;

begin
  Result:='threatLists';
end;

Class Function TThreatListsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TsafebrowsingAPI;
end;

Function TThreatListsResource.List : TListThreatListsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v4/threatLists';
  _Methodid   = 'safebrowsing.threatLists.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',Nil,TListThreatListsResponse) as TListThreatListsResponse;
end;



{ --------------------------------------------------------------------
  TSafebrowsingAPI
  --------------------------------------------------------------------}

Class Function TSafebrowsingAPI.APIName : String;

begin
  Result:='safebrowsing';
end;

Class Function TSafebrowsingAPI.APIVersion : String;

begin
  Result:='v4';
end;

Class Function TSafebrowsingAPI.APIRevision : String;

begin
  Result:='20160520';
end;

Class Function TSafebrowsingAPI.APIID : String;

begin
  Result:='safebrowsing:v4';
end;

Class Function TSafebrowsingAPI.APITitle : String;

begin
  Result:='Safe Browsing APIs';
end;

Class Function TSafebrowsingAPI.APIDescription : String;

begin
  Result:='Enables client applications to check web resources (most commonly URLs) against Google-generated lists of unsafe web resources.';
end;

Class Function TSafebrowsingAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TSafebrowsingAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TSafebrowsingAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TSafebrowsingAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TSafebrowsingAPI.APIdocumentationLink : String;

begin
  Result:='https://developers.google.com/safe-browsing/';
end;

Class Function TSafebrowsingAPI.APIrootUrl : string;

begin
  Result:='https://safebrowsing.googleapis.com/';
end;

Class Function TSafebrowsingAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TSafebrowsingAPI.APIbaseURL : String;

begin
  Result:='https://safebrowsing.googleapis.com/';
end;

Class Function TSafebrowsingAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TSafebrowsingAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TSafebrowsingAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TSafebrowsingAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TSafebrowsingAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TSafebrowsingAPI.RegisterAPIResources;

begin
  TFindThreatMatchesRequest.RegisterObject;
  TClientInfo.RegisterObject;
  TThreatInfo.RegisterObject;
  TThreatEntry.RegisterObject;
  TFindThreatMatchesResponse.RegisterObject;
  TThreatMatch.RegisterObject;
  TThreatEntryMetadata.RegisterObject;
  TMetadataEntry.RegisterObject;
  TFetchThreatListUpdatesRequest.RegisterObject;
  TListUpdateRequest.RegisterObject;
  TConstraints.RegisterObject;
  TFetchThreatListUpdatesResponse.RegisterObject;
  TListUpdateResponse.RegisterObject;
  TThreatEntrySet.RegisterObject;
  TRawHashes.RegisterObject;
  TRawIndices.RegisterObject;
  TRiceDeltaEncoding.RegisterObject;
  TChecksum.RegisterObject;
  TFindFullHashesRequest.RegisterObject;
  TFindFullHashesResponse.RegisterObject;
  TListThreatListsResponse.RegisterObject;
  TThreatListDescriptor.RegisterObject;
end;


Function TSafebrowsingAPI.GetThreatMatchesInstance : TThreatMatchesResource;

begin
  if (FThreatMatchesInstance=Nil) then
    FThreatMatchesInstance:=CreateThreatMatchesResource;
  Result:=FThreatMatchesInstance;
end;

Function TSafebrowsingAPI.CreateThreatMatchesResource : TThreatMatchesResource;

begin
  Result:=CreateThreatMatchesResource(Self);
end;


Function TSafebrowsingAPI.CreateThreatMatchesResource(AOwner : TComponent) : TThreatMatchesResource;

begin
  Result:=TThreatMatchesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TSafebrowsingAPI.GetThreatListUpdatesInstance : TThreatListUpdatesResource;

begin
  if (FThreatListUpdatesInstance=Nil) then
    FThreatListUpdatesInstance:=CreateThreatListUpdatesResource;
  Result:=FThreatListUpdatesInstance;
end;

Function TSafebrowsingAPI.CreateThreatListUpdatesResource : TThreatListUpdatesResource;

begin
  Result:=CreateThreatListUpdatesResource(Self);
end;


Function TSafebrowsingAPI.CreateThreatListUpdatesResource(AOwner : TComponent) : TThreatListUpdatesResource;

begin
  Result:=TThreatListUpdatesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TSafebrowsingAPI.GetFullHashesInstance : TFullHashesResource;

begin
  if (FFullHashesInstance=Nil) then
    FFullHashesInstance:=CreateFullHashesResource;
  Result:=FFullHashesInstance;
end;

Function TSafebrowsingAPI.CreateFullHashesResource : TFullHashesResource;

begin
  Result:=CreateFullHashesResource(Self);
end;


Function TSafebrowsingAPI.CreateFullHashesResource(AOwner : TComponent) : TFullHashesResource;

begin
  Result:=TFullHashesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TSafebrowsingAPI.GetThreatListsInstance : TThreatListsResource;

begin
  if (FThreatListsInstance=Nil) then
    FThreatListsInstance:=CreateThreatListsResource;
  Result:=FThreatListsInstance;
end;

Function TSafebrowsingAPI.CreateThreatListsResource : TThreatListsResource;

begin
  Result:=CreateThreatListsResource(Self);
end;


Function TSafebrowsingAPI.CreateThreatListsResource(AOwner : TComponent) : TThreatListsResource;

begin
  Result:=TThreatListsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TSafebrowsingAPI.RegisterAPI;
end.
