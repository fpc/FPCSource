unit googlespectrum;
{
   **********************************************************************
      This file is part of the Free Component Library (FCL)
      Copyright (c) 2015 The free pascal team.
  
      See the file COPYING.FPC, included in this distribution,
      for details about the copyright.
  
      This program is distributed in the hope that it will be useful,
      but WITHOUT ANY WARRANTY; without even the implied warranty of
      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  
   **********************************************************************
}
//Generated on: 16-5-15 08:53:08
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAntennaCharacteristics = Class;
  TDatabaseSpec = Class;
  TDbUpdateSpec = Class;
  TDeviceCapabilities = Class;
  TDeviceDescriptor = Class;
  TDeviceOwner = Class;
  TDeviceValidity = Class;
  TEventTime = Class;
  TFrequencyRange = Class;
  TGeoLocation = Class;
  TGeoLocationEllipse = Class;
  TGeoLocationPoint = Class;
  TGeoLocationPolygon = Class;
  TGeoSpectrumSchedule = Class;
  TPawsGetSpectrumBatchRequest = Class;
  TPawsGetSpectrumBatchResponse = Class;
  TPawsGetSpectrumRequest = Class;
  TPawsGetSpectrumResponse = Class;
  TPawsInitRequest = Class;
  TPawsInitResponse = Class;
  TPawsNotifySpectrumUseRequest = Class;
  TPawsNotifySpectrumUseResponse = Class;
  TPawsRegisterRequest = Class;
  TPawsRegisterResponse = Class;
  TPawsVerifyDeviceRequest = Class;
  TPawsVerifyDeviceResponse = Class;
  TRulesetInfo = Class;
  TSpectrumMessage = Class;
  TSpectrumSchedule = Class;
  TVcard = Class;
  TVcardAddress = Class;
  TVcardTelephone = Class;
  TVcardTypedText = Class;
  TAntennaCharacteristicsArray = Array of TAntennaCharacteristics;
  TDatabaseSpecArray = Array of TDatabaseSpec;
  TDbUpdateSpecArray = Array of TDbUpdateSpec;
  TDeviceCapabilitiesArray = Array of TDeviceCapabilities;
  TDeviceDescriptorArray = Array of TDeviceDescriptor;
  TDeviceOwnerArray = Array of TDeviceOwner;
  TDeviceValidityArray = Array of TDeviceValidity;
  TEventTimeArray = Array of TEventTime;
  TFrequencyRangeArray = Array of TFrequencyRange;
  TGeoLocationArray = Array of TGeoLocation;
  TGeoLocationEllipseArray = Array of TGeoLocationEllipse;
  TGeoLocationPointArray = Array of TGeoLocationPoint;
  TGeoLocationPolygonArray = Array of TGeoLocationPolygon;
  TGeoSpectrumScheduleArray = Array of TGeoSpectrumSchedule;
  TPawsGetSpectrumBatchRequestArray = Array of TPawsGetSpectrumBatchRequest;
  TPawsGetSpectrumBatchResponseArray = Array of TPawsGetSpectrumBatchResponse;
  TPawsGetSpectrumRequestArray = Array of TPawsGetSpectrumRequest;
  TPawsGetSpectrumResponseArray = Array of TPawsGetSpectrumResponse;
  TPawsInitRequestArray = Array of TPawsInitRequest;
  TPawsInitResponseArray = Array of TPawsInitResponse;
  TPawsNotifySpectrumUseRequestArray = Array of TPawsNotifySpectrumUseRequest;
  TPawsNotifySpectrumUseResponseArray = Array of TPawsNotifySpectrumUseResponse;
  TPawsRegisterRequestArray = Array of TPawsRegisterRequest;
  TPawsRegisterResponseArray = Array of TPawsRegisterResponse;
  TPawsVerifyDeviceRequestArray = Array of TPawsVerifyDeviceRequest;
  TPawsVerifyDeviceResponseArray = Array of TPawsVerifyDeviceResponse;
  TRulesetInfoArray = Array of TRulesetInfo;
  TSpectrumMessageArray = Array of TSpectrumMessage;
  TSpectrumScheduleArray = Array of TSpectrumSchedule;
  TVcardArray = Array of TVcard;
  TVcardAddressArray = Array of TVcardAddress;
  TVcardTelephoneArray = Array of TVcardTelephone;
  TVcardTypedTextArray = Array of TVcardTypedText;
  //Anonymous types, using auto-generated names
  TDbUpdateSpecTypedatabasesArray = Array of TDatabaseSpec;
  TDeviceCapabilitiesTypefrequencyRangesArray = Array of TFrequencyRange;
  TGeoLocationPolygonTypeexteriorArray = Array of TGeoLocationPoint;
  TGeoSpectrumScheduleTypespectrumSchedulesArray = Array of TSpectrumSchedule;
  TPawsGetSpectrumBatchRequestTypelocationsArray = Array of TGeoLocation;
  TPawsGetSpectrumBatchResponseTypegeoSpectrumSchedulesArray = Array of TGeoSpectrumSchedule;
  TPawsGetSpectrumResponseTypespectrumSchedulesArray = Array of TSpectrumSchedule;
  TPawsNotifySpectrumUseRequestTypespectraArray = Array of TSpectrumMessage;
  TPawsVerifyDeviceRequestTypedeviceDescsArray = Array of TDeviceDescriptor;
  TPawsVerifyDeviceResponseTypedeviceValiditiesArray = Array of TDeviceValidity;
  TSpectrumMessageTypefrequencyRangesArray = Array of TFrequencyRange;
  TSpectrumScheduleTypespectraArray = Array of TSpectrumMessage;
  
  { --------------------------------------------------------------------
    TAntennaCharacteristics
    --------------------------------------------------------------------}
  
  TAntennaCharacteristics = Class(TGoogleBaseObject)
  Private
    Fheight : double;
    FheightType : String;
    FheightUncertainty : double;
  Protected
    //Property setters
    Procedure Setheight(AIndex : Integer; AValue : double); virtual;
    Procedure SetheightType(AIndex : Integer; AValue : String); virtual;
    Procedure SetheightUncertainty(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property height : double Index 0 Read Fheight Write Setheight;
    Property heightType : String Index 8 Read FheightType Write SetheightType;
    Property heightUncertainty : double Index 16 Read FheightUncertainty Write SetheightUncertainty;
  end;
  TAntennaCharacteristicsClass = Class of TAntennaCharacteristics;
  
  { --------------------------------------------------------------------
    TDatabaseSpec
    --------------------------------------------------------------------}
  
  TDatabaseSpec = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Furi : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Seturi(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property uri : String Index 8 Read Furi Write Seturi;
  end;
  TDatabaseSpecClass = Class of TDatabaseSpec;
  
  { --------------------------------------------------------------------
    TDbUpdateSpec
    --------------------------------------------------------------------}
  
  TDbUpdateSpec = Class(TGoogleBaseObject)
  Private
    Fdatabases : TDbUpdateSpecTypedatabasesArray;
  Protected
    //Property setters
    Procedure Setdatabases(AIndex : Integer; AValue : TDbUpdateSpecTypedatabasesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property databases : TDbUpdateSpecTypedatabasesArray Index 0 Read Fdatabases Write Setdatabases;
  end;
  TDbUpdateSpecClass = Class of TDbUpdateSpec;
  
  { --------------------------------------------------------------------
    TDeviceCapabilities
    --------------------------------------------------------------------}
  
  TDeviceCapabilities = Class(TGoogleBaseObject)
  Private
    FfrequencyRanges : TDeviceCapabilitiesTypefrequencyRangesArray;
  Protected
    //Property setters
    Procedure SetfrequencyRanges(AIndex : Integer; AValue : TDeviceCapabilitiesTypefrequencyRangesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property frequencyRanges : TDeviceCapabilitiesTypefrequencyRangesArray Index 0 Read FfrequencyRanges Write SetfrequencyRanges;
  end;
  TDeviceCapabilitiesClass = Class of TDeviceCapabilities;
  
  { --------------------------------------------------------------------
    TDeviceDescriptor
    --------------------------------------------------------------------}
  
  TDeviceDescriptor = Class(TGoogleBaseObject)
  Private
    FetsiEnDeviceCategory : String;
    FetsiEnDeviceEmissionsClass : String;
    FetsiEnDeviceType : String;
    FetsiEnTechnologyId : String;
    FfccId : String;
    FfccTvbdDeviceType : String;
    FmanufacturerId : String;
    FmodelId : String;
    FrulesetIds : TStringArray;
    FserialNumber : String;
  Protected
    //Property setters
    Procedure SetetsiEnDeviceCategory(AIndex : Integer; AValue : String); virtual;
    Procedure SetetsiEnDeviceEmissionsClass(AIndex : Integer; AValue : String); virtual;
    Procedure SetetsiEnDeviceType(AIndex : Integer; AValue : String); virtual;
    Procedure SetetsiEnTechnologyId(AIndex : Integer; AValue : String); virtual;
    Procedure SetfccId(AIndex : Integer; AValue : String); virtual;
    Procedure SetfccTvbdDeviceType(AIndex : Integer; AValue : String); virtual;
    Procedure SetmanufacturerId(AIndex : Integer; AValue : String); virtual;
    Procedure SetmodelId(AIndex : Integer; AValue : String); virtual;
    Procedure SetrulesetIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetserialNumber(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property etsiEnDeviceCategory : String Index 0 Read FetsiEnDeviceCategory Write SetetsiEnDeviceCategory;
    Property etsiEnDeviceEmissionsClass : String Index 8 Read FetsiEnDeviceEmissionsClass Write SetetsiEnDeviceEmissionsClass;
    Property etsiEnDeviceType : String Index 16 Read FetsiEnDeviceType Write SetetsiEnDeviceType;
    Property etsiEnTechnologyId : String Index 24 Read FetsiEnTechnologyId Write SetetsiEnTechnologyId;
    Property fccId : String Index 32 Read FfccId Write SetfccId;
    Property fccTvbdDeviceType : String Index 40 Read FfccTvbdDeviceType Write SetfccTvbdDeviceType;
    Property manufacturerId : String Index 48 Read FmanufacturerId Write SetmanufacturerId;
    Property modelId : String Index 56 Read FmodelId Write SetmodelId;
    Property rulesetIds : TStringArray Index 64 Read FrulesetIds Write SetrulesetIds;
    Property serialNumber : String Index 72 Read FserialNumber Write SetserialNumber;
  end;
  TDeviceDescriptorClass = Class of TDeviceDescriptor;
  
  { --------------------------------------------------------------------
    TDeviceOwner
    --------------------------------------------------------------------}
  
  TDeviceOwner = Class(TGoogleBaseObject)
  Private
    F_operator : TVcard;
    Fowner : TVcard;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_operator(AIndex : Integer; AValue : TVcard); virtual;
    Procedure Setowner(AIndex : Integer; AValue : TVcard); virtual;
  Public
  Published
    Property _operator : TVcard Index 0 Read F_operator Write Set_operator;
    Property owner : TVcard Index 8 Read Fowner Write Setowner;
  end;
  TDeviceOwnerClass = Class of TDeviceOwner;
  
  { --------------------------------------------------------------------
    TDeviceValidity
    --------------------------------------------------------------------}
  
  TDeviceValidity = Class(TGoogleBaseObject)
  Private
    FdeviceDesc : TDeviceDescriptor;
    FisValid : boolean;
    Freason : String;
  Protected
    //Property setters
    Procedure SetdeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); virtual;
    Procedure SetisValid(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setreason(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property deviceDesc : TDeviceDescriptor Index 0 Read FdeviceDesc Write SetdeviceDesc;
    Property isValid : boolean Index 8 Read FisValid Write SetisValid;
    Property reason : String Index 16 Read Freason Write Setreason;
  end;
  TDeviceValidityClass = Class of TDeviceValidity;
  
  { --------------------------------------------------------------------
    TEventTime
    --------------------------------------------------------------------}
  
  TEventTime = Class(TGoogleBaseObject)
  Private
    FstartTime : String;
    FstopTime : String;
  Protected
    //Property setters
    Procedure SetstartTime(AIndex : Integer; AValue : String); virtual;
    Procedure SetstopTime(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property startTime : String Index 0 Read FstartTime Write SetstartTime;
    Property stopTime : String Index 8 Read FstopTime Write SetstopTime;
  end;
  TEventTimeClass = Class of TEventTime;
  
  { --------------------------------------------------------------------
    TFrequencyRange
    --------------------------------------------------------------------}
  
  TFrequencyRange = Class(TGoogleBaseObject)
  Private
    FchannelId : String;
    FmaxPowerDBm : double;
    FstartHz : double;
    FstopHz : double;
  Protected
    //Property setters
    Procedure SetchannelId(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaxPowerDBm(AIndex : Integer; AValue : double); virtual;
    Procedure SetstartHz(AIndex : Integer; AValue : double); virtual;
    Procedure SetstopHz(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property channelId : String Index 0 Read FchannelId Write SetchannelId;
    Property maxPowerDBm : double Index 8 Read FmaxPowerDBm Write SetmaxPowerDBm;
    Property startHz : double Index 16 Read FstartHz Write SetstartHz;
    Property stopHz : double Index 24 Read FstopHz Write SetstopHz;
  end;
  TFrequencyRangeClass = Class of TFrequencyRange;
  
  { --------------------------------------------------------------------
    TGeoLocation
    --------------------------------------------------------------------}
  
  TGeoLocation = Class(TGoogleBaseObject)
  Private
    Fconfidence : integer;
    Fpoint : TGeoLocationEllipse;
    Fregion : TGeoLocationPolygon;
  Protected
    //Property setters
    Procedure Setconfidence(AIndex : Integer; AValue : integer); virtual;
    Procedure Setpoint(AIndex : Integer; AValue : TGeoLocationEllipse); virtual;
    Procedure Setregion(AIndex : Integer; AValue : TGeoLocationPolygon); virtual;
  Public
  Published
    Property confidence : integer Index 0 Read Fconfidence Write Setconfidence;
    Property point : TGeoLocationEllipse Index 8 Read Fpoint Write Setpoint;
    Property region : TGeoLocationPolygon Index 16 Read Fregion Write Setregion;
  end;
  TGeoLocationClass = Class of TGeoLocation;
  
  { --------------------------------------------------------------------
    TGeoLocationEllipse
    --------------------------------------------------------------------}
  
  TGeoLocationEllipse = Class(TGoogleBaseObject)
  Private
    Fcenter : TGeoLocationPoint;
    Forientation : double;
    FsemiMajorAxis : double;
    FsemiMinorAxis : double;
  Protected
    //Property setters
    Procedure Setcenter(AIndex : Integer; AValue : TGeoLocationPoint); virtual;
    Procedure Setorientation(AIndex : Integer; AValue : double); virtual;
    Procedure SetsemiMajorAxis(AIndex : Integer; AValue : double); virtual;
    Procedure SetsemiMinorAxis(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property center : TGeoLocationPoint Index 0 Read Fcenter Write Setcenter;
    Property orientation : double Index 8 Read Forientation Write Setorientation;
    Property semiMajorAxis : double Index 16 Read FsemiMajorAxis Write SetsemiMajorAxis;
    Property semiMinorAxis : double Index 24 Read FsemiMinorAxis Write SetsemiMinorAxis;
  end;
  TGeoLocationEllipseClass = Class of TGeoLocationEllipse;
  
  { --------------------------------------------------------------------
    TGeoLocationPoint
    --------------------------------------------------------------------}
  
  TGeoLocationPoint = Class(TGoogleBaseObject)
  Private
    Flatitude : double;
    Flongitude : double;
  Protected
    //Property setters
    Procedure Setlatitude(AIndex : Integer; AValue : double); virtual;
    Procedure Setlongitude(AIndex : Integer; AValue : double); virtual;
  Public
  Published
    Property latitude : double Index 0 Read Flatitude Write Setlatitude;
    Property longitude : double Index 8 Read Flongitude Write Setlongitude;
  end;
  TGeoLocationPointClass = Class of TGeoLocationPoint;
  
  { --------------------------------------------------------------------
    TGeoLocationPolygon
    --------------------------------------------------------------------}
  
  TGeoLocationPolygon = Class(TGoogleBaseObject)
  Private
    Fexterior : TGeoLocationPolygonTypeexteriorArray;
  Protected
    //Property setters
    Procedure Setexterior(AIndex : Integer; AValue : TGeoLocationPolygonTypeexteriorArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property exterior : TGeoLocationPolygonTypeexteriorArray Index 0 Read Fexterior Write Setexterior;
  end;
  TGeoLocationPolygonClass = Class of TGeoLocationPolygon;
  
  { --------------------------------------------------------------------
    TGeoSpectrumSchedule
    --------------------------------------------------------------------}
  
  TGeoSpectrumSchedule = Class(TGoogleBaseObject)
  Private
    Flocation : TGeoLocation;
    FspectrumSchedules : TGeoSpectrumScheduleTypespectrumSchedulesArray;
  Protected
    //Property setters
    Procedure Setlocation(AIndex : Integer; AValue : TGeoLocation); virtual;
    Procedure SetspectrumSchedules(AIndex : Integer; AValue : TGeoSpectrumScheduleTypespectrumSchedulesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property location : TGeoLocation Index 0 Read Flocation Write Setlocation;
    Property spectrumSchedules : TGeoSpectrumScheduleTypespectrumSchedulesArray Index 8 Read FspectrumSchedules Write SetspectrumSchedules;
  end;
  TGeoSpectrumScheduleClass = Class of TGeoSpectrumSchedule;
  
  { --------------------------------------------------------------------
    TPawsGetSpectrumBatchRequest
    --------------------------------------------------------------------}
  
  TPawsGetSpectrumBatchRequest = Class(TGoogleBaseObject)
  Private
    Fantenna : TAntennaCharacteristics;
    Fcapabilities : TDeviceCapabilities;
    FdeviceDesc : TDeviceDescriptor;
    Flocations : TPawsGetSpectrumBatchRequestTypelocationsArray;
    FmasterDeviceDesc : TDeviceDescriptor;
    Fowner : TDeviceOwner;
    FrequestType : String;
    F_type : String;
    Fversion : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setantenna(AIndex : Integer; AValue : TAntennaCharacteristics); virtual;
    Procedure Setcapabilities(AIndex : Integer; AValue : TDeviceCapabilities); virtual;
    Procedure SetdeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); virtual;
    Procedure Setlocations(AIndex : Integer; AValue : TPawsGetSpectrumBatchRequestTypelocationsArray); virtual;
    Procedure SetmasterDeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); virtual;
    Procedure Setowner(AIndex : Integer; AValue : TDeviceOwner); virtual;
    Procedure SetrequestType(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property antenna : TAntennaCharacteristics Index 0 Read Fantenna Write Setantenna;
    Property capabilities : TDeviceCapabilities Index 8 Read Fcapabilities Write Setcapabilities;
    Property deviceDesc : TDeviceDescriptor Index 16 Read FdeviceDesc Write SetdeviceDesc;
    Property locations : TPawsGetSpectrumBatchRequestTypelocationsArray Index 24 Read Flocations Write Setlocations;
    Property masterDeviceDesc : TDeviceDescriptor Index 32 Read FmasterDeviceDesc Write SetmasterDeviceDesc;
    Property owner : TDeviceOwner Index 40 Read Fowner Write Setowner;
    Property requestType : String Index 48 Read FrequestType Write SetrequestType;
    Property _type : String Index 56 Read F_type Write Set_type;
    Property version : String Index 64 Read Fversion Write Setversion;
  end;
  TPawsGetSpectrumBatchRequestClass = Class of TPawsGetSpectrumBatchRequest;
  
  { --------------------------------------------------------------------
    TPawsGetSpectrumBatchResponse
    --------------------------------------------------------------------}
  
  TPawsGetSpectrumBatchResponse = Class(TGoogleBaseObject)
  Private
    FdatabaseChange : TDbUpdateSpec;
    FdeviceDesc : TDeviceDescriptor;
    FgeoSpectrumSchedules : TPawsGetSpectrumBatchResponseTypegeoSpectrumSchedulesArray;
    Fkind : String;
    FmaxContiguousBwHz : double;
    FmaxTotalBwHz : double;
    FneedsSpectrumReport : boolean;
    FrulesetInfo : TRulesetInfo;
    Ftimestamp : String;
    F_type : String;
    Fversion : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdatabaseChange(AIndex : Integer; AValue : TDbUpdateSpec); virtual;
    Procedure SetdeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); virtual;
    Procedure SetgeoSpectrumSchedules(AIndex : Integer; AValue : TPawsGetSpectrumBatchResponseTypegeoSpectrumSchedulesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaxContiguousBwHz(AIndex : Integer; AValue : double); virtual;
    Procedure SetmaxTotalBwHz(AIndex : Integer; AValue : double); virtual;
    Procedure SetneedsSpectrumReport(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetrulesetInfo(AIndex : Integer; AValue : TRulesetInfo); virtual;
    Procedure Settimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property databaseChange : TDbUpdateSpec Index 0 Read FdatabaseChange Write SetdatabaseChange;
    Property deviceDesc : TDeviceDescriptor Index 8 Read FdeviceDesc Write SetdeviceDesc;
    Property geoSpectrumSchedules : TPawsGetSpectrumBatchResponseTypegeoSpectrumSchedulesArray Index 16 Read FgeoSpectrumSchedules Write SetgeoSpectrumSchedules;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property maxContiguousBwHz : double Index 32 Read FmaxContiguousBwHz Write SetmaxContiguousBwHz;
    Property maxTotalBwHz : double Index 40 Read FmaxTotalBwHz Write SetmaxTotalBwHz;
    Property needsSpectrumReport : boolean Index 48 Read FneedsSpectrumReport Write SetneedsSpectrumReport;
    Property rulesetInfo : TRulesetInfo Index 56 Read FrulesetInfo Write SetrulesetInfo;
    Property timestamp : String Index 64 Read Ftimestamp Write Settimestamp;
    Property _type : String Index 72 Read F_type Write Set_type;
    Property version : String Index 80 Read Fversion Write Setversion;
  end;
  TPawsGetSpectrumBatchResponseClass = Class of TPawsGetSpectrumBatchResponse;
  
  { --------------------------------------------------------------------
    TPawsGetSpectrumRequest
    --------------------------------------------------------------------}
  
  TPawsGetSpectrumRequest = Class(TGoogleBaseObject)
  Private
    Fantenna : TAntennaCharacteristics;
    Fcapabilities : TDeviceCapabilities;
    FdeviceDesc : TDeviceDescriptor;
    Flocation : TGeoLocation;
    FmasterDeviceDesc : TDeviceDescriptor;
    Fowner : TDeviceOwner;
    FrequestType : String;
    F_type : String;
    Fversion : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setantenna(AIndex : Integer; AValue : TAntennaCharacteristics); virtual;
    Procedure Setcapabilities(AIndex : Integer; AValue : TDeviceCapabilities); virtual;
    Procedure SetdeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : TGeoLocation); virtual;
    Procedure SetmasterDeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); virtual;
    Procedure Setowner(AIndex : Integer; AValue : TDeviceOwner); virtual;
    Procedure SetrequestType(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property antenna : TAntennaCharacteristics Index 0 Read Fantenna Write Setantenna;
    Property capabilities : TDeviceCapabilities Index 8 Read Fcapabilities Write Setcapabilities;
    Property deviceDesc : TDeviceDescriptor Index 16 Read FdeviceDesc Write SetdeviceDesc;
    Property location : TGeoLocation Index 24 Read Flocation Write Setlocation;
    Property masterDeviceDesc : TDeviceDescriptor Index 32 Read FmasterDeviceDesc Write SetmasterDeviceDesc;
    Property owner : TDeviceOwner Index 40 Read Fowner Write Setowner;
    Property requestType : String Index 48 Read FrequestType Write SetrequestType;
    Property _type : String Index 56 Read F_type Write Set_type;
    Property version : String Index 64 Read Fversion Write Setversion;
  end;
  TPawsGetSpectrumRequestClass = Class of TPawsGetSpectrumRequest;
  
  { --------------------------------------------------------------------
    TPawsGetSpectrumResponse
    --------------------------------------------------------------------}
  
  TPawsGetSpectrumResponse = Class(TGoogleBaseObject)
  Private
    FdatabaseChange : TDbUpdateSpec;
    FdeviceDesc : TDeviceDescriptor;
    Fkind : String;
    FmaxContiguousBwHz : double;
    FmaxTotalBwHz : double;
    FneedsSpectrumReport : boolean;
    FrulesetInfo : TRulesetInfo;
    FspectrumSchedules : TPawsGetSpectrumResponseTypespectrumSchedulesArray;
    Ftimestamp : String;
    F_type : String;
    Fversion : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdatabaseChange(AIndex : Integer; AValue : TDbUpdateSpec); virtual;
    Procedure SetdeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaxContiguousBwHz(AIndex : Integer; AValue : double); virtual;
    Procedure SetmaxTotalBwHz(AIndex : Integer; AValue : double); virtual;
    Procedure SetneedsSpectrumReport(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetrulesetInfo(AIndex : Integer; AValue : TRulesetInfo); virtual;
    Procedure SetspectrumSchedules(AIndex : Integer; AValue : TPawsGetSpectrumResponseTypespectrumSchedulesArray); virtual;
    Procedure Settimestamp(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property databaseChange : TDbUpdateSpec Index 0 Read FdatabaseChange Write SetdatabaseChange;
    Property deviceDesc : TDeviceDescriptor Index 8 Read FdeviceDesc Write SetdeviceDesc;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property maxContiguousBwHz : double Index 24 Read FmaxContiguousBwHz Write SetmaxContiguousBwHz;
    Property maxTotalBwHz : double Index 32 Read FmaxTotalBwHz Write SetmaxTotalBwHz;
    Property needsSpectrumReport : boolean Index 40 Read FneedsSpectrumReport Write SetneedsSpectrumReport;
    Property rulesetInfo : TRulesetInfo Index 48 Read FrulesetInfo Write SetrulesetInfo;
    Property spectrumSchedules : TPawsGetSpectrumResponseTypespectrumSchedulesArray Index 56 Read FspectrumSchedules Write SetspectrumSchedules;
    Property timestamp : String Index 64 Read Ftimestamp Write Settimestamp;
    Property _type : String Index 72 Read F_type Write Set_type;
    Property version : String Index 80 Read Fversion Write Setversion;
  end;
  TPawsGetSpectrumResponseClass = Class of TPawsGetSpectrumResponse;
  
  { --------------------------------------------------------------------
    TPawsInitRequest
    --------------------------------------------------------------------}
  
  TPawsInitRequest = Class(TGoogleBaseObject)
  Private
    FdeviceDesc : TDeviceDescriptor;
    Flocation : TGeoLocation;
    F_type : String;
    Fversion : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : TGeoLocation); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property deviceDesc : TDeviceDescriptor Index 0 Read FdeviceDesc Write SetdeviceDesc;
    Property location : TGeoLocation Index 8 Read Flocation Write Setlocation;
    Property _type : String Index 16 Read F_type Write Set_type;
    Property version : String Index 24 Read Fversion Write Setversion;
  end;
  TPawsInitRequestClass = Class of TPawsInitRequest;
  
  { --------------------------------------------------------------------
    TPawsInitResponse
    --------------------------------------------------------------------}
  
  TPawsInitResponse = Class(TGoogleBaseObject)
  Private
    FdatabaseChange : TDbUpdateSpec;
    Fkind : String;
    FrulesetInfo : TRulesetInfo;
    F_type : String;
    Fversion : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdatabaseChange(AIndex : Integer; AValue : TDbUpdateSpec); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetrulesetInfo(AIndex : Integer; AValue : TRulesetInfo); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property databaseChange : TDbUpdateSpec Index 0 Read FdatabaseChange Write SetdatabaseChange;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property rulesetInfo : TRulesetInfo Index 16 Read FrulesetInfo Write SetrulesetInfo;
    Property _type : String Index 24 Read F_type Write Set_type;
    Property version : String Index 32 Read Fversion Write Setversion;
  end;
  TPawsInitResponseClass = Class of TPawsInitResponse;
  
  { --------------------------------------------------------------------
    TPawsNotifySpectrumUseRequest
    --------------------------------------------------------------------}
  
  TPawsNotifySpectrumUseRequest = Class(TGoogleBaseObject)
  Private
    FdeviceDesc : TDeviceDescriptor;
    Flocation : TGeoLocation;
    Fspectra : TPawsNotifySpectrumUseRequestTypespectraArray;
    F_type : String;
    Fversion : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : TGeoLocation); virtual;
    Procedure Setspectra(AIndex : Integer; AValue : TPawsNotifySpectrumUseRequestTypespectraArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property deviceDesc : TDeviceDescriptor Index 0 Read FdeviceDesc Write SetdeviceDesc;
    Property location : TGeoLocation Index 8 Read Flocation Write Setlocation;
    Property spectra : TPawsNotifySpectrumUseRequestTypespectraArray Index 16 Read Fspectra Write Setspectra;
    Property _type : String Index 24 Read F_type Write Set_type;
    Property version : String Index 32 Read Fversion Write Setversion;
  end;
  TPawsNotifySpectrumUseRequestClass = Class of TPawsNotifySpectrumUseRequest;
  
  { --------------------------------------------------------------------
    TPawsNotifySpectrumUseResponse
    --------------------------------------------------------------------}
  
  TPawsNotifySpectrumUseResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    F_type : String;
    Fversion : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property _type : String Index 8 Read F_type Write Set_type;
    Property version : String Index 16 Read Fversion Write Setversion;
  end;
  TPawsNotifySpectrumUseResponseClass = Class of TPawsNotifySpectrumUseResponse;
  
  { --------------------------------------------------------------------
    TPawsRegisterRequest
    --------------------------------------------------------------------}
  
  TPawsRegisterRequest = Class(TGoogleBaseObject)
  Private
    Fantenna : TAntennaCharacteristics;
    FdeviceDesc : TDeviceDescriptor;
    FdeviceOwner : TDeviceOwner;
    Flocation : TGeoLocation;
    F_type : String;
    Fversion : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setantenna(AIndex : Integer; AValue : TAntennaCharacteristics); virtual;
    Procedure SetdeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); virtual;
    Procedure SetdeviceOwner(AIndex : Integer; AValue : TDeviceOwner); virtual;
    Procedure Setlocation(AIndex : Integer; AValue : TGeoLocation); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property antenna : TAntennaCharacteristics Index 0 Read Fantenna Write Setantenna;
    Property deviceDesc : TDeviceDescriptor Index 8 Read FdeviceDesc Write SetdeviceDesc;
    Property deviceOwner : TDeviceOwner Index 16 Read FdeviceOwner Write SetdeviceOwner;
    Property location : TGeoLocation Index 24 Read Flocation Write Setlocation;
    Property _type : String Index 32 Read F_type Write Set_type;
    Property version : String Index 40 Read Fversion Write Setversion;
  end;
  TPawsRegisterRequestClass = Class of TPawsRegisterRequest;
  
  { --------------------------------------------------------------------
    TPawsRegisterResponse
    --------------------------------------------------------------------}
  
  TPawsRegisterResponse = Class(TGoogleBaseObject)
  Private
    FdatabaseChange : TDbUpdateSpec;
    Fkind : String;
    F_type : String;
    Fversion : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdatabaseChange(AIndex : Integer; AValue : TDbUpdateSpec); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property databaseChange : TDbUpdateSpec Index 0 Read FdatabaseChange Write SetdatabaseChange;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property _type : String Index 16 Read F_type Write Set_type;
    Property version : String Index 24 Read Fversion Write Setversion;
  end;
  TPawsRegisterResponseClass = Class of TPawsRegisterResponse;
  
  { --------------------------------------------------------------------
    TPawsVerifyDeviceRequest
    --------------------------------------------------------------------}
  
  TPawsVerifyDeviceRequest = Class(TGoogleBaseObject)
  Private
    FdeviceDescs : TPawsVerifyDeviceRequestTypedeviceDescsArray;
    F_type : String;
    Fversion : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdeviceDescs(AIndex : Integer; AValue : TPawsVerifyDeviceRequestTypedeviceDescsArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property deviceDescs : TPawsVerifyDeviceRequestTypedeviceDescsArray Index 0 Read FdeviceDescs Write SetdeviceDescs;
    Property _type : String Index 8 Read F_type Write Set_type;
    Property version : String Index 16 Read Fversion Write Setversion;
  end;
  TPawsVerifyDeviceRequestClass = Class of TPawsVerifyDeviceRequest;
  
  { --------------------------------------------------------------------
    TPawsVerifyDeviceResponse
    --------------------------------------------------------------------}
  
  TPawsVerifyDeviceResponse = Class(TGoogleBaseObject)
  Private
    FdatabaseChange : TDbUpdateSpec;
    FdeviceValidities : TPawsVerifyDeviceResponseTypedeviceValiditiesArray;
    Fkind : String;
    F_type : String;
    Fversion : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdatabaseChange(AIndex : Integer; AValue : TDbUpdateSpec); virtual;
    Procedure SetdeviceValidities(AIndex : Integer; AValue : TPawsVerifyDeviceResponseTypedeviceValiditiesArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property databaseChange : TDbUpdateSpec Index 0 Read FdatabaseChange Write SetdatabaseChange;
    Property deviceValidities : TPawsVerifyDeviceResponseTypedeviceValiditiesArray Index 8 Read FdeviceValidities Write SetdeviceValidities;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property _type : String Index 24 Read F_type Write Set_type;
    Property version : String Index 32 Read Fversion Write Setversion;
  end;
  TPawsVerifyDeviceResponseClass = Class of TPawsVerifyDeviceResponse;
  
  { --------------------------------------------------------------------
    TRulesetInfo
    --------------------------------------------------------------------}
  
  TRulesetInfo = Class(TGoogleBaseObject)
  Private
    Fauthority : String;
    FmaxLocationChange : double;
    FmaxPollingSecs : integer;
    FrulesetIds : TStringArray;
  Protected
    //Property setters
    Procedure Setauthority(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaxLocationChange(AIndex : Integer; AValue : double); virtual;
    Procedure SetmaxPollingSecs(AIndex : Integer; AValue : integer); virtual;
    Procedure SetrulesetIds(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property authority : String Index 0 Read Fauthority Write Setauthority;
    Property maxLocationChange : double Index 8 Read FmaxLocationChange Write SetmaxLocationChange;
    Property maxPollingSecs : integer Index 16 Read FmaxPollingSecs Write SetmaxPollingSecs;
    Property rulesetIds : TStringArray Index 24 Read FrulesetIds Write SetrulesetIds;
  end;
  TRulesetInfoClass = Class of TRulesetInfo;
  
  { --------------------------------------------------------------------
    TSpectrumMessage
    --------------------------------------------------------------------}
  
  TSpectrumMessage = Class(TGoogleBaseObject)
  Private
    Fbandwidth : double;
    FfrequencyRanges : TSpectrumMessageTypefrequencyRangesArray;
  Protected
    //Property setters
    Procedure Setbandwidth(AIndex : Integer; AValue : double); virtual;
    Procedure SetfrequencyRanges(AIndex : Integer; AValue : TSpectrumMessageTypefrequencyRangesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property bandwidth : double Index 0 Read Fbandwidth Write Setbandwidth;
    Property frequencyRanges : TSpectrumMessageTypefrequencyRangesArray Index 8 Read FfrequencyRanges Write SetfrequencyRanges;
  end;
  TSpectrumMessageClass = Class of TSpectrumMessage;
  
  { --------------------------------------------------------------------
    TSpectrumSchedule
    --------------------------------------------------------------------}
  
  TSpectrumSchedule = Class(TGoogleBaseObject)
  Private
    FeventTime : TEventTime;
    Fspectra : TSpectrumScheduleTypespectraArray;
  Protected
    //Property setters
    Procedure SeteventTime(AIndex : Integer; AValue : TEventTime); virtual;
    Procedure Setspectra(AIndex : Integer; AValue : TSpectrumScheduleTypespectraArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property eventTime : TEventTime Index 0 Read FeventTime Write SeteventTime;
    Property spectra : TSpectrumScheduleTypespectraArray Index 8 Read Fspectra Write Setspectra;
  end;
  TSpectrumScheduleClass = Class of TSpectrumSchedule;
  
  { --------------------------------------------------------------------
    TVcard
    --------------------------------------------------------------------}
  
  TVcard = Class(TGoogleBaseObject)
  Private
    Fadr : TVcardAddress;
    Femail : TVcardTypedText;
    Ffn : String;
    Forg : TVcardTypedText;
    Ftel : TVcardTelephone;
  Protected
    //Property setters
    Procedure Setadr(AIndex : Integer; AValue : TVcardAddress); virtual;
    Procedure Setemail(AIndex : Integer; AValue : TVcardTypedText); virtual;
    Procedure Setfn(AIndex : Integer; AValue : String); virtual;
    Procedure Setorg(AIndex : Integer; AValue : TVcardTypedText); virtual;
    Procedure Settel(AIndex : Integer; AValue : TVcardTelephone); virtual;
  Public
  Published
    Property adr : TVcardAddress Index 0 Read Fadr Write Setadr;
    Property email : TVcardTypedText Index 8 Read Femail Write Setemail;
    Property fn : String Index 16 Read Ffn Write Setfn;
    Property org : TVcardTypedText Index 24 Read Forg Write Setorg;
    Property tel : TVcardTelephone Index 32 Read Ftel Write Settel;
  end;
  TVcardClass = Class of TVcard;
  
  { --------------------------------------------------------------------
    TVcardAddress
    --------------------------------------------------------------------}
  
  TVcardAddress = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    Fcountry : String;
    Flocality : String;
    Fpobox : String;
    Fregion : String;
    Fstreet : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : String); virtual;
    Procedure Setcountry(AIndex : Integer; AValue : String); virtual;
    Procedure Setlocality(AIndex : Integer; AValue : String); virtual;
    Procedure Setpobox(AIndex : Integer; AValue : String); virtual;
    Procedure Setregion(AIndex : Integer; AValue : String); virtual;
    Procedure Setstreet(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property country : String Index 8 Read Fcountry Write Setcountry;
    Property locality : String Index 16 Read Flocality Write Setlocality;
    Property pobox : String Index 24 Read Fpobox Write Setpobox;
    Property region : String Index 32 Read Fregion Write Setregion;
    Property street : String Index 40 Read Fstreet Write Setstreet;
  end;
  TVcardAddressClass = Class of TVcardAddress;
  
  { --------------------------------------------------------------------
    TVcardTelephone
    --------------------------------------------------------------------}
  
  TVcardTelephone = Class(TGoogleBaseObject)
  Private
    Furi : String;
  Protected
    //Property setters
    Procedure Seturi(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property uri : String Index 0 Read Furi Write Seturi;
  end;
  TVcardTelephoneClass = Class of TVcardTelephone;
  
  { --------------------------------------------------------------------
    TVcardTypedText
    --------------------------------------------------------------------}
  
  TVcardTypedText = Class(TGoogleBaseObject)
  Private
    Ftext : String;
  Protected
    //Property setters
    Procedure Settext(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property text : String Index 0 Read Ftext Write Settext;
  end;
  TVcardTypedTextClass = Class of TVcardTypedText;
  
  { --------------------------------------------------------------------
    TPawsResource
    --------------------------------------------------------------------}
  
  TPawsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function GetSpectrum(aPawsGetSpectrumRequest : TPawsGetSpectrumRequest) : TPawsGetSpectrumResponse;
    Function GetSpectrumBatch(aPawsGetSpectrumBatchRequest : TPawsGetSpectrumBatchRequest) : TPawsGetSpectrumBatchResponse;
    Function Init(aPawsInitRequest : TPawsInitRequest) : TPawsInitResponse;
    Function NotifySpectrumUse(aPawsNotifySpectrumUseRequest : TPawsNotifySpectrumUseRequest) : TPawsNotifySpectrumUseResponse;
    Function Register(aPawsRegisterRequest : TPawsRegisterRequest) : TPawsRegisterResponse;
    Function VerifyDevice(aPawsVerifyDeviceRequest : TPawsVerifyDeviceRequest) : TPawsVerifyDeviceResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TSpectrumAPI
    --------------------------------------------------------------------}
  
  TSpectrumAPI = Class(TGoogleAPI)
  Private
    FPawsInstance : TPawsResource;
    Function GetPawsInstance : TPawsResource;virtual;
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
    Function CreatePawsResource(AOwner : TComponent) : TPawsResource;virtual;overload;
    Function CreatePawsResource : TPawsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property PawsResource : TPawsResource Read GetPawsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAntennaCharacteristics
  --------------------------------------------------------------------}


Procedure TAntennaCharacteristics.Setheight(AIndex : Integer; AValue : double); 

begin
  If (Fheight=AValue) then exit;
  Fheight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAntennaCharacteristics.SetheightType(AIndex : Integer; AValue : String); 

begin
  If (FheightType=AValue) then exit;
  FheightType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAntennaCharacteristics.SetheightUncertainty(AIndex : Integer; AValue : double); 

begin
  If (FheightUncertainty=AValue) then exit;
  FheightUncertainty:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatabaseSpec
  --------------------------------------------------------------------}


Procedure TDatabaseSpec.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDatabaseSpec.Seturi(AIndex : Integer; AValue : String); 

begin
  If (Furi=AValue) then exit;
  Furi:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDbUpdateSpec
  --------------------------------------------------------------------}


Procedure TDbUpdateSpec.Setdatabases(AIndex : Integer; AValue : TDbUpdateSpecTypedatabasesArray); 

begin
  If (Fdatabases=AValue) then exit;
  Fdatabases:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDbUpdateSpec.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'databases' : SetLength(Fdatabases,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDeviceCapabilities
  --------------------------------------------------------------------}


Procedure TDeviceCapabilities.SetfrequencyRanges(AIndex : Integer; AValue : TDeviceCapabilitiesTypefrequencyRangesArray); 

begin
  If (FfrequencyRanges=AValue) then exit;
  FfrequencyRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDeviceCapabilities.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'frequencyranges' : SetLength(FfrequencyRanges,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDeviceDescriptor
  --------------------------------------------------------------------}


Procedure TDeviceDescriptor.SetetsiEnDeviceCategory(AIndex : Integer; AValue : String); 

begin
  If (FetsiEnDeviceCategory=AValue) then exit;
  FetsiEnDeviceCategory:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeviceDescriptor.SetetsiEnDeviceEmissionsClass(AIndex : Integer; AValue : String); 

begin
  If (FetsiEnDeviceEmissionsClass=AValue) then exit;
  FetsiEnDeviceEmissionsClass:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeviceDescriptor.SetetsiEnDeviceType(AIndex : Integer; AValue : String); 

begin
  If (FetsiEnDeviceType=AValue) then exit;
  FetsiEnDeviceType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeviceDescriptor.SetetsiEnTechnologyId(AIndex : Integer; AValue : String); 

begin
  If (FetsiEnTechnologyId=AValue) then exit;
  FetsiEnTechnologyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeviceDescriptor.SetfccId(AIndex : Integer; AValue : String); 

begin
  If (FfccId=AValue) then exit;
  FfccId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeviceDescriptor.SetfccTvbdDeviceType(AIndex : Integer; AValue : String); 

begin
  If (FfccTvbdDeviceType=AValue) then exit;
  FfccTvbdDeviceType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeviceDescriptor.SetmanufacturerId(AIndex : Integer; AValue : String); 

begin
  If (FmanufacturerId=AValue) then exit;
  FmanufacturerId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeviceDescriptor.SetmodelId(AIndex : Integer; AValue : String); 

begin
  If (FmodelId=AValue) then exit;
  FmodelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeviceDescriptor.SetrulesetIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FrulesetIds=AValue) then exit;
  FrulesetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeviceDescriptor.SetserialNumber(AIndex : Integer; AValue : String); 

begin
  If (FserialNumber=AValue) then exit;
  FserialNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TDeviceDescriptor.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'rulesetids' : SetLength(FrulesetIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDeviceOwner
  --------------------------------------------------------------------}


Procedure TDeviceOwner.Set_operator(AIndex : Integer; AValue : TVcard); 

begin
  If (F_operator=AValue) then exit;
  F_operator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeviceOwner.Setowner(AIndex : Integer; AValue : TVcard); 

begin
  If (Fowner=AValue) then exit;
  Fowner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TDeviceOwner.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_operator' : Result:='operator';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TDeviceValidity
  --------------------------------------------------------------------}


Procedure TDeviceValidity.SetdeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); 

begin
  If (FdeviceDesc=AValue) then exit;
  FdeviceDesc:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeviceValidity.SetisValid(AIndex : Integer; AValue : boolean); 

begin
  If (FisValid=AValue) then exit;
  FisValid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDeviceValidity.Setreason(AIndex : Integer; AValue : String); 

begin
  If (Freason=AValue) then exit;
  Freason:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEventTime
  --------------------------------------------------------------------}


Procedure TEventTime.SetstartTime(AIndex : Integer; AValue : String); 

begin
  If (FstartTime=AValue) then exit;
  FstartTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEventTime.SetstopTime(AIndex : Integer; AValue : String); 

begin
  If (FstopTime=AValue) then exit;
  FstopTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFrequencyRange
  --------------------------------------------------------------------}


Procedure TFrequencyRange.SetchannelId(AIndex : Integer; AValue : String); 

begin
  If (FchannelId=AValue) then exit;
  FchannelId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFrequencyRange.SetmaxPowerDBm(AIndex : Integer; AValue : double); 

begin
  If (FmaxPowerDBm=AValue) then exit;
  FmaxPowerDBm:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFrequencyRange.SetstartHz(AIndex : Integer; AValue : double); 

begin
  If (FstartHz=AValue) then exit;
  FstartHz:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFrequencyRange.SetstopHz(AIndex : Integer; AValue : double); 

begin
  If (FstopHz=AValue) then exit;
  FstopHz:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeoLocation
  --------------------------------------------------------------------}


Procedure TGeoLocation.Setconfidence(AIndex : Integer; AValue : integer); 

begin
  If (Fconfidence=AValue) then exit;
  Fconfidence:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoLocation.Setpoint(AIndex : Integer; AValue : TGeoLocationEllipse); 

begin
  If (Fpoint=AValue) then exit;
  Fpoint:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoLocation.Setregion(AIndex : Integer; AValue : TGeoLocationPolygon); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeoLocationEllipse
  --------------------------------------------------------------------}


Procedure TGeoLocationEllipse.Setcenter(AIndex : Integer; AValue : TGeoLocationPoint); 

begin
  If (Fcenter=AValue) then exit;
  Fcenter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoLocationEllipse.Setorientation(AIndex : Integer; AValue : double); 

begin
  If (Forientation=AValue) then exit;
  Forientation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoLocationEllipse.SetsemiMajorAxis(AIndex : Integer; AValue : double); 

begin
  If (FsemiMajorAxis=AValue) then exit;
  FsemiMajorAxis:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoLocationEllipse.SetsemiMinorAxis(AIndex : Integer; AValue : double); 

begin
  If (FsemiMinorAxis=AValue) then exit;
  FsemiMinorAxis:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeoLocationPoint
  --------------------------------------------------------------------}


Procedure TGeoLocationPoint.Setlatitude(AIndex : Integer; AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoLocationPoint.Setlongitude(AIndex : Integer; AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGeoLocationPolygon
  --------------------------------------------------------------------}


Procedure TGeoLocationPolygon.Setexterior(AIndex : Integer; AValue : TGeoLocationPolygonTypeexteriorArray); 

begin
  If (Fexterior=AValue) then exit;
  Fexterior:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGeoLocationPolygon.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'exterior' : SetLength(Fexterior,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGeoSpectrumSchedule
  --------------------------------------------------------------------}


Procedure TGeoSpectrumSchedule.Setlocation(AIndex : Integer; AValue : TGeoLocation); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGeoSpectrumSchedule.SetspectrumSchedules(AIndex : Integer; AValue : TGeoSpectrumScheduleTypespectrumSchedulesArray); 

begin
  If (FspectrumSchedules=AValue) then exit;
  FspectrumSchedules:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGeoSpectrumSchedule.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'spectrumschedules' : SetLength(FspectrumSchedules,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPawsGetSpectrumBatchRequest
  --------------------------------------------------------------------}


Procedure TPawsGetSpectrumBatchRequest.Setantenna(AIndex : Integer; AValue : TAntennaCharacteristics); 

begin
  If (Fantenna=AValue) then exit;
  Fantenna:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchRequest.Setcapabilities(AIndex : Integer; AValue : TDeviceCapabilities); 

begin
  If (Fcapabilities=AValue) then exit;
  Fcapabilities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchRequest.SetdeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); 

begin
  If (FdeviceDesc=AValue) then exit;
  FdeviceDesc:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchRequest.Setlocations(AIndex : Integer; AValue : TPawsGetSpectrumBatchRequestTypelocationsArray); 

begin
  If (Flocations=AValue) then exit;
  Flocations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchRequest.SetmasterDeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); 

begin
  If (FmasterDeviceDesc=AValue) then exit;
  FmasterDeviceDesc:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchRequest.Setowner(AIndex : Integer; AValue : TDeviceOwner); 

begin
  If (Fowner=AValue) then exit;
  Fowner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchRequest.SetrequestType(AIndex : Integer; AValue : String); 

begin
  If (FrequestType=AValue) then exit;
  FrequestType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchRequest.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchRequest.Setversion(AIndex : Integer; AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPawsGetSpectrumBatchRequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPawsGetSpectrumBatchRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'locations' : SetLength(Flocations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPawsGetSpectrumBatchResponse
  --------------------------------------------------------------------}


Procedure TPawsGetSpectrumBatchResponse.SetdatabaseChange(AIndex : Integer; AValue : TDbUpdateSpec); 

begin
  If (FdatabaseChange=AValue) then exit;
  FdatabaseChange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchResponse.SetdeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); 

begin
  If (FdeviceDesc=AValue) then exit;
  FdeviceDesc:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchResponse.SetgeoSpectrumSchedules(AIndex : Integer; AValue : TPawsGetSpectrumBatchResponseTypegeoSpectrumSchedulesArray); 

begin
  If (FgeoSpectrumSchedules=AValue) then exit;
  FgeoSpectrumSchedules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchResponse.SetmaxContiguousBwHz(AIndex : Integer; AValue : double); 

begin
  If (FmaxContiguousBwHz=AValue) then exit;
  FmaxContiguousBwHz:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchResponse.SetmaxTotalBwHz(AIndex : Integer; AValue : double); 

begin
  If (FmaxTotalBwHz=AValue) then exit;
  FmaxTotalBwHz:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchResponse.SetneedsSpectrumReport(AIndex : Integer; AValue : boolean); 

begin
  If (FneedsSpectrumReport=AValue) then exit;
  FneedsSpectrumReport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchResponse.SetrulesetInfo(AIndex : Integer; AValue : TRulesetInfo); 

begin
  If (FrulesetInfo=AValue) then exit;
  FrulesetInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchResponse.Settimestamp(AIndex : Integer; AValue : String); 

begin
  If (Ftimestamp=AValue) then exit;
  Ftimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchResponse.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumBatchResponse.Setversion(AIndex : Integer; AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPawsGetSpectrumBatchResponse.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPawsGetSpectrumBatchResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'geospectrumschedules' : SetLength(FgeoSpectrumSchedules,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPawsGetSpectrumRequest
  --------------------------------------------------------------------}


Procedure TPawsGetSpectrumRequest.Setantenna(AIndex : Integer; AValue : TAntennaCharacteristics); 

begin
  If (Fantenna=AValue) then exit;
  Fantenna:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumRequest.Setcapabilities(AIndex : Integer; AValue : TDeviceCapabilities); 

begin
  If (Fcapabilities=AValue) then exit;
  Fcapabilities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumRequest.SetdeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); 

begin
  If (FdeviceDesc=AValue) then exit;
  FdeviceDesc:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumRequest.Setlocation(AIndex : Integer; AValue : TGeoLocation); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumRequest.SetmasterDeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); 

begin
  If (FmasterDeviceDesc=AValue) then exit;
  FmasterDeviceDesc:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumRequest.Setowner(AIndex : Integer; AValue : TDeviceOwner); 

begin
  If (Fowner=AValue) then exit;
  Fowner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumRequest.SetrequestType(AIndex : Integer; AValue : String); 

begin
  If (FrequestType=AValue) then exit;
  FrequestType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumRequest.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumRequest.Setversion(AIndex : Integer; AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPawsGetSpectrumRequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPawsGetSpectrumResponse
  --------------------------------------------------------------------}


Procedure TPawsGetSpectrumResponse.SetdatabaseChange(AIndex : Integer; AValue : TDbUpdateSpec); 

begin
  If (FdatabaseChange=AValue) then exit;
  FdatabaseChange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumResponse.SetdeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); 

begin
  If (FdeviceDesc=AValue) then exit;
  FdeviceDesc:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumResponse.SetmaxContiguousBwHz(AIndex : Integer; AValue : double); 

begin
  If (FmaxContiguousBwHz=AValue) then exit;
  FmaxContiguousBwHz:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumResponse.SetmaxTotalBwHz(AIndex : Integer; AValue : double); 

begin
  If (FmaxTotalBwHz=AValue) then exit;
  FmaxTotalBwHz:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumResponse.SetneedsSpectrumReport(AIndex : Integer; AValue : boolean); 

begin
  If (FneedsSpectrumReport=AValue) then exit;
  FneedsSpectrumReport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumResponse.SetrulesetInfo(AIndex : Integer; AValue : TRulesetInfo); 

begin
  If (FrulesetInfo=AValue) then exit;
  FrulesetInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumResponse.SetspectrumSchedules(AIndex : Integer; AValue : TPawsGetSpectrumResponseTypespectrumSchedulesArray); 

begin
  If (FspectrumSchedules=AValue) then exit;
  FspectrumSchedules:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumResponse.Settimestamp(AIndex : Integer; AValue : String); 

begin
  If (Ftimestamp=AValue) then exit;
  Ftimestamp:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumResponse.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsGetSpectrumResponse.Setversion(AIndex : Integer; AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPawsGetSpectrumResponse.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPawsGetSpectrumResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'spectrumschedules' : SetLength(FspectrumSchedules,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPawsInitRequest
  --------------------------------------------------------------------}


Procedure TPawsInitRequest.SetdeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); 

begin
  If (FdeviceDesc=AValue) then exit;
  FdeviceDesc:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsInitRequest.Setlocation(AIndex : Integer; AValue : TGeoLocation); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsInitRequest.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsInitRequest.Setversion(AIndex : Integer; AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPawsInitRequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPawsInitResponse
  --------------------------------------------------------------------}


Procedure TPawsInitResponse.SetdatabaseChange(AIndex : Integer; AValue : TDbUpdateSpec); 

begin
  If (FdatabaseChange=AValue) then exit;
  FdatabaseChange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsInitResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsInitResponse.SetrulesetInfo(AIndex : Integer; AValue : TRulesetInfo); 

begin
  If (FrulesetInfo=AValue) then exit;
  FrulesetInfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsInitResponse.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsInitResponse.Setversion(AIndex : Integer; AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPawsInitResponse.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPawsNotifySpectrumUseRequest
  --------------------------------------------------------------------}


Procedure TPawsNotifySpectrumUseRequest.SetdeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); 

begin
  If (FdeviceDesc=AValue) then exit;
  FdeviceDesc:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsNotifySpectrumUseRequest.Setlocation(AIndex : Integer; AValue : TGeoLocation); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsNotifySpectrumUseRequest.Setspectra(AIndex : Integer; AValue : TPawsNotifySpectrumUseRequestTypespectraArray); 

begin
  If (Fspectra=AValue) then exit;
  Fspectra:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsNotifySpectrumUseRequest.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsNotifySpectrumUseRequest.Setversion(AIndex : Integer; AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPawsNotifySpectrumUseRequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPawsNotifySpectrumUseRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'spectra' : SetLength(Fspectra,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPawsNotifySpectrumUseResponse
  --------------------------------------------------------------------}


Procedure TPawsNotifySpectrumUseResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsNotifySpectrumUseResponse.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsNotifySpectrumUseResponse.Setversion(AIndex : Integer; AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPawsNotifySpectrumUseResponse.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPawsRegisterRequest
  --------------------------------------------------------------------}


Procedure TPawsRegisterRequest.Setantenna(AIndex : Integer; AValue : TAntennaCharacteristics); 

begin
  If (Fantenna=AValue) then exit;
  Fantenna:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsRegisterRequest.SetdeviceDesc(AIndex : Integer; AValue : TDeviceDescriptor); 

begin
  If (FdeviceDesc=AValue) then exit;
  FdeviceDesc:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsRegisterRequest.SetdeviceOwner(AIndex : Integer; AValue : TDeviceOwner); 

begin
  If (FdeviceOwner=AValue) then exit;
  FdeviceOwner:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsRegisterRequest.Setlocation(AIndex : Integer; AValue : TGeoLocation); 

begin
  If (Flocation=AValue) then exit;
  Flocation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsRegisterRequest.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsRegisterRequest.Setversion(AIndex : Integer; AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPawsRegisterRequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPawsRegisterResponse
  --------------------------------------------------------------------}


Procedure TPawsRegisterResponse.SetdatabaseChange(AIndex : Integer; AValue : TDbUpdateSpec); 

begin
  If (FdatabaseChange=AValue) then exit;
  FdatabaseChange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsRegisterResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsRegisterResponse.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsRegisterResponse.Setversion(AIndex : Integer; AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPawsRegisterResponse.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPawsVerifyDeviceRequest
  --------------------------------------------------------------------}


Procedure TPawsVerifyDeviceRequest.SetdeviceDescs(AIndex : Integer; AValue : TPawsVerifyDeviceRequestTypedeviceDescsArray); 

begin
  If (FdeviceDescs=AValue) then exit;
  FdeviceDescs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsVerifyDeviceRequest.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsVerifyDeviceRequest.Setversion(AIndex : Integer; AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPawsVerifyDeviceRequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPawsVerifyDeviceRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'devicedescs' : SetLength(FdeviceDescs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPawsVerifyDeviceResponse
  --------------------------------------------------------------------}


Procedure TPawsVerifyDeviceResponse.SetdatabaseChange(AIndex : Integer; AValue : TDbUpdateSpec); 

begin
  If (FdatabaseChange=AValue) then exit;
  FdatabaseChange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsVerifyDeviceResponse.SetdeviceValidities(AIndex : Integer; AValue : TPawsVerifyDeviceResponseTypedeviceValiditiesArray); 

begin
  If (FdeviceValidities=AValue) then exit;
  FdeviceValidities:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsVerifyDeviceResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsVerifyDeviceResponse.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPawsVerifyDeviceResponse.Setversion(AIndex : Integer; AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPawsVerifyDeviceResponse.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPawsVerifyDeviceResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'devicevalidities' : SetLength(FdeviceValidities,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TRulesetInfo
  --------------------------------------------------------------------}


Procedure TRulesetInfo.Setauthority(AIndex : Integer; AValue : String); 

begin
  If (Fauthority=AValue) then exit;
  Fauthority:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRulesetInfo.SetmaxLocationChange(AIndex : Integer; AValue : double); 

begin
  If (FmaxLocationChange=AValue) then exit;
  FmaxLocationChange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRulesetInfo.SetmaxPollingSecs(AIndex : Integer; AValue : integer); 

begin
  If (FmaxPollingSecs=AValue) then exit;
  FmaxPollingSecs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRulesetInfo.SetrulesetIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FrulesetIds=AValue) then exit;
  FrulesetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TRulesetInfo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'rulesetids' : SetLength(FrulesetIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSpectrumMessage
  --------------------------------------------------------------------}


Procedure TSpectrumMessage.Setbandwidth(AIndex : Integer; AValue : double); 

begin
  If (Fbandwidth=AValue) then exit;
  Fbandwidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSpectrumMessage.SetfrequencyRanges(AIndex : Integer; AValue : TSpectrumMessageTypefrequencyRangesArray); 

begin
  If (FfrequencyRanges=AValue) then exit;
  FfrequencyRanges:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSpectrumMessage.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'frequencyranges' : SetLength(FfrequencyRanges,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSpectrumSchedule
  --------------------------------------------------------------------}


Procedure TSpectrumSchedule.SeteventTime(AIndex : Integer; AValue : TEventTime); 

begin
  If (FeventTime=AValue) then exit;
  FeventTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSpectrumSchedule.Setspectra(AIndex : Integer; AValue : TSpectrumScheduleTypespectraArray); 

begin
  If (Fspectra=AValue) then exit;
  Fspectra:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSpectrumSchedule.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'spectra' : SetLength(Fspectra,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVcard
  --------------------------------------------------------------------}


Procedure TVcard.Setadr(AIndex : Integer; AValue : TVcardAddress); 

begin
  If (Fadr=AValue) then exit;
  Fadr:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVcard.Setemail(AIndex : Integer; AValue : TVcardTypedText); 

begin
  If (Femail=AValue) then exit;
  Femail:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVcard.Setfn(AIndex : Integer; AValue : String); 

begin
  If (Ffn=AValue) then exit;
  Ffn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVcard.Setorg(AIndex : Integer; AValue : TVcardTypedText); 

begin
  If (Forg=AValue) then exit;
  Forg:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVcard.Settel(AIndex : Integer; AValue : TVcardTelephone); 

begin
  If (Ftel=AValue) then exit;
  Ftel:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVcardAddress
  --------------------------------------------------------------------}


Procedure TVcardAddress.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVcardAddress.Setcountry(AIndex : Integer; AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVcardAddress.Setlocality(AIndex : Integer; AValue : String); 

begin
  If (Flocality=AValue) then exit;
  Flocality:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVcardAddress.Setpobox(AIndex : Integer; AValue : String); 

begin
  If (Fpobox=AValue) then exit;
  Fpobox:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVcardAddress.Setregion(AIndex : Integer; AValue : String); 

begin
  If (Fregion=AValue) then exit;
  Fregion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVcardAddress.Setstreet(AIndex : Integer; AValue : String); 

begin
  If (Fstreet=AValue) then exit;
  Fstreet:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVcardTelephone
  --------------------------------------------------------------------}


Procedure TVcardTelephone.Seturi(AIndex : Integer; AValue : String); 

begin
  If (Furi=AValue) then exit;
  Furi:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVcardTypedText
  --------------------------------------------------------------------}


Procedure TVcardTypedText.Settext(AIndex : Integer; AValue : String); 

begin
  If (Ftext=AValue) then exit;
  Ftext:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPawsResource
  --------------------------------------------------------------------}


Class Function TPawsResource.ResourceName : String;

begin
  Result:='paws';
end;

Class Function TPawsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TspectrumAPI;
end;

Function TPawsResource.GetSpectrum(aPawsGetSpectrumRequest : TPawsGetSpectrumRequest) : TPawsGetSpectrumResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'getSpectrum';
  _Methodid   = 'spectrum.paws.getSpectrum';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aPawsGetSpectrumRequest,TPawsGetSpectrumResponse) as TPawsGetSpectrumResponse;
end;

Function TPawsResource.GetSpectrumBatch(aPawsGetSpectrumBatchRequest : TPawsGetSpectrumBatchRequest) : TPawsGetSpectrumBatchResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'getSpectrumBatch';
  _Methodid   = 'spectrum.paws.getSpectrumBatch';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aPawsGetSpectrumBatchRequest,TPawsGetSpectrumBatchResponse) as TPawsGetSpectrumBatchResponse;
end;

Function TPawsResource.Init(aPawsInitRequest : TPawsInitRequest) : TPawsInitResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'init';
  _Methodid   = 'spectrum.paws.init';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aPawsInitRequest,TPawsInitResponse) as TPawsInitResponse;
end;

Function TPawsResource.NotifySpectrumUse(aPawsNotifySpectrumUseRequest : TPawsNotifySpectrumUseRequest) : TPawsNotifySpectrumUseResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'notifySpectrumUse';
  _Methodid   = 'spectrum.paws.notifySpectrumUse';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aPawsNotifySpectrumUseRequest,TPawsNotifySpectrumUseResponse) as TPawsNotifySpectrumUseResponse;
end;

Function TPawsResource.Register(aPawsRegisterRequest : TPawsRegisterRequest) : TPawsRegisterResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'register';
  _Methodid   = 'spectrum.paws.register';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aPawsRegisterRequest,TPawsRegisterResponse) as TPawsRegisterResponse;
end;

Function TPawsResource.VerifyDevice(aPawsVerifyDeviceRequest : TPawsVerifyDeviceRequest) : TPawsVerifyDeviceResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'verifyDevice';
  _Methodid   = 'spectrum.paws.verifyDevice';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aPawsVerifyDeviceRequest,TPawsVerifyDeviceResponse) as TPawsVerifyDeviceResponse;
end;



{ --------------------------------------------------------------------
  TSpectrumAPI
  --------------------------------------------------------------------}

Class Function TSpectrumAPI.APIName : String;

begin
  Result:='spectrum';
end;

Class Function TSpectrumAPI.APIVersion : String;

begin
  Result:='v1explorer';
end;

Class Function TSpectrumAPI.APIRevision : String;

begin
  Result:='20150112';
end;

Class Function TSpectrumAPI.APIID : String;

begin
  Result:='spectrum:v1explorer';
end;

Class Function TSpectrumAPI.APITitle : String;

begin
  Result:='Google Spectrum Database API';
end;

Class Function TSpectrumAPI.APIDescription : String;

begin
  Result:='API for spectrum-management functions.';
end;

Class Function TSpectrumAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TSpectrumAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TSpectrumAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TSpectrumAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TSpectrumAPI.APIdocumentationLink : String;

begin
  Result:='http://developers.google.com/spectrum';
end;

Class Function TSpectrumAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TSpectrumAPI.APIbasePath : string;

begin
  Result:='/spectrum/v1explorer/paws/';
end;

Class Function TSpectrumAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/spectrum/v1explorer/paws/';
end;

Class Function TSpectrumAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TSpectrumAPI.APIservicePath : string;

begin
  Result:='spectrum/v1explorer/paws/';
end;

Class Function TSpectrumAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TSpectrumAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TSpectrumAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TSpectrumAPI.RegisterAPIResources;

begin
  TAntennaCharacteristics.RegisterObject;
  TDatabaseSpec.RegisterObject;
  TDbUpdateSpec.RegisterObject;
  TDeviceCapabilities.RegisterObject;
  TDeviceDescriptor.RegisterObject;
  TDeviceOwner.RegisterObject;
  TDeviceValidity.RegisterObject;
  TEventTime.RegisterObject;
  TFrequencyRange.RegisterObject;
  TGeoLocation.RegisterObject;
  TGeoLocationEllipse.RegisterObject;
  TGeoLocationPoint.RegisterObject;
  TGeoLocationPolygon.RegisterObject;
  TGeoSpectrumSchedule.RegisterObject;
  TPawsGetSpectrumBatchRequest.RegisterObject;
  TPawsGetSpectrumBatchResponse.RegisterObject;
  TPawsGetSpectrumRequest.RegisterObject;
  TPawsGetSpectrumResponse.RegisterObject;
  TPawsInitRequest.RegisterObject;
  TPawsInitResponse.RegisterObject;
  TPawsNotifySpectrumUseRequest.RegisterObject;
  TPawsNotifySpectrumUseResponse.RegisterObject;
  TPawsRegisterRequest.RegisterObject;
  TPawsRegisterResponse.RegisterObject;
  TPawsVerifyDeviceRequest.RegisterObject;
  TPawsVerifyDeviceResponse.RegisterObject;
  TRulesetInfo.RegisterObject;
  TSpectrumMessage.RegisterObject;
  TSpectrumSchedule.RegisterObject;
  TVcard.RegisterObject;
  TVcardAddress.RegisterObject;
  TVcardTelephone.RegisterObject;
  TVcardTypedText.RegisterObject;
end;


Function TSpectrumAPI.GetPawsInstance : TPawsResource;

begin
  if (FPawsInstance=Nil) then
    FPawsInstance:=CreatePawsResource;
  Result:=FPawsInstance;
end;

Function TSpectrumAPI.CreatePawsResource : TPawsResource;

begin
  Result:=CreatePawsResource(Self);
end;


Function TSpectrumAPI.CreatePawsResource(AOwner : TComponent) : TPawsResource;

begin
  Result:=TPawsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TSpectrumAPI.RegisterAPI;
end.
