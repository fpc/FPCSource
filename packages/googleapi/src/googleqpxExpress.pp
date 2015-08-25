unit googleqpxExpress;
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
//Generated on: 16-5-15 08:53:07
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAircraftData = Class;
  TAirportData = Class;
  TBagDescriptor = Class;
  TCarrierData = Class;
  TCityData = Class;
  TData = Class;
  TFareInfo = Class;
  TFlightInfo = Class;
  TFreeBaggageAllowance = Class;
  TLegInfo = Class;
  TPassengerCounts = Class;
  TPricingInfo = Class;
  TSegmentInfo = Class;
  TSegmentPricing = Class;
  TSliceInfo = Class;
  TSliceInput = Class;
  TTaxData = Class;
  TTaxInfo = Class;
  TTimeOfDayRange = Class;
  TTripOption = Class;
  TTripOptionsRequest = Class;
  TTripOptionsResponse = Class;
  TTripsSearchRequest = Class;
  TTripsSearchResponse = Class;
  TAircraftDataArray = Array of TAircraftData;
  TAirportDataArray = Array of TAirportData;
  TBagDescriptorArray = Array of TBagDescriptor;
  TCarrierDataArray = Array of TCarrierData;
  TCityDataArray = Array of TCityData;
  TDataArray = Array of TData;
  TFareInfoArray = Array of TFareInfo;
  TFlightInfoArray = Array of TFlightInfo;
  TFreeBaggageAllowanceArray = Array of TFreeBaggageAllowance;
  TLegInfoArray = Array of TLegInfo;
  TPassengerCountsArray = Array of TPassengerCounts;
  TPricingInfoArray = Array of TPricingInfo;
  TSegmentInfoArray = Array of TSegmentInfo;
  TSegmentPricingArray = Array of TSegmentPricing;
  TSliceInfoArray = Array of TSliceInfo;
  TSliceInputArray = Array of TSliceInput;
  TTaxDataArray = Array of TTaxData;
  TTaxInfoArray = Array of TTaxInfo;
  TTimeOfDayRangeArray = Array of TTimeOfDayRange;
  TTripOptionArray = Array of TTripOption;
  TTripOptionsRequestArray = Array of TTripOptionsRequest;
  TTripOptionsResponseArray = Array of TTripOptionsResponse;
  TTripsSearchRequestArray = Array of TTripsSearchRequest;
  TTripsSearchResponseArray = Array of TTripsSearchResponse;
  //Anonymous types, using auto-generated names
  TDataTypeaircraftArray = Array of TAircraftData;
  TDataTypeairportArray = Array of TAirportData;
  TDataTypecarrierArray = Array of TCarrierData;
  TDataTypecityArray = Array of TCityData;
  TDataTypetaxArray = Array of TTaxData;
  TFreeBaggageAllowanceTypebagDescriptorArray = Array of TBagDescriptor;
  TPricingInfoTypefareArray = Array of TFareInfo;
  TPricingInfoTypesegmentPricingArray = Array of TSegmentPricing;
  TPricingInfoTypetaxArray = Array of TTaxInfo;
  TSegmentInfoTypelegArray = Array of TLegInfo;
  TSegmentPricingTypefreeBaggageOptionArray = Array of TFreeBaggageAllowance;
  TSliceInfoTypesegmentArray = Array of TSegmentInfo;
  TTripOptionTypepricingArray = Array of TPricingInfo;
  TTripOptionTypesliceArray = Array of TSliceInfo;
  TTripOptionsRequestTypesliceArray = Array of TSliceInput;
  TTripOptionsResponseTypetripOptionArray = Array of TTripOption;
  
  { --------------------------------------------------------------------
    TAircraftData
    --------------------------------------------------------------------}
  
  TAircraftData = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TAircraftDataClass = Class of TAircraftData;
  
  { --------------------------------------------------------------------
    TAirportData
    --------------------------------------------------------------------}
  
  TAirportData = Class(TGoogleBaseObject)
  Private
    Fcity : String;
    Fcode : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setcity(AIndex : Integer; AValue : String); virtual;
    Procedure Setcode(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property city : String Index 0 Read Fcity Write Setcity;
    Property code : String Index 8 Read Fcode Write Setcode;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property name : String Index 24 Read Fname Write Setname;
  end;
  TAirportDataClass = Class of TAirportData;
  
  { --------------------------------------------------------------------
    TBagDescriptor
    --------------------------------------------------------------------}
  
  TBagDescriptor = Class(TGoogleBaseObject)
  Private
    FcommercialName : String;
    Fcount : integer;
    Fdescription : TStringArray;
    Fkind : String;
    Fsubcode : String;
  Protected
    //Property setters
    Procedure SetcommercialName(AIndex : Integer; AValue : String); virtual;
    Procedure Setcount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsubcode(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property commercialName : String Index 0 Read FcommercialName Write SetcommercialName;
    Property count : integer Index 8 Read Fcount Write Setcount;
    Property description : TStringArray Index 16 Read Fdescription Write Setdescription;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property subcode : String Index 32 Read Fsubcode Write Setsubcode;
  end;
  TBagDescriptorClass = Class of TBagDescriptor;
  
  { --------------------------------------------------------------------
    TCarrierData
    --------------------------------------------------------------------}
  
  TCarrierData = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TCarrierDataClass = Class of TCarrierData;
  
  { --------------------------------------------------------------------
    TCityData
    --------------------------------------------------------------------}
  
  TCityData = Class(TGoogleBaseObject)
  Private
    Fcode : String;
    Fcountry : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : String); virtual;
    Procedure Setcountry(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property code : String Index 0 Read Fcode Write Setcode;
    Property country : String Index 8 Read Fcountry Write Setcountry;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property name : String Index 24 Read Fname Write Setname;
  end;
  TCityDataClass = Class of TCityData;
  
  { --------------------------------------------------------------------
    TData
    --------------------------------------------------------------------}
  
  TData = Class(TGoogleBaseObject)
  Private
    Faircraft : TDataTypeaircraftArray;
    Fairport : TDataTypeairportArray;
    Fcarrier : TDataTypecarrierArray;
    Fcity : TDataTypecityArray;
    Fkind : String;
    Ftax : TDataTypetaxArray;
  Protected
    //Property setters
    Procedure Setaircraft(AIndex : Integer; AValue : TDataTypeaircraftArray); virtual;
    Procedure Setairport(AIndex : Integer; AValue : TDataTypeairportArray); virtual;
    Procedure Setcarrier(AIndex : Integer; AValue : TDataTypecarrierArray); virtual;
    Procedure Setcity(AIndex : Integer; AValue : TDataTypecityArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Settax(AIndex : Integer; AValue : TDataTypetaxArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property aircraft : TDataTypeaircraftArray Index 0 Read Faircraft Write Setaircraft;
    Property airport : TDataTypeairportArray Index 8 Read Fairport Write Setairport;
    Property carrier : TDataTypecarrierArray Index 16 Read Fcarrier Write Setcarrier;
    Property city : TDataTypecityArray Index 24 Read Fcity Write Setcity;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property tax : TDataTypetaxArray Index 40 Read Ftax Write Settax;
  end;
  TDataClass = Class of TData;
  
  { --------------------------------------------------------------------
    TFareInfo
    --------------------------------------------------------------------}
  
  TFareInfo = Class(TGoogleBaseObject)
  Private
    FbasisCode : String;
    Fcarrier : String;
    Fdestination : String;
    Fid : String;
    Fkind : String;
    Forigin : String;
    F_private : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetbasisCode(AIndex : Integer; AValue : String); virtual;
    Procedure Setcarrier(AIndex : Integer; AValue : String); virtual;
    Procedure Setdestination(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setorigin(AIndex : Integer; AValue : String); virtual;
    Procedure Set_private(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property basisCode : String Index 0 Read FbasisCode Write SetbasisCode;
    Property carrier : String Index 8 Read Fcarrier Write Setcarrier;
    Property destination : String Index 16 Read Fdestination Write Setdestination;
    Property id : String Index 24 Read Fid Write Setid;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property origin : String Index 40 Read Forigin Write Setorigin;
    Property _private : boolean Index 48 Read F_private Write Set_private;
  end;
  TFareInfoClass = Class of TFareInfo;
  
  { --------------------------------------------------------------------
    TFlightInfo
    --------------------------------------------------------------------}
  
  TFlightInfo = Class(TGoogleBaseObject)
  Private
    Fcarrier : String;
    Fnumber : String;
  Protected
    //Property setters
    Procedure Setcarrier(AIndex : Integer; AValue : String); virtual;
    Procedure Setnumber(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property carrier : String Index 0 Read Fcarrier Write Setcarrier;
    Property number : String Index 8 Read Fnumber Write Setnumber;
  end;
  TFlightInfoClass = Class of TFlightInfo;
  
  { --------------------------------------------------------------------
    TFreeBaggageAllowance
    --------------------------------------------------------------------}
  
  TFreeBaggageAllowance = Class(TGoogleBaseObject)
  Private
    FbagDescriptor : TFreeBaggageAllowanceTypebagDescriptorArray;
    Fkilos : integer;
    FkilosPerPiece : integer;
    Fkind : String;
    Fpieces : integer;
    Fpounds : integer;
  Protected
    //Property setters
    Procedure SetbagDescriptor(AIndex : Integer; AValue : TFreeBaggageAllowanceTypebagDescriptorArray); virtual;
    Procedure Setkilos(AIndex : Integer; AValue : integer); virtual;
    Procedure SetkilosPerPiece(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setpieces(AIndex : Integer; AValue : integer); virtual;
    Procedure Setpounds(AIndex : Integer; AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property bagDescriptor : TFreeBaggageAllowanceTypebagDescriptorArray Index 0 Read FbagDescriptor Write SetbagDescriptor;
    Property kilos : integer Index 8 Read Fkilos Write Setkilos;
    Property kilosPerPiece : integer Index 16 Read FkilosPerPiece Write SetkilosPerPiece;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property pieces : integer Index 32 Read Fpieces Write Setpieces;
    Property pounds : integer Index 40 Read Fpounds Write Setpounds;
  end;
  TFreeBaggageAllowanceClass = Class of TFreeBaggageAllowance;
  
  { --------------------------------------------------------------------
    TLegInfo
    --------------------------------------------------------------------}
  
  TLegInfo = Class(TGoogleBaseObject)
  Private
    Faircraft : String;
    FarrivalTime : String;
    FchangePlane : boolean;
    FconnectionDuration : integer;
    FdepartureTime : String;
    Fdestination : String;
    FdestinationTerminal : String;
    Fduration : integer;
    Fid : String;
    Fkind : String;
    Fmeal : String;
    Fmileage : integer;
    FonTimePerformance : integer;
    FoperatingDisclosure : String;
    Forigin : String;
    ForiginTerminal : String;
    Fsecure : boolean;
  Protected
    //Property setters
    Procedure Setaircraft(AIndex : Integer; AValue : String); virtual;
    Procedure SetarrivalTime(AIndex : Integer; AValue : String); virtual;
    Procedure SetchangePlane(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetconnectionDuration(AIndex : Integer; AValue : integer); virtual;
    Procedure SetdepartureTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setdestination(AIndex : Integer; AValue : String); virtual;
    Procedure SetdestinationTerminal(AIndex : Integer; AValue : String); virtual;
    Procedure Setduration(AIndex : Integer; AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setmeal(AIndex : Integer; AValue : String); virtual;
    Procedure Setmileage(AIndex : Integer; AValue : integer); virtual;
    Procedure SetonTimePerformance(AIndex : Integer; AValue : integer); virtual;
    Procedure SetoperatingDisclosure(AIndex : Integer; AValue : String); virtual;
    Procedure Setorigin(AIndex : Integer; AValue : String); virtual;
    Procedure SetoriginTerminal(AIndex : Integer; AValue : String); virtual;
    Procedure Setsecure(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property aircraft : String Index 0 Read Faircraft Write Setaircraft;
    Property arrivalTime : String Index 8 Read FarrivalTime Write SetarrivalTime;
    Property changePlane : boolean Index 16 Read FchangePlane Write SetchangePlane;
    Property connectionDuration : integer Index 24 Read FconnectionDuration Write SetconnectionDuration;
    Property departureTime : String Index 32 Read FdepartureTime Write SetdepartureTime;
    Property destination : String Index 40 Read Fdestination Write Setdestination;
    Property destinationTerminal : String Index 48 Read FdestinationTerminal Write SetdestinationTerminal;
    Property duration : integer Index 56 Read Fduration Write Setduration;
    Property id : String Index 64 Read Fid Write Setid;
    Property kind : String Index 72 Read Fkind Write Setkind;
    Property meal : String Index 80 Read Fmeal Write Setmeal;
    Property mileage : integer Index 88 Read Fmileage Write Setmileage;
    Property onTimePerformance : integer Index 96 Read FonTimePerformance Write SetonTimePerformance;
    Property operatingDisclosure : String Index 104 Read FoperatingDisclosure Write SetoperatingDisclosure;
    Property origin : String Index 112 Read Forigin Write Setorigin;
    Property originTerminal : String Index 120 Read ForiginTerminal Write SetoriginTerminal;
    Property secure : boolean Index 128 Read Fsecure Write Setsecure;
  end;
  TLegInfoClass = Class of TLegInfo;
  
  { --------------------------------------------------------------------
    TPassengerCounts
    --------------------------------------------------------------------}
  
  TPassengerCounts = Class(TGoogleBaseObject)
  Private
    FadultCount : integer;
    FchildCount : integer;
    FinfantInLapCount : integer;
    FinfantInSeatCount : integer;
    Fkind : String;
    FseniorCount : integer;
  Protected
    //Property setters
    Procedure SetadultCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetchildCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetinfantInLapCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetinfantInSeatCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetseniorCount(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property adultCount : integer Index 0 Read FadultCount Write SetadultCount;
    Property childCount : integer Index 8 Read FchildCount Write SetchildCount;
    Property infantInLapCount : integer Index 16 Read FinfantInLapCount Write SetinfantInLapCount;
    Property infantInSeatCount : integer Index 24 Read FinfantInSeatCount Write SetinfantInSeatCount;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property seniorCount : integer Index 40 Read FseniorCount Write SetseniorCount;
  end;
  TPassengerCountsClass = Class of TPassengerCounts;
  
  { --------------------------------------------------------------------
    TPricingInfo
    --------------------------------------------------------------------}
  
  TPricingInfo = Class(TGoogleBaseObject)
  Private
    FbaseFareTotal : String;
    Ffare : TPricingInfoTypefareArray;
    FfareCalculation : String;
    Fkind : String;
    FlatestTicketingTime : String;
    Fpassengers : TPassengerCounts;
    Fptc : String;
    Frefundable : boolean;
    FsaleFareTotal : String;
    FsaleTaxTotal : String;
    FsaleTotal : String;
    FsegmentPricing : TPricingInfoTypesegmentPricingArray;
    Ftax : TPricingInfoTypetaxArray;
  Protected
    //Property setters
    Procedure SetbaseFareTotal(AIndex : Integer; AValue : String); virtual;
    Procedure Setfare(AIndex : Integer; AValue : TPricingInfoTypefareArray); virtual;
    Procedure SetfareCalculation(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlatestTicketingTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setpassengers(AIndex : Integer; AValue : TPassengerCounts); virtual;
    Procedure Setptc(AIndex : Integer; AValue : String); virtual;
    Procedure Setrefundable(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsaleFareTotal(AIndex : Integer; AValue : String); virtual;
    Procedure SetsaleTaxTotal(AIndex : Integer; AValue : String); virtual;
    Procedure SetsaleTotal(AIndex : Integer; AValue : String); virtual;
    Procedure SetsegmentPricing(AIndex : Integer; AValue : TPricingInfoTypesegmentPricingArray); virtual;
    Procedure Settax(AIndex : Integer; AValue : TPricingInfoTypetaxArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property baseFareTotal : String Index 0 Read FbaseFareTotal Write SetbaseFareTotal;
    Property fare : TPricingInfoTypefareArray Index 8 Read Ffare Write Setfare;
    Property fareCalculation : String Index 16 Read FfareCalculation Write SetfareCalculation;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property latestTicketingTime : String Index 32 Read FlatestTicketingTime Write SetlatestTicketingTime;
    Property passengers : TPassengerCounts Index 40 Read Fpassengers Write Setpassengers;
    Property ptc : String Index 48 Read Fptc Write Setptc;
    Property refundable : boolean Index 56 Read Frefundable Write Setrefundable;
    Property saleFareTotal : String Index 64 Read FsaleFareTotal Write SetsaleFareTotal;
    Property saleTaxTotal : String Index 72 Read FsaleTaxTotal Write SetsaleTaxTotal;
    Property saleTotal : String Index 80 Read FsaleTotal Write SetsaleTotal;
    Property segmentPricing : TPricingInfoTypesegmentPricingArray Index 88 Read FsegmentPricing Write SetsegmentPricing;
    Property tax : TPricingInfoTypetaxArray Index 96 Read Ftax Write Settax;
  end;
  TPricingInfoClass = Class of TPricingInfo;
  
  { --------------------------------------------------------------------
    TSegmentInfo
    --------------------------------------------------------------------}
  
  TSegmentInfo = Class(TGoogleBaseObject)
  Private
    FbookingCode : String;
    FbookingCodeCount : integer;
    Fcabin : String;
    FconnectionDuration : integer;
    Fduration : integer;
    Fflight : TFlightInfo;
    Fid : String;
    Fkind : String;
    Fleg : TSegmentInfoTypelegArray;
    FmarriedSegmentGroup : String;
    FsubjectToGovernmentApproval : boolean;
  Protected
    //Property setters
    Procedure SetbookingCode(AIndex : Integer; AValue : String); virtual;
    Procedure SetbookingCodeCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setcabin(AIndex : Integer; AValue : String); virtual;
    Procedure SetconnectionDuration(AIndex : Integer; AValue : integer); virtual;
    Procedure Setduration(AIndex : Integer; AValue : integer); virtual;
    Procedure Setflight(AIndex : Integer; AValue : TFlightInfo); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setleg(AIndex : Integer; AValue : TSegmentInfoTypelegArray); virtual;
    Procedure SetmarriedSegmentGroup(AIndex : Integer; AValue : String); virtual;
    Procedure SetsubjectToGovernmentApproval(AIndex : Integer; AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property bookingCode : String Index 0 Read FbookingCode Write SetbookingCode;
    Property bookingCodeCount : integer Index 8 Read FbookingCodeCount Write SetbookingCodeCount;
    Property cabin : String Index 16 Read Fcabin Write Setcabin;
    Property connectionDuration : integer Index 24 Read FconnectionDuration Write SetconnectionDuration;
    Property duration : integer Index 32 Read Fduration Write Setduration;
    Property flight : TFlightInfo Index 40 Read Fflight Write Setflight;
    Property id : String Index 48 Read Fid Write Setid;
    Property kind : String Index 56 Read Fkind Write Setkind;
    Property leg : TSegmentInfoTypelegArray Index 64 Read Fleg Write Setleg;
    Property marriedSegmentGroup : String Index 72 Read FmarriedSegmentGroup Write SetmarriedSegmentGroup;
    Property subjectToGovernmentApproval : boolean Index 80 Read FsubjectToGovernmentApproval Write SetsubjectToGovernmentApproval;
  end;
  TSegmentInfoClass = Class of TSegmentInfo;
  
  { --------------------------------------------------------------------
    TSegmentPricing
    --------------------------------------------------------------------}
  
  TSegmentPricing = Class(TGoogleBaseObject)
  Private
    FfareId : String;
    FfreeBaggageOption : TSegmentPricingTypefreeBaggageOptionArray;
    Fkind : String;
    FsegmentId : String;
  Protected
    //Property setters
    Procedure SetfareId(AIndex : Integer; AValue : String); virtual;
    Procedure SetfreeBaggageOption(AIndex : Integer; AValue : TSegmentPricingTypefreeBaggageOptionArray); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetsegmentId(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property fareId : String Index 0 Read FfareId Write SetfareId;
    Property freeBaggageOption : TSegmentPricingTypefreeBaggageOptionArray Index 8 Read FfreeBaggageOption Write SetfreeBaggageOption;
    Property kind : String Index 16 Read Fkind Write Setkind;
    Property segmentId : String Index 24 Read FsegmentId Write SetsegmentId;
  end;
  TSegmentPricingClass = Class of TSegmentPricing;
  
  { --------------------------------------------------------------------
    TSliceInfo
    --------------------------------------------------------------------}
  
  TSliceInfo = Class(TGoogleBaseObject)
  Private
    Fduration : integer;
    Fkind : String;
    Fsegment : TSliceInfoTypesegmentArray;
  Protected
    //Property setters
    Procedure Setduration(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setsegment(AIndex : Integer; AValue : TSliceInfoTypesegmentArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property duration : integer Index 0 Read Fduration Write Setduration;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property segment : TSliceInfoTypesegmentArray Index 16 Read Fsegment Write Setsegment;
  end;
  TSliceInfoClass = Class of TSliceInfo;
  
  { --------------------------------------------------------------------
    TSliceInput
    --------------------------------------------------------------------}
  
  TSliceInput = Class(TGoogleBaseObject)
  Private
    Falliance : String;
    Fdate : String;
    Fdestination : String;
    Fkind : String;
    FmaxConnectionDuration : integer;
    FmaxStops : integer;
    Forigin : String;
    FpermittedCarrier : TStringArray;
    FpermittedDepartureTime : TTimeOfDayRange;
    FpreferredCabin : String;
    FprohibitedCarrier : TStringArray;
  Protected
    //Property setters
    Procedure Setalliance(AIndex : Integer; AValue : String); virtual;
    Procedure Setdate(AIndex : Integer; AValue : String); virtual;
    Procedure Setdestination(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaxConnectionDuration(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmaxStops(AIndex : Integer; AValue : integer); virtual;
    Procedure Setorigin(AIndex : Integer; AValue : String); virtual;
    Procedure SetpermittedCarrier(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetpermittedDepartureTime(AIndex : Integer; AValue : TTimeOfDayRange); virtual;
    Procedure SetpreferredCabin(AIndex : Integer; AValue : String); virtual;
    Procedure SetprohibitedCarrier(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property alliance : String Index 0 Read Falliance Write Setalliance;
    Property date : String Index 8 Read Fdate Write Setdate;
    Property destination : String Index 16 Read Fdestination Write Setdestination;
    Property kind : String Index 24 Read Fkind Write Setkind;
    Property maxConnectionDuration : integer Index 32 Read FmaxConnectionDuration Write SetmaxConnectionDuration;
    Property maxStops : integer Index 40 Read FmaxStops Write SetmaxStops;
    Property origin : String Index 48 Read Forigin Write Setorigin;
    Property permittedCarrier : TStringArray Index 56 Read FpermittedCarrier Write SetpermittedCarrier;
    Property permittedDepartureTime : TTimeOfDayRange Index 64 Read FpermittedDepartureTime Write SetpermittedDepartureTime;
    Property preferredCabin : String Index 72 Read FpreferredCabin Write SetpreferredCabin;
    Property prohibitedCarrier : TStringArray Index 80 Read FprohibitedCarrier Write SetprohibitedCarrier;
  end;
  TSliceInputClass = Class of TSliceInput;
  
  { --------------------------------------------------------------------
    TTaxData
    --------------------------------------------------------------------}
  
  TTaxData = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TTaxDataClass = Class of TTaxData;
  
  { --------------------------------------------------------------------
    TTaxInfo
    --------------------------------------------------------------------}
  
  TTaxInfo = Class(TGoogleBaseObject)
  Private
    FchargeType : String;
    Fcode : String;
    Fcountry : String;
    Fid : String;
    Fkind : String;
    FsalePrice : String;
  Protected
    //Property setters
    Procedure SetchargeType(AIndex : Integer; AValue : String); virtual;
    Procedure Setcode(AIndex : Integer; AValue : String); virtual;
    Procedure Setcountry(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetsalePrice(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property chargeType : String Index 0 Read FchargeType Write SetchargeType;
    Property code : String Index 8 Read Fcode Write Setcode;
    Property country : String Index 16 Read Fcountry Write Setcountry;
    Property id : String Index 24 Read Fid Write Setid;
    Property kind : String Index 32 Read Fkind Write Setkind;
    Property salePrice : String Index 40 Read FsalePrice Write SetsalePrice;
  end;
  TTaxInfoClass = Class of TTaxInfo;
  
  { --------------------------------------------------------------------
    TTimeOfDayRange
    --------------------------------------------------------------------}
  
  TTimeOfDayRange = Class(TGoogleBaseObject)
  Private
    FearliestTime : String;
    Fkind : String;
    FlatestTime : String;
  Protected
    //Property setters
    Procedure SetearliestTime(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetlatestTime(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property earliestTime : String Index 0 Read FearliestTime Write SetearliestTime;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property latestTime : String Index 16 Read FlatestTime Write SetlatestTime;
  end;
  TTimeOfDayRangeClass = Class of TTimeOfDayRange;
  
  { --------------------------------------------------------------------
    TTripOption
    --------------------------------------------------------------------}
  
  TTripOption = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    Fpricing : TTripOptionTypepricingArray;
    FsaleTotal : String;
    Fslice : TTripOptionTypesliceArray;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setpricing(AIndex : Integer; AValue : TTripOptionTypepricingArray); virtual;
    Procedure SetsaleTotal(AIndex : Integer; AValue : String); virtual;
    Procedure Setslice(AIndex : Integer; AValue : TTripOptionTypesliceArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property pricing : TTripOptionTypepricingArray Index 16 Read Fpricing Write Setpricing;
    Property saleTotal : String Index 24 Read FsaleTotal Write SetsaleTotal;
    Property slice : TTripOptionTypesliceArray Index 32 Read Fslice Write Setslice;
  end;
  TTripOptionClass = Class of TTripOption;
  
  { --------------------------------------------------------------------
    TTripOptionsRequest
    --------------------------------------------------------------------}
  
  TTripOptionsRequest = Class(TGoogleBaseObject)
  Private
    FmaxPrice : String;
    Fpassengers : TPassengerCounts;
    Frefundable : boolean;
    FsaleCountry : String;
    Fslice : TTripOptionsRequestTypesliceArray;
    Fsolutions : integer;
  Protected
    //Property setters
    Procedure SetmaxPrice(AIndex : Integer; AValue : String); virtual;
    Procedure Setpassengers(AIndex : Integer; AValue : TPassengerCounts); virtual;
    Procedure Setrefundable(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsaleCountry(AIndex : Integer; AValue : String); virtual;
    Procedure Setslice(AIndex : Integer; AValue : TTripOptionsRequestTypesliceArray); virtual;
    Procedure Setsolutions(AIndex : Integer; AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property maxPrice : String Index 0 Read FmaxPrice Write SetmaxPrice;
    Property passengers : TPassengerCounts Index 8 Read Fpassengers Write Setpassengers;
    Property refundable : boolean Index 16 Read Frefundable Write Setrefundable;
    Property saleCountry : String Index 24 Read FsaleCountry Write SetsaleCountry;
    Property slice : TTripOptionsRequestTypesliceArray Index 32 Read Fslice Write Setslice;
    Property solutions : integer Index 40 Read Fsolutions Write Setsolutions;
  end;
  TTripOptionsRequestClass = Class of TTripOptionsRequest;
  
  { --------------------------------------------------------------------
    TTripOptionsResponse
    --------------------------------------------------------------------}
  
  TTripOptionsResponse = Class(TGoogleBaseObject)
  Private
    Fdata : TData;
    Fkind : String;
    FrequestId : String;
    FtripOption : TTripOptionsResponseTypetripOptionArray;
  Protected
    //Property setters
    Procedure Setdata(AIndex : Integer; AValue : TData); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure SetrequestId(AIndex : Integer; AValue : String); virtual;
    Procedure SettripOption(AIndex : Integer; AValue : TTripOptionsResponseTypetripOptionArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property data : TData Index 0 Read Fdata Write Setdata;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property requestId : String Index 16 Read FrequestId Write SetrequestId;
    Property tripOption : TTripOptionsResponseTypetripOptionArray Index 24 Read FtripOption Write SettripOption;
  end;
  TTripOptionsResponseClass = Class of TTripOptionsResponse;
  
  { --------------------------------------------------------------------
    TTripsSearchRequest
    --------------------------------------------------------------------}
  
  TTripsSearchRequest = Class(TGoogleBaseObject)
  Private
    Frequest : TTripOptionsRequest;
  Protected
    //Property setters
    Procedure Setrequest(AIndex : Integer; AValue : TTripOptionsRequest); virtual;
  Public
  Published
    Property request : TTripOptionsRequest Index 0 Read Frequest Write Setrequest;
  end;
  TTripsSearchRequestClass = Class of TTripsSearchRequest;
  
  { --------------------------------------------------------------------
    TTripsSearchResponse
    --------------------------------------------------------------------}
  
  TTripsSearchResponse = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Ftrips : TTripOptionsResponse;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Settrips(AIndex : Integer; AValue : TTripOptionsResponse); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property trips : TTripOptionsResponse Index 8 Read Ftrips Write Settrips;
  end;
  TTripsSearchResponseClass = Class of TTripsSearchResponse;
  
  { --------------------------------------------------------------------
    TTripsResource
    --------------------------------------------------------------------}
  
  TTripsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Search(aTripsSearchRequest : TTripsSearchRequest) : TTripsSearchResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TQpxExpressAPI
    --------------------------------------------------------------------}
  
  TQpxExpressAPI = Class(TGoogleAPI)
  Private
    FTripsInstance : TTripsResource;
    Function GetTripsInstance : TTripsResource;virtual;
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
    Function CreateTripsResource(AOwner : TComponent) : TTripsResource;virtual;overload;
    Function CreateTripsResource : TTripsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property TripsResource : TTripsResource Read GetTripsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAircraftData
  --------------------------------------------------------------------}


Procedure TAircraftData.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAircraftData.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAircraftData.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAirportData
  --------------------------------------------------------------------}


Procedure TAirportData.Setcity(AIndex : Integer; AValue : String); 

begin
  If (Fcity=AValue) then exit;
  Fcity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAirportData.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAirportData.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAirportData.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBagDescriptor
  --------------------------------------------------------------------}


Procedure TBagDescriptor.SetcommercialName(AIndex : Integer; AValue : String); 

begin
  If (FcommercialName=AValue) then exit;
  FcommercialName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBagDescriptor.Setcount(AIndex : Integer; AValue : integer); 

begin
  If (Fcount=AValue) then exit;
  Fcount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBagDescriptor.Setdescription(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBagDescriptor.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBagDescriptor.Setsubcode(AIndex : Integer; AValue : String); 

begin
  If (Fsubcode=AValue) then exit;
  Fsubcode:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBagDescriptor.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'description' : SetLength(Fdescription,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCarrierData
  --------------------------------------------------------------------}


Procedure TCarrierData.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCarrierData.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCarrierData.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCityData
  --------------------------------------------------------------------}


Procedure TCityData.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCityData.Setcountry(AIndex : Integer; AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCityData.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCityData.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TData
  --------------------------------------------------------------------}


Procedure TData.Setaircraft(AIndex : Integer; AValue : TDataTypeaircraftArray); 

begin
  If (Faircraft=AValue) then exit;
  Faircraft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TData.Setairport(AIndex : Integer; AValue : TDataTypeairportArray); 

begin
  If (Fairport=AValue) then exit;
  Fairport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TData.Setcarrier(AIndex : Integer; AValue : TDataTypecarrierArray); 

begin
  If (Fcarrier=AValue) then exit;
  Fcarrier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TData.Setcity(AIndex : Integer; AValue : TDataTypecityArray); 

begin
  If (Fcity=AValue) then exit;
  Fcity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TData.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TData.Settax(AIndex : Integer; AValue : TDataTypetaxArray); 

begin
  If (Ftax=AValue) then exit;
  Ftax:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TData.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'aircraft' : SetLength(Faircraft,ALength);
  'airport' : SetLength(Fairport,ALength);
  'carrier' : SetLength(Fcarrier,ALength);
  'city' : SetLength(Fcity,ALength);
  'tax' : SetLength(Ftax,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TFareInfo
  --------------------------------------------------------------------}


Procedure TFareInfo.SetbasisCode(AIndex : Integer; AValue : String); 

begin
  If (FbasisCode=AValue) then exit;
  FbasisCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFareInfo.Setcarrier(AIndex : Integer; AValue : String); 

begin
  If (Fcarrier=AValue) then exit;
  Fcarrier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFareInfo.Setdestination(AIndex : Integer; AValue : String); 

begin
  If (Fdestination=AValue) then exit;
  Fdestination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFareInfo.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFareInfo.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFareInfo.Setorigin(AIndex : Integer; AValue : String); 

begin
  If (Forigin=AValue) then exit;
  Forigin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFareInfo.Set_private(AIndex : Integer; AValue : boolean); 

begin
  If (F_private=AValue) then exit;
  F_private:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TFareInfo.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_private' : Result:='private';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TFlightInfo
  --------------------------------------------------------------------}


Procedure TFlightInfo.Setcarrier(AIndex : Integer; AValue : String); 

begin
  If (Fcarrier=AValue) then exit;
  Fcarrier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFlightInfo.Setnumber(AIndex : Integer; AValue : String); 

begin
  If (Fnumber=AValue) then exit;
  Fnumber:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFreeBaggageAllowance
  --------------------------------------------------------------------}


Procedure TFreeBaggageAllowance.SetbagDescriptor(AIndex : Integer; AValue : TFreeBaggageAllowanceTypebagDescriptorArray); 

begin
  If (FbagDescriptor=AValue) then exit;
  FbagDescriptor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFreeBaggageAllowance.Setkilos(AIndex : Integer; AValue : integer); 

begin
  If (Fkilos=AValue) then exit;
  Fkilos:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFreeBaggageAllowance.SetkilosPerPiece(AIndex : Integer; AValue : integer); 

begin
  If (FkilosPerPiece=AValue) then exit;
  FkilosPerPiece:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFreeBaggageAllowance.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFreeBaggageAllowance.Setpieces(AIndex : Integer; AValue : integer); 

begin
  If (Fpieces=AValue) then exit;
  Fpieces:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFreeBaggageAllowance.Setpounds(AIndex : Integer; AValue : integer); 

begin
  If (Fpounds=AValue) then exit;
  Fpounds:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TFreeBaggageAllowance.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'bagdescriptor' : SetLength(FbagDescriptor,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLegInfo
  --------------------------------------------------------------------}


Procedure TLegInfo.Setaircraft(AIndex : Integer; AValue : String); 

begin
  If (Faircraft=AValue) then exit;
  Faircraft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.SetarrivalTime(AIndex : Integer; AValue : String); 

begin
  If (FarrivalTime=AValue) then exit;
  FarrivalTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.SetchangePlane(AIndex : Integer; AValue : boolean); 

begin
  If (FchangePlane=AValue) then exit;
  FchangePlane:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.SetconnectionDuration(AIndex : Integer; AValue : integer); 

begin
  If (FconnectionDuration=AValue) then exit;
  FconnectionDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.SetdepartureTime(AIndex : Integer; AValue : String); 

begin
  If (FdepartureTime=AValue) then exit;
  FdepartureTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.Setdestination(AIndex : Integer; AValue : String); 

begin
  If (Fdestination=AValue) then exit;
  Fdestination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.SetdestinationTerminal(AIndex : Integer; AValue : String); 

begin
  If (FdestinationTerminal=AValue) then exit;
  FdestinationTerminal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.Setduration(AIndex : Integer; AValue : integer); 

begin
  If (Fduration=AValue) then exit;
  Fduration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.Setmeal(AIndex : Integer; AValue : String); 

begin
  If (Fmeal=AValue) then exit;
  Fmeal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.Setmileage(AIndex : Integer; AValue : integer); 

begin
  If (Fmileage=AValue) then exit;
  Fmileage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.SetonTimePerformance(AIndex : Integer; AValue : integer); 

begin
  If (FonTimePerformance=AValue) then exit;
  FonTimePerformance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.SetoperatingDisclosure(AIndex : Integer; AValue : String); 

begin
  If (FoperatingDisclosure=AValue) then exit;
  FoperatingDisclosure:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.Setorigin(AIndex : Integer; AValue : String); 

begin
  If (Forigin=AValue) then exit;
  Forigin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.SetoriginTerminal(AIndex : Integer; AValue : String); 

begin
  If (ForiginTerminal=AValue) then exit;
  ForiginTerminal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.Setsecure(AIndex : Integer; AValue : boolean); 

begin
  If (Fsecure=AValue) then exit;
  Fsecure:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPassengerCounts
  --------------------------------------------------------------------}


Procedure TPassengerCounts.SetadultCount(AIndex : Integer; AValue : integer); 

begin
  If (FadultCount=AValue) then exit;
  FadultCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPassengerCounts.SetchildCount(AIndex : Integer; AValue : integer); 

begin
  If (FchildCount=AValue) then exit;
  FchildCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPassengerCounts.SetinfantInLapCount(AIndex : Integer; AValue : integer); 

begin
  If (FinfantInLapCount=AValue) then exit;
  FinfantInLapCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPassengerCounts.SetinfantInSeatCount(AIndex : Integer; AValue : integer); 

begin
  If (FinfantInSeatCount=AValue) then exit;
  FinfantInSeatCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPassengerCounts.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPassengerCounts.SetseniorCount(AIndex : Integer; AValue : integer); 

begin
  If (FseniorCount=AValue) then exit;
  FseniorCount:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPricingInfo
  --------------------------------------------------------------------}


Procedure TPricingInfo.SetbaseFareTotal(AIndex : Integer; AValue : String); 

begin
  If (FbaseFareTotal=AValue) then exit;
  FbaseFareTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.Setfare(AIndex : Integer; AValue : TPricingInfoTypefareArray); 

begin
  If (Ffare=AValue) then exit;
  Ffare:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.SetfareCalculation(AIndex : Integer; AValue : String); 

begin
  If (FfareCalculation=AValue) then exit;
  FfareCalculation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.SetlatestTicketingTime(AIndex : Integer; AValue : String); 

begin
  If (FlatestTicketingTime=AValue) then exit;
  FlatestTicketingTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.Setpassengers(AIndex : Integer; AValue : TPassengerCounts); 

begin
  If (Fpassengers=AValue) then exit;
  Fpassengers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.Setptc(AIndex : Integer; AValue : String); 

begin
  If (Fptc=AValue) then exit;
  Fptc:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.Setrefundable(AIndex : Integer; AValue : boolean); 

begin
  If (Frefundable=AValue) then exit;
  Frefundable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.SetsaleFareTotal(AIndex : Integer; AValue : String); 

begin
  If (FsaleFareTotal=AValue) then exit;
  FsaleFareTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.SetsaleTaxTotal(AIndex : Integer; AValue : String); 

begin
  If (FsaleTaxTotal=AValue) then exit;
  FsaleTaxTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.SetsaleTotal(AIndex : Integer; AValue : String); 

begin
  If (FsaleTotal=AValue) then exit;
  FsaleTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.SetsegmentPricing(AIndex : Integer; AValue : TPricingInfoTypesegmentPricingArray); 

begin
  If (FsegmentPricing=AValue) then exit;
  FsegmentPricing:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.Settax(AIndex : Integer; AValue : TPricingInfoTypetaxArray); 

begin
  If (Ftax=AValue) then exit;
  Ftax:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPricingInfo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'fare' : SetLength(Ffare,ALength);
  'segmentpricing' : SetLength(FsegmentPricing,ALength);
  'tax' : SetLength(Ftax,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSegmentInfo
  --------------------------------------------------------------------}


Procedure TSegmentInfo.SetbookingCode(AIndex : Integer; AValue : String); 

begin
  If (FbookingCode=AValue) then exit;
  FbookingCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentInfo.SetbookingCodeCount(AIndex : Integer; AValue : integer); 

begin
  If (FbookingCodeCount=AValue) then exit;
  FbookingCodeCount:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentInfo.Setcabin(AIndex : Integer; AValue : String); 

begin
  If (Fcabin=AValue) then exit;
  Fcabin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentInfo.SetconnectionDuration(AIndex : Integer; AValue : integer); 

begin
  If (FconnectionDuration=AValue) then exit;
  FconnectionDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentInfo.Setduration(AIndex : Integer; AValue : integer); 

begin
  If (Fduration=AValue) then exit;
  Fduration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentInfo.Setflight(AIndex : Integer; AValue : TFlightInfo); 

begin
  If (Fflight=AValue) then exit;
  Fflight:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentInfo.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentInfo.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentInfo.Setleg(AIndex : Integer; AValue : TSegmentInfoTypelegArray); 

begin
  If (Fleg=AValue) then exit;
  Fleg:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentInfo.SetmarriedSegmentGroup(AIndex : Integer; AValue : String); 

begin
  If (FmarriedSegmentGroup=AValue) then exit;
  FmarriedSegmentGroup:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentInfo.SetsubjectToGovernmentApproval(AIndex : Integer; AValue : boolean); 

begin
  If (FsubjectToGovernmentApproval=AValue) then exit;
  FsubjectToGovernmentApproval:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSegmentInfo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'leg' : SetLength(Fleg,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSegmentPricing
  --------------------------------------------------------------------}


Procedure TSegmentPricing.SetfareId(AIndex : Integer; AValue : String); 

begin
  If (FfareId=AValue) then exit;
  FfareId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentPricing.SetfreeBaggageOption(AIndex : Integer; AValue : TSegmentPricingTypefreeBaggageOptionArray); 

begin
  If (FfreeBaggageOption=AValue) then exit;
  FfreeBaggageOption:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentPricing.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentPricing.SetsegmentId(AIndex : Integer; AValue : String); 

begin
  If (FsegmentId=AValue) then exit;
  FsegmentId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSegmentPricing.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'freebaggageoption' : SetLength(FfreeBaggageOption,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSliceInfo
  --------------------------------------------------------------------}


Procedure TSliceInfo.Setduration(AIndex : Integer; AValue : integer); 

begin
  If (Fduration=AValue) then exit;
  Fduration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInfo.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInfo.Setsegment(AIndex : Integer; AValue : TSliceInfoTypesegmentArray); 

begin
  If (Fsegment=AValue) then exit;
  Fsegment:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSliceInfo.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'segment' : SetLength(Fsegment,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSliceInput
  --------------------------------------------------------------------}


Procedure TSliceInput.Setalliance(AIndex : Integer; AValue : String); 

begin
  If (Falliance=AValue) then exit;
  Falliance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInput.Setdate(AIndex : Integer; AValue : String); 

begin
  If (Fdate=AValue) then exit;
  Fdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInput.Setdestination(AIndex : Integer; AValue : String); 

begin
  If (Fdestination=AValue) then exit;
  Fdestination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInput.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInput.SetmaxConnectionDuration(AIndex : Integer; AValue : integer); 

begin
  If (FmaxConnectionDuration=AValue) then exit;
  FmaxConnectionDuration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInput.SetmaxStops(AIndex : Integer; AValue : integer); 

begin
  If (FmaxStops=AValue) then exit;
  FmaxStops:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInput.Setorigin(AIndex : Integer; AValue : String); 

begin
  If (Forigin=AValue) then exit;
  Forigin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInput.SetpermittedCarrier(AIndex : Integer; AValue : TStringArray); 

begin
  If (FpermittedCarrier=AValue) then exit;
  FpermittedCarrier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInput.SetpermittedDepartureTime(AIndex : Integer; AValue : TTimeOfDayRange); 

begin
  If (FpermittedDepartureTime=AValue) then exit;
  FpermittedDepartureTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInput.SetpreferredCabin(AIndex : Integer; AValue : String); 

begin
  If (FpreferredCabin=AValue) then exit;
  FpreferredCabin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInput.SetprohibitedCarrier(AIndex : Integer; AValue : TStringArray); 

begin
  If (FprohibitedCarrier=AValue) then exit;
  FprohibitedCarrier:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSliceInput.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'permittedcarrier' : SetLength(FpermittedCarrier,ALength);
  'prohibitedcarrier' : SetLength(FprohibitedCarrier,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTaxData
  --------------------------------------------------------------------}


Procedure TTaxData.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaxData.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaxData.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTaxInfo
  --------------------------------------------------------------------}


Procedure TTaxInfo.SetchargeType(AIndex : Integer; AValue : String); 

begin
  If (FchargeType=AValue) then exit;
  FchargeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaxInfo.Setcode(AIndex : Integer; AValue : String); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaxInfo.Setcountry(AIndex : Integer; AValue : String); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaxInfo.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaxInfo.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaxInfo.SetsalePrice(AIndex : Integer; AValue : String); 

begin
  If (FsalePrice=AValue) then exit;
  FsalePrice:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTimeOfDayRange
  --------------------------------------------------------------------}


Procedure TTimeOfDayRange.SetearliestTime(AIndex : Integer; AValue : String); 

begin
  If (FearliestTime=AValue) then exit;
  FearliestTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeOfDayRange.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeOfDayRange.SetlatestTime(AIndex : Integer; AValue : String); 

begin
  If (FlatestTime=AValue) then exit;
  FlatestTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTripOption
  --------------------------------------------------------------------}


Procedure TTripOption.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOption.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOption.Setpricing(AIndex : Integer; AValue : TTripOptionTypepricingArray); 

begin
  If (Fpricing=AValue) then exit;
  Fpricing:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOption.SetsaleTotal(AIndex : Integer; AValue : String); 

begin
  If (FsaleTotal=AValue) then exit;
  FsaleTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOption.Setslice(AIndex : Integer; AValue : TTripOptionTypesliceArray); 

begin
  If (Fslice=AValue) then exit;
  Fslice:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTripOption.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'pricing' : SetLength(Fpricing,ALength);
  'slice' : SetLength(Fslice,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTripOptionsRequest
  --------------------------------------------------------------------}


Procedure TTripOptionsRequest.SetmaxPrice(AIndex : Integer; AValue : String); 

begin
  If (FmaxPrice=AValue) then exit;
  FmaxPrice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOptionsRequest.Setpassengers(AIndex : Integer; AValue : TPassengerCounts); 

begin
  If (Fpassengers=AValue) then exit;
  Fpassengers:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOptionsRequest.Setrefundable(AIndex : Integer; AValue : boolean); 

begin
  If (Frefundable=AValue) then exit;
  Frefundable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOptionsRequest.SetsaleCountry(AIndex : Integer; AValue : String); 

begin
  If (FsaleCountry=AValue) then exit;
  FsaleCountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOptionsRequest.Setslice(AIndex : Integer; AValue : TTripOptionsRequestTypesliceArray); 

begin
  If (Fslice=AValue) then exit;
  Fslice:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOptionsRequest.Setsolutions(AIndex : Integer; AValue : integer); 

begin
  If (Fsolutions=AValue) then exit;
  Fsolutions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTripOptionsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'slice' : SetLength(Fslice,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTripOptionsResponse
  --------------------------------------------------------------------}


Procedure TTripOptionsResponse.Setdata(AIndex : Integer; AValue : TData); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOptionsResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOptionsResponse.SetrequestId(AIndex : Integer; AValue : String); 

begin
  If (FrequestId=AValue) then exit;
  FrequestId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOptionsResponse.SettripOption(AIndex : Integer; AValue : TTripOptionsResponseTypetripOptionArray); 

begin
  If (FtripOption=AValue) then exit;
  FtripOption:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTripOptionsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'tripoption' : SetLength(FtripOption,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTripsSearchRequest
  --------------------------------------------------------------------}


Procedure TTripsSearchRequest.Setrequest(AIndex : Integer; AValue : TTripOptionsRequest); 

begin
  If (Frequest=AValue) then exit;
  Frequest:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTripsSearchResponse
  --------------------------------------------------------------------}


Procedure TTripsSearchResponse.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripsSearchResponse.Settrips(AIndex : Integer; AValue : TTripOptionsResponse); 

begin
  If (Ftrips=AValue) then exit;
  Ftrips:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTripsResource
  --------------------------------------------------------------------}


Class Function TTripsResource.ResourceName : String;

begin
  Result:='trips';
end;

Class Function TTripsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TqpxExpressAPI;
end;

Function TTripsResource.Search(aTripsSearchRequest : TTripsSearchRequest) : TTripsSearchResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'search';
  _Methodid   = 'qpxExpress.trips.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aTripsSearchRequest,TTripsSearchResponse) as TTripsSearchResponse;
end;



{ --------------------------------------------------------------------
  TQpxExpressAPI
  --------------------------------------------------------------------}

Class Function TQpxExpressAPI.APIName : String;

begin
  Result:='qpxExpress';
end;

Class Function TQpxExpressAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TQpxExpressAPI.APIRevision : String;

begin
  Result:='20140321';
end;

Class Function TQpxExpressAPI.APIID : String;

begin
  Result:='qpxExpress:v1';
end;

Class Function TQpxExpressAPI.APITitle : String;

begin
  Result:='QPX Express API';
end;

Class Function TQpxExpressAPI.APIDescription : String;

begin
  Result:='Lets you find the least expensive flights between an origin and a destination.';
end;

Class Function TQpxExpressAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TQpxExpressAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TQpxExpressAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TQpxExpressAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TQpxExpressAPI.APIdocumentationLink : String;

begin
  Result:='http://developers.google.com/qpx-express';
end;

Class Function TQpxExpressAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TQpxExpressAPI.APIbasePath : string;

begin
  Result:='/qpxExpress/v1/trips/';
end;

Class Function TQpxExpressAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/qpxExpress/v1/trips/';
end;

Class Function TQpxExpressAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TQpxExpressAPI.APIservicePath : string;

begin
  Result:='qpxExpress/v1/trips/';
end;

Class Function TQpxExpressAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TQpxExpressAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,0);
  
end;

Class Function TQpxExpressAPI.APINeedsAuth : Boolean;

begin
  Result:=False;
end;

Class Procedure TQpxExpressAPI.RegisterAPIResources;

begin
  TAircraftData.RegisterObject;
  TAirportData.RegisterObject;
  TBagDescriptor.RegisterObject;
  TCarrierData.RegisterObject;
  TCityData.RegisterObject;
  TData.RegisterObject;
  TFareInfo.RegisterObject;
  TFlightInfo.RegisterObject;
  TFreeBaggageAllowance.RegisterObject;
  TLegInfo.RegisterObject;
  TPassengerCounts.RegisterObject;
  TPricingInfo.RegisterObject;
  TSegmentInfo.RegisterObject;
  TSegmentPricing.RegisterObject;
  TSliceInfo.RegisterObject;
  TSliceInput.RegisterObject;
  TTaxData.RegisterObject;
  TTaxInfo.RegisterObject;
  TTimeOfDayRange.RegisterObject;
  TTripOption.RegisterObject;
  TTripOptionsRequest.RegisterObject;
  TTripOptionsResponse.RegisterObject;
  TTripsSearchRequest.RegisterObject;
  TTripsSearchResponse.RegisterObject;
end;


Function TQpxExpressAPI.GetTripsInstance : TTripsResource;

begin
  if (FTripsInstance=Nil) then
    FTripsInstance:=CreateTripsResource;
  Result:=FTripsInstance;
end;

Function TQpxExpressAPI.CreateTripsResource : TTripsResource;

begin
  Result:=CreateTripsResource(Self);
end;


Function TQpxExpressAPI.CreateTripsResource(AOwner : TComponent) : TTripsResource;

begin
  Result:=TTripsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TQpxExpressAPI.RegisterAPI;
end.
