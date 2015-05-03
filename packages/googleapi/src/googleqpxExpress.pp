unit googleqpxExpress;
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
  TAircraftData = class;
  TAircraftDataArray = Array of TAircraftData;
  TAirportData = class;
  TAirportDataArray = Array of TAirportData;
  TBagDescriptor = class;
  TBagDescriptorArray = Array of TBagDescriptor;
  TBagDescriptordescription = class;
  TBagDescriptordescriptionArray = Array of TBagDescriptordescription;
  TCarrierData = class;
  TCarrierDataArray = Array of TCarrierData;
  TCityData = class;
  TCityDataArray = Array of TCityData;
  TData = class;
  TDataArray = Array of TData;
  TDataaircraft = class;
  TDataaircraftArray = Array of TDataaircraft;
  TDataairport = class;
  TDataairportArray = Array of TDataairport;
  TDatacarrier = class;
  TDatacarrierArray = Array of TDatacarrier;
  TDatacity = class;
  TDatacityArray = Array of TDatacity;
  TDatatax = class;
  TDatataxArray = Array of TDatatax;
  TFareInfo = class;
  TFareInfoArray = Array of TFareInfo;
  TFlightInfo = class;
  TFlightInfoArray = Array of TFlightInfo;
  TFreeBaggageAllowance = class;
  TFreeBaggageAllowanceArray = Array of TFreeBaggageAllowance;
  TFreeBaggageAllowancebagDescriptor = class;
  TFreeBaggageAllowancebagDescriptorArray = Array of TFreeBaggageAllowancebagDescriptor;
  TLegInfo = class;
  TLegInfoArray = Array of TLegInfo;
  TPassengerCounts = class;
  TPassengerCountsArray = Array of TPassengerCounts;
  TPricingInfo = class;
  TPricingInfoArray = Array of TPricingInfo;
  TPricingInfofare = class;
  TPricingInfofareArray = Array of TPricingInfofare;
  TPricingInfosegmentPricing = class;
  TPricingInfosegmentPricingArray = Array of TPricingInfosegmentPricing;
  TPricingInfotax = class;
  TPricingInfotaxArray = Array of TPricingInfotax;
  TSegmentInfo = class;
  TSegmentInfoArray = Array of TSegmentInfo;
  TSegmentInfoleg = class;
  TSegmentInfolegArray = Array of TSegmentInfoleg;
  TSegmentPricing = class;
  TSegmentPricingArray = Array of TSegmentPricing;
  TSegmentPricingfreeBaggageOption = class;
  TSegmentPricingfreeBaggageOptionArray = Array of TSegmentPricingfreeBaggageOption;
  TSliceInfo = class;
  TSliceInfoArray = Array of TSliceInfo;
  TSliceInfosegment = class;
  TSliceInfosegmentArray = Array of TSliceInfosegment;
  TSliceInput = class;
  TSliceInputArray = Array of TSliceInput;
  TSliceInputpermittedCarrier = class;
  TSliceInputpermittedCarrierArray = Array of TSliceInputpermittedCarrier;
  TSliceInputprohibitedCarrier = class;
  TSliceInputprohibitedCarrierArray = Array of TSliceInputprohibitedCarrier;
  TTaxData = class;
  TTaxDataArray = Array of TTaxData;
  TTaxInfo = class;
  TTaxInfoArray = Array of TTaxInfo;
  TTimeOfDayRange = class;
  TTimeOfDayRangeArray = Array of TTimeOfDayRange;
  TTripOption = class;
  TTripOptionArray = Array of TTripOption;
  TTripOptionpricing = class;
  TTripOptionpricingArray = Array of TTripOptionpricing;
  TTripOptionslice = class;
  TTripOptionsliceArray = Array of TTripOptionslice;
  TTripOptionsRequest = class;
  TTripOptionsRequestArray = Array of TTripOptionsRequest;
  TTripOptionsRequestslice = class;
  TTripOptionsRequestsliceArray = Array of TTripOptionsRequestslice;
  TTripOptionsResponse = class;
  TTripOptionsResponseArray = Array of TTripOptionsResponse;
  TTripOptionsResponsetripOption = class;
  TTripOptionsResponsetripOptionArray = Array of TTripOptionsResponsetripOption;
  TTripsSearchRequest = class;
  TTripsSearchRequestArray = Array of TTripsSearchRequest;
  TTripsSearchResponse = class;
  TTripsSearchResponseArray = Array of TTripsSearchResponse;
  
  { --------------------------------------------------------------------
    TAircraftData
    --------------------------------------------------------------------}
  
  TAircraftData = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fkind : string;
    Fname : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property name : string Index 16 Read Fname Write Setname;
  end;
  TAircraftDataClass = Class of TAircraftData;
  
  { --------------------------------------------------------------------
    TAirportData
    --------------------------------------------------------------------}
  
  TAirportData = Class(TGoogleBaseObject)
  Private
    Fcity : string;
    Fcode : string;
    Fkind : string;
    Fname : string;
  Protected
    //Property setters
    Procedure Setcity(AIndex : Integer; AValue : string); virtual;
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property city : string Index 0 Read Fcity Write Setcity;
    Property code : string Index 8 Read Fcode Write Setcode;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property name : string Index 24 Read Fname Write Setname;
  end;
  TAirportDataClass = Class of TAirportData;
  
  { --------------------------------------------------------------------
    TBagDescriptor
    --------------------------------------------------------------------}
  
  TBagDescriptor = Class(TGoogleBaseObject)
  Private
    FcommercialName : string;
    Fcount : integer;
    Fdescription : TBagDescriptordescription;
    Fkind : string;
    Fsubcode : string;
  Protected
    //Property setters
    Procedure SetcommercialName(AIndex : Integer; AValue : string); virtual;
    Procedure Setcount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : TBagDescriptordescription); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setsubcode(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property commercialName : string Index 0 Read FcommercialName Write SetcommercialName;
    Property count : integer Index 8 Read Fcount Write Setcount;
    Property description : TBagDescriptordescription Index 16 Read Fdescription Write Setdescription;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property subcode : string Index 32 Read Fsubcode Write Setsubcode;
  end;
  TBagDescriptorClass = Class of TBagDescriptor;
  
  { --------------------------------------------------------------------
    TBagDescriptordescription
    --------------------------------------------------------------------}
  
  TBagDescriptordescription = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBagDescriptordescriptionClass = Class of TBagDescriptordescription;
  
  { --------------------------------------------------------------------
    TCarrierData
    --------------------------------------------------------------------}
  
  TCarrierData = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fkind : string;
    Fname : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property name : string Index 16 Read Fname Write Setname;
  end;
  TCarrierDataClass = Class of TCarrierData;
  
  { --------------------------------------------------------------------
    TCityData
    --------------------------------------------------------------------}
  
  TCityData = Class(TGoogleBaseObject)
  Private
    Fcode : string;
    Fcountry : string;
    Fkind : string;
    Fname : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setcountry(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : string Index 0 Read Fcode Write Setcode;
    Property country : string Index 8 Read Fcountry Write Setcountry;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property name : string Index 24 Read Fname Write Setname;
  end;
  TCityDataClass = Class of TCityData;
  
  { --------------------------------------------------------------------
    TData
    --------------------------------------------------------------------}
  
  TData = Class(TGoogleBaseObject)
  Private
    Faircraft : TDataaircraft;
    Fairport : TDataairport;
    Fcarrier : TDatacarrier;
    Fcity : TDatacity;
    Fkind : string;
    Ftax : TDatatax;
  Protected
    //Property setters
    Procedure Setaircraft(AIndex : Integer; AValue : TDataaircraft); virtual;
    Procedure Setairport(AIndex : Integer; AValue : TDataairport); virtual;
    Procedure Setcarrier(AIndex : Integer; AValue : TDatacarrier); virtual;
    Procedure Setcity(AIndex : Integer; AValue : TDatacity); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Settax(AIndex : Integer; AValue : TDatatax); virtual;
  Public
  Published
    Property aircraft : TDataaircraft Index 0 Read Faircraft Write Setaircraft;
    Property airport : TDataairport Index 8 Read Fairport Write Setairport;
    Property carrier : TDatacarrier Index 16 Read Fcarrier Write Setcarrier;
    Property city : TDatacity Index 24 Read Fcity Write Setcity;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property tax : TDatatax Index 40 Read Ftax Write Settax;
  end;
  TDataClass = Class of TData;
  
  { --------------------------------------------------------------------
    TDataaircraft
    --------------------------------------------------------------------}
  
  TDataaircraft = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDataaircraftClass = Class of TDataaircraft;
  
  { --------------------------------------------------------------------
    TDataairport
    --------------------------------------------------------------------}
  
  TDataairport = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDataairportClass = Class of TDataairport;
  
  { --------------------------------------------------------------------
    TDatacarrier
    --------------------------------------------------------------------}
  
  TDatacarrier = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatacarrierClass = Class of TDatacarrier;
  
  { --------------------------------------------------------------------
    TDatacity
    --------------------------------------------------------------------}
  
  TDatacity = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatacityClass = Class of TDatacity;
  
  { --------------------------------------------------------------------
    TDatatax
    --------------------------------------------------------------------}
  
  TDatatax = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TDatataxClass = Class of TDatatax;
  
  { --------------------------------------------------------------------
    TFareInfo
    --------------------------------------------------------------------}
  
  TFareInfo = Class(TGoogleBaseObject)
  Private
    FbasisCode : string;
    Fcarrier : string;
    Fdestination : string;
    Fid : string;
    Fkind : string;
    Forigin : string;
    F_private : boolean;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetbasisCode(AIndex : Integer; AValue : string); virtual;
    Procedure Setcarrier(AIndex : Integer; AValue : string); virtual;
    Procedure Setdestination(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setorigin(AIndex : Integer; AValue : string); virtual;
    Procedure Set_private(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property basisCode : string Index 0 Read FbasisCode Write SetbasisCode;
    Property carrier : string Index 8 Read Fcarrier Write Setcarrier;
    Property destination : string Index 16 Read Fdestination Write Setdestination;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property origin : string Index 40 Read Forigin Write Setorigin;
    Property _private : boolean Index 48 Read F_private Write Set_private;
  end;
  TFareInfoClass = Class of TFareInfo;
  
  { --------------------------------------------------------------------
    TFlightInfo
    --------------------------------------------------------------------}
  
  TFlightInfo = Class(TGoogleBaseObject)
  Private
    Fcarrier : string;
    Fnumber : string;
  Protected
    //Property setters
    Procedure Setcarrier(AIndex : Integer; AValue : string); virtual;
    Procedure Setnumber(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property carrier : string Index 0 Read Fcarrier Write Setcarrier;
    Property number : string Index 8 Read Fnumber Write Setnumber;
  end;
  TFlightInfoClass = Class of TFlightInfo;
  
  { --------------------------------------------------------------------
    TFreeBaggageAllowance
    --------------------------------------------------------------------}
  
  TFreeBaggageAllowance = Class(TGoogleBaseObject)
  Private
    FbagDescriptor : TFreeBaggageAllowancebagDescriptor;
    Fkilos : integer;
    FkilosPerPiece : integer;
    Fkind : string;
    Fpieces : integer;
    Fpounds : integer;
  Protected
    //Property setters
    Procedure SetbagDescriptor(AIndex : Integer; AValue : TFreeBaggageAllowancebagDescriptor); virtual;
    Procedure Setkilos(AIndex : Integer; AValue : integer); virtual;
    Procedure SetkilosPerPiece(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setpieces(AIndex : Integer; AValue : integer); virtual;
    Procedure Setpounds(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property bagDescriptor : TFreeBaggageAllowancebagDescriptor Index 0 Read FbagDescriptor Write SetbagDescriptor;
    Property kilos : integer Index 8 Read Fkilos Write Setkilos;
    Property kilosPerPiece : integer Index 16 Read FkilosPerPiece Write SetkilosPerPiece;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property pieces : integer Index 32 Read Fpieces Write Setpieces;
    Property pounds : integer Index 40 Read Fpounds Write Setpounds;
  end;
  TFreeBaggageAllowanceClass = Class of TFreeBaggageAllowance;
  
  { --------------------------------------------------------------------
    TFreeBaggageAllowancebagDescriptor
    --------------------------------------------------------------------}
  
  TFreeBaggageAllowancebagDescriptor = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TFreeBaggageAllowancebagDescriptorClass = Class of TFreeBaggageAllowancebagDescriptor;
  
  { --------------------------------------------------------------------
    TLegInfo
    --------------------------------------------------------------------}
  
  TLegInfo = Class(TGoogleBaseObject)
  Private
    Faircraft : string;
    FarrivalTime : string;
    FchangePlane : boolean;
    FconnectionDuration : integer;
    FdepartureTime : string;
    Fdestination : string;
    FdestinationTerminal : string;
    Fduration : integer;
    Fid : string;
    Fkind : string;
    Fmeal : string;
    Fmileage : integer;
    FonTimePerformance : integer;
    FoperatingDisclosure : string;
    Forigin : string;
    ForiginTerminal : string;
    Fsecure : boolean;
  Protected
    //Property setters
    Procedure Setaircraft(AIndex : Integer; AValue : string); virtual;
    Procedure SetarrivalTime(AIndex : Integer; AValue : string); virtual;
    Procedure SetchangePlane(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetconnectionDuration(AIndex : Integer; AValue : integer); virtual;
    Procedure SetdepartureTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setdestination(AIndex : Integer; AValue : string); virtual;
    Procedure SetdestinationTerminal(AIndex : Integer; AValue : string); virtual;
    Procedure Setduration(AIndex : Integer; AValue : integer); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setmeal(AIndex : Integer; AValue : string); virtual;
    Procedure Setmileage(AIndex : Integer; AValue : integer); virtual;
    Procedure SetonTimePerformance(AIndex : Integer; AValue : integer); virtual;
    Procedure SetoperatingDisclosure(AIndex : Integer; AValue : string); virtual;
    Procedure Setorigin(AIndex : Integer; AValue : string); virtual;
    Procedure SetoriginTerminal(AIndex : Integer; AValue : string); virtual;
    Procedure Setsecure(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property aircraft : string Index 0 Read Faircraft Write Setaircraft;
    Property arrivalTime : string Index 8 Read FarrivalTime Write SetarrivalTime;
    Property changePlane : boolean Index 16 Read FchangePlane Write SetchangePlane;
    Property connectionDuration : integer Index 24 Read FconnectionDuration Write SetconnectionDuration;
    Property departureTime : string Index 32 Read FdepartureTime Write SetdepartureTime;
    Property destination : string Index 40 Read Fdestination Write Setdestination;
    Property destinationTerminal : string Index 48 Read FdestinationTerminal Write SetdestinationTerminal;
    Property duration : integer Index 56 Read Fduration Write Setduration;
    Property id : string Index 64 Read Fid Write Setid;
    Property kind : string Index 72 Read Fkind Write Setkind;
    Property meal : string Index 80 Read Fmeal Write Setmeal;
    Property mileage : integer Index 88 Read Fmileage Write Setmileage;
    Property onTimePerformance : integer Index 96 Read FonTimePerformance Write SetonTimePerformance;
    Property operatingDisclosure : string Index 104 Read FoperatingDisclosure Write SetoperatingDisclosure;
    Property origin : string Index 112 Read Forigin Write Setorigin;
    Property originTerminal : string Index 120 Read ForiginTerminal Write SetoriginTerminal;
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
    Fkind : string;
    FseniorCount : integer;
  Protected
    //Property setters
    Procedure SetadultCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetchildCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetinfantInLapCount(AIndex : Integer; AValue : integer); virtual;
    Procedure SetinfantInSeatCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetseniorCount(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property adultCount : integer Index 0 Read FadultCount Write SetadultCount;
    Property childCount : integer Index 8 Read FchildCount Write SetchildCount;
    Property infantInLapCount : integer Index 16 Read FinfantInLapCount Write SetinfantInLapCount;
    Property infantInSeatCount : integer Index 24 Read FinfantInSeatCount Write SetinfantInSeatCount;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property seniorCount : integer Index 40 Read FseniorCount Write SetseniorCount;
  end;
  TPassengerCountsClass = Class of TPassengerCounts;
  
  { --------------------------------------------------------------------
    TPricingInfo
    --------------------------------------------------------------------}
  
  TPricingInfo = Class(TGoogleBaseObject)
  Private
    FbaseFareTotal : string;
    Ffare : TPricingInfofare;
    FfareCalculation : string;
    Fkind : string;
    FlatestTicketingTime : string;
    Fpassengers : TPassengerCounts;
    Fptc : string;
    Frefundable : boolean;
    FsaleFareTotal : string;
    FsaleTaxTotal : string;
    FsaleTotal : string;
    FsegmentPricing : TPricingInfosegmentPricing;
    Ftax : TPricingInfotax;
  Protected
    //Property setters
    Procedure SetbaseFareTotal(AIndex : Integer; AValue : string); virtual;
    Procedure Setfare(AIndex : Integer; AValue : TPricingInfofare); virtual;
    Procedure SetfareCalculation(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlatestTicketingTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setpassengers(AIndex : Integer; AValue : TPassengerCounts); virtual;
    Procedure Setptc(AIndex : Integer; AValue : string); virtual;
    Procedure Setrefundable(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsaleFareTotal(AIndex : Integer; AValue : string); virtual;
    Procedure SetsaleTaxTotal(AIndex : Integer; AValue : string); virtual;
    Procedure SetsaleTotal(AIndex : Integer; AValue : string); virtual;
    Procedure SetsegmentPricing(AIndex : Integer; AValue : TPricingInfosegmentPricing); virtual;
    Procedure Settax(AIndex : Integer; AValue : TPricingInfotax); virtual;
  Public
  Published
    Property baseFareTotal : string Index 0 Read FbaseFareTotal Write SetbaseFareTotal;
    Property fare : TPricingInfofare Index 8 Read Ffare Write Setfare;
    Property fareCalculation : string Index 16 Read FfareCalculation Write SetfareCalculation;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property latestTicketingTime : string Index 32 Read FlatestTicketingTime Write SetlatestTicketingTime;
    Property passengers : TPassengerCounts Index 40 Read Fpassengers Write Setpassengers;
    Property ptc : string Index 48 Read Fptc Write Setptc;
    Property refundable : boolean Index 56 Read Frefundable Write Setrefundable;
    Property saleFareTotal : string Index 64 Read FsaleFareTotal Write SetsaleFareTotal;
    Property saleTaxTotal : string Index 72 Read FsaleTaxTotal Write SetsaleTaxTotal;
    Property saleTotal : string Index 80 Read FsaleTotal Write SetsaleTotal;
    Property segmentPricing : TPricingInfosegmentPricing Index 88 Read FsegmentPricing Write SetsegmentPricing;
    Property tax : TPricingInfotax Index 96 Read Ftax Write Settax;
  end;
  TPricingInfoClass = Class of TPricingInfo;
  
  { --------------------------------------------------------------------
    TPricingInfofare
    --------------------------------------------------------------------}
  
  TPricingInfofare = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPricingInfofareClass = Class of TPricingInfofare;
  
  { --------------------------------------------------------------------
    TPricingInfosegmentPricing
    --------------------------------------------------------------------}
  
  TPricingInfosegmentPricing = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPricingInfosegmentPricingClass = Class of TPricingInfosegmentPricing;
  
  { --------------------------------------------------------------------
    TPricingInfotax
    --------------------------------------------------------------------}
  
  TPricingInfotax = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPricingInfotaxClass = Class of TPricingInfotax;
  
  { --------------------------------------------------------------------
    TSegmentInfo
    --------------------------------------------------------------------}
  
  TSegmentInfo = Class(TGoogleBaseObject)
  Private
    FbookingCode : string;
    FbookingCodeCount : integer;
    Fcabin : string;
    FconnectionDuration : integer;
    Fduration : integer;
    Fflight : TFlightInfo;
    Fid : string;
    Fkind : string;
    Fleg : TSegmentInfoleg;
    FmarriedSegmentGroup : string;
    FsubjectToGovernmentApproval : boolean;
  Protected
    //Property setters
    Procedure SetbookingCode(AIndex : Integer; AValue : string); virtual;
    Procedure SetbookingCodeCount(AIndex : Integer; AValue : integer); virtual;
    Procedure Setcabin(AIndex : Integer; AValue : string); virtual;
    Procedure SetconnectionDuration(AIndex : Integer; AValue : integer); virtual;
    Procedure Setduration(AIndex : Integer; AValue : integer); virtual;
    Procedure Setflight(AIndex : Integer; AValue : TFlightInfo); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setleg(AIndex : Integer; AValue : TSegmentInfoleg); virtual;
    Procedure SetmarriedSegmentGroup(AIndex : Integer; AValue : string); virtual;
    Procedure SetsubjectToGovernmentApproval(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property bookingCode : string Index 0 Read FbookingCode Write SetbookingCode;
    Property bookingCodeCount : integer Index 8 Read FbookingCodeCount Write SetbookingCodeCount;
    Property cabin : string Index 16 Read Fcabin Write Setcabin;
    Property connectionDuration : integer Index 24 Read FconnectionDuration Write SetconnectionDuration;
    Property duration : integer Index 32 Read Fduration Write Setduration;
    Property flight : TFlightInfo Index 40 Read Fflight Write Setflight;
    Property id : string Index 48 Read Fid Write Setid;
    Property kind : string Index 56 Read Fkind Write Setkind;
    Property leg : TSegmentInfoleg Index 64 Read Fleg Write Setleg;
    Property marriedSegmentGroup : string Index 72 Read FmarriedSegmentGroup Write SetmarriedSegmentGroup;
    Property subjectToGovernmentApproval : boolean Index 80 Read FsubjectToGovernmentApproval Write SetsubjectToGovernmentApproval;
  end;
  TSegmentInfoClass = Class of TSegmentInfo;
  
  { --------------------------------------------------------------------
    TSegmentInfoleg
    --------------------------------------------------------------------}
  
  TSegmentInfoleg = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSegmentInfolegClass = Class of TSegmentInfoleg;
  
  { --------------------------------------------------------------------
    TSegmentPricing
    --------------------------------------------------------------------}
  
  TSegmentPricing = Class(TGoogleBaseObject)
  Private
    FfareId : string;
    FfreeBaggageOption : TSegmentPricingfreeBaggageOption;
    Fkind : string;
    FsegmentId : string;
  Protected
    //Property setters
    Procedure SetfareId(AIndex : Integer; AValue : string); virtual;
    Procedure SetfreeBaggageOption(AIndex : Integer; AValue : TSegmentPricingfreeBaggageOption); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetsegmentId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property fareId : string Index 0 Read FfareId Write SetfareId;
    Property freeBaggageOption : TSegmentPricingfreeBaggageOption Index 8 Read FfreeBaggageOption Write SetfreeBaggageOption;
    Property kind : string Index 16 Read Fkind Write Setkind;
    Property segmentId : string Index 24 Read FsegmentId Write SetsegmentId;
  end;
  TSegmentPricingClass = Class of TSegmentPricing;
  
  { --------------------------------------------------------------------
    TSegmentPricingfreeBaggageOption
    --------------------------------------------------------------------}
  
  TSegmentPricingfreeBaggageOption = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSegmentPricingfreeBaggageOptionClass = Class of TSegmentPricingfreeBaggageOption;
  
  { --------------------------------------------------------------------
    TSliceInfo
    --------------------------------------------------------------------}
  
  TSliceInfo = Class(TGoogleBaseObject)
  Private
    Fduration : integer;
    Fkind : string;
    Fsegment : TSliceInfosegment;
  Protected
    //Property setters
    Procedure Setduration(AIndex : Integer; AValue : integer); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setsegment(AIndex : Integer; AValue : TSliceInfosegment); virtual;
  Public
  Published
    Property duration : integer Index 0 Read Fduration Write Setduration;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property segment : TSliceInfosegment Index 16 Read Fsegment Write Setsegment;
  end;
  TSliceInfoClass = Class of TSliceInfo;
  
  { --------------------------------------------------------------------
    TSliceInfosegment
    --------------------------------------------------------------------}
  
  TSliceInfosegment = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSliceInfosegmentClass = Class of TSliceInfosegment;
  
  { --------------------------------------------------------------------
    TSliceInput
    --------------------------------------------------------------------}
  
  TSliceInput = Class(TGoogleBaseObject)
  Private
    Falliance : string;
    Fdate : string;
    Fdestination : string;
    Fkind : string;
    FmaxConnectionDuration : integer;
    FmaxStops : integer;
    Forigin : string;
    FpermittedCarrier : TSliceInputpermittedCarrier;
    FpermittedDepartureTime : TTimeOfDayRange;
    FpreferredCabin : string;
    FprohibitedCarrier : TSliceInputprohibitedCarrier;
  Protected
    //Property setters
    Procedure Setalliance(AIndex : Integer; AValue : string); virtual;
    Procedure Setdate(AIndex : Integer; AValue : string); virtual;
    Procedure Setdestination(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaxConnectionDuration(AIndex : Integer; AValue : integer); virtual;
    Procedure SetmaxStops(AIndex : Integer; AValue : integer); virtual;
    Procedure Setorigin(AIndex : Integer; AValue : string); virtual;
    Procedure SetpermittedCarrier(AIndex : Integer; AValue : TSliceInputpermittedCarrier); virtual;
    Procedure SetpermittedDepartureTime(AIndex : Integer; AValue : TTimeOfDayRange); virtual;
    Procedure SetpreferredCabin(AIndex : Integer; AValue : string); virtual;
    Procedure SetprohibitedCarrier(AIndex : Integer; AValue : TSliceInputprohibitedCarrier); virtual;
  Public
  Published
    Property alliance : string Index 0 Read Falliance Write Setalliance;
    Property date : string Index 8 Read Fdate Write Setdate;
    Property destination : string Index 16 Read Fdestination Write Setdestination;
    Property kind : string Index 24 Read Fkind Write Setkind;
    Property maxConnectionDuration : integer Index 32 Read FmaxConnectionDuration Write SetmaxConnectionDuration;
    Property maxStops : integer Index 40 Read FmaxStops Write SetmaxStops;
    Property origin : string Index 48 Read Forigin Write Setorigin;
    Property permittedCarrier : TSliceInputpermittedCarrier Index 56 Read FpermittedCarrier Write SetpermittedCarrier;
    Property permittedDepartureTime : TTimeOfDayRange Index 64 Read FpermittedDepartureTime Write SetpermittedDepartureTime;
    Property preferredCabin : string Index 72 Read FpreferredCabin Write SetpreferredCabin;
    Property prohibitedCarrier : TSliceInputprohibitedCarrier Index 80 Read FprohibitedCarrier Write SetprohibitedCarrier;
  end;
  TSliceInputClass = Class of TSliceInput;
  
  { --------------------------------------------------------------------
    TSliceInputpermittedCarrier
    --------------------------------------------------------------------}
  
  TSliceInputpermittedCarrier = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSliceInputpermittedCarrierClass = Class of TSliceInputpermittedCarrier;
  
  { --------------------------------------------------------------------
    TSliceInputprohibitedCarrier
    --------------------------------------------------------------------}
  
  TSliceInputprohibitedCarrier = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSliceInputprohibitedCarrierClass = Class of TSliceInputprohibitedCarrier;
  
  { --------------------------------------------------------------------
    TTaxData
    --------------------------------------------------------------------}
  
  TTaxData = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
    Fname : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property name : string Index 16 Read Fname Write Setname;
  end;
  TTaxDataClass = Class of TTaxData;
  
  { --------------------------------------------------------------------
    TTaxInfo
    --------------------------------------------------------------------}
  
  TTaxInfo = Class(TGoogleBaseObject)
  Private
    FchargeType : string;
    Fcode : string;
    Fcountry : string;
    Fid : string;
    Fkind : string;
    FsalePrice : string;
  Protected
    //Property setters
    Procedure SetchargeType(AIndex : Integer; AValue : string); virtual;
    Procedure Setcode(AIndex : Integer; AValue : string); virtual;
    Procedure Setcountry(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetsalePrice(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property chargeType : string Index 0 Read FchargeType Write SetchargeType;
    Property code : string Index 8 Read Fcode Write Setcode;
    Property country : string Index 16 Read Fcountry Write Setcountry;
    Property id : string Index 24 Read Fid Write Setid;
    Property kind : string Index 32 Read Fkind Write Setkind;
    Property salePrice : string Index 40 Read FsalePrice Write SetsalePrice;
  end;
  TTaxInfoClass = Class of TTaxInfo;
  
  { --------------------------------------------------------------------
    TTimeOfDayRange
    --------------------------------------------------------------------}
  
  TTimeOfDayRange = Class(TGoogleBaseObject)
  Private
    FearliestTime : string;
    Fkind : string;
    FlatestTime : string;
  Protected
    //Property setters
    Procedure SetearliestTime(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetlatestTime(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property earliestTime : string Index 0 Read FearliestTime Write SetearliestTime;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property latestTime : string Index 16 Read FlatestTime Write SetlatestTime;
  end;
  TTimeOfDayRangeClass = Class of TTimeOfDayRange;
  
  { --------------------------------------------------------------------
    TTripOption
    --------------------------------------------------------------------}
  
  TTripOption = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
    Fpricing : TTripOptionpricing;
    FsaleTotal : string;
    Fslice : TTripOptionslice;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setpricing(AIndex : Integer; AValue : TTripOptionpricing); virtual;
    Procedure SetsaleTotal(AIndex : Integer; AValue : string); virtual;
    Procedure Setslice(AIndex : Integer; AValue : TTripOptionslice); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property pricing : TTripOptionpricing Index 16 Read Fpricing Write Setpricing;
    Property saleTotal : string Index 24 Read FsaleTotal Write SetsaleTotal;
    Property slice : TTripOptionslice Index 32 Read Fslice Write Setslice;
  end;
  TTripOptionClass = Class of TTripOption;
  
  { --------------------------------------------------------------------
    TTripOptionpricing
    --------------------------------------------------------------------}
  
  TTripOptionpricing = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTripOptionpricingClass = Class of TTripOptionpricing;
  
  { --------------------------------------------------------------------
    TTripOptionslice
    --------------------------------------------------------------------}
  
  TTripOptionslice = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTripOptionsliceClass = Class of TTripOptionslice;
  
  { --------------------------------------------------------------------
    TTripOptionsRequest
    --------------------------------------------------------------------}
  
  TTripOptionsRequest = Class(TGoogleBaseObject)
  Private
    FmaxPrice : string;
    Fpassengers : TPassengerCounts;
    Frefundable : boolean;
    FsaleCountry : string;
    Fslice : TTripOptionsRequestslice;
    Fsolutions : integer;
  Protected
    //Property setters
    Procedure SetmaxPrice(AIndex : Integer; AValue : string); virtual;
    Procedure Setpassengers(AIndex : Integer; AValue : TPassengerCounts); virtual;
    Procedure Setrefundable(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsaleCountry(AIndex : Integer; AValue : string); virtual;
    Procedure Setslice(AIndex : Integer; AValue : TTripOptionsRequestslice); virtual;
    Procedure Setsolutions(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property maxPrice : string Index 0 Read FmaxPrice Write SetmaxPrice;
    Property passengers : TPassengerCounts Index 8 Read Fpassengers Write Setpassengers;
    Property refundable : boolean Index 16 Read Frefundable Write Setrefundable;
    Property saleCountry : string Index 24 Read FsaleCountry Write SetsaleCountry;
    Property slice : TTripOptionsRequestslice Index 32 Read Fslice Write Setslice;
    Property solutions : integer Index 40 Read Fsolutions Write Setsolutions;
  end;
  TTripOptionsRequestClass = Class of TTripOptionsRequest;
  
  { --------------------------------------------------------------------
    TTripOptionsRequestslice
    --------------------------------------------------------------------}
  
  TTripOptionsRequestslice = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTripOptionsRequestsliceClass = Class of TTripOptionsRequestslice;
  
  { --------------------------------------------------------------------
    TTripOptionsResponse
    --------------------------------------------------------------------}
  
  TTripOptionsResponse = Class(TGoogleBaseObject)
  Private
    Fdata : TData;
    Fkind : string;
    FrequestId : string;
    FtripOption : TTripOptionsResponsetripOption;
  Protected
    //Property setters
    Procedure Setdata(AIndex : Integer; AValue : TData); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure SetrequestId(AIndex : Integer; AValue : string); virtual;
    Procedure SettripOption(AIndex : Integer; AValue : TTripOptionsResponsetripOption); virtual;
  Public
  Published
    Property data : TData Index 0 Read Fdata Write Setdata;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property requestId : string Index 16 Read FrequestId Write SetrequestId;
    Property tripOption : TTripOptionsResponsetripOption Index 24 Read FtripOption Write SettripOption;
  end;
  TTripOptionsResponseClass = Class of TTripOptionsResponse;
  
  { --------------------------------------------------------------------
    TTripOptionsResponsetripOption
    --------------------------------------------------------------------}
  
  TTripOptionsResponsetripOption = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTripOptionsResponsetripOptionClass = Class of TTripOptionsResponsetripOption;
  
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
    Fkind : string;
    Ftrips : TTripOptionsResponse;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Settrips(AIndex : Integer; AValue : TTripOptionsResponse); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
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


Procedure TAircraftData.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAircraftData.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAircraftData.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAirportData
  --------------------------------------------------------------------}


Procedure TAirportData.Setcity(AIndex : Integer; AValue : string); 

begin
  If (Fcity=AValue) then exit;
  Fcity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAirportData.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAirportData.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAirportData.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBagDescriptor
  --------------------------------------------------------------------}


Procedure TBagDescriptor.SetcommercialName(AIndex : Integer; AValue : string); 

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



Procedure TBagDescriptor.Setdescription(AIndex : Integer; AValue : TBagDescriptordescription); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBagDescriptor.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBagDescriptor.Setsubcode(AIndex : Integer; AValue : string); 

begin
  If (Fsubcode=AValue) then exit;
  Fsubcode:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBagDescriptordescription
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCarrierData
  --------------------------------------------------------------------}


Procedure TCarrierData.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCarrierData.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCarrierData.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCityData
  --------------------------------------------------------------------}


Procedure TCityData.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCityData.Setcountry(AIndex : Integer; AValue : string); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCityData.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCityData.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TData
  --------------------------------------------------------------------}


Procedure TData.Setaircraft(AIndex : Integer; AValue : TDataaircraft); 

begin
  If (Faircraft=AValue) then exit;
  Faircraft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TData.Setairport(AIndex : Integer; AValue : TDataairport); 

begin
  If (Fairport=AValue) then exit;
  Fairport:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TData.Setcarrier(AIndex : Integer; AValue : TDatacarrier); 

begin
  If (Fcarrier=AValue) then exit;
  Fcarrier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TData.Setcity(AIndex : Integer; AValue : TDatacity); 

begin
  If (Fcity=AValue) then exit;
  Fcity:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TData.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TData.Settax(AIndex : Integer; AValue : TDatatax); 

begin
  If (Ftax=AValue) then exit;
  Ftax:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDataaircraft
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDataairport
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDatacarrier
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDatacity
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TDatatax
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TFareInfo
  --------------------------------------------------------------------}


Procedure TFareInfo.SetbasisCode(AIndex : Integer; AValue : string); 

begin
  If (FbasisCode=AValue) then exit;
  FbasisCode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFareInfo.Setcarrier(AIndex : Integer; AValue : string); 

begin
  If (Fcarrier=AValue) then exit;
  Fcarrier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFareInfo.Setdestination(AIndex : Integer; AValue : string); 

begin
  If (Fdestination=AValue) then exit;
  Fdestination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFareInfo.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFareInfo.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFareInfo.Setorigin(AIndex : Integer; AValue : string); 

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


Procedure TFlightInfo.Setcarrier(AIndex : Integer; AValue : string); 

begin
  If (Fcarrier=AValue) then exit;
  Fcarrier:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFlightInfo.Setnumber(AIndex : Integer; AValue : string); 

begin
  If (Fnumber=AValue) then exit;
  Fnumber:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFreeBaggageAllowance
  --------------------------------------------------------------------}


Procedure TFreeBaggageAllowance.SetbagDescriptor(AIndex : Integer; AValue : TFreeBaggageAllowancebagDescriptor); 

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



Procedure TFreeBaggageAllowance.Setkind(AIndex : Integer; AValue : string); 

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





{ --------------------------------------------------------------------
  TFreeBaggageAllowancebagDescriptor
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLegInfo
  --------------------------------------------------------------------}


Procedure TLegInfo.Setaircraft(AIndex : Integer; AValue : string); 

begin
  If (Faircraft=AValue) then exit;
  Faircraft:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.SetarrivalTime(AIndex : Integer; AValue : string); 

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



Procedure TLegInfo.SetdepartureTime(AIndex : Integer; AValue : string); 

begin
  If (FdepartureTime=AValue) then exit;
  FdepartureTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.Setdestination(AIndex : Integer; AValue : string); 

begin
  If (Fdestination=AValue) then exit;
  Fdestination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.SetdestinationTerminal(AIndex : Integer; AValue : string); 

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



Procedure TLegInfo.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.Setmeal(AIndex : Integer; AValue : string); 

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



Procedure TLegInfo.SetoperatingDisclosure(AIndex : Integer; AValue : string); 

begin
  If (FoperatingDisclosure=AValue) then exit;
  FoperatingDisclosure:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.Setorigin(AIndex : Integer; AValue : string); 

begin
  If (Forigin=AValue) then exit;
  Forigin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLegInfo.SetoriginTerminal(AIndex : Integer; AValue : string); 

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



Procedure TPassengerCounts.Setkind(AIndex : Integer; AValue : string); 

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


Procedure TPricingInfo.SetbaseFareTotal(AIndex : Integer; AValue : string); 

begin
  If (FbaseFareTotal=AValue) then exit;
  FbaseFareTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.Setfare(AIndex : Integer; AValue : TPricingInfofare); 

begin
  If (Ffare=AValue) then exit;
  Ffare:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.SetfareCalculation(AIndex : Integer; AValue : string); 

begin
  If (FfareCalculation=AValue) then exit;
  FfareCalculation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.SetlatestTicketingTime(AIndex : Integer; AValue : string); 

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



Procedure TPricingInfo.Setptc(AIndex : Integer; AValue : string); 

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



Procedure TPricingInfo.SetsaleFareTotal(AIndex : Integer; AValue : string); 

begin
  If (FsaleFareTotal=AValue) then exit;
  FsaleFareTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.SetsaleTaxTotal(AIndex : Integer; AValue : string); 

begin
  If (FsaleTaxTotal=AValue) then exit;
  FsaleTaxTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.SetsaleTotal(AIndex : Integer; AValue : string); 

begin
  If (FsaleTotal=AValue) then exit;
  FsaleTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.SetsegmentPricing(AIndex : Integer; AValue : TPricingInfosegmentPricing); 

begin
  If (FsegmentPricing=AValue) then exit;
  FsegmentPricing:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPricingInfo.Settax(AIndex : Integer; AValue : TPricingInfotax); 

begin
  If (Ftax=AValue) then exit;
  Ftax:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPricingInfofare
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPricingInfosegmentPricing
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPricingInfotax
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSegmentInfo
  --------------------------------------------------------------------}


Procedure TSegmentInfo.SetbookingCode(AIndex : Integer; AValue : string); 

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



Procedure TSegmentInfo.Setcabin(AIndex : Integer; AValue : string); 

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



Procedure TSegmentInfo.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentInfo.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentInfo.Setleg(AIndex : Integer; AValue : TSegmentInfoleg); 

begin
  If (Fleg=AValue) then exit;
  Fleg:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentInfo.SetmarriedSegmentGroup(AIndex : Integer; AValue : string); 

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





{ --------------------------------------------------------------------
  TSegmentInfoleg
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSegmentPricing
  --------------------------------------------------------------------}


Procedure TSegmentPricing.SetfareId(AIndex : Integer; AValue : string); 

begin
  If (FfareId=AValue) then exit;
  FfareId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentPricing.SetfreeBaggageOption(AIndex : Integer; AValue : TSegmentPricingfreeBaggageOption); 

begin
  If (FfreeBaggageOption=AValue) then exit;
  FfreeBaggageOption:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentPricing.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSegmentPricing.SetsegmentId(AIndex : Integer; AValue : string); 

begin
  If (FsegmentId=AValue) then exit;
  FsegmentId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSegmentPricingfreeBaggageOption
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSliceInfo
  --------------------------------------------------------------------}


Procedure TSliceInfo.Setduration(AIndex : Integer; AValue : integer); 

begin
  If (Fduration=AValue) then exit;
  Fduration:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInfo.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInfo.Setsegment(AIndex : Integer; AValue : TSliceInfosegment); 

begin
  If (Fsegment=AValue) then exit;
  Fsegment:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSliceInfosegment
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSliceInput
  --------------------------------------------------------------------}


Procedure TSliceInput.Setalliance(AIndex : Integer; AValue : string); 

begin
  If (Falliance=AValue) then exit;
  Falliance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInput.Setdate(AIndex : Integer; AValue : string); 

begin
  If (Fdate=AValue) then exit;
  Fdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInput.Setdestination(AIndex : Integer; AValue : string); 

begin
  If (Fdestination=AValue) then exit;
  Fdestination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInput.Setkind(AIndex : Integer; AValue : string); 

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



Procedure TSliceInput.Setorigin(AIndex : Integer; AValue : string); 

begin
  If (Forigin=AValue) then exit;
  Forigin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInput.SetpermittedCarrier(AIndex : Integer; AValue : TSliceInputpermittedCarrier); 

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



Procedure TSliceInput.SetpreferredCabin(AIndex : Integer; AValue : string); 

begin
  If (FpreferredCabin=AValue) then exit;
  FpreferredCabin:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSliceInput.SetprohibitedCarrier(AIndex : Integer; AValue : TSliceInputprohibitedCarrier); 

begin
  If (FprohibitedCarrier=AValue) then exit;
  FprohibitedCarrier:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSliceInputpermittedCarrier
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSliceInputprohibitedCarrier
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTaxData
  --------------------------------------------------------------------}


Procedure TTaxData.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaxData.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaxData.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTaxInfo
  --------------------------------------------------------------------}


Procedure TTaxInfo.SetchargeType(AIndex : Integer; AValue : string); 

begin
  If (FchargeType=AValue) then exit;
  FchargeType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaxInfo.Setcode(AIndex : Integer; AValue : string); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaxInfo.Setcountry(AIndex : Integer; AValue : string); 

begin
  If (Fcountry=AValue) then exit;
  Fcountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaxInfo.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaxInfo.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTaxInfo.SetsalePrice(AIndex : Integer; AValue : string); 

begin
  If (FsalePrice=AValue) then exit;
  FsalePrice:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTimeOfDayRange
  --------------------------------------------------------------------}


Procedure TTimeOfDayRange.SetearliestTime(AIndex : Integer; AValue : string); 

begin
  If (FearliestTime=AValue) then exit;
  FearliestTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeOfDayRange.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTimeOfDayRange.SetlatestTime(AIndex : Integer; AValue : string); 

begin
  If (FlatestTime=AValue) then exit;
  FlatestTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTripOption
  --------------------------------------------------------------------}


Procedure TTripOption.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOption.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOption.Setpricing(AIndex : Integer; AValue : TTripOptionpricing); 

begin
  If (Fpricing=AValue) then exit;
  Fpricing:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOption.SetsaleTotal(AIndex : Integer; AValue : string); 

begin
  If (FsaleTotal=AValue) then exit;
  FsaleTotal:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOption.Setslice(AIndex : Integer; AValue : TTripOptionslice); 

begin
  If (Fslice=AValue) then exit;
  Fslice:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTripOptionpricing
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTripOptionslice
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTripOptionsRequest
  --------------------------------------------------------------------}


Procedure TTripOptionsRequest.SetmaxPrice(AIndex : Integer; AValue : string); 

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



Procedure TTripOptionsRequest.SetsaleCountry(AIndex : Integer; AValue : string); 

begin
  If (FsaleCountry=AValue) then exit;
  FsaleCountry:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOptionsRequest.Setslice(AIndex : Integer; AValue : TTripOptionsRequestslice); 

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





{ --------------------------------------------------------------------
  TTripOptionsRequestslice
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTripOptionsResponse
  --------------------------------------------------------------------}


Procedure TTripOptionsResponse.Setdata(AIndex : Integer; AValue : TData); 

begin
  If (Fdata=AValue) then exit;
  Fdata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOptionsResponse.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOptionsResponse.SetrequestId(AIndex : Integer; AValue : string); 

begin
  If (FrequestId=AValue) then exit;
  FrequestId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTripOptionsResponse.SettripOption(AIndex : Integer; AValue : TTripOptionsResponsetripOption); 

begin
  If (FtripOption=AValue) then exit;
  FtripOption:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTripOptionsResponsetripOption
  --------------------------------------------------------------------}




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


Procedure TTripsSearchResponse.Setkind(AIndex : Integer; AValue : string); 

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
  Result:='https://www.googleapis.com/';
end;

Class Function TQpxExpressAPI.APIbasePath : string;

begin
  Result:='/qpxExpress/v1/trips/';
end;

Class Function TQpxExpressAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/qpxExpress/v1/trips/';
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
  TBagDescriptordescription.RegisterObject;
  TCarrierData.RegisterObject;
  TCityData.RegisterObject;
  TData.RegisterObject;
  TDataaircraft.RegisterObject;
  TDataairport.RegisterObject;
  TDatacarrier.RegisterObject;
  TDatacity.RegisterObject;
  TDatatax.RegisterObject;
  TFareInfo.RegisterObject;
  TFlightInfo.RegisterObject;
  TFreeBaggageAllowance.RegisterObject;
  TFreeBaggageAllowancebagDescriptor.RegisterObject;
  TLegInfo.RegisterObject;
  TPassengerCounts.RegisterObject;
  TPricingInfo.RegisterObject;
  TPricingInfofare.RegisterObject;
  TPricingInfosegmentPricing.RegisterObject;
  TPricingInfotax.RegisterObject;
  TSegmentInfo.RegisterObject;
  TSegmentInfoleg.RegisterObject;
  TSegmentPricing.RegisterObject;
  TSegmentPricingfreeBaggageOption.RegisterObject;
  TSliceInfo.RegisterObject;
  TSliceInfosegment.RegisterObject;
  TSliceInput.RegisterObject;
  TSliceInputpermittedCarrier.RegisterObject;
  TSliceInputprohibitedCarrier.RegisterObject;
  TTaxData.RegisterObject;
  TTaxInfo.RegisterObject;
  TTimeOfDayRange.RegisterObject;
  TTripOption.RegisterObject;
  TTripOptionpricing.RegisterObject;
  TTripOptionslice.RegisterObject;
  TTripOptionsRequest.RegisterObject;
  TTripOptionsRequestslice.RegisterObject;
  TTripOptionsResponse.RegisterObject;
  TTripOptionsResponsetripOption.RegisterObject;
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
  Result.API:=Self;
end;



initialization
  TQpxExpressAPI.RegisterAPI;
end.
