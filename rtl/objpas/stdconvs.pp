{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2004 by Marco van de Voort
   member of the Free Pascal development team.

   An implementation for unit stdconv, 

   Based on list of function of delphibasics.co.uk and #7482.

   Quantities are mostly taken from my HP48g/gx or the unix units program

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

unit stdconvs;

interface

{$mode objfpc}
{$H+}

Uses convutils;


var
{cbArea family}

  auSquareMillimeters,
  auSquareCentimeters,
  auSquareDecimeters,
  auSquareMeters,
  auSquareDecameters,
  auSquareHectometers,
  auSquareKilometers,
  auSquareInches,
  auSquareFeet  ,
  auSquareYards ,
  auSquareMiles,
  auAcres       ,
  auCentares    ,
  auAres        ,
  auHectares    ,
  auSquareRods  ,

{cbDistance family}

  duMicromicrons,
  duAngstroms   ,
  duMillimicrons,
  duMicrons,
  duMillimeters,
  duCentimeters,
  duDecimeters,
  duMeters,
  duDecameters,
  duHectometers,
  duKilometers,
  duMegameters,
  duGigameters,
  duInches,
  duFeet,
  duYards,
  duMiles       ,
  duNauticalMiles,
  duAstronomicalUnits,
  duLightYears,
  duParsecs,
  duCubits,
  duFathoms,
  duFurlongs,
  duHands,
  duPaces,
  duRods,
  duChains,
  duLinks,
  duPicas,
  duPoints,

{cbMass family}

  muNanograms,
  muMicrograms,
  muMilligrams,
  muCentigrams,
  muDecigrams,
  muGrams,
  muDecagrams,
  muHectograms,
  muKilograms,
  muMetricTons,
  muDrams,
  muGrains,
  muLongTons,
  muTons,
  muOunces,
  muPounds,
  muStones,

{cbTemperature family}

  tuCelsius,
  tuKelvin,
  tuFahrenheit,
  tuRankine,
  tuReamur,

{
cbTime family
}

  tuMilliSeconds,
  tuSeconds,
  tuMinutes,
  tuHours,
  tuDays,
  tuWeeks,
  tuFortnights,
  tuMonths,
  tuYears,
  tuDecades,
  tuCenturies,
  tuMillennia,
  tuDateTime,
  tuJulianDate,
  tuModifiedJulianDate,

{
cbVolume family
}

  vuCubicMillimeters,
  vuCubicCentimeters,
  vuCubicDecimeters,
  vuCubicMeters,
  vuCubicDecameters,
  vuCubicHectometers,
  vuCubicKilometers,
  vuCubicInches,
  vuCubicFeet,
  vuCubicYards,
  vuCubicMiles,
  vuMilliLiters,
  vuCentiLiters,
  vuDeciLiters,
  vuLiters,
  vuDecaLiters,
  vuHectoLiters,
  vuKiloLiters,
  vuAcreFeet,
  vuAcreInches,
  vuCords,
  vuCordFeet,
  vuDecisteres,
  vuSteres,
  vuDecasteres,
  vuFluidGallons,
  vuFluidQuarts,
  vuFluidPints,
  vuFluidCups,
  vuFluidGills,
  vuFluidOunces,
  vuFluidTablespoons,
  vuFluidTeaspoons,
  vuDryGallons,
  vuDryQuarts,
  vuDryPints,
  vuDryPecks,
  vuDryBuckets,
  vuDryBushels,
  vuUKGallons,
  vuUKPottles,
  vuUKQuarts,
  vuUKPints,
  vuUKGills,
  vuUKOunces,
  vuUKPecks,
  vuUKBuckets,
  vuUKBushels     : TConvType;

var
    cbArea        : TConvFamily;
    cbDistance    : TConvFamily;
    cbMass        : TConvFamily;
    cbTemperature : TConvFamily;
    cbTime        : TConvFamily;
    cbVolume      : TConvFamily;


function CelsiusToFahrenheit(const AValue: Double): Double;
function FahrenheitToCelsius(const AValue: Double): Double;
function CelsiusToKelvin    (const AValue: Double): Double;
function KelvinToCelsius    (const AValue: Double): Double;

implementation

function FahrenheitToCelsius(const AValue: Double): Double;
begin
  result:= 32.0 + ((9.0 * AValue)/ 5.0);
end;

function CelsiusToFahrenheit(const AValue: Double): Double;
begin
  result:= (5.0/9.0)  * (Avalue - 32.0);
end;

function CelsiusToKelvin    (const AValue: Double): Double;
begin
  result:=AValue+273.15;
end;

function KelvinToCelsius    (const AValue: Double): Double;
begin
  result:=AValue-273.15;
end;

ResourceString  // Note, designations for FFU's are guesses.

  txtauSquareMillimeters   = 'Square millimeters (mm^2)';
  txtauSquareCentimeters   = 'Square centimeters (cm^2)';
  txtauSquareDecimeters    = 'Square decimeters (dm^2)';
  txtauSquareMeters        = 'Square meters (m^2)';
  txtauSquareDecameters    = 'Square decameters (dam^2)';
  txtauSquareHectometers   = 'Square hectometers (hm^2)';
  txtauSquareKilometers    = 'Square kilometers (km^2)';
  txtauSquareInches        = 'Square inch (in^2)';
  txtauSquareFeet          = 'Square feet (ft^2)';
  txtauSquareYards         = 'Square yards (yd^2)';
  txtauSquareMiles         = 'Square miles  (mi^2)';
  txtauAcres               = 'Square acres (acre^2)';
  txtauCentares            = 'Centares (care^2)';
  txtauAres                = 'Ares (are=dam^2)';
  txtauHectares            = 'Hectares (ha=hm^2)';
  txtauSquareRods          = 'Square Rods (sqr)';
  txtduMicromicrons        = 'micro microms (mumum)';
  txtduAngstroms           = 'Aengstroem (ang)';
  txtduMillimicrons        = 'millimicroms (mmum)';
  txtduMicrons             = 'microns (um)';
  txtduMillimeters         = 'millimeters (mm)';
  txtduCentimeters         = 'centimeters (cm)';
  txtduDecimeters          = 'decimeters (dm)';
  txtduMeters              = 'meters (m)';
  txtduDecameters          = 'decameters (dam)';
  txtduHectometers         = 'hectometers (hm)';
  txtduKilometers          = 'kilometers (km)';
  txtduMegameters          = 'megameters (Mm)';
  txtduGigameters          = 'gigameters (Gm)';
  txtduInches              = 'inches (in)';
  txtduFeet                = 'feet (ft)';
  txtduYards               = 'yards (yd)';
  txtduMiles               = 'miles (mi)';
  txtduNauticalMiles       = 'nautical miles (nmi)';
  txtduAstronomicalUnits   = 'astronomical units (au)';
  txtduLightYears          = 'light years (ly)';
  txtduParsecs             = 'Parsec (Ps)';
  txtduCubits              = 'Cubits (cb)';
  txtduFathoms             = 'Fathom (Fth)';
  txtduFurlongs            = 'Furlongs (furl)';
  txtduHands               = 'Hands (hnd)';
  txtduPaces               = 'Paces (pc)';
  txtduRods                = 'Rods (rd)';
  txtduChains              = 'Chains (ch)';
  txtduLinks               = 'Links (lnk)';
  txtduPicas               = 'Pica''s (pc)';
  txtduPoints              = 'Points (pnts)';           // carat/Karaat 2E-6 gram ?
  txtmuNanograms           = 'nanograms (ng)';
  txtmuMicrograms          = 'micrograms (um)';
  txtmuMilligrams          = 'milligrams (mg)';
  txtmuCentigrams          = 'centigrams (cg)';
  txtmuDecigrams           = 'decigrams (dg)';
  txtmuGrams               = 'grams (g)';
  txtmuDecagrams           = 'decagrams (dag)';
  txtmuHectograms          = 'hectograms (hg)';
  txtmuKilograms           = 'kilograms (kg)';
  txtmuMetricTons          = 'metric ton (t)';
  txtmuDrams               = 'dramgs (??)';
  txtmuGrains              = 'grains (??)';
  txtmuLongTons            = 'longton (??)';
  txtmuTons                = 'imperial ton (??)'; // calling metric ton "ton" is normal in metric countries
  txtmuOunces              = 'ounce (??)';
  txtmuPounds              = 'pounds (??)';     // what kind? Metric pound =0.5
  txtmuStones              = 'stones (??)';
  txttuCelsius             = 'degrees Celsius (degC)';
  txttuKelvin              = 'degrees Kelvin (K)';
  txttuFahrenheit          = 'degrees Fahrenheit (degF)';
  txttuRankine             = 'degrees Rankine (degR)';
  txttuReamur              = 'degrees Reamur (degReam)';
  txttuMilliSeconds        = 'milli seconds (ms)';
  txttuSeconds             = 'seconds (s)';
  txttuMinutes             = 'minutes (min)';
  txttuHours               = 'hours (hr)';
  txttuDays                = 'days (days)';
  txttuWeeks               = 'weeks (weeks)';
  txttuFortnights          = 'Fortnights (??)';
  txttuMonths              = 'Months (months)';
  txttuYears               = 'Years (years)';
  txttuDecades             = 'Decades (decades)';
  txttuCenturies           = 'Centuries (centuries)';
  txttuMillennia           = 'Millennia (millenia)';
  txttuDateTime            = 'DateTime (??)';
  txttuJulianDate          = 'JulianDate (??)';
  txttuModifiedJulianDate  = 'Modified JulianData (??)';

  txtvuCubicMillimeters    = 'cubic millimeters (mm^3)';
  txtvuCubicCentimeters    = 'cubic centimeters (cm^3)';
  txtvuCubicDecimeters     = 'cubic decimeters (dm^3)';
  txtvuCubicMeters         = 'cubic meters (m^3)';
  txtvuCubicDecameters     = 'cubic decameters (dam^3)';
  txtvuCubicHectometers    = 'cubic hectometers (hm^3)';
  txtvuCubicKilometers     = 'cubic kilometers (km^3)';
  txtvuCubicInches         = 'cubic inches (in^3)';
  txtvuCubicFeet           = 'cubic feet (ft^3)';
  txtvuCubicYards          = 'cubic yards (yd^3)';
  txtvuCubicMiles          = 'cubic miles (mi^3)';
  txtvuMilliLiters         = 'milliliters (ml)';
  txtvuCentiLiters         = 'centiliters (cl)';
  txtvuDeciLiters          = 'deciliters (dl)';
  txtvuLiters              = 'liters (l)';
  txtvuDecaLiters          = 'decaliters (dal)';
  txtvuHectoLiters         = 'hectoliters (hl)';
  txtvuKiloLiters          = 'kiloliters (kl)';
  txtvuAcreFeet            = 'acrefeet (acre ft)';
  txtvuAcreInches          = 'acreinches (acre in)';
  txtvuCords               = 'cords (??)';
  txtvuCordFeet            = 'cordfeet (??)';
  txtvuDecisteres          = 'decisteres (??)';
  txtvuSteres              = 'steres (??)';
  txtvuDecasteres          = 'decasteres (??)';
  txtvuFluidGallons        = 'US fluid gallons (fl gal)';
  txtvuFluidQuarts         = 'US fluid Quarts (fl Quart)';
  txtvuFluidPints          = 'US fluid Pints (fl pints)';
  txtvuFluidCups           = 'US fluid Cups (fl Cups)';
  txtvuFluidGills          = 'US fluid Gills (fl Quart)';
  txtvuFluidOunces         = 'US fluid Ounces (fl Ounces)';
  txtvuFluidTablespoons    = 'US fluid Tablespoons (fl Tablespoons)';
  txtvuFluidTeaspoons      = 'US fluid teaspoons (fl teaspoon)';
  txtvuDryGallons          = 'US dry gallons (dr gal)';
  txtvuDryQuarts           = 'US dry Quarts (dr Quart)';
  txtvuDryPints            = 'US dry Pints (dr pints)';
  txtvuDryPecks            = 'US dry pecks (dr pecks)';
  txtvuDryBuckets          = 'US dry buckets (dr buckets)';
  txtvuDryBushels          = 'US dry bushels (dr bushels)';
  txtvuUKGallons           = 'UK gallons (fl gal)';
  txtvuUKPottles           = 'UK Pottles (fl pttle)';
  txtvuUKQuarts            = 'UK Quarts (fl Quart)';
  txtvuUKPints             = 'UK Pints (fl pints)';
  txtvuUKGills             = 'UK Gills (fl Quart)';
  txtvuUKOunces            = 'UK Ounces (fl Ounces)';
  txtvuUKPecks             = 'UK pecks (dr pecks)';
  txtvuUKBuckets           = 'UK buckets (dr buckets)';
  txtvuUKBushels           = 'UK bushels (dr bushels)';


  // initial FFU factors from a HP48g calculator and BSD units program. However after
  //    a while, the bushels/forthnight got boring, so please check.
  // undefined/uncertain factors get -1, and convert() functions
  // should check that and bomb on it.

procedure RegisterArea;

begin
  auSquareMillimeters := RegisterConversionType(cbArea,txtauSquareMillimeters,1E-6);
  auSquareCentimeters := RegisterConversionType(cbArea,txtauSquareCentimeters,1E-4);
  auSquareDecimeters  := RegisterConversionType(cbArea,txtauSquareDecimeters,1E-2);
  auSquareMeters      := RegisterConversionType(cbArea,txtauSquareMeters,1);
  auSquareDecameters  := RegisterConversionType(cbArea,txtauSquareDecameters,1E2);
  auSquareHectometers := RegisterConversionType(cbArea,txtauSquareHectometers,1E4);
  auSquareKilometers  := RegisterConversionType(cbArea,txtauSquareKilometers,1E6);
  auSquareInches      := RegisterConversionType(cbArea,txtauSquareInches,0.00064516);
  auSquareFeet        := RegisterConversionType(cbArea,txtauSquareFeet,0.092903040);
  auSquareYards       := RegisterConversionType(cbArea,txtauSquareYards,0.83612736);
  auSquareMiles       := RegisterConversionType(cbArea,txtauSquareMiles,2589988.11034);
  auAcres             := RegisterConversionType(cbArea,txtauAcres,4046.87260987);
  auCentares          := RegisterConversionType(cbArea,txtauCentares,-1);
  auAres              := RegisterConversionType(cbArea,txtauAres,100);
  auHectares          := RegisterConversionType(cbArea,txtauHectares,10000);
  auSquareRods        := RegisterConversionType(cbArea,txtauSquareRods,25.2929538117);
end;

procedure RegisterLengths;

begin
  duMicromicrons      := RegisterConversionType(cbDistance,txtduMicromicrons,1E-12);
  duAngstroms         := RegisterConversionType(cbDistance,txtduAngstroms,1E-10);
  duMillimicrons      := RegisterConversionType(cbDistance,txtduMillimicrons,1E-9);
  duMicrons           := RegisterConversionType(cbDistance,txtduMicrons,1E-6);
  duMillimeters       := RegisterConversionType(cbDistance,txtduMillimeters,1E-3);
  duCentimeters       := RegisterConversionType(cbDistance,txtduCentimeters,1E-2);
  duDecimeters        := RegisterConversionType(cbDistance,txtduDecimeters,1E-1);
  duMeters            := RegisterConversionType(cbDistance,txtduMeters,1);
  duDecameters        := RegisterConversionType(cbDistance,txtduDecameters,10);
  duHectometers       := RegisterConversionType(cbDistance,txtduHectometers,100);
  duKilometers        := RegisterConversionType(cbDistance,txtduKilometers,1000);
  duMegameters        := RegisterConversionType(cbDistance,txtduMegameters,1E6);
  duGigameters        := RegisterConversionType(cbDistance,txtduGigameters,1E9);
  duInches            := RegisterConversionType(cbDistance,txtduInches,0.0254);
  duFeet              := RegisterConversionType(cbDistance,txtduFeet,0.3048);
  duYards             := RegisterConversionType(cbDistance,txtduYards,0.9144);
  duMiles             := RegisterConversionType(cbDistance,txtduMiles,1609.344);
  duNauticalMiles     := RegisterConversionType(cbDistance,txtduNauticalMiles,1852);
  duAstronomicalUnits := RegisterConversionType(cbDistance,txtduAstronomicalUnits,149597900000.0);
  duLightYears        := RegisterConversionType(cbDistance,txtduLightYears,9.46052840488E15);
  duParsecs           := RegisterConversionType(cbDistance,txtduParsecs, 3.08567818585E16);
  duCubits            := RegisterConversionType(cbDistance,txtduCubits,0.4572);
  duFathoms           := RegisterConversionType(cbDistance,txtduFathoms,1.8288);
  duFurlongs          := RegisterConversionType(cbDistance,txtduFurlongs,201.168);
  duHands             := RegisterConversionType(cbDistance,txtduHands,0.1016);
  duPaces             := RegisterConversionType(cbDistance,txtduPaces,0.9144);
  duRods              := RegisterConversionType(cbDistance,txtduRods,5.0292);
  duChains            := RegisterConversionType(cbDistance,txtduChains,20.1168);
  duLinks             := RegisterConversionType(cbDistance,txtduLinks,0.201168);
  duPicas             := RegisterConversionType(cbDistance,txtduPicas,0.0042333333);
  duPoints            := RegisterConversionType(cbDistance,txtduPoints,0.00035277778);
end;

procedure Registermass;  // weight? :)

begin
  muNanograms         := RegisterConversionType(cbMass,txtmuNanograms,1E-12);
  muMicrograms        := RegisterConversionType(cbMass,txtmuMicrograms,1E-9);
  muMilligrams        := RegisterConversionType(cbMass,txtmuMilligrams,1E-6);
  muCentigrams        := RegisterConversionType(cbMass,txtmuCentigrams,1E-5);
  muDecigrams         := RegisterConversionType(cbMass,txtmuDecigrams,1E-4);
  muGrams             := RegisterConversionType(cbMass,txtmuGrams,1E-3);
  muDecagrams         := RegisterConversionType(cbMass,txtmuDecagrams,1E-2);
  muHectograms        := RegisterConversionType(cbMass,txtmuHectograms,1E-1);
  muKilograms         := RegisterConversionType(cbMass,txtmuKilograms,1);
  muMetricTons        := RegisterConversionType(cbMass,txtmuMetricTons,1000);
  muDrams             := RegisterConversionType(cbMass,txtmuDrams,0.0017718452);
  muGrains            := RegisterConversionType(cbMass,txtmuGrains,6.479891E-5);
  muLongTons          := RegisterConversionType(cbMass,txtmuLongTons,1016.0469);
  muTons              := RegisterConversionType(cbMass,txtmuTons,907.18474);
  muOunces            := RegisterConversionType(cbMass,txtmuOunces,0.028349523);
  muPounds            := RegisterConversionType(cbMass,txtmuPounds,0.45359237);
  muStones            := RegisterConversionType(cbMass,txtmuStones,6.3502932);
end;

procedure RegisterTemperature;
begin
 tuCelsius    := RegisterConversionType(cbTemperature,txttuCelsius,1);
 tuKelvin     := RegisterConversionType(cbTemperature,txttuKelvin,1);
 tuFahrenheit := RegisterConversionType(cbTemperature,txttuFahrenheit,5/9);
 tuRankine    := RegisterConversionType(cbTemperature,txttuRankine,0.5555556);
 tuReamur     := RegisterConversionType(cbTemperature,txttuReamur,10/8);   // Reaumur?
end;

Const Yearsec=365.24219879*24*3600.0;   // year in seconds;

procedure RegisterTimes;

begin
  tuMilliSeconds           := RegisterConversionType(cbTime,txttuMilliSeconds,1E-3);
  tuSeconds                := RegisterConversionType(cbTime,txttuSeconds,1);
  tuMinutes                := RegisterConversionType(cbTime,txttuMinutes,60.0);
  tuHours                  := RegisterConversionType(cbTime,txttuHours,3600.0);
  tuDays                   := RegisterConversionType(cbTime,txttuDays,24*3600.0);
  tuWeeks                  := RegisterConversionType(cbTime,txttuWeeks,7*24*3600.0);
  tuFortnights             := RegisterConversionType(cbTime,txttuFortnights,14*24*3600.0);
  tuMonths                 := RegisterConversionType(cbTime,txttuMonths,1/12*YearSec);
  tuYears                  := RegisterConversionType(cbTime,txttuYears,YearSec);
  tuDecades                := RegisterConversionType(cbTime,txttuDecades,10*YearSec);
  tuCenturies              := RegisterConversionType(cbTime,txttuCenturies,100*yearsec);
  tuMillennia              := RegisterConversionType(cbTime,txttuMillennia,1000*yearsec);
  tuDateTime               := RegisterConversionType(cbTime,txttuDateTime,-1);
  tuJulianDate             := RegisterConversionType(cbTime,txttuJulianDate,-1);
  tuModifiedJulianDate     := RegisterConversionType(cbTime,txttuModifiedJulianDate,-1);
end;

const flgal=0.0037854118;


procedure RegisterVolumes;
begin
  vuCubicMillimeters      := RegisterConversionType(cbVolume,txtvuCubicMillimeters,1E-9);
  vuCubicCentimeters      := RegisterConversionType(cbVolume,txtvuCubicCentimeters,1E-6);
  vuCubicDecimeters       := RegisterConversionType(cbVolume,txtvuCubicDecimeters,1E-3);
  vuCubicMeters           := RegisterConversionType(cbVolume,txtvuCubicMeters,1);
  vuCubicDecameters       := RegisterConversionType(cbVolume,txtvuCubicDecameters,1E3);
  vuCubicHectometers      := RegisterConversionType(cbVolume,txtvuCubicHectometers,1E6);
  vuCubicKilometers       := RegisterConversionType(cbVolume,txtvuCubicKilometers,1E9);
  vuCubicInches           := RegisterConversionType(cbVolume,txtvuCubicInches,1.6387064E-5);
  vuCubicFeet             := RegisterConversionType(cbVolume,txtvuCubicFeet,0.028316847);
  vuCubicYards            := RegisterConversionType(cbVolume,txtvuCubicYards,0.76455486);
  vuCubicMiles            := RegisterConversionType(cbVolume,txtvuCubicMiles,4.1681818E9);
  vuMilliLiters           := RegisterConversionType(cbVolume,txtvuMilliLiters,1E-6);
  vuCentiLiters           := RegisterConversionType(cbVolume,txtvuCentiLiters,1E-5);
  vuDeciLiters            := RegisterConversionType(cbVolume,txtvuDeciLiters,1E-4);
  vuLiters                := RegisterConversionType(cbVolume,txtvuLiters,1E-3);
  vuDecaLiters            := RegisterConversionType(cbVolume,txtvuDecaLiters,1E-2);
  vuHectoLiters           := RegisterConversionType(cbVolume,txtvuHectoLiters,1E-1);
  vuKiloLiters            := RegisterConversionType(cbVolume,txtvuKiloLiters,1);
  vuAcreFeet              := RegisterConversionType(cbVolume,txtvuAcreFeet,          -1);
  vuAcreInches            := RegisterConversionType(cbVolume,txtvuAcreInches,        -1);
  vuCords                 := RegisterConversionType(cbVolume,txtvuCords,128*0.028316847);
  vuCordFeet              := RegisterConversionType(cbVolume,txtvuCordFeet,128*0.028316847);
  vuDecisteres            := RegisterConversionType(cbVolume,txtvuDecisteres,0.1);
  vuSteres                := RegisterConversionType(cbVolume,txtvuSteres,1);
  vuDecasteres            := RegisterConversionType(cbVolume,txtvuDecasteres,10);
  vuFluidGallons          := RegisterConversionType(cbVolume,txtvuFluidGallons,flgal);
  vuFluidQuarts           := RegisterConversionType(cbVolume,txtvuFluidQuarts,0.25*flgal);
  vuFluidPints            := RegisterConversionType(cbVolume,txtvuFluidPints,0.5*0.25*flgal);
  vuFluidCups             := RegisterConversionType(cbVolume,txtvuFluidCups, -1);
  vuFluidGills            := RegisterConversionType(cbVolume,txtvuFluidGills,-1);
  vuFluidOunces           := RegisterConversionType(cbVolume,txtvuFluidOunces,1/16*0.5*0.25*flgal);
  vuFluidTablespoons      := RegisterConversionType(cbVolume,txtvuFluidTablespoons,-1);
  vuFluidTeaspoons        := RegisterConversionType(cbVolume,txtvuFluidTeaspoons,-1);
  vuDryGallons            := RegisterConversionType(cbVolume,txtvuDryGallons,-1);
  vuDryQuarts             := RegisterConversionType(cbVolume,txtvuDryQuarts,-1);
  vuDryPints              := RegisterConversionType(cbVolume,txtvuDryPints,-1);
  vuDryPecks              := RegisterConversionType(cbVolume,txtvuDryPecks, 0.0088097675);
  vuDryBuckets            := RegisterConversionType(cbVolume,txtvuDryBuckets,-1);
  vuDryBushels            := RegisterConversionType(cbVolume,txtvuDryBushels,0.03523907);
  vuUKGallons             := RegisterConversionType(cbVolume,txtvuUKGallons,0.0045460993);
  vuUKPottles             := RegisterConversionType(cbVolume,txtvuUKPottles,-1);
  vuUKQuarts              := RegisterConversionType(cbVolume,txtvuUKQuarts,0.0011365248);
  vuUKPints               := RegisterConversionType(cbVolume,txtvuUKPints,-1);
  vuUKGills               := RegisterConversionType(cbVolume,txtvuUKGills,-1);
  vuUKOunces              := RegisterConversionType(cbVolume,txtvuUKOunces,2.8413121E-5);
  vuUKPecks               := RegisterConversionType(cbVolume,txtvuUKPecks,0.0090921986);
  vuUKBuckets             := RegisterConversionType(cbVolume,txtvuUKBuckets,-1);
  vuUKBushels             := RegisterConversionType(cbVolume,txtvuUKBushels,0.036368794);
end;

Procedure RegisterFamilies;
Begin
    cbArea        := RegisterConversionFamily('Area');
    cbDistance    := RegisterConversionFamily('Distance');
    cbMass        := RegisterConversionFamily('Mass');
    cbTemperature := RegisterConversionFamily('Temperature');
    cbTime        := RegisterConversionFamily('Time');
    cbVolume      := RegisterConversionFamily('Volume');
End;


Procedure RegisterAll;
begin
   RegisterFamilies;
   RegisterVolumes;
   RegisterTimes;
   RegisterTemperature;
   Registermass;
   RegisterLengths;
   RegisterArea;
end;

initialization
 registerall;
end.
