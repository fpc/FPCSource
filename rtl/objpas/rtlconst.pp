{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
 
unit rtlconst;

interface

ResourceString

{ ---------------------------------------------------------------------
    Various error messages.
  ---------------------------------------------------------------------}

  SAncestorNotFound             = 'Ancestor class for "%s" not found.';
  SAssignError                  = 'Cannot assign a %s to a %s.';
  SBitsIndexError               = 'Bits index out of range.';
  SBucketListLocked             = 'List is locked during an active ForEach.';
  SCantWriteResourceStreamError = 'Can''t write to a read-only resource stream.';
  SCharExpected                 = '"%s" expected';
  SCheckSynchronizeError        = 'CheckSynchronize called from non-main thread "$%x"';
  SClassNotFound                = 'Class "%s" not found';
  SCmplxCouldNotParseImaginary  = 'Failed to parse imaginary portion';
  SCmplxCouldNotParsePlus       = 'Failed to parse required "+" (or "-") symbol';
  SCmplxCouldNotParseReal       = 'Failed to parse real portion';
  SCmplxCouldNotParseSymbol     = 'Failed to parse required "%s" symbol';
  SCmplxErrorSuffix             = '%s [%s<?>%s]';
  SCmplxUnexpectedChars         = 'Unexpected characters';
  SCmplxUnexpectedEOS           = 'Unexpected end of string [%s]';
  SComponentNameTooLong         = 'Component name "%s" exceeds 64 character limit';
  SConvDuplicateFamily          = 'Conversion family "%s" already registered';
  SConvDuplicateType            = 'Conversion type (%s) already registered in %s';
  SConvFactorZero               = '"%s" has a factor of zero';
  SConvIllegalFamily            = 'Illegal family';
  SConvIllegalType              = 'Illegal type';
  SConvIncompatibleTypes2       = 'Incompatible conversion types (%s, %s)';
  SConvIncompatibleTypes3       = 'Incompatible conversion types (%s, %s, %s)';
  SConvIncompatibleTypes4       = 'Incompatible conversion types (%s - %s, %s - %s)';
  SConvStrParseError            = 'Could not parse %s';
  SConvUnknownDescription       = '[$%.8x]' ; // no longer used
  SConvUnknownDescriptionWithPrefix = '[%s%.8x]';
  SConvUnknownFamily            = 'Unknown conversion family: "%s"';
  SConvUnknownType              = 'Unknown conversion type: "%s"';
  SDelimiterQuoteCharError      = 'Delimiter and QuoteChar properties cannot have the same value';
  SDuplicateClass               = 'A class named "%s" already exists';
  SDuplicateItem                = 'Duplicates not allowed in this list ($0%x)';
  SDuplicateName                = 'Duplcate name: A component named "%s" already exists';
  SDuplicateString              = 'String list does not allow duplicates';
  SErrOutOfMemory               = 'Out of memory';
  SErrInvalidBitIndex           = 'Invalid bit index : %d';
  SErrindexTooLarge             = 'Bit index exceeds array limit: %d';
  SFCreateError                 = 'Unable to create file "%s"';
  SFCreateErrorEx               = 'Unable to create file "%s": %s';
  SFOpenError                   = 'Unable to open file "%s"';
  SFOpenErrorEx                 = 'Unable to open file "%s": %s';
  SFailedToCallConstructor      = 'TStrings descendant "%s" failed to call inherited constructor';
  SFixedColTooBig               = 'Fixed column count must be less than column count';
  SFixedRowTooBig               = 'Fixed row count must be less than row count';
  SGridTooLarge                 = 'Grid too large for this operation';
  SIdentifierExpected           = 'Identifier expected';
  SIndexOutOfRange              = 'Grid index out of range';
  SIniFileWriteError            = 'Unable to write to "%s"';
  SInvalidActionCreation        = 'Invalid action creation';
  SInvalidActionEnumeration     = 'Invalid action enumeration';
  SInvalidActionRegistration    = 'Invalid action registration';
  SInvalidActionUnregistration  = 'Invalid action unregistration';
  SInvalidBinary                = 'Invalid binary value';
  SInvalidDateDay               = '(%d, %d) is not a valid DateDay pair';
  SInvalidDateMonthWeek         = '(%d, %d, %d, %d) is not a valid DateMonthWeek quad';
  SInvalidDateWeek              = '(%d, %d, %d) is not a valid DateWeek triplet';
  SInvalidDayOfWeekInMonth      = '(%d, %d, %d, %d) is not a valid DayOfWeekInMonth quad';
  SInvalidFileName              = '"%s" is not a valid file name.';
  SInvalidImage                 = 'Invalid stream format';
  SInvalidJulianDate            = '%f Julian cannot be represented as a DateTime';
  SInvalidMask                  = '"%s" is not a valid mask at (%d)';
  SInvalidName                  = '"%s" is not a valid component name';
  SInvalidProperty              = 'Invalid property value';
  SInvalidPropertyElement       = 'Invalid property element: "%s"';
  SInvalidPropertyPath          = 'Invalid property path';
  SInvalidPropertyType          = 'Property type (%s) is not valid';
  SInvalidPropertyValue         = 'Invalid value for property';
  SInvalidRegType               = 'Invalid data type for "%s"';
  SInvalidString                = 'Invalid string constant';
  SInvalidStringGridOp          = 'Unable to insert rows in or delete rows from grid';
  SItemNotFound                 = 'Item not found ($0%x)';
  SLineTooLong                  = 'Line too long';
  SListCapacityError            = 'List capacity (%d) exceeded.';
  SListCountError               = 'List count (%d) out of bounds.';
  SListIndexError               = 'List index (%d) out of bounds';
  SMaskEditErr                  = 'Invalid mask input value.  Use escape key to abandon changes';
  SMaskErr                      = 'Invalid mask input value';
  SMemoryStreamError            = 'Out of memory while expanding memory stream';
  SMissingDateTimeField         = '?';
  SNoComSupport                 = '"%s" has not been registered as a COM class';
  SNotPrinting                  = 'Printer is not currently printing';
  SNumberExpected               = 'Number expected';
  SParseError                   = '%s on line %d';
  SPrinting                     = 'Printing in progress';
  SPropertyException            = 'Error reading %s%s%s: %s';
  SReadError                    = 'Stream read error';
  SReadOnlyProperty             = 'Property is read-only';
  SRegCreateFailed              = 'Failed to create key %s';
  SRegGetDataFailed             = 'Failed to get data for "%s"';
  SRegSetDataFailed             = 'Failed to set data for "%s"';
  SRegisterError                = 'Invalid component registration';
  SResNotFound                  = 'Resource "%s" not found';
  SSeekNotImplemented           = '%s.Seek not implemented';
  SSortedListError              = 'Operation not allowed on sorted list';
  SStreamSetSize                = 'Error setting stream size';
  SStringExpected               = 'String expected';
  SSymbolExpected               = '%s expected';
  SThreadCreateError            = 'Thread creation error: %s';
  SThreadError                  = 'Thread Error: %s (%d)';
  STooManyDeleted               = 'Too many rows or columns deleted';
  SUnknownGroup                 = '%s not in a class registration group';
  SUnknownProperty              = 'Unknown property: "%s"';
  SWriteError                   = 'Stream write error';
  hNoContext                    = 'No context-sensitive Help installed.';
  hNoSystem                     = 'No Help Manager installed.';
  hNoTableOfContents            = 'No Table of Contents found.';
  hNoTopics                     = 'No topic-based Help installed.';
  hNothingFound                 = 'No help found for "%s"';
  sAsyncSocketError             = 'Asynchronous socket error: %d';
  sCannotCreateSocket           = 'Unable to create new socket';
  sCannotListenOnOpen           = 'Listening on an open socket is not allowed';
  sCantChangeWhileActive        = 'Changing value on an active socket is not allowed';
  sNoAddress                    = 'No address specified';
  sSocketAlreadyOpen            = 'Socket is already open';
  sSocketIOError                = '%s error %d, %s';
  sSocketMustBeBlocking         = 'Socket must be in blocking mode';
  sSocketRead                   = 'Read';
  sSocketWrite                  = 'Write';
  sWindowsSocketError           = 'A Windows socket error occurred: %s (%d), on API "%s"';

{ ---------------------------------------------------------------------
    "Distance" family type and conversion types
  ---------------------------------------------------------------------}

  SAngstromsDescription         = 'Angstroms';
  SAstronomicalUnitsDescription = 'AstronomicalUnits';
  SCentimetersDescription       = 'Centimeters';
  SChainsDescription            = 'Chains';
  SCubitsDescription            = 'Cubits';
  SDecametersDescription        = 'Decameters';
  SDecimetersDescription        = 'Decimeters';
  SDistanceDescription          = 'Distance';
  SFathomsDescription           = 'Fathoms';
  SFeetDescription              = 'Feet';
  SFurlongsDescription          = 'Furlongs';
  SGigametersDescription        = 'Gigameters';
  SHandsDescription             = 'Hands';
  SHectometersDescription       = 'Hectometers';
  SInchesDescription            = 'Inches';
  SKilometersDescription        = 'Kilometers';
  SLightYearsDescription        = 'LightYears';
  SLinksDescription             = 'Links';
  SMegametersDescription        = 'Megameters';
  SMetersDescription            = 'Meters';
  SMicromicronsDescription      = 'Micromicrons';
  SMicronsDescription           = 'Microns';
  SMilesDescription             = 'Miles';
  SMillimetersDescription       = 'Millimeters';
  SMillimicronsDescription      = 'Millimicrons';
  SNauticalMilesDescription     = 'NauticalMiles';
  SPacesDescription             = 'Paces';
  SParsecsDescription           = 'Parsecs';
  SPicasDescription             = 'Picas';
  SPointsDescription            = 'Points';
  SRodsDescription              = 'Rods';
  SYardsDescription             = 'Yards';

{ ---------------------------------------------------------------------
    "Area" family type and conversion types
  ---------------------------------------------------------------------}

  SAcresDescription             = 'Acres';
  SAreaDescription              = 'Area';
  SAresDescription              = 'Ares';
  SCentaresDescription          = 'Centares';
  SHectaresDescription          = 'Hectares';
  SSquareCentimetersDescription = 'SquareCentimeters';
  SSquareDecametersDescription  = 'SquareDecameters';
  SSquareDecimetersDescription  = 'SquareDecimeters';
  SSquareFeetDescription        = 'SquareFeet';
  SSquareHectometersDescription = 'SquareHectometers';
  SSquareInchesDescription      = 'SquareInches';
  SSquareKilometersDescription  = 'SquareKilometers';
  SSquareMetersDescription      = 'SquareMeters';
  SSquareMilesDescription       = 'SquareMiles';
  SSquareMillimetersDescription = 'SquareMillimeters';
  SSquareRodsDescription        = 'SquareRods';
  SSquareYardsDescription       = 'SquareYards';

{ ---------------------------------------------------------------------
    "Volume" family type and conversion types
  ---------------------------------------------------------------------}

  SAcreFeetDescription          = 'AcreFeet';
  SAcreInchesDescription        = 'AcreInches';
  SCentiLitersDescription       = 'CentiLiters';
  SCordFeetDescription          = 'CordFeet';
  SCordsDescription             = 'Cords';
  SCubicCentimetersDescription  = 'CubicCentimeters';
  SCubicDecametersDescription   = 'CubicDecameters';
  SCubicDecimetersDescription   = 'CubicDecimeters';
  SCubicFeetDescription         = 'CubicFeet';
  SCubicHectometersDescription  = 'CubicHectometers';
  SCubicInchesDescription       = 'CubicInches';
  SCubicKilometersDescription   = 'CubicKilometers';
  SCubicMetersDescription       = 'CubicMeters';
  SCubicMilesDescription        = 'CubicMiles';
  SCubicMillimetersDescription  = 'CubicMillimeters';
  SCubicYardsDescription        = 'CubicYards';
  SDecaLitersDescription        = 'DecaLiters';
  SDecasteresDescription        = 'Decasteres';
  SDeciLitersDescription        = 'DeciLiters';
  SDecisteresDescription        = 'Decisteres';
  SHectoLitersDescription       = 'HectoLiters';
  SKiloLitersDescription        = 'KiloLiters';
  SLitersDescription            = 'Liters';
  SMilliLitersDescription       = 'MilliLiters';
  SSteresDescription            = 'Steres';
  SVolumeDescription            = 'Volume';

  // US Fluid Units
  SFluidCupsDescription         = 'FluidCups';
  SFluidGallonsDescription      = 'FluidGallons';
  SFluidGillsDescription        = 'FluidGills';
  SFluidOuncesDescription       = 'FluidOunces';
  SFluidPintsDescription        = 'FluidPints';
  SFluidQuartsDescription       = 'FluidQuarts';
  SFluidTablespoonsDescription  = 'FluidTablespoons';
  SFluidTeaspoonsDescription    = 'FluidTeaspoons';

  // US Dry Units
  SDryBucketsDescription        = 'DryBuckets';
  SDryBushelsDescription        = 'DryBushels';
  SDryGallonsDescription        = 'DryGallons';
  SDryPecksDescription          = 'DryPecks';
  SDryPintsDescription          = 'DryPints';
  SDryQuartsDescription         = 'DryQuarts';

  // UK Fluid/Dry Units
  SUKBucketsDescription         = 'UKBuckets';
  SUKBushelsDescription         = 'UKBushels';
  SUKGallonsDescription         = 'UKGallons';
  SUKGillsDescription           = 'UKGill';
  SUKOuncesDescription          = 'UKOunces';
  SUKPecksDescription           = 'UKPecks';
  SUKPintsDescription           = 'UKPints';
  SUKPottlesDescription         = 'UKPottle';
  SUKQuartsDescription          = 'UKQuarts';

{ ---------------------------------------------------------------------
    "Mass" family type and conversion types
  ---------------------------------------------------------------------}

  SCentigramsDescription        = 'Centigrams';
  SDecagramsDescription         = 'Decagrams';
  SDecigramsDescription         = 'Decigrams';
  SDramsDescription             = 'Drams';
  SGrainsDescription            = 'Grains';
  SGramsDescription             = 'Grams';
  SHectogramsDescription        = 'Hectograms';
  SKilogramsDescription         = 'Kilograms';
  SLongTonsDescription          = 'LongTons';
  SMassDescription              = 'Mass';
  SMetricTonsDescription        = 'MetricTons';
  SMicrogramsDescription        = 'Micrograms';
  SMilligramsDescription        = 'Milligrams';
  SNanogramsDescription         = 'Nanograms';
  SOuncesDescription            = 'Ounces';
  SPoundsDescription            = 'Pounds';
  SStonesDescription            = 'Stones';
  STonsDescription              = 'Tons';
  
{ ---------------------------------------------------------------------
    "Temperature" family type and conversion types
  ---------------------------------------------------------------------}

  SCelsiusDescription           = 'Celsius';
  SFahrenheitDescription        = 'Fahrenheit';
  SKelvinDescription            = 'Kelvin';
  SRankineDescription           = 'Rankine';
  SReaumurDescription           = 'Reaumur';
  STemperatureDescription       = 'Temperature';

{ ---------------------------------------------------------------------
    "Time" family type and conversion types
  ---------------------------------------------------------------------}

  SCenturiesDescription          = 'Centuries';
  SDateTimeDescription           = 'DateTime';
  SDaysDescription               = 'Days';
  SDecadesDescription            = 'Decades';
  SFortnightsDescription         = 'Fortnights';
  SHoursDescription              = 'Hours';
  SJulianDateDescription         = 'JulianDate';
  SMillenniaDescription          = 'Millennia';
  SMilliSecondsDescription       = 'MilliSeconds';
  SMinutesDescription            = 'Minutes';
  SModifiedJulianDateDescription = 'ModifiedJulianDate';
  SMonthsDescription             = 'Months';
  SSecondsDescription            = 'Seconds';
  STimeDescription               = 'Time';
  SWeeksDescription              = 'Weeks';
  SYearsDescription              = 'Years';

{ ---------------------------------------------------------------------
    Strings also found in SysConsts.pas
  ---------------------------------------------------------------------}

  SInvalidDate                   = '"%s" is not a valid date' ;
  SInvalidDateTime               = '"%s" is not a valid date and time' ;
  SInvalidInteger                = '"%s" is not a valid integer value' ;
  SInvalidTime                   = '"%s" is not a valid time' ;
  STimeEncodeError               = 'Invalid argument to time encode' ;

implementation

end.
{
  $Log$
  Revision 1.3  2004-01-10 19:35:17  michael
  + Moved all resource strings to rtlconst/sysconst

  Revision 1.2  2004/01/10 17:30:32  michael
  + Implemented all constants for compatibility

  Revision 1.1  2003/09/03 14:09:37  florian
    * arm fixes to the common rtl code
    * some generic math code fixed
    * ...
}
