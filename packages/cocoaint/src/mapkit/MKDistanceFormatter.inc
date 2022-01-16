{ Parsed from MapKit.framework MKDistanceFormatter.h }


{ Types from MKDistanceFormatter }
{$ifdef TYPES}

type
  MKDistanceFormatterUnits = NSUInteger;
type
  MKDistanceFormatterUnitsPtr = ^MKDistanceFormatterUnits;

const
  MKDistanceFormatterUnitsDefault = 0;
  MKDistanceFormatterUnitsMetric = 1;
  MKDistanceFormatterUnitsImperial = 2;
  MKDistanceFormatterUnitsImperialWithYards = 3;

type
  MKDistanceFormatterUnitStyle = NSUInteger;
type
  MKDistanceFormatterUnitStylePtr = ^MKDistanceFormatterUnitStyle;

const
  MKDistanceFormatterUnitStyleDefault = 0;
  MKDistanceFormatterUnitStyleAbbreviated = 1;
  MKDistanceFormatterUnitStyleFull = 2;
{$endif}


{$ifdef TYPES}
type
  MKDistanceFormatterPtr = ^MKDistanceFormatter;
{$endif}

{$ifdef CLASSES}

type
  MKDistanceFormatter = objcclass external (NSFormatter)
  public
    function stringFromDistance (distance: CLLocationDistance): NSString; message 'stringFromDistance:';
    function distanceFromString (distance: NSString): CLLocationDistance; message 'distanceFromString:';
    procedure setLocale(newValue: NSLocale); message 'setLocale:';
    function locale: NSLocale; message 'locale';
    procedure setUnits(newValue: MKDistanceFormatterUnits); message 'setUnits:';
    function units: MKDistanceFormatterUnits; message 'units';
    procedure setUnitStyle(newValue: MKDistanceFormatterUnitStyle); message 'setUnitStyle:';
    function unitStyle: MKDistanceFormatterUnitStyle; message 'unitStyle';
  end;
{$endif}

