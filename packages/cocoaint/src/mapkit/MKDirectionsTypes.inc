{ Parsed from MapKit.framework MKDirectionsTypes.h }

{$ifdef TYPES}
type
  MKDirectionsTransportType = NSUInteger;
  MKDirectionsTransportTypePtr = ^MKDirectionsTransportType;

const
  MKDirectionsTransportTypeAutomobile = 1 shl 0;
  MKDirectionsTransportTypeWalking = 1 shl 1;
  MKDirectionsTransportTypeAny = $0FFFFFFF;
{$endif}

