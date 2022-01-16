{ Parsed from CoreLocation.framework CLError.h }

{$ifdef TYPES}
type
  CLError = NSInteger;
  CLErrorPtr = ^CLError;

const
  kCLErrorLocationUnknown = 0;
  kCLErrorDenied = 1;
  kCLErrorNetwork = 2;
  kCLErrorHeadingFailure = 3;
  kCLErrorRegionMonitoringDenied = 4;
  kCLErrorRegionMonitoringFailure = 5;
  kCLErrorRegionMonitoringSetupDelayed = 6;
  kCLErrorRegionMonitoringResponseDelayed = 7;
  kCLErrorGeocodeFoundNoResult = 8;
  kCLErrorGeocodeFoundPartialResult = 9;
  kCLErrorGeocodeCanceled = 10;
  kCLErrorDeferredFailed = 11;
  kCLErrorDeferredNotUpdatingLocation = 12;
  kCLErrorDeferredAccuracyTooLow = 13;
  kCLErrorDeferredDistanceFiltered = 14;
  kCLErrorDeferredCanceled = 15;
  kCLErrorRangingUnavailable = 16;
  kCLErrorRangingFailure = 17;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCLErrorUserInfoAlternateRegionKey: NSString { available in 10_7, 5_0 }; cvar; external;
{$endif}

