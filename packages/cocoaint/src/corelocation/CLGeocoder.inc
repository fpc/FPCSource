{ Parsed from CoreLocation.framework CLGeocoder.h }


{$ifdef TYPES}
type
  CLGeocoderPtr = ^CLGeocoder;
{$endif}

{$ifdef TYPES}
type
  CLGeocodeCompletionHandler = OpaqueCBlock;
{$endif}

{$ifdef CLASSES}

type
  CLGeocoder = objcclass external (NSObject)
  private
    _internal: CLGeocoderInternal;
  public
    function isGeocoding: ObjCBOOL; message 'isGeocoding';
    procedure reverseGeocodeLocation_completionHandler (location: CLLocation; completionHandler: CLGeocodeCompletionHandler); message 'reverseGeocodeLocation:completionHandler:';
    procedure geocodeAddressDictionary_completionHandler (addressDictionary: NSDictionary; completionHandler: CLGeocodeCompletionHandler); message 'geocodeAddressDictionary:completionHandler:';
    procedure geocodeAddressString_completionHandler (addressString: NSString; completionHandler: CLGeocodeCompletionHandler); message 'geocodeAddressString:completionHandler:';
    procedure geocodeAddressString_inRegion_completionHandler (addressString: NSString; region: CLRegion; completionHandler: CLGeocodeCompletionHandler); message 'geocodeAddressString:inRegion:completionHandler:';
    procedure cancelGeocode; message 'cancelGeocode';
  end;
{$endif}

