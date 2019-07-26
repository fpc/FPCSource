{ Parsed from MapKit.framework MKDirectionsResponse.h }


{$ifdef TYPES}
type
  MKDirectionsResponsePtr = ^MKDirectionsResponse;
  MKRoutePtr = ^MKRoute;
  MKRouteStepPtr = ^MKRouteStep;
  MKETAResponsePtr = ^MKETAResponse;
{$endif}

{$ifdef CLASSES}

type
  MKDirectionsResponse = objcclass external (NSObject)
  public
    function source: MKMapItem; message 'source';
    function destination: MKMapItem; message 'destination';
    function routes: NSArray; message 'routes';
  end;

type
  MKRoute = objcclass external (NSObject)
  public
    function name: NSString; message 'name';
    function advisoryNotices: NSArray; message 'advisoryNotices';
    function distance: CLLocationDistance; message 'distance';
    function expectedTravelTime: NSTimeInterval; message 'expectedTravelTime';
    function transportType: MKDirectionsTransportType; message 'transportType';
    function polyline: MKPolyline; message 'polyline';
    function steps: NSArray; message 'steps';
  end;

type
  MKRouteStep = objcclass external (NSObject)
  public
    function instructions: NSString; message 'instructions';
    function notice: NSString; message 'notice';
    function polyline: MKPolyline; message 'polyline';
    function distance: CLLocationDistance; message 'distance';
    function transportType: MKDirectionsTransportType; message 'transportType';
  end;

type
  MKETAResponse = objcclass external (NSObject)
  public
    function source: MKMapItem; message 'source';
    function destination: MKMapItem; message 'destination';
    function expectedTravelTime: NSTimeInterval; message 'expectedTravelTime';
  end;
{$endif}

