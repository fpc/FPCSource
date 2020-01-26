{ Parsed from MapKit.framework MKDirectionsRequest.h }


{$ifdef TYPES}
type
  MKDirectionsRequestPtr = ^MKDirectionsRequest;
{$endif}

{$ifdef CLASSES}

type
  MKDirectionsRequest = objcclass external (NSObject)
  public
    function source: MKMapItem; message 'source'; { available in 10_9, 6_0 }
    procedure setSource (source_: MKMapItem); message 'setSource:'; { available in 10_9, 7_0 }
    function destination: MKMapItem; message 'destination'; { available in 10_9, 6_0 }
    procedure setDestination (destination_: MKMapItem); message 'setDestination:'; { available in 10_9, 7_0 }
  end;


type
  MKRequestOptions = objccategory external (MKDirectionsRequest)
    procedure setTransportType(newValue: MKDirectionsTransportType); message 'setTransportType:';
    function transportType: MKDirectionsTransportType; message 'transportType';
    procedure setRequestsAlternateRoutes(newValue: ObjCBOOL); message 'setRequestsAlternateRoutes:';
    function requestsAlternateRoutes: ObjCBOOL; message 'requestsAlternateRoutes';
    procedure setDepartureDate(newValue: NSDate); message 'setDepartureDate:';
    function departureDate: NSDate; message 'departureDate';
    procedure setArrivalDate(newValue: NSDate); message 'setArrivalDate:';
    function arrivalDate: NSDate; message 'arrivalDate';
  end;

type
  MKDirectionsURL = objccategory external (MKDirectionsRequest)
    function initWithContentsOfURL (url: NSURL): instancetype; message 'initWithContentsOfURL:'; { available in 10_9, 6_0 }
    class function isDirectionsRequestURL (url: NSURL): ObjCBOOL; message 'isDirectionsRequestURL:'; { available in 10_9, 6_0 }
  end;
{$endif}

