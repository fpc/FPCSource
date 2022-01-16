{ Parsed from MapKit.framework MKLocalSearchResponse.h }


{$ifdef TYPES}
type
  MKLocalSearchResponsePtr = ^MKLocalSearchResponse;
{$endif}

{$ifdef CLASSES}

type
  MKLocalSearchResponse = objcclass external (NSObject)
  public
    function mapItems: NSArray; message 'mapItems';
    function boundingRegion: MKCoordinateRegion; message 'boundingRegion';
  end;
{$endif}

