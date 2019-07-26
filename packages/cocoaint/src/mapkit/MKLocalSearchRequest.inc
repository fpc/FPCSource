{ Parsed from MapKit.framework MKLocalSearchRequest.h }


{$ifdef TYPES}
type
  MKLocalSearchRequestPtr = ^MKLocalSearchRequest;
{$endif}

{$ifdef CLASSES}

type
  MKLocalSearchRequest = objcclass external (NSObject, NSCopyingProtocol)
  public
    procedure setNaturalLanguageQuery(newValue: NSString); message 'setNaturalLanguageQuery:';
    function naturalLanguageQuery: NSString; message 'naturalLanguageQuery';
    procedure setRegion(newValue: MKCoordinateRegion); message 'setRegion:';
    function region: MKCoordinateRegion; message 'region';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

