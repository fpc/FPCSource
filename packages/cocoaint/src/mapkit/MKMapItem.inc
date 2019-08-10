{ Parsed from MapKit.framework MKMapItem.h }


{$ifdef TYPES}
type
  MKMapItemPtr = ^MKMapItem;
{$endif}

{$ifdef CLASSES}

type
  MKMapItem = objcclass external (NSObject)
  public
    function placemark: MKPlacemark; message 'placemark';
    function isCurrentLocation: ObjCBOOL; message 'isCurrentLocation';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setPhoneNumber(newValue: NSString); message 'setPhoneNumber:';
    function phoneNumber: NSString; message 'phoneNumber';
    procedure setUrl(newValue: NSURL); message 'setUrl:';
    function url: NSURL; message 'url';
    class function mapItemForCurrentLocation: MKMapItem; message 'mapItemForCurrentLocation';
    function initWithPlacemark (placemark_: MKPlacemark): instancetype; message 'initWithPlacemark:';
    function openInMapsWithLaunchOptions (launchOptions: NSDictionary): ObjCBOOL; message 'openInMapsWithLaunchOptions:';
    class function openMapsWithItems_launchOptions (mapItems: NSArray; launchOptions: NSDictionary): ObjCBOOL; message 'openMapsWithItems:launchOptions:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  MKLaunchOptionsDirectionsModeKey: NSString { available in 10_9, 6_0 }; cvar; external;
  MKLaunchOptionsMapTypeKey: NSString { available in 10_9, 6_0 }; cvar; external;
  MKLaunchOptionsShowsTrafficKey: NSString { available in 10_9, 6_0 }; cvar; external;
  MKLaunchOptionsDirectionsModeDriving: NSString { available in 10_9, 6_0 }; cvar; external;
  MKLaunchOptionsDirectionsModeWalking: NSString { available in 10_9, 6_0 }; cvar; external;
  MKLaunchOptionsMapCenterKey: NSString { available in 10_9, 6_0 }; cvar; external;
  MKLaunchOptionsMapSpanKey: NSString { available in 10_9, 6_0 }; cvar; external;
  MKLaunchOptionsCameraKey: NSString { available in 10_10, 7_1 }; cvar; external;
{$endif}

