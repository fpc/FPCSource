{ Parsed from MapKit.framework MKTileOverlay.h }


{$ifdef TYPES}
type
  MKTileOverlayPtr = ^MKTileOverlay;
{$endif}

{$ifdef CLASSES}

type
  MKTileOverlay = objcclass external (NSObject, MKOverlayProtocol)
  public
    function initWithURLTemplate (URLTemplate: NSString): instancetype; message 'initWithURLTemplate:'; { NS_DESIGNATED_INITIALIZER }
    procedure setTileSize(newValue: CGSize); message 'setTileSize:';
    function tileSize: CGSize; message 'tileSize';
    procedure setGeometryFlipped(newValue: ObjCBOOL); message 'setGeometryFlipped:';
    function isGeometryFlipped: ObjCBOOL; message 'isGeometryFlipped';
    procedure setMinimumZ(newValue: NSInteger); message 'setMinimumZ:';
    function minimumZ: NSInteger; message 'minimumZ';
    procedure setMaximumZ(newValue: NSInteger); message 'setMaximumZ:';
    function maximumZ: NSInteger; message 'maximumZ';
    function URLTemplate: NSString; message 'URLTemplate';
    procedure setCanReplaceMapContent(newValue: ObjCBOOL); message 'setCanReplaceMapContent:';
    function canReplaceMapContent: ObjCBOOL; message 'canReplaceMapContent';

    { Adopted protocols }
    function boundingMapRect: MKMapRect; message 'boundingMapRect';
    function coordinate: CLLocationCoordinate2D; message 'coordinate';
    function intersectsMapRect (mapRect: MKMapRect): ObjCBOOL; message 'intersectsMapRect:';
    procedure setCoordinate (newCoordinate: CLLocationCoordinate2D); message 'setCoordinate:'; { available in 10_9, 4_0 }
    function subtitle: NSString; message 'subtitle';
    function title: NSString; message 'title';
  end;
{$endif}

{$ifdef TYPES}
type
  MKTileOverlayPath = record
    x: NSInteger;
    y: NSInteger;
    z: NSInteger;
    contentScaleFactor: CGFloat;
  end;
type
  MKTileOverlayPathPtr = ^MKTileOverlayPath;
{$endif}

{$ifdef CLASSES}

type
  CustomLoading = objccategory external (MKTileOverlay)
    function URLForTilePath (path: MKTileOverlayPath): NSURL; message 'URLForTilePath:';
    procedure loadTileAtPath_result (path: MKTileOverlayPath; result_: OpaqueCBlock); message 'loadTileAtPath:result:';
  end;
{$endif}

