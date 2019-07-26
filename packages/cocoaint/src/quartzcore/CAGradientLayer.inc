{ Parsed from QuartzCore.framework CAGradientLayer.h }


{$ifdef TYPES}
type
  CAGradientLayerPtr = ^CAGradientLayer;
{$endif}

{$ifdef CLASSES}

type
  CAGradientLayer = objcclass external (CALayer)
  public
    procedure setColors(newValue: NSArray); message 'setColors:';
    function colors: NSArray; message 'colors';
    procedure setLocations(newValue: NSArray); message 'setLocations:';
    function locations: NSArray; message 'locations';
    procedure setStartPoint(newValue: CGPoint); message 'setStartPoint:';
    function startPoint: CGPoint; message 'startPoint';
    procedure setEndPoint(newValue: CGPoint); message 'setEndPoint:';
    function endPoint: CGPoint; message 'endPoint';
    procedure setType(newValue: NSString); message 'setType:';
    function type_: NSString; message 'type';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCAGradientLayerAxial: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
{$endif}

