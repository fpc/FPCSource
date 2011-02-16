{ Parsed from Quartzcore.framework CAGradientLayer.h }

{$ifdef TYPES}
{$ifndef CAGRADIENTLAYER_PAS_T}
{$define CAGRADIENTLAYER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CAGRADIENTLAYER_PAS_R}
{$define CAGRADIENTLAYER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CAGRADIENTLAYER_PAS_F}
{$define CAGRADIENTLAYER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CAGRADIENTLAYER_PAS_S}
{$define CAGRADIENTLAYER_PAS_S}

{ External string constants }
var
  kCAGradientLayerAxial: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  CAGradientLayer = objcclass;
  CAGradientLayerPointer = ^CAGradientLayer;
  CAGradientLayerPtr = CAGradientLayerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CAGRADIENTLAYER_PAS_C}
{$define CAGRADIENTLAYER_PAS_C}

{ CAGradientLayer }
  CAGradientLayer = objcclass external (CALayer)
    
  public
    procedure setColors (newValue: NSArray); message 'setColors:';
    function colors: NSArray; message 'colors';
    procedure setLocations (newValue: NSArray); message 'setLocations:';
    function locations: NSArray; message 'locations';
    procedure setStartPoint (newValue: CGPoint); message 'setStartPoint:';
    function startPoint: CGPoint; message 'startPoint';
    procedure setEndPoint (newValue: CGPoint); message 'setEndPoint:';
    function endPoint: CGPoint; message 'endPoint';
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
  end;

{$endif}
{$endif}
