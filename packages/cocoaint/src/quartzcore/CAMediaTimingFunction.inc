{ Parsed from Quartzcore.framework CAMediaTimingFunction.h }

{$ifdef TYPES}
{$ifndef CAMEDIATIMINGFUNCTION_PAS_T}
{$define CAMEDIATIMINGFUNCTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CAMEDIATIMINGFUNCTION_PAS_R}
{$define CAMEDIATIMINGFUNCTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CAMEDIATIMINGFUNCTION_PAS_F}
{$define CAMEDIATIMINGFUNCTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CAMEDIATIMINGFUNCTION_PAS_S}
{$define CAMEDIATIMINGFUNCTION_PAS_S}

{ External string constants }
var
  kCAMediaTimingFunctionLinear: NSString; cvar; external;
  kCAMediaTimingFunctionEaseIn: NSString; cvar; external;
  kCAMediaTimingFunctionEaseOut: NSString; cvar; external;
  kCAMediaTimingFunctionEaseInEaseOut: NSString; cvar; external;
  kCAMediaTimingFunctionDefault: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  CAMediaTimingFunction = objcclass;
  CAMediaTimingFunctionPointer = ^CAMediaTimingFunction;
  CAMediaTimingFunctionPtr = CAMediaTimingFunctionPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CAMEDIATIMINGFUNCTION_PAS_C}
{$define CAMEDIATIMINGFUNCTION_PAS_C}

{ CAMediaTimingFunction }
  CAMediaTimingFunction = objcclass external (NSObject, NSCodingProtocol)
  private
    __priv: Pointer;
    
  public
    class function functionWithName(name: NSString): id; message 'functionWithName:';
    class function functionWithControlPoints(c: single; c1: single; c2: single; c3: single): id; message 'functionWithControlPoints:c1x:c1y:c2x:';
    function initWithControlPoints(c: single; c1: single; c2: single; c3: single): id; message 'initWithControlPoints:c1x:c1y:c2x:';
    procedure getControlPointAtIndex_values(idx: size_t; ptr: single); message 'getControlPointAtIndex:values:';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{$endif}
{$endif}
