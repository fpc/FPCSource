{ Parsed from QuartzCore.framework CAMediaTimingFunction.h }


{$ifdef TYPES}
type
  CAMediaTimingFunctionPtr = ^CAMediaTimingFunction;
{$endif}

{$ifdef CLASSES}

type
  CAMediaTimingFunction = objcclass external (NSObject, NSCodingProtocol)
  private
    _priv: CAMediaTimingFunctionPrivatePtr;
  public
    class function functionWithName (name: NSString): instancetype; message 'functionWithName:';
    class function functionWithControlPoints_c1y (c1x: single; c2x: single): instancetype; message 'functionWithControlPoints:c1y:';
    function initWithControlPoints_c1y (c1x: single; c2x: single): instancetype; message 'initWithControlPoints:c1y:';
    procedure getControlPointAtIndex_values (idx: size_t; ptr: psingle { 2 element array of single }); message 'getControlPointAtIndex:values:';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCAMediaTimingFunctionLinear: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAMediaTimingFunctionEaseIn: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAMediaTimingFunctionEaseOut: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAMediaTimingFunctionEaseInEaseOut: NSString { available starting in __MAC_10_5, __IPHONE_2_0 }; cvar; external;
  kCAMediaTimingFunctionDefault: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
{$endif}

