{ Parsed from QuartzCore.framework CAValueFunction.h }


{$ifdef TYPES}
type
  CAValueFunctionPtr = ^CAValueFunction;
{$endif}

{$ifdef CLASSES}

type
  CAValueFunction = objcclass external (NSObject, NSCodingProtocol)
  private
    _string: NSString;
    _impl: pointer;
  public
    class function functionWithName (name: NSString): instancetype; message 'functionWithName:';
    function name: NSString; message 'name';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCAValueFunctionRotateX: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCAValueFunctionRotateY: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCAValueFunctionRotateZ: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCAValueFunctionScale: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCAValueFunctionScaleX: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCAValueFunctionScaleY: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCAValueFunctionScaleZ: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCAValueFunctionTranslate: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCAValueFunctionTranslateX: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCAValueFunctionTranslateY: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
  kCAValueFunctionTranslateZ: NSString { available starting in __MAC_10_6, __IPHONE_3_0 }; cvar; external;
{$endif}

