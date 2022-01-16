{ Parsed from WebKit.framework DOMCSSPrimitiveValue.h }


{$ifdef TYPES}
type
  DOMCSSPrimitiveValuePtr = ^DOMCSSPrimitiveValue;
{$endif}

{$ifdef TYPES}
const
  DOM_CSS_UNKNOWN = 0;
  DOM_CSS_NUMBER = 1;
  DOM_CSS_PERCENTAGE = 2;
  DOM_CSS_EMS = 3;
  DOM_CSS_EXS = 4;
  DOM_CSS_PX = 5;
  DOM_CSS_CM = 6;
  DOM_CSS_MM = 7;
  DOM_CSS_IN = 8;
  DOM_CSS_PT = 9;
  DOM_CSS_PC = 10;
  DOM_CSS_DEG = 11;
  DOM_CSS_RAD = 12;
  DOM_CSS_GRAD = 13;
  DOM_CSS_MS = 14;
  DOM_CSS_S = 15;
  DOM_CSS_HZ = 16;
  DOM_CSS_KHZ = 17;
  DOM_CSS_DIMENSION = 18;
  DOM_CSS_STRING = 19;
  DOM_CSS_URI = 20;
  DOM_CSS_IDENT = 21;
  DOM_CSS_ATTR = 22;
  DOM_CSS_COUNTER = 23;
  DOM_CSS_RECT = 24;
  DOM_CSS_RGBCOLOR = 25;
  DOM_CSS_VW = 26;
  DOM_CSS_VH = 27;
  DOM_CSS_VMIN = 28;
  DOM_CSS_VMAX = 29;
{$endif}

{$ifdef CLASSES}

type
  DOMCSSPrimitiveValue = objcclass external (DOMCSSValue)
  public
    function primitiveType: cushort; message 'primitiveType';
    procedure setFloatValue_floatValue (unitType: cushort; floatValue: single); message 'setFloatValue:floatValue:'; { available in 10_5 }
    function getFloatValue (unitType: cushort): single; message 'getFloatValue:';
    procedure setStringValue_stringValue (stringType: cushort; stringValue: NSString); message 'setStringValue:stringValue:'; { available in 10_5 }
    function getStringValue: NSString; message 'getStringValue';
    function getCounterValue: DOMCounter; message 'getCounterValue';
    function getRectValue: DOMRect; message 'getRectValue';
    function getRGBColorValue: DOMRGBColor; message 'getRGBColorValue';
  end;


type
  DOMCSSPrimitiveValueDeprecated = objccategory external (DOMCSSPrimitiveValue)
    procedure setFloatValue (unitType: cushort); message 'setFloatValue:'; deprecated 'in 10_4, 10_5';
    procedure setStringValue (stringType: cushort); message 'setStringValue:'; deprecated 'in 10_4, 10_5';
  end;
{$endif}

