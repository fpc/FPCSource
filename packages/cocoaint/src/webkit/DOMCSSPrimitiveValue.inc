{ Parsed from Webkit.framework DOMCSSPrimitiveValue.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMCSSPRIMITIVEVALUE_PAS_T}
{$define DOMCSSPRIMITIVEVALUE_PAS_T}

{ Constants }

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

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCSSPRIMITIVEVALUE_PAS_R}
{$define DOMCSSPRIMITIVEVALUE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCSSPRIMITIVEVALUE_PAS_F}
{$define DOMCSSPRIMITIVEVALUE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCSSPRIMITIVEVALUE_PAS_S}
{$define DOMCSSPRIMITIVEVALUE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCSSPrimitiveValue = objcclass;
  DOMCSSPrimitiveValuePointer = ^DOMCSSPrimitiveValue;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSPRIMITIVEVALUE_PAS_C}
{$define DOMCSSPRIMITIVEVALUE_PAS_C}

{ DOMCSSPrimitiveValue }
  DOMCSSPrimitiveValue = objcclass(DOMCSSValue)
    
  public
    class function alloc: DOMCSSPrimitiveValue; message 'alloc';

    function primitiveType: cushort; message 'primitiveType';
    procedure setFloatValue_floatValue(unitType: cushort; floatValue: single); message 'setFloatValue:floatValue:';
    function getFloatValue(unitType: cushort): single; message 'getFloatValue:';
    procedure setStringValue_stringValue(stringType: cushort; stringValue: NSString); message 'setStringValue:stringValue:';
    function getStringValue: NSString; message 'getStringValue';
    function getCounterValue: DOMCounter; message 'getCounterValue';
    function getRectValue: DOMRect; message 'getRectValue';
    function getRGBColorValue: DOMRGBColor; message 'getRGBColorValue';

    { Category: DOMCSSPrimitiveValueDeprecated }
    procedure setFloatValue(unitType: cushort; floatValue: single); message 'setFloatValue:unitType:';
    procedure setStringValue(stringType: cushort; stringValue: NSString); message 'setStringValue:stringType:';
  end; external;

{$endif}
{$endif}
