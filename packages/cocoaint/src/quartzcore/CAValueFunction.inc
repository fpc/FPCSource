{ Parsed from Quartzcore.framework CAValueFunction.h }

{$ifdef TYPES}
{$ifndef CAVALUEFUNCTION_PAS_T}
{$define CAVALUEFUNCTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CAVALUEFUNCTION_PAS_R}
{$define CAVALUEFUNCTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CAVALUEFUNCTION_PAS_F}
{$define CAVALUEFUNCTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CAVALUEFUNCTION_PAS_S}
{$define CAVALUEFUNCTION_PAS_S}

{ External string constants }
var
  kCAValueFunctionRotateX: NSString; cvar; external;
  kCAValueFunctionRotateY: NSString; cvar; external;
  kCAValueFunctionRotateZ: NSString; cvar; external;
  kCAValueFunctionScale: NSString; cvar; external;
  kCAValueFunctionScaleX: NSString; cvar; external;
  kCAValueFunctionScaleY: NSString; cvar; external;
  kCAValueFunctionScaleZ: NSString; cvar; external;
  kCAValueFunctionTranslate: NSString; cvar; external;
  kCAValueFunctionTranslateX: NSString; cvar; external;
  kCAValueFunctionTranslateY: NSString; cvar; external;
  kCAValueFunctionTranslateZ: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  CAValueFunction = objcclass;
  CAValueFunctionPointer = ^CAValueFunction;
  CAValueFunctionPtr = CAValueFunctionPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CAVALUEFUNCTION_PAS_C}
{$define CAVALUEFUNCTION_PAS_C}

{ CAValueFunction }
  CAValueFunction = objcclass external (NSObject, NSCodingProtocol)
  private
    _string: NSString;
    _impl: Pointer;
    
  public
    class function functionWithName(name: NSString): id; message 'functionWithName:';
    function name: NSString; message 'name';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{$endif}
{$endif}
