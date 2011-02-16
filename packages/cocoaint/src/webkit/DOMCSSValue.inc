{ Parsed from Webkit.framework DOMCSSValue.h }

{$ifdef TYPES}
{$ifndef DOMCSSVALUE_PAS_T}
{$define DOMCSSVALUE_PAS_T}

{ Constants }

const
  DOM_CSS_INHERIT = 0;
  DOM_CSS_PRIMITIVE_VALUE = 1;
  DOM_CSS_VALUE_LIST = 2;
  DOM_CSS_CUSTOM = 3;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCSSVALUE_PAS_R}
{$define DOMCSSVALUE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCSSVALUE_PAS_F}
{$define DOMCSSVALUE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCSSVALUE_PAS_S}
{$define DOMCSSVALUE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCSSValue = objcclass;
  DOMCSSValuePointer = ^DOMCSSValue;
  DOMCSSValuePtr = DOMCSSValuePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSVALUE_PAS_C}
{$define DOMCSSVALUE_PAS_C}

{ DOMCSSValue }
  DOMCSSValue = objcclass external (DOMObject)
    
  public
    procedure setCssText (newValue: NSString); message 'setCssText:';
    function cssText: NSString; message 'cssText';
    function cssValueType: cuint; message 'cssValueType';
  end;

{$endif}
{$endif}
