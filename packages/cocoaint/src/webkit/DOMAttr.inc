{ Parsed from Webkit.framework DOMAttr.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMATTR_PAS_T}
{$define DOMATTR_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMATTR_PAS_R}
{$define DOMATTR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMATTR_PAS_F}
{$define DOMATTR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMATTR_PAS_S}
{$define DOMATTR_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMAttr = objcclass;
  DOMAttrPointer = ^DOMAttr;
  DOMAttrPtr = DOMAttrPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMATTR_PAS_C}
{$define DOMATTR_PAS_C}

{ DOMAttr }
  DOMAttr = objcclass(DOMNode)
    
  public
    class function alloc: DOMAttr; message 'alloc';

    function name: NSString; message 'name';
    function specified: Boolean; message 'specified';
    procedure setValue (newValue: NSString); message 'setValue:';
    function value: NSString; message 'value';
    function ownerElement: DOMElement; message 'ownerElement';
    function style: DOMCSSStyleDeclaration; message 'style';
  end; external;

{$endif}
{$endif}
