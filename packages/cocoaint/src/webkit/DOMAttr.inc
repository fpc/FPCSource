{ Parsed from Webkit.framework DOMAttr.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


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
