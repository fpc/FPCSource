{ Parsed from Webkit.framework DOMHTMLPreElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLPREELEMENT_PAS_T}
{$define DOMHTMLPREELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLPREELEMENT_PAS_R}
{$define DOMHTMLPREELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLPREELEMENT_PAS_F}
{$define DOMHTMLPREELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLPREELEMENT_PAS_S}
{$define DOMHTMLPREELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLPreElement = objcclass;
  DOMHTMLPreElementPointer = ^DOMHTMLPreElement;
  DOMHTMLPreElementPtr = DOMHTMLPreElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLPREELEMENT_PAS_C}
{$define DOMHTMLPREELEMENT_PAS_C}

{ DOMHTMLPreElement }
  DOMHTMLPreElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setWidth (newValue: cint); message 'setWidth:';
    function width: cint; message 'width';
    procedure setWrap (newValue: Boolean); message 'setWrap:';
    function wrap: Boolean; message 'wrap';
  end;

{$endif}
{$endif}
