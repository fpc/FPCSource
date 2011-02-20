{ Parsed from Webkit.framework DOMHTMLDivElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLDIVELEMENT_PAS_T}
{$define DOMHTMLDIVELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLDIVELEMENT_PAS_R}
{$define DOMHTMLDIVELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLDIVELEMENT_PAS_F}
{$define DOMHTMLDIVELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLDIVELEMENT_PAS_S}
{$define DOMHTMLDIVELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLDivElement = objcclass;
  DOMHTMLDivElementPointer = ^DOMHTMLDivElement;
  DOMHTMLDivElementPtr = DOMHTMLDivElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLDIVELEMENT_PAS_C}
{$define DOMHTMLDIVELEMENT_PAS_C}

{ DOMHTMLDivElement }
  DOMHTMLDivElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
  end;

{$endif}
{$endif}
