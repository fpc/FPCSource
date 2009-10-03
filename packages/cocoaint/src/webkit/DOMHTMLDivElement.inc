{ Parsed from Webkit.framework DOMHTMLDivElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLDIVELEMENT_PAS_C}
{$define DOMHTMLDIVELEMENT_PAS_C}

{ DOMHTMLDivElement }
  DOMHTMLDivElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLDivElement; message 'alloc';

    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
  end; external;

{$endif}
{$endif}
