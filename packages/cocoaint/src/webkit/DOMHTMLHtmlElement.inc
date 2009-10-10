{ Parsed from Webkit.framework DOMHTMLHtmlElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLHTMLELEMENT_PAS_T}
{$define DOMHTMLHTMLELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLHTMLELEMENT_PAS_R}
{$define DOMHTMLHTMLELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLHTMLELEMENT_PAS_F}
{$define DOMHTMLHTMLELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLHTMLELEMENT_PAS_S}
{$define DOMHTMLHTMLELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLHtmlElement = objcclass;
  DOMHTMLHtmlElementPointer = ^DOMHTMLHtmlElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLHTMLELEMENT_PAS_C}
{$define DOMHTMLHTMLELEMENT_PAS_C}

{ DOMHTMLHtmlElement }
  DOMHTMLHtmlElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLHtmlElement; message 'alloc';

    procedure setVersion (newValue: NSString); message 'setVersion:';
    function version: NSString; message 'version';
  end; external;

{$endif}
{$endif}
