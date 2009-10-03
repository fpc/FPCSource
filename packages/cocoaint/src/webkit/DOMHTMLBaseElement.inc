{ Parsed from Webkit.framework DOMHTMLBaseElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLBASEELEMENT_PAS_T}
{$define DOMHTMLBASEELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLBASEELEMENT_PAS_R}
{$define DOMHTMLBASEELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLBASEELEMENT_PAS_F}
{$define DOMHTMLBASEELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLBASEELEMENT_PAS_S}
{$define DOMHTMLBASEELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLBaseElement = objcclass;
  DOMHTMLBaseElementPointer = ^DOMHTMLBaseElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLBASEELEMENT_PAS_C}
{$define DOMHTMLBASEELEMENT_PAS_C}

{ DOMHTMLBaseElement }
  DOMHTMLBaseElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLBaseElement; message 'alloc';

    procedure setHref (newValue: NSString); message 'setHref:';
    function href: NSString; message 'href';
    procedure setTarget (newValue: NSString); message 'setTarget:';
    function target: NSString; message 'target';
  end; external;

{$endif}
{$endif}
