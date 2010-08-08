{ Parsed from Webkit.framework DOMHTMLParagraphElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:19:00 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLPARAGRAPHELEMENT_PAS_T}
{$define DOMHTMLPARAGRAPHELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLPARAGRAPHELEMENT_PAS_R}
{$define DOMHTMLPARAGRAPHELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLPARAGRAPHELEMENT_PAS_F}
{$define DOMHTMLPARAGRAPHELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLPARAGRAPHELEMENT_PAS_S}
{$define DOMHTMLPARAGRAPHELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLParagraphElement = objcclass;
  DOMHTMLParagraphElementPointer = ^DOMHTMLParagraphElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLPARAGRAPHELEMENT_PAS_C}
{$define DOMHTMLPARAGRAPHELEMENT_PAS_C}

{ DOMHTMLParagraphElement }
  DOMHTMLParagraphElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLParagraphElement; message 'alloc';

    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
  end; external;

{$endif}
{$endif}
