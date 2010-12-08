{ Parsed from Webkit.framework DOMHTMLParagraphElement.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


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
  DOMHTMLParagraphElementPtr = DOMHTMLParagraphElementPointer;

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
