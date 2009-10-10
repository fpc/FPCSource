{ Parsed from Webkit.framework DOMHTMLDListElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLDLISTELEMENT_PAS_T}
{$define DOMHTMLDLISTELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLDLISTELEMENT_PAS_R}
{$define DOMHTMLDLISTELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLDLISTELEMENT_PAS_F}
{$define DOMHTMLDLISTELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLDLISTELEMENT_PAS_S}
{$define DOMHTMLDLISTELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLDListElement = objcclass;
  DOMHTMLDListElementPointer = ^DOMHTMLDListElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLDLISTELEMENT_PAS_C}
{$define DOMHTMLDLISTELEMENT_PAS_C}

{ DOMHTMLDListElement }
  DOMHTMLDListElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLDListElement; message 'alloc';
  end; external;

{$endif}
{$endif}
