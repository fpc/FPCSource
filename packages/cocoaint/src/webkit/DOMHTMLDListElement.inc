{ Parsed from Webkit.framework DOMHTMLDListElement.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


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
  DOMHTMLDListElementPtr = DOMHTMLDListElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLDLISTELEMENT_PAS_C}
{$define DOMHTMLDLISTELEMENT_PAS_C}

{ DOMHTMLDListElement }
  DOMHTMLDListElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLDListElement; message 'alloc';

    procedure setCompact (newValue: Boolean); message 'setCompact:';
    function compact: Boolean; message 'compact';
  end; external;

{$endif}
{$endif}
