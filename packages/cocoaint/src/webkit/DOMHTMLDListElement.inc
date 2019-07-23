{ Parsed from Webkit.framework DOMHTMLDListElement.h }

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
  DOMHTMLDListElement = objcclass external (DOMHTMLElement)
    
  public
    procedure setCompact (newValue: ObjCBOOL); message 'setCompact:';
    function compact: ObjCBOOL; message 'compact';
  end;

{$endif}
{$endif}
