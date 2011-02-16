{ Parsed from Webkit.framework DOMHTMLMapElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLMAPELEMENT_PAS_T}
{$define DOMHTMLMAPELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLMAPELEMENT_PAS_R}
{$define DOMHTMLMAPELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLMAPELEMENT_PAS_F}
{$define DOMHTMLMAPELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLMAPELEMENT_PAS_S}
{$define DOMHTMLMAPELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLMapElement = objcclass;
  DOMHTMLMapElementPointer = ^DOMHTMLMapElement;
  DOMHTMLMapElementPtr = DOMHTMLMapElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLMAPELEMENT_PAS_C}
{$define DOMHTMLMAPELEMENT_PAS_C}

{ DOMHTMLMapElement }
  DOMHTMLMapElement = objcclass external (DOMHTMLElement)
    
  public
    function areas: DOMHTMLCollection; message 'areas';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
  end;

{$endif}
{$endif}
