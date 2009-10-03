{ Parsed from Webkit.framework DOMHTMLMapElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLMAPELEMENT_PAS_C}
{$define DOMHTMLMAPELEMENT_PAS_C}

{ DOMHTMLMapElement }
  DOMHTMLMapElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLMapElement; message 'alloc';

    function areas: DOMHTMLCollection; message 'areas';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
  end; external;

{$endif}
{$endif}
