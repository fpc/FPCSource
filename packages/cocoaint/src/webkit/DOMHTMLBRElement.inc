{ Parsed from Webkit.framework DOMHTMLBRElement.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMHTMLBRELEMENT_PAS_T}
{$define DOMHTMLBRELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLBRELEMENT_PAS_R}
{$define DOMHTMLBRELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLBRELEMENT_PAS_F}
{$define DOMHTMLBRELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLBRELEMENT_PAS_S}
{$define DOMHTMLBRELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLBRElement = objcclass;
  DOMHTMLBRElementPointer = ^DOMHTMLBRElement;
  DOMHTMLBRElementPtr = DOMHTMLBRElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLBRELEMENT_PAS_C}
{$define DOMHTMLBRELEMENT_PAS_C}

{ DOMHTMLBRElement }
  DOMHTMLBRElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLBRElement; message 'alloc';

    procedure setClear (newValue: NSString); message 'setClear:';
    function clear: NSString; message 'clear';
  end; external;

{$endif}
{$endif}
