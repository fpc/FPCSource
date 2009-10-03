{ Parsed from Webkit.framework DOMHTMLBRElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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
