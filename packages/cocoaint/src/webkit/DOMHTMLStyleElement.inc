{ Parsed from Webkit.framework DOMHTMLStyleElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLSTYLEELEMENT_PAS_T}
{$define DOMHTMLSTYLEELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLSTYLEELEMENT_PAS_R}
{$define DOMHTMLSTYLEELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLSTYLEELEMENT_PAS_F}
{$define DOMHTMLSTYLEELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLSTYLEELEMENT_PAS_S}
{$define DOMHTMLSTYLEELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLStyleElement = objcclass;
  DOMHTMLStyleElementPointer = ^DOMHTMLStyleElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLSTYLEELEMENT_PAS_C}
{$define DOMHTMLSTYLEELEMENT_PAS_C}

{ DOMHTMLStyleElement }
  DOMHTMLStyleElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLStyleElement; message 'alloc';

    procedure setMedia (newValue: NSString); message 'setMedia:';
    function media: NSString; message 'media';
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
  end; external;

{$endif}
{$endif}
