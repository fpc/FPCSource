{ Parsed from Webkit.framework DOMHTMLLIElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLLIELEMENT_PAS_T}
{$define DOMHTMLLIELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLLIELEMENT_PAS_R}
{$define DOMHTMLLIELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLLIELEMENT_PAS_F}
{$define DOMHTMLLIELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLLIELEMENT_PAS_S}
{$define DOMHTMLLIELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLLIElement = objcclass;
  DOMHTMLLIElementPointer = ^DOMHTMLLIElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLLIELEMENT_PAS_C}
{$define DOMHTMLLIELEMENT_PAS_C}

{ DOMHTMLLIElement }
  DOMHTMLLIElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLLIElement; message 'alloc';

    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
  end; external;

{$endif}
{$endif}
