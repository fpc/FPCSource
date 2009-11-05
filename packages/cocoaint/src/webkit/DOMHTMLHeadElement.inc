{ Parsed from Webkit.framework DOMHTMLHeadElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLHEADELEMENT_PAS_T}
{$define DOMHTMLHEADELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLHEADELEMENT_PAS_R}
{$define DOMHTMLHEADELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLHEADELEMENT_PAS_F}
{$define DOMHTMLHEADELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLHEADELEMENT_PAS_S}
{$define DOMHTMLHEADELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLHeadElement = objcclass;
  DOMHTMLHeadElementPointer = ^DOMHTMLHeadElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLHEADELEMENT_PAS_C}
{$define DOMHTMLHEADELEMENT_PAS_C}

{ DOMHTMLHeadElement }
  DOMHTMLHeadElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLHeadElement; message 'alloc';

    procedure setProfile (newValue: NSString); message 'setProfile:';
    function profile: NSString; message 'profile';
  end; external;

{$endif}
{$endif}
