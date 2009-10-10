{ Parsed from Webkit.framework DOMHTMLDirectoryElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLDIRECTORYELEMENT_PAS_T}
{$define DOMHTMLDIRECTORYELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLDIRECTORYELEMENT_PAS_R}
{$define DOMHTMLDIRECTORYELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLDIRECTORYELEMENT_PAS_F}
{$define DOMHTMLDIRECTORYELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLDIRECTORYELEMENT_PAS_S}
{$define DOMHTMLDIRECTORYELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLDirectoryElement = objcclass;
  DOMHTMLDirectoryElementPointer = ^DOMHTMLDirectoryElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLDIRECTORYELEMENT_PAS_C}
{$define DOMHTMLDIRECTORYELEMENT_PAS_C}

{ DOMHTMLDirectoryElement }
  DOMHTMLDirectoryElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLDirectoryElement; message 'alloc';
  end; external;

{$endif}
{$endif}
