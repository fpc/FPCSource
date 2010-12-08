{ Parsed from Webkit.framework DOMHTMLIsIndexElement.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMHTMLISINDEXELEMENT_PAS_T}
{$define DOMHTMLISINDEXELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLISINDEXELEMENT_PAS_R}
{$define DOMHTMLISINDEXELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLISINDEXELEMENT_PAS_F}
{$define DOMHTMLISINDEXELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLISINDEXELEMENT_PAS_S}
{$define DOMHTMLISINDEXELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLIsIndexElement = objcclass;
  DOMHTMLIsIndexElementPointer = ^DOMHTMLIsIndexElement;
  DOMHTMLIsIndexElementPtr = DOMHTMLIsIndexElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLISINDEXELEMENT_PAS_C}
{$define DOMHTMLISINDEXELEMENT_PAS_C}

{ DOMHTMLIsIndexElement }
  DOMHTMLIsIndexElement = objcclass(DOMHTMLInputElement)
    
  public
    class function alloc: DOMHTMLIsIndexElement; message 'alloc';

    function form: DOMHTMLFormElement; message 'form';
    procedure setPrompt (newValue: NSString); message 'setPrompt:';
    function prompt: NSString; message 'prompt';
  end; external;

{$endif}
{$endif}
