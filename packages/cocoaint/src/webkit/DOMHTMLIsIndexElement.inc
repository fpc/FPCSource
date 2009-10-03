{ Parsed from Webkit.framework DOMHTMLIsIndexElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


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
