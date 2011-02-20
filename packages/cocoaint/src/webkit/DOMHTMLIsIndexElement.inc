{ Parsed from Webkit.framework DOMHTMLIsIndexElement.h }

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
  DOMHTMLIsIndexElement = objcclass external (DOMHTMLInputElement)
    
  public
    function form: DOMHTMLFormElement; message 'form';
    procedure setPrompt (newValue: NSString); message 'setPrompt:';
    function prompt: NSString; message 'prompt';
  end;

{$endif}
{$endif}
