{ Parsed from Webkit.framework DOMHTMLLabelElement.h }

{$ifdef TYPES}
{$ifndef DOMHTMLLABELELEMENT_PAS_T}
{$define DOMHTMLLABELELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLLABELELEMENT_PAS_R}
{$define DOMHTMLLABELELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLLABELELEMENT_PAS_F}
{$define DOMHTMLLABELELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLLABELELEMENT_PAS_S}
{$define DOMHTMLLABELELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLLabelElement = objcclass;
  DOMHTMLLabelElementPointer = ^DOMHTMLLabelElement;
  DOMHTMLLabelElementPtr = DOMHTMLLabelElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLLABELELEMENT_PAS_C}
{$define DOMHTMLLABELELEMENT_PAS_C}

{ DOMHTMLLabelElement }
  DOMHTMLLabelElement = objcclass external (DOMHTMLElement)
    
  public
    function form: DOMHTMLFormElement; message 'form';
    procedure setAccessKey (newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
    procedure setHtmlFor (newValue: NSString); message 'setHtmlFor:';
    function htmlFor: NSString; message 'htmlFor';
  end;

{$endif}
{$endif}
