{ Parsed from Webkit.framework DOMHTMLOptionElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLOPTIONELEMENT_PAS_T}
{$define DOMHTMLOPTIONELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLOPTIONELEMENT_PAS_R}
{$define DOMHTMLOPTIONELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLOPTIONELEMENT_PAS_F}
{$define DOMHTMLOPTIONELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLOPTIONELEMENT_PAS_S}
{$define DOMHTMLOPTIONELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLOptionElement = objcclass;
  DOMHTMLOptionElementPointer = ^DOMHTMLOptionElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLOPTIONELEMENT_PAS_C}
{$define DOMHTMLOPTIONELEMENT_PAS_C}

{ DOMHTMLOptionElement }
  DOMHTMLOptionElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLOptionElement; message 'alloc';

    function form: DOMHTMLFormElement; message 'form';
    function text: NSString; message 'text';
    function index: cint; message 'index';
    procedure setLabel (newValue: NSString); message 'setLabel:';
    function label_: NSString; message 'label_';
    procedure setValue (newValue: NSString); message 'setValue:';
    function value: NSString; message 'value';
  end; external;

{$endif}
{$endif}
