{ Parsed from Webkit.framework DOMHTMLOptionElement.h }
{ Version: 2.1.5 - Fri Feb 11 21:46:20 CET 2011 }


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
  DOMHTMLOptionElementPtr = DOMHTMLOptionElementPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLOPTIONELEMENT_PAS_C}
{$define DOMHTMLOPTIONELEMENT_PAS_C}

{ DOMHTMLOptionElement }
  DOMHTMLOptionElement = objcclass external (DOMHTMLElement)
    
  public
    function form: DOMHTMLFormElement; message 'form';
    procedure setDefaultSelected (newValue: Boolean); message 'setDefaultSelected:';
    function defaultSelected: Boolean; message 'defaultSelected';
    function text: NSString; message 'text';
    function index: cint; message 'index';
    procedure setDisabled (newValue: Boolean); message 'setDisabled:';
    function disabled: Boolean; message 'disabled';
    procedure setLabel (newValue: NSString); message 'setLabel:';
    function label_: NSString; message 'label_';
    procedure setSelected (newValue: Boolean); message 'setSelected:';
    function selected: Boolean; message 'selected';
    procedure setValue (newValue: NSString); message 'setValue:';
    function value: NSString; message 'value';
  end;

{$endif}
{$endif}
