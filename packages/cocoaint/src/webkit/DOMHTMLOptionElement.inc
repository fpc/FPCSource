{ Parsed from Webkit.framework DOMHTMLOptionElement.h }

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
    procedure setDefaultSelected (newValue: BOOL); message 'setDefaultSelected:';
    function defaultSelected: BOOL; message 'defaultSelected';
    function text: NSString; message 'text';
    function index: cint; message 'index';
    procedure setDisabled (newValue: BOOL); message 'setDisabled:';
    function disabled: BOOL; message 'disabled';
    procedure setLabel (newValue: NSString); message 'setLabel:';
    function label_: NSString; message 'label_';
    procedure setSelected (newValue: BOOL); message 'setSelected:';
    function selected: BOOL; message 'selected';
    procedure setValue (newValue: NSString); message 'setValue:';
    function value: NSString; message 'value';
  end;

{$endif}
{$endif}
