{ Parsed from Webkit.framework DOMHTMLSelectElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:19:00 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLSELECTELEMENT_PAS_T}
{$define DOMHTMLSELECTELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLSELECTELEMENT_PAS_R}
{$define DOMHTMLSELECTELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLSELECTELEMENT_PAS_F}
{$define DOMHTMLSELECTELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLSELECTELEMENT_PAS_S}
{$define DOMHTMLSELECTELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLSelectElement = objcclass;
  DOMHTMLSelectElementPointer = ^DOMHTMLSelectElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLSELECTELEMENT_PAS_C}
{$define DOMHTMLSELECTELEMENT_PAS_C}

{ DOMHTMLSelectElement }
  DOMHTMLSelectElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLSelectElement; message 'alloc';

    function type_: NSString; message 'type_';
    procedure setValue (newValue: NSString); message 'setValue:';
    function value: NSString; message 'value';
    function length: cint; message 'length';
    function form: DOMHTMLFormElement; message 'form';
    function options: DOMHTMLOptionsCollection; message 'options';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure add_before(element: DOMHTMLElement; before: DOMHTMLElement); message 'add:before:';
    procedure remove(index: cint); message 'remove:';
    procedure blur; message 'blur';
    procedure focus; message 'focus';

    { Category: DOMHTMLSelectElementDeprecated }
    procedure add(element: DOMHTMLElement; before: DOMHTMLElement); message 'add:element:';
  end; external;

{$endif}
{$endif}
