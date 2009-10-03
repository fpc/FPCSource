{ Parsed from Webkit.framework DOMHTMLButtonElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLBUTTONELEMENT_PAS_T}
{$define DOMHTMLBUTTONELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLBUTTONELEMENT_PAS_R}
{$define DOMHTMLBUTTONELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLBUTTONELEMENT_PAS_F}
{$define DOMHTMLBUTTONELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLBUTTONELEMENT_PAS_S}
{$define DOMHTMLBUTTONELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLButtonElement = objcclass;
  DOMHTMLButtonElementPointer = ^DOMHTMLButtonElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLBUTTONELEMENT_PAS_C}
{$define DOMHTMLBUTTONELEMENT_PAS_C}

{ DOMHTMLButtonElement }
  DOMHTMLButtonElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLButtonElement; message 'alloc';

    function form: DOMHTMLFormElement; message 'form';
    procedure setAccessKey (newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    function type_: NSString; message 'type_';
    procedure setValue (newValue: NSString); message 'setValue:';
    function value: NSString; message 'value';
  end; external;

{$endif}
{$endif}
