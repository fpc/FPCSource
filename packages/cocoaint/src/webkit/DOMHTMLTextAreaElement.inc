{ Parsed from Webkit.framework DOMHTMLTextAreaElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLTEXTAREAELEMENT_PAS_T}
{$define DOMHTMLTEXTAREAELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLTEXTAREAELEMENT_PAS_R}
{$define DOMHTMLTEXTAREAELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLTEXTAREAELEMENT_PAS_F}
{$define DOMHTMLTEXTAREAELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLTEXTAREAELEMENT_PAS_S}
{$define DOMHTMLTEXTAREAELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLTextAreaElement = objcclass;
  DOMHTMLTextAreaElementPointer = ^DOMHTMLTextAreaElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLTEXTAREAELEMENT_PAS_C}
{$define DOMHTMLTEXTAREAELEMENT_PAS_C}

{ DOMHTMLTextAreaElement }
  DOMHTMLTextAreaElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLTextAreaElement; message 'alloc';

    procedure setDefaultValue (newValue: NSString); message 'setDefaultValue:';
    function defaultValue: NSString; message 'defaultValue';
    function form: DOMHTMLFormElement; message 'form';
    procedure setAccessKey (newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    function type_: NSString; message 'type_';
    procedure setValue (newValue: NSString); message 'setValue:';
    function value: NSString; message 'value';
    procedure blur; message 'blur';
    procedure focus; message 'focus';
    procedure select; message 'select';
  end; external;

{$endif}
{$endif}
