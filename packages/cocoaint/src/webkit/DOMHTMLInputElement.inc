{ Parsed from Webkit.framework DOMHTMLInputElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLINPUTELEMENT_PAS_T}
{$define DOMHTMLINPUTELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLINPUTELEMENT_PAS_R}
{$define DOMHTMLINPUTELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLINPUTELEMENT_PAS_F}
{$define DOMHTMLINPUTELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLINPUTELEMENT_PAS_S}
{$define DOMHTMLINPUTELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLInputElement = objcclass;
  DOMHTMLInputElementPointer = ^DOMHTMLInputElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLINPUTELEMENT_PAS_C}
{$define DOMHTMLINPUTELEMENT_PAS_C}

{ DOMHTMLInputElement }
  DOMHTMLInputElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLInputElement; message 'alloc';

    procedure setDefaultValue (newValue: NSString); message 'setDefaultValue:';
    function defaultValue: NSString; message 'defaultValue';
    function form: DOMHTMLFormElement; message 'form';
    procedure setAccept (newValue: NSString); message 'setAccept:';
    function accept: NSString; message 'accept';
    procedure setAccessKey (newValue: NSString); message 'setAccessKey:';
    function accessKey: NSString; message 'accessKey';
    procedure setAlign (newValue: NSString); message 'setAlign:';
    function align: NSString; message 'align';
    procedure setAlt (newValue: NSString); message 'setAlt:';
    function alt: NSString; message 'alt';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setSize (newValue: NSString); message 'setSize:';
    function size: NSString; message 'size';
    procedure setSrc (newValue: NSString); message 'setSrc:';
    function src: NSString; message 'src';
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
    procedure setUseMap (newValue: NSString); message 'setUseMap:';
    function useMap: NSString; message 'useMap';
    procedure setValue (newValue: NSString); message 'setValue:';
    function value: NSString; message 'value';
    function altDisplayString: NSString; message 'altDisplayString';
    function absoluteImageURL: NSURL; message 'absoluteImageURL';
    procedure blur; message 'blur';
    procedure focus; message 'focus';
    procedure select; message 'select';
    procedure click; message 'click';
  end; external;

{$endif}
{$endif}
