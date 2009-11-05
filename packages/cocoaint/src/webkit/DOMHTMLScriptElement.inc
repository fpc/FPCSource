{ Parsed from Webkit.framework DOMHTMLScriptElement.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:19:00 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLSCRIPTELEMENT_PAS_T}
{$define DOMHTMLSCRIPTELEMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLSCRIPTELEMENT_PAS_R}
{$define DOMHTMLSCRIPTELEMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLSCRIPTELEMENT_PAS_F}
{$define DOMHTMLSCRIPTELEMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLSCRIPTELEMENT_PAS_S}
{$define DOMHTMLSCRIPTELEMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLScriptElement = objcclass;
  DOMHTMLScriptElementPointer = ^DOMHTMLScriptElement;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLSCRIPTELEMENT_PAS_C}
{$define DOMHTMLSCRIPTELEMENT_PAS_C}

{ DOMHTMLScriptElement }
  DOMHTMLScriptElement = objcclass(DOMHTMLElement)
    
  public
    class function alloc: DOMHTMLScriptElement; message 'alloc';

    procedure setText (newValue: NSString); message 'setText:';
    function text: NSString; message 'text';
    procedure setHtmlFor (newValue: NSString); message 'setHtmlFor:';
    function htmlFor: NSString; message 'htmlFor';
    procedure setEvent (newValue: NSString); message 'setEvent:';
    function event: NSString; message 'event';
    procedure setCharset (newValue: NSString); message 'setCharset:';
    function charset: NSString; message 'charset';
    procedure setSrc (newValue: NSString); message 'setSrc:';
    function src: NSString; message 'src';
    procedure setType (newValue: NSString); message 'setType:';
    function type_: NSString; message 'type_';
  end; external;

{$endif}
{$endif}
